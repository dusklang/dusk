//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "TypeChecker.h"

#include "mpark/patterns.hpp"

using namespace mpark::patterns;

#define ERR(msg) Diagnostic(Diagnostic::Error, file, msg)

void TypeChecker::visitType(Type *type) {
    match(type->data)(
        pattern(as<StructTy>(arg)) = [&](auto& structTy) {
            StructDecl* structDecl;
            for(auto decl: structs) {
                if(structTy.name == decl->name) {
                    structDecl = decl;
                    break;
                }
            }
            if(!structDecl) {
                reportDiag(ERR("reference to undeclared type `" + structTy.name.getText() + "`")
                           .primaryRange(type->range));
            }
            structTy.decl = structDecl;
        },
        pattern(as<PointerTy>(ds(arg))) = [&](auto& pointedTy) {
            visitType(pointedTy);
        },
        pattern(_) = [] {}
    );
}
void TypeChecker::visitDecl(Decl* decl) {
    auto protoRange = [&]() -> SourceRange {
        SourceRange range = decl->name.range;
        if(decl->externRange) range += *decl->externRange;
        if(decl->keywordRange) range += *decl->keywordRange;
    };
    // Reject nested functions.
    if(declLists.size() > 1 && decl->isComputed()) {
        reportDiag(ERR("nested functions are not supported")
                   .primaryRange(protoRange()));
    }

    if(decl->isComputed()) {
        // If this is a computed declaration, insert declaration into the enclosing scope now to
        // enable recursion.
        declLists.back().push_back(decl);
    }

    if(decl->type != ErrorTy()) {
        visitType(&decl->type);
    }
    for(auto param: decl->paramList) {
        visitType(&param->type);
    }

    if(decl->hasDefinition()) {
        // If we have parameters, start a new scope for referencing them.
        if(decl->isParameterized()) {
            declLists.push_back(std::vector<Decl*>());
            for(auto param: decl->paramList) {
                declLists.back().push_back(param);
            }
        }
        if(decl->isComputed()) {
            // Add Void type to computed declaration if it doesn't have one.
            if(decl->type == ErrorTy()) {
                decl->type = VoidTy();
            } else {
                visitType(&decl->type);
            }

            returnTypeStack.push(decl->type);
            visitScope(decl->body());
            returnTypeStack.pop();

            if(decl->isParameterized()) {
                declLists.pop_back();
            }
        } else {
            visitExpr(decl->expression());
            if(decl->type == ErrorTy()) {
                decl->type = decl->expression()->type;
            } else {
                if(decl->type != decl->expression()->type) {
                    reportDiag(ERR("cannot initialize declaration of type `" +
                                   decl->type.name() + "` with value of type `" +
                                   decl->expression()->type.name())
                               .primaryRange(decl->expression()->totalRange()));
                }
            }
            if(decl->type == VoidTy()) {
                reportDiag(ERR("stored declarations can not have type `void`")
                           .primaryRange(decl->type.range));
            }

            if(decl->isParameterized()) declLists.pop_back();

            // If this is a stored declaration, add the decl to the enclosing scope now, so we can shadow
            // declarations from outer scopes.
            declLists.back().push_back(decl);
        }
    } else {
        if(decl->isExtern()) {
            declLists.back().push_back(decl);
        } else {
            reportDiag(ERR("non-`extern` declaration needs a definition").primaryRange(protoRange()));
        }

        if(decl->type == ErrorTy()) {
            reportDiag(ERR("standalone decl prototype needs an explicit type").primaryRange(protoRange()));
        }
    }
}
void TypeChecker::visitStructDecl(StructDecl* decl) {
    if(declLists.size() > 1) {
        reportDiag(ERR("struct declarations may not yet be nested").primaryRange(decl->structRange + decl->name.range));
    }
    for(auto other: structs) {
        if(decl->name == other->name) {
            reportDiag(ERR("re-declaration of structure `" + decl->name.getText() + "`")
                       .range(other->structRange + other->name.range, "previous declaration here")
                       .primaryRange(decl->structRange + decl->name.range));
        }
    }
    for(auto field: decl->fields) {
        visitType(&field->type);
    }
    structs.push_back(decl);
}
void TypeChecker::visitScope(Scope* scope) {
    // Start a new namespace for declarations inside the scope.
    declLists.push_back(std::vector<Decl*>());
    for(auto node: scope->nodes) {
        if(auto expr = dynamic_cast<Expr*>(node)) {
            visitExpr(expr);
            // Disallow unused expressions.
            if(expr->type != VoidTy()) {
                reportDiag(ERR("unused expression").primaryRange(expr->totalRange()));
            }
        } else {
            visit(node);
        }
    }
    // End the new namespace.
    declLists.pop_back();
}
void TypeChecker::visitIntegerLiteralExpr(IntegerLiteralExpr* expr) {
    expr->type = IntTy::I32();
}
void TypeChecker::visitDecimalLiteralExpr(DecimalLiteralExpr* expr) {
    expr->type = DoubleTy();
}
void TypeChecker::visitBooleanLiteralExpr(BooleanLiteralExpr* expr) {
    expr->type = BoolTy();
}
void TypeChecker::visitCharLiteralExpr(CharLiteralExpr* expr) {
    expr->type = IntTy::I8();
}
void TypeChecker::visitStringLiteralExpr(StringLiteralExpr* expr) {
    expr->type = PointerTy::get(IntTy::I8());
}
void TypeChecker::visitPreOpExpr(PreOpExpr* expr) {
    visitExpr(expr->operand);
    // FIXME: Ensure the given operator is valid for the operand type.
    switch(expr->op) {
        case PreOp::Deref:
            expr->type = *expr->operand->type.pointeeType();
            break;
        case PreOp::AddrOf:
            expr->type = PointerTy::get(expr->operand->type);
            break;
        default:
            expr->type = expr->operand->type;
            break;
    }
}
void TypeChecker::visitBinOpExpr(BinOpExpr* expr) {
    visitExpr(expr->lhs);
    visitExpr(expr->rhs);
    bool pointerAdd = false;
    expr->type = expr->lhs->type;
    if(expr->lhs->type != expr->rhs->type) {
        // Allow +, +=, - and -= between pointers and integers.
        match(expr->lhs->type.data, expr->rhs->type.data)(
            pattern(as<PointerTy>(_), as<IntTy>(_)) = [&] {
                WHEN(expr->op == BinOp::Add || expr->op == BinOp::AddAssignment)
                {
                    pointerAdd = true;
                };
            },
            pattern(_, _) = [&] {
                reportDiag(ERR("mismatched types `" + expr->lhs->type.name() + "` and `" + expr->rhs->type.name() + "` in binary operator expression")
                           .primaryRange(expr->lhs->totalRange())
                           .primaryRange(expr->rhs->totalRange()));
            }
        );
    }
    // FIXME: Check that operators are valid on the operand types.
    switch(expr->op) {
        case BinOp::Add:        break;
        case BinOp::Sub:        break;
        case BinOp::Mult:       break;
        case BinOp::Div:        break;
        case BinOp::Mod:        break;
        case BinOp::BitwiseOr:  break;
        case BinOp::Or:         break;
        case BinOp::BitwiseAnd: break;
        case BinOp::And:        break;
        case BinOp::Equal:
        case BinOp::NotEqual:
        case BinOp::LessThan:
        case BinOp::LessThanOrEqual:
        case BinOp::GreaterThan:
        case BinOp::GreaterThanOrEqual:
            expr->type = BoolTy();
            break;
        case BinOp::Assignment:
        case BinOp::AddAssignment:
        case BinOp::SubAssignment:
        case BinOp::MultAssignment:
        case BinOp::DivAssignment:
        case BinOp::ModAssignment:
        case BinOp::AndAssignment:
        case BinOp::OrAssignment: {
            if(!expr->lhs->isMutable()) {
                reportDiag(ERR("cannot assign to expression").primaryRange(expr->lhs->totalRange(), "expression is immutable"));
            }
            expr->type = VoidTy();
        }
    }
}
void TypeChecker::visitCastExpr(CastExpr* expr) {
    visitExpr(expr->operand);
    visitType(&expr->destType);
    expr->type = expr->destType;
}
void TypeChecker::visitDeclRefExpr(DeclRefExpr* expr) {
    // Type-check arguments.
    for(auto arg: expr->argList) {
        visitExpr(arg);
    }

    // Find the prototype to reference.
    //
    // TODO: Setup a dependency system to allow decls to be referenced before we know about them.
    std::vector<Decl*> nameMatches;
    for(auto it = declLists.rbegin(); it != declLists.rend(); ++it) {
        auto& declList = *it;

        for(auto* decl: declList) {
            if(decl->name != expr->name) continue;
            nameMatches.push_back(decl);
            if(decl->paramList.size() != expr->argList.size()) continue;
            // Check the types of all parameters.
            auto param = decl->paramList.begin();
            auto arg = expr->argList.begin();
            for(;param != decl->paramList.end(); ++param, ++arg) {
                if((*param)->type != (*arg)->type) goto failedToFindMatchInCurrentList;
            }
            // We must have succeeded! Add the decl's prototype and type to the declRefExpr and return.
            expr->decl = decl;
            expr->type = decl->type;
            return;

        failedToFindMatchInCurrentList: continue;
        }
    }
    // We must have failed.
    auto errorMessage = ERR("invalid reference to identifier `" + expr->name.getText() + "`")
        .primaryRange(expr->name.range);
    if(!nameMatches.empty()) {
        for(auto match: nameMatches) {
            errorMessage.range(match->name.range, "differs only in parameter types");
        }
    }
    reportDiag(errorMessage);
}
void TypeChecker::visitMemberRefExpr(MemberRefExpr* expr) {
    visitExpr(expr->root);

    if_let(pattern(as<PointerTy>(_)) = expr->root->type.data) = [&] {
        reportDiag(ERR("cannot yet reference a member of a pointer type")
                       .primaryRange(expr->dotRange + expr->name.range)
                       .range(expr->root->totalRange(), "expression is of type `" + expr->root->type.name() + "`")
                );
    };
    match(expr->root->type.data)(
        pattern(as<StructTy>(arg)) = [&](auto structTy) {
            std::optional<size_t> fieldDecl = std::nullopt;

            auto& fields = structTy.decl->fields;
            for(size_t field = 0; field < fields.size(); ++field) {
                if(expr->name == fields[field]->name) {
                    fieldDecl = field;
                }
            }
            if(!fieldDecl) {
                reportDiag(ERR("struct `" + structTy.name.getText() + "` has no member `" + expr->name.getText() + "`")
                               .primaryRange(expr->dotRange + expr->name.range));
            }
            expr->declIndex = *fieldDecl;
            expr->type = fields[*fieldDecl]->type;
        },
        pattern(_) = [&] {
            reportDiag(ERR("cannot reference member `" + expr->name.getText() + "` of non-struct type `" + expr->type.name() + "`")
                       .primaryRange(expr->dotRange + expr->name.range));
        }
    );
}
void TypeChecker::visitReturnStmt(ReturnStmt* stmt) {
    if(stmt->value) {
        visitExpr(stmt->value);
    }
    // Handle returning value in a void computed decl.
    if(returnTypeStack.top() == VoidTy()) {
        if(stmt->value) {
            reportDiag(ERR("cannot return value from void computed declaration")
                       .primaryRange(stmt->value->totalRange(), "remove this"));
        } else {
            return;
        }
    }

    // Handle returning no value in a non-void computed decl.
    if(!stmt->value) {
        reportDiag(ERR("computed declaration must return a value").primaryRange(stmt->returnRange, "add a return value"));
    }

    // Handle returning a value of a type incompatible with the computed decl's type
    // (currently, "incompatible with" just means "not equal to").
    if(stmt->value->type != returnTypeStack.top()) {
        // TODO: Include in the error message the type of the returned expr.
        reportDiag(ERR("cannot return value of type `" + stmt->value->type.name() + "` from computed declaration of type `" + returnTypeStack.top().name())
                   .primaryRange(stmt->value->totalRange()));
    }
}
void TypeChecker::visitIfStmt(IfStmt* stmt) {
    visitExpr(stmt->condition);
    if(stmt->condition->type != BoolTy()) {
        reportDiag(ERR("conditions in if statements must be of type `bool`")
                   .primaryRange(stmt->condition->totalRange(), "expression is of type `" + stmt->condition->type.name() + "`"));
    }
    visitScope(stmt->thenScope);
    match(stmt->elseNode)(
        pattern(some(as<Scope*>(arg))) = [&](auto scope) { visitScope(scope); },
        pattern(some(as<IfStmt*>(arg))) = [&](auto stmt) { visitIfStmt(stmt); },
        pattern(_) = []{}
    );
}
void TypeChecker::visitWhileStmt(WhileStmt* stmt) {
    visitExpr(stmt->condition);
    if(stmt->condition->type != BoolTy()) {
        reportDiag(ERR("conditions in while statements must be of type `bool`")
                   .primaryRange(stmt->condition->totalRange(), "expression is of type `" + stmt->condition->type.name() + "`"));
    }
    visitScope(stmt->thenScope);
}
