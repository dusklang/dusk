//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "TypeChecker.h"
#include "General/Array.h"

#include "mpark/patterns.hpp"

using namespace mpark::patterns;

#define ERR(msg) Diagnostic(Diagnostic::Error, file, msg)

void TypeChecker::visitDeclPrototype(Decl* decl) {
    switch(decl->protoState) {
        case Decl::Unresolved:
            decl->protoState = Decl::Resolving;
            break;
        case Decl::Resolving:
            reportDiag(ERR("cyclic decl prototype dependency")
                       .primaryRange(decl->name.range));
            break;
        case Decl::Resolved:
            return;
    }

    if(decl->type != ErrorTy()) {
        visitType(&decl->type);
    }
    for(auto param: decl->paramList) {
        visitType(&param->type);
    }

    if(decl->hasDefinition()) {
        if(decl->isComputed()) {
            // Add void type to computed declaration if it doesn't have one.
            if(decl->type == ErrorTy()) {
                decl->type = VoidTy();
            } else {
                visitType(&decl->type);
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
        }
    } else {
        if(decl->type == ErrorTy()) {
            reportDiag(ERR("standalone decl prototype needs an explicit type").primaryRange(decl->protoRange()));
        }
    }
    decl->protoState = Decl::Resolved;
}
void TypeChecker::visitTopLevel(std::vector<ASTNode*> nodes) {
    for(auto node: nodes) {
        if(auto decl = dynamic_cast<Decl*>(node)) {
            declLists.back().push_back(decl);
        } else if(auto decl = dynamic_cast<StructDecl*>(node)) {
            for(auto other: structs) {
                if(decl->name == other->name) {
                    reportDiag(ERR("re-declaration of structure `" + decl->name.getText() + "`")
                               .range(other->structRange + other->name.range, "previous declaration here")
                               .primaryRange(decl->structRange + decl->name.range));
                }
            }
            structs.push_back(decl);
        }
    }
    for(auto node: nodes) {
        visit(node);
    }
}
void TypeChecker::visitType(Type *type, bool shouldResolveStructDecls) {
    match(type->data)(
        pattern(as<StructTy>(arg)) = [&](auto& structTy) {
            if(!structTy.decl) {
                for(auto decl: structs) {
                    if(structTy.name == decl->name) {
                        structTy.decl = decl;
                        break;
                    }
                }
                if(!structTy.decl) {
                    reportDiag(ERR("reference to undeclared type `" + structTy.name.getText() + "`")
                               .primaryRange(type->range));
                }
            }
            if(shouldResolveStructDecls) {
                visitStructDecl(structTy.decl);
            }
        },
        pattern(as<PointerTy>(ds(arg))) = [&](auto pointedTy) {
            visitType(pointedTy, false);
        },
        pattern(_) = [] {}
    );
}
void TypeChecker::visitDecl(Decl* decl) {
    visitDeclPrototype(decl);
    if(decl->hasDefinition()) {
        // If we have parameters, start a new scope for referencing them.
        if(decl->isParameterized()) {
            declLists.push_back(std::vector<Decl*>());
            for(auto param: decl->paramList) {
                declLists.back().push_back(param);
            }
        }
        if(decl->isComputed()) {
            returnTypeStack.push(decl->type);
            visitScope(decl->body());
            returnTypeStack.pop();
        }
        if(decl->isParameterized()) declLists.pop_back();
    } else {
        if(!decl->isExtern()) {
            reportDiag(ERR("non-`extern` declaration needs a definition").primaryRange(decl->protoRange()));
        }
    }
}
void TypeChecker::visitStructDecl(StructDecl* decl) {
    switch(decl->state) {
        case StructDecl::Unresolved:
            decl->state = StructDecl::Resolving;
            break;
        case StructDecl::Resolving:
            reportDiag(ERR("cyclic struct dependency")
                       .primaryRange(decl->name.range));
            break;
        case StructDecl::Resolved:
            return;
    }
    for(auto field: decl->fields) {
        visitType(&field->type);
    }
    decl->state = StructDecl::Resolved;
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
        } else if(auto decl = dynamic_cast<Decl*>(node)) {
            visitDecl(decl);
            if(decl->isComputed()) {
                // Reject nested functions.
                reportDiag(ERR("nested functions are not supported")
                           .primaryRange(decl->protoRange()));
                //declLists.back().push_back(decl);
            }
            if(decl->isStored()) {
                declLists.back().push_back(decl);
            }
        } else if(auto decl = dynamic_cast<StructDecl*>(node)) {
            reportDiag(ERR("struct declarations may not yet be nested").primaryRange(decl->structRange + decl->name.range));
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
            visitType(expr->operand->type.pointeeType());
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
    for(auto& declList: reverse(declLists)) {
        for(auto decl: declList) {
            if(decl->name != expr->name) continue;
            nameMatches.push_back(decl);
            if(decl->paramList.size() != expr->argList.size()) continue;
            visitDeclPrototype(decl);
            for(auto [param, arg]: zip(decl->paramList, expr->argList)) {
                if(param->type != arg->type) goto failedToFindMatchInCurrentList;
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
void TypeChecker::visitReturnExpr(ReturnExpr* expr) {
    expr->type = VoidTy();
    if(expr->value) {
        visitExpr(expr->value);
    }
    // Handle returning value in a void computed decl.
    if(returnTypeStack.top() == VoidTy()) {
        if(expr->value) {
            reportDiag(ERR("cannot return value from void computed declaration")
                       .primaryRange(expr->value->totalRange(), "remove this"));
        } else {
            return;
        }
    }

    // Handle returning no value in a non-void computed decl.
    if(!expr->value) {
        reportDiag(ERR("computed declaration must return a value").primaryRange(expr->returnRange, "add a return value"));
    }

    // Handle returning a value of a type incompatible with the computed decl's type
    // (currently, "incompatible with" just means "not equal to").
    if(expr->value->type != returnTypeStack.top()) {
        // TODO: Include in the error message the type of the returned expr.
        reportDiag(ERR("cannot return value of type `" + expr->value->type.name() + "` from computed declaration of type `" + returnTypeStack.top().name())
                   .primaryRange(expr->value->totalRange()));
    }
}
void TypeChecker::visitIfExpr(IfExpr* expr) {
    expr->type = VoidTy();
    visitExpr(expr->condition);
    if(expr->condition->type != BoolTy()) {
        reportDiag(ERR("conditions in if expressions must be of type `bool`")
                   .primaryRange(expr->condition->totalRange(), "expression is of type `" + expr->condition->type.name() + "`"));
    }
    visitScope(expr->thenScope);
    match(expr->elseNode)(
        pattern(some(as<Scope*>(arg))) = [&](auto scope) { visitScope(scope); },
        pattern(some(as<IfExpr*>(arg))) = [&](auto expr) { visitIfExpr(expr); },
        pattern(_) = []{}
    );
}
void TypeChecker::visitWhileExpr(WhileExpr* expr) {
    expr->type = VoidTy();
    visitExpr(expr->condition);
    if(expr->condition->type != BoolTy()) {
        reportDiag(ERR("conditions in while expressions must be of type `bool`")
                   .primaryRange(expr->condition->totalRange(), "expression is of type `" + expr->condition->type.name() + "`"));
    }
    visitScope(expr->thenScope);
}
