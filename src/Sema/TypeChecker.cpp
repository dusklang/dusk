//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "TypeChecker.h"

#include "mpark/patterns.hpp"

using namespace mpark::patterns;

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
                reportError<ASTNode>("Reference to undeclared type `" + structTy.name + '`', nullptr);
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
    // Reject nested functions.
    if(declLists.size() > 1 && decl->isComputed()) {
        // TODO: Just report the source range of the prototype, which now is not its own ASTNode
        // so is not possible.
        reportError("Unexpected nested function '" + decl->name + "'", decl);
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
                    reportError("Cannot assign value of type '" +
                                decl->expression()->type.name() +
                                "' to declaration of type '" +
                                decl->type.name() + "'",
                                decl);
                }
            }
            if(decl->type == VoidTy()) reportError("Stored declarations can not have type Void", decl);

            if(decl->isParameterized()) declLists.pop_back();

            // If this is a stored declaration, add the decl to the enclosing scope now, so we can shadow
            // declarations from outer scopes.
            declLists.back().push_back(decl);
        }
    } else {
        if(decl->isExtern()) {
            declLists.back().push_back(decl);
        } else {
            reportError("Non-extern declarations currently always need definitions", decl);
        }

        if(decl->type == ErrorTy()) {
            reportError("Standalone decl prototypes need types", decl);
        }
    }
}
void TypeChecker::visitStructDecl(StructDecl* decl) {
    if(declLists.size() > 1) {
        reportError("`struct` declarations may not (yet) be nested", decl);
    }
    for(auto other: structs) {
        if(decl->name == other->name) {
            reportError(std::string("Re-declaration of structure `") + decl->name + '`', decl);
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
                reportError("Unused expression", expr);
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
                reportError("Mismatched types `" + expr->lhs->type.name() + "` and `" + expr->rhs->type.name() + "` in binary operator expression", expr);
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
                reportError("Cannot assign to immutable expression", expr);
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
    std::string errorMessage = "Invalid reference to identifier '" + expr->name + "'";
    if(!nameMatches.empty()) {
        errorMessage += "\n\nHere are some matches that differ only in parameter types:";
        for(auto& match: nameMatches) {
            //errorMessage += "\n\t" + file.substringFromRange(match->range);
        }
    }
    reportError(errorMessage, expr);
}
void TypeChecker::visitMemberRefExpr(MemberRefExpr* expr) {
    visitExpr(expr->root);

    if_let(pattern(as<PointerTy>(_)) = expr->root->type.data) = [&] {
        reportError("Cannot reference a member of a pointer type", expr);
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
                reportError("Struct `" + structTy.name + "` has no member `" + expr->name + "`", expr);
            }
            expr->declIndex = *fieldDecl;
            expr->type = fields[*fieldDecl]->type;
        },
        pattern(_) = [&] {
            reportError("Cannot reference member `" + expr->name + "` of non-struct type", expr);
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
            reportError("Attempted to return value from Void computed decl '", stmt);
        } else {
            return;
        }
    }

    // Handle returning no value in a non-void computed decl.
    if(!stmt->value) {
        reportError("Computed decl must return a value", stmt);
    }

    // Handle returning a value of a type incompatible with the computed decl's type
    // (currently, "incompatible with" just means "not equal to").
    if(stmt->value->type != returnTypeStack.top()) {
        // TODO: Include in the error message the type of the returned expr.
        reportError("Attempted to return value of incompatible type from computed decl "
                    "of type " + returnTypeStack.top().name(),
                    stmt);
    }
}
void TypeChecker::visitIfStmt(IfStmt* stmt) {
    visitExpr(stmt->condition);
    if(stmt->condition->type != BoolTy()) {
        reportError("Expression in if statement is not of type Bool", stmt);
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
        reportError("Expression in while statement is not of type Bool", stmt);
    }
    visitScope(stmt->thenScope);
}
