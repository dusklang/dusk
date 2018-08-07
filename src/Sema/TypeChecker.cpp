//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "TypeChecker.h"

#include "mpark/patterns.hpp"

using namespace mpark::patterns;

void TypeChecker::visitType(Type *type) {
    match(type->data)(
        pattern(as<Type::StructTy>(arg)) = [&](auto& structTy) {
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

    if(decl->type != Type::Error()) {
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
            if(decl->type == Type::Error()) {
                decl->type = Type::Void();
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
            if(decl->type == Type::Error()) {
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
            if(decl->type == Type::Void()) reportError("Stored declarations can not have type Void", decl);

            if(decl->isParameterized()) declLists.pop_back();

            // If this is a stored declaration, add the decl to the enclosing scope now, so we can shadow
            // declarations from outer scopes.
            declLists.back().push_back(decl);
        }
    } else {
        if(decl->isExtern) {
            declLists.back().push_back(decl);
        } else {
            reportError("Non-extern declarations currently always need definitions", decl);
        }

        if(decl->type == Type::Error()) {
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
            if(expr->type != Type::Void()) {
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
    expr->type = Type::I32();
}
void TypeChecker::visitDecimalLiteralExpr(DecimalLiteralExpr* expr) {
    expr->type = Type::Double();
}
void TypeChecker::visitBooleanLiteralExpr(BooleanLiteralExpr* expr) {
    expr->type = Type::Bool();
}
void TypeChecker::visitCharLiteralExpr(CharLiteralExpr* expr) {
    expr->type = Type::I8();
}
void TypeChecker::visitStringLiteralExpr(StringLiteralExpr* expr) {
    expr->type = Type::Pointer(Type::I8());
}
void TypeChecker::visitPreOpExpr(PreOpExpr* expr) {
    visitExpr(expr->operand);
    // FIXME: Ensure the given operator is valid for the operand type.
    switch(expr->op) {
        case PreOp::Deref:
            expr->type = expr->operand->type.pointeeType();
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
        IDENTIFIERS(x);
        // Allow +, +=, - and -= between pointers and integers.
        match(expr->lhs->type.indirection, expr->rhs->type.indirection, expr->rhs->type.data)(
            pattern(x, 0, as<Type::IntegerTy>(_)) = [&](auto x) {
                WHEN(x > 0 && (expr->op == BinOp::Add || expr->op == BinOp::AddAssignment))
                {
                    pointerAdd = true;
                };
            },
            pattern(_, _, _) = [&] {
                reportError("Mismatched types `" + expr->lhs->type.name() + "` and `" + expr->rhs->type.name() + "` in binary operator expression", expr);
            }
        );
    }
    // FIXME: Check that operators are valid on the operand types.
    switch(expr->op) {
        case BinOp::Add:  break;
        case BinOp::Sub:  break;
        case BinOp::Mult: break;
        case BinOp::Div:  break;
        case BinOp::Mod:  break;
        case BinOp::Or:   break;
        case BinOp::And:  break;
        case BinOp::Equal:
        case BinOp::NotEqual:
        case BinOp::LessThan:
        case BinOp::LessThanOrEqual:
        case BinOp::GreaterThan:
        case BinOp::GreaterThanOrEqual:
            expr->type = Type::Bool();
            break;
        case BinOp::Assignment:
        case BinOp::AddAssignment:
        case BinOp::SubAssignment:
        case BinOp::MultAssignment:
        case BinOp::DivAssignment:
        case BinOp::ModAssignment: {
            auto declRef = dynamic_cast<DeclRefExpr*>(expr->lhs);
            if(!declRef) {
                reportError("Only stored declaration references can currently be assigned to", expr);
            }
            if(!declRef->decl->isVar) {
                reportError("Cannot assign to constant declaration '" + declRef->decl->name + "'", expr);
            }
            if(declRef->type != expr->rhs->type && !pointerAdd) {
                reportError("Cannot assign value of type '" + expr->rhs->type.name()
                            + "' to mutable declaration of type '" + declRef->type.name(),
                            expr);
            }
            switch(expr->op) {
                case BinOp::AddAssignment: break;
                case BinOp::SubAssignment: break;
                case BinOp::MultAssignment: break;
                case BinOp::DivAssignment: break;
                case BinOp::ModAssignment: break;
                default: break;
            }
            expr->type = Type::Void();
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
    for(auto& arg: expr->argList) {
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
            errorMessage += "\n\t" + match->range.getSubstring();
        }
    }
    reportError(errorMessage, expr);
}
void TypeChecker::visitReturnStmt(ReturnStmt* stmt) {
    if(stmt->value) {
        visitExpr(stmt->value);
    }
    // Handle returning value in a void computed decl.
    if(returnTypeStack.top() == Type::Void()) {
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
    if(stmt->condition->type != Type::Bool()) {
        reportError("Expression in if statement is not of type Bool", stmt);
    }
    visitScope(stmt->thenScope);
    if(stmt->elseScope) visitScope(stmt->elseScope);
}
void TypeChecker::visitWhileStmt(WhileStmt* stmt) {
    visitExpr(stmt->condition);
    if(stmt->condition->type != Type::Bool()) {
        reportError("Expression in while statement is not of type Bool", stmt);
    }
    visitScope(stmt->thenScope);
}
