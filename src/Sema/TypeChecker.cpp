//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "TypeChecker.h"

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

    if(decl->hasDefinition()) {
        // If we have parameters, start a new scope for referencing them.
        if(decl->isParameterized()) {
            declLists.push_back(std::vector<Decl*>());
            for(auto& param: decl->paramList) {
                declLists.back().push_back(param);
            }
        }
        if(decl->isComputed()) {
            // Add Void type to computed declaration if it doesn't have one.
            if(decl->type == Type::Error()) decl->type = Type::Void();

            returnTypeStack.push(decl->type);
            visitScope(decl->body());
            returnTypeStack.pop();

            if(decl->isParameterized()) declLists.pop_back();
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
        if(!decl->isExtern) {
            reportError("Non-extern declarations currently always need definitions", decl);
        }

        if(decl->type == Type::Error()) {
            reportError("Standalone decl prototypes need types", decl);
        }
    }
}
void TypeChecker::visitScope(Scope* scope) {
    // Start a new namespace for declarations inside the scope.
    declLists.push_back(std::vector<Decl*>());
    for(auto& node: scope->nodes) {
        if(auto expr = dynamic_cast<Expr*>(node)) {
            visitExpr(expr);
            // Warn on unused expressions.
            if(expr->type != Type::Void()) {
                reportWarning("Unused expression", expr);
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

void TypeChecker::visitAssignmentStmt(AssignmentStmt* stmt) {
    visitDeclRefExpr(stmt->lhs);
    visitExpr(stmt->rhs);
    if(!stmt->lhs->decl->isVar) {
        reportError("Cannot assign to constant declaration '" + stmt->lhs->decl->name + "'", stmt);
    }
    if(stmt->lhs->type != stmt->rhs->type) {
        reportError("Cannot assign value of type '" + stmt->rhs->type.name()
                    + "' to mutable declaration of type '" + stmt->lhs->type.name(),
                    stmt);
    }
}
