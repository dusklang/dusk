//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "ConstraintGenerator.h"

void ConstraintGenerator::visitDecl(std::shared_ptr<Decl> decl) {
    // Search for an existing declaration in the current scope with the same name and parameter types.
    for(auto& existingDecl: declLists.back()) {
        if(existingDecl->name != decl->name) continue;
        if(existingDecl->paramList.size() != decl->paramList.size()) continue;

        auto&& param = decl->paramList.begin();
        auto&& existingParam = existingDecl->paramList.begin();
        for(;param != decl->paramList.end() && existingParam != existingDecl->paramList.end();
            ++param, ++existingParam) {
            if((*existingParam)->type != (*param)->type) goto notAMatch;
        }

        // We must have found a matching declaration in the same scope.
        reportError("Cannot redeclare '" + decl->name + "'\n" +
                    "\tPrevious declaration here: " + existingDecl->range.getSubstring(),
                    decl);

        notAMatch: continue;
    }

    if(decl->isComputed()) {
        // If this is a computed declaration, insert declaration into the enclosing scope now to
        // enable recursion.
        declLists.back().push_back(decl);
    }

    if(decl->hasDefinition()) {
        // If we have parameters, start a new scope for referencing them.
        if(decl->isParameterized()) {
            declLists.push_back(std::vector<std::shared_ptr<Decl>>());
            for(auto& param: decl->paramList) {
                declLists.back().push_back(param);
            }
        }

        if(decl->isComputed()) {
            if(decl->type == Type::Error()) {
                decl->type = Type::Void();
            }
            returnTypeStack.push(decl->type);

            // Visit the body of the computed decl.
            visitScope(decl->body());

            returnTypeStack.pop();
        } else {
            // Stored.

            visitExpr(decl->expression());

            // If the declaration doesn't have an explicit type, then just use the type of the expression.
            if(decl->type == Type::Error()) {
                decl->type = decl->expression()->type;
            }
            // Otherwise, constrain the two types to be equal.
            else {
                constrain(Constraint::Equal(decl->type, decl->expression()->type));
            }
        }
        // If we have parameters, end the scope we created earlier (see above).
        if(decl->isParameterized()) declLists.pop_back();

        // If this is a stored declaration, add the decl to the enclosing scope now, so we can shadow
        // declarations from outer scopes.
        if(decl->isStored()) declLists.back().push_back(decl);
    } else {
        // No definition.
        if(decl->type == Type::Error()) {
            reportError("Declarations without definitions need explicit types", decl);
        }
    }
}
void ConstraintGenerator::visitScope(std::shared_ptr<Scope> scope) {
    // Start a new namespace for declarations inside the scope.
    declLists.push_back(std::vector<std::shared_ptr<Decl>>());
    for(auto& node: scope->nodes) {
        visit(node);
    }
    // End the new namespace.
    declLists.pop_back();
}
void ConstraintGenerator::visitIntegerLiteralExpr(std::shared_ptr<IntegerLiteralExpr> expr) {
    expr->type = newIntTypeVariable();
}
void ConstraintGenerator::visitDecimalLiteralExpr(std::shared_ptr<DecimalLiteralExpr> expr) {
    expr->type = newDecimalTypeVariable();
}
void ConstraintGenerator::visitBooleanLiteralExpr(std::shared_ptr<BooleanLiteralExpr> expr) {
    expr->type = Type::Bool();
}
void ConstraintGenerator::visitCharLiteralExpr(std::shared_ptr<CharLiteralExpr> expr) {
    expr->type = Type::I8();
}
void ConstraintGenerator::visitStringLiteralExpr(std::shared_ptr<StringLiteralExpr> expr) {
    expr->type = Type::Pointer(Type::I8());
}
void ConstraintGenerator::visitDeclRefExpr(std::shared_ptr<DeclRefExpr> expr) {
    if(expr->type == Type::Error()) {
        expr->type = newTypeVariable();
    }

    // Constrain arguments.
    for(auto& arg: expr->argList) {
        visitExpr(arg.value);
    }

    // Now, constrain all of the argument types to their corresponding parameter types.
    auto param = expr->decl->paramList.begin();
    auto arg = expr->argList.begin();
    for(;param != expr->decl->paramList.end(); ++param, ++arg) {
        constrain(Constraint::Equal((*param)->type, arg->value->type));
    }
}
void ConstraintGenerator::visitReturnStmt(std::shared_ptr<ReturnStmt> stmt) {
    if(returnTypeStack.empty()) {
        reportError("Unexpected return statement outside of a computed declaration", stmt);
    }
    if(!stmt->value) {
        constrain(Constraint::Equal(Type::Void(), returnTypeStack.top()));
    } else {
        visitExpr(stmt->value);
    }
}
void ConstraintGenerator::visitAssignmentStmt(std::shared_ptr<AssignmentStmt> stmt) {
    visitDeclRefExpr(stmt->lhs);
    visitExpr(stmt->rhs);
    constrain(Constraint::Equal(stmt->lhs->type, stmt->rhs->type));
}
void ConstraintGenerator::visitIfStmt(std::shared_ptr<IfStmt> stmt) {
    visitExpr(stmt->condition);
    constrain(Constraint::Equal(stmt->condition->type, Type::Bool()));

    visitScope(stmt->thenScope);
    if(stmt->elseScope) {
        visitScope(*stmt->elseScope);
    }
}
void ConstraintGenerator::visitWhileStmt(std::shared_ptr<WhileStmt> stmt) {
    visitExpr(stmt->condition);
    constrain(Constraint::Equal(stmt->condition->type, Type::Bool()));

    visitScope(stmt->thenScope);
}
