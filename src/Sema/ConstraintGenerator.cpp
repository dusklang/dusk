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

            // If this is a stored declaration, add the decl to the enclosing scope now, so we can shadow
            // declarations from outer scopes.
            declLists.back().push_back(decl);
            visitExpr(decl->expression());

            // If the declaration doesn't have an explicit type, then just use the type of the expression.
            if(decl->type == Type::Error()) {
                decl->type = decl->expression()->type;
            }
            // Otherwise, constrain the two types to be equal.
            else {
                constraints.push_back(Constraint::Equal(decl->type, decl->expression()->type));
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
    expr->type = newTypeVariable();
    constraints.push_back(Constraint::IntegerLiteral(expr->type));
}
void ConstraintGenerator::visitDecimalLiteralExpr(std::shared_ptr<DecimalLiteralExpr> expr) {
    expr->type = newTypeVariable();
    constraints.push_back(Constraint::DecimalLiteral(expr->type));
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
    expr->type = newTypeVariable();

    // Constrain arguments.
    for(auto& arg: expr->argList) {
        visitExpr(arg.value);
    }

    // Look up all possible matches for this declaration reference, starting with the current scope
    // and working outwards from there.
    std::vector<std::shared_ptr<Decl>> nameMatches;
    std::vector<Constraint> choices;
    for(auto it = declLists.rbegin(); it != declLists.rend(); ++it) {
        auto& declList = *it;
        for(auto& decl: declList) {
            if(decl->name != expr->name) continue;
            nameMatches.push_back(decl);
            if(decl->paramList.size() != expr->argList.size()) continue;

            // Come up with all the constraints that must be satisfied for this declaration to be a
            // a valid choice for this reference.
            std::vector<Constraint> currentChoice;

            // Begin with recording the declaration so we can access it later.
            currentChoice.push_back(Constraint::BindOverload(expr->type, decl));

            // Next, constrain the type of the decl to be equal to the type of the expression.
            currentChoice.push_back(Constraint::Equal(decl->type, expr->type));

            // Now, let's constrain all of the argument types to their corresponding parameter types.
            auto param = decl->paramList.begin();
            auto arg = expr->argList.begin();
            for(;param != decl->paramList.end(); ++param, ++arg) {
                currentChoice.push_back(Constraint::Equal((*param)->type, arg->value->type));
            }

            choices.push_back(Constraint::Conjunction(currentChoice));
        }
    }
    if(choices.empty()) {
        std::string errorMessage = "Invalid reference to identifier '" + expr->name + "'";
        if(!nameMatches.empty()) {
            errorMessage += "\n\nHere are some matches that differ only in the number or types of parameters:";
            for(auto& match: nameMatches) {
                errorMessage += "\n\t" + match->range.getSubstring();
            }
        }
        reportError(errorMessage, expr);
    } else {
        constraints.push_back(Constraint::Disjunction(choices));
    }
}
void ConstraintGenerator::visitReturnStmt(std::shared_ptr<ReturnStmt> stmt) {
    if(returnTypeStack.empty()) {
        reportError("Unexpected return statement outside of a computed declaration", stmt);
    }
    if(!stmt->value) {
        constraints.push_back(Constraint::Equal(Type::Void(), returnTypeStack.top()));
    } else {
        visitExpr(stmt->value);
    }
}
void ConstraintGenerator::visitAssignmentStmt(std::shared_ptr<AssignmentStmt> stmt) {
    visitDeclRefExpr(stmt->lhs);
    visitExpr(stmt->rhs);
    constraints.push_back(Constraint::Equal(stmt->lhs->type, stmt->rhs->type));
}
void ConstraintGenerator::visitIfStmt(std::shared_ptr<IfStmt> stmt) {
    visitExpr(stmt->condition);
    constraints.push_back(Constraint::Equal(stmt->condition->type, Type::Bool()));

    visitScope(stmt->thenScope);
    if(stmt->elseScope) {
        visitScope(*stmt->elseScope);
    }
}
void ConstraintGenerator::visitWhileStmt(std::shared_ptr<WhileStmt> stmt) {
    visitExpr(stmt->condition);
    constraints.push_back(Constraint::Equal(stmt->condition->type, Type::Bool()));

    visitScope(stmt->thenScope);
}
