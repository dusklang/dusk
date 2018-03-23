//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "NameResolver.h"

void NameResolver::visitDecl(std::shared_ptr<Decl> decl) {
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
            visitScope(decl->body());
            if(decl->isParameterized()) declLists.pop_back();
            return;
        } else {
            visitExpr(decl->expression());

            if(decl->isParameterized()) declLists.pop_back();

            // If this is a stored declaration, add the decl to the enclosing scope now, so we can shadow
            // declarations from outer scopes.
            declLists.back().push_back(decl);
        }
    }
}
void NameResolver::visitScope(std::shared_ptr<Scope> scope) {
    declLists.push_back(std::vector<std::shared_ptr<Decl>>());
    for(auto&& node = scope->nodes.begin(); node != scope->nodes.end(); ++node) {
        visit(*node);
    }
    declLists.pop_back();
}
void NameResolver::visitDeclRefExpr(std::shared_ptr<DeclRefExpr> expr) {
    // Resolve any decl references in the arguments.
    for(auto& arg: expr->argList) {
        visitExpr(arg.value);
    }

    // Find the declaration to reference.
    //
    // TODO: Setup a dependency system to allow decls to be referenced before we know about them.
    std::vector<std::shared_ptr<Decl>> nameMatches;
    for(auto it = declLists.rbegin(); it != declLists.rend(); ++it) {
        auto& declList = *it;
        for(auto& decl: declList) {
            if(decl->name != expr->name) continue;
            nameMatches.push_back(decl);
            if(decl->paramList.size() != expr->argList.size()) continue;

            // We must have succeeded! Add the decl's prototype and type to the declRefExpr and return.
            expr->decl = decl;
            return;
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

void NameResolver::visitReturnStmt(std::shared_ptr<ReturnStmt> stmt) {
    if(stmt->value) {
        visitExpr(stmt->value);
    }
}

void NameResolver::visitAssignmentStmt(std::shared_ptr<AssignmentStmt> stmt) {
    visitDeclRefExpr(stmt->lhs);
    visitExpr(stmt->rhs);
}

void NameResolver::visitIfStmt(std::shared_ptr<IfStmt> stmt) {
    visitExpr(stmt->condition);
    visitScope(stmt->thenScope);
    if(stmt->elseScope) visitScope(*stmt->elseScope);
}

void NameResolver::visitWhileStmt(std::shared_ptr<WhileStmt> stmt) {
    visitExpr(stmt->condition);
    visitScope(stmt->thenScope);
}
