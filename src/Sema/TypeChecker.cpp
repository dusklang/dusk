//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "TypeChecker.h"

void TypeChecker::visitDecl(std::shared_ptr<Decl> decl) {
    // Search for existing declaration in the current scope with the same name and parameter types.
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
            declLists.push_back(std::vector<std::shared_ptr<Decl>>());
            for(auto& param: decl->paramList) {
                declLists.back().push_back(param);
            }
        }
        if(decl->isComputed()) {
            // Add Void type to computed declaration if it doesn't have one.
            if(decl->type.isInferred()) decl->type = BuiltinType::Void;

            // Recursive lambda for handling nested scopes inside the function, as well as the function
            // body itself.
            //
            // TODO: Move this to the visitScope() method by storing context in the TypeChecker.
            std::function<void(std::shared_ptr<Scope>)> handleScope = [&](std::shared_ptr<Scope> scope) {
                // Start another new, inner scope for declarations inside the scope.
                declLists.push_back(std::vector<std::shared_ptr<Decl>>());

                // These variables track state for nodes after a return statement. We still typecheck
                // them, but they'll be removed from the AST after we're done.
                auto alreadyReturned = false;
                auto alreadyDiagnosedStatementsAfterReturnStatement = false;
                auto afterReturnIterator = scope->nodes.end();
                for(auto&& node = scope->nodes.begin(); node != scope->nodes.end(); ++node) {
                    // Warn on code after a return statement.
                    if(alreadyReturned && !alreadyDiagnosedStatementsAfterReturnStatement) {
                        reportWarning("Code after return statement will not be executed", *node);
                        alreadyReturned = alreadyDiagnosedStatementsAfterReturnStatement;
                        afterReturnIterator = node;
                    }
                    // Handle return statements.
                    if(auto ret = std::dynamic_pointer_cast<ReturnStmt>(*node)) {
                        alreadyReturned = true;
                        visitReturnStmt(ret);
                        // Handle returning value in a void computed decl.
                        if(decl->type == BuiltinType::Void) {
                            if(ret->value) {
                                reportError("Attempted to return value from Void computed decl '"
                                            + decl->name + "'",
                                            ret);
                            } else {
                                continue;
                            }
                        }

                        // Handle returning no value in a non-void computed decl.
                        if(!ret->value) {
                            reportError("Computed decl '" + decl->name + "' must return a value",
                                        ret);
                        }

                        // Handle returning a value of a type incompatible with the computed decl's type
                        // (currently, "incompatible with" just means "not equal to").
                        if(ret->value->type != decl->type) {
                            // TODO: Include in the error message the type of the returned expr.
                            reportError("Attempted to return value of incompatible type from computed decl "
                                        "of type " + decl->type.name(),
                                        ret);
                        }
                    } else if(auto ifStmt = std::dynamic_pointer_cast<IfStmt>(*node)) {
                        visitExpr(ifStmt->condition);
                        if(ifStmt->condition->type != BuiltinType::Bool) {
                            reportError("Expression in if statement is not of type Bool", ifStmt);
                        }
                        handleScope(ifStmt->thenScope);
                        if(ifStmt->elseScope) handleScope(*ifStmt->elseScope);
                    } else if(auto whileStmt = std::dynamic_pointer_cast<WhileStmt>(*node)) {
                        visitExpr(whileStmt->condition);
                        if(whileStmt->condition->type != BuiltinType::Bool) {
                            reportError("Expression in while statement is not of type Bool", whileStmt);
                        }
                        handleScope(whileStmt->thenScope);
                    } else if(auto expr = std::dynamic_pointer_cast<Expr>(*node)) {
                        visitExpr(expr);
                        // Warn on unused expressions.
                        if(expr->type != BuiltinType::Void) {
                            reportWarning("Unused expression", expr);
                        }
                    } else {
                        visit(*node);
                    }
                }
                declLists.pop_back();
                // Remove nodes after a return statement.
                scope->nodes.erase(afterReturnIterator, scope->nodes.end());
            };
            handleScope(decl->body());
            if(decl->isParameterized()) declLists.pop_back();
            return;
        }

        // We can now assume the decl is stored.
        assert(decl->isStored());

        visitExpr(decl->expression());
        if(decl->type.isInferred()) {
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
        if(decl->type.builtinType() == BuiltinType::Void) reportError("Stored declarations can not have type Void", decl);

        if(decl->isParameterized()) declLists.pop_back();

        // If this is a stored declaration, add the decl to the enclosing scope now, so we can shadow
        // declarations from outer scopes.
        declLists.back().push_back(decl);
    } else {
        if(!decl->isExtern) {
            reportError("Non-extern declarations currently always need definitions", decl);
        }

        if(decl->type.isInferred()) {
            reportError("Standalone decl prototypes need types", decl);
        }
    }
}
void TypeChecker::visitScope(std::shared_ptr<Scope> scope) {}
void TypeChecker::visitArgument(std::shared_ptr<Argument> argument) {}
void TypeChecker::visitIntegerLiteralExpr(std::shared_ptr<IntegerLiteralExpr> expr) {
    expr->type = BuiltinType::i32;
}
void TypeChecker::visitDecimalLiteralExpr(std::shared_ptr<DecimalLiteralExpr> expr) {
    expr->type = BuiltinType::f64;
}
void TypeChecker::visitBooleanLiteralExpr(std::shared_ptr<BooleanLiteralExpr> expr) {
    expr->type = BuiltinType::Bool;
}
void TypeChecker::visitCharLiteralExpr(std::shared_ptr<CharLiteralExpr> expr) {
    expr->type = BuiltinType::Char;
}
void TypeChecker::visitStringLiteralExpr(std::shared_ptr<StringLiteralExpr> expr) {
    expr->type = Type(std::make_shared<Type>(BuiltinType::Char));
}
void TypeChecker::visitDeclRefExpr(std::shared_ptr<DeclRefExpr> expr) {
    // Type-check arguments.
    for(auto& arg: expr->argList) {
        visitExpr(arg.value);
    }

    // Find the prototype to reference.
    //
    // TODO: Setup a dependency system to allow decls to be referenced before we know about them.
    std::vector<std::shared_ptr<Decl>> nameMatches;
    for(auto it = declLists.rbegin(); it != declLists.rend(); ++it) {
        auto& declList = *it;
        for(auto& decl: declList) {
            if(decl->name != expr->name) continue;
            nameMatches.push_back(decl);
            if(decl->paramList.size() != expr->argList.size()) continue;
            // Check the types of all parameters
            auto param = decl->paramList.begin();
            auto arg = expr->argList.begin();
            for(;param != decl->paramList.end(); ++param, ++arg) {
                if((*param)->type != arg->value->type) goto failedToFindMatchInCurrentList;
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

void TypeChecker::visitReturnStmt(std::shared_ptr<ReturnStmt> stmt) {
    if(stmt->value) {
        visitExpr(stmt->value);
    }
}

void TypeChecker::visitAssignmentStmt(std::shared_ptr<AssignmentStmt> stmt) {
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
