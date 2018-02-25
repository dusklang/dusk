//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "TypeChecker.h"

void TypeChecker::visitDecl(std::shared_ptr<Decl> decl) {
    // Insert declaration into the current scope.
    declLists.back().push_back(decl);
    // Reject nested functions.
    if(declLists.size() > 1 && decl->isComputed()) {
        // TODO: Just report the source range of the prototype, which now is not its own
        // ASTNode so is not possible.
        reportError("Unexpected nested function '" + decl->name + "'", decl);
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
            // Start another new, inner scope for declarations inside the body of the computed declaration.
            declLists.push_back(std::vector<std::shared_ptr<Decl>>());
            // Add Void type to computed declaration if it doesn't have one.
            if(decl->type.isInferred()) decl->type.resolveType(BuiltinType::Void);

            for(auto& node: decl->body()->nodes) {
                // Handle return statements.
                if(auto ret = std::dynamic_pointer_cast<ReturnStmt>(node)) {
                    visitReturnStmt(ret);
                    // Handle returning value in a void computed decl.
                    if(decl->type.getType() == BuiltinType::Void) {
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
                    if(*ret->value->type != decl->type.getType()) {
                        // TODO: Include in the error message the type of the returned expr.
                        reportError("Attempted to return value of incompatible type from computed decl "
                                    "of type " + getNameForBuiltinType(decl->type.getType()),
                                    ret);
                    }
                } else if(auto expr = std::dynamic_pointer_cast<Expr>(node)) {
                    visitExpr(expr);
                    // Warn on unused expressions.
                    if(expr->type != BuiltinType::Void) {
                        reportWarning("Unused expression", expr);
                    }
                } else {
                    visit(node);
                }
            }
            declLists.pop_back();
            if(decl->isParameterized()) declLists.pop_back();
            return;
        }

        // We can now assume the decl is stored.
        assert(decl->isStored());

        visitExpr(decl->expression());
        if(decl->type.isInferred()) decl->type.resolveType(*decl->expression()->type);

        if(decl->isParameterized()) declLists.pop_back();
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
void TypeChecker::visitPhysicalTypeRef(std::shared_ptr<PhysicalTypeRef> expr) {}
void TypeChecker::visitIntegerLiteralExpr(std::shared_ptr<IntegerLiteralExpr> expr) {
    expr->type = BuiltinType::i32;
}
void TypeChecker::visitDecimalLiteralExpr(std::shared_ptr<DecimalLiteralExpr> expr) {
    expr->type = BuiltinType::f64;
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
            // Check the names of all parameters
            auto param = decl->paramList.begin();
            auto arg = expr->argList.begin();
            for(;param != decl->paramList.end(); ++param, ++arg) {
                if((*param)->name != arg->name) goto failedToFindMatchInCurrentList;
                if((*param)->type.getType() != arg->value->type) goto failedToFindMatchInCurrentList;
            }
            // We must have succeeded! Add the decl's prototype and type to the declRefExpr and return.
            expr->decl = decl;
            expr->type = decl->type.getType();
            return;

            failedToFindMatchInCurrentList: continue;
        }
    }
    // We must have failed.
    std::string errorMessage = "Invalid reference to identifier '" + expr->name + "'";
    if(!nameMatches.empty()) {
        errorMessage += "\n\nHere are some matches that differ only in parameter labels or types:";
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
