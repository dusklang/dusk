//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "TypeChecker.h"

void TypeChecker::visitDecl(Decl* decl) {
    // Insert prototype into the current scope.
    #warning Overhaul ASTVisitor using smart pointers, and remove this pointer creation:
    declLists.back().push_back(AbstractDecl(std::shared_ptr<Decl>(decl)));

    // If we have parameters, start a new scope for referencing them.
    if(decl->isParameterized()) {
        declLists.push_back(std::vector<AbstractDecl>());
        for(auto& param: decl->prototype->paramList) {
            declLists.back().push_back(AbstractDecl(param));
        }
    }
    if(decl->isComputed()) {
        // Start another new, inner scope for declarations inside the body of the computed declaration.
        declLists.push_back(std::vector<AbstractDecl>());
        // Add Void type to computed declaration if it doesn't have one.
        if(!decl->prototype->physicalType) decl->getTypeRef()->resolveType(BuiltinType::Void);

        for(auto& node: decl->body()->nodes) {
            // Handle return statements.
            if(auto ret = std::dynamic_pointer_cast<ReturnStmt>(node)) {
                visitReturnStmt(ret.get());
                // Handle returning value in a void computed decl.
                if(decl->getTypeRef()->getType() == BuiltinType::Void) {
                    if(ret->value) {
                        bool wasInferred = (bool)!decl->prototype->physicalType;
                        reportError("Attempted to return value from " +
                                    std::string(wasInferred ? "inferred-" : "") +
                                    "Void computed decl '" + decl->prototype->name + "'",
                                    ret.get());
                    } else {
                        continue;
                    }
                }

                // Handle returning no value in a non-void computed decl.
                if(!ret->value) {
                    reportError("Computed decl '" + decl->prototype->name + "' must return a value",
                                ret.get());
                }

                // Handle returning a value of a type incompatible with the computed decl's type
                // (currently, "incompatible with" just means "not equal to").
                if(*ret->value->type != decl->getTypeRef()->getType()) {
                    // TODO: Include in the error message the type of the returned expr.
                    reportError("Attempted to return value of incompatible type from computed decl "
                                "of type " + decl->prototype->physicalType->range.getSubstring(),
                                ret.get());
                }
            }
            // Handle expressions.
            else if(auto expr = std::dynamic_pointer_cast<Expr>(node)) {
                visitExpr(expr.get());
                // Warn on unused expressions.
                if(expr->type != BuiltinType::Void) {
                    reportWarning("Unused expression", expr.get());
                }
            }
            else {
                visit(node.get());
            }
        }
        declLists.pop_back();
        if(decl->isParameterized()) declLists.pop_back();
        return;
    }

    // We can now assume the decl is stored.
    assert(decl->isStored());

    visitExpr(decl->expression().get());
    if(!decl->prototype->physicalType) decl->getTypeRef()->resolveType(*decl->expression()->type);

    if(decl->isParameterized()) declLists.pop_back();
}
void TypeChecker::visitDeclPrototype(DeclPrototype* prototype) {
    // THIS SHOULD ONLY EVER BY INVOKED IN THE CASE OF A STANDALONE PROTOTYPE, aka NOT in visitDecl().

    if(!prototype->isExtern) {
        reportError("Non-extern decl prototypes not yet supported", prototype);
    }

    if(!prototype->physicalType) {
        reportError("Standalone decl prototypes need types", prototype);
    }
}
void TypeChecker::visitScope(Scope* scope) {}
void TypeChecker::visitParam(Param* param) {}
void TypeChecker::visitArgument(Argument* argument) {}
void TypeChecker::visitPhysicalTypeRef(PhysicalTypeRef* expr) {}
void TypeChecker::visitIntegerLiteralExpr(IntegerLiteralExpr* expr) {
    expr->type = BuiltinType::i32;
}
void TypeChecker::visitDecimalLiteralExpr(DecimalLiteralExpr* expr) {
    expr->type = BuiltinType::f64;
}
void TypeChecker::visitDeclRefExpr(DeclRefExpr* expr) {
    // Type-check arguments.
    for(auto& arg: expr->argList) {
        visitExpr(arg.value.get());
    }

    // Find the prototype to reference.
    //
    // TODO: Setup a dependency system to allow decls to be declared after they are referenced.
    std::vector<AbstractDecl> nameMatches;
    for(auto it = declLists.rbegin(); it != declLists.rend(); ++it) {
        auto& declList = *it;
        for(auto& decl: declList) {
            if(decl.name() != expr->name) continue;
            nameMatches.push_back(decl);
            if(decl.paramList().size() != expr->argList.size()) continue;
            // Check the names of all parameters
            auto param = decl.paramList().begin();
            auto arg = expr->argList.begin();
            for(;param != decl.paramList().end(); ++param, ++arg) {
                if((*param)->name != arg->name) goto failedToFindMatchInCurrentList;
                if((*param)->value.type != arg->value->type) goto failedToFindMatchInCurrentList;
            }
            // We must have succeeded! Add the decl's prototype and type to the declRefExpr and return.
            expr->decl = AbstractDecl(decl);
            expr->type = decl.typeRef().getType();
            return;

            failedToFindMatchInCurrentList: continue;
        }
    }
    // We must have failed.
    std::string errorMessage = "Invalid reference to identifier '" + expr->name + "'";
    if(!nameMatches.empty()) {
        errorMessage += "\n\nHere are some matches that differ only in parameter labels or types:";
        for(auto& match: nameMatches) {
            errorMessage += "\n\t" + match.range().getSubstring();
        }
    }
    reportError(errorMessage, expr);
}

void TypeChecker::visitReturnStmt(ReturnStmt* stmt) {
    if(stmt->value) {
        visitExpr(stmt->value.get());
    }
}
