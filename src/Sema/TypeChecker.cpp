//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "TypeChecker.h"

void TypeChecker::visitDecl(Decl* decl) {
    if(declLists.empty()) declLists.push_back(std::vector<Decl*>());
    declLists.back().push_back(decl);

    if(decl->isComputed()) {
        // Add void type to computed declaration if it doesn't have one.
        if(!decl->prototype->physicalType) decl->getTypeRef()->resolveType(BuiltinType::Void);

        for(auto& node: decl->body()->nodes) {
            visit(node.get());
        }
        return;
    }

    declLists.pop_back();

    // We can now assume the decl is stored.
    assert(!decl->isStored());

    visitExpr(decl->expression().get());
    if(!decl->prototype->physicalType) decl->getTypeRef()->resolveType(*decl->expression()->type);
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
    // Type-check return type
    std::vector<Decl*> nameMatches;
    for(auto it = declLists.rbegin(); it != declLists.rend(); ++it) {
        auto& declList = *it;
        for(auto& decl: declList) {
            if(decl->prototype->name != expr->name) break;
            nameMatches.push_back(decl);
            if(decl->prototype->paramList.size() != expr->argList.size()) break;
            // Check the names of all parameters
            auto param = decl->prototype->paramList.begin();
            auto arg = expr->argList.begin();
            for(;param != decl->prototype->paramList.end(); ++param, ++arg) {
                if(param->name != arg->name) goto failedToFindMatchInCurrentList;
                if(param->value.type != arg->value->type) goto failedToFindMatchInCurrentList;
            }
            // We must have succeeded! Add the decl's prototype and type to the declRefExpr and return.
            expr->prototype = decl->prototype;
            expr->type = decl->getTypeRef()->getType();
            return;
        }
        failedToFindMatchInCurrentList: break;
    }
    // We must have failed.
    std::string errorMessage = "Reference to undeclared identifier '" + expr->name + "'";
    if(!nameMatches.empty()) {
        errorMessage += "\n\nHere are some matches that differ only in parameter labels or types:";
        for(auto& match: nameMatches) {
            errorMessage += "\n\t" + match->prototype->range.getSubstring();
        }
    }
    reportError(errorMessage, expr);
}

void TypeChecker::visitReturnStmt(ReturnStmt* stmt) {
    if(stmt->value) {
        visitExpr(stmt->value.get());
    }
}
