//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <map>
#include <memory>
#include <iostream>

#include "AST/ASTVisitor.h"

class TypeChecker final: public ASTVisitor<TypeChecker,
                                           void,
                                           void,
                                           void,
                                           void,
                                           void,
                                           void,
                                           void,
                                           void,
                                           void>
{
private:
    // FIXME: Name lookup will be super slow; figure out how to hash function overloads.
    std::vector<std::vector<Decl*>> declLists;
    void reportError(std::string message, ASTNode* node) {
        std::cout << "TYPE-CHECKING ERROR: " << message << '\n';
        std::cout << "Offending area: " << node->range.getSubstring() << "\n\n";
        // TODO: Support multiple errors per file.
        exit(1);
    }
public:
    TypeChecker() {}

    void visitDecl(Decl* decl);
    void visitDeclPrototype(DeclPrototype* prototype);
    void visitScope(Scope* scope);
    void visitParam(Param* param);
    void visitArgument(Argument* argument);
    void visitPhysicalTypeRef(PhysicalTypeRef* expr);
    void visitIntegerLiteralExpr(IntegerLiteralExpr* expr);
    void visitDecimalLiteralExpr(DecimalLiteralExpr* expr);
    void visitDeclRefExpr(DeclRefExpr* expr);

    void visitReturnStmt(ReturnStmt* stmt);
};
