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
                                           void>
{
private:
    // FIXME: Name lookup will be super slow; figure out how to hash function overloads.
    std::vector<std::vector<std::shared_ptr<Decl>>> declLists;
    template<typename Node>
    void reportError(std::string message, std::shared_ptr<Node> node) {
        std::cout << "TYPE-CHECKING ERROR: " << message << '\n';
        std::cout << "Offending area: " << node->range.getSubstring() << "\n\n";
        // TODO: Support multiple errors per file.
        exit(1);
    }

    template<typename Node>
    void reportWarning(std::string message, std::shared_ptr<Node> node) {
        std::cout << "TYPE-CHECKING WARNING: " << message << '\n';
        std::cout << "Offending area: " << node->range.getSubstring() << "\n\n";
    }
public:
    TypeChecker() {
        declLists.push_back(std::vector<std::shared_ptr<Decl>>());
    }

    void visitDecl(std::shared_ptr<Decl> decl);
    void visitScope(std::shared_ptr<Scope> scope);
    void visitArgument(std::shared_ptr<Argument> argument);
    void visitPhysicalTypeRef(std::shared_ptr<PhysicalTypeRef> expr);
    void visitIntegerLiteralExpr(std::shared_ptr<IntegerLiteralExpr> expr);
    void visitDecimalLiteralExpr(std::shared_ptr<DecimalLiteralExpr> expr);
    void visitDeclRefExpr(std::shared_ptr<DeclRefExpr> expr);

    void visitReturnStmt(std::shared_ptr<ReturnStmt> stmt);
};
