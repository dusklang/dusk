//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <map>
#include <memory>
#include <iostream>

#include "AST/ASTVisitor.h"

class NameResolver final: public ASTVisitor<NameResolver> {
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
    NameResolver() {
        declLists.push_back(std::vector<std::shared_ptr<Decl>>());
    }

    void visitDecl(std::shared_ptr<Decl> decl);
    void visitScope(std::shared_ptr<Scope> scope);
    void visitArgument(std::shared_ptr<Argument> argument) {}
    void visitIntegerLiteralExpr(std::shared_ptr<IntegerLiteralExpr> expr) {}
    void visitDecimalLiteralExpr(std::shared_ptr<DecimalLiteralExpr> expr) {}
    void visitBooleanLiteralExpr(std::shared_ptr<BooleanLiteralExpr> expr) {}
    void visitCharLiteralExpr(std::shared_ptr<CharLiteralExpr> expr) {}
    void visitStringLiteralExpr(std::shared_ptr<StringLiteralExpr> expr) {}
    void visitDeclRefExpr(std::shared_ptr<DeclRefExpr> expr);

    void visitReturnStmt(std::shared_ptr<ReturnStmt> stmt);
    void visitAssignmentStmt(std::shared_ptr<AssignmentStmt> stmt);
    void visitIfStmt(std::shared_ptr<IfStmt> stmt);
    void visitWhileStmt(std::shared_ptr<WhileStmt> stmt);
};
