//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <iostream>
#include <stack>

#include "AST/ASTVisitor.h"

class TypeChecker final: public ASTVisitor<TypeChecker> {
private:
    std::vector<std::vector<Decl*>> declLists;
    std::stack<Type> returnTypeStack;
    template<typename Node>
    void reportError(std::string message, Node* node) {
        std::cout << "TYPE-CHECKING ERROR: " << message << '\n';
        std::cout << "Offending area: " << node->range.getSubstring() << "\n\n";
        // TODO: Support multiple errors per file.
        exit(1);
    }

    template<typename Node>
    void reportWarning(std::string message, Node* node) {
        std::cout << "TYPE-CHECKING WARNING: " << message << '\n';
        std::cout << "Offending area: " << node->range.getSubstring() << "\n\n";
    }
public:
    TypeChecker() {
        declLists.push_back(std::vector<Decl*>());
    }

    void visitDecl(Decl* decl);
    void visitStructDecl(StructDecl* decl);
    void visitScope(Scope* scope);
    void visitIntegerLiteralExpr(IntegerLiteralExpr* expr);
    void visitDecimalLiteralExpr(DecimalLiteralExpr* expr);
    void visitBooleanLiteralExpr(BooleanLiteralExpr* expr);
    void visitCharLiteralExpr(CharLiteralExpr* expr);
    void visitStringLiteralExpr(StringLiteralExpr* expr);
    void visitPreOpExpr(PreOpExpr* expr);
    void visitBinOpExpr(BinOpExpr* expr);
    void visitCastExpr(CastExpr* expr);
    void visitDeclRefExpr(DeclRefExpr* expr);

    void visitReturnStmt(ReturnStmt* stmt);
    void visitIfStmt(IfStmt* stmt);
    void visitWhileStmt(WhileStmt* stmt);
};
