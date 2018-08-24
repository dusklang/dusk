//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <iostream>
#include <stack>

#include "AST/ASTVisitor.h"
#include "General/SourceInfo.h"
#include "General/Diagnostics.h"

class TypeChecker final: public ASTVisitor<TypeChecker> {
    SourceFile const& file;
    std::vector<std::vector<Decl*>> declLists;
    std::vector<StructDecl*> structs;
    std::stack<Type> returnTypeStack;
    void reportDiag(Diagnostic diag) const {
        diag.print(std::cout);
        if(diag.kind == Diagnostic::Error) {
            exit(1);
        }
    }
public:
    TypeChecker(SourceFile const& file) : file(file) {
        declLists.push_back(std::vector<Decl*>());
    }

    void visitType(Type* type);
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
    void visitMemberRefExpr(MemberRefExpr* expr);

    void visitReturnExpr(ReturnExpr* expr);
    void visitIfExpr(IfExpr* expr);
    void visitWhileExpr(WhileExpr* expr);
};
