//  Copyright © 2019 Zach Wolfe. All rights reserved.

#pragma once

#include <iostream>

#include "AST/ASTVisitor.h"
#include "General/SourceInfo.h"
#include "General/Diagnostics.h"

class TypeChecker final: public ASTVisitor<TypeChecker> {
    SourceFile const& file;
    Array<Array<Decl*>> declLists;
    Array<StructDecl*> structs;
    Array<Type> returnTypeStack;
    void reportDiag(Diagnostic diag) const {
        diag.print(std::cout);
        if(diag.kind == Diagnostic::Error) {
            exit(1);
        }
    }
    /// Typechecks only the prototype of a declaration. If the declaration is stored
    /// and has no written type annotation, its assigned value also gets typechecked.
    void visitDeclPrototype(Decl* decl);
public:
    TypeChecker(SourceFile const& file) : file(file) {
        declLists.append(Array<Decl*>());
    }

    void visitTopLevel(Array<ASTNode*> nodes);
    void visitType(Type* type, bool dependsOnStructDecls = true);
    // NOTE: visitDecl assumes that visitDeclPrototype was already called.
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
    void visitDoExpr(DoExpr* expr);
};
