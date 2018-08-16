//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include "LIR.h"
#include "AST/ASTVisitor.h"
#include <string>

class LIRGenerator final: public ASTVisitor<LIRGenerator,
                                            void,
                                            lir::Reg,
                                            void,
                                            void,
                                            lir::Reg,
                                            lir::Reg>
{
    std::vector<lir::Stmt*> statements;
    std::map<std::string, lir::Stmt*> entryPoints;

public:
    lir::Reg visitDecl(Decl* decl);
    void visitScope(Scope* scope);
    void visitStructDecl(StructDecl* decl);
    lir::Reg visitIntegerLiteralExpr(IntegerLiteralExpr* expr);
    lir::Reg visitDecimalLiteralExpr(DecimalLiteralExpr* expr);
    lir::Reg visitBooleanLiteralExpr(BooleanLiteralExpr* expr);
    lir::Reg visitCharLiteralExpr(CharLiteralExpr* expr);
    lir::Reg visitStringLiteralExpr(StringLiteralExpr* expr);
    lir::Reg visitPreOpExpr(PreOpExpr* expr);
    lir::Reg visitBinOpExpr(BinOpExpr* expr);
    lir::Reg visitCastExpr(CastExpr* expr);
    lir::Reg visitDeclRefExpr(DeclRefExpr* expr);
    lir::Reg visitMemberRefExpr(MemberRefExpr* expr);

    lir::Reg visitReturnStmt(ReturnStmt* stmt);
    lir::Reg visitIfStmt(IfStmt* stmt);
    lir::Reg visitWhileStmt(WhileStmt* stmt) { return -1; }
    void printIR() const;
};
