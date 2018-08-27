//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include "LIR.h"
#include "AST/ASTVisitor.h"
#include <string>

class LIRGenerator final: public ASTVisitor<LIRGenerator,
                                            lir::Value*,
                                            lir::Value*,
                                            lir::Value*,
                                            lir::Value*,
                                            lir::Value*>
{
public:
    lir::Value* visitDecl(Decl* decl);
    lir::Value* visitScope(Scope* scope);
    lir::Value* visitStructDecl(StructDecl* decl);
    lir::Value* visitIntegerLiteralExpr(IntegerLiteralExpr* expr);
    lir::Value* visitDecimalLiteralExpr(DecimalLiteralExpr* expr);
    lir::Value* visitBooleanLiteralExpr(BooleanLiteralExpr* expr);
    lir::Value* visitCharLiteralExpr(CharLiteralExpr* expr);
    lir::Value* visitStringLiteralExpr(StringLiteralExpr* expr);
    lir::Value* visitPreOpExpr(PreOpExpr* expr);
    lir::Value* visitBinOpExpr(BinOpExpr* expr);
    lir::Value* visitCastExpr(CastExpr* expr);
    lir::Value* visitDeclRefExpr(DeclRefExpr* expr);
    lir::Value* visitMemberRefExpr(MemberRefExpr* expr);

    lir::Value* visitReturnExpr(ReturnExpr* expr);
    lir::Value* visitIfExpr(IfExpr* expr);
    lir::Value* visitWhileExpr(WhileExpr* expr);
    void printIR() const;
};
