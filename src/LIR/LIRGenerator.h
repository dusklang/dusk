//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include "LIR.h"
#include "AST/ASTVisitor.h"
#include <string>

class LIRGenerator final: public ASTVisitor<LIRGenerator,
                                            lir::Var,
                                            lir::Var,
                                            lir::Var,
                                            lir::Var,
                                            lir::Var>
{
    lir::Program program;
    lir::Func currentFunction;
public:
    lir::Var visitDecl(Decl* decl);
    lir::Var visitScope(Scope* scope);
    lir::Var visitStructDecl(StructDecl* decl);
    lir::Var visitIntegerLiteralExpr(IntegerLiteralExpr* expr);
    lir::Var visitDecimalLiteralExpr(DecimalLiteralExpr* expr);
    lir::Var visitBooleanLiteralExpr(BooleanLiteralExpr* expr);
    lir::Var visitCharLiteralExpr(CharLiteralExpr* expr);
    lir::Var visitStringLiteralExpr(StringLiteralExpr* expr);
    lir::Var visitPreOpExpr(PreOpExpr* expr);
    lir::Var visitBinOpExpr(BinOpExpr* expr);
    lir::Var visitCastExpr(CastExpr* expr);
    lir::Var visitDeclRefExpr(DeclRefExpr* expr);
    lir::Var visitMemberRefExpr(MemberRefExpr* expr);

    lir::Var visitReturnExpr(ReturnExpr* expr);
    lir::Var visitIfExpr(IfExpr* expr);
    lir::Var visitWhileExpr(WhileExpr* expr);
    lir::Var visitDoExpr(DoExpr* expr);

    lir::Var visitPreOpExprAsLValue(PreOpExpr* expr);
    lir::Var visitDeclRefExprAsLValue(DeclRefExpr* expr);
    lir::Var visitMemberRefExprAsLValue(MemberRefExpr* expr);
    lir::Var visitIntegerLiteralExprAsLValue(IntegerLiteralExpr* expr) {
        panic("Integer literal expression can not be taken as an lvalue");
    }
    lir::Var visitDecimalLiteralExprAsLValue(DecimalLiteralExpr* expr) {
        panic("Decimal literal expression can not be taken as an lvalue");
    }
    lir::Var visitBooleanLiteralExprAsLValue(BooleanLiteralExpr* expr) {
        panic("Boolean literal expression can not be taken as an lvalue");
    }
    lir::Var visitCharLiteralExprAsLValue(CharLiteralExpr* expr) {
        panic("Char literal expression can not be taken as an lvalue");
    }
    lir::Var visitStringLiteralExprAsLValue(StringLiteralExpr* expr) {
        panic("String literal expression can not be taken as an lvalue");
    }
    lir::Var visitBinOpExprAsLValue(BinOpExpr* expr) {
        panic("Binary operator expression can not be taken as an lvalue");
    }
    lir::Var visitCastExprAsLValue(CastExpr* expr) {
        panic("Cast expression can not be taken as an lvalue");
    }
    lir::Var visitReturnExprAsLValue(ReturnExpr* expr) {
        panic("Return expression can not be taken as an lvalue");
    }
    lir::Var visitIfExprAsLValue(IfExpr* expr) {
        panic("If expression can not be taken as an lvalue");
    }
    lir::Var visitWhileExprAsLValue(WhileExpr* expr) {
        panic("While expression can not be taken as an lvalue");
    }
    lir::Var visitDoExprAsLValue(DoExpr* expr) {
        panic("Do expression can not be taken as an lvalue");
    }

    lir::Var visitExprAsLValue(Expr* expr) {
        #define EXPR_NODE(name) if(auto val = dynamic_cast<name##Expr*>(expr)) { \
            return this->visit##name##ExprAsLValue(val); \
        }
        #include "AST/ASTNodes.def"
        unreachable;
    }
    void printIR() const;
};
