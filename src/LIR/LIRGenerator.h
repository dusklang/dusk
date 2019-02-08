//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include "LIR.h"
#include "AST/ASTVisitor.h"
#include <string>

class LIRGenerator final: public ASTVisitor<LIRGenerator,
                                            lir::ROperand,
                                            lir::ROperand,
                                            lir::ROperand,
                                            lir::ROperand,
                                            lir::ROperand>
{
    lir::Program program;
    lir::Func currentFunction;
public:
    lir::ROperand visitDecl(Decl* decl);
    lir::ROperand visitScope(Scope* scope);
    lir::ROperand visitStructDecl(StructDecl* decl);
    lir::ROperand visitIntegerLiteralExpr(IntegerLiteralExpr* expr);
    lir::ROperand visitDecimalLiteralExpr(DecimalLiteralExpr* expr);
    lir::ROperand visitBooleanLiteralExpr(BooleanLiteralExpr* expr);
    lir::ROperand visitCharLiteralExpr(CharLiteralExpr* expr);
    lir::ROperand visitStringLiteralExpr(StringLiteralExpr* expr);
    lir::ROperand visitPreOpExpr(PreOpExpr* expr);
    lir::ROperand visitBinOpExpr(BinOpExpr* expr);
    lir::ROperand visitCastExpr(CastExpr* expr);
    lir::ROperand visitDeclRefExpr(DeclRefExpr* expr);
    lir::ROperand visitMemberRefExpr(MemberRefExpr* expr);

    lir::ROperand visitReturnExpr(ReturnExpr* expr);
    lir::ROperand visitIfExpr(IfExpr* expr);
    lir::ROperand visitWhileExpr(WhileExpr* expr);
    lir::ROperand visitDoExpr(DoExpr* expr);

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
