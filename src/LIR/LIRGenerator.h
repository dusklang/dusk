//  Copyright © 2019 Zach Wolfe. All rights reserved.

#pragma once

#include "LIR.h"
#include "AST/ASTVisitor.h"
#include <string>
#include <unordered_map>
#include <variant>

enum class RCKind {
    /// We don't care about the result of this expression.
    DontCare,
    /// We need the result of this expression to be put in a specific location.
    Copy,
    /// We need to read the result of this expression.
    Read,
    /// We need be able to read and modify the result of this expression.
    Write
};

/// What we want to do with the result of visiting an expression.
struct ResultContext {
    RCKind kind;
    union {
        lir::RWOperand copy;
        lir::ROperand* read;
        lir::RWOperand* write;
    };

    static ResultContext DontCare() {
        return ResultContext { RCKind::DontCare };
    }
    static ResultContext Copy(lir::RWOperand copy) {
        ResultContext ctx { RCKind::Copy };
        ctx.copy = copy;
        return ctx;
    }
    static ResultContext Read(lir::ROperand* read) {
        ResultContext ctx { RCKind::Read };
        ctx.read = read;
        return ctx;
    }
    static ResultContext Write(lir::RWOperand* write) {
        ResultContext ctx { RCKind::Write };
        ctx.write = write;
        return ctx;
    }
};

using DeclVal = std::variant<lir::Var, lir::Func>;
class LIRGenerator final: public ASTVisitor<LIRGenerator,
                                            void,
                                            DeclVal,
                                            std::optional<lir::ROperand>,
                                            void,
                                            lir::ROperand>
{
    lir::Program program;
    Array<lir::Func> functionStack;
    std::unordered_map<Decl*, DeclVal> declMap;

    lir::ROperand variableOperand(lir::Var variable);
    lir::ROperand variableOperand(lir::RWOperand operand);
    lir::RWOperand mutableVariableOperand(lir::Var variable);
    lir::ROperand localConstantOperand(lir::Value value);
    lir::ROperand globalConstantOperand(lir::Const constant);
public:
    void visit(Array<ASTNode*> const& nodes);
    DeclVal visitDecl(Decl* decl);
    std::optional<lir::ROperand> visitScope(Scope* scope);
    void visitStructDecl(StructDecl* decl) {}
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

    lir::RWOperand visitPreOpExprAsLValue(PreOpExpr* expr);
    lir::RWOperand visitDeclRefExprAsLValue(DeclRefExpr* expr);
    lir::RWOperand visitMemberRefExprAsLValue(MemberRefExpr* expr);
    lir::RWOperand visitIntegerLiteralExprAsLValue(IntegerLiteralExpr* expr) {
        panic("Integer literal expression can not be taken as an lvalue");
    }
    lir::RWOperand visitDecimalLiteralExprAsLValue(DecimalLiteralExpr* expr) {
        panic("Decimal literal expression can not be taken as an lvalue");
    }
    lir::RWOperand visitBooleanLiteralExprAsLValue(BooleanLiteralExpr* expr) {
        panic("Boolean literal expression can not be taken as an lvalue");
    }
    lir::RWOperand visitCharLiteralExprAsLValue(CharLiteralExpr* expr) {
        panic("Char literal expression can not be taken as an lvalue");
    }
    lir::RWOperand visitStringLiteralExprAsLValue(StringLiteralExpr* expr) {
        panic("String literal expression can not be taken as an lvalue");
    }
    lir::RWOperand visitBinOpExprAsLValue(BinOpExpr* expr) {
        panic("Binary operator expression can not be taken as an lvalue");
    }
    lir::RWOperand visitCastExprAsLValue(CastExpr* expr) {
        panic("Cast expression can not be taken as an lvalue");
    }
    lir::RWOperand visitReturnExprAsLValue(ReturnExpr* expr) {
        panic("Return expression can not be taken as an lvalue");
    }
    lir::RWOperand visitIfExprAsLValue(IfExpr* expr) {
        panic("If expression can not be taken as an lvalue");
    }
    lir::RWOperand visitWhileExprAsLValue(WhileExpr* expr) {
        panic("While expression can not be taken as an lvalue");
    }
    lir::RWOperand visitDoExprAsLValue(DoExpr* expr) {
        panic("Do expression can not be taken as an lvalue");
    }
    lir::RWOperand visitExprAsLValue(Expr* expr) {
        #define EXPR_NODE(name) if(auto val = dynamic_cast<name##Expr*>(expr)) { \
            return this->visit##name##ExprAsLValue(val); \
        }
        #include "AST/ASTNodes.def"
        unreachable;
    }
    void printIR() const;
};
