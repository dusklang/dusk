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
    /// We need to return the result of this expression from the current function.
    Return,
    /// We need the result of this expression to be put in a specific location.
    Copy,
    /// We need to read the result of this expression.
    Read,
    /// We need be able to read and modify the result of this expression.
    Write,
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
    static ResultContext Return() {
        return ResultContext { RCKind::Return };
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
                                            void,
                                            void,
                                            void>
{
    lir::Program program;
    Array<lir::Func> functionStack;
    std::unordered_map<Decl*, DeclVal> declMap;

    lir::ROperand variableOperand(lir::Var variable);
    lir::ROperand variableOperand(lir::RWOperand operand);
    lir::RWOperand mutableVariableOperand(lir::Var variable);
    lir::ROperand localConstantOperand(lir::Value value);
    lir::ROperand globalConstantOperand(lir::Const constant);

    void placeConstant(lir::ROperand operand, ResultContext ctx);
public:
    void visit(Array<ASTNode*> const& nodes);
    DeclVal visitDecl(Decl* decl);
    void visitScope(Scope* scope, ResultContext ctx);
    void visitStructDecl(StructDecl* decl) {}
    void visitIntegerLiteralExpr(IntegerLiteralExpr* expr, ResultContext ctx);
    void visitDecimalLiteralExpr(DecimalLiteralExpr* expr, ResultContext ctx);
    void visitBooleanLiteralExpr(BooleanLiteralExpr* expr, ResultContext ctx);
    void visitCharLiteralExpr(CharLiteralExpr* expr, ResultContext ctx);
    void visitStringLiteralExpr(StringLiteralExpr* expr, ResultContext ctx);
    void visitPreOpExpr(PreOpExpr* expr, ResultContext ctx);
    void visitBinOpExpr(BinOpExpr* expr, ResultContext ctx);
    void visitCastExpr(CastExpr* expr, ResultContext ctx);
    void visitDeclRefExpr(DeclRefExpr* expr, ResultContext ctx);
    void visitMemberRefExpr(MemberRefExpr* expr, ResultContext ctx);
    void visitReturnExpr(ReturnExpr* expr, ResultContext ctx);
    void visitIfExpr(IfExpr* expr, ResultContext ctx);
    void visitWhileExpr(WhileExpr* expr, ResultContext ctx);
    void visitDoExpr(DoExpr* expr, ResultContext ctx);

    void visitExpr(Expr* expr, ResultContext ctx = ResultContext::DontCare()) {
        #define EXPR_NODE(name) if(auto val = dynamic_cast<name##Expr*>(expr)) { \
            return visit##name##Expr(val, ctx); \
        }
        #include "AST/ASTNodes.def"
        unreachable;
    }
    void printIR() const;
};
