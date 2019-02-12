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
        lir::MemoryLoc copy;
        lir::Operand* read;
        lir::MemoryLoc* write;
    };

    static ResultContext DontCare() {
        return ResultContext { RCKind::DontCare };
    }
    static ResultContext Return() {
        return ResultContext { RCKind::Return };
    }
    static ResultContext Copy(lir::MemoryLoc copy) {
        ResultContext ctx { RCKind::Copy };
        ctx.copy = copy;
        return ctx;
    }
    static ResultContext Read(lir::Operand* read) {
        ResultContext ctx { RCKind::Read };
        ctx.read = read;
        return ctx;
    }
    static ResultContext Write(lir::MemoryLoc* write) {
        ResultContext ctx { RCKind::Write };
        ctx.write = write;
        return ctx;
    }
};

struct InsertionState {
    lir::Func function;
    lir::BB basicBlock;
};

using DeclVal = std::variant<lir::MemoryLoc, lir::Func>;
class LIRGenerator final: public ASTVisitor<LIRGenerator,
                                            void,
                                            DeclVal,
                                            void,
                                            void,
                                            void>
{
    lir::Program program;
    Array<InsertionState> insertionState;
    std::unordered_map<Decl*, DeclVal> declMap;

    lir::Function& currentFunction();
    lir::BasicBlock& currentBasicBlock();
    void setBasicBlock(lir::BB bb);
    lir::BB createBasicBlock();
    lir::Operand U64Constant(uint64_t constant);
    lir::Operand U32Constant(uint32_t constant);
    lir::Operand U16Constant(uint16_t constant);
    lir::Operand U8Constant(uint16_t constant);
    lir::Operand globalStringConstant(char const* data, uint64_t size);

    lir::MemoryLoc variable(Type& type);
    lir::MemoryLoc global(lir::Value initialValue);
    lir::MemoryLoc externGlobal(std::string name, Type& type);

    lir::Func function(std::string name, Type& returnType, bool isExtern);
    lir::Func beginFunction(std::string name, Type& returnType, bool isExtern);
    void endFunction();

    void twoAddressCode(lir::OpCode op, lir::MemoryLoc dest, lir::Operand operand, Type& meaningfulType);
    void threeAddressCode(lir::OpCode op, lir::MemoryLoc dest, lir::Operand operandA, lir::Operand operandB, Type& meaningfulType);
    void zeroExtend(lir::MemoryLoc dest, Type& destType, lir::Operand operand, Type& operandType);
    void signExtend(lir::MemoryLoc dest, Type& destType, lir::Operand operand, Type& operandType);
    void branch(lir::BB branch);
    void condBranch(lir::Operand condition, lir::BB trueBranch, lir::BB falseBranch);
    void call(lir::Func function, Array<lir::Argument> arguments);
    void returnValue(lir::Operand operand, Type& type);
    void returnVoid();
    void unreachableInstr();
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
