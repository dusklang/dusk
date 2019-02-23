#pragma once

#include <iostream>
#include <variant>
#include <unordered_map>

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"

#include "AST/AST.h"
#include "AST/ASTVisitor.h"
#include "AST/Expr.h"
#include "AST/Decl.h"

struct DirectVal final {
    llvm::Value* val;
};
struct IndirectVal final {
    llvm::Value* val;
};
/// DirectVal means the underlying llvm value is of the same llvm type you'd get if you passed the type of the expression it
/// was derived from to `toLLVMTy`.
/// IndirectVal means the underlying llvm value is one level of indirection removed from its direct val.
///
/// Consider the following code sample:
///     var foo = 5
///     foo += 10
/// During code generation the following things (among others) will occur:
///  - `visitBinOpExpr` will be called on the second line
///  - `visitBinOpExpr` will call `visitDeclRefExpr(foo)` (which will return an IndirectVal), then
///    `visitIntegerLiteralExpr(10)` (which will return a DirectVal)
///  - `visitBinOpExpr` needs direct values for both of its operands, so it calls `toDirect` on both
///  - Since `foo` is an indirect value, `toDirect` will trigger a load of its value from memory
///  - Since `10` is a direct value, `toDirect` doesn't need to do anything special
///  - `visitBinOpExpr` will also call `toIndirect` on its left operand so that it can store the result
///    of the addition back in memory
///  - Since `foo` is already an indirect value, `toIndirect` doesn't need to do anything special
using CodeGenVal = std::variant<DirectVal, IndirectVal>;

class LLVMGenerator final: public ASTVisitor<LLVMGenerator,
                                             CodeGenVal,
                                             CodeGenVal,
                                             CodeGenVal,
                                             CodeGenVal,
                                             CodeGenVal>
{
    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;
    std::unordered_map<Decl*, llvm::Value*> declVals;
    Array<llvm::Function*> functionStack;
    llvm::Type* toLLVMTy(Type type);
    /// If `val` is direct, returns its underlying value unchanged.
    /// If `val` is indirect, generates a load instruction to get at the direct value.
    llvm::Value* toDirect(CodeGenVal val);
    /// If `val` is indirect, returns its underlying value unchanged.
    /// If `val` is direct, generates code to copy the direct value on the stack and returns
    /// a pointer to that copy.
    llvm::Value* toIndirect(CodeGenVal val);

    /// Creates an entry in `declVals`, but doesn't visit the scope of a computed declaration
    /// or the expression of a stored one.
    void visitDeclPrototype(Decl* decl);

    template<typename HasValue, typename HasNoValue>
    void handleTerminal(Scope* scope, HasValue hasValue, HasNoValue hasNoValue);
public:
    llvm::Module* module;
    LLVMGenerator() : builder(context) {
        module = new llvm::Module("my module", context);
    }

    void visitTopLevel(Array<ASTNode*> nodes);
    /// Generates code for the scope of a computed declaration, or for the expression of a stored one.
    /// NOTE: `visitDeclPrototype` must be called on a declaration before this.
    CodeGenVal visitDecl(Decl* decl);
    CodeGenVal visitScope(Scope* scope);
    CodeGenVal visitStructDecl(StructDecl* decl);
    CodeGenVal visitIntegerLiteralExpr(IntegerLiteralExpr* expr);
    CodeGenVal visitDecimalLiteralExpr(DecimalLiteralExpr* expr);
    CodeGenVal visitBooleanLiteralExpr(BooleanLiteralExpr* expr);
    CodeGenVal visitCharLiteralExpr(CharLiteralExpr* expr);
    CodeGenVal visitStringLiteralExpr(StringLiteralExpr* expr);
    CodeGenVal visitPreOpExpr(PreOpExpr* expr);
    CodeGenVal visitBinOpExpr(BinOpExpr* expr);
    CodeGenVal visitCastExpr(CastExpr* expr);
    CodeGenVal visitDeclRefExpr(DeclRefExpr* expr);
    CodeGenVal visitMemberRefExpr(MemberRefExpr* expr);

    CodeGenVal visitReturnExpr(ReturnExpr* expr);
    CodeGenVal visitIfExpr(IfExpr* expr);
    CodeGenVal visitWhileExpr(WhileExpr* expr);
    CodeGenVal visitDoExpr(DoExpr* expr);

    void outputObjectFile(char const* fileName) const;
    void printIR() const;
};
