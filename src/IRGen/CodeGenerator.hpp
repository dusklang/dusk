//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <memory>
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"

#include "AST/AST.hpp"
#include "AST/ASTVisitor.hpp"
#include "AST/Expr.hpp"
#include "AST/Decl.hpp"

class CodeGenerator final: public ASTVisitor<CodeGenerator,
                                             void,
                                             void,
                                             void,
                                             void,
                                             void,
                                             void,
                                             void,
                                             llvm::Value*>
{
private:
    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;
    std::unique_ptr<llvm::Module> module;
public:
    CodeGenerator() : builder(context) {}

    void visitDecl(Decl* decl, int indentationLevel);
    void visitDeclPrototype(DeclPrototype* prototype, int indentationLevel);
    void visitScope(Scope* scope, int indentationLevel);
    void visitParam(Param* param, int indentationLevel);
    void visitArgument(Argument* argument, int indentationLevel);
    llvm::Value* visitIntegerLiteralExpr(IntegerLiteralExpr* expr);
    llvm::Value* visitDecimalLiteralExpr(DecimalLiteralExpr* expr);
    llvm::Value* visitDeclRefExpr(DeclRefExpr* expr);
    llvm::Value* visitPlaceholderTypeRefExpr(PlaceholderTypeRefExpr* expr);
};
