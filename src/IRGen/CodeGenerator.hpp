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
    std::string visitDeclPrototype(DeclPrototype* prototype, int indentationLevel);
    std::string visitScope(Scope* scope, int indentationLevel);
    std::string visitParam(Param* param, int indentationLevel);
    std::string visitArgument(Argument* argument, int indentationLevel);
    std::string visitIntegerLiteralExpr(IntegerLiteralExpr* expr, int indentationLevel);
    std::string visitDecimalLiteralExpr(DecimalLiteralExpr* expr, int indentationLevel);
    std::string visitDeclRefExpr(DeclRefExpr* expr, int indentationLevel);
    std::string visitTypeRefExpr(TypeRefExpr* expr, int indentationLevel);
};
