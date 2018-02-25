//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <memory>
#include <iostream>

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"

#include "AST/AST.h"
#include "AST/ASTVisitor.h"
#include "AST/Expr.h"
#include "AST/Decl.h"
#include "AST/Stmt.h"

class CodeGenerator final: public ASTVisitor<CodeGenerator,
                                             void,
                                             llvm::Function*,
                                             void,
                                             void,
                                             llvm::Function*,
                                             void,
                                             void,
                                             llvm::Value*,
                                             llvm::Value*>
{
private:
    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;
public:
    llvm::Type* mapBuiltinTypeToLLVM(BuiltinType type);
    std::unique_ptr<llvm::Module> module;
    CodeGenerator() : builder(context) {
        module = std::make_unique<llvm::Module>("my module", context);
    }

    llvm::Function* visitDecl(std::shared_ptr<Decl> decl);
    llvm::Function* visitDeclPrototype(std::shared_ptr<DeclPrototype> prototype);
    void visitScope(std::shared_ptr<Scope> scope);
    void visitParam(std::shared_ptr<Param> param);
    void visitArgument(std::shared_ptr<Argument> argument);
    void visitPhysicalTypeRef(std::shared_ptr<PhysicalTypeRef> expr);
    llvm::Value* visitIntegerLiteralExpr(std::shared_ptr<IntegerLiteralExpr> expr);
    llvm::Value* visitDecimalLiteralExpr(std::shared_ptr<DecimalLiteralExpr> expr);
    llvm::Value* visitDeclRefExpr(std::shared_ptr<DeclRefExpr> expr);

    llvm::Value* visitReturnStmt(std::shared_ptr<ReturnStmt> stmt);
};
