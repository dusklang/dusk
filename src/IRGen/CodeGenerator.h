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
    std::map<std::string, llvm::Value*> storedNonParameterizedDecls;
public:
    llvm::Type* mapBuiltinTypeToLLVM(BuiltinType type);
    std::unique_ptr<llvm::Module> module;
    CodeGenerator() : builder(context) {
        module = std::make_unique<llvm::Module>("my module", context);
    }

    llvm::Function* visitDecl(Decl* decl);
    llvm::Function* visitDeclPrototype(DeclPrototype* prototype);
    void visitScope(Scope* scope);
    void visitParam(Param* param);
    void visitArgument(Argument* argument);
    void visitPhysicalTypeRef(PhysicalTypeRef* expr);
    llvm::Value* visitIntegerLiteralExpr(IntegerLiteralExpr* expr);
    llvm::Value* visitDecimalLiteralExpr(DecimalLiteralExpr* expr);
    llvm::Value* visitDeclRefExpr(DeclRefExpr* expr);

    llvm::Value* visitReturnStmt(ReturnStmt* stmt);
};
