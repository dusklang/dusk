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
                                             llvm::Value*,
                                             void,
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

    llvm::Value* visitDecl(std::shared_ptr<Decl> decl);
    void visitScope(std::shared_ptr<Scope> scope);
    void visitArgument(std::shared_ptr<Argument> argument);
    void visitPhysicalTypeRef(std::shared_ptr<PhysicalTypeRef> expr);
    llvm::Value* visitIntegerLiteralExpr(std::shared_ptr<IntegerLiteralExpr> expr);
    llvm::Value* visitDecimalLiteralExpr(std::shared_ptr<DecimalLiteralExpr> expr);
    llvm::Value* visitBooleanLiteralExpr(std::shared_ptr<BooleanLiteralExpr> expr);
    llvm::Value* visitDeclRefExpr(std::shared_ptr<DeclRefExpr> expr);

    llvm::Value* visitReturnStmt(std::shared_ptr<ReturnStmt> stmt);
    llvm::Value* visitAssignmentStmt(std::shared_ptr<AssignmentStmt> stmt);
    llvm::Value* visitIfStmt(std::shared_ptr<IfStmt> stmt);
    llvm::Value* visitWhileStmt(std::shared_ptr<WhileStmt> stmt) { return nullptr; }
};
