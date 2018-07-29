//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <iostream>

#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/IRBuilder.h"

#include "AST/AST.h"
#include "AST/ASTVisitor.h"
#include "AST/Expr.h"
#include "AST/Decl.h"
#include "AST/Stmt.h"

llvm::Type* mapTypeToLLVM(llvm::LLVMContext& context, Type type);

class CodeGenerator final: public ASTVisitor<CodeGenerator,
                                             void,
                                             llvm::Value*,
                                             void,
                                             void,
                                             llvm::Value*,
                                             llvm::Value*>
{
private:
    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;
public:
    llvm::Module* module;
    CodeGenerator() : builder(context) {
        module = new llvm::Module("my module", context);
    }
    ~CodeGenerator() { delete module; }

    llvm::Value* visitDecl(Decl* decl);
    void visitScope(Scope* scope);
    void visitArgument(Argument* argument);
    llvm::Value* visitIntegerLiteralExpr(IntegerLiteralExpr* expr);
    llvm::Value* visitDecimalLiteralExpr(DecimalLiteralExpr* expr);
    llvm::Value* visitBooleanLiteralExpr(BooleanLiteralExpr* expr);
    llvm::Value* visitCharLiteralExpr(CharLiteralExpr* expr);
    llvm::Value* visitStringLiteralExpr(StringLiteralExpr* expr);
    llvm::Value* visitDeclRefExpr(DeclRefExpr* expr);

    llvm::Value* visitReturnStmt(ReturnStmt* stmt);
    llvm::Value* visitAssignmentStmt(AssignmentStmt* stmt);
    llvm::Value* visitIfStmt(IfStmt* stmt);
    llvm::Value* visitWhileStmt(WhileStmt* stmt) { return nullptr; }
};
