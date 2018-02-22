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
                                std::unique_ptr<llvm::Module>,
                                             llvm::Function*,
                                             void,
                                             llvm::Function*,
                                             void,
                                             void,
                                             llvm::Value*>
{
private:
    llvm::LLVMContext context;
    llvm::IRBuilder<> builder;
    std::map<std::string, llvm::Value*> storedNonParameterizedDecls;
public:
    std::unique_ptr<llvm::Module> module;
    CodeGenerator() : builder(context) {
        module = std::make_unique<llvm::Module>("my module", context);
    }

    void visit(ASTNode* node) {
        switch(node->kind) {
            #define AST_NODE(name) case NodeKind::name: \
            visit##name(static_cast<name*>(node)); return;
            #include "AST/ASTNodes.def"
            default: break;
        }
        LLVM_BUILTIN_UNREACHABLE;
    }
    llvm::Function* visitDecl(Decl* decl);
    llvm::Function* visitDeclPrototype(DeclPrototype* prototype);
    void visitScope(Scope* scope);
    void visitParam(Param* param);
    void visitArgument(Argument* argument);
    llvm::Value* visitIntegerLiteralExpr(IntegerLiteralExpr* expr);
    llvm::Value* visitDecimalLiteralExpr(DecimalLiteralExpr* expr);
    llvm::Value* visitDeclRefExpr(DeclRefExpr* expr);
    llvm::Value* visitPlaceholderTypeRefExpr(PlaceholderTypeRefExpr* expr);
};
