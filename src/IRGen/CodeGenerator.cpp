//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "CodeGenerator.hpp"
#include "llvm/IR/Verifier.h"

llvm::Function* CodeGenerator::visitDecl(Decl* decl) {
    llvm::Function* function = module->getFunction(decl->prototype.name);
    if(!function) function = visitDeclPrototype(&decl->prototype);

    if(!function) return nullptr;

    assert(function->empty() && "Function cannot be redefined");

    llvm::BasicBlock* block = llvm::BasicBlock::Create(context, "entry", function);
    builder.SetInsertPoint(block);

    storedNonParameterizedDecls.clear();
    for(auto &arg: function->args()) {
        storedNonParameterizedDecls[arg.getName()] = &arg;
    }

    visitScope(decl->body().get());
    builder.CreateRet(llvm::ConstantInt::get(context, llvm::APInt(32, 0)));

    llvm::verifyFunction(*function);

    return function;
}

llvm::Function* CodeGenerator::visitDeclPrototype(DeclPrototype* prototype) {
    std::vector<llvm::Type*> arguments(prototype->paramList.size(), llvm::Type::getInt32Ty(context));
    llvm::FunctionType* functionTy = llvm::FunctionType::get(llvm::Type::getInt32Ty(context), arguments, false);
    llvm::Function* function = llvm::Function::Create(functionTy, llvm::Function::ExternalLinkage, prototype->name, module.get());

    int i = 0;
    for(auto &arg: function->args()) {
        arg.setName(prototype->paramList[i++].name);
    }
    return function;
}
void CodeGenerator::visitScope(Scope* scope) {
    for(auto node: scope->nodes) {
        visit(node.get());
    }
}
void CodeGenerator::visitParam(Param* param) {}
void CodeGenerator::visitArgument(Argument* argument) {}
llvm::Value* CodeGenerator::visitIntegerLiteralExpr(IntegerLiteralExpr* expr) {
    return llvm::ConstantInt::get(context, llvm::APInt(32, std::stoi(expr->literal)));
}
llvm::Value* CodeGenerator::visitDecimalLiteralExpr(DecimalLiteralExpr* expr) {
    return nullptr;
}
llvm::Value* CodeGenerator::visitDeclRefExpr(DeclRefExpr* expr) {
    return nullptr;
}
llvm::Value* CodeGenerator::visitPlaceholderTypeRefExpr(PlaceholderTypeRefExpr* expr) {
    return nullptr;
}
