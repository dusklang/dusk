//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "CodeGenerator.h"
#include "llvm/IR/Verifier.h"

llvm::Type* CodeGenerator::mapBuiltinTypeToLLVM(BuiltinType type) {
    switch(type) {
        case BuiltinType::i32: return llvm::Type::getInt32Ty(context);
        case BuiltinType::f64: return llvm::Type::getDoubleTy(context);
    }
}

llvm::Function* CodeGenerator::visitDecl(Decl* decl) {
    llvm::Function* function = module->getFunction(decl->prototype.name);
    if(!function) function = visitDeclPrototype(&decl->prototype);

    if(!function) return nullptr;

    if(!function->empty()) {
        // TODO: Get the location of the original declaration of the function.
        reportError("Re-declaration of function " + decl->prototype.name, &decl->prototype);
    }

    llvm::BasicBlock* block = llvm::BasicBlock::Create(context, "entry", function);
    builder.SetInsertPoint(block);

    storedNonParameterizedDecls.clear();
    for(auto &arg: function->args()) {
        storedNonParameterizedDecls[arg.getName()] = &arg;
    }
    if(auto expr = decl->expression()) {
        reportError("Code generation for stored decls is not yet supported.", &decl->prototype);
    } else {
        visitScope(decl->body().get());
    }

    llvm::verifyFunction(*function);

    return function;
}

llvm::Function* CodeGenerator::visitDeclPrototype(DeclPrototype* prototype) {
    std::vector<llvm::Type*> arguments;
    for(auto& param: prototype->paramList) {
        arguments.push_back(mapBuiltinTypeToLLVM(param.value.type));
    }
    if(!prototype->type) {
        reportError("Type inference is not yet supported", prototype);
    }
    llvm::FunctionType* functionTy = llvm::FunctionType::get(mapBuiltinTypeToLLVM(prototype->type->type),
                                                             arguments,
                                                             false);
    llvm::Function* function = llvm::Function::Create(functionTy, llvm::Function::ExternalLinkage, prototype->name, module.get());

    int i = 0;
    for(auto &arg: function->args()) {
        arg.setName(prototype->paramList[i++].name);
    }
    return function;
}
void CodeGenerator::visitScope(Scope* scope) {
    for(auto& node: scope->nodes) {
        visit(node.get());
    }
}
void CodeGenerator::visitParam(Param* param) {}
void CodeGenerator::visitArgument(Argument* argument) {}
void CodeGenerator::visitTypeRef(TypeRef* expr) {}
llvm::Value* CodeGenerator::visitIntegerLiteralExpr(IntegerLiteralExpr* expr) {
    return llvm::ConstantInt::get(context, llvm::APInt(32, std::stoi(expr->literal)));
}
llvm::Value* CodeGenerator::visitDecimalLiteralExpr(DecimalLiteralExpr* expr) {
    return llvm::ConstantFP::get(context, llvm::APFloat(std::stod(expr->literal)));
}
llvm::Value* CodeGenerator::visitDeclRefExpr(DeclRefExpr* expr) {
    llvm::Function* callee = module->getFunction(expr->name);
    if(!callee) {
        reportError("Undeclared identifier " + expr->name, expr);
    }

    if(callee->arg_size() != expr->argList.size()) {
        reportError("Incorrect number of arguments passed to function " + expr->name, expr);
    }

    std::vector<llvm::Value*> args;
    for(auto& arg: expr->argList) {
        args.push_back(visitExpr(arg.value.get()));
        if(!args.back()) {
            return nullptr;
        }
    }

    return builder.CreateCall(callee, args, "calltmp");
}

llvm::Value* CodeGenerator::visitReturnStmt(ReturnStmt* stmt) {
    if(!stmt->value) {
        return builder.CreateRetVoid();
    } else {
        return builder.CreateRet(visitExpr(stmt->value.get()));
    }
}
