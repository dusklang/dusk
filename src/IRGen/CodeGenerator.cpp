//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "CodeGenerator.h"
#include "llvm/IR/Verifier.h"

llvm::Type* CodeGenerator::mapBuiltinTypeToLLVM(BuiltinType type) {
    switch(type) {
        case BuiltinType::i32: return llvm::Type::getInt32Ty(context);
        case BuiltinType::f64: return llvm::Type::getDoubleTy(context);
        case BuiltinType::Void: return llvm::Type::getVoidTy(context);
    }
}

llvm::Function* CodeGenerator::visitDecl(std::shared_ptr<Decl> decl) {
    llvm::Function* function = visitDeclPrototype(decl->prototype);

    llvm::BasicBlock* block = llvm::BasicBlock::Create(context, "entry", function);
    builder.SetInsertPoint(block);

    auto param = decl->prototype->paramList.begin();
    auto arg = function->args().begin();
    for(; param != decl->prototype->paramList.end() && arg != function->args().end(); ++param, ++arg) {
        (*param)->codegenVal = arg;
    }

    if(auto expr = decl->expression()) {
        assert(false && "Code generation for stored decls is not yet supported");
    } else {
        visitScope(decl->body());
    }

    llvm::verifyFunction(*function);

    return function;
}

llvm::Function* CodeGenerator::visitDeclPrototype(std::shared_ptr<DeclPrototype> prototype) {
    std::vector<llvm::Type*> arguments;
    for(auto& param: prototype->paramList) {
        arguments.push_back(mapBuiltinTypeToLLVM(param->value.type));
    }
    llvm::Type* type;
    if(prototype->physicalType) {
        type = mapBuiltinTypeToLLVM(prototype->physicalType->type);
    } else {
        type = llvm::Type::getVoidTy(context);
    }
    llvm::FunctionType* functionTy = llvm::FunctionType::get(type,
                                                             arguments,
                                                             false);
    llvm::Function* function = llvm::Function::Create(functionTy, llvm::Function::ExternalLinkage, prototype->name, module.get());

    int i = 0;
    for(auto &arg: function->args()) {
        arg.setName(prototype->paramList[i++]->name);
    }
    prototype->codegenVal = (llvm::Function*) function;
    return function;
}
void CodeGenerator::visitScope(std::shared_ptr<Scope> scope) {
    for(auto& node: scope->nodes) {
        visit(node);
    }
}
void CodeGenerator::visitParam(std::shared_ptr<Param> param) {}
void CodeGenerator::visitArgument(std::shared_ptr<Argument> argument) {}
void CodeGenerator::visitPhysicalTypeRef(std::shared_ptr<PhysicalTypeRef> expr) {}
llvm::Value* CodeGenerator::visitIntegerLiteralExpr(std::shared_ptr<IntegerLiteralExpr> expr) {
    return llvm::ConstantInt::get(context, llvm::APInt(32, std::stoi(expr->literal)));
}
llvm::Value* CodeGenerator::visitDecimalLiteralExpr(std::shared_ptr<DecimalLiteralExpr> expr) {
    return llvm::ConstantFP::get(context, llvm::APFloat(std::stod(expr->literal)));
}
llvm::Value* CodeGenerator::visitDeclRefExpr(std::shared_ptr<DeclRefExpr> expr) {
    auto referencedVal = expr->decl->codegenVal();
    if(expr->decl->isComputed()) {
        auto callee = static_cast<llvm::Function*>(referencedVal);
        assert((callee->arg_size() == expr->argList.size()) &&
               "Incorrect number of arguments passed to function");

        std::vector<llvm::Value*> args;
        for(auto& arg: expr->argList) {
            args.push_back(visitExpr(arg.value));
            if(!args.back()) {
                return nullptr;
            }
        }

        return builder.CreateCall(callee, args, "calltmp");
    } else {
        return referencedVal;
    }
}

llvm::Value* CodeGenerator::visitReturnStmt(std::shared_ptr<ReturnStmt> stmt) {
    if(!stmt->value) {
        return builder.CreateRetVoid();
    } else {
        return builder.CreateRet(visitExpr(stmt->value));
    }
}
