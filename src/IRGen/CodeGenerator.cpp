//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "CodeGenerator.h"
#include "llvm/IR/Verifier.h"

llvm::Type* CodeGenerator::mapBuiltinTypeToLLVM(BuiltinType type) {
    switch(type) {
        case BuiltinType::i32: return llvm::Type::getInt32Ty(context);
        case BuiltinType::f64: return llvm::Type::getDoubleTy(context);
        case BuiltinType::Void: return llvm::Type::getVoidTy(context);
        case BuiltinType::Bool: return llvm::Type::getInt1Ty(context);
    }
}

llvm::Value* CodeGenerator::visitDecl(std::shared_ptr<Decl> decl) {
    if(auto expr = decl->expression()) {
        if(decl->isMut) {
            decl->codegenVal = builder.CreateAlloca(mapBuiltinTypeToLLVM(decl->type.getType()), 0, decl->name.c_str());
            builder.CreateStore(visitExpr(expr), decl->codegenVal);
        } else {
            decl->codegenVal = visitExpr(expr);
        }
        return decl->codegenVal;
    } else {
        std::vector<llvm::Type*> arguments;
        for(auto& param: decl->paramList) {
            arguments.push_back(mapBuiltinTypeToLLVM(param->type.getType()));
        }
        llvm::Type* type = mapBuiltinTypeToLLVM(decl->type.getType());
        llvm::FunctionType* functionTy = llvm::FunctionType::get(type,
                                                                 arguments,
                                                                 false);
        llvm::Function* function = llvm::Function::Create(functionTy, llvm::Function::ExternalLinkage, decl->name, module.get());

        int i = 0;
        for(auto &arg: function->args()) {
            arg.setName(decl->paramList[i++]->name);
        }
        decl->codegenVal = function;

        if(decl->hasDefinition()) {
            llvm::BasicBlock* block = llvm::BasicBlock::Create(context, "entry", function);
            builder.SetInsertPoint(block);

            auto param = decl->paramList.begin();
            auto arg = function->args().begin();
            for(; param != decl->paramList.end() && arg != function->args().end(); ++param, ++arg) {
                (*param)->codegenVal = arg;
            }

            visitScope(decl->body());

            llvm::verifyFunction(*function);
        }

        return function;
    }
}
void CodeGenerator::visitScope(std::shared_ptr<Scope> scope) {
    for(auto& node: scope->nodes) {
        visit(node);
    }
}
void CodeGenerator::visitArgument(std::shared_ptr<Argument> argument) {}
void CodeGenerator::visitPhysicalTypeRef(std::shared_ptr<PhysicalTypeRef> expr) {}
llvm::Value* CodeGenerator::visitIntegerLiteralExpr(std::shared_ptr<IntegerLiteralExpr> expr) {
    return llvm::ConstantInt::get(context, llvm::APInt(32, std::stoi(expr->literal)));
}
llvm::Value* CodeGenerator::visitDecimalLiteralExpr(std::shared_ptr<DecimalLiteralExpr> expr) {
    return llvm::ConstantFP::get(context, llvm::APFloat(std::stod(expr->literal)));
}
llvm::Value* CodeGenerator::visitBooleanLiteralExpr(std::shared_ptr<BooleanLiteralExpr> expr) {
    return llvm::ConstantInt::get(context, llvm::APInt(1, expr->literal ? 1 : 0));
}
llvm::Value* CodeGenerator::visitDeclRefExpr(std::shared_ptr<DeclRefExpr> expr) {
    auto referencedVal = expr->decl->codegenVal;
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
        if(expr->decl->isMut) {
            return builder.CreateLoad(expr->decl->codegenVal, expr->decl->name.c_str());
        } else {
            return referencedVal;
        }
    }
}

llvm::Value* CodeGenerator::visitReturnStmt(std::shared_ptr<ReturnStmt> stmt) {
    if(!stmt->value) {
        return builder.CreateRetVoid();
    } else {
        return builder.CreateRet(visitExpr(stmt->value));
    }
}

llvm::Value* CodeGenerator::visitAssignmentStmt(std::shared_ptr<AssignmentStmt> stmt) {
    return builder.CreateStore(visitExpr(stmt->rhs), stmt->lhs->decl->codegenVal);
}

llvm::Value* CodeGenerator::visitIfStmt(std::shared_ptr<IfStmt> stmt) {
    
}
