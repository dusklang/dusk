//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "CodeGenerator.h"
#include "llvm/IR/Verifier.h"

llvm::Type* CodeGenerator::mapTypeToLLVM(Type type) {
    if(auto builtinTy = type.builtinType()) {
        switch(*type.builtinType()) {
            case BuiltinType::i32: return llvm::Type::getInt32Ty(context);
            case BuiltinType::f64: return llvm::Type::getDoubleTy(context);
            case BuiltinType::Void: return llvm::Type::getVoidTy(context);
            case BuiltinType::Bool: return llvm::Type::getInt1Ty(context);
            case BuiltinType::Char: return llvm::Type::getInt8Ty(context);
        }
    } else {
        return llvm::PointerType::get(mapTypeToLLVM(**type.pointedType()), 0);
    }
}

llvm::Value* CodeGenerator::visitDecl(std::shared_ptr<Decl> decl) {
    if(auto expr = decl->expression()) {
        if(decl->isMut) {
            decl->codegenVal = builder.CreateAlloca(mapTypeToLLVM(decl->type), 0, decl->name.c_str());
            builder.CreateStore(visitExpr(expr), decl->codegenVal);
        } else {
            decl->codegenVal = visitExpr(expr);
        }
        return decl->codegenVal;
    } else {
        std::vector<llvm::Type*> arguments;
        for(auto& param: decl->paramList) {
            arguments.push_back(mapTypeToLLVM(param->type));
        }
        llvm::Type* type = mapTypeToLLVM(decl->type);
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

            std::function<void(std::shared_ptr<Scope>)> visitInnerScope = [&](std::shared_ptr<Scope> scope) {
                for(auto& node: scope->nodes) {
                    if(auto ifStmt = std::dynamic_pointer_cast<IfStmt>(node)) {
                        auto thenBlock = llvm::BasicBlock::Create(context, "if.then", function);
                        llvm::BasicBlock* elseBlock = nullptr;
                        if(ifStmt->elseScope) {
                            elseBlock = llvm::BasicBlock::Create(context, "if.else", function);
                        }
                        auto endBlock = llvm::BasicBlock::Create(context, "if.end", function);
                        builder.CreateCondBr(visitExpr(ifStmt->condition), thenBlock, ifStmt->elseScope ? elseBlock : endBlock);

                        builder.SetInsertPoint(thenBlock);
                        visitInnerScope(ifStmt->thenScope);
                        // If the last node in thenScope is a return statement, we need to avoid creating
                        // a branch after it because a basic block can only have one terminal instruction.
                        // http://llvm.org/doxygen/classllvm_1_1BasicBlock.html
                        if(ifStmt->thenScope->nodes.empty() ||
                           !std::dynamic_pointer_cast<ReturnStmt>(ifStmt->thenScope->nodes.back())) {
                            builder.CreateBr(endBlock);
                        }

                        if(auto elseScope = ifStmt->elseScope) {
                            builder.SetInsertPoint(elseBlock);
                            visitInnerScope(*elseScope);
                            // Same situation as above.
                            if((*elseScope)->nodes.empty() ||
                               !std::dynamic_pointer_cast<ReturnStmt>((*elseScope)->nodes.back())) {
                                builder.CreateBr(endBlock);
                            }
                        }

                        builder.SetInsertPoint(endBlock);
                    } else if(auto whileStmt = std::dynamic_pointer_cast<WhileStmt>(node)) {
                        auto checkBlock = llvm::BasicBlock::Create(context, "while.check", function);
                        auto thenBlock = llvm::BasicBlock::Create(context, "while.then", function);
                        auto endBlock = llvm::BasicBlock::Create(context, "while.end", function);
                        builder.CreateBr(checkBlock);

                        builder.SetInsertPoint(checkBlock);
                        builder.CreateCondBr(visitExpr(whileStmt->condition), thenBlock, endBlock);

                        builder.SetInsertPoint(thenBlock);
                        visitInnerScope(whileStmt->thenScope);
                        // If the last node in thenScope is a return statement, we need to avoid creating
                        // a branch after it because a basic block can only have one terminal instruction.
                        // http://llvm.org/doxygen/classllvm_1_1BasicBlock.html
                        if(whileStmt->thenScope->nodes.empty() ||
                           !std::dynamic_pointer_cast<ReturnStmt>(whileStmt->thenScope->nodes.back())) {
                            builder.CreateBr(checkBlock);
                        }

                        builder.SetInsertPoint(endBlock);
                    } else {
                        visit(node);
                    }
                }
            };
            visitInnerScope(decl->body());

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
llvm::Value* CodeGenerator::visitIntegerLiteralExpr(std::shared_ptr<IntegerLiteralExpr> expr) {
    return llvm::ConstantInt::get(context, llvm::APInt(32, std::stoi(expr->literal)));
}
llvm::Value* CodeGenerator::visitDecimalLiteralExpr(std::shared_ptr<DecimalLiteralExpr> expr) {
    return llvm::ConstantFP::get(context, llvm::APFloat(std::stod(expr->literal)));
}
llvm::Value* CodeGenerator::visitBooleanLiteralExpr(std::shared_ptr<BooleanLiteralExpr> expr) {
    return llvm::ConstantInt::get(context, llvm::APInt(1, expr->literal ? -1 : 0));
}
llvm::Value* CodeGenerator::visitCharLiteralExpr(std::shared_ptr<CharLiteralExpr> expr) {
    return llvm::ConstantInt::get(context, llvm::APInt(8, expr->literal));
}
llvm::Value* CodeGenerator::visitStringLiteralExpr(std::shared_ptr<StringLiteralExpr> expr) {
    return builder.CreateGlobalStringPtr(expr->literal);
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
    return nullptr;
}
