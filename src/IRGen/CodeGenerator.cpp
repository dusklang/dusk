//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "CodeGenerator.h"
#include "llvm/IR/Verifier.h"

llvm::Type* mapTypeToLLVM(llvm::LLVMContext& context, Type type) {
    struct TypeVisitor {
        llvm::LLVMContext& context;
        TypeVisitor(llvm::LLVMContext& context) : context(context) {}
        llvm::Type* operator()(Type::Variable typeVariable) const {
            assert(false && "Encountered type variable");
            LLVM_BUILTIN_UNREACHABLE;
        }
        llvm::Type* operator()(Type::ErrorTy) const {
            assert(false && "Encountered error type");
            LLVM_BUILTIN_UNREACHABLE;
        }
        llvm::Type* operator()(Type::IntegerTy properties) const {
            return llvm::Type::getIntNTy(context, properties.bitWidth);
        }
        llvm::Type* operator()(Type::PointerTy pointer) const {
            return llvm::PointerType::get(mapTypeToLLVM(context, *pointer.pointedTy), 0);
        }
        llvm::Type* operator()(Type::VoidTy) const {
            return llvm::Type::getVoidTy(context);
        }
        llvm::Type* operator()(Type::BoolTy) const {
            return llvm::Type::getInt1Ty(context);
        }
        llvm::Type* operator()(Type::FloatTy) const {
            return llvm::Type::getFloatTy(context);
        }
        llvm::Type* operator()(Type::DoubleTy) const {
            return llvm::Type::getDoubleTy(context);
        }
    };
    return std::visit(TypeVisitor(context), type.data);
}

llvm::Value* CodeGenerator::visitDecl(std::shared_ptr<Decl> decl) {
    if(auto expr = decl->expression()) {
        if(decl->isVar) {
            decl->codegenVal = builder.CreateAlloca(mapTypeToLLVM(context, decl->type), 0, decl->name.c_str());
            builder.CreateStore(visitExpr(expr), decl->codegenVal);
        } else {
            decl->codegenVal = visitExpr(expr);
        }
        return decl->codegenVal;
    } else {
        std::vector<llvm::Type*> arguments;
        for(auto& param: decl->paramList) {
            arguments.push_back(mapTypeToLLVM(context, param->type));
        }
        llvm::Type* type = mapTypeToLLVM(context, decl->type);
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
        if(expr->decl->isVar) {
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
