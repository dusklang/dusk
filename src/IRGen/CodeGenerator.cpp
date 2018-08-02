//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "CodeGenerator.h"
#include "llvm/IR/Verifier.h"

llvm::Type* mapTypeToLLVM(llvm::LLVMContext& context, Type type) {
    struct TypeVisitor {
        llvm::LLVMContext& context;
        TypeVisitor(llvm::LLVMContext& context) : context(context) {}
        llvm::Type* operator()(Type::Variable typeVariable) const {
            assert(false && "Encountered type variable");
            __builtin_unreachable();
        }
        llvm::Type* operator()(Type::ErrorTy) const {
            assert(false && "Encountered error type");
            __builtin_unreachable();
        }
        llvm::Type* operator()(Type::IntegerTy properties) const {
            return llvm::Type::getIntNTy(context, properties.bitWidth);
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
    llvm::Type* ty = std::visit(TypeVisitor(context), type.data);
    for(uint8_t i = 0; i < type.indirection; ++i) {
        ty = llvm::PointerType::get(ty, 0);
    }
    return ty;
}

llvm::Value* CodeGenerator::visitDecl(Decl* decl) {
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
        llvm::Function* function = llvm::Function::Create(functionTy, llvm::Function::ExternalLinkage, decl->name, module);

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

            std::function<void(Scope*)> visitInnerScope = [&](Scope* scope) {
                for(auto& node: scope->nodes) {
                    if(auto ifStmt = dynamic_cast<IfStmt*>(node)) {
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
                           !dynamic_cast<ReturnStmt*>(ifStmt->thenScope->nodes.back())) {
                            builder.CreateBr(endBlock);
                        }

                        if(auto elseScope = ifStmt->elseScope) {
                            builder.SetInsertPoint(elseBlock);
                            visitInnerScope(elseScope);
                            // Same situation as above.
                            if(elseScope->nodes.empty() ||
                               !dynamic_cast<ReturnStmt*>(elseScope->nodes.back())) {
                                builder.CreateBr(endBlock);
                            }
                        }

                        builder.SetInsertPoint(endBlock);
                    } else if(auto whileStmt = dynamic_cast<WhileStmt*>(node)) {
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
                           !dynamic_cast<ReturnStmt*>(whileStmt->thenScope->nodes.back())) {
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
void CodeGenerator::visitScope(Scope* scope) {
    for(auto& node: scope->nodes) {
        visit(node);
    }
}
llvm::Value* CodeGenerator::visitIntegerLiteralExpr(IntegerLiteralExpr* expr) {
    return llvm::ConstantInt::get(context, llvm::APInt(32, std::stoi(expr->literal)));
}
llvm::Value* CodeGenerator::visitDecimalLiteralExpr(DecimalLiteralExpr* expr) {
    return llvm::ConstantFP::get(context, llvm::APFloat(std::stod(expr->literal)));
}
llvm::Value* CodeGenerator::visitBooleanLiteralExpr(BooleanLiteralExpr* expr) {
    return llvm::ConstantInt::get(context, llvm::APInt(1, expr->literal ? -1 : 0));
}
llvm::Value* CodeGenerator::visitCharLiteralExpr(CharLiteralExpr* expr) {
    return llvm::ConstantInt::get(context, llvm::APInt(8, expr->literal));
}
llvm::Value* CodeGenerator::visitStringLiteralExpr(StringLiteralExpr* expr) {
    return builder.CreateGlobalStringPtr(expr->literal);
}
llvm::Value* CodeGenerator::visitPrefixOpExpr(PrefixOpExpr* expr) {
    auto operand = visitExpr(expr->operand);
    switch(expr->op) {
        case OperatorKind::add: return operand;
        case OperatorKind::subtract: return builder.CreateNeg(operand);
        case OperatorKind::asterisk: return builder.CreateLoad(operand);
        case OperatorKind::b_not: return builder.CreateNot(operand);
        case OperatorKind::b_and:
        case OperatorKind::b_or:
        case OperatorKind::divide:
        case OperatorKind::equal:
        case OperatorKind::not_equal:
        case OperatorKind::less_than:
        case OperatorKind::less_than_or_equal:
        case OperatorKind::greater_than:
        case OperatorKind::greater_than_or_equal:
        case OperatorKind::modulo: assert(false && "Invalid prefix operator");
    }
}
llvm::Value* CodeGenerator::visitBinOpExpr(BinOpExpr* expr) {
    auto lhs = visitExpr(expr->lhs);
    auto rhs = visitExpr(expr->rhs);
    switch(expr->op) {
        case OperatorKind::add: return builder.CreateAdd(lhs, rhs);
        case OperatorKind::subtract: return builder.CreateSub(lhs, rhs);
        case OperatorKind::asterisk: return builder.CreateMul(lhs, rhs);
        case OperatorKind::b_not: assert(false && "Invalid prefix operator");
        case OperatorKind::b_and: return builder.CreateAnd(lhs, rhs);
        case OperatorKind::b_or: return builder.CreateOr(lhs, rhs);
        case OperatorKind::divide: return builder.CreateSDiv(lhs, rhs);
        case OperatorKind::equal: return builder.CreateICmpEQ(lhs, rhs);
        case OperatorKind::not_equal: return builder.CreateICmpNE(lhs, rhs);
        case OperatorKind::less_than: return builder.CreateICmpSLT(lhs, rhs);
        case OperatorKind::less_than_or_equal: return builder.CreateICmpSLE(lhs, rhs);
        case OperatorKind::greater_than: return builder.CreateICmpSGT(lhs, rhs);
        case OperatorKind::greater_than_or_equal: return builder.CreateICmpSGE(lhs, rhs);
        case OperatorKind::modulo: return builder.CreateSRem(lhs, rhs);
    }
}
llvm::Value* CodeGenerator::visitDeclRefExpr(DeclRefExpr* expr) {
    auto referencedVal = expr->decl->codegenVal;
    if(expr->decl->isComputed()) {
        auto callee = static_cast<llvm::Function*>(referencedVal);
        assert((callee->arg_size() == expr->argList.size()) &&
               "Incorrect number of arguments passed to function");

        std::vector<llvm::Value*> args;
        for(auto* arg: expr->argList) {
            args.push_back(visitExpr(arg));
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
llvm::Value* CodeGenerator::visitReturnStmt(ReturnStmt* stmt) {
    if(!stmt->value) {
        return builder.CreateRetVoid();
    } else {
        return builder.CreateRet(visitExpr(stmt->value));
    }
}
llvm::Value* CodeGenerator::visitAssignmentStmt(AssignmentStmt* stmt) {
    return builder.CreateStore(visitExpr(stmt->rhs), stmt->lhs->decl->codegenVal);
}
llvm::Value* CodeGenerator::visitIfStmt(IfStmt* stmt) {
    return nullptr;
}
