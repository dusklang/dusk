//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "CodeGenerator.h"
#include "llvm/IR/Verifier.h"

#include "mpark/patterns.hpp"

using namespace mpark::patterns;

llvm::Type* mapTypeToLLVM(llvm::LLVMContext& context, Type type) {
    llvm::Type* ty = match(type.data)(
        pattern(as<Type::Variable>(_)) = []() -> llvm::Type* {
            assert(false && "Encountered type variable");
            return nullptr;
        },
        pattern(as<Type::ErrorTy>(_)) = []() -> llvm::Type* {
            assert(false && "Encountered error type");
            return nullptr;
        },
        pattern(as<Type::IntegerTy>(arg)) = [&](auto properties) -> llvm::Type* {
            return llvm::Type::getIntNTy(context, properties.bitWidth);
        },
        pattern(as<Type::VoidTy>(_)) = [&]() -> llvm::Type* {
            return llvm::Type::getVoidTy(context);
        },
        pattern(as<Type::BoolTy>(_)) = [&]() -> llvm::Type* {
            return llvm::Type::getInt1Ty(context);
        },
        pattern(as<Type::FloatTy>(_)) = [&]() -> llvm::Type* {
            return llvm::Type::getFloatTy(context);
        },
        pattern(as<Type::DoubleTy>(_)) = [&]() -> llvm::Type* {
            return llvm::Type::getDoubleTy(context);
        }
    );
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
void CodeGenerator::visitStructDecl(StructDecl* decl) {
    
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
llvm::Value* CodeGenerator::visitPreOpExpr(PreOpExpr* expr) {
    auto operand = visitExpr(expr->operand);
    switch(expr->op) {
        case PreOp::Positive: return operand;
        case PreOp::Negative: return builder.CreateNeg(operand);
        case PreOp::Deref: return builder.CreateLoad(operand);
        case PreOp::Not: return builder.CreateNot(operand);
    }
}
llvm::Value* CodeGenerator::visitBinOpExpr(BinOpExpr* expr) {
    llvm::Value* lhs;
    if(expr->op != BinOp::Assignment) {
        lhs = visitExpr(expr->lhs);
    }
    auto rhs = visitExpr(expr->rhs);
    auto createAdd = [&]() -> llvm::Value* {
        if(expr->lhs->type.indirection > 0) {
            return builder.CreateGEP(lhs, rhs);
        } else {
            return match(expr->lhs->type.data)(
                pattern(as<Type::IntegerTy>(arg)) = [&](auto properties) {
                    return builder.CreateAdd(lhs, rhs);
                },
                pattern(as<Type::FloatTy>(_)) = [&] {
                    return builder.CreateFAdd(lhs, rhs);
                },
                pattern(as<Type::DoubleTy>(_)) = [&] {
                    return builder.CreateFAdd(lhs, rhs);
                },
                pattern(_) = [&]() -> llvm::Value* {
                    assert(false && "Can't add values of that type");
                    __builtin_unreachable();
                }
            );
        }
    };
    auto createSub = [&]() -> llvm::Value* {
        return match(expr->lhs->type.data)(
            pattern(as<Type::IntegerTy>(arg)) = [&](auto properties) {
                return builder.CreateSub(lhs, rhs);
            },
            pattern(as<Type::FloatTy>(_)) = [&] {
                return builder.CreateFSub(lhs, rhs);
            },
            pattern(as<Type::DoubleTy>(_)) = [&] {
                return builder.CreateFSub(lhs, rhs);
            },
            pattern(_) = [&]() -> llvm::Value* {
                assert(false && "Can't subtract values of that type");
            }
        );
    };
    auto createMult = [&]() -> llvm::Value* {
        return match(expr->lhs->type.data)(
            pattern(as<Type::IntegerTy>(arg)) = [&](auto properties) {
                return builder.CreateMul(lhs, rhs);
            },
            pattern(as<Type::FloatTy>(_)) = [&] {
                return builder.CreateFMul(lhs, rhs);
            },
            pattern(as<Type::DoubleTy>(_)) = [&] {
                return builder.CreateFMul(lhs, rhs);
            },
            pattern(_) = [&]() -> llvm::Value* {
                assert(false && "Can't multiply values of that type");
            }
        );
    };
    auto createDiv = [&]() -> llvm::Value* {
        return match(expr->lhs->type.data)(
            pattern(as<Type::IntegerTy>(arg)) = [&](auto properties) {
                if(properties.isSigned) {
                    return builder.CreateSDiv(lhs, rhs);
                } else {
                    return builder.CreateUDiv(lhs, rhs);
                }
            },
            pattern(as<Type::FloatTy>(_)) = [&] {
                return builder.CreateFDiv(lhs, rhs);
            },
            pattern(as<Type::DoubleTy>(_)) = [&] {
                return builder.CreateFDiv(lhs, rhs);
            },
            pattern(_) = [&]() -> llvm::Value* {
                assert(false && "Can't divide values of that type");
            }
        );
    };
    auto createMod = [&]() -> llvm::Value* {
        return match(expr->lhs->type.data)(
            pattern(as<Type::IntegerTy>(arg)) = [&](auto properties) {
                if(properties.isSigned) {
                    return builder.CreateSRem(lhs, rhs);
                } else {
                    return builder.CreateURem(lhs, rhs);
                }
            },
            pattern(as<Type::FloatTy>(_)) = [&] {
                return builder.CreateFRem(lhs, rhs);
            },
            pattern(as<Type::DoubleTy>(_)) = [&] {
                return builder.CreateFRem(lhs, rhs);
            },
            pattern(_) = [&]() -> llvm::Value* {
                assert(false && "Can't modulo values of that type");
            }
        );
    };
    switch(expr->op) {
        case BinOp::Add: return createAdd();
        case BinOp::Sub: return createSub();
        case BinOp::Mult: return createMult();
        case BinOp::Div: return createDiv();
        case BinOp::Mod: return createMod();
        case BinOp::And: return builder.CreateAnd(lhs, rhs);
        case BinOp::Or: return builder.CreateOr(lhs, rhs);
        case BinOp::Equal: return builder.CreateICmpEQ(lhs, rhs);
        case BinOp::NotEqual: return builder.CreateICmpNE(lhs, rhs);
        case BinOp::LessThan: return builder.CreateICmpSLT(lhs, rhs);
        case BinOp::LessThanOrEqual: return builder.CreateICmpSLE(lhs, rhs);
        case BinOp::GreaterThan: return builder.CreateICmpSGT(lhs, rhs);
        case BinOp::GreaterThanOrEqual: return builder.CreateICmpSGE(lhs, rhs);
        case BinOp::Assignment:
        case BinOp::AddAssignment:
        case BinOp::SubAssignment:
        case BinOp::MultAssignment:
        case BinOp::DivAssignment:
        case BinOp::ModAssignment:
            auto declRef = dynamic_cast<DeclRefExpr*>(expr->lhs);
            llvm::Value* value;
            switch(expr->op) {
                case BinOp::Assignment:
                    value = rhs;
                    break;
                case BinOp::AddAssignment:
                    value = createAdd();
                    break;
                case BinOp::SubAssignment:
                    value = createSub();
                    break;
                case BinOp::MultAssignment:
                    value = createMult();
                    break;
                case BinOp::DivAssignment:
                    value = createDiv();
                    break;
                case BinOp::ModAssignment:
                    value = createMod();
                    break;
                default: __builtin_unreachable();
            }
            return builder.CreateStore(value, declRef->decl->codegenVal);
    }
}
llvm::Value* CodeGenerator::visitCastExpr(CastExpr* expr) {
    auto ogType = expr->operand->type;
    auto destType = expr->destType;
    auto ogValue = visitExpr(expr->operand);

    if(ogType == destType) { return ogValue; }

    auto destTypeLLVM = mapTypeToLLVM(context, destType);
    assert(
        (ogType.indirection == 0 && destType.indirection == 0) &&
        "pointer casting isn't yet allowed"
    );
    return match(expr->operand->type.data, expr->destType.data)(
        pattern(as<Type::IntegerTy>(arg), as<Type::IntegerTy>(arg)) = [&](auto og, auto dest) {
            // TODO: Detect overflow.
            if(dest.isSigned) {
                return builder.CreateSExtOrTrunc(ogValue, destTypeLLVM);
            } else {
                return builder.CreateZExtOrTrunc(ogValue, destTypeLLVM);
            }
        },
        pattern(as<Type::IntegerTy>(arg), as<Type::FloatTy>(arg)) = [&](auto og, auto dest) {
            if(og.isSigned) {
                return builder.CreateSIToFP(ogValue, destTypeLLVM);
            } else {
                return builder.CreateUIToFP(ogValue, destTypeLLVM);
            }
        },
        pattern(as<Type::FloatTy>(arg), as<Type::IntegerTy>(arg)) = [&](auto og, auto dest) {
            if(dest.isSigned) {
                return builder.CreateFPToSI(ogValue, destTypeLLVM);
            } else {
                return builder.CreateFPToSI(ogValue, destTypeLLVM);
            }
        },
        pattern(as<Type::FloatTy>(_), as<Type::DoubleTy>(_)) = [&] {
            return builder.CreateFPExt(ogValue, destTypeLLVM);
        },
        pattern(as<Type::DoubleTy>(_), as<Type::FloatTy>(_)) = [&] {
            return builder.CreateFPTrunc(ogValue, destTypeLLVM);
        }
    );
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
llvm::Value* CodeGenerator::visitIfStmt(IfStmt* stmt) {
    return nullptr;
}
