//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "CodeGenerator.h"
#include "llvm/IR/Verifier.h"

#include "mpark/patterns.hpp"

using namespace mpark::patterns;

llvm::Value* CodeGenerator::toDirect(CodeGenVal val) {
    return match(val)(
        pattern(as<DirectVal>(arg)) = [](auto val) { return val.val; },
        pattern(as<IndirectVal>(arg)) = [&](auto val) -> llvm::Value* {
            // If we have a pointer to the value, load from it.
            return builder.CreateLoad(val.val);
        }
    );
}
llvm::Value* CodeGenerator::toIndirect(CodeGenVal val) {
    return match(val)(
        pattern(as<IndirectVal>(arg)) = [](auto val) { return val.val; },
        pattern(as<DirectVal>(arg)) = [&](auto val) -> llvm::Value* {
            // If we don't already have a pointer to the value, we'll need to copy it to the stack.
            auto copy = builder.CreateAlloca(val.val->getType());
            builder.CreateStore(val.val, copy);
            return copy;
        }
    );
}
llvm::Type* CodeGenerator::toLLVMTy(Type type) {
    return match(type.data)(
        pattern(as<TyVariable>(_)) = []() -> llvm::Type* {
            assert(false && "Encountered type variable");
            return nullptr;
        },
        pattern(as<ErrorTy>(_)) = []() -> llvm::Type* {
            assert(false && "Encountered error type");
            return nullptr;
        },
        pattern(as<IntTy>(arg)) = [&](auto properties) -> llvm::Type* {
            return llvm::Type::getIntNTy(context, properties.bitWidth);
        },
        pattern(as<StructTy>(arg)) = [&](auto structTy) -> llvm::Type* {
            std::vector<llvm::Type*> types;
            for(auto field: structTy.decl->fields) {
                types.push_back(toLLVMTy(field->type));
            }
            return llvm::StructType::get(context, types);
        },
        pattern(as<VoidTy>(_)) = [&]() -> llvm::Type* {
            return llvm::Type::getVoidTy(context);
        },
        pattern(as<BoolTy>(_)) = [&]() -> llvm::Type* {
            return llvm::Type::getInt1Ty(context);
        },
        pattern(as<FloatTy>(_)) = [&]() -> llvm::Type* {
            return llvm::Type::getFloatTy(context);
        },
        pattern(as<DoubleTy>(_)) = [&]() -> llvm::Type* {
            return llvm::Type::getDoubleTy(context);
        },
        pattern(as<PointerTy>(ds(arg))) = [&](auto pointedTy) -> llvm::Type* {
            return llvm::PointerType::get(toLLVMTy(*pointedTy), 0);
        }
    );
}
CodeGenVal CodeGenerator::visitDecl(Decl* decl) {
    if(auto expr = decl->expression()) {
        if(decl->isVar) {
            decl->codegenVal = builder.CreateAlloca(toLLVMTy(decl->type), 0, decl->name.text.c_str());
            builder.CreateStore(toDirect(visitExpr(expr)), decl->codegenVal);
        } else {
            decl->codegenVal = toDirect(visitExpr(expr));
        }
        return DirectVal { decl->codegenVal };
    } else {
        std::vector<llvm::Type*> arguments;
        for(auto& param: decl->paramList) {
            arguments.push_back(toLLVMTy(param->type));
        }
        llvm::Type* type = toLLVMTy(decl->type);
        if(decl->isVar) {
            llvm::GlobalVariable* var = new llvm::GlobalVariable(*module, type, false, llvm::GlobalVariable::ExternalLinkage, nullptr, decl->name.text);
            decl->codegenVal = var;
            return DirectVal { var };
        } else {
            llvm::FunctionType* functionTy = llvm::FunctionType::get(type,
                                                                     arguments,
                                                                     false);
            llvm::Function* function = llvm::Function::Create(functionTy, llvm::Function::ExternalLinkage, decl->name.text, module);

            int i = 0;
            for(auto &arg: function->args()) {
                arg.setName(decl->paramList[i++]->name.text);
            }
            decl->codegenVal = function;

            if(decl->hasDefinition()) {
                llvm::BasicBlock* block = llvm::BasicBlock::Create(context, "entry", function);
                builder.SetInsertPoint(block);

                auto param = decl->paramList.begin();
                auto funcArg = function->args().begin();
                for(; param != decl->paramList.end() && funcArg != function->args().end(); ++param, ++funcArg) {
                    (*param)->codegenVal = funcArg;
                }

                std::function<void(Scope*)> visitInnerScope = [&](Scope* scope) {
                    for(auto& node: scope->nodes) {
                        if(auto ifStmt = dynamic_cast<IfStmt*>(node)) {
                            auto thenBlock = llvm::BasicBlock::Create(context, "if.then", function);
                            llvm::BasicBlock* elseBlock = nullptr;
                            if(ifStmt->elseNode) {
                                elseBlock = llvm::BasicBlock::Create(context, "if.else", function);
                            }
                            auto endBlock = llvm::BasicBlock::Create(context, "if.end", function);
                            builder.CreateCondBr(toDirect(visitExpr(ifStmt->condition)), thenBlock, ifStmt->elseNode ? elseBlock : endBlock);

                            builder.SetInsertPoint(thenBlock);
                            visitInnerScope(ifStmt->thenScope);
                            // If the last node in thenScope is a return statement, we need to avoid creating
                            // a branch after it because a basic block can only have one terminal instruction.
                            // http://llvm.org/doxygen/classllvm_1_1BasicBlock.html
                            if(ifStmt->thenScope->nodes.empty() ||
                               !dynamic_cast<ReturnStmt*>(ifStmt->thenScope->nodes.back())) {
                                builder.CreateBr(endBlock);
                            }

                            if(ifStmt->elseNode) {
                                builder.SetInsertPoint(elseBlock);
                                match(ifStmt->elseNode)(
                                    pattern(some(as<Scope*>(arg))) = [&](auto scope) {
                                        visitInnerScope(scope);
                                        // Same situation as above.
                                        if(scope->nodes.empty() ||
                                           !dynamic_cast<ReturnStmt*>(scope->nodes.back())) {
                                            builder.CreateBr(endBlock);
                                        }
                                    },
                                    pattern(some(as<IfStmt*>(arg))) = [&](auto stmt) {
                                        // FIXME: HACK. Make visitInnerScope into regular visitXXX methods
                                        // so we can just call visitIfStmt on stmt instead of allocating
                                        // a new Scope.
                                        visitInnerScope(new Scope(SourceRange(), { stmt }));
                                    },
                                    pattern(_) = []{}
                                );
                            }

                            builder.SetInsertPoint(endBlock);
                        } else if(auto whileStmt = dynamic_cast<WhileStmt*>(node)) {
                            auto checkBlock = llvm::BasicBlock::Create(context, "while.check", function);
                            auto thenBlock = llvm::BasicBlock::Create(context, "while.then", function);
                            auto endBlock = llvm::BasicBlock::Create(context, "while.end", function);
                            builder.CreateBr(checkBlock);

                            builder.SetInsertPoint(checkBlock);
                            builder.CreateCondBr(toDirect(visitExpr(whileStmt->condition)), thenBlock, endBlock);

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

            return DirectVal { function };
        }
    }
}
void CodeGenerator::visitStructDecl(StructDecl* decl) {
    
}
void CodeGenerator::visitScope(Scope* scope) {
    for(auto& node: scope->nodes) {
        visit(node);
    }
}
CodeGenVal CodeGenerator::visitIntegerLiteralExpr(IntegerLiteralExpr* expr) {
    return DirectVal { llvm::ConstantInt::get(context, llvm::APInt(32, std::stoi(expr->literal))) };
}
CodeGenVal CodeGenerator::visitDecimalLiteralExpr(DecimalLiteralExpr* expr) {
    return DirectVal { llvm::ConstantFP::get(context, llvm::APFloat(std::stod(expr->literal))) };
}
CodeGenVal CodeGenerator::visitBooleanLiteralExpr(BooleanLiteralExpr* expr) {
    return DirectVal { llvm::ConstantInt::get(context, llvm::APInt(1, expr->literal ? -1 : 0)) };
}
CodeGenVal CodeGenerator::visitCharLiteralExpr(CharLiteralExpr* expr) {
    return DirectVal { llvm::ConstantInt::get(context, llvm::APInt(8, expr->literal)) };
}
CodeGenVal CodeGenerator::visitStringLiteralExpr(StringLiteralExpr* expr) {
    return DirectVal { builder.CreateGlobalStringPtr(expr->literal) };
}
CodeGenVal CodeGenerator::visitPreOpExpr(PreOpExpr* expr) {
    auto operand = visitExpr(expr->operand);
    switch(expr->op) {
        case PreOp::Positive: return operand;
        case PreOp::Negative: return DirectVal { builder.CreateNeg(toDirect(operand)) };
        case PreOp::Deref: return IndirectVal { toDirect(operand) };
        case PreOp::Not: return DirectVal { builder.CreateNot(toDirect(operand)) };
        case PreOp::AddrOf: return DirectVal { toIndirect(operand) };
    }
}
CodeGenVal CodeGenerator::visitBinOpExpr(BinOpExpr* expr) {
    auto lhs = visitExpr(expr->lhs);
    auto rhs = visitExpr(expr->rhs);
    auto createAdd = [&]() -> DirectVal {
        return match(expr->lhs->type.data, expr->rhs->type.data)(
            pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto properties) {
                return DirectVal { builder.CreateAdd(toDirect(lhs), toDirect(rhs)) };
            },
            pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                return DirectVal { builder.CreateFAdd(toDirect(lhs), toDirect(rhs)) };
            },
            pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                return DirectVal { builder.CreateFAdd(toDirect(lhs), toDirect(rhs)) };
            },
            pattern(as<PointerTy>(_), as<IntTy>(_)) = [&] {
                return DirectVal { builder.CreateGEP(toDirect(lhs), toDirect(rhs)) };
            },
            pattern(_, _) = [&]() -> DirectVal {
                assert(false && "Can't add values of those types");
                __builtin_unreachable();
            }
        );
    };
    auto createSub = [&]() -> DirectVal {
        return match(expr->lhs->type.data)(
            pattern(as<IntTy>(arg)) = [&](auto properties) {
                return DirectVal { builder.CreateSub(toDirect(lhs), toDirect(rhs)) };
            },
            pattern(as<FloatTy>(_)) = [&] {
                return DirectVal { builder.CreateFSub(toDirect(lhs), toDirect(rhs)) };
            },
            pattern(as<DoubleTy>(_)) = [&] {
                return DirectVal { builder.CreateFSub(toDirect(lhs), toDirect(rhs)) };
            },
            pattern(_) = [&]() -> DirectVal {
                assert(false && "Can't subtract values of that type");
            }
        );
    };
    auto createMult = [&]() -> DirectVal {
        return match(expr->lhs->type.data)(
            pattern(as<IntTy>(arg)) = [&](auto properties) {
                return DirectVal { builder.CreateMul(toDirect(lhs), toDirect(rhs)) };
            },
            pattern(as<FloatTy>(_)) = [&] {
                return DirectVal { builder.CreateFMul(toDirect(lhs), toDirect(rhs)) };
            },
            pattern(as<DoubleTy>(_)) = [&] {
                return DirectVal { builder.CreateFMul(toDirect(lhs), toDirect(rhs)) };
            },
            pattern(_) = [&]() -> DirectVal {
                assert(false && "Can't multiply values of that type");
            }
        );
    };
    auto createDiv = [&]() -> DirectVal {
        return match(expr->lhs->type.data)(
            pattern(as<IntTy>(arg)) = [&](auto properties) {
                switch(properties.signedness) {
                    case Signedness::Signed:   return DirectVal { builder.CreateSDiv(toDirect(lhs), toDirect(rhs)) };
                    case Signedness::Unsigned: return DirectVal { builder.CreateUDiv(toDirect(lhs), toDirect(rhs)) };
                }
            },
            pattern(as<FloatTy>(_)) = [&] {
                return DirectVal { builder.CreateFDiv(toDirect(lhs), toDirect(rhs)) };
            },
            pattern(as<DoubleTy>(_)) = [&] {
                return DirectVal { builder.CreateFDiv(toDirect(lhs), toDirect(rhs)) };
            },
            pattern(_) = [&]() -> DirectVal {
                assert(false && "Can't divide values of that type");
            }
        );
    };
    auto createMod = [&]() -> DirectVal {
        return match(expr->lhs->type.data)(
            pattern(as<IntTy>(arg)) = [&](auto properties) {
                switch(properties.signedness) {
                    case Signedness::Signed:
                        return DirectVal { builder.CreateSRem(toDirect(lhs), toDirect(rhs)) };
                    case Signedness::Unsigned:
                        return DirectVal { builder.CreateURem(toDirect(lhs), toDirect(rhs)) };
                }
            },
            pattern(as<FloatTy>(_)) = [&] {
                return DirectVal { builder.CreateFRem(toDirect(lhs), toDirect(rhs)) };
            },
            pattern(as<DoubleTy>(_)) = [&] {
                return DirectVal { builder.CreateFRem(toDirect(lhs), toDirect(rhs)) };
            },
            pattern(_) = [&]() -> DirectVal {
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
        case BinOp::BitwiseAnd:
        case BinOp::And:
            return DirectVal { builder.CreateAnd(toDirect(lhs), toDirect(rhs)) };
        case BinOp::BitwiseOr:
        case BinOp::Or:
            return DirectVal { builder.CreateOr(toDirect(lhs), toDirect(rhs)) };
        case BinOp::Equal: return DirectVal { builder.CreateICmpEQ(toDirect(lhs), toDirect(rhs)) };
        case BinOp::NotEqual: return DirectVal { builder.CreateICmpNE(toDirect(lhs), toDirect(rhs)) };
        case BinOp::LessThan: return DirectVal { builder.CreateICmpSLT(toDirect(lhs), toDirect(rhs)) };
        case BinOp::LessThanOrEqual: return DirectVal { builder.CreateICmpSLE(toDirect(lhs), toDirect(rhs)) };
        case BinOp::GreaterThan: return DirectVal { builder.CreateICmpSGT(toDirect(lhs), toDirect(rhs)) };
        case BinOp::GreaterThanOrEqual:
            return DirectVal { builder.CreateICmpSGE(toDirect(lhs), toDirect(rhs)) };
        case BinOp::Assignment:
        case BinOp::AddAssignment:
        case BinOp::SubAssignment:
        case BinOp::MultAssignment:
        case BinOp::DivAssignment:
        case BinOp::ModAssignment:
        case BinOp::AndAssignment:
        case BinOp::OrAssignment:
            llvm::Value* lhsInd = toIndirect(lhs);
            llvm::Value* value;
            switch(expr->op) {
                case BinOp::Assignment:
                    value = toDirect(rhs);
                    break;
                case BinOp::AddAssignment:
                    value = createAdd().val;
                    break;
                case BinOp::SubAssignment:
                    value = createSub().val;
                    break;
                case BinOp::MultAssignment:
                    value = createMult().val;
                    break;
                case BinOp::DivAssignment:
                    value = createDiv().val;
                    break;
                case BinOp::ModAssignment:
                    value = createMod().val;
                    break;
                case BinOp::AndAssignment:
                    value = builder.CreateAnd(toDirect(lhs), toDirect(rhs));
                    break;
                case BinOp::OrAssignment:
                    value = builder.CreateOr(toDirect(lhs), toDirect(rhs));
                    break;
                default: __builtin_unreachable();
            }
            return DirectVal { builder.CreateStore(value, lhsInd) };
    }
}
CodeGenVal CodeGenerator::visitCastExpr(CastExpr* expr) {
    auto ogType = expr->operand->type;
    auto destType = expr->destType;
    auto ogValue = visitExpr(expr->operand);

    if(ogType == destType) { return ogValue; }

    auto destTypeLLVM = toLLVMTy(destType);
    return match(expr->operand->type.data, expr->destType.data)(
        pattern(as<IntTy>(arg), as<IntTy>(arg)) = [&](auto og, auto dest) -> DirectVal {
            // TODO: Detect overflow.
            switch(dest.signedness) {
                case Signedness::Signed:   return DirectVal { builder.CreateSExtOrTrunc(toDirect(ogValue), destTypeLLVM) };
                case Signedness::Unsigned: return DirectVal { builder.CreateZExtOrTrunc(toDirect(ogValue), destTypeLLVM) };
            }
        },
        pattern(as<IntTy>(arg), as<FloatTy>(arg)) = [&](auto og, auto dest) -> DirectVal {
            switch(og.signedness) {
                case Signedness::Signed:   return DirectVal { builder.CreateSIToFP(toDirect(ogValue), destTypeLLVM) };
                case Signedness::Unsigned: return DirectVal { builder.CreateUIToFP(toDirect(ogValue), destTypeLLVM) };
            }
        },
        pattern(as<FloatTy>(arg), as<IntTy>(arg)) = [&](auto og, auto dest) -> DirectVal {
            switch(dest.signedness) {
                case Signedness::Signed:   return DirectVal { builder.CreateFPToSI(toDirect(ogValue), destTypeLLVM) };
                case Signedness::Unsigned: return DirectVal { builder.CreateFPToSI(toDirect(ogValue), destTypeLLVM) };
            }
        },
        pattern(as<FloatTy>(_), as<DoubleTy>(_)) = [&]() -> DirectVal {
            return DirectVal { builder.CreateFPExt(toDirect(ogValue), destTypeLLVM) };
        },
        pattern(as<DoubleTy>(_), as<FloatTy>(_)) = [&]() -> DirectVal {
            return DirectVal { builder.CreateFPTrunc(toDirect(ogValue), destTypeLLVM) };
        },
        pattern(as<PointerTy>(_), _) = []() -> DirectVal {
            assert(false && "pointer casting is not yet supported");
            __builtin_unreachable();
        },
        pattern(_, as<PointerTy>(_)) = []() -> DirectVal {
            assert(false && "pointer casting is not yet supported");
            __builtin_unreachable();
        }
    );
}
CodeGenVal CodeGenerator::visitDeclRefExpr(DeclRefExpr* expr) {
    auto referencedVal = expr->decl->codegenVal;
    if(expr->decl->isComputed()) {
        auto callee = static_cast<llvm::Function*>(referencedVal);
        assert((callee->arg_size() == expr->argList.size()) &&
               "Incorrect number of arguments passed to function");

        std::vector<llvm::Value*> args;
        for(auto* arg: expr->argList) {
            args.push_back(toDirect(visitExpr(arg)));
            if(!args.back()) {
                return DirectVal { nullptr };
            }
        }

        return DirectVal { builder.CreateCall(callee, args, "calltmp") };
    } else {
        if(expr->decl->isVar) {
            return IndirectVal { referencedVal };
        } else {
            return DirectVal { referencedVal };
        }
    }
}
CodeGenVal CodeGenerator::visitMemberRefExpr(MemberRefExpr* expr) {
    llvm::Value* root = toIndirect(visitExpr(expr->root));
    auto firstIndex = llvm::ConstantInt::get(context, llvm::APInt(32, 0));
    auto index = llvm::ConstantInt::get(context, llvm::APInt(32, expr->declIndex));
    std::vector<llvm::Value*> indices { firstIndex, index };
    auto addr = builder.CreateGEP(root, indices);
    return IndirectVal { addr };
}
CodeGenVal CodeGenerator::visitReturnStmt(ReturnStmt* stmt) {
    if(!stmt->value) {
        return DirectVal { builder.CreateRetVoid() };
    } else {
        return DirectVal { builder.CreateRet(toDirect(visitExpr(stmt->value))) };
    }
}
CodeGenVal CodeGenerator::visitIfStmt(IfStmt* stmt) {
    return DirectVal { nullptr };
}
