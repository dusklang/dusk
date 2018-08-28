//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <optional>

#include "LLVMGenerator.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/IR/LegacyPassManager.h"

#include "General/General.h"

#include "mpark/patterns.hpp"

using namespace mpark::patterns;

void LLVMGenerator::visitTopLevel(std::vector<ASTNode*> nodes) {
    for(auto node: nodes) {
        if(auto decl = dynamic_cast<Decl*>(node)) {
            visitDeclPrototype(decl);
        }
    }
    for(auto node: nodes) {
        visit(node);
    }
}
llvm::Value* LLVMGenerator::toDirect(CodeGenVal val) {
    return match(val)(
        pattern(as<DirectVal>(arg)) = [](auto val) { return val.val; },
        pattern(as<IndirectVal>(arg)) = [&](auto val) -> llvm::Value* {
            // If we have a pointer to the value, load from it.
            return builder.CreateLoad(val.val);
        }
    );
}
llvm::Value* LLVMGenerator::toIndirect(CodeGenVal val) {
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
llvm::Type* LLVMGenerator::toLLVMTy(Type type) {
    return match(type.data)(
        pattern(as<IntLitVariable>(_)) = []() -> llvm::Type* {
            panic("Encountered type variable");
            return nullptr;
        },
        pattern(as<ErrorTy>(_)) = []() -> llvm::Type* {
            panic("Encountered error type");
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
void LLVMGenerator::visitDeclPrototype(Decl* decl) {
    if(auto expr = decl->expression()) {
        if(decl->isVar) {
            declVals[decl] = builder.CreateAlloca(toLLVMTy(decl->type), 0, decl->name.getText().cString());
        } else {
            declVals[decl] = nullptr;
        }
    } else {
        std::vector<llvm::Type*> arguments;
        for(auto& param: decl->paramList) {
            arguments.push_back(toLLVMTy(param->type));
        }
        llvm::Type* type = toLLVMTy(decl->type);
        if(decl->isVar) {
            declVals[decl] = new llvm::GlobalVariable(*module, type, false, llvm::GlobalVariable::ExternalLinkage, nullptr, decl->name.getText().string());
        } else {
            llvm::FunctionType* functionTy = llvm::FunctionType::get(type,
                                                                     arguments,
                                                                     false);
            llvm::Function* function = llvm::Function::Create(functionTy, llvm::Function::ExternalLinkage, decl->name.getText().string(), module);

            int i = 0;
            for(auto &arg: function->args()) {
                arg.setName(decl->paramList[i++]->name.getText().string());
            }
            declVals[decl] = function;
        }
    }
}
CodeGenVal LLVMGenerator::visitDecl(Decl* decl) {
    if(auto expr = decl->expression()) {
        llvm::Value* val;
        if(decl->isVar) {
            val = declVals.at(decl);
            builder.CreateStore(toDirect(visitExpr(expr)), val);
        } else {
            val = declVals.at(decl) = toDirect(visitExpr(expr));
        }
        return DirectVal { val };
    } else if(decl->isVar) {
        return DirectVal { declVals.at(decl) };
    } else {
        llvm::Function* function = static_cast<llvm::Function*>(declVals.at(decl));
        if(!decl->hasDefinition()) return DirectVal { function };

        llvm::BasicBlock* block = llvm::BasicBlock::Create(context, "entry", function);
        builder.SetInsertPoint(block);

        for(auto [param, funcArg]: zip(decl->paramList, function->args())) {
            declVals[param] = &funcArg;
        }

        functionStack.push(function);
        /*boyd*/ auto body /*and glass*/ = decl->body();
        auto scopeVal = visitScope(body);
        auto terminalExpr = body->terminalExpr;
        if(!terminalExpr) {
            builder.CreateRetVoid();
        } else if(!dynamic_cast<ReturnExpr*>(terminalExpr)) {
            // If we called a function that never returns (which technically doesn't exist yet),
            // terminate the block with an unreachable.
            if(terminalExpr->type == NeverTy()) {
                builder.CreateUnreachable();
            } else {
                builder.CreateRet(toDirect(scopeVal));
            }
        }
        functionStack.pop();

        llvm::verifyFunction(*function);

        return DirectVal { function };
    }
}
CodeGenVal LLVMGenerator::visitStructDecl(StructDecl* decl) {
    return DirectVal { nullptr };
}
CodeGenVal LLVMGenerator::visitScope(Scope* scope) {
    for(auto node: scope->nodes) {
        CodeGenVal nodeCodeGenVal = DirectVal { nullptr };
        if(auto decl = dynamic_cast<Decl*>(node)) {
            visitDeclPrototype(decl);
            nodeCodeGenVal = visitDecl(decl);
        } else {
            nodeCodeGenVal = visit(node);
        }
        if(scope->terminalExpr == node) {
            return nodeCodeGenVal;
        }
    }
    return DirectVal { nullptr };
}
CodeGenVal LLVMGenerator::visitIntegerLiteralExpr(IntegerLiteralExpr* expr) {
    return DirectVal { llvm::ConstantInt::get(context, llvm::APInt(32, expr->literal)) };
}
CodeGenVal LLVMGenerator::visitDecimalLiteralExpr(DecimalLiteralExpr* expr) {
    return DirectVal { llvm::ConstantFP::get(context, llvm::APFloat(std::stod(expr->literal))) };
}
CodeGenVal LLVMGenerator::visitBooleanLiteralExpr(BooleanLiteralExpr* expr) {
    return DirectVal { llvm::ConstantInt::get(context, llvm::APInt(1, expr->literal ? -1 : 0)) };
}
CodeGenVal LLVMGenerator::visitCharLiteralExpr(CharLiteralExpr* expr) {
    return DirectVal { llvm::ConstantInt::get(context, llvm::APInt(8, expr->literal)) };
}
CodeGenVal LLVMGenerator::visitStringLiteralExpr(StringLiteralExpr* expr) {
    return DirectVal { builder.CreateGlobalStringPtr(expr->literal) };
}
CodeGenVal LLVMGenerator::visitPreOpExpr(PreOpExpr* expr) {
    auto operand = visitExpr(expr->operand);
    switch(expr->op) {
        case PreOp::Positive: return operand;
        case PreOp::Negative: return DirectVal { builder.CreateNeg(toDirect(operand)) };
        case PreOp::Deref: return IndirectVal { toDirect(operand) };
        case PreOp::Not: return DirectVal { builder.CreateNot(toDirect(operand)) };
        case PreOp::AddrOf: return DirectVal { toIndirect(operand) };
    }
}
CodeGenVal LLVMGenerator::visitBinOpExpr(BinOpExpr* expr) {
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
                panic("Can't add values of those types");
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
                panic("Can't subtract values of that type");
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
                panic("Can't multiply values of that type");
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
                panic("Can't divide values of that type");
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
                panic("Can't modulo values of that type");
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
                default: unreachable;
            }
            return DirectVal { builder.CreateStore(value, lhsInd) };
    }
}
CodeGenVal LLVMGenerator::visitCastExpr(CastExpr* expr) {
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
        pattern(as<PointerTy>(_), as<PointerTy>(arg)) = [&](auto rhsTy) {
            return DirectVal { builder.CreateBitCast(toDirect(ogValue), destTypeLLVM) };
        },
        pattern(as<PointerTy>(_), _) = []() -> DirectVal {
            panic("pointer casting is not yet supported");
        },
        pattern(_, as<PointerTy>(_)) = []() -> DirectVal {
            panic("pointer casting is not yet supported");
        }
    );
}
CodeGenVal LLVMGenerator::visitDeclRefExpr(DeclRefExpr* expr) {
    auto referencedVal = declVals.at(expr->decl);
    if(expr->decl->isComputed()) {
        auto callee = static_cast<llvm::Function*>(referencedVal);
        assertEqualMessage(
			callee->arg_size(), expr->argList.size(),
            "Incorrect number of arguments passed to function"
		);

        std::vector<llvm::Value*> args;
        for(auto* arg: expr->argList) {
            args.push_back(toDirect(visitExpr(arg)));
            if(!args.back()) {
                return DirectVal { nullptr };
            }
        }

        return DirectVal { builder.CreateCall(callee, args) };
    } else {
        if(expr->decl->isVar) {
            return IndirectVal { referencedVal };
        } else {
            return DirectVal { referencedVal };
        }
    }
}
CodeGenVal LLVMGenerator::visitMemberRefExpr(MemberRefExpr* expr) {
    llvm::Value* root = toIndirect(visitExpr(expr->root));
    auto firstIndex = llvm::ConstantInt::get(context, llvm::APInt(32, 0));
    auto index = llvm::ConstantInt::get(context, llvm::APInt(32, expr->declIndex));
    std::vector<llvm::Value*> indices { firstIndex, index };
    auto addr = builder.CreateGEP(root, indices);
    return IndirectVal { addr };
}
CodeGenVal LLVMGenerator::visitReturnExpr(ReturnExpr* expr) {
    if(!expr->value) {
        return DirectVal { builder.CreateRetVoid() };
    } else {
        return DirectVal { builder.CreateRet(toDirect(visitExpr(expr->value))) };
    }
}
CodeGenVal LLVMGenerator::visitIfExpr(IfExpr* expr) {
    auto thenBlock = llvm::BasicBlock::Create(context, "if.then", functionStack.top());
    llvm::BasicBlock* elseBlock = nullptr;
    if(expr->elseNode) {
        elseBlock = llvm::BasicBlock::Create(context, "if.else", functionStack.top());
    }
    auto endBlock = llvm::BasicBlock::Create(context, "if.end", functionStack.top());
    builder.CreateCondBr(toDirect(visitExpr(expr->condition)), thenBlock, expr->elseNode ? elseBlock : endBlock);

    builder.SetInsertPoint(thenBlock);
    auto thenVal = visitScope(expr->thenScope);
    thenBlock = builder.GetInsertBlock();
    Type thenType = VoidTy();
    if(auto terminalExpr = expr->thenScope->terminalExpr) {
        thenType = terminalExpr->type;
        if(!dynamic_cast<ReturnExpr*>(expr->thenScope->terminalExpr)) {
            // If we called a function that never returns (which technically doesn't exist yet),
            // terminate the block with an unreachable.
            if(terminalExpr->type == NeverTy()) {
                builder.CreateUnreachable();
            } else {
                builder.CreateBr(endBlock);
            }
        }
    } else {
        builder.CreateBr(endBlock);
    }

    Type elseType = VoidTy();
    CodeGenVal elseVal = DirectVal { nullptr };
    if(expr->elseNode) {
        builder.SetInsertPoint(elseBlock);
        match(expr->elseNode)(
            pattern(some(as<Scope*>(arg))) = [&](auto scope) {
                elseVal = visitScope(scope);
                elseBlock = builder.GetInsertBlock();
                if(auto terminalExpr = scope->terminalExpr) {
                    elseType = terminalExpr->type;
                    if(!dynamic_cast<ReturnExpr*>(terminalExpr)) {
                        // If we called a function that never returns (which technically doesn't exist yet),
                        // terminate the block with an unreachable.
                        if(terminalExpr->type == NeverTy()) {
                            builder.CreateUnreachable();
                        } else {
                            builder.CreateBr(endBlock);
                        }
                    }
                } else {
                    builder.CreateBr(endBlock);
                }
            },
            pattern(some(as<IfExpr*>(arg))) = [&](auto expr) {
                elseVal = visitIfExpr(expr);
                elseBlock = builder.GetInsertBlock();
                elseType = expr->type;
                builder.CreateBr(endBlock);
            },
            pattern(none) = []{}
        );
    }

    builder.SetInsertPoint(endBlock);
    if(expr->type.isConvertibleTo(VoidTy())) {
        return DirectVal { nullptr };
    } else {
        llvm::PHINode* phi = builder.CreatePHI(toLLVMTy(expr->type), 2);
        if(!thenType.isConvertibleTo(VoidTy())) {
            phi->addIncoming(toDirect(thenVal), thenBlock);
        }
        if(!elseType.isConvertibleTo(VoidTy())) {
            phi->addIncoming(toDirect(elseVal), elseBlock);
        }
        return DirectVal { phi };
    }
}
CodeGenVal LLVMGenerator::visitWhileExpr(WhileExpr* expr) {
    auto checkBlock = llvm::BasicBlock::Create(context, "while.check", functionStack.top());
    auto thenBlock = llvm::BasicBlock::Create(context, "while.then", functionStack.top());
    auto endBlock = llvm::BasicBlock::Create(context, "while.end", functionStack.top());
    builder.CreateBr(checkBlock);

    builder.SetInsertPoint(checkBlock);
    builder.CreateCondBr(toDirect(visitExpr(expr->condition)), thenBlock, endBlock);

    builder.SetInsertPoint(thenBlock);
    visitScope(expr->thenScope);
    // If the last node in thenScope is a return expression, we need to avoid creating
    // a branch after it because a basic block can only have one terminal instruction.
    // http://llvm.org/doxygen/classllvm_1_1BasicBlock.html
    if(expr->thenScope->nodes.empty() ||
       !dynamic_cast<ReturnExpr*>(expr->thenScope->nodes.back())) {
        builder.CreateBr(checkBlock);
    }

    builder.SetInsertPoint(endBlock);

    return DirectVal { nullptr };
}
CodeGenVal LLVMGenerator::visitDoExpr(DoExpr* expr) {
    return match(expr->value)(
        pattern(as<Expr*>(arg)) = [&](auto expr) -> CodeGenVal {
            visitExpr(expr);
            return DirectVal { nullptr };
        },
        pattern(as<Scope*>(arg)) = [&](auto scope) -> CodeGenVal {
            return visitScope(scope);
        }
    );
}
void LLVMGenerator::outputObjectFile(char const* fileName) const {
    // Initialize the target registry etc.
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    auto triple = llvm::sys::getDefaultTargetTriple();
    module->setTargetTriple(triple);

    std::string error;
    auto target = llvm::TargetRegistry::lookupTarget(triple, error);

    // Print an error and exit if we couldn't find the requested target.
    // This generally occurs if we've forgotten to initialise the
    // TargetRegistry or we have a bogus target triple.
    if (!target) {
        llvm::errs() << error;
        return;
    }

    auto cpu = "generic";

    llvm::TargetOptions opt;
    auto relocationModel = llvm::Optional<llvm::Reloc::Model>();
    auto machine =
    target->createTargetMachine(triple, cpu, /*features=*/ "", opt, relocationModel);

    module->setDataLayout(machine->createDataLayout());

    std::error_code errorCode;
    llvm::raw_fd_ostream dest(fileName, errorCode, llvm::sys::fs::F_None);

    if(errorCode) {
        llvm::errs() << "Could not open file: " << errorCode.message();
        return;
    }

    llvm::legacy::PassManager pass;
    auto fileType = llvm::TargetMachine::CGFT_ObjectFile;

    if(machine->addPassesToEmitFile(pass, dest, fileType)) {
        llvm::errs() << "Machine can't emit object files.";
        return;
    }

    pass.run(*module);
    dest.flush();
}

void LLVMGenerator::printIR() const {
    module->print(llvm::errs(), nullptr);
}
