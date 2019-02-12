//  Copyright © 2019 Zach Wolfe. All rights reserved.

#include "LIRGenerator.h"
#include "mpark/patterns.hpp"

using namespace mpark::patterns;
using namespace lir;

lir::Function& LIRGenerator::currentFunction() {
    return program.functions[insertionState.last()->function];
}
lir::BasicBlock& LIRGenerator::currentBasicBlock() {
    return currentFunction().basicBlocks[insertionState.last()->basicBlock];
}
lir::BB LIRGenerator::createBasicBlock() {
    auto& basicBlocks = currentFunction().basicBlocks;
    basicBlocks.append({});
    return basicBlocks.count() - 1;
}
void LIRGenerator::setBasicBlock(lir::BB bb) {
    insertionState.last()->basicBlock = bb;
}
Operand LIRGenerator::U64Constant(uint64_t constant) {
    auto& func = currentFunction();

    Value val { 64 / 8 };
    val.u64 = constant;
    func.constants.append(val);

    Operand operand { Operand::Constant };
    operand.constant = func.constants.count() - 1;

    return operand;
}
Operand LIRGenerator::U32Constant(uint32_t constant) {
    auto& func = currentFunction();

    Value val { 32 / 8 };
    val.u32 = constant;
    func.constants.append(val);

    Operand operand { Operand::Constant };
    operand.constant = func.constants.count() - 1;

    return operand;
}
Operand LIRGenerator::U16Constant(uint16_t constant) {
    auto& func = currentFunction();

    Value val { 16 / 8 };
    val.u16 = constant;
    func.constants.append(val);

    Operand operand { Operand::Constant };
    operand.constant = func.constants.count() - 1;

    return operand;
}
Operand LIRGenerator::U8Constant(uint16_t constant) {
    auto& func = currentFunction();

    Value val { 8 / 8 };
    val.u8 = constant;
    func.constants.append(val);

    Operand operand { Operand::Constant };
    operand.constant = func.constants.count() - 1;

    return operand;
}
Operand LIRGenerator::globalStringConstant(char const* data, uint64_t size) {
    MemoryLoc loc { MemoryLoc::GlobalConstant, program.constants.count() };
    // TODO: Add a method to Array for quickly appending from a buffer.
    program.constants.reserve(program.constants.count() + size + 1);
    for(uint64_t i: Range<uint64_t> {0, size}) {
        program.constants.append(data[i]);
    }
    program.constants.append(0);
    Operand operand { Operand::Location };
    operand.location = loc;
    return operand;
}

MemoryLoc LIRGenerator::variable(Type& type) {
    auto& func = currentFunction();
    // TODO: alignment!
    MemoryLoc loc { MemoryLoc::StackFrame, func.frameSize };
    func.frameSize += type.layout().size;
    return loc;
}
MemoryLoc LIRGenerator::global(Value initialValue) {
    MemoryLoc loc { MemoryLoc::GlobalVariable, program.globals.count() };
    // TODO: Add a method to Array for quickly appending from a buffer.
    program.globals.reserve(program.globals.count() + initialValue.size);
    uint8_t* buf;
    if(initialValue.size <= 64 / 8) {
        buf = reinterpret_cast<uint8_t*>(&initialValue.u64);
    } else {
        buf = initialValue.data;
    }
    for(uint64_t i: Range<uint64_t> {0, initialValue.size}) {
        program.globals.append(buf[i]);
    }
    return loc;
}

Func LIRGenerator::beginFunction(std::string name, Type& returnType, bool isExtern) {
    Function func {};
    func.name = name;
    func.returnValueSize = returnType.layout().size;
    func.isExtern = isExtern;
    func.basicBlocks.append({});
    program.functions.append(func);
    Func function = program.functions.count() - 1;
    insertionState.append({function, 0});
    return function;
}

void LIRGenerator::endFunction() {
    insertionState.removeLast();
}

void LIRGenerator::twoAddressCode(OpCode op, MemoryLoc dest, Operand operand, Type& meaningfulType) {
    Instruction instr {};
    instr.op = op;
    instr.dest = dest;
    instr.operand = operand;
    instr.size = meaningfulType.layout().size;
    currentBasicBlock().instructions.append(instr);
}
void LIRGenerator::threeAddressCode(OpCode op, MemoryLoc dest, Operand operandA, Operand operandB, Type& meaningfulType) {
    Instruction instr {};
    instr.op = op;
    instr.dest = dest;
    instr.operands = { operandA, operandB };
    instr.size = meaningfulType.layout().size;
    currentBasicBlock().instructions.append(instr);
}
// TODO: the xExtend methods can be removed and written in terms of twoAddressCode by adding a destType pointer parameter to it with a default value of nullptr.
void LIRGenerator::zeroExtend(MemoryLoc dest, Type& destType, Operand operand, Type& operandType) {
    Instruction instr {};
    instr.op = OpCode::ZeroExtend;
    instr.dest = dest;
    instr.operand = operand;
    instr.destSize = destType.layout().size;
    instr.size = operandType.layout().size;
    currentBasicBlock().instructions.append(instr);
}
void LIRGenerator::signExtend(MemoryLoc dest, Type& destType, Operand operand, Type& operandType) {
    Instruction instr {};
    instr.op = OpCode::SignExtend;
    instr.dest = dest;
    instr.operand = operand;
    instr.destSize = destType.layout().size;
    instr.size = operandType.layout().size;
    currentBasicBlock().instructions.append(instr);
}
void LIRGenerator::branch(lir::BB branch) {
    Instruction instr {};
    instr.op = OpCode::Branch;
    instr.branch = branch;
    currentBasicBlock().instructions.append(instr);
}
void LIRGenerator::condBranch(Operand condition, lir::BB trueBranch, lir::BB falseBranch) {
    Instruction instr {};
    instr.op = OpCode::CondBranch;
    instr.trueBranch = trueBranch;
    instr.falseBranch = falseBranch;
    currentBasicBlock().instructions.append(instr);
}
void LIRGenerator::call(Func function, Array<Argument> arguments) {
    Instruction instr {};
    instr.op = OpCode::Call;
    instr.function = function;
    instr.arguments = arguments;
    currentBasicBlock().instructions.append(instr);
}
void LIRGenerator::returnValue(Operand operand, Type& type) {
    Instruction instr {};
    instr.op = OpCode::Return;
    instr.operand = operand;
    instr.size = type.layout().size;
    currentBasicBlock().instructions.append(instr);
}
void LIRGenerator::returnVoid() {
    Instruction instr {};
    instr.op = OpCode::ReturnVoid;
    currentBasicBlock().instructions.append(instr);
}
void LIRGenerator::unreachableInstr() {
    Instruction instr {};
    instr.op = OpCode::Unreachable;
    currentBasicBlock().instructions.append(instr);
}

void LIRGenerator::visit(Array<ASTNode*> const& nodes) {
    for(auto node: nodes) {
        ASTVisitor::visit(node);
    }
}

DeclVal LIRGenerator::visitDecl(Decl* decl) {
    auto valIt = declMap.find(decl);
    if(valIt != declMap.end()) return valIt->second;

    std::optional<DeclVal> retVal;
    if(decl->isExtern()) {
        if(decl->isVar) {
            retVal = program.appendExternGlobal(decl->type.layout().size);
        } else {
            Function function {};
            function.isExtern = true;
            function.name = decl->name.getText().string();
            retVal = program.appendFunction(function);
        }
    } else if(auto expr = decl->expression()) {
        Var var;
        ROperand val = visitExpr(decl->expression());
        if(functionStack.isEmpty()) {
            assert(val.kind == ROperand::LocalConstant);
            var = program.appendGlobal(val.localConstant);
        } else {
            Function& curFunction = program.functions[*functionStack.last()];
            var = curFunction.appendVariable({decl->type.layout().size});
            Instruction copy {};
            copy.op = OpCode::Copy;
            copy.dest = mutableVariableOperand(var);
            copy.operand = val;
            curFunction.appendInstruction(copy);
        }

        retVal = var;
    } else {
        Function function {};
        function.name = decl->name.getText().string();
        for(auto param: decl->paramList) {
            declMap[param] = function.appendVariable({param->type.layout().size});
        }
        Func func = program.appendFunction(function);
        functionStack.append(func);
        declMap[decl] = func;
        visitScope(decl->body());

        retVal = functionStack.removeLast();
    }
    declMap[decl] = *retVal;
    return *retVal;
}
std::optional<ROperand> LIRGenerator::visitScope(Scope* scope) {
    std::optional<ROperand> terminalValue;
    for(auto node: scope->nodes) {
        if(auto expr = dynamic_cast<Expr*>(node)) {
            terminalValue = visitExpr(expr);
        } else if(auto decl = dynamic_cast<Decl*>(node)) {
            visitDecl(decl);
        }
    }
    return terminalValue;
}
ROperand LIRGenerator::visitIntegerLiteralExpr(IntegerLiteralExpr* expr) {
    Value constant {};
    match(expr->type.data)(
        pattern(as<IntTy>(arg)) = [&](auto ty) {
            // Note: we don't have to worry about signedness because integer literal
            // expressions are always positive.
            switch(ty.bitWidth) {
                case 8: {
                    constant.u8 = expr->literal;
                } break;
                case 16: {
                    constant.u16 = expr->literal;
                } break;
                case 32: {
                    constant.u32 = expr->literal;
                } break;
                case 64: {
                    constant.u64 = expr->literal;
                } break;
                default: {
                    panic("Invalid integer literal bitWidth");
                } break;
            }
            constant.size = ty.bitWidth / 8;
        },
        pattern(as<FloatTy>(arg)) = [&](auto ty) {
            constant.size = 32 / 8;
            constant.f32 = expr->literal;
        },
        pattern(as<DoubleTy>(arg)) = [&](auto ty) {
            constant.size = 64 / 8;
            constant.f64 = expr->literal;
        },
        pattern(_) = []() { panic("Invalid integer literal expression"); }
    );
    return localConstantOperand(constant);
}
ROperand LIRGenerator::visitDecimalLiteralExpr(DecimalLiteralExpr* expr) {
    Value constant {};
    match(expr->type.data)(
        pattern(as<FloatTy>(arg)) = [&](auto ty) {
            constant.size = 32 / 8;
            constant.f32 = strtof(expr->literal.c_str(), nullptr);
        },
        pattern(as<DoubleTy>(arg)) = [&](auto ty) {
            constant.size = 64 / 8;
            constant.f64 = strtod(expr->literal.c_str(), nullptr);
        },
        pattern(_) = []() { panic("Invalid decimal literal expression"); }
    );

    return localConstantOperand(constant);
}
ROperand LIRGenerator::visitBooleanLiteralExpr(BooleanLiteralExpr* expr) {
    Value constant {};
    constant.size = 1;
    constant.boolean = expr->literal;

    return localConstantOperand(constant);
}
ROperand LIRGenerator::visitCharLiteralExpr(CharLiteralExpr* expr) {
    Value constant {};
    constant.size = 1;
    constant.u8 = expr->literal;

    return localConstantOperand(constant);
}
ROperand LIRGenerator::visitStringLiteralExpr(StringLiteralExpr* expr) {
    Value constant {};
    uint32_t size = expr->literal.size() + 1;
    constant.size = size;
    constant.buffer = new uint8_t[size];
    memcpy(constant.buffer, expr->literal.c_str(), size);

    return globalConstantOperand(program.appendConstant(constant));
}
ROperand LIRGenerator::visitDoExpr(DoExpr* expr) {
    return {};
}
ROperand LIRGenerator::visitPreOpExpr(PreOpExpr* expr) {
    Function& func = program.functions[*functionStack.last()];
    auto operand = visitExpr(expr->operand);
    Instruction instr {};
    instr.operand = operand;
    switch(expr->op) {
        case PreOp::Positive: {
            return operand;
        } break;
        case PreOp::Negative: {
            instr.op = OpCode::Negative;
            instr.dest = mutableVariableOperand(func.appendVariable({operand.size}));
        } break;
        case PreOp::Deref: {
            instr.op = OpCode::Load;
            instr.dest = mutableVariableOperand(func.appendVariable({expr->operand->type.layout().size}));
        } break;
        case PreOp::AddrOf: {
            instr.op = OpCode::GetAddress;
            instr.dest = mutableVariableOperand(func.appendVariable({operand.size}));
        } break;
        case PreOp::Not: {
            instr.op = OpCode::LogicalNot;
            instr.dest = mutableVariableOperand(func.appendVariable({1}));
        } break;
    }
    func.appendInstruction(instr);
    return variableOperand(instr.dest);
}
RWOperand LIRGenerator::visitPreOpExprAsLValue(PreOpExpr* expr) {
    switch(expr->op) {
        case PreOp::Positive:
        case PreOp::Negative:
        case PreOp::AddrOf:
        case PreOp::Not: {
            panic("Pre-op expressions other than pointer dereferences can not be taken as lvalues");
        } break;
        case PreOp::Deref: {
            auto op = visitExpr(expr->operand);
            assert(op.kind == ROperand::Variable);
            return mutableVariableOperand(op.variable);
        } break;
    }
}
ROperand LIRGenerator::visitBinOpExpr(BinOpExpr* expr) {
    Function& func = program.functions[*functionStack.last()];
    ROperand rvalueA, rvalueB;
    if(expr->op != BinOp::Assignment) {
        rvalueA = visitExpr(expr->lhs);
    }
    rvalueB = visitExpr(expr->rhs);

    Instruction instr {};
    switch(expr->op) {
        case BinOp::Assignment:
        case BinOp::AddAssignment:
        case BinOp::SubAssignment:
        case BinOp::MultAssignment:
        case BinOp::ModAssignment:
        case BinOp::AndAssignment:
        case BinOp::OrAssignment:
        case BinOp::DivAssignment: {
            instr.dest = visitExprAsLValue(expr->lhs);
            if(expr->op == BinOp::Assignment) {
                instr.op = OpCode::Copy;
                instr.operand = rvalueB;
                func.appendInstruction(instr);
                return variableOperand(instr.dest);
            } else {
                instr.operands = { rvalueA, rvalueB };
            }
        } break;
        case BinOp::Add:
        case BinOp::Sub:
        case BinOp::Mult:
        case BinOp::Div:
        case BinOp::Mod:
        case BinOp::BitwiseAnd:
        case BinOp::BitwiseOr: {
            instr.dest = mutableVariableOperand(func.appendVariable({rvalueA.size}));
            instr.operands = { rvalueA, rvalueB };
        } break;
        case BinOp::Equal:
        case BinOp::NotEqual:
        case BinOp::LessThanOrEqual:
        case BinOp::LessThan:
        case BinOp::GreaterThanOrEqual:
        case BinOp::GreaterThan:
        case BinOp::Or:
        case BinOp::And: {
            instr.dest = mutableVariableOperand(func.appendVariable({1}));
            instr.operands = { rvalueA, rvalueB };
        } break;
    }

    auto getPointerOffset = [&](PointerTy pointerTy, IntTy intTy) -> ROperand {
        Var offset = func.appendVariable({sizeof(uint8_t*)});
        ROperand offsetOperand = variableOperand(offset);
        Instruction getOffset {};
        getOffset.dest = mutableVariableOperand(offset);
        getOffset.operand = rvalueB;

        auto pointerBitWidth = sizeof(uint8_t*) * 8;
        assertTrueMessage(intTy.bitWidth <= pointerBitWidth, "Can't perform pointer arithmetic where the int type is bigger than the pointer type");
        if(intTy.bitWidth < pointerBitWidth) {
            switch(intTy.signedness) {
                case Signedness::Signed:
                    getOffset.op = OpCode::SignExtend;
                    break;
                case Signedness::Unsigned:
                    getOffset.op = OpCode::ZeroExtend;
                    break;
            }
        } else {
            getOffset.op = OpCode::Copy;
        }
        func.appendInstruction(getOffset);

        Value stride {};
        stride.size = sizeof(uint8_t*);
        assertEqual(stride.size, 64 / 8);
        stride.u64 = pointerTy.pointedTy->layout().stride();

        Instruction mult {};
        mult.op = OpCode::Mult;
        mult.dest = mutableVariableOperand(offset);
        mult.operands = { variableOperand(offset), localConstantOperand(stride) };
        func.appendInstruction(mult);

        return offsetOperand;
    };

    switch(expr->op) {
        case BinOp::Assignment: unreachable;

        case BinOp::AddAssignment:
        case BinOp::Add: {
            match(expr->lhs->type.data, expr->rhs->type.data)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    switch(ty.signedness) {
                        case Signedness::Signed:
                            instr.op = OpCode::WrappingAdd;
                            break;
                        case Signedness::Unsigned:
                            instr.op = OpCode::WrappingAdd;
                    }
                },
                pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                    instr.op = OpCode::FAdd;
                },
                pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                    instr.op = OpCode::FAdd;
                },
                pattern(as<PointerTy>(arg), as<IntTy>(arg)) = [&](auto pointerTy, auto intTy) {
                    instr.op = OpCode::WrappingAdd;
                    instr.operands.b = getPointerOffset(pointerTy, intTy);
                },
                pattern(_, _) = [] {
                    panic("Can't add values of non-pointer, non-integer, non-floating point types");
                }
            );
        } break;
        case BinOp::SubAssignment:
        case BinOp::Sub: {
            match(expr->lhs->type.data, expr->rhs->type.data)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    switch(ty.signedness) {
                        case Signedness::Signed:
                            instr.op = OpCode::WrappingSub;
                            break;
                        case Signedness::Unsigned:
                            instr.op = OpCode::WrappingSub;
                    }
                },
                pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                    instr.op = OpCode::FSub;
                },
                pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                    instr.op = OpCode::FSub;
                },
                pattern(as<PointerTy>(arg), as<IntTy>(arg)) = [&](auto pointerTy, auto intTy) {
                    instr.op = OpCode::WrappingSub;
                    instr.operands.b = getPointerOffset(pointerTy, intTy);
                },
                // Pointer subtraction should be supported, but we need to divide by the alignment and I don't feel
                // like writing that code right now.
//              pattern(as<PointerTy>(arg), as<PointerTy>(arg)) = [&](auto p1, auto p2) {
//                  instr.op = OpCode::WrappingSub;
//              },
                pattern(_, _) = [] {
                    panic("Can't subtract values of non-pointer, non-integer, non-floating point types");
                }
            );
        } break;
        case BinOp::MultAssignment:
        case BinOp::Mult: {
            match(expr->lhs->type.data, expr->rhs->type.data)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    instr.op = OpCode::Mult;
                },
                pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                    instr.op = OpCode::FMult;
                },
                pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                    instr.op = OpCode::FMult;
                },
                pattern(_, _) = [] {
                    panic("Can't multiply values of non-integer, non-floating point types");
                }
            );
        } break;
        case BinOp::DivAssignment:
        case BinOp::Div: {
            match(expr->lhs->type.data, expr->rhs->type.data)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    instr.op = OpCode::Div;
                },
                pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                    instr.op = OpCode::FDiv;
                },
                pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                    instr.op = OpCode::FDiv;
                },
                pattern(_, _) = [] {
                    panic("Can't divide values of non-integer, non-floating point types");
                }
            );
        } break;
        case BinOp::ModAssignment:
        case BinOp::Mod: {
            match(expr->lhs->type.data, expr->rhs->type.data)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    instr.op = OpCode::Mod;
                },
                pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                    instr.op = OpCode::FMod;
                },
                pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                    instr.op = OpCode::FMod;
                },
                pattern(_, _) = [] {
                    panic("Can't mod values of non-integer, non-floating point types");
                }
            );
        } break;
        case BinOp::LessThanOrEqual: {
            match(expr->lhs->type.data, expr->rhs->type.data)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    instr.op = OpCode::LTE;
                },
                pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                    instr.op = OpCode::FLTE;
                },
                pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                    instr.op = OpCode::FLTE;
                },
                pattern(_, _) = [] {
                    panic("Can't compare values of non-integer, non-floating point types");
                }
            );
        } break;
        case BinOp::LessThan: {
            match(expr->lhs->type.data, expr->rhs->type.data)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    instr.op = OpCode::LT;
                },
                pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                    instr.op = OpCode::FLT;
                },
                pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                    instr.op = OpCode::FLT;
                },
                pattern(_, _) = [] {
                    panic("Can't compare values of non-integer, non-floating point types");
                }
            );
        } break;
        case BinOp::GreaterThanOrEqual: {
            match(expr->lhs->type.data, expr->rhs->type.data)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    instr.op = OpCode::GTE;
                },
                pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                    instr.op = OpCode::FGTE;
                },
                pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                    instr.op = OpCode::FGTE;
                },
                pattern(_, _) = [] {
                    panic("Can't compare values of non-integer, non-floating point types");
                }
            );
        } break;
        case BinOp::GreaterThan: {
            match(expr->lhs->type.data, expr->rhs->type.data)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    instr.op = OpCode::GT;
                },
                pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                    instr.op = OpCode::FGT;
                },
                pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                    instr.op = OpCode::FGT;
                },
                pattern(_, _) = [] {
                    panic("Can't compare values of non-integer, non-floating point types");
                }
            );
        } break;
        case BinOp::AndAssignment:
        case BinOp::BitwiseAnd: {
            match(expr->lhs->type.data, expr->rhs->type.data)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    instr.op = OpCode::BitwiseAnd;
                },
                pattern(_, _) = [] {
                    panic("Can't bitwise and values of non-integer types");
                }
            );
        } break;
        case BinOp::OrAssignment:
        case BinOp::BitwiseOr: {
            match(expr->lhs->type.data, expr->rhs->type.data)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    instr.op = OpCode::BitwiseOr;
                },
                pattern(_, _) = [] {
                    panic("Can't bitwise or values of non-integer types");
                }
            );
        } break;
        case BinOp::And: {
            match(expr->lhs->type.data, expr->rhs->type.data)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    instr.op = OpCode::LogicalAnd;
                },
                pattern(_, _) = [] {
                    panic("Can't logical and values of non-integer types");
                }
            );
        } break;
        case BinOp::Or: {
            match(expr->lhs->type.data, expr->rhs->type.data)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    instr.op = OpCode::LogicalOr;
                },
                pattern(_, _) = [] {
                    panic("Can't logical or values of non-integer types");
                }
            );
        } break;
        case BinOp::Equal: {
            // TODO: Handle (at least) struct and floating point types in a special way
            instr.op = OpCode::Equal;
        } break;
        case BinOp::NotEqual: {
            // TODO: Handle (at least) struct and floating point types in a special way
            instr.op = OpCode::NotEqual;
        } break;
    }
    func.appendInstruction(instr);
    return variableOperand(instr.dest);
}
ROperand LIRGenerator::visitCastExpr(CastExpr* expr) {
    return {};
}
ROperand LIRGenerator::visitDeclRefExpr(DeclRefExpr* expr) {
    auto val = visitDecl(expr->decl);
    Function& curFunc = program.functions[*functionStack.last()];
    Instruction instr {};
    instr.dest = mutableVariableOperand(
        curFunc.appendVariable({expr->type.layout().size})
    );

    match(val)(
        pattern(as<Var>(arg)) = [&](auto var) {
            instr.op = OpCode::Copy;
            instr.operand = variableOperand(var);
        },
        pattern(as<Func>(arg)) = [&](auto func) {
            instr.op = OpCode::Call;
            instr.function = func;
            Array<ROperand> arguments {};
            arguments.reserve(expr->arguments.count());
            for(auto arg: expr->arguments) {
                auto argument = visitExpr(arg);
                arguments.append(argument);
            }
            
            instr.arguments = arguments;
        }
    );

    curFunc.appendInstruction(instr);

    ROperand retVal {};
    retVal.kind = ROperand::Variable;
    retVal.variable = instr.dest.variable;
    retVal.offset = instr.dest.offset;
    retVal.size = instr.dest.size;

    return retVal;
}
RWOperand LIRGenerator::visitDeclRefExprAsLValue(DeclRefExpr* expr) {
    auto val = visitDecl(expr->decl);

    return mutableVariableOperand(std::get<Var>(val));
}
ROperand LIRGenerator::visitMemberRefExpr(MemberRefExpr* expr) {
    Expr* root = expr->root;
    ROperand member = visitExpr(root);
    member.size = expr->type.layout().size;
    member.offset += root->type.layout().fieldOffsets[expr->declIndex];
    return member;
}
RWOperand LIRGenerator::visitMemberRefExprAsLValue(MemberRefExpr* expr) {
    Expr* root = expr->root;
    RWOperand member = visitExprAsLValue(root);
    member.size = expr->type.layout().size;
    member.offset += root->type.layout().fieldOffsets[expr->declIndex];
    return member;
}

ROperand LIRGenerator::visitReturnExpr(ReturnExpr* expr) {
    return {};
}
ROperand LIRGenerator::visitIfExpr(IfExpr* expr) {
    return {};
}
ROperand LIRGenerator::visitWhileExpr(WhileExpr* expr) {
    return {};
}

void LIRGenerator::printIR() const {
    for(size_t j: program.functions.indices()) {
        Function const& function = program.functions[j];
        std::cout << function.name << ":\n";
        for(size_t i: function.instructions.indices()) {
            Instruction const& instruction = function.instructions[i];
            auto printVariable = [&](Var variable) {
                std::cout << (variable.isGlobal ? "G" : "L");
                std::cout << "V" << variable.index;
            };
            auto printRange = [&](auto& operand) {
                std::cout << "[" << operand.offset << "..<" << (operand.offset + operand.size) << "]";
            };
            auto printDest = [&] {
                printVariable(instruction.dest.variable);
                printRange(instruction.dest);
            };
            auto printROperand = [&](ROperand operand) {
                switch(operand.kind) {
                    case ROperand::LocalConstant:
                        std::cout << "LC";
                        break;
                    case ROperand::GlobalConstantAddress:
                        std::cout << "GC" << operand.globalConstant;
                        break;
                    case ROperand::Variable:
                        printVariable(operand.variable);
                        break;
                }
                printRange(operand);
            };
            auto printInstructionBeginning = [&](auto name) {
                printf("%04zu: ", i);
                std::cout << name << " ";
            };
            auto printTwoAddressCode = [&](auto name) {
                printInstructionBeginning(name);
                printDest();
                std::cout << ", ";
                printROperand(instruction.operand);
            };
            auto printThreeAddressCode = [&](auto name) {
                printInstructionBeginning(name);
                printDest();
                std::cout << ", ";
                printROperand(instruction.operands.a);
                std::cout << ", ";
                printROperand(instruction.operands.b);
            };
            std::cout << "    ";
            switch(instruction.op) {
                case OpCode::GetAddress:
                    printTwoAddressCode("GetAddress");
                    break;
                case OpCode::Load:
                    printTwoAddressCode("Load");
                    break;
                case OpCode::Store:
                    printTwoAddressCode("Store");
                    break;
                case OpCode::Copy:
                    printTwoAddressCode("Copy");
                    break;
                case OpCode::Negative:
                    printTwoAddressCode("Negative");
                    break;
                case OpCode::WrappingAdd:
                    printThreeAddressCode("WrappingAdd");
                    break;
                case OpCode::WrappingSub:
                    printThreeAddressCode("WrappingSub");
                    break;
                case OpCode::Mult:
                    printThreeAddressCode("Mult");
                    break;
                case OpCode::Div:
                    printThreeAddressCode("Div");
                    break;
                case OpCode::Mod:
                    printThreeAddressCode("Mod");
                    break;
                case OpCode::SignExtend:
                    printTwoAddressCode("SignExtend");
                    break;
                case OpCode::ZeroExtend:
                    printTwoAddressCode("ZeroExtend");
                    break;
                case OpCode::LTE:
                    printThreeAddressCode("LTE");
                    break;
                case OpCode::LT:
                    printThreeAddressCode("LT");
                    break;
                case OpCode::GTE:
                    printThreeAddressCode("GTE");
                    break;
                case OpCode::GT:
                    printThreeAddressCode("GT");
                    break;
                case OpCode::FLTE:
                    printThreeAddressCode("FLTE");
                    break;
                case OpCode::FLT:
                    printThreeAddressCode("FLT");
                    break;
                case OpCode::FGTE:
                    printThreeAddressCode("FGTE");
                    break;
                case OpCode::FGT:
                    printThreeAddressCode("FGT");
                    break;
                case OpCode::Equal:
                    printThreeAddressCode("Equal");
                    break;
                case OpCode::NotEqual:
                    printThreeAddressCode("NotEqual");
                    break;
                case OpCode::FAdd:
                    printThreeAddressCode("FAdd");
                    break;
                case OpCode::FSub:
                    printThreeAddressCode("FSub");
                    break;
                case OpCode::FMult:
                    printThreeAddressCode("FMult");
                    break;
                case OpCode::FDiv:
                    printThreeAddressCode("FDiv");
                    break;
                case OpCode::FMod:
                    printThreeAddressCode("FMod");
                    break;
                case OpCode::LogicalNot:
                    printTwoAddressCode("LogicalNot");
                    break;
                case OpCode::LogicalAnd:
                    printThreeAddressCode("LogicalAnd");
                    break;
                case OpCode::LogicalOr:
                    printThreeAddressCode("LogicalOr");
                    break;
                case OpCode::BitwiseAnd:
                    printThreeAddressCode("BitwiseAnd");
                    break;
                case OpCode::BitwiseOr:
                    printThreeAddressCode("BitwiseOr");
                    break;
                case OpCode::Branch:
                    printInstructionBeginning("Branch");
                    std::cout << instruction.branch;
                    break;
                case OpCode::CondBranch:
                    printInstructionBeginning("CondBranch");
                    printVariable(instruction.condition);
                    std::cout << ", " << instruction.branch;
                    break;
                case OpCode::Call:
                    printInstructionBeginning("Call");
                    std::cout << instruction.function << "(";
                    bool first = true;
                    for(auto& argument: instruction.arguments) {
                        if(first) {
                            first = false;
                        } else {
                            std::cout << ", ";
                        }
                        printROperand(argument);
                    }
                    std::cout << ")";
                    break;
            }
            std::cout << "\n";
        }
    }
}
