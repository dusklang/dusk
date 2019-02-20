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
    Value val { 64 / 8 };
    val.u64 = constant;
    return val;
}
Operand LIRGenerator::U32Constant(uint32_t constant) {
    Value val { 32 / 8 };
    val.u32 = constant;
    return val;
}
Operand LIRGenerator::U16Constant(uint16_t constant) {
    Value val { 16 / 8 };
    val.u16 = constant;
    return val;
}
Operand LIRGenerator::U8Constant(uint8_t constant) {
    Value val { 8 / 8 };
    val.u8 = constant;
    return val;
}
Operand LIRGenerator::F64Constant(double constant) {
    Value val { 64 / 8 };
    val.f64 = constant;
    return val;
}
Operand LIRGenerator::F32Constant(float constant) {
    Value val { 32 / 8 };
    val.f32 = constant;
    return val;
}
Operand LIRGenerator::boolConstant(bool constant) {
    Value val { 8 / 8 };
    val.boolean = constant;
    return val;
}
Operand LIRGenerator::globalStringConstant(char const* data, uint64_t size) {
    MemoryLoc loc { MemoryLoc::GlobalConstant, { program.constants.count() } };
    // TODO: Add a method to Array for quickly appending from a buffer.
    program.constants.reserve(program.constants.count() + size + 1);
    for(uint64_t i: Range<uint64_t> {0, size}) {
        program.constants.append(data[i]);
    }
    program.constants.append(0);
    return loc;
}
MemoryLoc LIRGenerator::indirectMemoryLoc(Operand pointer) {
    MemoryLoc loc { MemoryLoc::Indirect };
    loc.pointer = new Operand(pointer);
    return loc;
}

MemoryLoc LIRGenerator::variable(Type type) {
    auto& func = currentFunction();
    // TODO: alignment!
    MemoryLoc loc { MemoryLoc::StackFrame, { func.frameSize } };
    func.frameSize += type.layout().size;
    return loc;
}
MemoryLoc LIRGenerator::global(Value initialValue) {
    MemoryLoc loc { MemoryLoc::GlobalVariable, { program.globals.count() } };
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
MemoryLoc LIRGenerator::externGlobal(std::string name, Type type) {
    MemoryLoc loc { MemoryLoc::ExternGlobalVariable, { program.externGlobals.count() } };
    program.externGlobals.append({name, type.layout().size});
    return loc;
}

Func LIRGenerator::function(std::string name, Type returnType, bool isExtern) {
    Function func {};
    func.name = name;
    func.returnValueSize = returnType.layout().size;
    func.isExtern = isExtern;
    program.functions.append(func);
    Func function = program.functions.count() - 1;
    return function;
}

Func LIRGenerator::beginFunction(std::string name, Type returnType, bool isExtern) {
    auto func = function(name, returnType, isExtern);
    program.functions[func].basicBlocks.append({});
    insertionState.append({func, 0});
    return func;
}

void LIRGenerator::endFunction() {
    insertionState.removeLast();
}

void LIRGenerator::twoAddressCode(OpCode op, MemoryLoc dest, Operand operand, Type meaningfulType) {
    Instruction instr {};
    instr.op = op;
    instr.dest = dest;
    instr.operand = operand;
    instr.size = meaningfulType.layout().size;
    currentBasicBlock().instructions.append(instr);
}
void LIRGenerator::threeAddressCode(OpCode op, MemoryLoc dest, Operand operandA, Operand operandB, Type meaningfulType) {
    Instruction instr {};
    instr.op = op;
    instr.dest = dest;
    instr.operands = { operandA, operandB };
    instr.size = meaningfulType.layout().size;
    currentBasicBlock().instructions.append(instr);
}
// TODO: the xExtend methods can be removed and written in terms of twoAddressCode by adding a destType pointer parameter to it with a default value of nullptr.
void LIRGenerator::zeroExtend(MemoryLoc dest, Type destType, Operand operand, Type operandType) {
    Instruction instr {};
    instr.op = OpCode::ZeroExtend;
    instr.dest = dest;
    instr.operand = operand;
    instr.destSize = destType.layout().size;
    instr.size = operandType.layout().size;
    currentBasicBlock().instructions.append(instr);
}
void LIRGenerator::signExtend(MemoryLoc dest, Type destType, Operand operand, Type operandType) {
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
void LIRGenerator::returnValue(Operand operand, Type type) {
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

void LIRGenerator::placeConstant(Operand val, Type type, ResultContext ctx) {
    switch(ctx.kind) {
        case RCKind::DontCare: return;
        case RCKind::Return: {
            returnValue(val, type);
        } return;
        case RCKind::Copy: {
            twoAddressCode(OpCode::Copy, ctx.copy, val, type);
        } break;
        case RCKind::Read: {
            *ctx.read = val;
        } break;
        case RCKind::Write: {
            panic("Cannot write to constant");
        }
    }
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
            retVal = externGlobal(decl->name.getText().string(), decl->type);
        } else {
            retVal = function(decl->name.getText().string(), decl->type, true);
        }
    } else if(auto expr = decl->expression()) {
        MemoryLoc var;
        if(insertionState.isEmpty()) {
            Operand operand;
            visitExpr(expr, ResultContext::Read(&operand));
            assert(operand.kind == Operand::Constant);
            var = global(operand.constant);
            return var;
        } else {
            var = variable(decl->type);
            visitExpr(expr, ResultContext::Copy(var));
            return var;
        }
        retVal = var;
    } else {
        retVal = declMap[decl] = beginFunction(decl->name.getText().string(), decl->type, false);
        for(auto param: decl->paramList) {
            declMap[param] = variable(param->type);
        }
        visitScope(decl->body(), ResultContext::Return());
        endFunction();
    }
    declMap[decl] = *retVal;
    return *retVal;
}
void LIRGenerator::visitScope(Scope* scope, ResultContext ctx) {
    for(auto node: scope->nodes) {
        if(auto expr = dynamic_cast<Expr*>(node)) {
            if(expr == scope->terminalExpr) {
                if(expr->type == NeverTy()) {
                    visitExpr(expr, ResultContext::DontCare());
                    unreachableInstr();
                } else if(ctx.kind == RCKind::Return) {
                    if(expr->type == VoidTy()) {
                        visitExpr(expr, ResultContext::DontCare());
                        returnVoid();
                    } else {
                        visitExpr(expr, ResultContext::Return());
                    }
                } else {
                    visitExpr(expr, ctx);
                }
            } else {
                visitExpr(expr, ResultContext::DontCare());
            }
        } else if(auto decl = dynamic_cast<Decl*>(node)) {
            visitDecl(decl);
        }
    }
}
void LIRGenerator::visitIntegerLiteralExpr(IntegerLiteralExpr* expr, ResultContext ctx) {
    Operand constant;
    match(expr->type.data)(
        pattern(as<IntTy>(arg)) = [&](auto ty) {
            // Note: we don't have to worry about signedness because integer literal
            // expressions are always positive.
            switch(ty.bitWidth) {
                case 8: {
                    constant = U8Constant(expr->literal);
                } break;
                case 16: {
                    constant = U16Constant(expr->literal);
                } break;
                case 32: {
                    constant = U32Constant(expr->literal);
                } break;
                case 64: {
                    constant = U64Constant(expr->literal);
                } break;
                default: {
                    panic("Invalid integer literal bitWidth");
                } break;
            }
        },
        pattern(as<FloatTy>(arg)) = [&](auto ty) {
            constant = F32Constant(expr->literal);
        },
        pattern(as<DoubleTy>(arg)) = [&](auto ty) {
            constant = F64Constant(expr->literal);
        },
        pattern(_) = []() { panic("Invalid integer literal expression"); }
    );
    placeConstant(constant, expr->type, ctx);
}
void LIRGenerator::visitDecimalLiteralExpr(DecimalLiteralExpr* expr, ResultContext ctx) {
    Operand constant;
    match(expr->type.data)(
        pattern(as<FloatTy>(arg)) = [&](auto ty) {
            constant = F32Constant(strtof(expr->literal.c_str(), nullptr));
        },
        pattern(as<DoubleTy>(arg)) = [&](auto ty) {
            constant = F64Constant(strtod(expr->literal.c_str(), nullptr));
        },
        pattern(_) = []() { panic("Invalid decimal literal expression"); }
    );
    placeConstant(constant, expr->type, ctx);
}
void LIRGenerator::visitBooleanLiteralExpr(BooleanLiteralExpr* expr, ResultContext ctx) {
    placeConstant(boolConstant(expr->literal), expr->type, ctx);
}
void LIRGenerator::visitCharLiteralExpr(CharLiteralExpr* expr, ResultContext ctx) {
    placeConstant(U8Constant(expr->literal), expr->type, ctx);
}
void LIRGenerator::visitStringLiteralExpr(StringLiteralExpr* expr, ResultContext ctx) {
    Operand str = globalStringConstant(expr->literal.c_str(), expr->literal.size() + 1);
    switch(ctx.kind) {
        case RCKind::DontCare: return;
        case RCKind::Return: {
            MemoryLoc addr = variable(expr->type);
            twoAddressCode(OpCode::GetAddress, addr, str, *expr->type.pointeeType());
            returnValue(addr, expr->type);
        } return;
        case RCKind::Copy: {
            twoAddressCode(OpCode::GetAddress, ctx.copy, str, *expr->type.pointeeType());
        } break;
        case RCKind::Read: {
            MemoryLoc addr = variable(expr->type);
            twoAddressCode(OpCode::GetAddress, addr, str, *expr->type.pointeeType());
            *ctx.read = addr;
        } break;
        case RCKind::Write: {
            panic("Cannot write to string literal");
        } break;
    }
}
void LIRGenerator::visitDoExpr(DoExpr* expr, ResultContext ctx) {
    match(expr->value)(
        pattern(as<Expr*>(arg)) = [&](auto expr) {
            visitExpr(expr, ResultContext::DontCare());
        },
        pattern(as<Scope*>(arg)) = [&](auto scope) {
            visitScope(scope, ctx);
        }
    );
}
void LIRGenerator::visitPreOpExpr(PreOpExpr* expr, ResultContext ctx) {
    switch(expr->op) {
        case PreOp::Positive: {
            assert(ctx.kind != RCKind::Write && "Can't write to constant expression");
            visitExpr(expr->operand, ctx);
        } break;
        case PreOp::Negative: {
            switch(ctx.kind) {
                case RCKind::DontCare: {
                    visitExpr(expr->operand, ctx);
                } break;
                case RCKind::Return: {
                    MemoryLoc res = variable(expr->type);
                    visitExpr(expr->operand, ResultContext::Copy(res));
                    twoAddressCode(OpCode::Negative, res, res, expr->type);
                    returnValue(res, expr->type);
                } break;
                case RCKind::Copy: {
                    visitExpr(expr->operand, ctx);
                    twoAddressCode(OpCode::Negative, ctx.copy, ctx.copy, expr->type);
                } break;
                case RCKind::Read: {
                    MemoryLoc res = variable(expr->type);
                    visitExpr(expr->operand, ResultContext::Copy(res));
                    twoAddressCode(OpCode::Negative, res, res, expr->type);
                    *ctx.read = res;
                } break;
                case RCKind::Write: {
                    panic("Can't write to constant expression");
                } break;
            }
        } break;
        case PreOp::Deref: {
            switch(ctx.kind) {
                case RCKind::DontCare: {
                    visitExpr(expr->operand, ctx);
                } break;
                case RCKind::Return: {
                    Operand addr;
                    visitExpr(expr->operand, ResultContext::Read(&addr));
                    MemoryLoc res = variable(expr->type);
                    twoAddressCode(OpCode::Load, res, addr, expr->type);
                    returnValue(res, expr->type);
                } break;
                case RCKind::Copy: {
                    Operand addr;
                    visitExpr(expr->operand, ResultContext::Read(&addr));
                    twoAddressCode(OpCode::Load, ctx.copy, addr, expr->type);
                } break;
                case RCKind::Read: {
                    Operand addr;
                    visitExpr(expr->operand, ResultContext::Read(&addr));
                    MemoryLoc res = variable(expr->type);
                    twoAddressCode(OpCode::Load, res, addr, expr->type);
                    *ctx.read = res;
                } break;
                case RCKind::Write: {
                    Operand addr;
                    visitExpr(expr->operand, ResultContext::Read(&addr));
                    *ctx.write = indirectMemoryLoc(addr);
                } break;
            }
        } break;
        case PreOp::AddrOf: {
            switch(ctx.kind) {
                case RCKind::DontCare: {
                    visitExpr(expr->operand, ctx);
                } break;
                case RCKind::Return: {
                    MemoryLoc loc;
                    visitExpr(expr->operand, ResultContext::Write(&loc));
                    MemoryLoc addr = variable(expr->type);
                    twoAddressCode(OpCode::GetAddress, addr, loc, expr->operand->type);
                    returnValue(addr, expr->type);
                } break;
                case RCKind::Copy: {
                    MemoryLoc loc;
                    visitExpr(expr->operand, ResultContext::Write(&loc));
                    twoAddressCode(OpCode::GetAddress, ctx.copy, loc, expr->operand->type);
                } break;
                case RCKind::Read: {
                    MemoryLoc loc;
                    visitExpr(expr->operand, ResultContext::Write(&loc));
                    MemoryLoc addr = variable(expr->type);
                    twoAddressCode(OpCode::GetAddress, addr, loc, expr->operand->type);
                    returnValue(addr, expr->type);
                    *ctx.read = addr;
                } break;
                case RCKind::Write: {
                    panic("Can't write to constant expression");
                } break;
            }
        } break;
        case PreOp::Not: {
            switch(ctx.kind) {
                case RCKind::DontCare: {
                    visitExpr(expr->operand, ctx);
                } break;
                case RCKind::Return: {
                    MemoryLoc res = variable(expr->type);
                    visitExpr(expr->operand, ResultContext::Copy(res));
                    twoAddressCode(OpCode::LogicalNot, res, res, expr->type);
                    returnValue(res, expr->type);
                } break;
                case RCKind::Copy: {
                    visitExpr(expr->operand, ctx);
                    twoAddressCode(OpCode::LogicalNot, ctx.copy, ctx.copy, expr->type);
                } break;
                case RCKind::Read: {
                    MemoryLoc res = variable(expr->type);
                    visitExpr(expr->operand, ResultContext::Copy(res));
                    twoAddressCode(OpCode::LogicalNot, res, res, expr->type);
                    *ctx.read = res;
                } break;
                case RCKind::Write: {
                    panic("Can't write to constant expression");
                } break;
            }
        } break;
    }
}
static bool isAssignment(BinOp op) {
    switch(op) {
        case BinOp::Assignment:
        case BinOp::AddAssignment:
        case BinOp::SubAssignment:
        case BinOp::MultAssignment:
        case BinOp::DivAssignment:
        case BinOp::ModAssignment:
        case BinOp::BitwiseAndAssignment:
        case BinOp::BitwiseOrAssignment:
            return true;
        case BinOp::Add:
        case BinOp::Sub:
        case BinOp::Mult:
        case BinOp::Div:
        case BinOp::Mod:
        case BinOp::Equal:
        case BinOp::NotEqual:
        case BinOp::LessThanOrEqual:
        case BinOp::LessThan:
        case BinOp::GreaterThanOrEqual:
        case BinOp::GreaterThan:
        case BinOp::Or:
        case BinOp::And:
        case BinOp::BitwiseAnd:
        case BinOp::BitwiseOr:
            return false;
    }
}

/// BinOp with no assignment.
enum class PureBinOp {
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    BitwiseAnd,
    BitwiseOr,
    Equal,
    NotEqual,
    LessThanOrEqual,
    LessThan,
    GreaterThanOrEqual,
    GreaterThan,
    Or,
    And,
    IdentityRHS
};
static PureBinOp pureOperation(BinOp op) {
    switch(op) {
        case BinOp::AddAssignment:
        case BinOp::Add:
            return PureBinOp::Add;
        case BinOp::SubAssignment:
        case BinOp::Sub:
            return PureBinOp::Sub;
        case BinOp::MultAssignment:
        case BinOp::Mult:
            return PureBinOp::Mult;
        case BinOp::DivAssignment:
        case BinOp::Div:
            return PureBinOp::Div;
        case BinOp::ModAssignment:
        case BinOp::Mod:
            return PureBinOp::Mod;
        case BinOp::BitwiseAndAssignment:
        case BinOp::BitwiseAnd:
            return PureBinOp::BitwiseAnd;
        case BinOp::BitwiseOrAssignment:
        case BinOp::BitwiseOr:
            return PureBinOp::BitwiseOr;
        case BinOp::Equal:
            return PureBinOp::Equal;
        case BinOp::NotEqual:
            return PureBinOp::NotEqual;
        case BinOp::LessThanOrEqual:
            return PureBinOp::LessThanOrEqual;
        case BinOp::LessThan:
            return PureBinOp::LessThan;
        case BinOp::GreaterThanOrEqual:
            return PureBinOp::GreaterThanOrEqual;
        case BinOp::GreaterThan:
            return PureBinOp::GreaterThan;
        case BinOp::Or:
            return PureBinOp::Or;
        case BinOp::And:
            return PureBinOp::And;
        case BinOp::Assignment:
            return PureBinOp::IdentityRHS;
    }
}
void LIRGenerator::visitBinOpExpr(BinOpExpr* expr, ResultContext _ctx) {
    ResultContext ctx = _ctx;
    switch(_ctx.kind) {
        case RCKind::DontCare: {
            if(isAssignment(expr->op)) {
                MemoryLoc loc;
                visitExpr(expr->lhs, ResultContext::Write(&loc));
                ctx = ResultContext::Copy(loc);
            }
        } break;

        case RCKind::Return:
        case RCKind::Copy:
        case RCKind::Read:
        case RCKind::Write: {
            assert(!isAssignment(expr->op));
        } break;
    }

    PureBinOp pureOp = pureOperation(expr->op);
    auto getPointerOffset = [&](Operand offset, PointerTy pointerTy, IntTy intTy) -> MemoryLoc {
        assert(intTy.bitWidth <= sizeof(uint8_t*) * 8);
        MemoryLoc val = variable(pointerTy);
        Type offsetType;
        if(intTy.bitWidth < sizeof(uint8_t*) * 8) {
            switch(intTy.signedness) {
                case Signedness::Unsigned: {
                    offsetType = IntTy::U64();
                    zeroExtend(val, offsetType, offset, expr->rhs->type);
                } break;
                case Signedness::Signed: {
                    offsetType = IntTy::I64();
                    signExtend(val, offsetType, offset, expr->rhs->type);
                } break;
            }
        }
        threeAddressCode(OpCode::Mult, val, val, U64Constant(expr->lhs->type.pointeeType()->layout().stride()), offsetType);
        return val;
    };
    auto doBinOp = [&](OpCode opcode) {
        switch(ctx.kind) {
            case RCKind::DontCare: {
                visitExpr(expr->lhs, ResultContext::DontCare());
                visitExpr(expr->rhs, ResultContext::DontCare());
            } break;
            case RCKind::Return: {
                Operand lhs, rhs;
                visitExpr(expr->lhs, ResultContext::Read(&lhs));
                visitExpr(expr->rhs, ResultContext::Read(&rhs));
                MemoryLoc val = variable(expr->type);
                threeAddressCode(opcode, val, lhs, rhs, expr->lhs->type);
                returnValue(val, expr->type);
            } break;
            case RCKind::Copy: {
                Operand lhs, rhs;
                visitExpr(expr->lhs, ResultContext::Read(&lhs));
                visitExpr(expr->rhs, ResultContext::Read(&rhs));
                threeAddressCode(opcode, ctx.copy, lhs, rhs, expr->lhs->type);
            } break;
            case RCKind::Read: {
                Operand lhs, rhs;
                visitExpr(expr->lhs, ResultContext::Read(&lhs));
                visitExpr(expr->rhs, ResultContext::Read(&rhs));
                MemoryLoc val = variable(expr->type);
                threeAddressCode(opcode, val, lhs, rhs, expr->lhs->type);
                *ctx.read = val;
            } break;
            case RCKind::Write: {
                panic("Can't write to constant expression");
            } break;
        }
    };
    switch(pureOp) {
        case PureBinOp::Add: {
            match(expr->lhs->type.data, expr->rhs->type.data)(
                pattern(as<IntTy>(_), as<IntTy>(_)) = [&] {
                    doBinOp(OpCode::WrappingAdd);
                },
                pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                    doBinOp(OpCode::FAdd);
                },
                pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                    doBinOp(OpCode::FAdd);
                },
                pattern(as<PointerTy>(arg), as<IntTy>(arg)) = [&](auto pointerTy, auto intTy) {
                    OpCode opcode = OpCode::WrappingAdd;
                    switch(ctx.kind) {
                        case RCKind::DontCare: {
                            visitExpr(expr->lhs, ResultContext::DontCare());
                            visitExpr(expr->rhs, ResultContext::DontCare());
                        } break;
                        case RCKind::Return: {
                            Operand lhs, rhs;
                            visitExpr(expr->lhs, ResultContext::Read(&lhs));
                            visitExpr(expr->rhs, ResultContext::Read(&rhs));

                            MemoryLoc val = getPointerOffset(rhs, pointerTy, intTy);
                            threeAddressCode(opcode, val, val, lhs, expr->type);
                            returnValue(val, expr->type);
                        } break;
                        case RCKind::Copy: {
                            Operand lhs, rhs;
                            visitExpr(expr->lhs, ResultContext::Read(&lhs));
                            visitExpr(expr->rhs, ResultContext::Read(&rhs));

                            MemoryLoc val = getPointerOffset(rhs, pointerTy, intTy);
                            threeAddressCode(opcode, ctx.copy, val, lhs, expr->type);
                        } break;
                        case RCKind::Read: {
                            Operand lhs, rhs;
                            visitExpr(expr->lhs, ResultContext::Read(&lhs));
                            visitExpr(expr->rhs, ResultContext::Read(&rhs));

                            MemoryLoc val = getPointerOffset(rhs, pointerTy, intTy);
                            threeAddressCode(opcode, val, val, lhs, expr->type);
                            *ctx.read = val;
                        } break;
                        case RCKind::Write: {
                            panic("Can't write to constant expression");
                        } break;
                    }
                    return;
                },
                pattern(_, _) = [] {
                    panic("Can't add values of non-pointer, non-integer, non-floating point types");
                }
            );
        } break;
        case PureBinOp::Sub: {
            match(expr->lhs->type.data, expr->rhs->type.data)(
                pattern(as<IntTy>(_), as<IntTy>(_)) = [&] {
                    doBinOp(OpCode::WrappingSub);
                },
                pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                    doBinOp(OpCode::FSub);
                },
                pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                    doBinOp(OpCode::FSub);
                },
                pattern(as<PointerTy>(arg), as<IntTy>(arg)) = [&](auto pointerTy, auto intTy) {
                    OpCode opcode = OpCode::WrappingSub;
                    switch(ctx.kind) {
                        case RCKind::DontCare: {
                            visitExpr(expr->lhs, ResultContext::DontCare());
                            visitExpr(expr->rhs, ResultContext::DontCare());
                        } break;
                        case RCKind::Return: {
                            Operand lhs, rhs;
                            visitExpr(expr->lhs, ResultContext::Read(&lhs));
                            visitExpr(expr->rhs, ResultContext::Read(&rhs));

                            MemoryLoc val = getPointerOffset(rhs, pointerTy, intTy);
                            threeAddressCode(opcode, val, val, lhs, expr->type);
                            returnValue(val, expr->type);
                        } break;
                        case RCKind::Copy: {
                            Operand lhs, rhs;
                            visitExpr(expr->lhs, ResultContext::Read(&lhs));
                            visitExpr(expr->rhs, ResultContext::Read(&rhs));

                            MemoryLoc val = getPointerOffset(rhs, pointerTy, intTy);
                            threeAddressCode(opcode, ctx.copy, val, lhs, expr->type);
                        } break;
                        case RCKind::Read: {
                            Operand lhs, rhs;
                            visitExpr(expr->lhs, ResultContext::Read(&lhs));
                            visitExpr(expr->rhs, ResultContext::Read(&rhs));

                            MemoryLoc val = getPointerOffset(rhs, pointerTy, intTy);
                            threeAddressCode(opcode, val, val, lhs, expr->type);
                            *ctx.read = val;
                        } break;
                        case RCKind::Write: {
                            panic("Can't write to constant expression");
                        } break;
                    }
                },
                pattern(as<PointerTy>(arg), as<PointerTy>(_)) = [&](auto pointerTy) {
                    switch(ctx.kind) {
                        case RCKind::DontCare: {
                            visitExpr(expr->lhs, ResultContext::DontCare());
                            visitExpr(expr->rhs, ResultContext::DontCare());
                        } break;
                        case RCKind::Return: {
                            Operand lhs, rhs;
                            visitExpr(expr->lhs, ResultContext::Read(&lhs));
                            visitExpr(expr->rhs, ResultContext::Read(&rhs));

                            MemoryLoc val = variable(IntTy::I64());
                            threeAddressCode(OpCode::WrappingSub, val, lhs, rhs, IntTy::I64());
                            threeAddressCode(OpCode::Div, val, val, U64Constant(pointerTy.pointedTy->layout().stride()), IntTy::I64());
                            returnValue(val, expr->type);
                        } break;
                        case RCKind::Copy: {
                            Operand lhs, rhs;
                            visitExpr(expr->lhs, ResultContext::Read(&lhs));
                            visitExpr(expr->rhs, ResultContext::Read(&rhs));

                            MemoryLoc val = variable(IntTy::I64());
                            threeAddressCode(OpCode::WrappingSub, val, lhs, rhs, IntTy::I64());
                            threeAddressCode(OpCode::Div, ctx.copy, val, U64Constant(pointerTy.pointedTy->layout().stride()), IntTy::I64());
                        } break;
                        case RCKind::Read: {
                            Operand lhs, rhs;
                            visitExpr(expr->lhs, ResultContext::Read(&lhs));
                            visitExpr(expr->rhs, ResultContext::Read(&rhs));

                            MemoryLoc val = variable(IntTy::I64());
                            threeAddressCode(OpCode::WrappingSub, val, lhs, rhs, IntTy::I64());
                            threeAddressCode(OpCode::Div, val, val, U64Constant(pointerTy.pointedTy->layout().stride()), IntTy::I64());
                            *ctx.read = val;
                        } break;
                        case RCKind::Write: {
                            panic("Can't write to constant expression");
                        } break;
                    }
                },
                pattern(_, _) = [] {
                    panic("Can't subtract values of non-pointer, non-integer, non-floating point types");
                }
            );
        } break;
        case PureBinOp::Mult: {
            match(expr->lhs->type.data, expr->rhs->type.data)(
                pattern(as<IntTy>(_), as<IntTy>(_)) = [&] {
                    doBinOp(OpCode::Mult);
                },
                pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                    doBinOp(OpCode::FMult);
                },
                pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                    doBinOp(OpCode::FMult);
                },
                pattern(_, _) = [] {
                    panic("Can't mod values of non-integer, non-floating point types");
                }
            );
        } break;
        case PureBinOp::Div: {
            match(expr->lhs->type.data, expr->rhs->type.data)(
                pattern(as<IntTy>(_), as<IntTy>(_)) = [&] {
                    doBinOp(OpCode::Div);
                },
                pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                    doBinOp(OpCode::FDiv);
                },
                pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                    doBinOp(OpCode::FDiv);
                },
                pattern(_, _) = [] {
                    panic("Can't divide values of non-integer, non-floating point types");
                }
            );
        } break;
        case PureBinOp::Mod: {
            match(expr->lhs->type.data, expr->rhs->type.data)(
                pattern(as<IntTy>(_), as<IntTy>(_)) = [&] {
                    doBinOp(OpCode::Mod);
                },
                pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                    doBinOp(OpCode::FMod);
                },
                pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                    doBinOp(OpCode::FMod);
                },
                pattern(_, _) = [] {
                    panic("Can't mod values of non-integer, non-floating point types");
                }
            );
        } break;
        case PureBinOp::BitwiseAnd: {
            doBinOp(OpCode::BitwiseAnd);
        } break;
        case PureBinOp::BitwiseOr: {
            doBinOp(OpCode::BitwiseOr);
        } break;
        case PureBinOp::Equal: {
            // TODO: handle (at least) structure and floating point types in a specialized way.
            doBinOp(OpCode::Equal);
        } break;
        case PureBinOp::NotEqual: {
            // TODO: handle (at least) structure and floating point types in a specialized way.
            doBinOp(OpCode::NotEqual);
        } break;
        case PureBinOp::LessThanOrEqual: {
            match(expr->lhs->type.data, expr->rhs->type.data)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    doBinOp(OpCode::LTE);
                },
                pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                    doBinOp(OpCode::FLTE);
                },
                pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                    doBinOp(OpCode::FLTE);
                },
                pattern(_, _) = [] {
                    panic("Can't compare values of non-integer, non-floating point types");
                }
            );
        } break;
        case PureBinOp::LessThan: {
            match(expr->lhs->type.data, expr->rhs->type.data)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    doBinOp(OpCode::LT);
                },
                pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                    doBinOp(OpCode::FLT);
                },
                pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                    doBinOp(OpCode::FLT);
                },
                pattern(_, _) = [] {
                    panic("Can't compare values of non-integer, non-floating point types");
                }
            );
        } break;
        case PureBinOp::GreaterThanOrEqual: {
            match(expr->lhs->type.data, expr->rhs->type.data)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    doBinOp(OpCode::GTE);
                },
                pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                    doBinOp(OpCode::FGTE);
                },
                pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                    doBinOp(OpCode::FGTE);
                },
                pattern(_, _) = [] {
                    panic("Can't compare values of non-integer, non-floating point types");
                }
            );
        } break;
        case PureBinOp::GreaterThan: {
            match(expr->lhs->type.data, expr->rhs->type.data)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    doBinOp(OpCode::GT);
                },
                pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                    doBinOp(OpCode::FGT);
                },
                pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                    doBinOp(OpCode::FGT);
                },
                pattern(_, _) = [] {
                    panic("Can't compare values of non-integer, non-floating point types");
                }
            );
        } break;
        case PureBinOp::Or: {
            doBinOp(OpCode::LogicalOr);
        } break;
        case PureBinOp::And: {
            doBinOp(OpCode::LogicalAnd);
        } break;
        case PureBinOp::IdentityRHS: {
            assert(ctx.kind == RCKind::Copy);
            visitExpr(expr->rhs, ctx);
        } break;
    }
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
