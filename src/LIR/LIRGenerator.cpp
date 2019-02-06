//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "LIRGenerator.h"
#include "mpark/patterns.hpp"

using namespace mpark::patterns;
using namespace lir;

static uint32_t getSizeForType(Type const& ty) {
    // TODO: IMPLEMENT
    return 0;
}

static uint32_t getStrideForType(Type const& ty) {
    // TODO: IMPLEMENT
    return 0;
}

Var LIRGenerator::visitDecl(Decl* decl) {
    return -1;
}
Var LIRGenerator::visitScope(Scope* scope) {
    return -1;
}
Var LIRGenerator::visitStructDecl(StructDecl* decl) {
    return -1;
}
Var LIRGenerator::visitIntegerLiteralExpr(IntegerLiteralExpr* expr) {
    Value constant {};
    uint32_t size;
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
            size = ty.bitWidth / 8;
            constant.size = size;
        },
        pattern(as<FloatTy>(arg)) = [&](auto ty) {
            size = 32;
            constant.size = size;
            constant.f32 = expr->literal;
        },
        pattern(as<DoubleTy>(arg)) = [&](auto ty) {
            size = 64;
            constant.size = size;
            constant.f64 = expr->literal;
        },
        pattern(_) = []() { panic("Invalid integer literal expression"); }
    );
    Function& func = program.functions[currentFunction];
    Instruction instr {};
    instr.op = OpCode::GetConstant;
    instr.dest = func.appendVariable({size});
    instr.constant = func.appendConstant(constant);
    func.appendInstruction(instr);
    return instr.dest;
}
Var LIRGenerator::visitDecimalLiteralExpr(DecimalLiteralExpr* expr) {
    Value constant {};
    uint32_t size;
    match(expr->type.data)(
        pattern(as<FloatTy>(arg)) = [&](auto ty) {
            size = 32;
            constant.size = size;
            constant.f32 = strtof(expr->literal.c_str(), nullptr);
        },
        pattern(as<DoubleTy>(arg)) = [&](auto ty) {
            size = 64;
            constant.size = size;
            constant.f64 = strtod(expr->literal.c_str(), nullptr);
        },
        pattern(_) = []() { panic("Invalid decimal literal expression"); }
    );

    Function& func = program.functions[currentFunction];
    Instruction instr {};
    instr.op = OpCode::GetConstant;
    instr.dest = func.appendVariable({size});
    instr.constant = func.appendConstant(constant);
    func.appendInstruction(instr);
    return instr.dest;
}
Var LIRGenerator::visitBooleanLiteralExpr(BooleanLiteralExpr* expr) {
    Value constant {};
    constant.size = 1;
    constant.boolean = expr->literal;

    Function& func = program.functions[currentFunction];
    Instruction instr {};
    instr.op = OpCode::GetConstant;
    instr.dest = func.appendVariable({1});
    instr.constant = func.appendConstant(constant);
    func.appendInstruction(instr);
    return instr.dest;
}
Var LIRGenerator::visitCharLiteralExpr(CharLiteralExpr* expr) {
    Value constant {};
    constant.size = 1;
    constant.u8 = expr->literal;

    Function& func = program.functions[currentFunction];
    Instruction instr {};
    instr.op = OpCode::GetConstant;
    instr.dest = func.appendVariable({1});
    instr.constant = func.appendConstant(constant);
    func.appendInstruction(instr);
    return instr.dest;
}
Var LIRGenerator::visitStringLiteralExpr(StringLiteralExpr* expr) {
    Value constant {};
    uint32_t size = expr->literal.size() + 1;
    constant.size = size;
    constant.buffer = new uint8_t[size];
    memcpy(constant.buffer, expr->literal.c_str(), size);

    Function& func = program.functions[currentFunction];
    Instruction load {};
    load.op = OpCode::GetConstantAddress;
    load.dest = func.appendVariable({sizeof(uint8_t*)});
    load.constant = func.appendConstant(constant);
    func.appendInstruction(load);
    return load.dest;
}
Var LIRGenerator::visitPreOpExpr(PreOpExpr* expr) {
    Function& func = program.functions[currentFunction];
    auto operand = visitExpr(expr->operand);
    Instruction instr {};
    instr.operand = operand;
    auto opSize = func.variables[operand].size;
    switch(expr->op) {
        case PreOp::Positive: {
            return operand;
        } break;
        case PreOp::Negative: {
            instr.op = OpCode::Negative;
            instr.dest = func.appendVariable({opSize});
        } break;
        case PreOp::Deref: {
            instr.op = OpCode::Load;
            instr.dest = func.appendVariable({getSizeForType(expr->operand->type)});
        } break;
        case PreOp::AddrOf: {
            instr.op = OpCode::GetAddress;
            instr.dest = func.appendVariable({opSize});
        } break;
        case PreOp::Not: {
            instr.op = OpCode::LogicalNot;
            instr.dest = func.appendVariable({1});
        } break;
    }
    func.appendInstruction(instr);
    return instr.dest;
}
Var LIRGenerator::visitPreOpExprAsLValue(PreOpExpr* expr) {
    switch(expr->op) {
        case PreOp::Positive:
        case PreOp::Negative:
        case PreOp::AddrOf:
        case PreOp::Not: {
            panic("Pre-op expressions other than pointer dereferences can not be taken as lvalues");
        } break;
        case PreOp::Deref: {
            return visitExpr(expr->operand);
        } break;
    }
}
Var LIRGenerator::visitBinOpExpr(BinOpExpr* expr) {
    Function& func = program.functions[currentFunction];
    Var rvalueA, rvalueB;
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
                return instr.dest;
            } else {
                instr.operands.a = rvalueA;
                instr.operands.b = rvalueB;
            }
        } break;
        case BinOp::Add:
        case BinOp::Sub:
        case BinOp::Mult:
        case BinOp::Div:
        case BinOp::Mod:
        case BinOp::BitwiseAnd:
        case BinOp::BitwiseOr: {
            instr.dest = func.appendVariable({func.variables[rvalueA].size});
            instr.operands.a = rvalueA;
            instr.operands.b = rvalueB;
        } break;
        case BinOp::Equal:
        case BinOp::NotEqual:
        case BinOp::LessThanOrEqual:
        case BinOp::LessThan:
        case BinOp::GreaterThanOrEqual:
        case BinOp::GreaterThan:
        case BinOp::Or:
        case BinOp::And: {
            instr.dest = func.appendVariable({1});
            instr.operands.a = rvalueA;
            instr.operands.b = rvalueB;
        } break;
    }

    auto getPointerOffset = [&](PointerTy pointerTy, IntTy intTy) -> Var {
        Var offset = func.appendVariable({sizeof(uint8_t*)});
        Instruction getOffset {};
        getOffset.dest = offset;
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

        Value strideVal {};
        strideVal.size = sizeof(uint8_t*);
        assertEqual(strideVal.size, 64 / 8);
        strideVal.u64 = getStrideForType(*pointerTy.pointedTy);
        Const strideConst = func.appendConstant(strideVal);

        Instruction getStride {};
        getStride.op = OpCode::GetConstant;
        auto stride = getStride.dest = func.appendVariable({strideVal.size});
        getStride.constant = strideConst;
        func.appendInstruction(getStride);

        Instruction mult {};
        mult.op = OpCode::Mult;
        mult.dest = offset;
        mult.operands = { offset, stride };
        func.appendInstruction(mult);

        return offset;
    };

    switch(expr->op) {
        case BinOp::Assignment: unreachable;

        case BinOp::AddAssignment:
        case BinOp::Add: {
            match(expr->lhs->type, expr->rhs->type)(
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
                pattern(_) = [] {
                    panic("Can't add values of non-pointer, non-integer, non-floating point types");
                }
            );
        } break;
        case BinOp::SubAssignment:
        case BinOp::Sub: {
            match(expr->lhs->type, expr->rhs->type)(
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
                pattern(_) = [] {
                    panic("Can't subtract values of non-pointer, non-integer, non-floating point types");
                }
            );
        } break;
        case BinOp::MultAssignment:
        case BinOp::Mult: {
            match(expr->lhs->type, expr->rhs->type)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    instr.op = OpCode::Mult;
                },
                pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                    instr.op = OpCode::FMult;
                },
                pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                    instr.op = OpCode::FMult;
                },
                pattern(_) = [] {
                    panic("Can't multiply values of non-integer, non-floating point types");
                }
            );
        } break;
        case BinOp::DivAssignment:
        case BinOp::Div: {
            match(expr->lhs->type, expr->rhs->type)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    instr.op = OpCode::Div;
                },
                pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                    instr.op = OpCode::FDiv;
                },
                pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                    instr.op = OpCode::FDiv;
                },
                pattern(_) = [] {
                    panic("Can't divide values of non-integer, non-floating point types");
                }
            );
        } break;
        case BinOp::ModAssignment:
        case BinOp::Mod: {
            match(expr->lhs->type, expr->rhs->type)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    instr.op = OpCode::Mod;
                },
                pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                    instr.op = OpCode::FMod;
                },
                pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                    instr.op = OpCode::FMod;
                },
                pattern(_) = [] {
                    panic("Can't mod values of non-integer, non-floating point types");
                }
            );
        } break;
        case BinOp::LessThanOrEqual: {
            match(expr->lhs->type, expr->rhs->type)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    instr.op = OpCode::LTE;
                },
                pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                    instr.op = OpCode::FLTE;
                },
                pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                    instr.op = OpCode::FLTE;
                },
                pattern(_) = [] {
                    panic("Can't compare values of non-integer, non-floating point types");
                }
            );
        } break;
        case BinOp::LessThan: {
            match(expr->lhs->type, expr->rhs->type)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    instr.op = OpCode::LT;
                },
                pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                    instr.op = OpCode::FLT;
                },
                pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                    instr.op = OpCode::FLT;
                },
                pattern(_) = [] {
                    panic("Can't compare values of non-integer, non-floating point types");
                }
            );
        } break;
        case BinOp::GreaterThanOrEqual: {
            match(expr->lhs->type, expr->rhs->type)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    instr.op = OpCode::GTE;
                },
                pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                    instr.op = OpCode::FGTE;
                },
                pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                    instr.op = OpCode::FGTE;
                },
                pattern(_) = [] {
                    panic("Can't compare values of non-integer, non-floating point types");
                }
            );
        } break;
        case BinOp::GreaterThan: {
            match(expr->lhs->type, expr->rhs->type)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    instr.op = OpCode::GT;
                },
                pattern(as<FloatTy>(_), as<FloatTy>(_)) = [&] {
                    instr.op = OpCode::FGT;
                },
                pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [&] {
                    instr.op = OpCode::FGT;
                },
                pattern(_) = [] {
                    panic("Can't compare values of non-integer, non-floating point types");
                }
            );
        } break;
        case BinOp::AndAssignment:
        case BinOp::BitwiseAnd: {
            match(expr->lhs->type, expr->rhs->type)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    instr.op = OpCode::BitwiseAnd;
                },
                pattern(_) = [] {
                    panic("Can't bitwise and values of non-integer types");
                }
            );
        } break;
        case BinOp::OrAssignment:
        case BinOp::BitwiseOr: {
            match(expr->lhs->type, expr->rhs->type)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    instr.op = OpCode::BitwiseOr;
                },
                pattern(_) = [] {
                    panic("Can't bitwise or values of non-integer types");
                }
            );
        } break;
        case BinOp::And: {
            match(expr->lhs->type, expr->rhs->type)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    instr.op = OpCode::LogicalAnd;
                },
                pattern(_) = [] {
                    panic("Can't logical and values of non-integer types");
                }
            );
        } break;
        case BinOp::Or: {
            match(expr->lhs->type, expr->rhs->type)(
                pattern(as<IntTy>(arg), as<IntTy>(_)) = [&](auto ty) {
                    instr.op = OpCode::LogicalOr;
                },
                pattern(_) = [] {
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
    return instr.dest;
}
Var LIRGenerator::visitCastExpr(CastExpr* expr) {
    return -1;
}
Var LIRGenerator::visitDeclRefExpr(DeclRefExpr* expr) {
    return -1;
}
Var LIRGenerator::visitMemberRefExpr(MemberRefExpr* expr) {
    return -1;
}
Var LIRGenerator::visitReturnExpr(ReturnExpr* expr) {
    return -1;
}
Var LIRGenerator::visitIfExpr(IfExpr* expr) {
    return -1;
}
Var LIRGenerator::visitWhileExpr(WhileExpr* expr) {
    return -1;
}

void LIRGenerator::printIR() const {}
