#include "LIR.h"

using namespace lir;

static void printType(LIRType const* type) {
    switch(type->kind) {
        case LIRType::Terminal: {
            std::cout << "terminal";
        } break;
        case LIRType::Void: {
            std::cout << "void";
        } break;
        case LIRType::i1: {
            std::cout << "i1";
        } break;
        case LIRType::i8: {
            std::cout << "i8";
        } break;
        case LIRType::i16: {
            std::cout << "i16";
        } break;
        case LIRType::i32: {
            std::cout << "i32";
        } break;
        case LIRType::i64: {
            std::cout << "i64";
        } break;
        case LIRType::f32: {
            std::cout << "f32";
        } break;
        case LIRType::f64: {
            std::cout << "f64";
        } break;
        case LIRType::Pointer: {
            std::cout << "*";
            printType(type->pointee);
        } break;
        case LIRType::Array: {
            std::cout << "[";
            printType(type->arrayElement);
            std::cout << " x " << type->count << "]";
        } break;
        case LIRType::Structure: {
            std::cout << "{";
            for(LIRType const* type: type->fields) {
                printType(type);
                std::cout << ", ";
            }
            std::cout << "}";
        } break;
    }
}

static void printConstant(ConstantValue val) {
    switch(val.type->kind) {
        case LIRType::Terminal: {
            panic("can't have terminal constant");
        } break;
        case LIRType::Void: {
            std::cout << "void";
        } break;
        case LIRType::i1: {
            std::cout << val.i1;
        } break;
        case LIRType::i8: {
            std::cout << val.i8;
        } break;
        case LIRType::i16: {
            std::cout << val.i16;
        } break;
        case LIRType::i32: {
            std::cout << val.i32;
        } break;
        case LIRType::i64: {
            std::cout << val.i64;
        } break;
        case LIRType::f32: {
            std::cout << val.f32;
        } break;
        case LIRType::f64: {
            std::cout << val.f64;
        } break;
        case LIRType::Pointer: {
            panic("constant pointers unsupported so far");
        } break;
        case LIRType::Array: {
            std::cout << "[";
            for(auto elem: val.elements) {
                printConstant(elem);
                std::cout << ", ";
            }
            std::cout << "]";
        } break;
        case LIRType::Structure: {
            std::cout << "{";
            for(auto field: val.fields) {
                printConstant(field);
                std::cout << ", ";
            }
            std::cout << "}";
        } break;
    }
}

static void printOperand(Operand operand);
static void printLoc(MemoryLoc loc) {
    switch(loc.base) {
        case lir::MemoryLoc::StackFrame: {
            std::cout << "SF+";
        } break;
        case lir::MemoryLoc::GlobalVariable: {
            std::cout << "GV+";
        } break;
        case lir::MemoryLoc::ExternGlobalVariable: {
            std::cout << "EV#";
        } break;
        case lir::MemoryLoc::GlobalConstant: {
            std::cout << "GC+";
        } break;
        case lir::MemoryLoc::Indirect: {
            std::cout << "*(";
            printOperand(*loc.pointer);
            std::cout << ")+";
        } break;
    }
    std::cout << loc.offset;
}
static void printOperand(Operand operand) {
    switch(operand.kind) {
        case Operand::Location: {
            printLoc(operand.location);
        } break;
        case Operand::Constant: {
            std::cout << "Constant(size: " << operand.constant.size << ")";
        } break;
    }
}
void Program::print() const {
    for(size_t j: functions.indices()) {
        Function const& function = functions[j];
        std::cout << function.name << ":\n";
        for(size_t i: function.basicBlocks.indices()) {
            BasicBlock const& bb = function.basicBlocks[i];
            std::cout << "BB" << i << ":\n";
            for(size_t j: bb.instructions.indices()) {
                Instruction const& instruction = bb.instructions[j];

                auto printInstructionBeginning = [&](auto name) {
                    printf("%04zu: ", j);
                    std::cout << name << " ";
                };
                auto printTwoAddressCode = [&](auto name) {
                    printInstructionBeginning(name);
                    printLoc(instruction.dest);
                    std::cout << ", ";
                    printOperand(instruction.operand);
                };
                auto printThreeAddressCode = [&](auto name) {
                    printInstructionBeginning(name);
                    printLoc(instruction.dest);
                    std::cout << ", ";
                    printOperand(instruction.operands.a);
                    std::cout << ", ";
                    printOperand(instruction.operands.b);
                };
                std::cout << "    ";
                switch(instruction.op) {
                    case OpCode::GetAddress:
                        printTwoAddressCode("GetAddress");
                        break;
                    case OpCode::Load:
                        printTwoAddressCode("Load");
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
                        printOperand(instruction.condition);
                        std::cout << ", " << instruction.branch;
                        break;
                    case OpCode::Call: {
                        printInstructionBeginning("Call");
                        std::cout << instruction.function << "(";
                        bool first = true;
                        for(auto& argument: instruction.arguments) {
                            if(first) {
                                first = false;
                            } else {
                                std::cout << ", ";
                            }
                            printOperand(argument.operand);
                        }
                        std::cout << ")";
                    } break;
                    case lir::OpCode::Return:
                        printInstructionBeginning("Return");
                        printOperand(instruction.operand);
                        break;
                    case lir::OpCode::ReturnVoid:
                        printInstructionBeginning("Return");
                        break;
                    case lir::OpCode::Unreachable:
                        printInstructionBeginning("Unreachable");
                        break;
                }
                std::cout << "\n";
            }
        }
    }
}
