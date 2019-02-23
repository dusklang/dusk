#include "LIR.h"

using namespace lir;

Value::Value(struct Instr instr) : kind(Instr), bb(instr.basicBlock), index(instr.index) {}
Value::Value(struct GlobalVariable var) : kind(GlobalVariable), index(var.index) {}
Value::Value(struct ExternGlobalVariable var) : kind(ExternGlobalVariable), index(var.index) {}
Value::Value(struct GlobalConstant constant) : kind(GlobalConstant), index(constant.index) {}

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

static void printValue(Value val, Program const& prog, Func func, BB bb) {
    switch(val.kind) {
        case Value::Argument: {
            std::cout << "arg" << val.index;
        } break;
        case Value::Instr: {
            std::cout << "%" << val.bb << "." << val.index;
        } break;
        case Value::GlobalVariable: {
            std::cout << "G" << val.index;
        } break;
        case Value::ExternGlobalVariable: {
            std::cout << "@" << prog.externGlobals[val.index].name;
        } break;
        case Value::GlobalConstant:
            std::cout << "GC" << val.index;
            break;
        case Value::InlineConstant: {
            printConstant(val.inlineConstant);
        } break;
    }
}

void Program::print() const {
    for(size_t i: functions.indices()) {
        Function const& function = functions[i];
        std::cout << function.name << ":\n";
        for(size_t j: function.basicBlocks.indices()) {
            BasicBlock const& bb = function.basicBlocks[j];
            std::cout << "BB" << j << ":\n";
            for(size_t k: bb.instructions.indices()) {
                Instr const& instruction = bb.instructions[k];
                auto printInstructionBeginning = [&](auto name) {
                    if(instruction.type->kind != LIRType::Void && instruction.type->kind != LIRType::Terminal) {
                        std::cout << "%" << k << " = ";
                    }
                    std::cout << name << " ";
                };
                auto printTwoAddressCode = [&](auto name) {
                    printInstructionBeginning(name);
                    printValue(instruction.operand, *this, i, j);
                };
                auto printThreeAddressCode = [&](auto name) {
                    printInstructionBeginning(name);
                    printValue(instruction.operands.a, *this, i, j);
                    std::cout << ", ";
                    printValue(instruction.operands.b, *this, i, j);
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
                        printValue(instruction.condition, *this, i, j);
                        std::cout << ", " << instruction.branch;
                        break;
                    case OpCode::Call: {
                        printInstructionBeginning("Call");
                        std::cout << instruction.func << "(";
                        bool first = true;
                        for(auto& argument: instruction.arguments) {
                            if(first) {
                                first = false;
                            } else {
                                std::cout << ", ";
                            }
                            printValue(argument, *this, i, j);
                        }
                        std::cout << ")";
                    } break;
                    case lir::OpCode::Return:
                        printInstructionBeginning("Return");
                        printValue(instruction.operand, *this, i, j);
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
