//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <variant>
#include <optional>
#include <vector>
#include <string>

#include "General/General.h"

namespace lir {
    /// Variable index, local to a function.
    using Var = uint16_t;
    /// Constant index, local to a function.
    using Const = uint16_t;
    /// Instruction index, local to a function.
    using Instr = uint16_t;
    /// Function index.
    using Func = uint16_t;

    enum class OpCode: uint8_t {
        GetAddress,         /// dest = &operand
        Load,               /// dest = *operand
        Store,              /// *dest = operand
        Copy,               /// dest = operand

        Negative,           /// dest = -operand
        WrappingAdd,        /// dest = a + b
        WrappingSub,        /// dest = a - b
        Mult,               /// dest = a * b
        Div,                /// dest = a / b
        Mod,                /// dest = a % b
        SignExtend,         /// dest = operand as i{dest.bitWidth}
        ZeroExtend,         /// dest = operand as u{dest.bitWidth}
        LTE,                /// dest = a <= b
        LT,                 /// dest = a < b
        GTE,                /// dest = a >= b
        GT,                 /// dest = a > b
        FLTE,               /// dest = a <= b
        FLT,                /// dest = a < b
        FGTE,               /// dest = a >= b
        FGT,                /// dest = a > b
        Equal,              /// dest = a == b
        NotEqual,

        FAdd,               /// dest = a + b
        FSub,               /// dest = a - b
        FMult,              /// dest = a * b
        FDiv,               /// dest = a / b
        FMod,               /// dest = a % b

        LogicalNot,         /// dest = !operand
        LogicalAnd,         /// dest = a && b
        LogicalOr,          /// dest = a || b
        BitwiseAnd,         /// dest = a & b
        BitwiseOr,          /// dest = a | b

        Branch,             /// goto branch
        CondBranch,         /// if condition { goto trueBranch } else { goto falseBranch }
    };

    /// A bag of bits, used to represent constant values (and hopefully constexpr values as well)
    struct Value {
        /// Size in bytes.
        uint32_t size;
        union {
            /// The inline constant values. Valid iff size <= 64 / 8.
            uint64_t u64;
            uint32_t u32;
            uint16_t u16;
            uint8_t  u8;
            float    f32;
            double   f64;
            bool     boolean;

            /// The buffer that stores the constant value. Valid iff size > 64 / 8.
            uint8_t* buffer;
        };
    };

    /// A read-only operand to an instruction.
    struct ROperand {
        enum {
            /// Represents a variable stored on the stack (or, eventually, globals)
            Variable,
            /// Represents a constant stored inline in IR instructions
            LocalConstant,
            /// Represents the address of the beginning of a constant buffer stored in
            /// a program (offset by `offset`). Also `size` is just the size of a pointer
            /// with this type of operand.
            GlobalConstantAddress
        } kind;
        /// `offset` and `size` should be used for indexing into constant arrays or referencing
        /// the fields of structs. Both are in bytes.
        uint32_t offset, size;
        union {
            Var variable;
            // TODO: maybe local constants should be stored in a vector on Function and indexed into?
            Value localConstant;
            Const globalConstant;
        };
    };

    struct Instruction {
        union {
            Var dest;
            Var condition;
        };
        union {
            ROperand operand;
            struct {
                ROperand a, b;
            } operands;
            Var mutableOperand;
            lir::Instr branch;
            struct {
                lir::Instr trueBranch, falseBranch;
            };
            Const constant;
        };
        OpCode op;
    };
    struct Variable {
        uint32_t size;
    };

    struct Function {
        std::vector<Instruction> instructions;
        std::vector<Variable> variables;

        Var appendVariable(Variable variable) {
            Var var = variables.size();
            variables.push_back(variable);
            return var;
        }

        Instr appendInstruction(Instruction instruction) {
            Instr instr = instructions.size();
            instructions.push_back(instruction);
            return instr;
        }

        ROperand variableOperand(Var variable, std::optional<uint32_t> offset = std::nullopt, std::optional<uint32_t> size = std::nullopt) {
            ROperand res { ROperand::Variable };
            res.variable = variable;
            if(offset) {
                res.offset = *offset;
            } else {
                res.offset = 0;
            }
            if(size) {
                res.size = *size;
            } else {
                res.size = variables[variable].size;
            }
            return res;
        }
        ROperand localConstantOperand(Value value, std::optional<uint32_t> offset = std::nullopt, std::optional<uint32_t> size = std::nullopt) {
            ROperand res { ROperand::LocalConstant };
            res.localConstant = value;
            if(offset) {
                res.offset = *offset;
            } else {
                res.offset = 0;
            }
            if(size) {
                res.size = *size;
            } else {
                res.size = value.size;
            }
            return res;
        }
        ROperand globalConstantOperand(Const constant, std::optional<uint32_t> offset = std::nullopt, std::optional<uint32_t> size = std::nullopt) {
            ROperand res { ROperand::GlobalConstantAddress };
            res.globalConstant = constant;
            if(offset) {
                res.offset = *offset;
            } else {
                res.offset = 0;
            }
            if(size) {
                res.size = *size;
            } else {
                res.size = sizeof(uint8_t*);
            }
            return res;
        }
    };

    struct Program {
        std::vector<Function> functions;
        std::vector<Value> constants;

        Const appendConstant(Value val) {
            Const konst = constants.size();
            constants.push_back(val);
            return konst;
        }
    };
}
