//  Copyright © 2019 Zach Wolfe. All rights reserved.

#pragma once

#include <variant>
#include <optional>
#include <vector>
#include <string>

#include "General/General.h"

namespace lir {
    /// Variable index.
    struct Var {
        uint16_t index;
        bool isGlobal;
    };
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

    /// A mutable operand to an instruction.
    struct RWOperand {
        /// `offset` and `size` should be used for indexing into arrays or referencing
        /// the fields of structs. Both are in bytes.
        uint32_t offset, size;
        Var variable;
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
        /// `offset` and `size` should be used for indexing into arrays or referencing
        /// the fields of structs. Both are in bytes.
        uint32_t offset, size;
        union {
            Var variable;
            // TODO: maybe local constants should be stored in an array on Function and indexed into?
            // Or just have global constants be the only kind?
            Value localConstant;
            Const globalConstant;
        };
    };

    struct Instruction {
        union {
            RWOperand dest;
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
            Var var = { (uint16_t)variables.size(), false };
            variables.push_back(variable);
            return var;
        }

        Instr appendInstruction(Instruction instruction) {
            Instr instr = instructions.size();
            instructions.push_back(instruction);
            return instr;
        }
    };

    struct Program {
        std::vector<Function> functions;
        std::vector<Value> constants;
        std::vector<Variable> globals;

        Const appendConstant(Value val) {
            Const konst = constants.size();
            constants.push_back(val);
            return konst;
        }

        Var appendGlobal(Variable var) {
            Var variable = { (uint16_t)globals.size(), true };
            globals.push_back(var);
            return variable;
        }
    };
}
