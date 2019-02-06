//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <variant>
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
    /// Parameter index, local to a function.
    using Param = uint16_t;
    /// Function index.
    using Func = uint16_t;

    enum class OpCode: uint8_t {
        GetConstant,        /// dest = constant
        GetConstantAddress, /// dest = &constant
        GetParameter,       /// dest = parameter
        GetAddress,         /// dest = &operand
        Load,               /// dest = *operand
        Store,              /// *dest = operand
        Copy,               /// dest = operand

        Negative,           /// dest = -operand
        WrappingAdd,        /// dest = a + b
        WrappingSub,        /// dest = a - b
        SignExtend,         /// dest = operand as i{dest.bitWidth}
        ZeroExtend,         /// dest = operand as u{dest.bitWidth}
        Mult,               /// dest = a * b
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

        LogicalNot,         /// dest = !operand

        Branch,             /// goto branch
        CondBranch,         /// if condition { goto trueBranch } else { goto falseBranch }
    };
    struct Instruction {
        union {
            Var dest;
            Var condition;
        };
        union {
            struct {
                Var a, b;
            } operands;
            Var operand;
            lir::Instr branch;
            struct {
                lir::Instr trueBranch, falseBranch;
            };
            Const constant;
            Param parameter;
        };
        OpCode op;
    };

    struct Value {
        /// Size in bytes.
        uint32_t size;
        union {
            /// The inline constant values. Valid iff size <= 64.
            uint64_t u64;
            uint32_t u32;
            uint16_t u16;
            uint8_t  u8;
            float    f32;
            double   f64;
            bool     boolean;

            /// The buffer that stores the constant value. Valid iff size > 64.
            uint8_t* buffer;
        };
    };
    struct Variable {
        uint32_t size;
    };

    struct Parameter {
        std::string name;
        uint32_t size;
    };

    struct Function {
        std::vector<Parameter> parameters;
        std::vector<Instruction> instructions;
        std::vector<Value> constants;
        std::vector<Variable> variables;

        Const appendConstant(Value constant) {
            Const konst = constants.size();
            constants.push_back(constant);
            return konst;
        }

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
    };

    struct Program {
        std::vector<Function> functions;
    };
}
