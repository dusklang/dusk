//  Copyright © 2019 Zach Wolfe. All rights reserved.

#pragma once

#include <variant>
#include <optional>
#include <string>

#include "General/General.h"
#include "General/Array.h"

namespace lir {
    /// Basic block index, local to a function.
    using BB = size_t;
    /// Function index.
    using Func = size_t;

    enum class OpCode: uint8_t {
        /// &operand
        GetAddress,
        /// *operand
        Load,
        /// *a = b
        Store,

        /// -operand
        Negative,
        /// a + b
        WrappingAdd,
        /// a - b
        WrappingSub,
        /// a * b
        Mult,
        /// a / b
        Div,
        /// a % b
        Mod,
        /// operand as type
        SignExtend,
        /// operand as type
        ZeroExtend,
        /// a <= b
        LTE,
        /// a < b
        LT,
        /// a >= b
        GTE,
        /// a > b
        GT,
        /// a <= b
        FLTE,
        /// a < b
        FLT,
        /// a >= b
        FGTE,
        /// a > b
        FGT,
        /// a == b
        Equal,
        /// a != b
        NotEqual,

        /// a + b
        FAdd,
        /// a - b
        FSub,
        /// a * b
        FMult,
        /// a / b
        FDiv,
        /// a % b
        FMod,

        /// !operand
        LogicalNot,
        /// a && b
        LogicalAnd,
        /// a || b
        LogicalOr,
        /// a & b
        BitwiseAnd,
        /// a | b
        BitwiseOr,

        /// goto branch
        Branch,
        /// if !condition { goto branch }
        CondBranch,

        /// function(arguments...)
        Call,
        /// return operand
        Return,
        /// return
        ReturnVoid,

        /// Signals that we should never get to this point in execution.
        Unreachable,
    };

    /// A LIR type.
    struct LIRType {
        enum {
            /// Terminates a basic block (aka, never returns).
            Terminal,
            /// No value.
            Void,
            /// Bool (right now anyway).
            i1,

            i8,
            i16,
            i32,
            i64,

            f32,
            f64,

            Pointer,
            Structure,
            Array,
        } kind;

        union {
            LIRType const* pointee;
            ::Array<LIRType const*> fields;
            struct {
                LIRType const* arrayElement;
                size_t count;
            };
        };
    };

    struct ValueBase {
        size_t index;
        LIRType const* type;
    };

    /// A value known at compile-time.
    struct ConstantValue {
        LIRType const* type;
        union {
            bool i1;
            uint8_t i8;
            uint16_t i16;
            uint32_t i32;
            uint64_t i64;
            float f32;
            double f64;
            Array<ConstantValue> fields;
            Array<ConstantValue> elements;
        };
    };

    struct Value {
        enum {
            /// An argument value.
            Argument,
            /// The result of executing an instruction.
            Instr,
            /// Pointer to a global variable.
            GlobalVariable,
            /// Pointer to C global variable.
            ExternGlobalVariable,
            /// Pointer to a global constant.
            GlobalConstant,
            /// Inline constant.
            InlineConstant,
        } kind;

        union {
            struct {
                /// For qualifying instr.
                BB bb;
                size_t index;
            };
            ConstantValue inlineConstant;
        };

        Value(struct Parameter);
        Value(struct Instr);
        Value(struct GlobalVariable);
        Value(struct ExternGlobalVariable);
        Value(struct GlobalConstant);
        Value(struct ConstantValue inlineConstant) : kind(InlineConstant), inlineConstant(inlineConstant) {}
    };

    struct Parameter: public ValueBase {};
    struct Instr: public ValueBase {
        BB basicBlock;
        OpCode op;
        union {
            Value operand;
            struct { Value a, b; } operands;
            BB branch;
            struct {
                Value condition;
                BB trueBranch, elseBranch;
            };
            struct {
                Func func;
                Array<Value> arguments;
            };
        };
    };
    struct GlobalVariable: public ValueBase {
        std::optional<ConstantValue> initialValue;
    };
    struct ExternGlobalVariable: public ValueBase {
        std::string name;
    };
    struct GlobalConstant: public ValueBase {
        ConstantValue value;
    };

    struct BasicBlock {
        Array<Instr> instructions;
    };

    struct Function {
        std::string name;
        LIRType const* returnType;
        Array<Parameter> parameters;
        Array<BasicBlock> basicBlocks;
        bool isExtern;
    };

    struct Program {
        Array<Function> functions;
        Array<GlobalConstant> constants;
        Array<GlobalVariable> globals;
        Array<ExternGlobalVariable> externGlobals;

        void print() const;
    };
}
