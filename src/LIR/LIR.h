//  Copyright © 2019 Zach Wolfe. All rights reserved.

#pragma once

#include <variant>
#include <optional>
#include <string>

#include "General/General.h"
#include "General/Array.h"

namespace lir {
    /// Basic block index, local to a function.
    using BB = uint16_t;
    /// Function index.
    using Func = uint16_t;

    enum class OpCode: uint8_t {
        /// dest = &operand
        GetAddress,
        /// dest = *operand
        Load,
        /// dest = operand
        Copy,

        /// dest = -operand
        Negative,
        /// dest = a + b
        WrappingAdd,
        /// dest = a - b
        WrappingSub,
        /// dest = a * b
        Mult,
        /// dest = a / b
        Div,
        /// dest = a % b
        Mod,
        /// dest = operand as i{dest bit width}
        SignExtend,
        /// dest = operand as u{dest bit width}
        ZeroExtend,
        /// dest = a <= b
        LTE,
        /// dest = a < b
        LT,
        /// dest = a >= b
        GTE,
        /// dest = a > b
        GT,
        /// dest = a <= b
        FLTE,
        /// dest = a < b
        FLT,
        /// dest = a >= b
        FGTE,
        /// dest = a > b
        FGT,
        /// dest = a == b
        Equal,
        /// dest = a != b
        NotEqual,

        /// dest = a + b
        FAdd,
        /// dest = a - b
        FSub,
        /// dest = a * b
        FMult,
        /// dest = a / b
        FDiv,
        /// dest = a % b
        FMod,

        /// dest = !operand
        LogicalNot,
        /// dest = a && b
        LogicalAnd,
        /// dest = a || b
        LogicalOr,
        /// dest = a & b
        BitwiseAnd,
        /// dest = a | b
        BitwiseOr,

        /// goto branch
        Branch,
        /// if !condition { goto branch }
        CondBranch,

        /// dest = function(arguments...)
        Call,
        /// return operand
        Return,
        /// return
        ReturnVoid,

        /// Signals that we should never get to this point in execution.
        Unreachable,
    };

    struct Operand;

    /// Memory location, expressed either as a byte offset from a specified base or a raw pointer.
    struct MemoryLoc {
        enum {
            /// Offset from the beginning of the current stack frame.
            StackFrame,
            /// Offset from the beginning of the global variables.
            GlobalVariable,
            /// Index into the extern global variable array.
            ExternGlobalVariable,
            /// Offset from the beginning of the global constants.
            GlobalConstant,
            /// A pointer.
            Indirect,
        } base;
        union {
            /// The offset in bytes* from base.
            ///     *unless base is ExternGlobalVariable, in which case offset is an index into the
            ///     extern global variable array.
            uint64_t offset;

            /// The pointer. This must be dynamically allocated because Operand contains a MemoryLoc.
            ///
            /// TODO: We probably only ever have one layer of indirection, so we should be able to
            /// break the recursion without dynamic allocation.
            Operand* pointer;
        };
    };

    /// A small constant value that can be passed directly to instructions.
    struct Value {
        /// Size in bytes.
        uint64_t size;
        union {
            /// Valid iff size <= 64 / 8
            uint64_t u64;
            uint32_t u32;
            uint16_t u16;
            uint8_t  u8;
            float    f32;
            double   f64;
            bool     boolean;

            /// Valid iff size > 64 / 8
            uint8_t* data;
        };
    };

    /// An operand of an instruction. Either a memory location or a constant.
    struct Operand {
        enum {
            Location,
            Constant,
        } kind;
        union {
            MemoryLoc location;
            Value constant;
        };

        Operand() {}
        Operand(MemoryLoc location) : kind(Location), location(location) {}
        Operand(Value constant) : kind(Constant), constant(constant) {}
    };

    struct Argument {
        Operand operand;
        uint64_t size;
    };

    struct Instruction {
        union {
            MemoryLoc dest;
            Operand condition;
        };
        union {
            struct {
                union {
                    Operand operand;
                    struct {
                        Operand a, b;
                    } operands;
                };
                /// Used only for ZeroExtend and SignExtend instructions.
                uint64_t destSize;
                uint64_t size;
            };
            lir::BB branch;
            struct {
                lir::BB trueBranch, falseBranch;
            };
            struct {
                Func function;
                Array<Argument> arguments;
            };
        };
        OpCode op;
    };

    struct BasicBlock {
        Array<Instruction> instructions;
    };

    struct Function {
        std::string name;
        Array<BasicBlock> basicBlocks;
        uint64_t frameSize = 0;
        uint64_t returnValueSize;
        bool isExtern;
    };

    struct ExternGlobal {
        std::string name;
        uint64_t size;
    };

    struct Program {
        Array<Function> functions;
        Array<uint8_t> constants;
        /// The initial values of all the globals in the program.
        Array<uint8_t> globals;
        Array<ExternGlobal> externGlobals;
    };
}
