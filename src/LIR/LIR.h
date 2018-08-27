//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <variant>
#include <vector>

#include "General/General.h"

namespace lir {
    struct Type;
    struct VoidTy {};
    struct NeverTy {};
    enum class IntTy { I8, I16, I32, I64 };
    inline IntTy PointerTy() { return IntTy::I64; }
    struct StructTy {
        std::vector<Type*> types;
    };
    struct Type {
        using Data = std::variant<VoidTy, NeverTy, IntTy>;
        Data data;

        Type(Data data) : data(data) {}

        template<typename T>
        T& get_if() { return std::get_if<T>(&data); }

        template<typename T>
        T const& get_if() const { return std::get_if<T>(&data); }
    };

    struct Value;
    struct NOPInstr     {                                };
    struct AllocaInstr  { Value *size;                   };
    struct StoreInstr   { Value *destPtr, *srcVal;       };
    struct LoadInstr    { Type destTy;    Value *srcPtr; };
    struct SAddInstr    { Value *a,       *b;            };
    struct UAddInstr    { Value *a,       *b;            };
    struct SSubInstr    { Value *a,       *b;            };
    struct USubInstr    { Value *a,       *b;            };
    struct VoidConst    {                                };
    struct IntConst     { uint64_t val;                  };

    struct Value {
        using Data = std::variant<NOPInstr, AllocaInstr, StoreInstr, LoadInstr, SAddInstr, UAddInstr, SSubInstr, USubInstr, VoidConst, IntConst>;
        Data data;
        Type type;

        Value(NOPInstr instr) : data(instr), type(VoidTy()) {}
        Value(AllocaInstr instr) : data(instr), type(PointerTy()) {}
        Value(StoreInstr instr) : data(instr), type(VoidTy()) {}
        Value(LoadInstr instr) : data(instr), type(instr.destTy) {}
        Value(SAddInstr instr) : data(instr), type(instr.a->type) {}
        Value(UAddInstr instr) : data(instr), type(instr.a->type) {}
        Value(SSubInstr instr) : data(instr), type(instr.a->type) {}
        Value(USubInstr instr) : data(instr), type(instr.a->type) {}
        Value(VoidConst val) : data(val), type(VoidTy()) {}
        template<typename T>
        T& get_if() { return std::get_if<T>(&data); }

        template<typename T>
        T const& get_if() const { return std::get_if<T>(&data); }
    };
}

template<>
struct std::variant_size<lir::Type> {};
template<>
struct std::variant_size<lir::Value> {};
