//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <variant>
#include <map>

#include "General/SourceInfo.h"
#include "Ident.h"

struct StructDecl;
struct Type;

enum class Signedness: uint8_t {
    Signed, Unsigned
};
struct IntTy {
    int bitWidth;
    Signedness signedness;

    bool operator==(IntTy other) { return bitWidth == other.bitWidth && signedness == other.signedness; }

    #define _I_(w) static IntTy I ## w() { return IntTy { w, Signedness::Signed }; }
    #define _U_(w) static IntTy U ## w() { return IntTy { w, Signedness::Unsigned }; }
    _I_(8) _I_(16) _I_(32) _I_(64)
    _U_(8) _U_(16) _U_(32) _U_(64)
};
struct VoidTy {};
struct NeverTy {};
struct BoolTy {};
struct FloatTy {};
struct DoubleTy {};
struct ErrorTy {};
struct StructTy {
    Ident name;
    StructDecl* decl = nullptr;

    bool operator==(StructTy& other) const {
        return name == other.name && decl == other.decl;
    }
    bool operator!=(StructTy& other) const {
        return !(*this == other);
    }

    static StructTy get(Ident name) {
        return StructTy { name };
    }
};
struct PointerTy {
    Type* pointedTy;
    static PointerTy get(Type pointedTy);
};
struct TyVariable {
    enum Kind {
        General, Integer, Decimal
    };
    uint32_t num;
    Kind kind;

    bool operator==(TyVariable const& other) const {
        return num == other.num && kind == other.kind;
    }

    static TyVariable get(uint32_t num, Kind kind = General) {
        return TyVariable { num, kind };
    }
};

struct Type final {
    typedef std::variant<IntTy, TyVariable, VoidTy, NeverTy, BoolTy, FloatTy, DoubleTy, ErrorTy, StructTy, PointerTy> Data;

    Data data;
    SourceRange range;

    Type(Data data, SourceRange range = SourceRange()) : data(data), range(range) {}
    Type(IntTy ty) : data(ty) {}
    Type(ErrorTy ty) : data(ty) {}
    Type(VoidTy ty) : data(ty) {}
    Type(BoolTy ty) : data(ty) {}
    Type(FloatTy ty) : data(ty) {}
    Type(DoubleTy ty) : data(ty) {}
    Type(PointerTy ty) : data(ty) {}
    Type(StructTy ty) : data(ty) {}
    Type(TyVariable ty) : data(ty) {}
    Type(NeverTy ty) : data(ty) {}

    Type* pointeeType() const;

    Type(Type const& other) = default;
    Type& operator=(Type const& other) = default;

    bool operator==(Type other) const;
    bool operator!=(Type other) const { return !(*this == other); }

    bool isConvertibleTo(Type other) const;

    std::string name() const;

    Type substituting(std::map<int, Type> const& solution) const;
    void substitute(std::map<int, Type> const& solution) {
        *this = substituting(solution);
    }
};
