//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <variant>
#include <optional>
#include <map>

#include "General/SourceInfo.h"

struct StructDecl;
struct Type;

enum class Signedness: uint8_t {
    Signed, Unsigned
};
struct IntTy {
    int bitWidth;
    Signedness signedness;

    bool operator==(IntTy other) { return bitWidth == other.bitWidth && signedness == other.signedness; }

    static IntTy I8() {
        return IntTy { 8, Signedness::Signed };
    }
    static IntTy I16() {
        return IntTy { 16, Signedness::Signed };
    }
    static IntTy I32() {
        return IntTy { 32, Signedness::Signed };
    }
    static IntTy I64() {
        return IntTy { 64, Signedness::Signed };
    }
    static IntTy U8() {
        return IntTy { 8, Signedness::Unsigned };
    }
    static IntTy U16() {
        return IntTy { 16, Signedness::Unsigned };
    }
    static IntTy U32() {
        return IntTy { 32, Signedness::Unsigned };
    }
    static IntTy U64() {
        return IntTy { 64, Signedness::Unsigned };
    }
};
struct VoidTy {};
struct BoolTy {};
struct FloatTy {};
struct DoubleTy {};
struct ErrorTy {};
struct StructTy {
    std::string name;
    StructDecl* decl = nullptr;

    bool operator==(StructTy other) const {
        return name == other.name && decl == other.decl;
    }

    static StructTy get(std::string name) {
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
    typedef std::variant<IntTy, TyVariable, VoidTy, BoolTy, FloatTy, DoubleTy, ErrorTy, StructTy, PointerTy> DataType;

    DataType data;
    std::optional<SourceRange> sourceRange = std::nullopt;

    Type(DataType data) : data(data) {}
    Type(IntTy ty) : data(ty) {}
    Type(ErrorTy ty) : data(ty) {}
    Type(VoidTy ty) : data(ty) {}
    Type(BoolTy ty) : data(ty) {}
    Type(FloatTy ty) : data(ty) {}
    Type(DoubleTy ty) : data(ty) {}
    Type(PointerTy ty) : data(ty) {}
    Type(StructTy ty) : data(ty) {}
    Type(TyVariable ty) : data(ty) {}

    Type* pointeeType() const;

    ~Type() = default;
    Type(Type const& other) = default;
    Type& operator=(Type const& other) = default;

    bool operator==(Type other) const;
    bool operator!=(Type other) const { return !(*this == other); }

    std::string name() const;

    Type substituting(std::map<int, Type> const& solution) const;
    void substitute(std::map<int, Type> const& solution) {
        *this = substituting(solution);
    }
};
