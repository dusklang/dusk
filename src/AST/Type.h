//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <variant>
#include <map>

#include "General/SourceInfo.h"
#include "General/Enum.h"
#include "Ident.h"

struct StructDecl;
struct Type;

enum class Signedness: uint8_t {
    Signed, Unsigned
};
#define _I_(width) static IntTy I ## width() { return IntTy { width, Signedness::Signed }; }
#define _U_(width) static IntTy U ## width() { return IntTy { width, Signedness::Unsigned }; }
#define TYPE_CASES(n, firstCase, case) \
    firstCase(n, IntTy, \
        int bitWidth; \
        Signedness signedness; \
        \
        bool operator==(IntTy other) const { return bitWidth == other.bitWidth && signedness == other.signedness; } \
        _I_(8) _I_(16) _I_(32) _I_(64) \
        _U_(8) _U_(16) _U_(32) _U_(64) \
    ) \
    case(n, VoidTy) \
    case(n, BoolTy) \
    case(n, FloatTy) \
    case(n, DoubleTy) \
    case(n, ErrorTy) \
    case(n, StructTy, \
        Ident name; \
        StructDecl* decl = nullptr; \
        bool operator==(StructTy& other) const { \
            return name == other.name && decl == other.decl; \
        } \
        bool operator!=(StructTy& other) const { \
            return !(*this == other); \
        } \
        static StructTy get(Ident name) { \
            return StructTy { name }; \
        } \
    ) \
    case(n, PointerTy, \
        Type* pointedTy; \
        static PointerTy get(Type pointedTy); \
    ) \
    case(n, TyVariable, \
        enum Kind { \
            General, Integer, Decimal \
        }; \
        uint32_t num; \
        Kind kind; \
        \
        bool operator==(TyVariable const& other) const { \
            return num == other.num && kind == other.kind; \
        } \
        \
        static TyVariable get(uint32_t num, Kind kind = General) { \
            return TyVariable { num, kind }; \
        } \
    )
BEGIN_ENUM(Type, TYPE_CASES)
    SourceRange range;

    Type(Data data, SourceRange range) : data(data), range(range) {}

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
END_ENUM()
