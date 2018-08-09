//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <variant>
#include <optional>
#include <map>

#include "General/SourceLoc.h"

struct StructDecl;

struct Type final {
    struct IntegerTy {
        int bitWidth;
        bool isSigned;

        bool operator==(IntegerTy other) { return bitWidth == other.bitWidth && isSigned == other.isSigned; }
    };
    struct VoidTy {};
    struct BoolTy {};
    struct FloatTy {};
    struct DoubleTy {};
    struct ErrorTy {};
    struct StructTy {
        std::string name;
        StructDecl* decl;

        bool operator==(StructTy other) const {
            return name == other.name && decl == other.decl;
        }
    };
    struct PointerTy {
        Type* pointedTy;
    };
    struct Variable {
        enum Kind {
            General, Integer, Decimal
        };
        int num;
        Kind kind;

        bool operator==(Variable const& other) const {
            return num == other.num && kind == other.kind;
        }
    };

    typedef std::variant<IntegerTy, Variable, VoidTy, BoolTy, FloatTy, DoubleTy, ErrorTy, StructTy, PointerTy> DataType;

    DataType data;
    std::optional<SourceRange> sourceRange;

    Type(DataType data,
         std::optional<SourceRange> sourceRange = std::nullopt)
    : data(data), sourceRange(sourceRange) {}
    static Type Integer(int bitWidth, bool isSigned, std::optional<SourceRange> sourceRange = std::nullopt) {
        return Type(IntegerTy { bitWidth, isSigned }, sourceRange);
    }
    static Type I8(std::optional<SourceRange> sourceRange = std::nullopt) {
        return Integer(8, true, sourceRange);
    }
    static Type I16(std::optional<SourceRange> sourceRange = std::nullopt) {
        return Integer(16, true, sourceRange);
    }
    static Type I32(std::optional<SourceRange> sourceRange = std::nullopt) {
        return Integer(32, true, sourceRange);
    }
    static Type I64(std::optional<SourceRange> sourceRange = std::nullopt) {
        return Integer(64, true, sourceRange);
    }
    static Type U8(std::optional<SourceRange> sourceRange = std::nullopt) {
        return Integer(8, false, sourceRange);
    }
    static Type U16(std::optional<SourceRange> sourceRange = std::nullopt) {
        return Integer(16, false, sourceRange);
    }
    static Type U32(std::optional<SourceRange> sourceRange = std::nullopt) {
        return Integer(32, false, sourceRange);
    }
    static Type U64(std::optional<SourceRange> sourceRange = std::nullopt) {
        return Integer(64, false, sourceRange);
    }
    static Type Error() { return Type(ErrorTy()); }

    static Type Pointer(Type pointedTy, std::optional<SourceRange> sourceRange = std::nullopt) {
        return Type(PointerTy { new Type(pointedTy) }, sourceRange);
    }
    static Type TypeVariable(int number, Variable::Kind kind = Variable::General) {
        return Type(Variable { number });
    }
    static Type Void(std::optional<SourceRange> sourceRange = std::nullopt) {
        return Type(VoidTy(), sourceRange);
    }
    static Type Bool(std::optional<SourceRange> sourceRange = std::nullopt) {
        return Type(BoolTy(), sourceRange);
    }
    static Type Float(std::optional<SourceRange> sourceRange = std::nullopt) {
        return Type(FloatTy(), sourceRange);
    }
    static Type Double(std::optional<SourceRange> sourceRange = std::nullopt) {
        return Type(DoubleTy(), sourceRange);
    }
    static Type Struct(std::string name, StructDecl* decl = nullptr, std::optional<SourceRange> sourceRange = std::nullopt) {
        return Type(StructTy { name, decl }, sourceRange);
    }

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
