//  Copyright © 2019 Zach Wolfe. All rights reserved.

#pragma once

#include <variant>
#include <unordered_map>

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
struct IntLitVariable {};

struct TypeLayout {
    uint32_t size, alignment;
    Array<uint32_t> fieldOffsets;

    uint32_t stride() const {
        return (alignment - (size % alignment)) % alignment;
    }
};

struct Type final {
private:
    std::optional<TypeLayout> _layout = std::nullopt;
public:
    using Data = std::variant<IntTy, IntLitVariable, VoidTy, NeverTy, BoolTy, FloatTy, DoubleTy, ErrorTy, StructTy, PointerTy>;
    Data data;
    SourceRange range;

    Type() = default;
    Type(Data data, SourceRange range = SourceRange()) : data(data), range(range) {}
    Type(IntTy ty) : data(ty) {}
    Type(ErrorTy ty) : data(ty) {}
    Type(VoidTy ty) : data(ty) {}
    Type(BoolTy ty) : data(ty) {}
    Type(FloatTy ty) : data(ty) {}
    Type(DoubleTy ty) : data(ty) {}
    Type(PointerTy ty) : data(ty) {}
    Type(StructTy ty) : data(ty) {}
    Type(IntLitVariable ty) : data(ty) {}
    Type(NeverTy ty) : data(ty) {}

    Type* pointeeType() const;

    Type(Type const& other) = default;
    Type& operator=(Type const& other) = default;

    bool operator==(Type other) const;
    bool operator!=(Type other) const { return !(*this == other); }

    bool isConvertibleTo(Type other) const;
    bool isVariable() const { return std::holds_alternative<IntLitVariable>(data); }

    std::string name() const;

    // TODO: Move layout code into LIR.
    TypeLayout const& layout();

    Type substituting(std::unordered_map<int, Type> const& solution) const;
    void substitute(std::unordered_map<int, Type> const& solution) {
        *this = substituting(solution);
    }
};
