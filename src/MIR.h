#pragma once

#include <stdint.h>

#include "Collections.h"
#include "Misc.h"

namespace mir {
    enum class TypeKind: uint8_t {
        // TODO: Move Error type and the type variable stuff into data structures internal to the type
        // checker. That way in later phases we only have to worry about well-formed types.
        Error,
        UnknownInt,
        UnknownFloat,
        Int,
        Float,
        Bool,
        Void,
    };
    enum class IntWidth: uint8_t {
        W8, W16, W32, W64,
    };
    enum class FloatWidth: uint8_t {
        W32, W64
    };
    // TODO: Move Error type and the type variable stuff into data structures internal to the type
    // checker. That way in later phases we only have to worry about well-formed types.
    struct TypeVariableID {
        uint32_t id;
        explicit TypeVariableID(uint32_t id) : id(id) {}
    };
    struct Type {
        TypeKind kind = TypeKind::Error;
        union {
            struct {
                IntWidth width;
                bool isSigned;
            } intTy;
            FloatWidth floatTy;
        };
        // TODO: Move Error type and the type variable stuff into data structures internal to the type
        // checker. That way in later phases we only have to worry about well-formed types.
        TypeVariableID id = TypeVariableID(-1);

        Type() {}
        Type(TypeKind kind) : kind(kind) {}
        Type(IntWidth width, bool isSigned) : kind(TypeKind::Int), intTy { width, isSigned } {}
        Type(FloatWidth width) : kind(TypeKind::Float), floatTy(width) {}

        bool operator == (Type const& other) {
            switch(kind) {
                case TypeKind::Error: panic("cannot compare error type with any other");
                case TypeKind::UnknownInt:
                case TypeKind::UnknownFloat:
                case TypeKind::Bool:
                case TypeKind::Void:
                    return kind == other.kind;
                case TypeKind::Int:
                    if(kind != other.kind) return false;
                    return intTy.width == other.intTy.width && intTy.isSigned == other.intTy.isSigned;
                case TypeKind::Float:
                    if(kind != other.kind) return false;
                    return floatTy == other.floatTy;
            }
        }
        bool operator != (Type const& other) {
            return !(*this == other);
        }
    };
    // TODO: Move Error type and the type variable stuff into data structures internal to the type
    // checker. That way in later phases we only have to worry about well-formed types.
    struct TypeVariable {
        Array<Type*> locations;
        Type type;

        TypeVariable(Type* location, Type type = {}) : locations { location }, type(type) {}
    };
}
