//  Copyright © 2019 Zach Wolfe. All rights reserved.

#include <string>
#include "General/General.h"
#include <sstream>
#include "mpark/patterns.hpp"

#include "Expr.h"
#include "Decl.h"

using namespace mpark::patterns;

#include "Type.h"

PointerTy PointerTy::get(Type pointedTy) {
    return PointerTy { new Type(pointedTy) };
}

std::string Type::name() const {
    std::ostringstream stream;
    match(data)(
        pattern(as<IntLitVariable>(arg)) = [&](auto typeVariable) {
            stream << "<INT LITERAL TYPE VARIABLE>";
        },
        pattern(as<IntTy>(arg)) = [&](auto properties) {
            stream << (properties.signedness == Signedness::Signed ? "i" : "u") << properties.bitWidth;
        },
        pattern(as<StructTy>(arg)) = [&](auto structTy) {
            stream << structTy.name;
        },
        pattern(as<VoidTy>(_)) = [&] {
            stream << "void";
        },
        pattern(as<NeverTy>(_)) = [&] {
            stream << "never";
        },
        pattern(as<BoolTy>(_)) = [&] {
            stream << "bool";
        },
        pattern(as<FloatTy>(_)) = [&] {
            stream << "f32";
        },
        pattern(as<DoubleTy>(_)) = [&] {
            stream << "f64";
        },
        pattern(as<PointerTy>(ds(arg))) = [&](auto pointedTy) {
            stream << '*' << pointedTy->name();
        },
        pattern(as<ErrorTy>(_)) = [&] {
            stream << "#ERRORTYPE#";
        }
    );
    return stream.str();
}

Type* Type::pointeeType() const {
    return match(data)(
        pattern(as<PointerTy>(arg)) = [](auto pointer) { return pointer.pointedTy; },
        pattern(_) = []() -> Type* {
            panic("Attempt to get the pointee type of a non-pointer type");
        }
    );
}

bool Type::operator==(Type other) const {
    return match(data, other.data)(
        pattern(as<IntTy>(arg), as<IntTy>(arg))
            = [](auto lhs, auto rhs) { return lhs == rhs; },
        pattern(as<StructTy>(arg), as<StructTy>(arg))
            = [](auto lhs, auto rhs) { return lhs == rhs; },
        pattern(as<VoidTy>(_), as<VoidTy>(_)) = [] { return true; },
        pattern(as<NeverTy>(_), as<NeverTy>(_)) = [] { return true; },
        pattern(as<BoolTy>(_), as<BoolTy>(_)) = [] { return true; },
        pattern(as<FloatTy>(_), as<FloatTy>(_)) = [] { return true; },
        pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [] { return true; },
        pattern(as<ErrorTy>(_), as<ErrorTy>(_)) = [] { return true; },
        pattern(as<IntLitVariable>(_), as<IntLitVariable>(_)) = [] { return true; },
        pattern(as<PointerTy>(ds(arg)), as<PointerTy>(ds(arg))) = [](auto lhs, auto rhs) {
            return *lhs == *rhs;
        },
        pattern(_, _) = [] { return false; }
    );
}

bool Type::isConvertibleTo(Type other) const {
    return *this == other || *this == NeverTy();
}

TypeLayout const& Type::layout() {
    if(!_layout) {
        TypeLayout layout {};
        match(data)(
            pattern(as<IntTy>(arg)) = [&](auto ty) {
                layout.alignment = layout.size = ty.bitWidth / 8;
            },
            pattern(as<IntLitVariable>(arg)) = [](auto ty) {
                panic("Found int literal variable type in lirgen!");
            },
            pattern(as<VoidTy>(_)) = [&]() {
                layout.alignment = layout.size = 0;
            },
            pattern(as<NeverTy>(_)) = [&]() {
                layout.alignment = layout.size = 0;
            },
            pattern(as<BoolTy>(_)) = [&]() {
                layout.alignment = layout.size = 1;
            },
            pattern(as<FloatTy>(_)) = [&]() {
                layout.alignment = layout.size = 32 / 8;
            },
            pattern(as<DoubleTy>(_)) = [&]() {
                layout.alignment = layout.size = 64 / 8;
            },
            pattern(as<ErrorTy>(_)) = []() {
                panic("Found error type!");
            },
            pattern(as<PointerTy>(_)) = [&]() {
                layout.alignment = layout.size = sizeof(uint8_t*);
            },
            pattern(as<StructTy>(arg)) = [&](auto ty) {
                uint32_t offset = 0;
                uint32_t maxAlignment = 0;
                for(auto field: ty.decl->fields) {
                    auto fieldLayout = field->type.layout();
                    if(fieldLayout.alignment > maxAlignment) {
                        maxAlignment = fieldLayout.alignment;
                    }
                    offset += (fieldLayout.alignment - (offset % fieldLayout.alignment)) % fieldLayout.alignment;
                    layout.fieldOffsets.append(offset);
                    offset += fieldLayout.size;
                }
                layout.size = offset;
                layout.alignment = maxAlignment;
            }
        );
        _layout = std::move(layout);
    }
    return *_layout;
}
