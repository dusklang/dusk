//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <string>
#include "General/General.h"
#include <sstream>
#include "mpark/patterns.hpp"

using namespace mpark::patterns;

#include "Type.h"

PointerTy PointerTy::get(Type pointedTy) {
    return PointerTy { new Type(pointedTy) };
}

std::string Type::name() const {
    std::ostringstream stream;
    match(data)(
        pattern(as<TyVariable>(arg)) = [&](auto typeVariable) {
            stream << "<T" << typeVariable.num;
            switch(typeVariable.kind) {
                case TyVariable::Integer: stream << ": Integer"; break;
                case TyVariable::Decimal: stream << ": Decimal"; break;
                default: break;
            }
            stream << '>';
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
       pattern(as<TyVariable>(arg), as<TyVariable>(arg))
           = [](auto lhs, auto rhs) { return lhs == rhs; },
       pattern(as<StructTy>(arg), as<StructTy>(arg))
           = [](auto lhs, auto rhs) { return lhs == rhs; },
       pattern(as<VoidTy>(_), as<VoidTy>(_)) = [] { return true; },
       pattern(as<BoolTy>(_), as<BoolTy>(_)) = [] { return true; },
       pattern(as<FloatTy>(_), as<FloatTy>(_)) = [] { return true; },
       pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [] { return true; },
       pattern(as<ErrorTy>(_), as<ErrorTy>(_)) = [] { return true; },
       pattern(as<PointerTy>(ds(arg)), as<PointerTy>(ds(arg))) = [](auto lhs, auto rhs) {
           return *lhs == *rhs;
       },
       pattern(_, _) = [] { return false; }
    );
}

Type Type::substituting(std::map<int, Type> const& solution) const {
    return match(data)(
       pattern(as<TyVariable>(arg)) = [&](auto var) -> Type {
           for(auto const& solution: solution) {
               if(solution.first == var.num) {
                   return solution.second;
               }
           }
           return TyVariable::get(var.num, var.kind);
       },
       pattern(_) = [&] { return *this; }
    );
}
