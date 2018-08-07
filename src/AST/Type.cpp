//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <string>
#include <cassert>
#include <sstream>
#include "mpark/patterns.hpp"

using namespace mpark::patterns;

#include "Type.h"

std::string Type::name() const {
    std::ostringstream stream;
    for(uint8_t i = 0; i < indirection; ++i) {
        stream << "*";
    }
    match(data)(
        pattern(as<Variable>(arg)) = [&](auto typeVariable) {
            stream << "<T" << typeVariable.num;
            switch(typeVariable.kind) {
                case Variable::Integer: stream << ": Integer"; break;
                case Variable::Decimal: stream << ": Decimal"; break;
                default: break;
            }
            stream << '>';
        },
        pattern(as<IntegerTy>(arg)) = [&](auto properties) {
            stream << (properties.isSigned ? "i" : "u") << properties.bitWidth;
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
        pattern(as<ErrorTy>(_)) = [&] {
            stream << "#ERRORTYPE#";
        }
    );
    return stream.str();
}

bool Type::operator==(Type other) const {
    return match(data, other.data)(
       pattern(as<IntegerTy>(arg), as<IntegerTy>(arg))
           = [](auto lhs, auto rhs) { return lhs == rhs; },
       pattern(as<Variable>(arg), as<Variable>(arg))
           = [](auto lhs, auto rhs) { return lhs == rhs; },
       pattern(as<StructTy>(arg), as<StructTy>(arg))
           = [](auto lhs, auto rhs) { return lhs == rhs; },
       pattern(as<VoidTy>(_), as<VoidTy>(_)) = [] { return true; },
       pattern(as<BoolTy>(_), as<BoolTy>(_)) = [] { return true; },
       pattern(as<FloatTy>(_), as<FloatTy>(_)) = [] { return true; },
       pattern(as<DoubleTy>(_), as<DoubleTy>(_)) = [] { return true; },
       pattern(as<ErrorTy>(_), as<ErrorTy>(_)) = [] { return true; },
       pattern(_, _) = [] { return false; }
   ) && (indirection == other.indirection);
}

Type Type::substituting(std::map<int, Type> const& solution) const {
    return match(data)(
       pattern(as<Variable>(arg)) = [&](auto var) {
           for(auto const& solution: solution) {
               if(solution.first == var.num) {
                   return solution.second;
               }
           }
           return Type::TypeVariable(var.num, var.kind);
       },
       pattern(_) = [&] { return *this; }
    );
}
