//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <string>
#include <cassert>
#include <sstream>

#include "Type.h"

std::string Type::name() const {
    struct NameVisitor {
        std::ostringstream stream;
        NameVisitor(std::ostringstream stream) : stream(std::move(stream)) {}

        void operator()(Variable typeVariable) {
            stream << "<T" << typeVariable.num;
            switch(typeVariable.kind) {
                case Variable::Integer: stream << ": Integer"; break;
                case Variable::Decimal: stream << ": Decimal"; break;
                default: break;
            }
            stream << '>';
        }
        void operator()(IntegerTy properties) {
            stream << (properties.isSigned ? "i" : "u") << properties.bitWidth;
        }
        void operator()(VoidTy) {
            stream << "void";
        }
        void operator()(BoolTy) {
            stream << "bool";
        }
        void operator()(FloatTy) {
            stream << "f32";
        }
        void operator()(DoubleTy) {
            stream << "f64";
        }
        void operator()(ErrorTy) {
            stream << "#ERRORTYPE#";
        }
    };
    std::ostringstream stream;
    for(uint8_t i = 0; i < indirection; ++i) {
        stream << "*";
    }
    NameVisitor visitor(std::move(stream));
    std::visit(visitor, data);
    return visitor.stream.str();
}
