//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <string>
#include <cassert>
#include "AST.h"

std::string Type::name() const {
    struct NameVisitor: public boost::static_visitor<void> {
        std::ostringstream stream;
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
        void operator()(PointerTy pointer) {
            stream << '*' << pointer.pointedTy->name();
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
    NameVisitor visitor;
    boost::apply_visitor(visitor, data);
    return visitor.stream.str();
}
