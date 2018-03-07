//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <string>
#include <cassert>
#include "AST.h"

std::string Type::name() const {
    struct NameVisitor: public boost::static_visitor<std::string> {
        std::string operator()(int typeVariableNumber) const {
            std::ostringstream stream;
            stream << "<T" << typeVariableNumber << '>';
            return stream.str();
        }
        std::string operator()(IntProperties properties) const {
            return (properties.isSigned ? "i" : "u") + properties.bitWidth;
        }
        std::string operator()(std::shared_ptr<Type> pointedTy) const {
            return "*" + pointedTy->name();
        }
        std::string operator()(VoidTy) const {
            return "void";
        }
        std::string operator()(BoolTy) const {
            return "bool";
        }
        std::string operator()(FloatTy) const {
            return "f32";
        }
        std::string operator()(DoubleTy) const {
            return "f64";
        }
        std::string operator()(ErrorTy) const {
            return "#ERRORTYPE#";
        }
    };
    return boost::apply_visitor(NameVisitor(), data);
}
