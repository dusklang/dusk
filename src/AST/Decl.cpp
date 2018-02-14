//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "AST.hpp"
#include "Decl.hpp"
#include "Expr.hpp"

std::string Param::prettyPrint(int indentationLevel) const {
    return name + ": " + value->prettyPrint();
}

std::string Argument::prettyPrint(int indentationLevel) const {
    return name + ": " + value->prettyPrint();
}

std::string Decl::prettyPrint(int indentationLevel) const {
    std::string str = indentation(indentationLevel) + prototype.prettyPrint() + " : " + type->prettyPrint();
    if(expression()) str += expression()->prettyPrint();
    else str += " {\n" + body()->prettyPrint(indentationLevel) + "\n" + indentation(indentationLevel) + "}";
    return str;
}
