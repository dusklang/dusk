//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "AST.hpp"
#include "Decl.hpp"
#include "Expr.hpp"

std::string Param::prettyPrint(int indentationLevel) const {
    return name + ": " + value->prettyPrint();
}

std::string Decl::prettyPrint(int indentationLevel) const {
    return indentation(indentationLevel) + prototype.prettyPrint() + " : " + type->prettyPrint() + (isConstant ? " : " : " = ") + expression->prettyPrint();
}
