//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "AST.hpp"
#include "Decl.hpp"
#include "Expr.hpp"

std::string Param::prettyPrint() const {
    return name + ": " + value->prettyPrint();
}

std::string Decl::prettyPrint() const {
    return prototype.prettyPrint() + " : " + type->prettyPrint() + (isConstant ? " : " : " = ") + expression->prettyPrint();
}
