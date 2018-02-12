//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "Expr.hpp"
#include "Decl.hpp"

std::string IntegerLiteralExpr::prettyPrint() const {
    return literal;
}

std::string DecimalLiteralExpr::prettyPrint() const {
    return literal;
}

std::string DeclRefExpr::prettyPrint() const {
    return prototype.prettyPrint();
}
