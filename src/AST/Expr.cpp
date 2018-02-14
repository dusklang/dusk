//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "Expr.hpp"
#include "Decl.hpp"

std::string IntegerLiteralExpr::prettyPrint(int indentationLevel) const {
    return indentation(indentationLevel) + literal;
}

std::string DecimalLiteralExpr::prettyPrint(int indentationLevel) const {
    return indentation(indentationLevel) + literal;
}

std::string DeclRefExpr::prettyPrint(int indentationLevel) const {
    std::string str = name;
    for(auto& params: argLists) {
        str += "(";
        bool first = true;
        for(auto& arg: params) {
            if(!first) str += ", ";
            else first = false;

            str += arg.prettyPrint();
        }
        str += ")";
    }
    return indentation(indentationLevel) + str;
}
