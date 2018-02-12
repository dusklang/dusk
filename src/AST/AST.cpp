//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "AST.hpp"

std::string DeclPrototype::prettyPrint() const {
    std::string str = name;
    for(auto& params: paramLists) {
        str += "(";
        bool first = true;
        for(auto& param: params) {
            if(!first) str += ", ";
            else first = false;

            str += param.prettyPrint();
        }
        str += ")";
    }
    return str;
}
