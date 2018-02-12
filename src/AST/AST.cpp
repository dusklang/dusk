//
//  AST.cpp
//  quillc
//
//  Created by Zach Wolfe on 2018-02-12.
//

#include "AST.hpp"

std::string Decl::prettyPrint() const {
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
    str += " : " + typeName;
    return str;
}
