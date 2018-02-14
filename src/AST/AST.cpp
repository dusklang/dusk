//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "AST.hpp"
#include <memory>

std::string ASTNode::indentation(int level) const {
    const int multiplier = 4;
    std::string str;
    for(int i = 0; i < (level * multiplier); i++) str += " ";
    return str;
};

std::string DeclPrototype::prettyPrint(int indentationLevel) const {
    std::string str = (isMut ? "mut " : "") + name;
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
    return indentation(indentationLevel) + str;
}

std::string ScopeNode::prettyPrint(int indentationLevel) const {
    std::string str;
    for(auto& node: nodes)
        str += node->prettyPrint(indentationLevel + 1) + "\n";
    if(!str.empty()) str.pop_back();
    return str;
}
