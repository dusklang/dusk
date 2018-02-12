//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "Parser.hpp"

llvm::Optional<Decl> Parser::parseDecl() {
    if(!current()) return llvm::None;
    auto name = parseIdentifer();
    assert(name && "Expected declaration name.");
    llvm::SmallVector<llvm::SmallVector<Param, 2>, 1> paramLists;
    while(next() && current()->is(tok::sep_left_paren)) {
        llvm::SmallVector<Param, 2> parameters;
        do {
            next();
            auto paramName = parseIdentifer();
            assert(paramName && "Expected parameter name.");
            assert(next() && current()->is(tok::sep_colon) && "Expected colon after parameter name.");
            next();
            auto paramType = parseIdentifer();
            assert(paramType && "Expected parameter type expression.");
            parameters.push_back(Param::Param(*paramName, *paramType));
        } while(next() && current()->is(tok::sep_comma));
        paramLists.push_back(parameters);
    }
    assert(current() && current()->is(tok::sep_colon) && "Expected colon after declaration header");
    next();
    auto type = parseIdentifer();
    assert(type && "Expected type expression after declaration header");
    return Decl::Decl(*name, paramLists, *type);
}
