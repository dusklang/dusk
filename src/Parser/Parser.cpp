//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "Parser.hpp"
#include "AST/Expr.hpp"
#include <iostream>

std::shared_ptr<ASTNode> Parser::parseNode() {
    if(!current()) return nullptr;
    if(auto prototype = parseDeclPrototype()) {
        next();
        // There's no surefire way to tell what the prototype is until we see what comes
        // after it.
        if(auto decl = parseDecl(*prototype))
            return std::make_shared<Decl>(*decl);
    }
    return nullptr;
}

llvm::Optional<DeclPrototype> Parser::parseDeclPrototype() {
    if(!current()) return llvm::None;
    auto name = parseIdentifer();
    assert(name && "Expected declaration name.");
    llvm::SmallVector<llvm::SmallVector<Param, 2>, 1> paramLists;
    while(next() && current()->is(tok::sep_left_paren)) {
        llvm::SmallVector<Param, 2> parameters;
        do {
            next();
            auto param = parseIdentifer();
            assert(param && "Expected parameter name.");
            assert(next() && current()->is(tok::sep_colon) && "Expected ':' after parameter name.");
            next();
            auto argument = parseExpr();
            assert(argument && "Expected argument value or type.");
            parameters.push_back(Param::Param(*param, argument));
        } while(next() && current()->is(tok::sep_comma));
        paramLists.push_back(parameters);
    }
    assert(previous());

    return DeclPrototype(*name, paramLists);
}

llvm::Optional<Decl> Parser::parseDecl(DeclPrototype prototype) {
    if(!current() || !current()->is(tok::sep_colon)) return llvm::None;
    next();
    auto type = parseExpr();
    assert(type && "Expected type expression after declaration header");
    next();
    bool isConstant;
    if(current() && current()->is(tok::sep_colon)) {
        isConstant = true;
    } else if(current() && current()->is(tok::sep_equal)) {
        isConstant = false;
    } else {
        // TODO: add support for '{' and function bodies here
        assert(false && "Expected ':' or '=' after type annotation");
    }
    next();
    auto expression = parseExpr();
    assert(expression && "Expected expression after colon or equal sign");
    return Decl::Decl(prototype, type, isConstant, expression);
}

std::shared_ptr<Expr> Parser::parseDeclRefExpr(DeclPrototype prototype) {
    return std::make_unique<DeclRefExpr>(DeclRefExpr(prototype));
}

std::shared_ptr<Expr> Parser::parseExpr() {
    if(auto intVal = parseIntegerLiteral()) {
        return std::make_unique<IntegerLiteralExpr>(IntegerLiteralExpr(*intVal));
    } else if(auto decimalVal = parseDecimalLiteral()) {
        return std::make_unique<DecimalLiteralExpr>(DecimalLiteralExpr(*decimalVal));
    } else if(auto prototype = parseDeclPrototype()) {
        return parseDeclRefExpr(*prototype);
    }

    return nullptr;
}
