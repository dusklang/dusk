//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "Parser.hpp"
#include "AST/Expr.hpp"
#include <vector>
#include <memory>
#include <iostream>

std::shared_ptr<ScopeNode> Parser::parseScope() {
    std::vector<std::shared_ptr<ASTNode>> nodes;
    while(current()) {
        if(auto node = parseNode()) {
            nodes.push_back(node);
            next();
        } else {
            previous();
            break;
        }
    }
    return std::make_shared<ScopeNode>(nodes);
}

std::shared_ptr<ASTNode> Parser::parseNode() {
    if(!current()) return nullptr;

    // TODO: This line is a hack and should be replaced with a more robust solution to ending ScopeNode parsing.
    // Currently the user can enter an arbitrary number of '}' and nothing will fail
    // lexically.
    if(current()->is(tok::sep_right_curly)) return nullptr;
    if(auto prototype = parseDeclPrototype()) {
        next();
        // There's no surefire way to tell what the prototype is until we see what comes
        // after it.
        if(auto decl = parseDecl(*prototype))
            return std::make_shared<Decl>(*decl);
        if(auto expr = parseDeclRefExpr(*prototype)) {
            previous();
            return expr;
        }
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
    if(!(current() && current()->is(tok::sep_colon))) return llvm::None;
    next();
    auto type = parseExpr();
    assert(type && "Expected type expression after declaration header");
    next();
    bool isConstant;

    if(current()) {
        if(current()->is(tok::sep_colon) || current()->is(tok::sep_equal)) {
            if(current()->is(tok::sep_colon))
                isConstant = true;
            else isConstant = false;
            next();
            auto expression = parseExpr();
            assert(expression && "Expected expression after colon or equal sign");
            return Decl::Decl(prototype, type, isConstant, expression);
        } else if(current()->is(tok::sep_left_curly)) {
            isConstant = true;
            next();
            auto body = parseScope();
            assert(next() && current()->is(tok::sep_right_curly) && "Expected '}' at end of declaration");
            return Decl::Decl(prototype, type, isConstant, body);
        }
    }
    assert(false && "Expected ':', '=' or '{' after type annotation");
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
