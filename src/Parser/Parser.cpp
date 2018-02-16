//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "Parser.hpp"
#include "AST/Expr.hpp"
#include <vector>
#include <memory>
#include <iostream>

ParseResult<std::shared_ptr<ScopeNode>> Parser::parseScope() {
    std::vector<std::shared_ptr<ASTNode>> nodes;
    while(current().isNot(tok::eof)) {
        if(auto node = parseNode()) {
            nodes.push_back(*node);
            next();
        } else {
            previous();
            break;
        }
    }
    return std::make_shared<ScopeNode>(nodes);
}

ParseResult<std::shared_ptr<ASTNode>> Parser::parseNode() {
    if(current().is(tok::eof)) return Diagnostic("Expected node.");

    // TODO: This line is a hack and should be replaced with a more robust solution to ending ScopeNode parsing.
    // Currently the user can enter an arbitrary number of '}' and nothing will fail
    // lexically.
    if(current().is(tok::sep_right_curly)) return Diagnostic("Exiting scope hack");
    lexer.saveState();
    if(auto prototype = parseDeclPrototype()) {
        next();
        // There's no surefire way to tell what the prototype is until we see what comes
        // after it.
        lexer.saveState();
        if(auto decl = parseDecl(*prototype)) {
            return std::dynamic_pointer_cast<ASTNode>(std::make_shared<Decl>(*decl));
        }
        lexer.rollbackState();
    }
    lexer.rollbackState();
    lexer.saveState();
    /*if(auto expr = parseDeclRefExpr()) {
     previous();
     return std::dynamic_pointer_cast<ASTNode>(*expr);
     }*/
    return Diagnostic("Failed to parse a node.");
}

ParseResult<DeclPrototype> Parser::parseDeclPrototype() {
    if(current().is(tok::eof)) return Diagnostic("Expected declaration prototype");
    auto isMut = false;
    if(current().is(tok::kw_mut)) {
        isMut = true;
        if(next().is(tok::eof)) return Diagnostic("Expected declaration prototype");
    }
    auto name = parseIdentifer();
    assert(name && "Expected declaration name.");
    llvm::SmallVector<llvm::SmallVector<Param, 2>, 1> paramLists;
    while(next().is(tok::sep_left_paren)) {
        llvm::SmallVector<Param, 2> parameters;
        do {
            next();
            auto param = parseIdentifer();
            assert(param && "Expected parameter name.");
            assert(next().is(tok::sep_colon) && "Expected ':' after parameter name.");
            next();
            auto argument = parseExpr();
            assert(argument && "Expected argument value.");
            parameters.push_back(Param(*param, *argument));
        } while(next().is(tok::sep_comma));
        paramLists.push_back(parameters);
    }
    assert(previous());

    return DeclPrototype(*name, paramLists, isMut);
}

ParseResult<Decl> Parser::parseDecl(DeclPrototype prototype) {
    if(current().isNot(tok::sep_colon)) return Diagnostic("Expected declaration");
    next();
    if(current().is(tok::eof)) return Diagnostic("Expected type expression after declaration header");
    auto type = parseExpr();
    assert(type && "Expected type expression after declaration header");
    next();
    if(current().is(tok::sep_equal)) {
        next();
        auto expression = parseExpr();
        assert(expression && "Expected expression after assignment");
        return Decl(prototype, *type, *expression);
    } else if(current().is(tok::sep_left_curly)) {
        next();
        auto body = parseScope();
        assert(next().is(tok::sep_right_curly) && "Expected '}' at end of declaration");
        return Decl(prototype, *type, *body);
    }
    assert(false && "Expected '=' or '{' after type annotation");
}

ParseResult<std::shared_ptr<Expr>> Parser::parseDeclRefExpr(DeclPrototype prototype) {
    //return std::make_unique<DeclRefExpr>(DeclRefExpr(prototype));
}

ParseResult<std::shared_ptr<Expr>> Parser::parseExpr() {
    if(auto intVal = parseIntegerLiteral()) {
        return std::dynamic_pointer_cast<Expr>(std::make_shared<IntegerLiteralExpr>(*intVal));
    } else if(auto decimalVal = parseDecimalLiteral()) {
        return std::dynamic_pointer_cast<Expr>(std::make_shared<DecimalLiteralExpr>(*decimalVal));
    } /*else if(auto prototype = parseDeclPrototype()) {
       return parseDeclRefExpr(*prototype);
       }*/

    return Diagnostic("Failed to parse expression.");
}

