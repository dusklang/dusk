//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "Parser.hpp"
#include "AST/Expr.hpp"
#include <vector>
#include <memory>
#include <iostream>

#define EXPECT(token, message) if(current().isNot(token)) return Diagnostic(message)
#define EXPECT_NEXT(token, message) {\
    next();\
    EXPECT(token, message);\
}

#define TRY(result) [&]() {\
    lexer.saveState();\
    auto val = result;\
    if(!val) lexer.rollbackState();\
    return val;\
}()

ParseResult<std::vector<std::shared_ptr<ASTNode>>> Parser::parseTopLevel() {
    std::vector<std::shared_ptr<ASTNode>> nodes;
    while(true) {
        auto node = parseNode();
        if(!node) {
            if(node.failure().isEOF()) break;
            if(node.failure().isEOS()) { return Diagnostic("Extraneous closing brace '}'"); }
            return node.failure().diag();
        }
        nodes.push_back(*node);
    }
    return nodes;
}

ParseResult<std::shared_ptr<Scope>> Parser::parseScope() {
    std::vector<std::shared_ptr<ASTNode>> nodes;
    while(true) {
        auto node = parseNode();
        if(!node) {
            if(node.failure().isEOS()) break;
            if(node.failure().isEOF()) return Diagnostic("Unexpected EOF before end of scope");
            return node.failure().diag();
        }
        if(!*node) break;
        nodes.push_back(*node);
    }
    return std::make_shared<Scope>(nodes);
}

ParseResult<std::shared_ptr<ASTNode>, NodeParsingFailure> Parser::parseNode() {
    if(current().is(tok::eof)) return NodeParsingFailure::EndOfFile();
    if(current().is(tok::sep_right_curly)) {
        next();
        return NodeParsingFailure::EndOfScope();
    }
    if(auto prototype = TRY(parseDeclPrototype())) {
        auto decl = TRY(parseDecl(*prototype));
        if(decl) {
            return std::dynamic_pointer_cast<ASTNode>(std::make_shared<Decl>(*decl));
        }

        if(auto declRef = TRY(parseDeclRefExpr(*prototype))) {
            return std::dynamic_pointer_cast<ASTNode>(*declRef);
        }
    } else {
        if(auto expr = TRY(parseExpr())) {
            return std::dynamic_pointer_cast<ASTNode>(*expr);
        } else {
            return NodeParsingFailure(prototype.failure());
        }
    }
    return NodeParsingFailure("Failed to parse node");
}

ParseResult<DeclPrototype> Parser::parseDeclPrototype() {
    bool isMut = false;
    if(current().is(tok::kw_mut)) {
        isMut = true;
        next();
    }
    EXPECT(tok::identifier, "Expected identifier to begin declaration prototype");
    auto name = current().getText();
    std::vector<Param> paramList;
    if(next().is(tok::sep_left_paren)) {
        do {
            EXPECT_NEXT(tok::identifier, "Expected parameter name");
            auto param = current().getText();
            EXPECT_NEXT(tok::sep_colon, "Expected colon after parameter name");
            next();
            auto argument = TRY(parseExpr());
            if(!argument) return Diagnostic("Expected argument after parameter name");
            paramList.push_back(Param(param, *argument));
        } while(current().is(tok::sep_comma));
        EXPECT(tok::sep_right_paren, "Expected ')' after parameter and argument");
        if(paramList.empty()) return Diagnostic("Expected parameter list for function " + name + ", "
                                                "try removing the parentheses");
        next();
    }
    return DeclPrototype(name, paramList, isMut);
}

ParseResult<Decl> Parser::parseDecl(DeclPrototype prototype) {
    auto type = std::make_shared<TypeRefExpr>();
    if(current().is(tok::sep_colon)) {
        next();
        // If we encounter an equal sign, than we infer the type, which is implied by the
        // default constructor for TypeExpr, so no extra work required.
        //
        // Otherwise:
        if(current().isNot(tok::sep_equal)) {
            auto ty = TRY(parseExpr());
            if(!ty) return Diagnostic("Expected type for declaration");

            type = std::make_shared<TypeRefExpr>(*std::dynamic_pointer_cast<DeclRefExpr>(*ty));
        }
        if(current().is(tok::sep_equal)) {
            next();
            auto val = TRY(parseExpr());
            if(!val) return Diagnostic("Expected expression following assignment operator");
            return Decl(prototype, *val, std::dynamic_pointer_cast<Expr>(type));
        } else if(current().isNot(tok::sep_left_curly)) {
            return Diagnostic("Expected assignment operator or opening '{'.");
        }
    } else if(current().isNot(tok::sep_left_curly)) {
        return Diagnostic("Expected ':' and type expression, or opening '{' to begin computed declaration");
    }
    next();
    auto scope = TRY(parseScope());
    if(!scope) { return scope.failure(); }
    return Decl(prototype, *scope, std::dynamic_pointer_cast<Expr>(type));
}

ParseResult<std::shared_ptr<Expr>> Parser::parseDeclRefExpr(DeclPrototype prototype) {
    if(prototype.isMut) return Diagnostic("Declaration references cannot be mutable");
    std::vector<Argument> argList;
    for(auto& param: prototype.paramList) {
        argList.push_back(Argument(param.name, param.value));
    }
    return std::dynamic_pointer_cast<Expr>(std::make_shared<DeclRefExpr>(prototype.name, argList));

    return Diagnostic("Failed to parse decl ref expression");
}

ParseResult<std::shared_ptr<Expr>> Parser::parseExpr() {
    if(auto intVal = TRY(parseIntegerLiteral())) {
        return std::dynamic_pointer_cast<Expr>(std::make_shared<IntegerLiteralExpr>(*intVal));
    } else if(auto decimalVal = TRY(parseDecimalLiteral())) {
        return std::dynamic_pointer_cast<Expr>(std::make_shared<DecimalLiteralExpr>(*decimalVal));
    } else if(current().is(tok::identifier)) {
        ParseResult<DeclPrototype> prototype = TRY(parseDeclPrototype());
        if(prototype) { return parseDeclRefExpr(*prototype); }
        return prototype.failure();
    }

    return Diagnostic("Failed to parse expression.");
}
