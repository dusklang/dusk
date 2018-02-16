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
        next();
    }
    return nodes;
}

ParseResult<std::shared_ptr<ScopeNode>> Parser::parseScope() {
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
        next();
    }
    return std::make_shared<ScopeNode>(nodes);
}

ParseResult<std::shared_ptr<ASTNode>, NodeParsingFailure> Parser::parseNode() {
    if(current().is(tok::eof)) return NodeParsingFailure::EndOfFile();
    if(current().is(tok::sep_right_curly)) return NodeParsingFailure::EndOfScope();
    auto prototype = parseDeclPrototype();
    if(!prototype) return NodeParsingFailure(prototype.failure());

    lexer.saveState();

    if(auto decl = parseDecl(*prototype)) {
        return std::dynamic_pointer_cast<ASTNode>(std::make_shared<Decl>(*decl));
    }

    lexer.rollbackState();
    lexer.saveState();

    if(auto declRef = parseDeclRefExpr(*prototype)) {
        return std::dynamic_pointer_cast<ASTNode>(*declRef);
    } else {
        return NodeParsingFailure(declRef.failure());
    }
}

ParseResult<DeclPrototype> Parser::parseDeclPrototype() {
    bool isMut = false;
    if(current().is(tok::kw_mut)) {
        isMut = true;
        next();
    }
    EXPECT(tok::identifier, "Expected identifier to begin declaration prototype");
    auto name = current().getText();
    llvm::SmallVector<llvm::SmallVector<Param, 2>, 1> paramLists;
    while(next().is(tok::sep_left_paren)) {
        llvm::SmallVector<Param, 2> paramList;
        while(next().is(tok::sep_comma)) {
            EXPECT_NEXT(tok::identifier, "Expected parameter name");
            auto param = current().getText();
            EXPECT_NEXT(tok::sep_colon, "Expected colon after parameter name");
            next();
            auto argument = parseExpr();
            if(!argument) return Diagnostic("Expected argument after parameter name");
            paramList.push_back(Param(param, *argument));
        }
        EXPECT(tok::sep_right_paren, "Expected ')' after parameter and argument");
        if(paramList.empty()) return Diagnostic("Expected parameter list for function " + name + ", "
                                                "try removing the parentheses");
        paramLists.push_back(paramList);
    }
    return DeclPrototype(name, paramLists, isMut);
}

ParseResult<Decl> Parser::parseDecl(DeclPrototype prototype) {
    std::shared_ptr<TypeExpr> type;
    if(current().is(tok::sep_colon)) {
        next();
        // If we encounter an equal sign, than we infer the type, which is implied by the
        // default constructor for TypeExpr, so no extra work required. Otherwise:
        if(current().isNot(tok::sep_equal)) {
            next();
            auto typeProto = parseDeclPrototype();
            if(!typeProto) return Diagnostic("Expected type for declaration");
            auto ty = parseDeclRefExpr(*typeProto);
            if(!ty) return Diagnostic("Expected type for declaration");
            type = std::make_unique<TypeExpr>(*std::dynamic_pointer_cast<DeclRefExpr>(*ty));
            next();
        }
        if(current().is(tok::sep_equal)) {
            next();
            auto val = parseExpr();
            if(!val) return Diagnostic("Expected expression following assignment operator");
            return Decl(prototype, *val, std::dynamic_pointer_cast<Expr>(type));
        } else if(current().isNot(tok::sep_left_curly)) {
            return Diagnostic("Expected assignment operator or opening '{'.");
        }
    } else if(current().isNot(tok::sep_left_curly)) {
        return Diagnostic("Expected ':' and type expression, or opening '{' to begin computed declaration");
    }
    next();
    auto scope = parseScope();
    if(!scope) { return scope.failure(); }
    return Decl(prototype, *scope, std::dynamic_pointer_cast<Expr>(type));
}

ParseResult<std::shared_ptr<Expr>> Parser::parseDeclRefExpr(DeclPrototype prototype) {
}

ParseResult<std::shared_ptr<Expr>> Parser::parseExpr() {
    if(auto intVal = parseIntegerLiteral()) {
        return std::dynamic_pointer_cast<Expr>(std::make_shared<IntegerLiteralExpr>(*intVal));
    } else if(auto decimalVal = parseDecimalLiteral()) {
        return std::dynamic_pointer_cast<Expr>(std::make_shared<DecimalLiteralExpr>(*decimalVal));
    } else if(current().is(tok::identifier)) {
        ParseResult<DeclPrototype> prototype = parseDeclPrototype();
        if(prototype) { return parseDeclRefExpr(*prototype); }
        return prototype.failure();
    }

    assert(false);
    return Diagnostic("unreachable");
}

