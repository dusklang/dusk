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
    if(auto stmt = TRY(parseStmt())) {
        return std::dynamic_pointer_cast<ASTNode>(*stmt);
    }
    if(auto protoOrRef = TRY(parseDeclPrototypeORRef())) {
        if(auto proto = TRY(parseDeclPrototype(*protoOrRef))) {
            auto decl = TRY(parseDecl(*proto));
            if(decl) {
                return std::dynamic_pointer_cast<ASTNode>(std::make_shared<Decl>(*decl));
            } else {
                return std::dynamic_pointer_cast<ASTNode>(std::make_shared<DeclPrototype>(*proto));
            }
        }

        if(auto declRef = TRY(parseDeclRefExpr(*protoOrRef))) {
            return std::dynamic_pointer_cast<ASTNode>(*declRef);
        }
    } else {
        if(auto expr = TRY(parseExpr())) {
            return std::dynamic_pointer_cast<ASTNode>(*expr);
        } else {
            return NodeParsingFailure(protoOrRef.failure());
        }
    }
    return NodeParsingFailure("Failed to parse node");
}

ParseResult<DeclPrototypeORRef> Parser::parseDeclPrototypeORRef() {
    bool isMut = false;
    bool isExtern = false;
    if(current().is(tok::kw_mut)) {
        isMut = true;
        next();
    }
    if(current().is(tok::kw_extern)) {
        isExtern =  true;
        next();
    }
    if(current().is(tok::kw_mut) && !isMut) {
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

    std::shared_ptr<PlaceholderTypeRefExpr> type;
    if(current().is(tok::sep_colon)) {
        next();
        // If we encounter an equal sign or opening curly-brace, then we will infer the type, so no
        // extra work is required.
        if(current().isAny(tok::sep_equal, tok::sep_left_curly)) {
            type = std::make_shared<PlaceholderTypeRefExpr>();
            return DeclPrototypeORRef(name, paramList, std::dynamic_pointer_cast<Expr>(type), isMut, isExtern, DeclPrototypeORRef::prototype);
        } else {
            auto ty = TRY(parseExpr());
            if(!ty) return Diagnostic("Expected type for declaration");

            type = std::make_shared<PlaceholderTypeRefExpr>(*std::dynamic_pointer_cast<DeclRefExpr>(*ty));
            return DeclPrototypeORRef(name, paramList, std::dynamic_pointer_cast<Expr>(type), isMut, isExtern, DeclPrototypeORRef::prototype);
        }
    } else {
        return DeclPrototypeORRef(name, paramList, type, isMut, isExtern, DeclPrototypeORRef::ref);
    }
}

ParseResult<DeclPrototype> Parser::parseDeclPrototype(DeclPrototypeORRef protoOrRef) {
    if(!protoOrRef.type) return Diagnostic("Expected type for declaration prototype");
    return DeclPrototype(protoOrRef.name, protoOrRef.paramList, protoOrRef.type, protoOrRef.isMut, protoOrRef.isExtern);
}

ParseResult<Decl> Parser::parseDecl(DeclPrototype prototype) {
    if(current().is(tok::sep_equal)) {
        next();
        auto val = TRY(parseExpr());
        if(!val) return Diagnostic("Expected expression following assignment operator");
        return Decl(prototype, *val);
    } else if(current().is(tok::sep_left_curly)) {
        next();
        auto scope = TRY(parseScope());
        if(!scope) { return scope.failure(); }
        return Decl(prototype, *scope);
    }
    return Diagnostic("Failed to parse declaration");
}

ParseResult<std::shared_ptr<Stmt>> Parser::parseStmt() {
    if(current().is(tok::kw_return)) {
        next();
        auto value = parseExpr();
        if(!value) return std::dynamic_pointer_cast<Stmt>(std::make_shared<ReturnStmt>(nullptr));
        return std::dynamic_pointer_cast<Stmt>(std::make_shared<ReturnStmt>(*value));
    } else {
        return Diagnostic("Unable to parse statement");
    }
}

ParseResult<std::shared_ptr<Expr>> Parser::parseDeclRefExpr(DeclPrototypeORRef protoOrRef) {
    if(protoOrRef.isMut && protoOrRef.isExtern) return Diagnostic("Expected ':' after declaration");
    assert(!protoOrRef.type && "ProtoOrRef passed to parseDeclRefExpr should not have a type.");
    std::vector<Argument> argList;
    for(auto& param: protoOrRef.paramList) {
        argList.push_back(Argument(param.name, param.value));
    }
    return std::dynamic_pointer_cast<Expr>(std::make_shared<DeclRefExpr>(protoOrRef.name, argList));

    return Diagnostic("Failed to parse decl ref expression");
}

ParseResult<std::shared_ptr<Expr>> Parser::parseExpr() {
    if(auto intVal = TRY(parseIntegerLiteral())) {
        return std::dynamic_pointer_cast<Expr>(std::make_shared<IntegerLiteralExpr>(*intVal));
    } else if(auto decimalVal = TRY(parseDecimalLiteral())) {
        return std::dynamic_pointer_cast<Expr>(std::make_shared<DecimalLiteralExpr>(*decimalVal));
    } else if(current().is(tok::identifier)) {
        auto protoOrRef = TRY(parseDeclPrototypeORRef());
        if(protoOrRef) { return parseDeclRefExpr(*protoOrRef); }
        return protoOrRef.failure();
    }

    return Diagnostic("Failed to parse expression.");
}
