//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include "AST/AST.hpp"
#include "Lexer.hpp"

template<typename Success>
class ParseResult {
public:
    enum Tag { succeeded, failed };
private:
    Tag _tag;
    union {
        Success _success;
        std::string _failureMessage;
    };
public:
    ~ParseResult() {}
    ParseResult(Success success) : _tag(Tag::succeeded), _success(success) {}
    ParseResult(const std::string& failureMessage) : _tag(Tag::failed), _failureMessage(failureMessage) {}

    Tag tag() const { return _tag; }
    const Success& success() const {
        assert(_tag == succeeded);
        return _success;
    }
    const std::string& failureMessage() const {
        assert(_tag == failed);
        return _failureMessage;
    }
    operator bool() const { return _tag == succeeded; }
    Success& operator*() { return _success; }
    Success* operator->() { return &_success; }
};

#include "llvm/ADT/Optional.h"

#include "Lexer.hpp"
#include "AST/Decl.hpp"

typedef std::string Diagnostic;

class Parser {
private:
    Lexer lexer;

    Token current() { return lexer.curTok(); }
    Token next() { return lexer.nextTok(); }
    llvm::Optional<Token> previous() { return lexer.prevTok(); }
    llvm::Optional<std::string> parseIdentifer() {
        if(current().is(tok::identifier))
            return current().getText();
        return llvm::None;
    }
    llvm::Optional<std::string> parseIntegerLiteral() {
        if(current().is(tok::integer_literal))
            return current().getText();
        return llvm::None;
    }
    llvm::Optional<std::string> parseDecimalLiteral() {
        if(current().is(tok::decimal_literal))
            return current().getText();
        return llvm::None;
    }
public:
    Parser(const std::string& source) : lexer(source) {
        lexer.nextTokIncludingInsignificant();
    }

    ParseResult<std::shared_ptr<ScopeNode>>  parseScope();
    ParseResult<std::shared_ptr<ASTNode>> parseNode();
    ParseResult<DeclPrototype> parseDeclPrototype();
    ParseResult<Decl> parseDecl(DeclPrototype prototype);
    ParseResult<std::shared_ptr<Expr>> parseDeclRefExpr(DeclPrototype prototype);
    ParseResult<std::shared_ptr<Expr>> parseExpr();
};
