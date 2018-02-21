//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include "AST/AST.hpp"
#include "Lexer.hpp"

typedef std::string Diagnostic;

template<typename Success, typename Failure = Diagnostic>
class ParseResult {
public:
private:
    enum { succeeded, failed }
    _tag;
    union {
        Success _success;
        Failure _failure;
    };
public:
    ~ParseResult() {}
    ParseResult(const Success& success) : _tag(succeeded), _success(success) {}
    ParseResult(const Failure& failure) : _tag(failed), _failure(failure) {}
    ParseResult(const ParseResult<Success, Failure>& other) : _tag(other._tag) {
        if(_tag == succeeded) _success = other._success;
        else _failure = other._failure;
    }

    const Success& success() const {
        assert(_tag == succeeded);
        return _success;
    }
    const Failure& failure() const {
        assert(_tag == failed);
        return _failure;
    }
    operator bool() const { return _tag == succeeded; }
    Success& operator*() { return _success; }
    Success* operator->() { return &_success; }
};

class NodeParsingFailure final {
private:
    enum Type {
        eof,
        eos,
        diagnostic
    } type;
    Diagnostic _diag;
public:
    ~NodeParsingFailure() {}
    NodeParsingFailure(Type type) : type(type) {}
    NodeParsingFailure(const Diagnostic& diag) : type(diagnostic), _diag(diag) {}
    static NodeParsingFailure EndOfFile() { return eof; }
    static NodeParsingFailure EndOfScope() { return eos; }

    const Diagnostic& diag() const {
        assert(type == diagnostic);
        return _diag;
    }

    bool isEOF() const { return type == eof; }
    bool isEOS() const { return type == eos; }
    bool isDiag() const { return type == diagnostic; }
};

#include "llvm/ADT/Optional.h"

#include "Lexer.hpp"
#include "AST/Decl.hpp"

class Parser {
private:
    Lexer lexer;

    Token current() { return lexer.curTok(); }
    Token next() { return lexer.nextTok(); }
    llvm::Optional<Token> previous() { return lexer.prevTok(); }
    llvm::Optional<std::string> parseIdentifer() {
        if(current().is(tok::identifier)) {
            auto text = current().getText();
            next();
            return text;
        }
        return llvm::None;
    }
    llvm::Optional<std::string> parseIntegerLiteral() {
        if(current().is(tok::integer_literal)) {
            auto text = current().getText();
            next();
            return text;
        }
        return llvm::None;
    }
    llvm::Optional<std::string> parseDecimalLiteral() {
        if(current().is(tok::decimal_literal)) {
            auto text = current().getText();
            next();
            return text;
        }
        return llvm::None;
    }
public:
    Parser(const std::string& source) : lexer(source) {
        lexer.nextTokIncludingInsignificant();
    }

    ParseResult<std::vector<std::shared_ptr<ASTNode>>> parseTopLevel();
    ParseResult<std::shared_ptr<Scope>> parseScope();
    ParseResult<std::shared_ptr<ASTNode>, NodeParsingFailure> parseNode();
    ParseResult<DeclPrototypeORRef> parseDeclPrototypeORRef();
    ParseResult<DeclPrototype> parseDeclPrototype(DeclPrototypeORRef protoOrRef);
    ParseResult<Decl> parseDecl(DeclPrototype prototype);
    ParseResult<std::shared_ptr<Expr>> parseDeclRefExpr(DeclPrototypeORRef protoOrRef);
    ParseResult<std::shared_ptr<Expr>> parseExpr();
};
