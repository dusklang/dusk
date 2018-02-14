//  Copyright © 2018 Zach Wolfe. All rights reserved.

#ifndef Parser_hpp
#define Parser_hpp

#include "llvm/ADT/Optional.h"

#include "Lexer.hpp"
#include "AST/Decl.hpp"
#include "ParseResult.hpp"
#include "General/Diagnostic.hpp"

class Parser {
private:
    Lexer lexer;

    llvm::Optional<Token> current() { return lexer.getCurrentToken(); }
    llvm::Optional<Token> next() { return lexer.getNextToken(); }
    llvm::Optional<Token> previous() { return lexer.getPreviousToken(); }
    llvm::Optional<std::string> parseIdentifer() {
        if(current() && current()->is(tok::identifier))
            return current()->getText();
        return llvm::None;
    }
    llvm::Optional<std::string> parseIntegerLiteral() {
        if(current() && current()->is(tok::integer_literal))
            return current()->getText();
        return llvm::None;
    }
    llvm::Optional<std::string> parseDecimalLiteral() {
        if(current() && current()->is(tok::decimal_literal))
            return current()->getText();
        return llvm::None;
    }
public:
    Parser(const std::string& source) : lexer(source) {}

    ParseResult<std::shared_ptr<ScopeNode>>  parseScope();
    ParseResult<std::shared_ptr<ASTNode>> parseNode();
    ParseResult<DeclPrototype> parseDeclPrototype();
    ParseResult<Decl> parseDecl(DeclPrototype prototype);
    ParseResult<std::shared_ptr<Expr>> parseDeclRefExpr(DeclPrototype prototype);
    ParseResult<std::shared_ptr<Expr>> parseExpr();
    Lexer* getLexer() { return &lexer; }
};

#endif /* Parser_hpp */
