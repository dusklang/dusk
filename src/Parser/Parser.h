//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <optional>

#include "AST/AST.h"
#include "AST/Stmt.h"
#include "AST/Decl.h"

#include "Lexer.h"

class Parser {
private:
    Lexer lexer;

    Token current() { return lexer.curTok(); }
    Token next() { return lexer.nextTok(); }
    std::optional<Token> previous() { return lexer.prevTok(); }
    std::optional<std::string> parseIdentifer() {
        if(current().is(tok::identifier)) {
            auto text = current().getText();
            next();
            return text;
        }
        return std::nullopt;
    }
    std::optional<std::string> parseIntegerLiteral() {
        auto lit = current().getIntegerLiteral();
        if(lit) next();
        return lit;
    }
    std::optional<std::string> parseDecimalLiteral() {
        auto lit = current().getDecimalLiteral();
        if(lit) next();
        return lit;
    }
    std::optional<char> parseCharLiteral() {
        auto lit = current().getCharLiteral();
        if(lit) next();
        return lit;
    }
    std::optional<std::string> parseStringLiteral() {
        auto lit = current().getStringLiteral();
        if(lit) next();
        return lit;
    }
    // TODO: Replace all this source location state tracking with manual state tracking.
    std::stack<SourceLoc> currentLoc;
    void recordCurrentLoc() {
        currentLoc.push(current().getLoc());
    }
    SourceRange currentRange() {
        auto beginLoc = currentLoc.top();
        currentLoc.pop();
        // This is a hack because everywhere this function is used, we're already at the first token
        // *after* the last token of the range we're interested in.
        auto currentLoc = previous()->getRange();
        next();
        return rangeFrom(beginLoc, currentLoc);
    }
    SourceRange rangeFrom(SourceLoc beginLoc, SourceRange endRange) {
        auto diff = endRange.begin.location - beginLoc.location;
        auto length = endRange.length + diff;
        //assert(diff >= 0 && "Attempt to get range between a location and a range that occurs after it");
        return SourceRange(beginLoc, length);
    }
    void reportError(std::string message, SourceRange offendingRange) {
        std::cout << "PARSING ERROR: " << message << '\n';
        std::cout << "Offending area: " << offendingRange.getSubstring() << "\n\n";
        exit(1);
    }
    void reportError(std::string message) {
        reportError(message, currentRange());
    }
public:
    Parser(std::string const& source) : lexer(source) {
        lexer.nextTokIncludingInsignificant();
    }

    std::vector<std::shared_ptr<ASTNode>> parseTopLevel();
    std::optional<std::shared_ptr<Scope>> parseScope();
    std::shared_ptr<ASTNode> parseNode();
    Type parseType();
    std::optional<Decl> parseDecl();
    std::optional<std::shared_ptr<Stmt>> parseStmt();
    std::optional<std::shared_ptr<Stmt>> parseIfStmt();
    std::optional<std::shared_ptr<Stmt>> parseWhileStmt();
    std::optional<std::shared_ptr<Expr>> parseDeclRefExpr();
    std::optional<std::shared_ptr<Expr>> parseExpr();
};
