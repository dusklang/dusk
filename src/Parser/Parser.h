//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <optional>
#include <stack>

#include "AST/AST.h"
#include "AST/Type.h"
#include "Lexer.h"

struct Expr;
struct Stmt;
struct Decl;

class Parser final {
    std::vector<Token> tokens;
    uint32_t curTok = -1;
    std::optional<uint32_t> savedState = std::nullopt;

    Token cur() {
        return tokens[curTok];
    }
    Token nextIncludingInsignificant() {
        return tokens[++curTok];
    }
    Token next() {
        while(true) {
            auto next = nextIncludingInsignificant();
            if(next.isSignificant())
                return next;
        }
    }
    std::optional<Token> prevIncludingInsignificant() {
        curTok--;
        if(curTok == 0) return std::nullopt;
        return tokens[curTok];
    }
    std::optional<Token> prev() {
        while(auto prev = prevIncludingInsignificant()) {
            if(prev->isSignificant()) return prev;
        }
        return std::nullopt;
    }
    void saveState() {
        savedState = curTok;
    }
    void recallState() {
        if(savedState) {
            curTok = *savedState;
        }
        savedState = std::nullopt;
    }

    std::optional<std::string> parseIdentifer() {
        if(cur().is(tok::identifier)) {
            auto text = cur().getText();
            next();
            return text;
        }
        return std::nullopt;
    }
    std::optional<std::string> parseIntegerLiteral() {
        auto lit = cur().getIntegerLiteral();
        if(lit) next();
        return lit;
    }
    std::optional<std::string> parseDecimalLiteral() {
        auto lit = cur().getDecimalLiteral();
        if(lit) next();
        return lit;
    }
    std::optional<char> parseCharLiteral() {
        auto lit = cur().getCharLiteral();
        if(lit) next();
        return lit;
    }
    std::optional<std::string> parseStringLiteral() {
        auto lit = cur().getStringLiteral();
        if(lit) next();
        return lit;
    }
    // TODO: Replace all this source location state tracking with manual state tracking.
    std::stack<SourceLoc> currentLoc;
    void recordCurrentLoc() {
        currentLoc.push(cur().getLoc());
    }
    SourceRange currentRange() {
        auto beginLoc = currentLoc.top();
        currentLoc.pop();
        // This is a hack because everywhere this function is used, we're already at the first token
        // *after* the last token of the range we're interested in.
        auto currentLoc = prev()->getRange();
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
    Parser(std::string const& source) {
        tokens = lex(source);
        next();
    }

    std::vector<ASTNode*> parseTopLevel();
    Scope* parseScope();
    ASTNode* parseNode();
    Type parseType();
    Decl* parseDecl();
    StructDecl* parseStructDecl();
    Stmt* parseStmt();
    Stmt* parseIfStmt();
    Stmt* parseWhileStmt();
    Expr* parseDeclRefExpr();
    Expr* parseExpr();
    Expr* parseTerm();
};
