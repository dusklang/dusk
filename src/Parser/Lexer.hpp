//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <stack>
#include "llvm/ADT/Optional.h"
#include "Token.hpp"
#include <iostream>

class Lexer final {
private:
    // TODO: I'll eventually want to replace std::string with something that supports Unicode.
    const std::string source;
    // These two stacks aren't parallel: tokenPositions always has the next position (not yet lexed)
    // on top while tokens always has the most-recently lexed token on top. In other words,
    // tokenPositions should always have exactly one more element than tokens.
    std::stack<Token> tokens;
    std::stack<int> tokenPositions;

    // This counter stores how many new tokens have been added since last calling saveState().
    // This is useful because we may need to rollback the lexer after realizing we can't actually
    // parse the thing we were trying to and should try something else.
    llvm::Optional<int> numberOfNewTokens;

    void reportError(int endPos, std::string message) {
        auto startPos = tokenPositions.top();
        SourceRange range(startPos, endPos - startPos);
        std::cout << "LEXING ERROR: " << message << '\n';
        std::cout << "Offending area: " << substringAtSourceRange(source, range) << '\n';
        exit(1);
    }
public:
    Lexer(const std::string& source) : source(source) {
        tokenPositions.push(0);
    }

    Token curTok() {
        auto cur = tokens.top();
        if(cur.isInsignificant()) return nextTok();
        return cur;
    }
    Token nextTokIncludingInsignificant();
    llvm::Optional<Token> prevTokIncludingInsignificant() {
        tokens.pop();
        tokenPositions.pop();
        if(numberOfNewTokens) *numberOfNewTokens -= 1;
        if(tokens.empty()) return llvm::None;
        return curTok();
    }

    Token nextTok() {
        while(true) {
            auto next = nextTokIncludingInsignificant();
            if(next.isSignificant())
                return next;
        }
    }
    llvm::Optional<Token> prevTok() {
        while(auto prev = prevTokIncludingInsignificant()) {
            if(prev->isSignificant()) return prev;
        }
        return llvm::None;
    }

    // See the above description of the numberOfNewTokens member.
    void saveState() { numberOfNewTokens = 0; }
    void rollbackState() {
        // TODO: Maybe it's better to just fail here, not silently return?
        if(!numberOfNewTokens) return;

        for(int i = 0; i < *numberOfNewTokens; i++) {
            tokens.pop();
            tokenPositions.pop();
        }

        numberOfNewTokens = llvm::None;
    }
};
