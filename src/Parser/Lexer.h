//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <stack>
#include <map>
#include <optional>
#include <iostream>

#include "Token.h"

class Lexer final {
private:
    // TODO: I'll eventually want to replace std::string with something that supports Unicode.
    std::string const source;
    // These two stacks aren't parallel: tokenPositions always has the next position (not yet lexed)
    // on top while tokens always has the most-recently lexed token on top. In other words,
    // tokenPositions should always have exactly one more element than tokens.
    std::stack<Token> tokens;
    std::stack<int> tokenPositions;

    // This counter stores how many new tokens have been added since last calling saveState().
    // This is useful because we may need to rollback the lexer after realizing we can't actually
    // parse the thing we were trying to and should try something else.
    std::optional<int> numberOfNewTokens;

    std::map<char, char> specialEscapeCharacters {
        { 'n', '\n' },
        { '"', '"' },
        { '0', '\0' },
        { '\\', '\\' }
    };

    void reportError(int endPos, std::string message) {
        auto startPos = tokenPositions.top();
        SourceRange range(SourceLoc(&source, startPos), endPos - startPos);
        std::cout << "LEXING ERROR: " << message << '\n';
        std::cout << "Offending area: " << range.getSubstring() << "\n\n";
        exit(1);
    }
public:
    Lexer(std::string const& source) : source(source) {
        tokenPositions.push(0);
    }

    Token curTok() {
        auto cur = tokens.top();
        if(cur.isInsignificant()) return nextTok();
        return cur;
    }
    Token nextTokIncludingInsignificant();
    std::optional<Token> prevTokIncludingInsignificant() {
        tokens.pop();
        tokenPositions.pop();
        if(numberOfNewTokens) *numberOfNewTokens -= 1;
        if(tokens.empty()) return std::nullopt;
        return tokens.top();
    }

    Token nextTok() {
        while(true) {
            auto next = nextTokIncludingInsignificant();
            if(next.isSignificant())
                return next;
        }
    }
    std::optional<Token> prevTok() {
        while(auto prev = prevTokIncludingInsignificant()) {
            if(prev->isSignificant()) return prev;
        }
        return std::nullopt;
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

        numberOfNewTokens = std::nullopt;
    }

    std::string const& getSource() const { return source; }
};
