//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <stack>
#include "llvm/ADT/Optional.h"
#include "Token.hpp"

class Lexer {
private:
    // TODO: I'll eventually want to replace std::string with something that supports Unicode.
    const std::string source;
    // These two stacks aren't parallel: tokenPositions always has the next position (not yet lexed)
    // on top while tokens always has the most-recently lexed token on top. In other words,
    // tokenPositions should always have exactly one more element than tokens.
    std::stack<Token> tokens;
    std::stack<int> tokenPositions;
public:
    Lexer(const std::string& source) : source(source) {
        tokenPositions.push(0);
    }

    Token nextTok();
    Token curTok() { return tokens.top(); }
    llvm::Optional<Token> prevTok() {
        tokens.pop();
        tokenPositions.pop();
        if(tokens.empty()) return llvm::None;
        return curTok();
    }
};
