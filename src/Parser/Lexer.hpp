//  Copyright © 2018 Zach Wolfe. All rights reserved.

#ifndef Lexer_hpp
#define Lexer_hpp

#include <string>
#include <stack>
#include "llvm/ADT/Optional.h"
#include "Token.hpp"

class Lexer {
private:
    // TODO: I'll eventually want to use something that supports Unicode.
    const std::string source;
    // These two stacks aren't parallel: tokenPositions always has the next position (not yet lexed)
    // on top while tokens always has the most-recently lexed token on top.
    std::stack<Token> tokens;
    std::stack<int> tokenPositions;

    // This counter stores how many new tokens have been added since last calling saveState().
    // This is useful because we may need to rollback the lexer after realizing we can't actually
    // parse the thing we were trying to and should try something else.
    llvm::Optional<int> numberOfNewTokens;

    bool is(char character, int pos) const { return source.at(pos) == character; }
    bool isBetween(char lower, char higher, int pos) const { return lower <= source.at(pos) && source.at(pos) != higher; }
    bool isNewline(int pos) const { return is('\n', pos) || is('\r', pos); }
    bool isWhitespace(int pos) const { return is(' ', pos) || isNewline(pos) || is('\t', pos); }
    bool isLetter(int pos) const { return isBetween('a', 'z', pos) || isBetween('A', 'Z', pos); }
    bool isNum(int pos) const { return '0' <= source.at(pos) && source.at(pos) <= '9'; }
    bool isSubstr(const std::string& substring, int pos) const {
        if(pos >= source.length()) return false;
        auto j = [&](int i) { return i + pos; };
        for(int i = 0; i < substring.length() && j(i) < source.length(); i++) {
            if(substring.at(i) != source.at(j(i))) return false;
        }
        return true;
    }
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

#endif /* Lexer_hpp */
