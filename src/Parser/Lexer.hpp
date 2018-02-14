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

    const char& nextChar(int pos) const { return source.at(pos); }
    bool is(char character, int pos) const { return nextChar(pos) == character; }
    bool isSpace(int pos) const { return is(' ', pos); }
    bool isNewline(int pos) const { return is('\n', pos) || is('\r', pos); }
    bool isTab(int pos) const { return is('\t', pos); }
    bool isWhitespace(int pos) const { return isSpace(pos) || isNewline(pos) || isTab(pos); }
    bool isLowercaseLetter(int pos) const { return 'a' <= nextChar(pos) && nextChar(pos) <= 'z'; }
    bool isUppercaseLetter(int pos) const { return 'A' <= nextChar(pos) && nextChar(pos) <= 'Z'; }
    bool isLetter(int pos) const { return isLowercaseLetter(pos) || isUppercaseLetter(pos); }
    bool isNumber(int pos) const { return '0' <= nextChar(pos) && nextChar(pos) <= '9'; }
    bool isDot(int pos) const { return is('.', pos); }
    bool isDoubleQuote(int pos) const { return is('"', pos); }
    bool isSubstr(const std::string& substring, int pos) const {
        auto j = [this](int i) { return i + this->tokenPositions.top(); };
        for(int i = 0; i < substring.length() && j(i) < source.length(); i++) {
            if(substring.at(i) != source.at(j(i))) return false;
        }
        return true;
    }
public:
    Lexer(const std::string& source) : source(source) {
        tokenPositions.push(0);
    }

    llvm::Optional<Token> getNextToken();
    llvm::Optional<Token> getCurrentToken() {
        if(tokens.empty()) return llvm::None;
        return tokens.top();
    }
    llvm::Optional<Token> getPreviousToken() {
        tokens.pop();
        tokenPositions.pop();

        if(tokens.empty())
            return llvm::None;
        return tokens.top();
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

#endif /* Lexer_hpp */
