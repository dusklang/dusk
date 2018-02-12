//  Copyright © 2018 Zach Wolfe. All rights reserved.

#ifndef Lexer_hpp
#define Lexer_hpp

#include <string>
#include "llvm/ADT/Optional.h"
#include "Token.hpp"

class Lexer {
private:
    // TODO: I'll eventually want to use something that supports Unicode.
    const std::string source;
    llvm::Optional<Token> currentToken;
    llvm::Optional<Token> previousToken;

    // Position of the next character (not yet lexed).
    int nextPosition = 0;
    llvm::Optional<int> currentPosition;

    const char& nextChar() const { return source.at(nextPosition); }
    bool is(char character) const { return nextChar() == character; }
    bool isSpace() const { return is(' '); }
    bool isNewline() const { return is('\n') || is('\r'); }
    bool isTab() const { return is('\t'); }
    bool isWhitespace() const { return isSpace() || isNewline() || isTab(); }
    bool isLowercaseLetter() const { return 'a' <= nextChar() && nextChar() <= 'z'; }
    bool isUppercaseLetter() const { return 'A' <= nextChar() && nextChar() <= 'Z'; }
    bool isLetter() const { return isLowercaseLetter() || isUppercaseLetter(); }
    bool isNumber() const { return '0' <= nextChar() && nextChar() <= '9'; }
    bool isDot() const { return is('.'); }
    bool isDoubleQuote() const { return is('"'); }
    bool isSubstr(const std::string& substring) const {
        auto j = [this](int i) { return i + this->nextPosition; };
        for(int i = 0; i < substring.length() && j(i) < source.length(); i++) {
            if(substring.at(i) != source.at(j(i))) return false;
        }
        return true;
    }
public:
    Lexer(const std::string& source) : source(source) {}

    llvm::Optional<Token> getNextToken();
    llvm::Optional<Token> getCurrentToken() const { return currentToken; }
    llvm::Optional<Token> getPreviousToken() {
        if(!currentPosition)
            return llvm::None;
        currentToken = previousToken;
        previousToken = llvm::None;

        nextPosition = *currentPosition;
        currentPosition = llvm::None;

        return currentToken;
    }
};

#endif /* Lexer_hpp */
