//  Created by Zach Wolfe on 2018-02-11.

#ifndef Lexer_hpp
#define Lexer_hpp

#include <string>
#include "llvm/ADT/Optional.h"
#include "Token.hpp"

class Lexer {
private:
    // TODO: I'll eventually want to use something that supports Unicode.
    const std::string source;
    Token currentToken;

    // Position of the next character (not yet lexed).
    std::string::const_iterator nextPosition;

    bool is(char character) const { return *nextPosition == character; }
    bool isSpace() const { return is(' '); }
    bool isNewline() const { return is('\n') || is('\r'); }
    bool isTab() const { return is('\t'); }
    bool isWhitespace() const { return isSpace() || isNewline() || isTab(); }
    bool isLowercaseLetter() const { return 'a' <= *nextPosition && *nextPosition <= 'z'; }
    bool isUppercaseLetter() const { return 'A' <= *nextPosition && *nextPosition <= 'Z'; }
    bool isLetter() const { return isLowercaseLetter() || isUppercaseLetter(); }
    bool isNumber() const { return '0' <= *nextPosition && *nextPosition <= '9'; }
    bool isDot() const { return is('.'); }
    bool is(const std::string& substring) const {
        std::string accumulatedSubstr;
        for(auto iter = nextPosition; iter != source.end(); iter++) {
            accumulatedSubstr += *iter;
            if(accumulatedSubstr.length() > substring.length()) return false;
            if(accumulatedSubstr.length() == substring.length())
                return accumulatedSubstr == substring;
        }
        return true;
    }
public:
    Lexer(const std::string& source) : source(source), nextPosition(source.begin()) {}

    llvm::Optional<Token> nextToken();
};

#endif /* Lexer_hpp */
