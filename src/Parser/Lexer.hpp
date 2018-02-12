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

    bool isSpace() const { return *nextPosition == ' '; }
    bool isNewline() const { return *nextPosition == '\n' || *nextPosition == '\r'; }
    bool isTab() const { return *nextPosition == '\t'; }
    bool isWhitespace() const { return isSpace() || isNewline() || isTab(); }
    bool isLowercaseLetter() const { return 'a' <= *nextPosition && *nextPosition <= 'z'; }
    bool isUppercaseLetter() const { return 'A' <= *nextPosition && *nextPosition <= 'Z'; }
    bool isLetter() const { return isLowercaseLetter() || isUppercaseLetter(); }
    //bool isSubstring(const std::string& substring) const;
public:
    Lexer(const std::string& source) : source(source), nextPosition(source.begin()) {}

    llvm::Optional<Token> nextToken();
};

#endif /* Lexer_hpp */
