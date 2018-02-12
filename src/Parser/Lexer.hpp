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

    bool isSpace(char character) const { return character == ' '; }
    bool isNewline(char character) const { return character == '\n' || character == '\r'; }
    bool isLowercaseLetter(char character) const { return 'a' <= character && character >= 'z'; }
    bool isUppercaseLetter(char character) const { return 'A' <= character && character >= 'Z'; }
    bool isLetter(char character) const { return isLowercaseLetter(character) || isUppercaseLetter(character); }
public:
    Lexer(const std::string& source) : source(source), nextPosition(source.begin()) {}

    llvm::Optional<Token> nextToken();
};

#endif /* Lexer_hpp */
