//  Created by Zach Wolfe on 2018-02-11.

#include "Lexer.hpp"
#include <assert.h>
#include <iostream>

llvm::Optional<Token> Lexer::nextToken() {
    while(nextPosition != source.end()) {
        // Skip whitespace.
        if(isWhitespace()) nextPosition++;
        // Skip single-line comments.
        else if(isSubstr("//"))
            for(nextPosition += 2; nextPosition != source.end() && !isNewline(); nextPosition++);
        else break;
    }

    // Return None if at EOF.
    if(nextPosition == source.end()) { return llvm::NoneType::None; }

    Token Tok;
    #define RETURN(token) Tok = token; currentToken = Tok; return Tok
    // Lex separators.
    switch(*nextPosition) {
        #define TOKEN_SEPARATOR(name, character) case character:\
            nextPosition++; RETURN(Token::Token(tok::sep_ ## name, character));
        #include "TokenKinds.def"
    }

    std::string tokenText;

    // Lex an identifier.
    while(isLetter())
        tokenText += *nextPosition++;
    if(!tokenText.empty()) {
        RETURN(Token(tok::identifier, tokenText));
    }

    // Lex an integer or decimal literal.
    auto hasDot = false;
    while(isNumber() || isDot()) {
        if(isDot()) {
            // TODO: Add a better diagnostic system than runtime assertions.
            assert(!hasDot && "Decimal literals can have only one dot.");
            hasDot = true;
        }
        tokenText += *nextPosition++;
    }
    if(!tokenText.empty()) {
        RETURN(Token::Token(hasDot ? tok::decimal_literal : tok::integer_literal, tokenText));
    }

    assert(false && "Unhandled token");
}
