//  Created by Zach Wolfe on 2018-02-11.

#include "Lexer.hpp"

llvm::Optional<Token> Lexer::nextToken() {
    // Skip whitespace.
    for(; nextPosition != source.end() && isWhitespace(); nextPosition++);

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

    // Lex an integer literal.
    while(isNumber())
        tokenText += *nextPosition++;
    if(!tokenText.empty()) {
        RETURN(Token::Token(tok::integer_literal, tokenText));
    }

    nextPosition++;

    return llvm::NoneType::None;
}
