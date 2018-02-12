//  Created by Zach Wolfe on 2018-02-11.

#include "Lexer.hpp"

llvm::Optional<Token> Lexer::nextToken() {
    #define RETURN(token) currentToken = token; return token
    // Skip whitespace.
    for(; nextPosition != source.end() && isWhitespace(); nextPosition++);

    if(nextPosition == source.end()) { return llvm::NoneType::None; }

    // Parse separators.
    switch(*nextPosition) {
        #define TOKEN_SEPARATOR(name, character) case character:\
            nextPosition++; RETURN(Token::Token(tok::sep_ ## name, character));
        #include "TokenKinds.def"
    }

    std::string tokenText;

    // Parse an identifier.
    while(isLetter())
        tokenText += *nextPosition++;
    if(!tokenText.empty()) {
        Token Tok(tok::identifier, tokenText);
        RETURN(Tok);
    }

    nextPosition++;

    return llvm::NoneType::None;
}
