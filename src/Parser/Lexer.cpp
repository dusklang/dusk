//  Created by Zach Wolfe on 2018-02-11.

#include "Lexer.hpp"

llvm::Optional<Token> Lexer::nextToken() {
    // Skip whitespace.
    for(; nextPosition != source.end() && (isSpace(*nextPosition) || isNewline(*nextPosition));
        nextPosition++) {}
    if(nextPosition == source.end()) { return llvm::NoneType::None; }

    // Parse separators.
    switch(*nextPosition) {
#define TOKEN_SEPARATOR(name, character) case character:\
return Token::Token(tok::sep_ ## name, character);
#include "TokenKinds.def"
    }
    nextPosition++;

    return llvm::NoneType::None;
}
