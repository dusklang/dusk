//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "Lexer.hpp"
#include <assert.h>
#include <iostream>

llvm::Optional<Token> Lexer::getNextToken() {
    auto nextPosition = tokenPositions.top();
    while(nextPosition < source.length()) {
        // Skip whitespace.
        if(isWhitespace()) nextPosition++;
        // Skip single-line comments.
        else if(isSubstr("//"))
            for(nextPosition += 2; nextPosition < source.length() && !isNewline(); nextPosition++);
        // Skip (optionally nested) multi-line comments.
        else if(isSubstr("/*")) {
            int levels = 1;
            for(nextPosition += 2; nextPosition < source.length(); nextPosition++) {
                if(isSubstr("/*")) {
                    nextPosition++;
                    levels++;
                } else if(isSubstr("*/")) {
                    nextPosition++;
                    levels--;
                }
                if(levels == 0) {
                    nextPosition++;
                    break;
                }
            }
        } else break;
    }

    llvm::Optional<Token> _tok;
#define RETURN(token) _tok = token; if(_tok) { std::cout << _tok->prettyPrint(); tokenPositions.push(nextPosition); tokens.push(*_tok); } return _tok

    // Return nothing if at the end.
    if(nextPosition == source.length()) {
        RETURN(llvm::None);
    }

    // Lex separators.
    switch(nextChar()) {
        #define TOKEN_SEPARATOR(name, character) case character:\
            nextPosition++; RETURN(Token::Token(tok::sep_ ## name, character));
        #include "TokenKinds.def"
    }

    std::string tokenText;

    // Lex a string literal.
    if(isDoubleQuote()) {
        tokenText += '"';
        nextPosition++;
        while(nextPosition < source.length() && !isNewline()) {
            tokenText += nextChar();

            if(isDoubleQuote() && source.at(nextPosition - 1) != '\\') {
                nextPosition++;
                RETURN(Token(tok::string_literal, tokenText));
            } else {
                nextPosition++;
            }
        }
        assert(false && "Unterminated string literal");
    }

    // Lex an identifier.
    if(nextPosition < source.length() && isLetter())
        while(nextPosition < source.length() && (isLetter() || isNumber()))
            tokenText += source.at(nextPosition++);
    if(!tokenText.empty()) {
        #define TOKEN_KEYWORD(name, sourcerepr) if(tokenText == #sourcerepr) RETURN(Token(tok::kw_ ## name, tokenText));
        #include "TokenKinds.def"
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
        tokenText += source.at(nextPosition++);
    }
    if(!tokenText.empty()) {
        RETURN(Token::Token(hasDot ? tok::decimal_literal : tok::integer_literal, tokenText));
    }

    assert(false && "Unhandled token");
}
