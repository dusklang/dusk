//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "Lexer.hpp"
#include <assert.h>
#include <iostream>

Token Lexer::getNextToken() {
    auto pos = tokenPositions.top();
    while(pos < source.length()) {
        // Skip whitespace.
        if(isWhitespace(pos)) pos++;
        // Skip single-line comments.
        else if(isSubstr("//", pos))
            for(pos += 2; pos < source.length() && !isNewline(pos); pos++);
        // Skip (optionally nested) multi-line comments.
        else if(isSubstr("/*", pos)) {
            int levels = 1;
            for(pos += 2; pos < source.length(); pos++) {
                if(isSubstr("/*", pos)) {
                    pos++;
                    levels++;
                } else if(isSubstr("*/", pos)) {
                    pos++;
                    levels--;
                }
                if(levels == 0) {
                    pos++;
                    break;
                }
            }
        } else break;
    }

    Token _tok;
    #define RETURN(token) { \
        _tok = token;\
        tokenPositions.push(pos);\
        tokens.push(_tok);\
        return _tok;\
    }

    // Return nothing if at the end.
    if(pos == source.length()) RETURN(Token(tok::eof, ""));

    // Lex separators.
    switch(nextChar(pos)) {
        #define TOKEN_SEPARATOR(name, character) case character:\
            pos++; RETURN(Token(tok::sep_ ## name, character));
        #include "TokenKinds.def"
    }

    std::string tokenText;

    // Lex a string literal.
    if(isDoubleQuote(pos)) {
        tokenText += '"';
        pos++;
        while(pos < source.length() && !isNewline(pos)) {
            tokenText += nextChar(pos);

            if(isDoubleQuote(pos) && source.at(pos - 1) != '\\') {
                pos++;
                RETURN(Token(tok::string_literal, tokenText));
            } else {
                pos++;
            }
        }
        assert(false && "Unterminated string literal");
    }

    // Lex an identifier.
    if(pos < source.length() && isLetter(pos))
        while(pos < source.length() && (isLetter(pos) || isNumber(pos)))
            tokenText += source.at(pos++);
    if(!tokenText.empty()) {
        #define TOKEN_KEYWORD(name, sourcerepr) if(tokenText == #sourcerepr) RETURN(Token(tok::kw_ ## name, tokenText));
        #include "TokenKinds.def"

        RETURN(Token(tok::identifier, tokenText));
    }

    // Lex an integer or decimal literal.
    auto hasDot = false;
    while(isNumber(pos) || isDot(pos)) {
        if(isDot(pos)) {
            // TODO: Add a better diagnostic system than runtime assertions.
            assert(!hasDot && "Decimal literals can have only one dot.");
            hasDot = true;
        }
        tokenText += source.at(pos++);
    }
    if(!tokenText.empty()) {
        RETURN(Token(hasDot ? tok::decimal_literal : tok::integer_literal, tokenText));
    }

    assert(false && "Unhandled token");
}
