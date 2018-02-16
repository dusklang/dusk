//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "Lexer.hpp"
#include <assert.h>
#include <iostream>

Token Lexer::nextTok() {
    auto pos = tokenPositions.top();
    std::string tokenText;

    Token _tok;
    #define RETURN(token) { \
        _tok = token;\
        tokenPositions.push(pos);\
        tokens.push(_tok);\
        return _tok;\
    }

    // Return eof if at the end.
    if(pos == source.length()) RETURN(Token(tok::eof, ""));

    // Lex newlines.
    for(;pos < source.length() && isNewline(pos); pos++) tokenText += source.at(pos);
    if(!tokenText.empty()) RETURN(Token(tok::newline, tokenText));

    // Lex whitespace.
    for(;pos < source.length() && isWhitespace(pos); pos++) tokenText += source.at(pos);
    if(!tokenText.empty()) RETURN(Token(tok::whitespace, tokenText));

    // Lex single-line comments.
    if(isSubstr("//", pos)) {
        tokenText = "//";
        for(pos += 2; pos < source.length() && !isNewline(pos); pos++) tokenText += source.at(pos);
    }
    if(!tokenText.empty()) RETURN(Token(tok::comment_single_line, tokenText));

    // Lex (optionally-nested) multi-line comments.
    if(isSubstr("/*", pos)) {
        tokenText = "/*";
        int levels = 1;
        for(pos += 2; pos < source.length(); pos++) {
            tokenText += source.at(pos);
            if(auto test = isSubstr("/*", pos)) {
                pos++;
                tokenText += source.at(pos);
                levels++;
            } else if(isSubstr("*/", pos)) {
                pos++;
                tokenText += source.at(pos);
                levels--;
                if(levels == 0) {
                    pos++;
                    tokenText += source.at(pos);
                    break;
                }
            }
        }
        if(levels > 0) assert(false && "Unterminated /* comment.");
    }
    if(!tokenText.empty()) RETURN(Token(tok::comment_multiple_line, tokenText));

    // Lex separators.
    switch(source.at(pos)) {
        #define TOKEN_SEPARATOR(name, character) case character:\
            pos++; RETURN(Token(tok::sep_ ## name, character));
        #include "TokenKinds.def"
    }

    // Lex a string literal.
    if(is('"', pos)) {
        tokenText += '"';
        pos++;
        while(pos < source.length() && !isNewline(pos)) {
            tokenText += source.at(pos);

            if(is('"', pos) && source.at(pos - 1) != '\\') {
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
        while(pos < source.length() && (isLetter(pos) || isNum(pos)))
            tokenText += source.at(pos++);
    if(!tokenText.empty()) {
        #define TOKEN_KEYWORD(name, sourcerepr) if(tokenText == #sourcerepr) RETURN(Token(tok::kw_ ## name, tokenText));
        #include "TokenKinds.def"

        RETURN(Token(tok::identifier, tokenText));
    }

    // Lex an integer or decimal literal.
    auto hasDot = false;
    while(isNum(pos) || is('.', pos)) {
        if(is('.', pos)) {
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
