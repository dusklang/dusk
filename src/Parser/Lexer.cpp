//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "Lexer.hpp"
#include <assert.h>
#include <iostream>

Token Lexer::nextTok() {
    auto pos = tokenPositions.top();
    std::string tokenText;

    auto curChar = [&]() -> char { return source.at(pos); };
    auto is = [&](char character) -> bool { return curChar() == character; };
    auto isBetween = [&](char lower, char higher) -> bool { return lower <= curChar() && curChar() != higher; };
    auto isNewline = [&]() -> bool { return is('\n') || is('\r'); };
    auto isWhitespace = [&]() -> bool { return is(' ') || isNewline() || is('\t'); };
    auto isLetter = [&]() -> bool { return isBetween('a', 'z') || isBetween('A', 'Z'); };
    auto isNum = [&]() -> bool { return '0' <= curChar() && curChar() <= '9'; };
    auto isSubstr = [&](const std::string& substring) -> bool {
        if(pos >= source.length()) return false;
        auto j = [&](int i) { return i + pos; };
        for(int i = 0; i < substring.length() && j(i) < source.length(); i++) {
            if(substring.at(i) != source.at(j(i))) return false;
        }
        return true;
    };

    Token _tok;
    #define RETURN(token) { \
        _tok = token;\
        tokenPositions.push(pos);\
        tokens.push(_tok);\
        return _tok;\
    }

    // Return eof if at the end.
    if(pos == source.length()) RETURN(Token(tok::eof, "", tokenPositions.top()));

    // Lex newlines.
    for(;pos < source.length() && isNewline(); pos++) tokenText += curChar();
    if(!tokenText.empty()) RETURN(Token(tok::newline, tokenText, tokenPositions.top()));

    // Lex whitespace.
    for(;pos < source.length() && isWhitespace(); pos++) tokenText += curChar();
    if(!tokenText.empty()) RETURN(Token(tok::whitespace, tokenText, tokenPositions.top()));

    // Lex single-line comments.
    if(isSubstr("//")) {
        tokenText = "//";
        for(pos += 2; pos < source.length() && !isNewline(); pos++) tokenText += curChar();
    }
    if(!tokenText.empty()) RETURN(Token(tok::comment_single_line, tokenText, tokenPositions.top()));

    // Lex (optionally-nested) multi-line comments.
    if(isSubstr("/*")) {
        tokenText = "/*";
        int levels = 1;
        for(pos += 2; pos < source.length(); pos++) {
            tokenText += curChar();
            if(auto test = isSubstr("/*")) {
                pos++;
                tokenText += curChar();
                levels++;
            } else if(isSubstr("*/")) {
                pos++;
                tokenText += curChar();
                levels--;
                if(levels == 0) {
                    pos++;
                    tokenText += curChar();
                    break;
                }
            }
        }
        if(levels > 0) assert(false && "Unterminated /* comment.");
    }
    if(!tokenText.empty()) RETURN(Token(tok::comment_multiple_line, tokenText, tokenPositions.top()));

    // Lex separators.
    switch(curChar()) {
        #define TOKEN_SEPARATOR(name, character) case character:\
            pos++; RETURN(Token(tok::sep_ ## name, character, tokenPositions.top()));
        #include "TokenKinds.def"
    }

    // Lex a string literal.
    if(is('"')) {
        tokenText += '"';
        pos++;
        while(pos < source.length() && !isNewline()) {
            tokenText += curChar();

            if(is('"') && source.at(pos - 1) != '\\') {
                pos++;
                RETURN(Token(tok::string_literal, tokenText, tokenPositions.top()));
            } else {
                pos++;
            }
        }
        assert(false && "Unterminated string literal");
    }

    // Lex an identifier.
    if(pos < source.length() && isLetter())
        while(pos < source.length() && (isLetter() || isNum()))
            tokenText += source.at(pos++);
    if(!tokenText.empty()) {
        #define TOKEN_KEYWORD(name, sourcerepr) if(tokenText == #sourcerepr) RETURN(Token(tok::kw_ ## name, tokenText, tokenPositions.top()));
        #include "TokenKinds.def"

        RETURN(Token(tok::identifier, tokenText, tokenPositions.top()));
    }

    // Lex an integer or decimal literal.
    auto hasDot = false;
    while(isNum() || is('.')) {
        if(is('.')) {
            // TODO: Add a better diagnostic system than runtime assertions.
            assert(!hasDot && "Decimal literals can have only one dot.");
            hasDot = true;
        }
        tokenText += source.at(pos++);
    }
    if(!tokenText.empty()) {
        RETURN(Token(hasDot ? tok::decimal_literal : tok::integer_literal, tokenText, tokenPositions.top()));
    }

    assert(false && "Unhandled token");
}
