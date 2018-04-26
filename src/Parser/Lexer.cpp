//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "Lexer.h"
#include <iostream>

Token Lexer::nextTokIncludingInsignificant() {
    auto pos = tokenPositions.top();
    std::string tokenText;

    auto curChar = [&]() -> char { return source.at(pos); };
    auto is = [&](char character) -> bool { return curChar() == character; };
    auto isBetween = [&](char lower, char higher) -> bool { return lower <= curChar() && curChar() != higher; };
    auto isNewline = [&]() -> bool { return is('\n') || is('\r'); };
    auto isWhitespace = [&]() -> bool { return is(' ') || isNewline() || is('\t'); };
    auto isLetter = [&]() -> bool { return isBetween('a', 'z') || isBetween('A', 'Z'); };
    auto isNum = [&]() -> bool { return '0' <= curChar() && curChar() <= '9'; };
    auto isSubstr = [&](std::string const& substring) -> bool {
        if(pos >= source.length()) return false;
        auto j = [&](int i) { return i + pos; };
        for(int i = 0; i < substring.length() && j(i) < source.length(); i++) {
            if(substring.at(i) != source.at(j(i))) return false;
        }
        return true;
    };

    Token _tok;
    #define RETURN_LIT(tokenKind, literal) { \
        if(numberOfNewTokens) *numberOfNewTokens += 1;\
        _tok = Token(tokenKind, SourceRange(SourceLoc(&source, tokenPositions.top()), pos - tokenPositions.top()), literal);\
        tokenPositions.push(pos);\
        tokens.push(_tok);\
        return _tok;\
    }
    #define RETURN(tokenKind) RETURN_LIT(tokenKind, "")

    // Return eof if at the end.
    if(pos == source.length()) RETURN(tok::eof);

    // Lex newlines.
    for(;pos < source.length() && isNewline(); pos++) tokenText += curChar();
    if(!tokenText.empty()) RETURN(tok::newline);

    // Lex whitespace.
    for(;pos < source.length() && isWhitespace(); pos++) tokenText += curChar();
    if(!tokenText.empty()) RETURN(tok::whitespace);

    // Lex single-line comments.
    if(isSubstr("//")) {
        tokenText = "//";
        for(pos += 2; pos < source.length() && !isNewline(); pos++) tokenText += curChar();
    }
    if(!tokenText.empty()) RETURN(tok::comment_single_line);

    // Lex (optionally-nested) multi-line comments.
    if(isSubstr("/*")) {
        tokenText = "/*";
        int levels = 1;
        for(pos += 2; pos < source.length(); pos++) {
            tokenText += curChar();
            if(isSubstr("/*")) {
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
        if(levels > 0) reportError(pos, "Unterminated /* comment.");
    }
    if(!tokenText.empty()) RETURN(tok::comment_multiple_line);

    // Lex separators.
    switch(curChar()) {
        #define TOKEN_SEPARATOR(name, character) case character:\
            pos++; RETURN(tok::sep_ ## name);
        #include "TokenKinds.def"
    }

    // Lex a string or character literal.
    if(is('"')) {
        std::string literal = "";
        bool escapeMode = false;
        tokenText += '"';
        pos++;
        while(pos < source.length() && !isNewline()) {
            tokenText += curChar();

            char charToInsert = curChar();
            if(escapeMode) {
                try {
                    charToInsert = specialEscapeCharacters.at(charToInsert);
                } catch(...) {
                    reportError(pos, std::string("Invalid escape character '") + charToInsert + "'");
                }
                escapeMode = false;
            }

            if(is('\\')) {
                pos++;
                escapeMode = true;
            } else if(is('"') && !escapeMode) {
                pos++;
                if(literal.size() == 1) {
                    RETURN_LIT(tok::char_literal, literal);
                } else {
                    RETURN_LIT(tok::string_literal, literal);
                }
            } else {
                literal += charToInsert;
                pos++;
            }
        }
        reportError(pos, "Unterminated string literal");
    }

    // Lex an identifier.
    if(pos < source.length() && (isLetter() || is('_')))
        while(pos < source.length() && (isLetter() || isNum() || is('_')))
            tokenText += source.at(pos++);
    if(!tokenText.empty()) {
        #define TOKEN_KEYWORD(name, sourcerepr) if(tokenText == #sourcerepr) RETURN(tok::kw_ ## name);
        #include "TokenKinds.def"

        RETURN(tok::identifier);
    }

    // Lex an integer or decimal literal.
    auto hasDot = false;
    while(pos < source.length() && (isNum() || is('.'))) {
        if(is('.')) {
            if(hasDot) reportError(pos, "Decimal literals can have only one dot.");
            hasDot = true;
        }
        tokenText += source.at(pos++);
    }
    if(!tokenText.empty()) {
        RETURN_LIT(hasDot ? tok::decimal_literal : tok::integer_literal, tokenText);
    }

    assert(false && "Unhandled token");
    _LIBCPP_UNREACHABLE();
}
