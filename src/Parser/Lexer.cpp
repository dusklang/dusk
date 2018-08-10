//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <iostream>
#include <vector>
#include <map>
#include <optional>

#include "Lexer.h"

std::map<char, char> specialEscapeCharacters {
    { 'n', '\n' },
    { '"', '"' },
    { '0', '\0' },
    { '\\', '\\' }
};

std::vector<Token> lex(SourceFile* file) {
    auto& source = file->source;
    uint32_t pos = 0;
    uint32_t lastLineBegin = 0;
    uint32_t curTokBegin = 0;
    std::vector<Token> tokens;
    auto reportError = [&](uint32_t endPos, std::string message) {
        SourceRange range { SourcePos { pos }, SourcePos { endPos } };
        std::cout << "LEXING ERROR: " << message << '\n';
        std::cout << "Offending area: " << file->substringFromRange(range) << "\n\n";
        exit(1);
    };
    auto curChar = [&]() -> char { return source.at(pos); };
    auto is = [&](char character) -> bool {
        if(pos >= source.length()) {
            return false;
        } else {
            return curChar() == character;
        }
    };
    auto isSubstr = [&](std::string const& substring) -> bool {
        if(pos >= source.length()) return false;
        auto j = [&](int i) { return i + pos; };
        for(int i = 0; i < substring.length() && j(i) < source.length(); i++) {
            if(substring.at(i) != source.at(j(i))) return false;
        }
        return true;
    };
    auto isBetween = [&](char lower, char higher) -> bool { return lower <= curChar() && curChar() != higher; };
    auto newLineChars = [&]() -> uint32_t {
        if(is('\n')) {
            return 1;
        } else if(isSubstr("\r\n")) {
            return 2;
        } else if(is('\r')) {
            return 1;
        } else {
            return 0;
        }
    };
    auto isWhitespace = [&]() -> bool { return is(' ') || is('\t'); };
    auto isLetter = [&]() -> bool { return isBetween('a', 'z') || isBetween('A', 'Z'); };
    auto isNum = [&]() -> bool { return '0' <= curChar() && curChar() <= '9'; };

    while(true) {
        std::string tokenText;

        #define PUSH_TEXT(tokenKind, literal) { \
            tokens.push_back( \
                Token(tokenKind, SourceRange(curTokBegin, pos), literal) \
            ); \
            curTokBegin = pos;\
            tokenText = "";\
        }
        #define PUSH(tokenKind) { \
            PUSH_TEXT(tokenKind, "") \
        }

        // Return eof if at the end.
        if(pos == source.length()) {
            PUSH(tok::eof);
            break;
        }

        // Lex newlines.
        uint32_t newLines;
        while(pos < source.length() && (newLines = newLineChars()) > 0) {
            file->nextLinePosition(lastLineBegin);
            for(int i = 0; i < newLines; i++) {
                tokenText += curChar();
                pos++;
            }
            lastLineBegin = pos;
        }
        if(!tokenText.empty()) PUSH(tok::newline);

        // Lex whitespace.
        for(;pos < source.length() && isWhitespace(); pos++) tokenText += curChar();
        if(!tokenText.empty()) PUSH(tok::whitespace);

        // Lex single-line comments.
        if(isSubstr("//")) {
            tokenText = "//";
            for(pos += 2; pos < source.length() && newLineChars() == 0; pos++) tokenText += curChar();
        }
        if(!tokenText.empty()) PUSH(tok::comment_single_line);

        // Lex (optionally-nested) multi-line comments.
        if(isSubstr("/*")) {
            tokenText = "/*";
            int levels = 1;
            for(pos += 2; pos < source.length(); pos++) {
                tokenText += curChar();
                auto newLines = newLineChars();
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
                } else if(newLines > 0) {
                    file->nextLinePosition(lastLineBegin);
                    pos++;
                    for(int i = 0; i < newLines - 1; i++) {
                        tokenText += curChar();
                        pos++;
                    }
                    lastLineBegin = pos;
                    PUSH_TEXT(tok::comment_multiple_line, tokenText);
                }
            }
            if(levels > 0) reportError(pos, "Unterminated /* comment.");
        }
        if(!tokenText.empty()) PUSH_TEXT(tok::comment_multiple_line, tokenText);


        // Lex symbols.
        if(false) {}
        #define TOKEN_SYMBOL(name, text) else if(isSubstr(text)) { \
            pos += std::string(text).size();\
            PUSH(tok::sym_ ## name); \
        }
        #include "TokenKinds.def"

        // Lex a string or character literal.
        if(is('"')) {
            std::string literal = "";
            bool escapeMode = false;
            tokenText += '"';
            pos++;
            while(pos < source.length() && newLineChars() == 0) {
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
                        PUSH_TEXT(tok::char_literal, literal);
                        goto afterStrings;
                    } else {
                        PUSH_TEXT(tok::string_literal, literal);
                        goto afterStrings;
                    }
                } else {
                    literal += charToInsert;
                    pos++;
                }
            }
            reportError(pos, "Unterminated string literal");
        }
        afterStrings:

        // Lex an identifier or a keyword.
        if(pos < source.length() && (isLetter() || is('_')))
            while(pos < source.length() && (isLetter() || isNum() || is('_')))
                tokenText += source.at(pos++);
        if(!tokenText.empty()) {
            #define TOKEN_KEYWORD(name, sourcerepr) if(tokenText == #sourcerepr) { \
                PUSH(tok::kw_ ## name); \
                goto afterIdentifiers;\
            }
            #include "TokenKinds.def"

            PUSH_TEXT(tok::identifier, tokenText);
        }
        afterIdentifiers:

        // Lex an integer or decimal literal.
        if(pos < source.length() && isNum()) {
            auto hasDot = false;
            do {
                if(is('.')) {
                    if(hasDot) break;
                    hasDot = true;
                }
                tokenText += source.at(pos++);
            } while(pos < source.length() && (isNum() || is('.')));

            if(!tokenText.empty()) {
                PUSH_TEXT(hasDot ? tok::decimal_literal : tok::integer_literal, tokenText);
            }
        }
    }
    return tokens;
}
