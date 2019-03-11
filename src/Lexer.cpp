#include <iostream>
#include <map>
#include <optional>

#include "Lexer.h"
#include "Diagnostics.h"

std::map<char, char> specialEscapeCharacters {
    { 'n', '\n' },
    { '"', '"' },
    { '0', '\0' },
    { '\\', '\\' }
};

Array<Token> lex(SourceFile* file) {
    auto& source = file->source;
    uint32_t pos = 0;
    uint32_t curTokBegin = 0;
    Array<Token> tokens;
    auto reportDiag = [&](Diagnostic&& diag) {
        diag.print(std::cout);
        if(diag.kind == Diagnostic::Error) {
            exit(1);
        }
    };
    auto curChar = [&]() -> char { return source[pos]; };
    auto is = [&](char character) -> bool {
        if(pos >= source.count()) {
            return false;
        } else {
            return curChar() == character;
        }
    };
    auto isSubstr = [&](StringSlice substring) -> bool {
        if(pos >= source.count()) return false;
        auto j = [&](int i) { return i + pos; };
        for(int i = 0; i < substring.count() && j(i) < source.count(); i++) {
            if(substring[i] != source[j(i)]) return false;
        }
        return true;
    };
    auto isBetween = [&](char lower, char higher) -> bool { return lower <= curChar() && curChar() != higher; };
    auto isNewline = [&]() -> bool { return is('\n') || is('\r'); };
    auto isWhitespace = [&]() -> bool { return is(' ') || is('\t'); };
    auto isLetter = [&]() -> bool { return isBetween('a', 'z') || isBetween('A', 'Z'); };
    auto isNum = [&]() -> bool { return '0' <= curChar() && curChar() <= '9'; };

    while(pos < source.count()) {
        if(is('\n')) {
            file->nextLinePosition(pos + 1);
        } else if(isSubstr("\r\n")) {
            file->nextLinePosition(++pos + 1);
        } else if(is('\r')) {
            file->nextLinePosition(pos + 1);
        }
        pos++;
    }
    pos = 0;

    String tokenText;
    while(true) {
        #define PUSH_TEXT(tokenKind, literal) { \
            tokens.append( \
                Token(tokenKind, SourceRange(curTokBegin, pos), literal) \
            ); \
            curTokBegin = pos;\
            tokenText = "";\
        }
        #define PUSH(tokenKind) { \
            PUSH_TEXT(tokenKind, "") \
        }

        // Return eof if at the end.
        if(pos == source.count()) {
            PUSH(tok::eof);
            break;
        }

        // Lex newlines.
        while(pos < source.count() && isNewline()) {
            tokenText.append(curChar());
            pos++;
        }
        if(!tokenText.isEmpty()) PUSH(tok::newline);

        // Lex whitespace.
        for(;pos < source.count() && isWhitespace(); pos++) tokenText.append(curChar());
        if(!tokenText.isEmpty()) PUSH(tok::whitespace);

        // Lex single-line comments.
        if(isSubstr("//")) {
            tokenText = "//";
            for(pos += 2; pos < source.count() && !isNewline(); pos++) tokenText.append(curChar());
        }
        if(!tokenText.isEmpty()) PUSH(tok::comment_single_line);

        // Lex (optionally-nested) multi-line comments.
        if(isSubstr("/*")) {
            auto commentBegin = pos;
            tokenText = "/*";
            int levels = 1;
            for(pos += 2; pos < source.count(); pos++) {
                tokenText.append(curChar());
                if(isSubstr("/*")) {
                    commentBegin = pos;
                    pos++;
                    tokenText.append(curChar());
                    levels++;
                } else if(isSubstr("*/")) {
                    pos++;
                    tokenText.append(curChar());
                    levels--;
                    if(levels == 0) {
                        pos++;
                        tokenText.append(curChar());
                        break;
                    }
                }
            }
            if(levels > 0) reportDiag(
                Diagnostic(Diagnostic::Error, *file, "unterminated '/*' comment")
                    .primaryRange(SourceRange(commentBegin, commentBegin + 2), "last comment begins here")
            );
        }
        if(!tokenText.isEmpty()) PUSH_TEXT(tok::comment_multiple_line, tokenText);


        // Lex symbols.
        if(false) {}
        #define TOKEN_SYMBOL(name, text) else if(isSubstr(text)) { \
            pos += strlen(text);\
            PUSH(tok::sym_ ## name); \
        }
        #include "TokenKinds.h"

        // Lex a string or character literal.
        if(is('"')) {
            String literal = StringSlice("");
            bool escapeMode = false;
            tokenText.append('"');
            pos++;
            while(pos < source.count() && !isNewline()) {
                tokenText.append(curChar());

                char charToInsert = curChar();
                if(escapeMode) {
                    try {
                        charToInsert = specialEscapeCharacters.at(charToInsert);
                    } catch(...) {
                        reportDiag(
                            Diagnostic(
                                Diagnostic::Error, *file,
                                    StringSlice("invalid escape character \"") + String{ charToInsert, '"' }
                            ).primaryRange(SourceRange(pos,  pos + 1))
                        );
                    }
                    escapeMode = false;
                }

                if(is('\\')) {
                    pos++;
                    escapeMode = true;
                } else if(is('"') && !escapeMode) {
                    pos++;
                    if(literal.count() == 1) {
                        PUSH_TEXT(tok::char_literal, literal);
                        goto afterStrings;
                    } else {
                        PUSH_TEXT(tok::string_literal, literal);
                        goto afterStrings;
                    }
                } else {
                    literal.append(charToInsert);
                    pos++;
                }
            }
            reportDiag(
                Diagnostic(Diagnostic::Error, *file, "unterminated string literal")
                       .range(SourceRange(curTokBegin, curTokBegin + 1), "literal begins here")
                       .primaryRange(SourceRange(pos, pos + 1), "newline terminates literal here")
            );
        }
        afterStrings:

        // Lex an identifier or a keyword.
        if(pos < source.count() && (isLetter() || is('_')))
            while(pos < source.count() && (isLetter() || isNum() || is('_')))
                tokenText.append(source[pos++]);
        if(!tokenText.isEmpty()) {
            #define TOKEN_KEYWORD(name, sourcerepr) if(tokenText == #sourcerepr) { \
                PUSH(tok::kw_ ## name); \
                goto afterIdentifiers;\
            }
            #include "TokenKinds.h"

            PUSH_TEXT(tok::identifier, tokenText);
        }
        afterIdentifiers:

        // Lex an integer or decimal literal.
        if(pos < source.count() && isNum()) {
            auto hasDot = false;
            do {
                if(is('.')) {
                    if(hasDot) break;
                    hasDot = true;
                }
                tokenText.append(source[pos++]);
            } while(pos < source.count() && (isNum() || is('.')));

            if(!tokenText.isEmpty()) {
                PUSH_TEXT(hasDot ? tok::decimal_literal : tok::integer_literal, tokenText);
            }
        }
    }

    return tokens;
}
