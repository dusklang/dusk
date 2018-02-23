//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <string>
#include "General/SourceLoc.h"

enum class tok {
    #define TOKEN(name) name,
    #include "TokenKinds.def"
    NUM_TOKENS
};

class Token {
private:
    tok kind = tok::NUM_TOKENS;
    SourceRange range;

public:
    Token(tok kind, SourceRange range) : kind(kind), range(range) {}
    Token() : range(SourceLoc(nullptr, 0), 0) {}
    Token& operator=(const Token& otherTok) {
        kind = otherTok.kind;
        range = otherTok.range;
        return *this;
    }

    bool is(tok k) const { return kind == k; }
    bool isAny(tok k) const { return is(k); }
    template <typename ...T>
    bool isAny(tok k1, tok k2, T... k) const {
        if(is(k1)) return true;
        return isAny(k2, k...);
    }

    bool isNot(tok k) const { return kind != k; }
    template <typename ...T>
    bool isNot(tok k1, tok k2, T... k) const { return !isAny(k1, k...); }
    bool isInsignificant() const {
        return isAny(tok::whitespace, tok::comment_single_line, tok::comment_multiple_line, tok::newline);
    }
    bool isSignificant() const { return !isInsignificant(); }

    tok getKind() const { return kind; }
    std::string getText() const { return range.getSubstring(); }
    SourceLoc getLoc() const { return range.begin; }
    SourceRange getRange() const { return range; }

    bool isAnySeparator() const {
        #define TOKEN_SEPARATOR(name, character) || kind == tok::sep_ ## name
        return false
            #include "TokenKinds.def"
        ;
    }

    std::string prettyPrint() {
        #define TOKEN_SEPARATOR(name, character) case tok::sep_ ## name: return "separator " + getText();
        #define TOKEN(name) case tok::name: return #name " " + getText();
        switch(kind) {
            #include "TokenKinds.def"
            default: return "undefined token";
        }
    };
};
