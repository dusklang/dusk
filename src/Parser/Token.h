//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <string>
#include <vector>

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

    // Used to get the actual text of a string or character literal.
    std::string literal;

public:
    Token(tok kind, SourceRange range, std::string literal) : kind(kind), range(range), literal(literal) {}
    Token() : range(SourceLoc("", 0), 0) {}
    Token& operator=(Token const& otherTok) = default;

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
    std::optional<std::string> getStringLiteral() const {
        if(isNot(tok::string_literal)) return std::nullopt;
        return literal;
    }
    std::optional<char> getCharLiteral() const {
        if(isNot(tok::char_literal)) return std::nullopt;
        return literal[0];
    }
    std::optional<std::string> getIntegerLiteral() const {
        if(isNot(tok::integer_literal)) return std::nullopt;
        return literal;
    }
    std::optional<std::string> getDecimalLiteral() const {
        if(isNot(tok::decimal_literal)) return std::nullopt;
        return literal;
    }

    std::string prettyPrint();
};
