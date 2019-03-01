#include "SourceInfo.h"

enum class tok {
    #define TOKEN(name) name,
    #include "TokenKinds.h"
    NUM_TOKENS
};

class Token {
    tok kind = tok::NUM_TOKENS;
    SourceRange range;

    // Used to get the actual text of a string, character, or numeric literal, or identifier.
    String<> text;

public:
    Token(tok kind, SourceRange range, String<> text) : kind(kind), range(range), text(text) {}
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
    String<> getText() const { return text; }
    SourcePos getLoc() const { return range.begin; }
    SourceRange getRange() const { return range; }
    bool isStringLiteral() const {
        return is(tok::string_literal);
    }
    bool isCharLiteral() const {
        return is(tok::char_literal);
    }
    bool isIntegerLiteral() const {
        return is(tok::integer_literal);
    }
    bool isDecimalLiteral() const {
        return is(tok::decimal_literal);
    }
    std::optional<String<>> getIdentifier() const {
        if(isNot(tok::identifier)) return std::nullopt;
        return text;
    }

    Slice<char> prettyPrint();
};
