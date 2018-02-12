//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <string>

enum class tok {
    #define TOKEN(name) name,
    #include "TokenKinds.def"
    NUM_TOKENS
};

class Token {
private:
    const tok kind;
    const std::string text;

public:
    Token(tok kind, std::string text) : kind(kind), text(text) {}

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

    tok getKind() const { return kind; }

    bool isAnySeparator() const {
        #define TOKEN_SEPARATOR(name) || kind == tok::sep_ ## name
        return false
            #include "TokenKinds.def"
        ;
    }
};
