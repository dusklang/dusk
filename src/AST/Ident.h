//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <iostream>
#include "General/SourceInfo.h"
#include "General/String.h"

class Ident {
    std::string text;
public:
    SourceRange range;

    friend std::ostream& operator<<(std::ostream& stream, Ident const& ident);

    Ident(std::string text, SourceRange range) : text(text), range(range) {}

    StringRef getText() const {
        return StringRef(text.data(), text.size());
    }

    bool operator==(char const* other) const {
        return text == other;
    }
    bool operator==(Ident const& other) const {
        return text == other.text;
    }
    bool operator!=(Ident const& other) const {
        return !(*this == other);
    }
};
