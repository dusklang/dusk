//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <iostream>
#include "General/SourceInfo.h"

struct Ident {
    std::string text;
    SourceRange range;

    friend std::ostream& operator<<(std::ostream& stream, Ident const& ident);

    Ident(std::string text, SourceRange range) : text(text), range(range) {}

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

std::ostream& operator<<(std::ostream& stream, Ident const& ident);
