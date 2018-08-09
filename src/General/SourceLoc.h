//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>

struct SourceLoc {
    // FIXME: Don't copy the entire source code into every SourceLoc. :(
    std::string source;
    int location;

    SourceLoc(std::string source, int location) : source(source), location(location) {}
    SourceLoc() {}
};

struct SourceRange {
    SourceLoc begin;
    int length;

    SourceRange(SourceLoc begin, int length) : begin(begin), length(length) {}
    SourceRange() {}
    std::string getSubstring() const;
};
