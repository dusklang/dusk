//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>

struct SourceLoc {
    const std::string* source;
    int location;

    SourceLoc(const std::string* source, int location) : source(source), location(location) {}
};

struct SourceRange {
    SourceLoc begin;
    int length;

    SourceRange(SourceLoc begin, int length) : begin(begin), length(length) {}
    std::string getSubstring() const;
};
