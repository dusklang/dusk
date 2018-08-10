//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <sstream>

#include "String.h"

struct SourcePos {
    uint32_t pos;

    SourcePos(uint32_t pos) : pos(pos) {}
};

struct SourceRange {
    SourcePos begin;
    SourcePos end;

    SourceRange(SourcePos begin, SourcePos end) : begin(begin), end(end) {}
    SourceRange() : begin(0), end(0) {}
};

struct SourceFile {
    std::string name;
    std::string source;

    SourceFile(std::string name, std::string source) : name(name), source(source) {}

    StringRef substringFromRange(SourceRange range) const;
};
