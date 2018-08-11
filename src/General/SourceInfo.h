//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <sstream>
#include <vector>
#include <optional>

#include "String.h"

struct SourcePos {
    uint32_t pos;

    SourcePos(uint32_t pos) : pos(pos) {}
    SourcePos() : pos(0) {}
};

struct SourceRange {
    SourcePos begin;
    SourcePos end;

    SourceRange(SourcePos begin, SourcePos end) : begin(begin), end(end) {}
    SourceRange() : begin(0), end(0) {}
};

struct LineRange {
    uint32_t line;
    SourcePos startColumn;
    SourcePos endColumn;

    LineRange(uint32_t line, SourcePos startColumn, SourcePos endColumn) :
        line(line), startColumn(startColumn), endColumn(endColumn) {}
};

class SourceFile {
    std::vector<SourcePos> lines;

public:
    std::string const name;
    std::string const source;

    SourceFile(std::string name, std::string source) : name(name), source(source) {}

    void nextLinePosition(SourcePos linePos) {
        lines.push_back(linePos);
    }
    StringRef substringFromRange(SourceRange range) const;
    StringRef substringFromLine(uint32_t lineNum) const;
    std::vector<LineRange> linesInRange(SourceRange range) const;
};
