#pragma once

#include <string>
#include <sstream>
#include <optional>
#include <algorithm>

#include "Array.h"
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

    SourceRange operator+(SourceRange other) const {
        return SourceRange(std::min(begin.pos, other.begin.pos), std::max(end.pos, other.end.pos));
    }
    void operator+=(SourceRange other) {
        *this = *this + other;
    }
};

struct LineRange {
    uint32_t line;
    SourcePos startColumn;
    SourcePos endColumn;

    LineRange(uint32_t line, SourcePos startColumn, SourcePos endColumn) :
        line(line), startColumn(startColumn), endColumn(endColumn) {}
};

class SourceFile {
    Array<SourcePos> lines;

public:
    std::string const name;
    std::string const source;

    SourceFile(std::string name, std::string source) : name(name), source(source) {}

    void nextLinePosition(SourcePos linePos) {
        lines.append(linePos);
    }
    StringRef substringFromRange(SourceRange range) const;
    StringRef substringFromLine(uint32_t lineNum) const;
    Array<LineRange> linesInRange(SourceRange range) const;
};
