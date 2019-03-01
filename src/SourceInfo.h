#pragma once

#include <sstream>
#include <optional>
#include <algorithm>

#include "Collections.h"

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

    LineRange() : line(-1) {}
    LineRange(uint32_t line, SourcePos startColumn, SourcePos endColumn) :
        line(line), startColumn(startColumn), endColumn(endColumn) {}
};

class SourceFile {
    Array<SourcePos> lines;

public:
    String<20> const name;
    String<> const source;

    SourceFile(String<20> name, String<> source) : name(name), source(source) {}

    void nextLinePosition(SourcePos linePos) {
        lines.append(linePos);
    }
    StringSlice substringFromRange(SourceRange range) const;
    StringSlice substringFromLine(uint32_t lineNum) const;
    Array<LineRange, 2> linesInRange(SourceRange range) const;
};
