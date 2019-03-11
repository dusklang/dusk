#pragma once

#include <optional>

#include "Misc.h"
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
        return SourceRange(min(begin.pos, other.begin.pos), max(end.pos, other.end.pos));
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
    StringSlice name;
    StringSlice source;

    SourceFile(StringSlice name, StringSlice source) : name(name), source(source) {}

    void nextLinePosition(SourcePos linePos) {
        lines.append(linePos);
    }
    StringSlice substringFromRange(SourceRange range) const;
    StringSlice substringFromLine(uint32_t lineNum) const;
    Array<LineRange> linesInRange(SourceRange range) const;
};
