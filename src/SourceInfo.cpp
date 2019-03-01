#include "SourceInfo.h"

StringSlice SourceFile::substringFromRange(SourceRange range) const {
    return source[Range<>(range.begin.pos, range.end.pos)];
}

StringSlice SourceFile::substringFromLine(uint32_t lineNum) const {
    uint32_t begin = lines[lineNum].pos;
    uint32_t end;
    if(lineNum == lines.count() - 1) {
        end = source.count();
    } else {
        end = lines[lineNum + 1].pos;
    }
    return source[Range<>(begin, end)];
}

Array<LineRange, 2> SourceFile::linesInRange(SourceRange range) const {
    Array<LineRange> result;

    for(uint32_t i = 0; i < lines.count(); i++) {
        uint32_t lineBegin = lines[i].pos;
        uint32_t lineEnd;
        if(i == lines.count() - 1) {
            lineEnd = source.count();
        } else {
            lineEnd = lines[i + 1].pos;
        }

        uint32_t columnBegin = 0;
        uint32_t columnEnd = lineEnd - lineBegin;

        bool beginInRange = lineBegin <= range.begin.pos && lineEnd > range.begin.pos;
        bool endInRange = lineBegin < range.end.pos && lineEnd >= range.end.pos;
        if(beginInRange || endInRange) {
            if(endInRange) {
                columnEnd = range.end.pos - lineBegin;
            }
            if(beginInRange) {
                columnBegin = range.begin.pos - lineBegin;
            }
        }
        // Unless the entire line is inside the range, skip this line.
        else if(!(range.begin.pos < lineBegin && range.end.pos > lineEnd)) {
            continue;
        }
        result.append(LineRange(i, columnBegin, columnEnd));
    }

    return result;
}
