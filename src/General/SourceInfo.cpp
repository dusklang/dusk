//  Copyright © 2019 Zach Wolfe. All rights reserved.

#include "SourceInfo.h"

StringRef SourceFile::substringFromRange(SourceRange range) const {
    return StringRef(source.data() + range.begin.pos, range.end.pos - range.begin.pos);
}

StringRef SourceFile::substringFromLine(uint32_t lineNum) const {
    uint32_t length;
    uint32_t begin = lines[lineNum].pos;
    if(lineNum == lines.count() - 1) {
        length = source.size() - begin;
    } else {
        length = lines[lineNum + 1].pos - begin;
    }
    return StringRef(source.data() + begin, length);
}

Array<LineRange> SourceFile::linesInRange(SourceRange range) const {
    Array<LineRange> result;

    for(uint32_t i = 0; i < lines.count(); i++) {
        uint32_t lineBegin = lines[i].pos;
        uint32_t lineEnd;
        if(i == lines.count() - 1) {
            lineEnd = source.size();
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
