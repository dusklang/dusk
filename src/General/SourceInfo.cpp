//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "SourceInfo.h"

StringRef SourceFile::substringFromRange(SourceRange range) const {
    return StringRef(source.data() + range.begin.pos, range.end.pos - range.begin.pos);
}

StringRef SourceFile::substringFromLine(uint32_t lineNum) const {
    uint32_t length;
    uint32_t begin = lines[lineNum].pos;
    if(lineNum == lines.size() - 1) {
        length = source.size() - begin;
    } else {
        length = lines[lineNum + 1].pos - begin;
    }
    return StringRef(source.data() + begin, length);
}

std::vector<LineRange> SourceFile::linesInRange(SourceRange range) const {
    std::vector<LineRange> result;

    for(uint32_t i = 0; i < lines.size(); i++) {
        auto lineBegin = lines[i].pos;
        uint32_t lineEnd;
        if(i == lines.size() - 1) {
            lineEnd = source.size();
        } else {
            lineEnd = lines[i + 1].pos;
        }

        bool beginInRange = lineBegin <= range.begin.pos && lineEnd > range.begin.pos;
        bool endInRange = lineBegin < range.end.pos && lineEnd >= range.end.pos;
        if(beginInRange || endInRange) {
            if(endInRange) {
                lineEnd = range.begin.pos - lineBegin;
            }
            if(beginInRange) {
                lineBegin = range.begin.pos - lineBegin;
            }
        } else if(range.begin.pos < lineBegin && range.end.pos < lineEnd) {
            lineEnd -= lineBegin;
            lineBegin = 0;
        } else {
            continue;
        }
        result.push_back(LineRange(i, lineBegin, lineEnd));
    }

    return result;
}
