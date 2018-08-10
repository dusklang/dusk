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
