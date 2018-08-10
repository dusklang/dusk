//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "SourceInfo.h"

StringRef SourceFile::substringFromRange(SourceRange range) const {
    char const* begin = source.data() + range.begin.pos;
    uint32_t length = range.end.pos - range.begin.pos;
    return StringRef(begin, length);
}
