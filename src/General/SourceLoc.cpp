//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "SourceLoc.h"

std::string SourceRange::getSubstring() const {
    return begin.source.substr(begin.location, length);
}
