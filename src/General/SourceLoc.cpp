//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "SourceLoc.hpp"

std::string SourceRange::getSubstring() const {
    return begin.source->substr(begin.location, length);
}
