//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "SourceLoc.hpp"

std::string substringAtSourceRange(std::string string, SourceRange range) {
    return string.substr(range.begin(), range.length());
}
