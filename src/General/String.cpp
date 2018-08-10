//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "String.h"
#include <sstream>

std::ostream& operator<<(std::ostream& stream, StringRef str) {
    for(uint32_t i = 0; i < str.length; i++) {
        stream << str.data[i];
    }
    return stream;
}

std::string operator+(char const* lit, StringRef str) {
    std::ostringstream stream;
    stream << lit << str;
    return stream.str();
}
