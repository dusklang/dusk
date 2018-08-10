//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <iostream>
#include <string>

class StringRef final {
    char const* data;
    const uint32_t length;
    friend std::ostream& operator<<(std::ostream& stream, StringRef str);
    friend std::string operator+(char const* lit, StringRef str);
public:
    StringRef(char const* data, uint32_t length) : data(data), length(length) {}
};
