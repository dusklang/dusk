//  Copyright © 2019 Zach Wolfe. All rights reserved.

#pragma once

#include <iostream>
#include <string>
#include <sstream>

class StringRef final {
    char const* data;
    const uint32_t length;
    friend std::ostream& operator<<(std::ostream& stream, StringRef str);
    friend std::string operator+(char const* lit, StringRef str);
    friend std::string operator+(std::string a, StringRef b);
    friend std::string operator+(StringRef str, char const* lit);
public:
    char const* cString() {
        char* newBuf = new char[length + 1];
        newBuf[length] = '\0';
        for(uint32_t i = 0; i < length; i++) {
            newBuf[i] = data[i];
        }
        return newBuf;
    }
    std::string string() {
        std::ostringstream stream;
        for(uint32_t i = 0; i < length; i++) {
            stream << data[i];
        }
        return stream.str();
    }

    StringRef(char const* data, uint32_t length) : data(data), length(length) {}
};
