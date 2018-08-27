//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>

class BigDecimal {
    std::string str;
public:
    BigDecimal(std::string str) : str(str) {}
    std::string getString() const { return str; }
};
