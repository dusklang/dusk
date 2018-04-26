//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <map>
#include <vector>
#include <iostream>

#include "AST/AST.h"

struct Solution {
    std::map<int, Type> types;

    Solution operator + (Solution const& other) const {
        auto types = this->types;
        types.insert(this->types.begin(), this->types.end());
        types.insert(other.types.begin(), other.types.end());
        return Solution {
            types
        };
    }

    void operator += (Solution const& other) {
        types.insert(other.types.begin(), other.types.end());
    }

    void dump(std::ostream& stream) const {
        stream << "Solution:" << std::endl;
        for(auto& unification: types) {
            stream << unification.first << " = " << unification.second.name() << std::endl;
        }
    }
};
