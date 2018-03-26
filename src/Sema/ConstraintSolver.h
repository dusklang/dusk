//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <map>
#include <vector>

#include "AST/AST.h"
#include "Constraint.h"

struct Solution {
    std::map<int, Type> types;

    Solution operator + (const Solution& other) const {
        auto types = this->types;
        types.insert(this->types.begin(), this->types.end());
        types.insert(other.types.begin(), other.types.end());
        return Solution {
            types
        };
    }

    void operator += (const Solution& other) {
        types.insert(other.types.begin(), other.types.end());
    }
};

Optional<Solution> solveSystem(const std::vector<Constraint>& constraints);
Optional<Solution> solveConstraint(const Constraint& constraint);
