//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <map>
#include <vector>

#include "AST/AST.h"
#include "Constraint.h"

struct Solution {
    struct DeclBinding {
        std::shared_ptr<Decl> decl;
        std::shared_ptr<DeclRefExpr> expr;
    };
    std::vector<DeclBinding> bindings;
    std::map<int, Type> types;

    Solution operator + (const Solution& other) const {
        std::vector<DeclBinding> bindings;
        bindings.insert(bindings.end(), this->bindings.begin(), this->bindings.end());
        bindings.insert(bindings.end(), other.bindings.begin(), other.bindings.end());

        auto types = this->types;
        types.insert(this->types.begin(), this->types.end());
        types.insert(other.types.begin(), other.types.end());
        return Solution {
            bindings,
            types
        };
    }

    void operator += (const Solution& other) {
        bindings.insert(bindings.end(), other.bindings.begin(), other.bindings.end());
        types.insert(other.types.begin(), other.types.end());
    }
};

Optional<Solution> solveConjunction(const std::vector<Constraint>& constraints);
Optional<Solution> solveConstraint(const Constraint& constraint);
