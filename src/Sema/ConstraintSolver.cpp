//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "ConstraintSolver.h"
#include <iostream>
#include <tuple>

void reportError(std::string message) {
    std::cout << "CONSTRAINT-SOLVING ERROR: " << message << '\n';
    exit(0);
}

struct EqualConstraintSolver: public boost::static_visitor<Optional<Solution>> {
    template<typename T, typename U>
    Optional<Solution> operator()(T, U) const {
        return None;
    }

    template<typename T>
    Optional<Solution> operator()(T lhs, T rhs) const {
        if(Type(lhs) != Type(rhs)) { return None; }
        return Solution { {} };
    }

    Optional<Solution> operator()(int var, int rhs) const {
        reportError("Ambiguous type");
        LLVM_BUILTIN_UNREACHABLE;
    }

    template<typename T>
    Optional<Solution> operator()(int var, T rhs) const {
        return Solution { {{var, Type(rhs)}} };
    }

    template<typename T>
    Optional<Solution> operator()(T lhs, int var) const {
        return Solution { {{var, Type(lhs)}} };
    }
};
Optional<Solution> solveConstraint(Constraint const& constraint) {
    struct ConstraintVisitor: public boost::static_visitor<Optional<Solution>> {
        Optional<Solution> operator()(Constraint::EqualConstraint constraint) const {
            if(constraint.lhs == constraint.rhs) return Solution();
            return boost::apply_visitor(EqualConstraintSolver(),
                                        constraint.lhs.data, constraint.rhs.data);
        }
    };
    return boost::apply_visitor(ConstraintVisitor(), constraint.data);
}
Optional<Solution> solveSystem(std::vector<Constraint> const& constraints) {
    Solution solution;
    for(auto&& it = constraints.rbegin(); it < constraints.rend(); ++it) {
        auto subst = it->substituting(solution.types);
        if(auto subSolution = solveConstraint(*it)) {
            solution += *subSolution;
        } else {
            return None;
        }
    }
    return solution;
}

