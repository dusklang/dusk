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
        return Solution { {}, {} };
    }

    Optional<Solution> operator()(int var, int rhs) const {
        reportError("Attempt to constrain two type variables together");
        LLVM_BUILTIN_UNREACHABLE;
    }

    template<typename T>
    Optional<Solution> operator()(int var, T rhs) const {
        return Solution { {}, {{var, Type(rhs)}} };
    }

    template<typename T>
    Optional<Solution> operator()(T lhs, int var) const {
        return Solution { {}, {{var, Type(lhs)}} };
    }
};
Optional<Solution> solveConstraint(const Constraint& constraint) {
    struct ConstraintVisitor: public boost::static_visitor<Optional<Solution>> {
        Optional<Solution> operator()(Constraint::EqualConstraint constraint) const {
            if(constraint.lhs == constraint.rhs) return Solution();
            return boost::apply_visitor(EqualConstraintSolver(),
                                        constraint.lhs.data, constraint.rhs.data);
        }
        Optional<Solution> operator()(Constraint::ConjunctionConstraint constraint) const {
            // IMMINENT TODO: Simplify constraints, or we'll get errors trying to constrain two type
            // variables together.
            Solution solution;
            for(auto& subConstraint: constraint.constraints) {
                if(auto subSolution = solveConstraint(subConstraint)) {
                    solution += *subSolution;
                } else {
                    return None;
                }
            }
            return solution;
        }
        Optional<Solution> operator()(Constraint::DisjunctionConstraint constraint) const {
            reportError("Attempted to solve disjunction constraint (unimplemented)");
            LLVM_BUILTIN_UNREACHABLE;
        }
        Optional<Solution> operator()(Constraint::BindOverloadConstraint constraint) const {
            constraint.expr->decl = constraint.decl;
            reportError("Attempted to solve bind overload constraint (unimplemented)");
            LLVM_BUILTIN_UNREACHABLE;
        }
    };
    return boost::apply_visitor(ConstraintVisitor(), constraint.data);
}

Optional<Solution> solveSystem(const std::vector<Constraint>& constraints) {
    return solveConstraint(Constraint::Conjunction(constraints));
}
