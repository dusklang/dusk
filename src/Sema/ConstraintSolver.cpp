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

    Optional<Solution> operator()(Type::Variable var, Type::Variable rhs) const {
        reportError("Ambiguous type");
        LLVM_BUILTIN_UNREACHABLE;
    }

    template<typename T>
    Optional<Solution> operator()(Type::Variable var, T rhs) const {
        switch(var.kind) {
            case Type::Variable::General: break;
            case Type::Variable::Integer:
                if constexpr(!std::is_same_v<T, Type::IntegerTy>) {
                    reportError("Integer literal cannot unify with non-integer type");
                }
                break;
            case Type::Variable::Decimal:
                if constexpr(!std::is_same_v<T, Type::FloatTy> && !std::is_same_v<T, Type::DoubleTy>) {
                    reportError("Decimal literal cannot unify with non-decimal or floating point type");
                }
                break;
        }
        return Solution { {{var.num, Type(rhs)}} };
    }

    template<typename T>
    Optional<Solution> operator()(T lhs, Type::Variable var) const {
        return Solution { {{var.num, Type(lhs)}} };
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

