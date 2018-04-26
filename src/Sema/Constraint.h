//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <vector>
#include <variant>

#include "AST/AST.h"
#include "AST/Decl.h"

struct Constraint {
    struct EqualConstraint {
        Type lhs, rhs;
    };
    typedef std::variant<EqualConstraint> Data;
    Data data;
private:
    Constraint(Data data) : data(data) {}
public:
    static Constraint Equal(Type lhs, Type rhs) { return Constraint(EqualConstraint { lhs, rhs }); }

    void dump(std::ostream& stream);
    Constraint substituting(std::map<int, Type> const& solution) const {
        struct SubstitutionVisitor {
            std::map<int, Type> const& solution;
            SubstitutionVisitor(std::map<int, Type> const& solution) : solution(solution) {}
            Constraint operator()(EqualConstraint constraint) const {
                return Constraint::Equal(constraint.lhs.substituting(solution), constraint.rhs.substituting(solution));
            }
        };
        return std::visit(SubstitutionVisitor(solution), data);
    }
};
