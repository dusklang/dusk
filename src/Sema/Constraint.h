//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <vector>
#include "boost/variant.hpp"

#include "AST/AST.h"
#include "AST/Decl.h"

struct Constraint {
    struct EqualConstraint {
        Type lhs, rhs;
    };
    typedef boost::variant<EqualConstraint> Data;
    Data data;
private:
    Constraint(Data data) : data(data) {}
public:
    static Constraint Equal(Type lhs, Type rhs) { return Constraint(EqualConstraint { lhs, rhs }); }

    void dump(std::ostream& stream);
    Constraint substituting(const std::map<int, Type>& solution) const {
        struct SubstitutionVisitor: public boost::static_visitor<Constraint> {
            const std::map<int, Type>& solution;
            SubstitutionVisitor(const std::map<int, Type>& solution) : solution(solution) {}
            Constraint operator()(EqualConstraint constraint) const {
                return Constraint::Equal(constraint.lhs.substituting(solution), constraint.rhs.substituting(solution));
            }
        };
        return boost::apply_visitor(SubstitutionVisitor(solution), data);
    }
};
