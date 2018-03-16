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
    struct BindOverloadConstraint {
        Type ty;
        std::shared_ptr<Decl> decl;
        std::shared_ptr<DeclRefExpr> expr;
    };
    struct DisjunctionConstraint {
        std::vector<Constraint> constraints;
    };
    struct ConjunctionConstraint {
        std::vector<Constraint> constraints;
    };
    typedef boost::variant<EqualConstraint, BindOverloadConstraint, DisjunctionConstraint,
                           ConjunctionConstraint>
        Data;
    Data data;
private:
    Constraint(Data data) : data(data) {}
public:
    static Constraint Equal(Type lhs, Type rhs) { return Constraint(EqualConstraint { lhs, rhs }); }
    static Constraint Disjunction(std::vector<Constraint> constraints) {
        if(constraints.size() == 1) {
            return constraints[0];
        }
        return Constraint(DisjunctionConstraint { constraints });
    }
    static Constraint Conjunction(std::vector<Constraint> constraints) {
        if(constraints.size() == 1) {
            return constraints[0];
        }
        return Constraint(ConjunctionConstraint { constraints });
    }

    static Constraint BindOverload(Type ty, std::shared_ptr<Decl> decl, std::shared_ptr<DeclRefExpr> expr) {
        return Constraint(BindOverloadConstraint { ty, decl, expr });
    }

    void dump(std::ostream& stream);
    Constraint substituting(const std::map<int, Type>& solution) const {
        struct SubstitutionVisitor: public boost::static_visitor<Constraint> {
            const std::map<int, Type>& solution;
            SubstitutionVisitor(const std::map<int, Type>& solution) : solution(solution) {}
            Constraint operator()(EqualConstraint constraint) const {
                return Constraint::Equal(constraint.lhs.substituting(solution), constraint.rhs.substituting(solution));
            }
            Constraint operator()(BindOverloadConstraint constraint) const {
                return Constraint::BindOverload(constraint.ty.substituting(solution),
                                                constraint.decl, constraint.expr);
            }
            std::vector<Constraint> composite(const std::vector<Constraint>& constraints) const {
                std::vector<Constraint> newConstraints;
                for(auto& constraint: constraints) {
                    newConstraints.push_back(constraint.substituting(solution));
                }
                return newConstraints;
            }
            Constraint operator()(DisjunctionConstraint constraint) const {
                return Constraint::Disjunction(composite(constraint.constraints));
            }
            Constraint operator()(ConjunctionConstraint constraint) const {
                return Constraint::Conjunction(composite(constraint.constraints));
            }
        };
        return boost::apply_visitor(SubstitutionVisitor(solution), data);
    }
};
