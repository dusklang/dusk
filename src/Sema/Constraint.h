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
private:
    Data data;
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

    const Data& getData() const { return data; }

    void dump(std::ostream& stream);
};
