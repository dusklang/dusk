//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "Expr.h"
#include "Decl.h"

#include "mpark/patterns.hpp"
using namespace mpark::patterns;

bool DeclRefExpr::isMutable() const {
    return decl->isVar;
}

SourceRange IfExpr::totalRange() const {
    auto range = ifRange + condition->totalRange() + thenScope->range;
    match(elseNode)(
        pattern(some(as<Scope*>(arg))) = [&](auto scope) {
            range += scope->range;
        },
        pattern(some(as<IfExpr*>(arg))) = [&](auto expr) {
            range += expr->totalRange();
        },
        pattern(_) = [] {}
    );
    return range;
}

SourceRange DoExpr::totalRange() const {
    return doRange +
        match(value)(
            pattern(as<Expr*>(arg)) = [&](auto expr) { return expr->totalRange(); },
            pattern(as<Scope*>(arg)) = [&](auto scope) { return scope->range; }
        );
}
