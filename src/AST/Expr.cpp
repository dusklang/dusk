#include "Expr.h"
#include "Decl.h"

#include "mpark/patterns.hpp"
using namespace mpark::patterns;

bool DeclRefExpr::isMutable() const {
    return decl->isVar;
}

SourceRange IfExpr::totalRange() const {
    auto range = ifRange + condition->totalRange() + thenScope->range;
    if(elseScope) range += elseScope->range;
    return range;
}

SourceRange DoExpr::totalRange() const {
    return doRange +
        match(value)(
            pattern(as<Expr*>(arg)) = [&](auto expr) { return expr->totalRange(); },
            pattern(as<Scope*>(arg)) = [&](auto scope) { return scope->range; }
        );
}
