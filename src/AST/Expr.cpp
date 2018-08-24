//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "Expr.h"
#include "Decl.h"

#include "mpark/patterns.hpp"
using namespace mpark::patterns;

DeclRefExpr::~DeclRefExpr() {
    if(decl) {
        delete decl;
    }
}

bool DeclRefExpr::isMutable() const {
    return decl->isVar;
}

ReturnExpr::~ReturnExpr() {
    if(value) {
        delete value;
    }
}

IfExpr::~IfExpr() {
    delete condition;
    delete thenScope;
    match(elseNode)(
        pattern(some(as<Scope*>(arg))) = [](auto scope) { delete scope; },
        pattern(some(as<IfExpr*>(arg))) = [](auto expr) { delete expr; },
        pattern(_) = []{}
    );
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

WhileExpr::~WhileExpr() {
    delete condition;
    delete thenScope;
}
