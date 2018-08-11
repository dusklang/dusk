//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "Stmt.h"
#include "Expr.h"
#include "mpark/patterns.hpp"
using namespace mpark::patterns;

ReturnStmt::~ReturnStmt() {
    if(value) {
        delete value;
    }
}

IfStmt::~IfStmt() {
    delete condition;
    delete thenScope;
    match(elseNode)(
        pattern(some(as<Scope*>(arg))) = [](auto scope) { delete scope; },
        pattern(some(as<IfStmt*>(arg))) = [](auto stmt) { delete stmt; },
        pattern(_) = []{}
    );
}

WhileStmt::~WhileStmt() {
    delete condition;
    delete thenScope;
}
