//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "Stmt.h"
#include "Expr.h"

ReturnStmt::~ReturnStmt() {
    if(value) {
        delete value;
    }
}

AssignmentStmt::~AssignmentStmt() {
    delete lhs;
    delete rhs;
}

IfStmt::~IfStmt() {
    delete condition;
    delete thenScope;
    if(elseScope) {
        delete elseScope;
    }
}

WhileStmt::~WhileStmt() {
    delete condition;
    delete thenScope;
}
