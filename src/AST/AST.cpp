//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "AST.h"
#include "Expr.h"

Type Scope::terminalType() const {
    Type ty = VoidTy();
    if(terminalExpr) ty = terminalExpr->type;
    return ty;
}
Expr* Scope::terminalValueExpr() const {
    if(!terminalExpr) return nullptr;
    if(terminalExpr->type == NeverTy()) return nullptr;
    return terminalExpr;
}
