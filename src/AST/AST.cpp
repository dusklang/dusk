//  Copyright © 2019 Zach Wolfe. All rights reserved.

#include "AST.h"
#include "Expr.h"

Type Scope::terminalType() const {
    Type ty = VoidTy();
    if(terminalExpr) ty = terminalExpr->type;
    return ty;
}
