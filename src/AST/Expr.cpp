//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "Expr.h"
#include "Decl.h"

DeclRefExpr::~DeclRefExpr() {
    if(decl) {
        delete decl;
    }
}

bool DeclRefExpr::isMutable() const {
    return decl->isVar;
}
