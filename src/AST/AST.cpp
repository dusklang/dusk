//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "AST.h"
#include "Expr.h"

Argument::~Argument() {
    delete value;
}
