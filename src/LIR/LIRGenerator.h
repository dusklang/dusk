#pragma once

#include "LIR.h"
#include "AST/AST.h"
#include "General/Array.h"

lir::Program generateLIR(Array<ASTNode*> nodes);
