#pragma once

#include "Collections.h"
#include "Token.h"
#include "HIR.h"

void parse(Slice<Token> tokens, hir::Builder* builder);
