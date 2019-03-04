#pragma once

#include "Collections.h"
#include "Token.h"
#include "HIR.h"

void parse(SourceFile const& file, Slice<Token> tokens, hir::Builder* builder);
