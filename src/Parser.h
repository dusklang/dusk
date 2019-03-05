#pragma once

#include "Collections.h"
#include "Token.h"
#include "HIR.h"

hir::Program parse(SourceFile const& file, Slice<Token> tokens);
