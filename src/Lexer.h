#pragma once

#include "Token.h"
#include "Collections.h"

class SourceFile;
Array<Token> lex(SourceFile* file);
