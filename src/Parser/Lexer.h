//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>

#include "Token.h"
#include "General/SourceInfo.h"
#include "General/Array.h"

Array<Token> lex(SourceFile* file);
