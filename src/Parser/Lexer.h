//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>

#include "Token.h"
#include "General/SourceInfo.h"

std::vector<Token> lex(SourceFile const& file);
