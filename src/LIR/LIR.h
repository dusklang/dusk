//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <variant>
#include <string>
#include "General/Enum.h"

namespace lir {
    typedef int32_t Reg;

    #define LIR_STMT_CASES(n, firstCase, case) \
        firstCase(n, NOP) \
        case(n, SAdd, Reg a, b, res) \
        case(n, UAdd, Reg a, b, res) \
        case(n, SSub, Reg a, b, res) \
        case(n, USub, Reg a, b, res) \
        case(n, AddrOf, Reg a, res) \
        case(n, IfZ, Reg a; std::string label) \
        case(n, IfNZ, Reg a; std::string label)

    DECLARE_ENUM(Stmt, LIR_STMT_CASES)
}
