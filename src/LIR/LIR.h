//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <variant>

namespace lir {
    typedef int32_t Reg;
    struct NOP {};
    struct SAdd { Reg a, b, res; };
    struct UAdd { Reg a, b, res; };
    struct SSub { Reg a, b, res; };
    struct USub { Reg a, b, res; };
    struct AddrOf { Reg a, res; };

    struct Stmt {
        typedef std::variant<NOP, SAdd, UAdd, SSub, USub, AddrOf> Data;
        Data data;

        Stmt(Data data) : data(data) {}
        Stmt(NOP stmt) : data(stmt) {}
        Stmt(SAdd stmt) : data(stmt) {}
        Stmt(UAdd stmt) : data(stmt) {}
        Stmt(SSub stmt) : data(stmt) {}
        Stmt(USub stmt) : data(stmt) {}
        Stmt(AddrOf stmt) : data(stmt) {}
    };
}
