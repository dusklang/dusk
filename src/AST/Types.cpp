//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <string>
#include <cassert>
#include "AST.h"
#include "Types.h"

std::string Type::name() const {
    switch(tag) {
        case inferred: return "<to be inferred>";
        case builtin: switch(builtinTy) {
            #define BUILTIN_TYPE(name) case BuiltinType::name: return #name;
            #include "BuiltinTypes.def"
        }
        case pointer: return "*" + pointedTy->name();
    }
}
