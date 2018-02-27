//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

enum class BuiltinType {
    #define BUILTIN_TYPE(name) name,
    #include "BuiltinTypes.def"
};
