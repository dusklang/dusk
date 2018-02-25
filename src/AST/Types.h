//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

enum class BuiltinType {
#define BUILTIN_TYPE(name) name,
#include "BuiltinTypes.def"
};

std::string getNameForBuiltinType(BuiltinType type) {
    switch(type) {
        #define BUILTIN_TYPE(name) case BuiltinType::name: return #name;
        #include "BuiltinTypes.def"
    }
}
