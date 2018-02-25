//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <string>
#include "Types.h"

std::string getNameForBuiltinType(BuiltinType type) {
    switch(type) {
        #define BUILTIN_TYPE(name) case BuiltinType::name: return #name;
        #include "BuiltinTypes.def"
    }
}
