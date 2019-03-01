#include <iostream>

#include "Collections.h"

std::ostream& operator << (std::ostream& stream, StringSlice slice) {
    for(char character: slice) {
        stream << character;
    }
    return stream;
}
