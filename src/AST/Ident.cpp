#include "Ident.h"

std::ostream& operator<<(std::ostream& stream, Ident const& ident) {
    stream << ident.text;
    return stream;
}
