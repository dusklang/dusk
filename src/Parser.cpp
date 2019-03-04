#include "Parser.h"

namespace {
    struct Parser {
        Slice<Token> tokens;
        hir::Builder* builder;
    };
}

void parse(Slice<Token> tokens, hir::Builder* builder) {
    
}
