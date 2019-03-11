#include "Token.h"

Slice<char> Token::prettyPrint() {
    #define TOKEN(name) case tok::name: return StringSlice(#name " ") + getText();
    switch(kind) {
        #include "TokenKinds.h"
        default: return StringSlice("undefined token");
    }
}
