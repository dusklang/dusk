#include "Token.h"

Slice<char> Token::prettyPrint() {
    #define TOKEN(name) case tok::name: return String<>(#name " ") + getText();
    switch(kind) {
        #include "TokenKinds.h"
        default: return String<>("undefined token");
    }
}
