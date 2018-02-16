//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <iostream>
#include "Parser/Parser.hpp"

int main(int argc, const char * argv[]) {
    std::cout << "Quill compiler version 0.0.1\n\n";
    Parser parser("helloThere // This is a test of parsing single-line comments\n"
                  "/* what about multi-line comments? \n aVariableDecl : 4 = 3.141592/* nested?\n */ */\n"
                  "aParameterizedDecl(that: 4)(currying: 4)(other: 4, sortOf: 4) : 4 { }\n");

    if(auto scope = parser.parseScope()) {
        std::cout << (*scope)->prettyPrint() << '\n';
    }

    return 0;
}
