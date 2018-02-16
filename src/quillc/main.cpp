//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <iostream>
#include "Parser/Lexer.hpp"

int main(int argc, const char * argv[]) {
    std::cout << "Quill compiler version 0.0.1\n\n";
    Lexer lexer("mut multiply(lhs: 0, rhs: 4) : 4 { hello: 4 { hiThereIndeed(wow: 56) : 4 { }  } }\n"
                  "aVariableDecl : 4 = 3.141592\n"
                  "aParameterizedDecl(that: 4)(currying: 4)(other: 4, sortOf: 4) : 4 { }\n");


    while(lexer.getNextToken().isNot(tok::eof))
        std::cout << lexer.getCurrentToken().prettyPrint() << "\n";

    return 0;
}
