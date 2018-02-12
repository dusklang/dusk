//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <iostream>
#include "Parser/Parser.hpp"

int main(int argc, const char * argv[]) {
    std::cout << "Dasquillette compiler version 0.0.1\n";
    Parser parser("multiply(lhs: Int , rhs: Int2) : Int3 : 5\n"
                  "aVariableDecl : Double = 3.141592\n"
                  "aParameterizedDecl(that: Allows)(currying: Into)(other: Functions, sortOf: AintThatCool) : QuestionMark : q");

    parser.getLexer()->getNextToken();
    while(auto node = parser.parseNode()) {
        std::cout << node->prettyPrint() << "\n";
        parser.getLexer()->getNextToken();
    }

    return 0;
}
