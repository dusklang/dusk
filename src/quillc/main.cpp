//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <iostream>
#include "Parser/Parser.hpp"

int main(int argc, const char * argv[]) {
    std::cout << "Dasquillette compiler version 0.0.1\n";
    Parser parser("multiply(lhs: Int, rhs: Int) : Int\n"
                  "aVariableDecl : Double\n"
                  "aParameterizedDecl(that: Allows)(currying: Into)(other: Functions, sortOf: AintThatCool) : QuestionMark");

    parser.getLexer()->getNextToken();
    while(auto decl = parser.parseDecl()) {
        std::cout << decl->prettyPrint() << "\n";
        parser.getLexer()->getNextToken();
    }


    return 0;
}
