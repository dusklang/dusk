//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <iostream>
#include "Parser/Parser.hpp"

int main(int argc, const char * argv[]) {
    std::cout << "Quill compiler version 0.0.1\n\n";
    Parser parser("multiply(lhs: Int(ohWowTypeParameters: 75), rhs: Int2) : Int3 { hello: Int { hiThereIndeed(wow: 56) : t { again }  } }\n"
                  "aVariableDecl : Double = 3.141592\n"
                  "aParameterizedDecl(that: Allows)(currying: Into)(other: Functions, sortOf: AintThatCool) : QuestionMark { currying }\n"
                  "multiply(lhs: 4, rhs: 5)\n"
                  "aVariableDeclRefWithNoParam\n"
                  "aVariableDeclRef(withParam: 4)");

    parser.getLexer()->getNextToken();
    if(auto scope = parser.parseScope())
        std::cout << scope->prettyPrint() << "\n\n";

    return 0;
}
