//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <iostream>
#include "Parser/Parser.hpp"

int main(int argc, const char * argv[]) {
    std::cout << "Quill compiler version 0.0.1\n\n";
    Parser parser("helloThere(hello: 4) : 4 {}");

//    while(lexer.nextTok().isNot(tok::eof)) {
//        std::cout << lexer.curTok().prettyPrint() << "\n";
//    }
    if(auto scope = parser.parseScope()) {
        std::cout << (*scope)->prettyPrint() << '\n';
    } else {
        std::cout << scope.failure() << "\n\n";
    }

    return 0;
}
