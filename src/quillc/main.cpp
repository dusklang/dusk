//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <iostream>
#include "Parser/Lexer.hpp"

int main(int argc, const char * argv[]) {
    std::cout << "Dasquillette compiler version 0.0.1\n";
    Lexer lexer("main(argc: Int, argv: char**) : Int {\n"
                "myFloat := 0.5 // a comment!\n    \n // another comment!\r\n// yet another comment\n"
                "myFloat = 5.4\n"
                "return 0\n"
                "}");

    while(auto Tok = lexer.nextToken()) {
        std::cout << Tok->prettyPrint() << "\n";
    }

    return 0;
}
