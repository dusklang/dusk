//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <iostream>
#include "Parser/Lexer.hpp"

int main(int argc, const char * argv[]) {
    std::cout << "Dasquillette compiler version 0.0.1\n";
    Lexer lexer("main(argc: Int, argv: char**) : Int {\n"
                "\treturn 759\n"
                "}");

    while(auto Tok = lexer.nextToken()) {
        std::cout << Tok->getText() << "\n";
    }

    return 0;
}
