//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <iostream>
#include "Parser/Lexer.hpp"

int main(int argc, const char * argv[]) {
    std::cout << "Dasquillette compiler version 0.0.1\n";
    Lexer lexer("main(argc: Int, argv: char**) : Int {\n"
                "\tmyFloat := 0.5\n"
                "\tmyFloat = 5.4.3\n" // Should cause an assert to fail.
                "\treturn 0\n"
                "}");

    while(auto Tok = lexer.nextToken()) {
        std::cout << Tok->getText() << "\n";
    }

    return 0;
}
