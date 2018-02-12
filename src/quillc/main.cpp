//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <iostream>
#include "Parser/Lexer.hpp"

int main(int argc, const char * argv[]) {
    std::cout << "Dasquillette compiler version 0.0.1\n";
    Lexer lexer(" } : { (     )\n\n\r     \r   \n  = ");

    while(auto Tok = lexer.nextToken()) {
        std::cout << Tok->getText() << "\n";
    }

    return 0;
}
