//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <iostream>
#include "Parser/Lexer.hpp"

int main(int argc, const char * argv[]) {
    std::cout << "Dasquillette compiler version 0.0.1\n";
    Lexer lexer("main(argc: Int, argv: char**) : Int {\n"
                "myFloat := 0.5 // a single-line comment!\n"
                "/* This is a /* nested /* multi-\nline /* \ncomment */ */ /*with another nest */ */ */"
                "// followed immediately by another single-line comment\n"
                "myFloat = 5.4\n"
                "return 0\n"
                "}");

    while(auto Tok = lexer.nextToken()) {
        std::cout << Tok->prettyPrint() << "\n";
    }

    return 0;
}
