//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <iostream>
#include "Parser/Parser.hpp"
#include "AST/ASTPrinter.hpp"

std::string sourceCode = R"~(
extern sin(val: __builtin_double): __builtin_double
main(someParams: __builtin_int): {
    mut myVal := 4.0
}
)~";

int main(int argc, const char * argv[]) {
    std::cout << "Quill compiler version 0.0.1\n\n";
    Parser parser(sourceCode);

//    while(lexer.nextTok().isNot(tok::eof)) {
//        std::cout << lexer.curTok().prettyPrint() << "\n";
//    }
    ASTPrinter printer;
    if(auto scope = parser.parseTopLevel()) {
        for(auto& node: *scope) {
            std::cout << printer.visit(node.get(), 0) << '\n';
        }
    } else {
        std::cout << scope.failure() << "\n";
    }
    std::cout << std::endl;

    return 0;
}
