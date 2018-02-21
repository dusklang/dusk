//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <iostream>
#include "Parser/Parser.hpp"
#include "AST/ASTPrinter.hpp"

std::string sourceCode = R"~(
main : Int = 4
main(arg: 75, secondArg: wow)
main(someParams: Array): Void {
    hello := 435
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
