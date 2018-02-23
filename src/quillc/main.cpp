//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <iostream>
#include "Parser/Parser.hpp"
#include "AST/ASTPrinter.hpp"
#include "IRGen/CodeGenerator.hpp"
#include "llvm/Support/raw_ostream.h"

std::string sourceCode = R"~(
def main: i32 {
    var something = 3
}
var myVar = 32003.3
)~";

int main(int argc, const char * argv[]) {
    std::cout << "Quill compiler version 0.0.1\n\n";
    Parser parser(sourceCode);

    ASTPrinter printer;
    CodeGenerator codeGenerator;
    auto file = parser.parseTopLevel();
    /*for(auto& node: file) {
        std::cout << printer.visit(node.get(), 0) << '\n';
    }*/
    for(auto& node: file) {
        codeGenerator.visit(node.get());
    }
    codeGenerator.module->print(llvm::errs(), nullptr);
    std::cout << std::endl;

    return 0;
}
