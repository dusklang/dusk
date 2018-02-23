//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <iostream>
#include "Parser/Parser.h"
#include "AST/ASTPrinter.h"
#include "Sema/TypeChecker.h"
#include "IRGen/CodeGenerator.h"
#include "llvm/Support/raw_ostream.h"

std::string sourceCode = R"~(
def abs(x: i32): i32 {
    return 4
}
def main {
    abs(x: 32.1)
}
)~";

int main(int argc, const char * argv[]) {
    std::cout << "Quill compiler version 0.0.1\n\n";
    Parser parser(sourceCode);

    ASTPrinter printer;
    TypeChecker tyChecker;
    CodeGenerator codeGenerator;
    auto file = parser.parseTopLevel();
    /*for(auto& node: file) {
        std::cout << printer.visit(node.get(), 0) << '\n';
    }*/
    for(auto& node: file) {
        tyChecker.visit(node.get());
    }
    for(auto& node: file) {
        codeGenerator.visit(node.get());
    }
    codeGenerator.module->print(llvm::errs(), nullptr);
    std::cout << std::endl;

    return 0;
}
