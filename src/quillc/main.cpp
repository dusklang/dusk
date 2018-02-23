//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <iostream>
#include "Parser/Parser.h"
#include "AST/ASTPrinter.h"
#include "Sema/TypeChecker.h"
#include "IRGen/CodeGenerator.h"
#include "llvm/Support/raw_ostream.h"

std::string sourceCode = R"~(
extern def doSomethingReallyCool: Void
def abs(x: i32): i32 {
    return 4
}
def main: i32 {
    doSomethingReallyCool
    return abs(x: 32)
}
)~";

int main(int argc, const char * argv[]) {
    std::cout << "Quill compiler version 0.0.1\n\n";
    Parser parser(sourceCode);

    ASTPrinter printer;
    TypeChecker tyChecker;
    CodeGenerator codeGenerator;
    auto file = parser.parseTopLevel();
//    for(auto& node: file) {
//        std::cout << printer.visit(node, 0) << '\n';
//    }
    for(auto& node: file) {
        tyChecker.visit(node);
    }
    for(auto& node: file) {
        codeGenerator.visit(node);
    }
    codeGenerator.module->print(llvm::errs(), nullptr);
    std::cout << std::endl;

    return 0;
}
