//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <iostream>
#include <fstream>
#include "Parser/Parser.h"
#include "AST/ASTPrinter.h"
#include "Sema/TypeChecker.h"
#include "IRGen/CodeGenerator.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetRegistry.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/IR/LegacyPassManager.h"

std::string standardLibrary = R"~(
#include <iostream>
extern "C" {
    int addInt(int l, int r) {
        return l + r;
    }
    double addDouble(double l, double r) {
        return l + r;
    }
    void printInt(int x) {
        std::cout << "Printing int from Quill: " << x << '\n';
    }
    void printDouble(double x) {
        std::cout << "Printing float from Quill: " << x << '\n';
    }
    void printBool(bool x) {
        std::cout << "Print bool from Quill: " << (x ? "true": "false") << '\n';
    }
}
)~";
std::string sourceCode = R"~(
extern def addInt(l: i32, r: i32): i32
extern def addDouble(l: f64, r: f64): f64
extern def printInt(x: i32): Void
extern def printDouble(x: f64): Void
extern def printBool(x: Bool): Void
def main {
    printBool(x: true)
    printBool(x: false)
    printBool(x: true)
    var myVar = 1
    printInt(x: myVar) // 1
    myVar = addInt(l: myVar, r: 1)
    printInt(x: myVar) // 2
    var myFloatVar = 2.0
    printDouble(x: myFloatVar) // 2.0
    myFloatVar = addDouble(l: myFloatVar, r: 0.1)
    printDouble(x: myFloatVar) // 2.1
    printInt(x: myVar) // Still 2
    return
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

    // Initialize the target registry etc.
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetMCs();
    llvm::InitializeAllAsmParsers();
    llvm::InitializeAllAsmPrinters();

    auto triple = llvm::sys::getDefaultTargetTriple();
    codeGenerator.module->setTargetTriple(triple);

    std::string error;
    auto target = llvm::TargetRegistry::lookupTarget(triple, error);

    // Print an error and exit if we couldn't find the requested target.
    // This generally occurs if we've forgotten to initialise the
    // TargetRegistry or we have a bogus target triple.
    if (!target) {
        llvm::errs() << error;
        return 1;
    }

    auto cpu = "generic";

    llvm::TargetOptions opt;
    auto relocationModel = llvm::Optional<llvm::Reloc::Model>();
    auto machine =
    target->createTargetMachine(triple, cpu, /*features=*/ "", opt, relocationModel);

    codeGenerator.module->setDataLayout(machine->createDataLayout());

    auto fileName = "main.o";
    std::error_code errorCode;
    llvm::raw_fd_ostream dest(fileName, errorCode, llvm::sys::fs::F_None);

    if(errorCode) {
        llvm::errs() << "Could not open file: " << errorCode.message();
        return 1;
    }

    llvm::legacy::PassManager pass;
    auto fileType = llvm::TargetMachine::CGFT_ObjectFile;

    if(machine->addPassesToEmitFile(pass, dest, fileType)) {
        llvm::errs() << "Machine can't emit object files.";
        return 1;
    }

    auto& module = *codeGenerator.module;
    // TODO: Detect computed decls that don't return a value (TODO is placed here because this line
    // fails when the code doesn't have a return). I'm going to wait on this at least until we have if
    // statements.
    pass.run(module);
    dest.flush();

    std::ofstream stdLibFile;
    stdLibFile.open("stdlib.cpp");
    stdLibFile << standardLibrary;
    stdLibFile.close();

    std::cout << '\n';
    std::system("clang++ main.o stdlib.cpp -o main");
    std::system("./main");
    std::cout << '\n';

    return 0;
}
