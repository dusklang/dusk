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
    int add(int l, int r) {
        return l + r;
        std::cout << l + r << "\n";
    }
    int sub(int l, int r) {
        return l - r;
    }
    bool eq(int l, int r) {
        return l == r;
    }
    bool lte(int l, int r) {
        return l <= r;
    }
    void print(int x) {
        std::cout << "Printing int from Quill: " << x << '\n';
    }
}
)~";
std::string sourceCode = R"~(
extern def add(l: i32, r: i32): i32
extern def lte(l: i32, r: i32): Bool
extern def print(x: i32): Void
def main {
    // Print numbers from 1 to 100.
    var i = 1
    while lte(l: i, r: 100) {
        print(x: i)
        i = add(l: i, r: 1)
    }

    return
}
)~";

int main() {
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
