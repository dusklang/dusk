//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <iostream>
#include <fstream>
#include "Parser/Parser.h"
#include "AST/ASTPrinter.h"
#include "Sema/NameResolver.h"
#include "Sema/ConstraintGenerator.h"
#include "Sema/ConstraintSolver.h"
#include "Sema/SolutionApplier.h"
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
    int add(int l, int r) { return l + r; }
    int mod(int l, int r) { return l % r; }
    int sub(int l, int r) { return l - r; }
    bool eq(int l, int r) { return l == r; }
    bool lte(int l, int r) { return l <= r; }
    bool notTrue(bool x) { return !x; }
    bool bothTrue(bool l, bool r) { return l && r; }
    void printInt(int x) { std::cout << x; }
    void printChar(char x) { std::cout << x; }
    bool charEq(char l, char r) { return l == r; }
    char charDeref(char* character) { return *character; }
    char* charPtrAdd(char* ptr, int advance) { return ptr + advance; }
}
)~";
std::string sourceCode = R"~(
extern def add(_: i32, _: i32): i32
extern def mod(_: i32, _: i32): i32
extern def lte(_: i32, _: i32): bool
extern def eq(_: i32, _: i32): bool
extern def notTrue(_: bool): bool
extern def bothTrue(_: bool, _: bool): bool
extern def printInt(_: i32): void
extern def printChar(_: i8): void
extern def charEq(_: i8, _: i8): bool
extern def charDeref(_: *i8): i8
extern def charPtrAdd(_: *i8, _: i32): *i8
def printString(str: *i8) {
    var curChar = str
    while notTrue(charEq(charDeref(curChar), "\0")) {
        printChar(charDeref(curChar))
        curChar = charPtrAdd(curChar, 1)
    }
    return
}
def performFizzBuzz(end: i32) {
    // Check range.
    if notTrue(lte(0, end)) {
        printString("Invalid bounds!\n")
        return
    }
    var i: i32 = 0
    while lte(i, end) {
        def fizz = eq(mod(i, 3), 0)
        def buzz = eq(mod(i, 5), 0)
        if fizz { printString("Fizz") }
        if buzz { printString("Buzz") }
        if bothTrue(notTrue(fizz), notTrue(buzz)) { printInt(i) }
        printChar("\n")
        i = add(i, 1)
    }
    return
}
def main {
    performFizzBuzz(1000)

    return
}
)~";

int main() {
    std::cout << "Meda compiler version 0.0.1\n\n";
    Parser parser(sourceCode);

    ASTPrinter printer;
    NameResolver nameResolver;
    ConstraintGenerator constraintGen;
    CodeGenerator codeGenerator;

    auto file = parser.parseTopLevel();
    for(auto node: file) {
        nameResolver.visit(node);
    }
    for(auto node: file) {
        constraintGen.visit(node);
    }
    auto solution = solveSystem(constraintGen.constraints);
    if(!solution) {
        std::cout << "Failed to solve system of constraints.\n";
        exit(0);
    }
    SolutionApplier solutionApplier { *solution };
    for(auto node: file) {
        solutionApplier.visit(node);
    }
    for(auto node: file) {
        printer.visit(node, 0, std::cout);
        std::cout << '\n';
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
    auto relocationModel = Optional<llvm::Reloc::Model>();
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
