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
    struct Person {
        const char* name;
        uint8_t age;
        uint8_t numberOfChildren;
    };
    Person henry = {
        "Henry",
        42,
        4
    };
    Person sally = {
       "Sally",
       25,
       1
    };
    Person alexandra = {
        "Alexandra",
        20,
        0
    };
    int32t_t* someNumber = new int32_t;
}
)~";
std::string sourceCode = R"~(

struct Person {
    var name: *i8
    var age: u8
    var numberOfChildren: u8
}
extern var henry: Person
extern var sally: Person
extern var alexandra: Person
extern var someNumber: *i32

extern def putchar(_: i32): void
def printChar(character: i8) {
    putchar(character as i32)
    return
}

def printString(str: *i8) {
    var curChar = str
    while *curChar != "\0" {
        printChar(*curChar)
        curChar += 1
    }
    return
}
def printIntRecursively(val: i32) {
    if val == 0 {
        return
    } else {
        printIntRecursively(val / 10)
        printChar("0" + (val % 10) as i8)
        return
    }
}
def printInt(val: i32) {
    if val == 0 {
        printChar("0")
    } else {
        printIntRecursively(val)
    }
    return
}
def performFizzBuzz(end: i32) {
    // Check range.
    if end < 0 {
        printString("Invalid bounds!\n")
        return
    }
    var i = 0
    while i <= end {
        def fizz = i % 3 == 0
        def buzz = i % 5 == 0
        if fizz { printString("Fizz") }
        if buzz { printString("Buzz") }
        if !fizz && !buzz { printInt(i) }
        printChar("\n")
        i += 1
    }
    return
}
def printPerson(person: Person) {
    printString(person.name)
    printString(" is ")
    printInt(person.age as i32)
    printString(" years old and has ")
    if person.numberOfChildren == 0 as u8 {
        printString("no")
    } else {
        printInt(person.numberOfChildren as i32)
    }
    if person.numberOfChildren == 1 as u8 {
        printString(" child.\n")
    } else {
        printString(" children.\n")
    }

    return
}
def main {
    printPerson(henry)
    printPerson(sally)
    printPerson(alexandra)

    var george = henry
    george.name = "George"
    george.age = 72 as u8
    george.numberOfChildren = 1 as u8
    printPerson(george)

    printPerson(henry)

    printString("someNumber before: ")
    printInt(*someNumber)
    (*someNumber) += 5
    printString("\nsomeNumber after: ")
    printInt(*someNumber)
    printChar("\n")

    return
}
)~";

int main() {
    std::cout << "Meda compiler version 0.0.1\n\n";

    Parser parser(sourceCode);
    ASTPrinter printer;
    TypeChecker tyChecker;
    CodeGenerator codeGenerator;

    auto file = parser.parseTopLevel();
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
