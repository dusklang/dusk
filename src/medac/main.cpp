//  Copyright © 2019 Zach Wolfe. All rights reserved.

#include <iostream>
#include <fstream>
#include <array>
#include "Parser/Parser.h"
#include "AST/ASTPrinter.h"
#include "Sema/TypeChecker.h"
#include "LLVMGen/LLVMGenerator.h"
#include "General/SourceInfo.h"
#include "LIR/LIRGenerator.h"
#include "General/General.h"
#include "General/Array.h"

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
    int32_t* someNumber = new int32_t;
    bool alreadyPrintedHenry = false;
}
)~";
std::string sourceCode = R"~(
extern def putchar(_: i32): void

def main {
    var number = 5
    def condition = number == number - 1
    putchar(number)
    number += number
    putchar(number)
}
/*
def main {
    printPerson(henry)
    printPerson(sally)
    printPerson(alexandra)

    def george = do {
        var george = henry
        george.name = "George"
        george.age = 72 as u8
        george.numberOfChildren = 1 as u8
        george
    }
    printPerson(george)

    printPerson(henry)

    do {
        printString("someNumber before: ")
        printInt(*someNumber)
        (*someNumber) |= 5
        printString("\nsomeNumber after: ")
        printInt(*someNumber)
        printChar("\n")
    }
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
}
def printInt(val: i32) {
    def printIntRecursively(val: i32) {
        if val == 0 { return }
        printIntRecursively(val / 10)
        printChar("0" + (val % 10) as i8)
    }
    if val == 0 {
        printChar("0")
    } else {
        printIntRecursively(val)
    }
}
def printString(str: *i8) {
    var curChar = str
    while *curChar != 0 as i8 {
        printChar(*curChar)
        curChar += 1
    }
}
def charToString(val: i8): *i8 {
    def buf = malloc(2 as u32) as *i8
    (*buf) = (val)
    (*(buf + 1)) = 0 as i8
    buf
}
def intToString(val: i32): *i8 {
    def intToStringRecursive(val: i32): *i8 {
        if val == 0 {
            ""
        } else {
            concat(
                intToStringRecursive(val / 10),
                charToString("0" + (val % 10) as i8)
            )
        }
    }
    if val == 0 {
        charToString("0")
    } else {
        intToStringRecursive(val)
    }
}
def printPerson(person: Person) {
    printString(
        concat(
            concat(
                concat(
                    concat(
                        concat(
                            concat(
                                person.name,
                                " is "
                            ),
                            if stringEquals(person.name, "Henry") && alreadyPrintedHenry {
                                "still "
                            } else {
                                ""
                            }
                        ),
                        intToString(person.age as i32)
                    ),
                    " years old and has "
                ),
                if person.numberOfChildren == 0 as u8 {
                    "no"
                } else if person.numberOfChildren == 1 as u8 {
                    "one"
                } else {
                    intToString(person.numberOfChildren as i32)
                }
            ),
            if person.numberOfChildren == 1 as u8 {
                " child.\n"
            } else {
                " children.\n"
            }
        )
    )
    if stringEquals(person.name, "Henry") {
        alreadyPrintedHenry = true
    }
}

def lengthOfString(str: *i8): i32 {
    var curChar = str
    var len = 0
    while *curChar != 0 as i8 {
        len += 1
        curChar += 1
    }
    len
}
def stringEquals(lhs: *i8, rhs: *i8): bool {
    var lhsChar = lhs
    var rhsChar = rhs

    while *lhsChar != 0 as i8 && *rhsChar != 0 as i8 {
        if *lhsChar != *rhsChar { return false }
        lhsChar += 1
        rhsChar += 1
    }
    return true
}
def concat(l: *i8, r: *i8): *i8 {
    def buf = do {
        def length = lengthOfString(l) + lengthOfString(r) + 1
        malloc(length as u32) as *i8
    }
    var destChar = buf
    var fromChar = l
    while *fromChar != 0 as i8 {
        *destChar = *fromChar
        destChar += 1
        fromChar += 1
    }
    fromChar = r
    while *fromChar != 0 as i8 {
        *(destChar) = *fromChar
        destChar += 1
        fromChar += 1
    }
    buf
}

struct Person {
    name: *i8
    age: u8
    numberOfChildren: u8
}
extern var henry: Person
extern var sally: Person
extern var alexandra: Person
extern var someNumber: *i32
extern var alreadyPrintedHenry: bool
extern def putchar(_: i32): void
extern def malloc(_: u32): *void
extern def free(_: *void): void

def printChar(character: i8) {
    putchar(character as i32)
}
 */
)~";

int main() {
    std::cout << "Meda compiler version 0.0.1\n";

    SourceFile file("main.meda", sourceCode);
    Parser parser(&file);
    TypeChecker tyChecker(file);
    LIRGenerator lirGen;
    ASTPrinter printer;

    auto nodes = parser.parseTopLevel();
    tyChecker.visitTopLevel(nodes);
    //printer.visit(nodes, 0, std::cout);


    lirGen.visit(nodes);
    lirGen.printIR();
/*
    LLVMGenerator llvmGen;
    llvmGen.visitTopLevel(nodes);
    llvmGen.printIR();
    llvmGen.outputObjectFile("main.o");

    std::ofstream stdLibFile;
    stdLibFile.open("stdlib.cpp");
    stdLibFile << standardLibrary;
    stdLibFile.close();

    std::cout << '\n';
    std::system("clang++ main.o stdlib.cpp -o main");
    std::system("./main");
    std::cout << '\n';*/

    return 0;
}
