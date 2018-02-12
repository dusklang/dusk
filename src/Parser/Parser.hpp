//  Copyright © 2018 Zach Wolfe. All rights reserved.

#ifndef Parser_hpp
#define Parser_hpp

#include "llvm/ADT/Optional.h"

#include "Lexer.hpp"
#include "AST/AST.hpp"

class Parser {
private:
    Lexer lexer;

    llvm::Optional<Token> current() { return lexer.getCurrentToken(); }
    llvm::Optional<Token> next() { return lexer.getNextToken(); }
    llvm::Optional<std::string> parseIdentifer() {
        if(current() && current()->is(tok::identifier))
            return current()->getText();
        return llvm::None;
    }
    template<typename T>
    T getValueOrThrow(llvm::Optional<T> opt) {
        if(opt) return *opt;
        else throw "Exception";
    }
public:
    Parser(const std::string& source) : lexer(source) {}

    llvm::Optional<Decl> parseDecl();
    Lexer* getLexer() { return &lexer; }

};

#endif /* Parser_hpp */
