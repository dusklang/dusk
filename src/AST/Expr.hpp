//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <memory>
#include "llvm/ADT/SmallVector.h"

#include "AST.hpp"
#include "Decl.hpp"

// Base class from which each Expression node inherits.
struct Expr : public ASTNode {};

struct IntegerLiteralExpr: public Expr {
    std::string literal;

    IntegerLiteralExpr(const std::string& literal): literal(literal) {}

    std::string prettyPrint(int indentationLevel = -1) const override;
};

struct DecimalLiteralExpr: public Expr {
    std::string literal;
    DecimalLiteralExpr(const std::string& literal): literal(literal) {}

    std::string prettyPrint(int indentationLevel = -1) const override;
};

struct DeclRefExpr: public Expr {
    std::string name;
    llvm::SmallVector<llvm::SmallVector<Argument, 2>, 1> argLists;

    DeclRefExpr(const std::string& name,
                const llvm::SmallVector<llvm::SmallVector<Argument, 2>, 1>& argLists)
    : name(name), argLists(argLists) {}

    std::string prettyPrint(int indentationLevel = -1) const override;
};
