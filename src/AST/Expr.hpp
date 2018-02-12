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

    std::string prettyPrint() const override;
};

struct DecimalLiteralExpr: public Expr {
    std::string literal;
    DecimalLiteralExpr(const std::string& literal): literal(literal) {}

    std::string prettyPrint() const override;
};

struct DeclRefExpr: public Expr {
    DeclPrototype prototype;

    DeclRefExpr(DeclPrototype prototype) : prototype(prototype) {}

    std::string prettyPrint() const override;
};
