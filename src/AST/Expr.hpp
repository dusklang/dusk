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
    void operator=(const DeclRefExpr& other) {
        name = other.name;
        argLists = other.argLists;
    }

    std::string prettyPrint(int indentationLevel = -1) const override;
};

struct TypeExpr: public Expr {
private:
    enum {
        inferred,
        referenced
    } tag;
    union {
        DeclRefExpr expr;
    };
public:
    TypeExpr() : tag(inferred) {}
    TypeExpr(const DeclRefExpr& expr) : tag(referenced), expr(expr) {}
    TypeExpr(const TypeExpr& other) : tag(other.tag) {
        if(tag == referenced) expr = other.expr;
    }
    ~TypeExpr() {}
    void operator=(const TypeExpr& other) {
        tag = other.tag;
        if(tag == referenced) expr = other.expr;
    }

    const DeclRefExpr& asDeclRef() const {
        assert(tag == referenced);
        return expr;
    }

    bool isInferred() const { return tag == inferred; }
    std::string prettyPrint(int indentationLevel = -1) const override {
        return tag == inferred ? "<inferred>" : expr.prettyPrint();
    }
};
