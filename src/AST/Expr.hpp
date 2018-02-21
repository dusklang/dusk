//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <memory>
#include "llvm/ADT/SmallVector.h"

#include "AST.hpp"
#include "Decl.hpp"

enum class ExprKind {
    #define EXPR_NODE(name) name,
    #include "ASTNodes.def"
    NUM_EXPRS
};

// Base class from which each Expression node inherits.
struct Expr : public ASTNode {
    ExprKind exprKind;
    AST_NODE_CONSTRUCTOR(Expr, ExprKind exprKind), exprKind(exprKind) {}
};

#define EXPR_CONSTRUCTOR(name, args...) name##Expr(args) : Expr(ExprKind::name)
struct IntegerLiteralExpr: public Expr {
    std::string literal;

    EXPR_CONSTRUCTOR(IntegerLiteral, const std::string& literal), literal(literal) {}
};

struct DecimalLiteralExpr: public Expr {
    std::string literal;
    EXPR_CONSTRUCTOR(DecimalLiteral, const std::string& literal), literal(literal) {}
};

struct DeclRefExpr: public Expr {
    std::string name;
    llvm::SmallVector<Argument, 2> argList;

    EXPR_CONSTRUCTOR(DeclRef, const std::string& name,
                     const llvm::SmallVector<Argument, 2>& argList),
                    name(name), argList(argList) {}

    void operator=(const DeclRefExpr& other) {
        name = other.name;
        argList = other.argList;
    }
};

struct TypeRefExpr: public Expr {
private:
    enum {
        inferred,
        referenced
    } tag;
    union {
        DeclRefExpr expr;
    };
public:
    EXPR_CONSTRUCTOR(TypeRef), tag(inferred) {}
    EXPR_CONSTRUCTOR(TypeRef, const DeclRefExpr& expr), tag(referenced), expr(expr) {}
    EXPR_CONSTRUCTOR(TypeRef, const TypeRefExpr& other), tag(other.tag) {
        if(tag == referenced) expr = other.expr;
    }
    ~TypeRefExpr() {}
    void operator=(const TypeRefExpr& other) {
        tag = other.tag;
        if(tag == referenced) expr = other.expr;
    }

    const DeclRefExpr& asDeclRef() const {
        assert(tag == referenced);
        return expr;
    }

    bool isInferred() const { return tag == inferred; }
};

#undef EXPR_CONSTRUCTOR
