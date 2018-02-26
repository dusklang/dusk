//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <memory>
#include "llvm/ADT/SmallVector.h"

#include "AST.h"
#include "Decl.h"

enum class ExprKind {
    #define EXPR_NODE(name) name,
    #include "ASTNodes.def"
    NUM_EXPRS
};

// Base class from which each Expression node inherits.
struct Expr : public ASTNode {
    ExprKind exprKind;
    llvm::Optional<BuiltinType> type;
    AST_NODE_CTOR(Expr, ExprKind exprKind, BuiltinType type), exprKind(exprKind), type(type) {}
    AST_NODE_CTOR(Expr, ExprKind exprKind), exprKind(exprKind) {}
};

#define EXPR_CTOR(name, args...) name##Expr(SourceRange range, args) : Expr(range, ExprKind::name)
struct IntegerLiteralExpr: public Expr {
    std::string literal;

    EXPR_CTOR(IntegerLiteral, const std::string& literal), literal(literal) {}
};

struct DecimalLiteralExpr: public Expr {
    std::string literal;
    EXPR_CTOR(DecimalLiteral, const std::string& literal), literal(literal) {}
};

struct BooleanLiteralExpr: public Expr {
    bool literal;
    EXPR_CTOR(BooleanLiteral, bool literal), literal(literal) {}
};

struct DeclRefExpr: public Expr {
    std::string name;
    std::vector<Argument> argList;
    std::shared_ptr<Decl> decl = nullptr;

    EXPR_CTOR(DeclRef, const std::string& name,
              const std::vector<Argument>& argList),
    name(name), argList(argList) {}

    void operator=(const DeclRefExpr& other) {
        range = other.range;
        type = other.type;
        name = other.name;
        argList = other.argList;
    }
};

#undef EXPR_CTOR
