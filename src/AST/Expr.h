//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>

#include "AST.h"
#include "Decl.h"

struct Decl;

enum class ExprKind {
    #define EXPR_NODE(name) name,
    #include "ASTNodes.def"
    NUM_EXPRS
};

// Base class from which each Expression node inherits.
struct Expr : public ASTNode {
    ExprKind exprKind;
    Type type;
    Expr(SourceRange range, ExprKind exprKind, Type type) : ASTNode(NodeKind::Expr, range), exprKind(exprKind), type(type) {}
    Expr(SourceRange range, ExprKind exprKind) : ASTNode(NodeKind::Expr, range), exprKind(exprKind), type(Type::Error()) {}
};

struct IntegerLiteralExpr: public Expr {
    std::string literal;

    IntegerLiteralExpr(SourceRange range, std::string const& literal) :
    Expr(range, ExprKind::IntegerLiteral), literal(literal) {}
};

struct DecimalLiteralExpr: public Expr {
    std::string literal;
    DecimalLiteralExpr(SourceRange range, std::string const& literal) : Expr(range, ExprKind::DecimalLiteral), literal(literal) {}
};

struct BooleanLiteralExpr: public Expr {
    bool literal;
    BooleanLiteralExpr(SourceRange range, bool literal) : Expr(range, ExprKind::BooleanLiteral), literal(literal) {}
};

struct CharLiteralExpr: public Expr {
    char literal;
    CharLiteralExpr(SourceRange range, char literal) : Expr(range, ExprKind::CharLiteral), literal(literal) {}
};

struct StringLiteralExpr: public Expr {
    std::string literal;
    StringLiteralExpr(SourceRange range, std::string literal) : Expr(range, ExprKind::StringLiteral), literal(literal) {}
};

struct DeclRefExpr: public Expr {
    std::string name;
    std::vector<Argument> argList;
    Decl* decl = nullptr;

    DeclRefExpr(SourceRange range, std::string name,
                std::vector<Argument> const& argList) : Expr(range, ExprKind::DeclRef),
    name(name), argList(argList) {}

    ~DeclRefExpr() override { delete decl; }

    DeclRefExpr& operator=(DeclRefExpr const& other) = default;
};
