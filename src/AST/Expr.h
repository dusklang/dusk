//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>

#include "AST.h"
#include "Type.h"

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

enum class BinOp {
    AddAssignment,
    SubAssignment,
    MultAssignment,
    DivAssignment,
    ModAssignment,
    Add,
    Sub,
    Mult,
    Div,
    Mod,
    Equal,
    NotEqual,
    LessThanOrEqual,
    LessThan,
    GreaterThanOrEqual,
    GreaterThan,
    Or,
    And,
    Assignment
};

enum class PreOp {
    Positive,
    Negative,
    Deref,
    Not
};

struct BinOpExpr: public Expr {
    Expr* lhs;
    Expr* rhs;
    BinOp op;

    BinOpExpr(SourceRange range, Expr* lhs, Expr* rhs, BinOp op) : Expr(range, ExprKind::BinOp), lhs(lhs), rhs(rhs), op(op) {}
};

struct PreOpExpr: public Expr {
    Expr* operand;
    PreOp op;

    PreOpExpr(SourceRange range, Expr* operand, PreOp op) : Expr(range, ExprKind::PreOp), operand(operand), op(op) {}
};

struct CastExpr: public Expr {
    Expr* operand;
    Type destType;

    CastExpr(SourceRange range, Expr* operand, Type destType) : Expr(range, ExprKind::Cast), operand(operand), destType(destType) {}
};

struct DeclRefExpr: public Expr {
    std::string name;
    std::vector<Expr*> argList;
    Decl* decl = nullptr;

    DeclRefExpr(SourceRange range, std::string name,
                std::vector<Expr*> const& argList) : Expr(range, ExprKind::DeclRef),
    name(name), argList(argList) {}

    ~DeclRefExpr() override;

    DeclRefExpr& operator=(DeclRefExpr const& other) = default;
};
