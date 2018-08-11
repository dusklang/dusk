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
    Expr(ExprKind exprKind, Type type) : ASTNode(NodeKind::Expr), exprKind(exprKind), type(type) {}
    Expr(ExprKind exprKind) : ASTNode(NodeKind::Expr), exprKind(exprKind), type(ErrorTy()) {}
    virtual bool isMutable() const { return false; }
};

struct IntegerLiteralExpr: public Expr {
    SourceRange range;
    std::string literal;

    IntegerLiteralExpr(SourceRange range, std::string const& literal) :
        Expr(ExprKind::IntegerLiteral), range(range), literal(literal) {}
};

struct DecimalLiteralExpr: public Expr {
    SourceRange range;
    std::string literal;
    DecimalLiteralExpr(SourceRange range, std::string const& literal) : Expr(ExprKind::DecimalLiteral), range(range), literal(literal) {}
};

struct BooleanLiteralExpr: public Expr {
    SourceRange range;
    bool literal;
    BooleanLiteralExpr(SourceRange range, bool literal) : Expr(ExprKind::BooleanLiteral), range(range), literal(literal) {}
};

struct CharLiteralExpr: public Expr {
    SourceRange range;
    char literal;
    CharLiteralExpr(SourceRange range, char literal) : Expr(ExprKind::CharLiteral), range(range), literal(literal) {}
};

struct StringLiteralExpr: public Expr {
    SourceRange range;
    std::string literal;
    StringLiteralExpr(SourceRange range, std::string literal) : Expr(ExprKind::StringLiteral), range(range), literal(literal) {}
};

enum class BinOp {
    AddAssignment,
    SubAssignment,
    MultAssignment,
    DivAssignment,
    ModAssignment,
    AndAssignment,
    OrAssignment,
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
    BitwiseAnd,
    BitwiseOr,
    Assignment
};

enum class PreOp {
    Positive,
    Negative,
    Deref,
    AddrOf,
    Not
};

struct BinOpExpr: public Expr {
    Expr* lhs;
    Expr* rhs;
    BinOp op;

    BinOpExpr(SourceRange range, Expr* lhs, Expr* rhs, BinOp op) : Expr(ExprKind::BinOp), lhs(lhs), rhs(rhs), op(op) {}

    bool isMutable() const override { return lhs->isMutable(); }
};

struct PreOpExpr: public Expr {
    Expr* operand;
    PreOp op;

    PreOpExpr(SourceRange range, Expr* operand, PreOp op) : Expr(ExprKind::PreOp), operand(operand), op(op) {}
    bool isMutable() const override {
        switch(op) {
            case PreOp::Deref: return true;
            default: return false;
        }
    }
};

struct CastExpr: public Expr {
    Expr* operand;
    Type destType;

    CastExpr(SourceRange range, Expr* operand, Type destType) : Expr(ExprKind::Cast), operand(operand), destType(destType) {}
};

struct DeclRefExpr: public Expr {
    std::string name;
    std::vector<Expr*> argList;
    Decl* decl = nullptr;

    DeclRefExpr(SourceRange range, std::string name,
                std::vector<Expr*> const& argList) : Expr(ExprKind::DeclRef),
    name(name), argList(argList) {}

    ~DeclRefExpr() override;

    DeclRefExpr& operator=(DeclRefExpr const& other) = default;

    bool isMutable() const override;
};

struct MemberRefExpr: public Expr {
    Expr* root;
    std::string name;
    size_t declIndex = -1;

    MemberRefExpr(SourceRange range, Expr* root, std::string name) : Expr(ExprKind::MemberRef),
    root(root), name(name) {}

    MemberRefExpr& operator=(MemberRefExpr const& other) = default;

    bool isMutable() const override {
        return root->isMutable();
    }
};
