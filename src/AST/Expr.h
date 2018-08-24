//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <utility> // for std::pair
#include <optional>

#include "AST.h"
#include "Type.h"
#include "Ident.h"

struct Decl;

enum class ExprKind {
    #define EXPR_NODE(name) name,
    #include "ASTNodes.def"
    NUM_EXPRS
};

// TODO: Make Expr a variant.
/// Base class from which each Expression node inherits.
struct Expr : public ASTNode {
    ExprKind exprKind;
    Type type;
    Expr(ExprKind exprKind, Type type) : ASTNode(NodeKind::Expr), exprKind(exprKind), type(type) {}
    Expr(ExprKind exprKind) : ASTNode(NodeKind::Expr), exprKind(exprKind), type(ErrorTy()) {}
    virtual bool isMutable() const { return false; }
    virtual SourceRange totalRange() const = 0;
};

struct IntegerLiteralExpr final: public Expr {
    SourceRange range;
    std::string literal;

    IntegerLiteralExpr(SourceRange range, std::string const& literal) :
        Expr(ExprKind::IntegerLiteral), range(range), literal(literal) {}

    SourceRange totalRange() const override { return range; }
};

struct DecimalLiteralExpr final: public Expr {
    SourceRange range;
    std::string literal;
    DecimalLiteralExpr(SourceRange range, std::string const& literal) : Expr(ExprKind::DecimalLiteral), range(range), literal(literal) {}

    SourceRange totalRange() const override { return range; }
};

struct BooleanLiteralExpr final: public Expr {
    SourceRange range;
    bool literal;
    BooleanLiteralExpr(SourceRange range, bool literal) : Expr(ExprKind::BooleanLiteral), range(range), literal(literal) {}

    SourceRange totalRange() const override { return range; }
};

struct CharLiteralExpr final: public Expr {
    SourceRange range;
    char literal;
    CharLiteralExpr(SourceRange range, char literal) : Expr(ExprKind::CharLiteral), range(range), literal(literal) {}

    SourceRange totalRange() const override { return range; }
};

struct StringLiteralExpr final: public Expr {
    SourceRange range;
    std::string literal;
    StringLiteralExpr(SourceRange range, std::string literal) : Expr(ExprKind::StringLiteral), range(range), literal(literal) {}

    SourceRange totalRange() const override { return range; }
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
    SourceRange opRange;
    Expr* lhs;
    Expr* rhs;
    BinOp op;

    BinOpExpr(SourceRange opRange, Expr* lhs, Expr* rhs, BinOp op) : Expr(ExprKind::BinOp), opRange(opRange), lhs(lhs), rhs(rhs), op(op) {}

    bool isMutable() const override { return lhs->isMutable(); }

    SourceRange totalRange() const override { return lhs->totalRange() + rhs->totalRange(); }
};

struct PreOpExpr: public Expr {
    SourceRange opRange;
    Expr* operand;
    PreOp op;

    PreOpExpr(SourceRange opRange, Expr* operand, PreOp op) : Expr(ExprKind::PreOp), opRange(opRange), operand(operand), op(op) {}
    bool isMutable() const override {
        switch(op) {
            case PreOp::Deref: return true;
            default: return false;
        }
    }

    SourceRange totalRange() const override { return opRange + operand->totalRange(); }
};

struct CastExpr final: public Expr {
    SourceRange asRange;
    Expr* operand;
    Type destType;

    CastExpr(SourceRange asRange, Expr* operand, Type destType) : Expr(ExprKind::Cast), asRange(asRange), operand(operand), destType(destType) {}

    SourceRange totalRange() const override { return operand->totalRange() + destType.range; }
};

struct DeclRefExpr final: public Expr {
    std::optional<std::pair<SourceRange, SourceRange>> parenRanges;
    Ident name;
    std::vector<Expr*> argList;
    Decl* decl = nullptr;

    DeclRefExpr(std::optional<std::pair<SourceRange, SourceRange>> parenRanges, Ident name, std::vector<Expr*> argList) :
        Expr(ExprKind::DeclRef), parenRanges(parenRanges), name(name), argList(argList) {}

    ~DeclRefExpr() override;

    DeclRefExpr& operator=(DeclRefExpr const& other) = default;

    bool isMutable() const override;

    SourceRange totalRange() const override {
        SourceRange range = name.range;
        if(parenRanges) {
            range += parenRanges->second;
        }
        return range;
    }
};

struct MemberRefExpr final: public Expr {
    SourceRange dotRange;
    Expr* root;
    Ident name;
    size_t declIndex = -1;

    MemberRefExpr(SourceRange dotRange, Expr* root, Ident name) :
        Expr(ExprKind::MemberRef), dotRange(dotRange), root(root), name(name) {}

    MemberRefExpr& operator=(MemberRefExpr const& other) = default;

    bool isMutable() const override {
        return root->isMutable();
    }

    SourceRange totalRange() const override {
        return root->totalRange() + name.range;
    }
};

struct ReturnExpr: public Expr {
    SourceRange returnRange;
    Expr* value;

    ReturnExpr(SourceRange returnRange, Expr* value) : Expr(ExprKind::Return), returnRange(returnRange), value(value) {}

    ~ReturnExpr() override;

    SourceRange totalRange() const override {
        auto range = returnRange;
        if(value) range += value->totalRange();
        return range;
    }
};

struct IfExpr: public Expr {
    SourceRange ifRange;
    Expr* condition;
    Scope* thenScope;
    std::optional<std::variant<Scope*, IfExpr*>> elseNode;

    IfExpr(SourceRange ifRange, Expr* condition, Scope* thenScope, std::optional<std::variant<Scope*, IfExpr*>> elseNode) :
    Expr(ExprKind::If), ifRange(ifRange), condition(condition), thenScope(thenScope), elseNode(elseNode) {}

    ~IfExpr() override;

    SourceRange totalRange() const override;
};

struct WhileExpr: public Expr {
    SourceRange whileRange;
    Expr* condition;
    Scope* thenScope;

    WhileExpr(SourceRange whileRange, Expr* condition, Scope* thenScope) :
        Expr(ExprKind::While), whileRange(whileRange), condition(condition), thenScope(thenScope) {}

    ~WhileExpr() override;

    SourceRange totalRange() const override {
        return whileRange + condition->totalRange() + thenScope->range;
    }
};
