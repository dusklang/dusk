//  Copyright © 2019 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <utility> // for std::pair
#include <optional>
#include <variant>

#include "AST.h"
#include "Type.h"
#include "Ident.h"

struct Decl;

// TODO: Make Expr a variant.
/// Base class from which each Expression node inherits.
struct Expr: public ASTNode {
    Type type;
    Expr(Type type) : type(type) {}
    Expr() : type(ErrorTy()) {}
    virtual bool isMutable() const { return false; }
};

struct IntegerLiteralExpr final: public Expr {
    SourceRange range;
    /// The value of the integer literal encoded as a 64-bit signed or unsigned integer.
    ///
    /// TODO: Make a BigInt type to represent this instead.
    uint64_t literal;

    IntegerLiteralExpr(SourceRange range, uint64_t literal) : range(range), literal(literal) {}

    SourceRange totalRange() const override { return range; }
};

struct DecimalLiteralExpr final: public Expr {
    SourceRange range;
    std::string literal;
    DecimalLiteralExpr(SourceRange range, std::string literal) : range(range), literal(literal) {}

    SourceRange totalRange() const override { return range; }
};

struct BooleanLiteralExpr final: public Expr {
    SourceRange range;
    bool literal;
    BooleanLiteralExpr(SourceRange range, bool literal) : range(range), literal(literal) {}

    SourceRange totalRange() const override { return range; }
};

struct CharLiteralExpr final: public Expr {
    SourceRange range;
    char literal;
    CharLiteralExpr(SourceRange range, char literal) : range(range), literal(literal) {}

    SourceRange totalRange() const override { return range; }
};

struct StringLiteralExpr final: public Expr {
    SourceRange range;
    std::string literal;
    StringLiteralExpr(SourceRange range, std::string literal) : range(range), literal(literal) {}

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

struct BinOpExpr final: public Expr {
    SourceRange opRange;
    Expr* lhs;
    Expr* rhs;
    BinOp op;

    BinOpExpr(SourceRange opRange, Expr* lhs, Expr* rhs, BinOp op) : opRange(opRange), lhs(lhs), rhs(rhs), op(op) {}

    bool isMutable() const override { return lhs->isMutable(); }

    SourceRange totalRange() const override { return lhs->totalRange() + rhs->totalRange(); }
};

struct PreOpExpr final: public Expr {
    SourceRange opRange;
    Expr* operand;
    PreOp op;

    PreOpExpr(SourceRange opRange, Expr* operand, PreOp op) : opRange(opRange), operand(operand), op(op) {}
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

    CastExpr(SourceRange asRange, Expr* operand, Type destType) : asRange(asRange), operand(operand), destType(destType) {}

    SourceRange totalRange() const override { return operand->totalRange() + destType.range; }
};

struct DeclRefExpr final: public Expr {
    std::optional<std::pair<SourceRange, SourceRange>> parenRanges;
    Ident name;
    Array<Expr*> argList;
    Decl* decl = nullptr;

    DeclRefExpr(std::optional<std::pair<SourceRange, SourceRange>> parenRanges, Ident name, Array<Expr*> argList) :
        parenRanges(parenRanges), name(name), argList(argList) {}

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
        dotRange(dotRange), root(root), name(name) {}

    MemberRefExpr& operator=(MemberRefExpr const& other) = default;

    bool isMutable() const override {
        return root->isMutable();
    }

    SourceRange totalRange() const override {
        return root->totalRange() + name.range;
    }
};

struct ReturnExpr final: public Expr {
    SourceRange returnRange;
    Expr* value;

    ReturnExpr(SourceRange returnRange, Expr* value) : returnRange(returnRange), value(value) {}

    SourceRange totalRange() const override {
        auto range = returnRange;
        if(value) range += value->totalRange();
        return range;
    }
};

struct IfExpr final: public Expr {
    SourceRange ifRange;
    Expr* condition;
    Scope* thenScope;
    /// NOTE: Can be null.
    Scope* elseScope;

    IfExpr(SourceRange ifRange, Expr* condition, Scope* thenScope, Scope* elseScope = nullptr) :
        ifRange(ifRange), condition(condition), thenScope(thenScope), elseScope(elseScope) {}

    IfExpr(SourceRange ifRange, Expr* condition, Scope* thenScope, IfExpr* elseIf) :
        IfExpr(ifRange, condition, thenScope, new Scope(elseIf->totalRange(), { elseIf })) {}

    SourceRange totalRange() const override;
};

struct WhileExpr final: public Expr {
    SourceRange whileRange;
    Expr* condition;
    Scope* thenScope;

    WhileExpr(SourceRange whileRange, Expr* condition, Scope* thenScope) :
        whileRange(whileRange), condition(condition), thenScope(thenScope) {}

    SourceRange totalRange() const override {
        return whileRange + condition->totalRange() + thenScope->range;
    }
};

struct DoExpr final: public Expr {
    SourceRange doRange;
    std::variant<Expr*, Scope*> value;

    DoExpr(SourceRange doRange, Expr* expression) : doRange(doRange), value(expression) {}
    DoExpr(SourceRange doRange, Scope* scope) : doRange(doRange), value(scope) {}

    SourceRange totalRange() const override;
};
