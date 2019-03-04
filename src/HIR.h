#pragma once

#include "Collections.h"
#include "Misc.h"

/// HIR (high-level intermediate representation) is the data structure output by the parser and read
/// by the type checker. The closest analogue to HIR in other language frontends would be an abstract
/// syntax tree. However, unlike an AST, HIR is specially optimized for solving the problem of type
/// checking Meda code, and basically nothing else.
namespace hir {
    struct ExprID {
        uint32_t id;
        explicit ExprID(uint32_t id) : id(id) {}
    };
    enum class BinOp {
        Mult, Div, Mod,
        Add, Sub,
        Less, LessOrEq, Greater, GreaterOrEq,
        Eq, NotEq,
        BitwiseAnd, BitwiseOr,
        LogicalAnd, LogicalOr,
        Assignment,
    };
    enum class UnOp {
        Not, Deref, Neg, Plus
    };
    enum class ExprKind {
        IntLit,
        DecLit,
        BinOp,
    };
    struct Expr {
        ExprKind kind;
        ExprID id;
        union {
            struct {
                BinOp op;
                ExprID lhs, rhs;
            } binOp;
        };
        Expr(ExprKind kind, ExprID id) : kind(kind), id(id) {}
        Expr(ExprID id, BinOp op, ExprID lhs, ExprID rhs) : Expr(ExprKind::BinOp, id) {
            this->binOp = {op, lhs, rhs};
        }
    };
    class Builder {
        Array<Array<Expr>> expressions {Array<Expr>()};
        Array<size_t> levels;
    public:
        ExprID intLit(uint64_t literal);
        ExprID binOp(BinOp op, ExprID lhs, ExprID rhs);

        void debugPrint() const;
    };
};
