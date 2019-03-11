#pragma once

#include "Collections.h"
#include "Misc.h"
#include "SourceInfo.h"

/// HIR (high-level intermediate representation) is the data structure output by the parser and read
/// by the type checker.
///
/// The closest analogue to HIR in other language frontends would be an abstract
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
        MultAssignment, DivAssignment, ModAssignment,
        AddAssignment, SubAssignment,
        BitwiseAndAssignment, BitwiseOrAssignment,
    };
    enum class UnOp {
        Not, Deref, Neg, Plus
    };
    constexpr const char* symbol(BinOp op) {
        switch(op) {
            case BinOp::Mult:                 return "*";
            case BinOp::Div:                  return "/";
            case BinOp::Mod:                  return "%";
            case BinOp::Add:                  return "+";
            case BinOp::Sub:                  return "-";
            case BinOp::Less:                 return "<";
            case BinOp::LessOrEq:             return "<=";
            case BinOp::Greater:              return ">";
            case BinOp::GreaterOrEq:          return ">=";
            case BinOp::Eq:                   return "==";
            case BinOp::NotEq:                return "!=";
            case BinOp::BitwiseAnd:           return "&";
            case BinOp::BitwiseOr:            return "|";
            case BinOp::LogicalAnd:           return "&&";
            case BinOp::LogicalOr:            return "||";
            case BinOp::Assignment:           return "=";
            case BinOp::MultAssignment:       return "*=";
            case BinOp::DivAssignment:        return "/=";
            case BinOp::ModAssignment:        return "%=";
            case BinOp::AddAssignment:        return "*=";
            case BinOp::SubAssignment:        return "*=";
            case BinOp::BitwiseAndAssignment: return "&=";
            case BinOp::BitwiseOrAssignment:  return "|=";
        }
    }
    constexpr char symbol(UnOp op) {
        switch(op) {
            case UnOp::Not:   return '!';
            case UnOp::Deref: return '*';
            case UnOp::Neg:   return '-';
            case UnOp::Plus:  return '+';
        }
    }
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

    struct Program {
        Array<Array<Expr>> expressions;
        SourceFile const& file;
        size_t numExpressions;

        void debugPrint();
    };

    struct Builder {
        Array<Array<Expr>> expressions {Array<Expr>()};
        Array<size_t> levels;

        ExprID intLit(uint64_t literal);
        ExprID decLit(double literal);
        ExprID binOp(BinOp op, ExprID lhs, ExprID rhs);
    };

};
