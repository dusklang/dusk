//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <vector>
#include <memory>
#include "AST.h"

#include "Expr.h"

enum class StmtKind {
    #define STMT_NODE(name) name,
    #include "ASTNodes.def"
    NUM_EXPRS
};

// Base class from which each statement node inherits.
struct Stmt : public ASTNode {
    StmtKind stmtKind;
    AST_NODE_CTOR(Stmt, StmtKind stmtKind), stmtKind(stmtKind) {}
};

#define STMT_CTOR(name, args...) name##Stmt(SourceRange range, args) : Stmt(range, StmtKind::name)

struct ReturnStmt: public Stmt {
    std::shared_ptr<Expr> value;

    STMT_CTOR(Return, std::shared_ptr<Expr> value), value(value) {}
};

struct AssignmentStmt: public Stmt {
    std::shared_ptr<DeclRefExpr> lhs;
    std::shared_ptr<Expr> rhs;

    STMT_CTOR(Assignment, std::shared_ptr<DeclRefExpr> lhs, std::shared_ptr<Expr> rhs),
    lhs(lhs), rhs(rhs) {}
};

struct IfStmt: public Stmt {
    std::shared_ptr<Expr> condition;
    std::shared_ptr<Scope> thenScope;
    std::optional<std::shared_ptr<Scope>> elseScope;

    STMT_CTOR(If, std::shared_ptr<Expr> condition, std::shared_ptr<Scope> thenScope,
              std::optional<std::shared_ptr<Scope>> elseScope),
    condition(condition), thenScope(thenScope), elseScope(elseScope) {}
};

struct WhileStmt: public Stmt {
    std::shared_ptr<Expr> condition;
    std::shared_ptr<Scope> thenScope;

    STMT_CTOR(While, std::shared_ptr<Expr> condition, std::shared_ptr<Scope> thenScope),
    condition(condition), thenScope(thenScope) {}
};
