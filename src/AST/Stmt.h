//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <vector>

#include "AST.h"

struct Expr;
struct DeclRefExpr;

enum class StmtKind {
    #define STMT_NODE(name) name,
    #include "ASTNodes.def"
    NUM_STMTS
};

// Base class from which each statement node inherits.
struct Stmt : public ASTNode {
    StmtKind stmtKind;
    Stmt(SourceRange range, StmtKind stmtKind) : ASTNode(NodeKind::Stmt, range), stmtKind(stmtKind) {}
};

struct ReturnStmt: public Stmt {
    Expr* value;

    ReturnStmt(SourceRange range, Expr* value) : Stmt(range, StmtKind::Return), value(value) {}

    ~ReturnStmt() override;
};

struct AssignmentStmt: public Stmt {
    DeclRefExpr* lhs;
    Expr* rhs;

    AssignmentStmt(SourceRange range, DeclRefExpr* lhs, Expr* rhs) : Stmt(range, StmtKind::Assignment),
    lhs(lhs), rhs(rhs) {}

    ~AssignmentStmt() override;
};

struct IfStmt: public Stmt {
    Expr* condition;
    Scope* thenScope;
    Scope* elseScope;

    IfStmt(SourceRange range, Expr* condition, Scope* thenScope,
           Scope* elseScope) : Stmt(range, StmtKind::If),
    condition(condition), thenScope(thenScope), elseScope(elseScope) {}

    ~IfStmt() override;
};

struct WhileStmt: public Stmt {
    Expr* condition;
    Scope* thenScope;

    WhileStmt(SourceRange range, Expr* condition, Scope* thenScope) : Stmt(range, StmtKind::While),
    condition(condition), thenScope(thenScope) {}

    ~WhileStmt() override;
};
