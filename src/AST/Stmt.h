//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <vector>
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
    Stmt(SourceRange range, StmtKind stmtKind) : ASTNode(NodeKind::Stmt, range), stmtKind(stmtKind) {}
};

struct ReturnStmt: public Stmt {
    Expr* value;

    ReturnStmt(SourceRange range, Expr* value) : Stmt(range, StmtKind::Return), value(value) {}

    ~ReturnStmt() override { delete value; }
};

struct AssignmentStmt: public Stmt {
    DeclRefExpr* lhs;
    Expr* rhs;

    AssignmentStmt(SourceRange range, DeclRefExpr* lhs, Expr* rhs) : Stmt(range, StmtKind::Assignment),
    lhs(lhs), rhs(rhs) {}

    ~AssignmentStmt() override {
        delete lhs;
        delete rhs;
    }
};

struct IfStmt: public Stmt {
    Expr* condition;
    Scope* thenScope;
    std::optional<Scope*> elseScope;

    IfStmt(SourceRange range, Expr* condition, Scope* thenScope,
           std::optional<Scope*> elseScope) : Stmt(range, StmtKind::If),
    condition(condition), thenScope(thenScope), elseScope(elseScope) {}

    ~IfStmt() override {
        delete condition;
        delete thenScope;
        if(elseScope) delete *elseScope;
    }
};

struct WhileStmt: public Stmt {
    Expr* condition;
    Scope* thenScope;

    WhileStmt(SourceRange range, Expr* condition, Scope* thenScope) : Stmt(range, StmtKind::While),
    condition(condition), thenScope(thenScope) {}

    ~WhileStmt() override {
        delete condition;
        delete thenScope;
    }
};
