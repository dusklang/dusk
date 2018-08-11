//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <vector>
#include <variant>
#include <optional>

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
    Stmt(StmtKind stmtKind) : ASTNode(NodeKind::Stmt), stmtKind(stmtKind) {}
};

struct ReturnStmt: public Stmt {
    SourceRange returnRange;
    Expr* value;

    ReturnStmt(SourceRange returnRange, Expr* value) : Stmt(StmtKind::Return), returnRange(returnRange), value(value) {}

    ~ReturnStmt() override;
};

struct IfStmt: public Stmt {
    SourceRange ifRange;
    Expr* condition;
    Scope* thenScope;
    std::optional<std::variant<Scope*, IfStmt*>> elseNode;

    IfStmt(SourceRange ifRange, Expr* condition, Scope* thenScope, std::optional<std::variant<Scope*, IfStmt*>> elseNode) :
        Stmt(StmtKind::If), ifRange(ifRange), condition(condition), thenScope(thenScope), elseNode(elseNode) {}

    ~IfStmt() override;
};

struct WhileStmt: public Stmt {
    SourceRange whileRange;
    Expr* condition;
    Scope* thenScope;

    WhileStmt(SourceRange whileRange, Expr* condition, Scope* thenScope) :
        Stmt(StmtKind::While), whileRange(whileRange), condition(condition), thenScope(thenScope) {}

    ~WhileStmt() override;
};
