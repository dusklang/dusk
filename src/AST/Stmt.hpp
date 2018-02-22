//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <vector>
#include <memory>
#include "AST.hpp"

#include "Expr.hpp"

enum class StmtKind {
    #define STMT_NODE(name) name,
    #include "ASTNodes.def"
    NUM_EXPRS
};

// Base class from which each statement node inherits.
struct Stmt : public ASTNode {
    StmtKind stmtKind;
    AST_NODE_CONSTRUCTOR(Stmt, StmtKind stmtKind), stmtKind(stmtKind) {}
};

#define STMT_CONSTRUCTOR(name, args...) name##Stmt(args) : Stmt(StmtKind::name)

struct ReturnStmt: public Stmt {
    std::shared_ptr<Expr> value;

    STMT_CONSTRUCTOR(Return, const std::shared_ptr<Expr>& value), value(value) {}
};
