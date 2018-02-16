//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <vector>
#include "llvm/ADT/SmallVector.h"

struct Expr;
struct TypeExpr;

// Abstract class from which each node in the AST inherits.
struct ASTNode {
    virtual std::string prettyPrint(int indentationLevel = -1) const = 0;
    std::string indentation(int level) const;
};

struct Param final : public ASTNode {
    std::string name;
    std::shared_ptr<Expr> value;

    Param(const std::string& name, std::shared_ptr<Expr> value) : name(name), value(value) {}
    std::string prettyPrint(int indentationLevel = -1) const override;
};

struct Argument final : public ASTNode {
    std::string name;
    std::shared_ptr<Expr> value;

    Argument(const std::string& name, std::shared_ptr<Expr> value) : name(name), value(value) {}
    std::string prettyPrint(int indentationLevel = -1) const override;
};

// This is used in Decls.
struct DeclPrototype final : public ASTNode {
    std::string name;
    llvm::SmallVector<llvm::SmallVector<Param, 2>, 1> paramLists;
    bool isMut;

    DeclPrototype(const std::string& name,
                  const llvm::SmallVector<llvm::SmallVector<Param, 2>, 1>& paramLists,
                  bool isMut)
            : name(name), paramLists(paramLists), isMut(isMut) {}

    std::string prettyPrint(int indentationLevel = -1) const override;
};

// A scope node represents a collection of other nodes.
struct ScopeNode final : public ASTNode {
    std::vector<std::shared_ptr<ASTNode>> nodes;

    ScopeNode(const std::vector<std::shared_ptr<ASTNode>>& nodes) : nodes(nodes) {}

    std::string prettyPrint(int indentationLevel = -1) const override;
};
