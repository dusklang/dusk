//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <vector>
#include "llvm/ADT/SmallVector.h"

struct Expr;
struct TypeRefExpr;

enum class NodeKind {
    #define AST_NODE(name) name,
    #include "ASTNodes.def"
    NUM_NODES
};

#define AST_NODE_CONSTRUCTOR(name, args...) name(args) : ASTNode(NodeKind::name)

// Abstract class from which each node in the AST inherits.
struct ASTNode {
    NodeKind kind;
    ASTNode(NodeKind kind) : kind(kind) {}
    virtual ~ASTNode() {}
};

struct Param final : public ASTNode {
    std::string name;
    std::shared_ptr<Expr> value;

    AST_NODE_CONSTRUCTOR(Param, const std::string& name, std::shared_ptr<Expr> value), name(name), value(value) {}
};

struct Argument final : public ASTNode {
    std::string name;
    std::shared_ptr<Expr> value;

    AST_NODE_CONSTRUCTOR(Argument, const std::string& name, std::shared_ptr<Expr> value), name(name), value(value) {}
};

// This is used in Decls.
struct DeclPrototype final : public ASTNode {
    std::string name;
    std::vector<Param> paramList;
    bool isMut;

    AST_NODE_CONSTRUCTOR(DeclPrototype,
                         const std::string& name,
                         const std::vector<Param>& paramList,
                         bool isMut),
    name(name), paramList(paramList), isMut(isMut) {}
};

// A scope node represents a collection of other nodes.
struct Scope final : public ASTNode {
    std::vector<std::shared_ptr<ASTNode>> nodes;

    AST_NODE_CONSTRUCTOR(Scope, const std::vector<std::shared_ptr<ASTNode>>& nodes), nodes(nodes) {}
};
