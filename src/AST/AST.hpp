//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <vector>
#include "llvm/ADT/SmallVector.h"

struct Expr;
struct PlaceholderTypeRefExpr;

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

// This is used in the parser only, because DeclPrototypes and DeclRefExprs are not always possible
// to differentiate between until the end.
struct DeclPrototypeORRef {
    std::string name;
    std::vector<Param> paramList;
    std::shared_ptr<Expr> type;
    bool isMut;
    bool isExtern;
    // The parser's best guess on what this is.
    typedef enum {
        prototype,
        ref
    } Guess;
    Guess guess;

    DeclPrototypeORRef(const std::string& name,
                       const std::vector<Param>& paramList,
                       const std::shared_ptr<Expr>& type,
                       bool isMut,
                       bool isExtern,
                       Guess guess) :
    name(name), paramList(paramList), type(type), isMut(isMut), isExtern(isExtern), guess(guess) {}
};

// A scope node represents a collection of other nodes.
struct Scope final : public ASTNode {
    std::vector<std::shared_ptr<ASTNode>> nodes;

    AST_NODE_CONSTRUCTOR(Scope, const std::vector<std::shared_ptr<ASTNode>>& nodes), nodes(nodes) {}
};
