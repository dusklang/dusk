//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <vector>
#include "llvm/ADT/Optional.h"
#include "Types.hpp"
#include "General/SourceLoc.hpp"

struct Expr;

enum class NodeKind {
    #define AST_NODE(name) name,
    #include "ASTNodes.def"
    NUM_NODES
};

#define AST_NODE_CTOR(name, args...) name(SourceRange range, args) : ASTNode(NodeKind::name, range)
#define AST_NODE_CTOR_NOARG(name) name(SourceRange range) : ASTNode(NodeKind::name, range)

// Abstract class from which each node in the AST inherits.
struct ASTNode {
    NodeKind kind;
    SourceRange range;
    ASTNode(NodeKind kind, SourceRange range) : kind(kind), range(range) {}
    virtual ~ASTNode() {}
};

struct TypeRef: public ASTNode {
    BuiltinType type;

    AST_NODE_CTOR(TypeRef, BuiltinType type), type(type) {}
    AST_NODE_CTOR(TypeRef, const TypeRef& other), type(other.type) {}
    ~TypeRef() {}

    void operator=(const TypeRef& other) {
        range = other.range;
        type = other.type;
    }
};

struct Param final : public ASTNode {
    std::string name;
    TypeRef value;

    AST_NODE_CTOR(Param, const std::string& name, TypeRef value), name(name), value(value) {}
};

struct Argument final : public ASTNode {
    std::string name;
    std::shared_ptr<Expr> value;

    AST_NODE_CTOR(Argument, const std::string& name, std::shared_ptr<Expr> value), name(name), value(value) {}
};

// A scope node represents a collection of other nodes.
struct Scope final : public ASTNode {
    std::vector<std::shared_ptr<ASTNode>> nodes;

    AST_NODE_CTOR(Scope, const std::vector<std::shared_ptr<ASTNode>>& nodes), nodes(nodes) {}
};
