//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <vector>
#include "llvm/ADT/Optional.h"
#include "Types.hpp"

struct Expr;

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

struct TypeRef: public ASTNode {
private:
    enum Tag {
        inferred,
        resolved
    } tag;
    BuiltinType type;
public:
    AST_NODE_CONSTRUCTOR(TypeRef), tag(inferred) {}
    AST_NODE_CONSTRUCTOR(TypeRef, BuiltinType type), tag(resolved), type(type) {}
    AST_NODE_CONSTRUCTOR(TypeRef, const TypeRef& other), type(other.type) {}
    ~TypeRef() {}

    bool isInferred() const { return tag == inferred; }
    bool isResolved() const { return tag == resolved; }
    BuiltinType getType() const {
        assert(tag == resolved);
        return type;
    }
    void setType(BuiltinType type) {
        this->type = type;
        tag = resolved;
    }

    void operator=(const TypeRef& other) {
        type = other.type;
    }
};

struct Param final : public ASTNode {
    std::string name;
    TypeRef value;

    AST_NODE_CONSTRUCTOR(Param, const std::string& name, TypeRef value), name(name), value(value) {}
};

struct Argument final : public ASTNode {
    std::string name;
    std::shared_ptr<Expr> value;

    AST_NODE_CONSTRUCTOR(Argument, const std::string& name, std::shared_ptr<Expr> value), name(name), value(value) {}
};

// A scope node represents a collection of other nodes.
struct Scope final : public ASTNode {
    std::vector<std::shared_ptr<ASTNode>> nodes;

    AST_NODE_CONSTRUCTOR(Scope, const std::vector<std::shared_ptr<ASTNode>>& nodes), nodes(nodes) {}
};
