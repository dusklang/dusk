//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <vector>
#include "llvm/ADT/Optional.h"
#include "llvm/IR/Value.h"
#include "Types.h"
#include "General/SourceLoc.h"

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

// This represents an actual reference to a type in a source file.
struct PhysicalTypeRef: public ASTNode {
    BuiltinType type;

    AST_NODE_CTOR(PhysicalTypeRef, BuiltinType type), type(type) {}
    AST_NODE_CTOR(PhysicalTypeRef, const PhysicalTypeRef& other), type(other.type) {}
    ~PhysicalTypeRef() {}

    void operator=(const PhysicalTypeRef& other) {
        range = other.range;
        type = other.type;
    }
};

struct TypeRef {
private:
    enum {
        inferred,
        physical,
        resolved
    } tag;
    union {
        BuiltinType resolvedType;
        PhysicalTypeRef physicalType;
    };
public:
    TypeRef(BuiltinType type) : tag(resolved), resolvedType(type) {}
    TypeRef(const PhysicalTypeRef& type) : tag(physical), physicalType(type) {}
    TypeRef() : tag(inferred) {}
    ~TypeRef() {}

    TypeRef(const TypeRef& other) : tag(other.tag) {
        if(other.isResolved()) resolvedType = other.resolvedType;
        else if(other.isPhysical()) physicalType = other.physicalType;
    }
    void operator=(const TypeRef& other) {
        tag = other.tag;
        if(other.isResolved()) resolvedType = other.resolvedType;
        else if(other.isPhysical()) physicalType = other.physicalType;
    }

    bool isInferred() const { return tag == inferred; }
    bool isPhysical() const { return tag == physical; }
    bool isResolved() const { return tag == resolved; }

    BuiltinType getType() const {
        assert(!isInferred());
        if(isPhysical()) return physicalType.type;
        if(isResolved()) return resolvedType;
        LLVM_BUILTIN_UNREACHABLE;
    }

    void resolveType(BuiltinType resolvedType) {
        assert(isInferred());
        tag = resolved;
        this->resolvedType = resolvedType;
    }
};

struct Param final : public ASTNode {
    std::string name;
    PhysicalTypeRef value;

    llvm::Value* codegenVal;

    AST_NODE_CTOR(Param, const std::string& name, PhysicalTypeRef value), name(name), value(value) {}
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
