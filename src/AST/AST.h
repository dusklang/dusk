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

struct Type final {
private:
    enum {
        builtin, pointer, inferred
    } tag;
    union {
        BuiltinType builtinTy;
    };
    std::shared_ptr<Type> pointedTy;
    llvm::Optional<SourceRange> sourceRange;
public:
    Type(BuiltinType builtinTy, llvm::Optional<SourceRange> sourceRange = llvm::None) :
        tag(builtin), builtinTy(builtinTy), sourceRange(sourceRange) {}
    Type(std::shared_ptr<Type> pointedTy, llvm::Optional<SourceRange> sourceRange = llvm::None) :
        tag(pointer), pointedTy(pointedTy), sourceRange(sourceRange) {}
    Type() : tag(inferred) {}
    ~Type() {}

    Type(const Type& other) : tag(other.tag) {
        switch(other.tag) {
            case builtin: builtinTy = other.builtinTy; break;
            case pointer: pointedTy = other.pointedTy; break;
            case inferred: break;
        }
    }

    void operator=(const Type& other) {
        tag = other.tag;
        switch(tag) {
            case builtin: builtinTy = other.builtinTy; break;
            case pointer: pointedTy = other.pointedTy; break;
            case inferred: break;
        }
    }

    bool operator==(Type other) const {
        if(tag != other.tag) return false;
        switch(tag) {
            case builtin: return other.builtinTy == builtinTy;
            case pointer: return *other.pointedTy == *pointedTy;
            case inferred: return true;
        }
    }

    bool operator!=(Type other) const {
        return !(*this == other);
    }

    bool isInferred() const { return tag == inferred; }
    llvm::Optional<BuiltinType> builtinType() {
        if(tag == builtin) return builtinTy;
        return llvm::None;
    }
    llvm::Optional<std::shared_ptr<Type>> pointedType() {
        if(tag == pointer) return pointedTy;
        return llvm::None;
    }

    std::string name() const;
};

struct Argument final : public ASTNode {
    std::shared_ptr<Expr> value;

    AST_NODE_CTOR(Argument, std::shared_ptr<Expr> value), value(value) {}
};

// A scope node represents a collection of other nodes.
struct Scope final : public ASTNode {
    std::vector<std::shared_ptr<ASTNode>> nodes;

    AST_NODE_CTOR(Scope, const std::vector<std::shared_ptr<ASTNode>>& nodes), nodes(nodes) {}
};
