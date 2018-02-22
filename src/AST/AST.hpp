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

// This is used in the parser only, because DeclPrototypes and DeclRefExprs are not always possible
// to differentiate between until the colon at the end (this will become even more true once TypeRefs
// are just expressions again). So we generate one of these, and then convert it to a DeclPrototype
// or a DeclRefExpr.
struct DeclPrototypeORRef {
public:
    std::string name;
private:
    enum {
        params,
        args
    } tag;
    union {
        std::vector<Param> paramList;
        std::vector<Argument> argList;
    };
public:
    // None == will never have a type (in the case of DeclRefExpr).
    // Some where TypeRef.isInferred() == will eventually be given a type by the TypeChecker.
    // Some where TypeRef.isResolved() == has a type.
    llvm::Optional<TypeRef> type;
    bool isMut;
    bool isExtern;

    DeclPrototypeORRef(const std::string& name,
                       const std::vector<Param>& paramList,
                       llvm::Optional<TypeRef> type,
                       bool isMut,
                       bool isExtern) :
    name(name), tag(params), paramList(paramList), type(type), isMut(isMut), isExtern(isExtern) {}

    DeclPrototypeORRef(const std::string& name,
                       const std::vector<Argument>& argList,
                       llvm::Optional<TypeRef> type,
                       bool isMut,
                       bool isExtern) :
    name(name), tag(args), argList(argList), type(type), isMut(isMut), isExtern(isExtern) {}

    DeclPrototypeORRef(const DeclPrototypeORRef& other) :
        name(other.name), tag(other.tag), type(other.type), isMut(other.isMut), isExtern(other.isExtern) {
        if(other.tag == params) {
            paramList = other.paramList;
        } else {
            argList = other.argList;
        }
    }
    void operator=(const DeclPrototypeORRef& other) {
        name = other.name;
        tag = other.tag;
        type = other.type;
        isMut = other.isMut;
        isExtern = other.isExtern;
        if(other.tag == params) {
            paramList = other.paramList;
        } else {
            argList = other.argList;
        }
    }
    ~DeclPrototypeORRef() {}

    bool isProto() const { return tag == params; }
    bool isDeclRef() const { return tag == args; }

    std::vector<Param> getParamList() const {
        assert(tag == params);
        return paramList;
    }

    std::vector<Argument> getArgList() const {
        assert(tag == args);
        return argList;
    }
};

// A scope node represents a collection of other nodes.
struct Scope final : public ASTNode {
    std::vector<std::shared_ptr<ASTNode>> nodes;

    AST_NODE_CONSTRUCTOR(Scope, const std::vector<std::shared_ptr<ASTNode>>& nodes), nodes(nodes) {}
};
