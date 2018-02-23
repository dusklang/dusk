//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <memory>
#include <string>
#include "AST.h"
#include "Expr.h"
#include "Types.h"

struct Expr;
struct Decl;

// This is used in Decls, or on its own as a standalone prototype.
struct DeclPrototype final : public ASTNode {
    std::string name;
    std::vector<std::shared_ptr<Param>> paramList;
    // This represents the actual type specified at a given location in a source file. This won't
    // be affected by the type checker. For the actual type of a decl, view Decl::getType()
    llvm::Optional<PhysicalTypeRef> physicalType;
    bool isMut;
    bool isExtern;
    bool isComputed;

    AST_NODE_CTOR(DeclPrototype,
                  const std::string& name,
                  const std::vector<std::shared_ptr<Param>>& paramList,
                  llvm::Optional<PhysicalTypeRef> type,
                  bool isMut,
                  bool isExtern,
                  bool isComputed = false),
    name(name), paramList(paramList), physicalType(type), isMut(isMut), isExtern(isExtern),
    isComputed(isComputed) {}

    bool isParameterized() const {
        return !paramList.empty();
    }
};

struct Decl final : public ASTNode {
private:
    std::shared_ptr<ASTNode> value;
    TypeRef type;
public:
    std::shared_ptr<DeclPrototype> prototype;

    std::shared_ptr<Expr> expression() const {
        return std::dynamic_pointer_cast<Expr>(value);
    }
    std::shared_ptr<Scope> body() const {
        return std::dynamic_pointer_cast<Scope>(value);
    }

    AST_NODE_CTOR(Decl,
                  std::shared_ptr<DeclPrototype> prototype,
                  std::shared_ptr<Expr> expression),
    value(std::dynamic_pointer_cast<ASTNode>(expression)), prototype(prototype) {
        if(prototype->physicalType) {
            type = TypeRef(*prototype->physicalType);
        }
    }

    AST_NODE_CTOR(Decl,
                  std::shared_ptr<DeclPrototype> prototype,
                  std::shared_ptr<Scope> body),
    value(std::dynamic_pointer_cast<ASTNode>(body)), prototype(prototype) {
        if(prototype->physicalType) {
            type = TypeRef(*prototype->physicalType);
        }
    }
    ~Decl() {}

    TypeRef* getTypeRef() {
        return &type;
    }

    // Opposites:
    bool isStored() const { return (bool)std::dynamic_pointer_cast<Expr>(value); }
    bool isComputed() const { return (bool)std::dynamic_pointer_cast<Scope>(value); }

    bool isParameterized() const { return prototype->isParameterized(); }
    bool isMut() const { return prototype->isMut; }
};

// This currently represents either a Decl, a parameter, or a prototype.
struct AbstractDecl final {
private:
    enum {
        parameter, prototype, declaration
    } tag;
    union {
        std::shared_ptr<Param> param;
        std::shared_ptr<DeclPrototype> proto;
        std::shared_ptr<Decl> decl;
    };
public:
    AbstractDecl(std::shared_ptr<Param> param) : tag(parameter), param(param) {}
    AbstractDecl(std::shared_ptr<DeclPrototype> proto) : tag(prototype), proto(proto) {}
    AbstractDecl(std::shared_ptr<Decl> decl) : tag(declaration), decl(decl) {}
    ~AbstractDecl() {}

    AbstractDecl(const AbstractDecl& other) : tag(other.tag) {
        if(other.tag == parameter) param = other.param;
        if(other.tag == prototype) proto = other.proto;
        if(other.tag == declaration) decl = other.decl;
    }
    void operator=(const AbstractDecl& other) {
        tag = other.tag;
        if(tag == parameter) param = other.param;
        if(tag == prototype) proto = other.proto;
        if(tag == declaration) decl = other.decl;
    }

    bool isParameter() const { return tag == parameter; }
    bool isPrototype() const { return tag == prototype; }
    bool isDeclaration() const { return tag == declaration; }

    const std::string& name() const {
        if(isParameter()) return param->name;
        if(isPrototype()) return proto->name;
        if(isDeclaration()) return decl->prototype->name;
        LLVM_BUILTIN_UNREACHABLE;
    }

    std::vector<std::shared_ptr<Param>> paramList() const {
        if(isParameter()) return std::vector<std::shared_ptr<Param>>();
        if(isPrototype()) return proto->paramList;
        if(isDeclaration()) return decl->prototype->paramList;
        LLVM_BUILTIN_UNREACHABLE;
    }

    llvm::Optional<PhysicalTypeRef> physicalType() const {
        if(isParameter()) return param->value;
        if(isPrototype()) return proto->physicalType;
        if(isDeclaration()) return decl->prototype->physicalType;
        LLVM_BUILTIN_UNREACHABLE;
    }

    TypeRef typeRef() {
        if(isParameter()) return TypeRef(param->value.type);
        if(isPrototype()) return TypeRef(proto->physicalType->type);
        if(isDeclaration()) return *decl->getTypeRef();
        LLVM_BUILTIN_UNREACHABLE;
    }

    SourceRange range() {
        if(isParameter()) return param->range;
        if(isPrototype()) return proto->range;
        if(isDeclaration()) return decl->prototype->range;
        LLVM_BUILTIN_UNREACHABLE;
    }

    bool isMut() const { return isPrototype() ? proto->isMut : false; }
    bool isExtern() const { return isPrototype() ? proto->isExtern : false; }
    bool isComputed() const { return isPrototype() ? proto->isComputed : false; }
};
