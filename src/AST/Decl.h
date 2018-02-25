//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <memory>
#include <string>

#include "llvm/IR/Value.h"

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

    llvm::Value* codegenVal;

    AST_NODE_CTOR(DeclPrototype,
                  const std::string& name,
                  const std::vector<std::shared_ptr<Param>>& paramList,
                  llvm::Optional<PhysicalTypeRef> type,
                  bool isMut,
                  bool isExtern),
    name(name), paramList(paramList), physicalType(type), isMut(isMut), isExtern(isExtern) {}

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
    std::shared_ptr<ASTNode> val;
    std::shared_ptr<Param> param() const { return std::dynamic_pointer_cast<Param>(val); }
    std::shared_ptr<DeclPrototype> proto() const {
        return std::dynamic_pointer_cast<DeclPrototype>(val);
    }
    std::shared_ptr<Decl> decl() const { return std::dynamic_pointer_cast<Decl>(val); }
public:
    AbstractDecl(std::shared_ptr<Param> param) : tag(parameter), val(param) {}
    AbstractDecl(std::shared_ptr<DeclPrototype> proto) : tag(prototype), val(proto) {}
    AbstractDecl(std::shared_ptr<Decl> decl) : tag(declaration), val(decl) {}
    ~AbstractDecl() {}

    AbstractDecl(const AbstractDecl& other) : tag(other.tag), val(other.val) {
    }
    void operator=(const AbstractDecl& other) {
        tag = other.tag;
        val = other.val;
    }

    bool isParameter() const { return tag == parameter; }
    bool isPrototype() const { return tag == prototype; }
    bool isDeclaration() const { return tag == declaration; }

    const std::string& name() const {
        if(isParameter()) return param()->name;
        if(isPrototype()) return proto()->name;
        if(isDeclaration()) return decl()->prototype->name;
        LLVM_BUILTIN_UNREACHABLE;
    }

    std::vector<std::shared_ptr<Param>> paramList() const {
        if(isParameter()) return std::vector<std::shared_ptr<Param>>();
        if(isPrototype()) return proto()->paramList;
        if(isDeclaration()) return decl()->prototype->paramList;
        LLVM_BUILTIN_UNREACHABLE;
    }

    llvm::Optional<PhysicalTypeRef> physicalType() const {
        if(isParameter()) return param()->value;
        if(isPrototype()) return proto()->physicalType;
        if(isDeclaration()) return decl()->prototype->physicalType;
        LLVM_BUILTIN_UNREACHABLE;
    }

    TypeRef typeRef() {
        if(isParameter()) return TypeRef(param()->value.type);
        if(isPrototype()) return TypeRef(proto()->physicalType->type);
        if(isDeclaration()) return *decl()->getTypeRef();
        LLVM_BUILTIN_UNREACHABLE;
    }

    SourceRange range() {
        if(auto proto = getProto()) return proto->range;
        return param()->range;
    }

    std::shared_ptr<DeclPrototype> getProto() const {
        if(isPrototype()) return proto();
        if(isDeclaration()) return decl()->prototype;
        return nullptr;
    }

    llvm::Value* codegenVal() {
        if(isParameter()) return param()->codegenVal;
        return getProto()->codegenVal;
    }

    bool isMut() const {
        if(auto proto = getProto()) return proto->isMut;
        return false;
    }
    bool isExtern() const {
        if(auto proto = getProto()) return proto->isExtern;
        return false;
    }
    bool isComputed() const {
        if(isDeclaration()) return decl()->isComputed();
        // FIXME: Assuming all prototypes are computed is bad. I'm probably going to get rid
        // of prototypes soon though, anyway (and just have everything be a decl).
        if(isPrototype()) return true;
        return false;
    }
};
