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

struct Decl final : public ASTNode {
    std::string name;
    std::vector<std::shared_ptr<Param>> paramList;
    TypeRef type;
    bool isMut;
    bool isExtern;

    llvm::Value* codegenVal;
private:
    std::shared_ptr<ASTNode> value;
public:

    std::shared_ptr<Expr> expression() const {
        return std::dynamic_pointer_cast<Expr>(value);
    }
    std::shared_ptr<Scope> body() const {
        return std::dynamic_pointer_cast<Scope>(value);
    }
    bool hasDefinition() const { return (bool)value; }

    AST_NODE_CTOR(Decl,
                  std::string name,
                  std::vector<std::shared_ptr<Param>> paramList,
                  TypeRef type,
                  bool isMut,
                  bool isExtern,
                  std::shared_ptr<Expr> expression = nullptr),
    name(name), paramList(paramList), type(type), isMut(isMut), isExtern(isExtern),
    value(std::dynamic_pointer_cast<ASTNode>(expression)) {}

    AST_NODE_CTOR(Decl,
                  std::string name,
                  std::vector<std::shared_ptr<Param>> paramList,
                  TypeRef type,
                  bool isMut,
                  bool isExtern,
                  std::shared_ptr<Scope> body),
    name(name), paramList(paramList), type(type), isMut(isMut), isExtern(isExtern),
    value(std::dynamic_pointer_cast<ASTNode>(body)) {}
    ~Decl() {}

    // Opposites:
    bool isStored() const { return (bool)std::dynamic_pointer_cast<Expr>(value); }
    bool isComputed() const { return (bool)std::dynamic_pointer_cast<Scope>(value); }

    bool isParameterized() const { return !paramList.empty(); }
};

// This currently represents either a Decl, a parameter, or a prototype.
struct AbstractDecl final {
private:
    enum {
        parameter, declaration
    } tag;
    std::shared_ptr<ASTNode> val;
    std::shared_ptr<Param> param() const { return std::dynamic_pointer_cast<Param>(val); }
    std::shared_ptr<Decl> decl() const { return std::dynamic_pointer_cast<Decl>(val); }
public:
    AbstractDecl(std::shared_ptr<Param> param) : tag(parameter), val(param) {}
    AbstractDecl(std::shared_ptr<Decl> decl) : tag(declaration), val(decl) {}
    ~AbstractDecl() {}

    AbstractDecl(const AbstractDecl& other) : tag(other.tag), val(other.val) { }
    void operator=(const AbstractDecl& other) {
        tag = other.tag;
        val = other.val;
    }

    bool isParameter() const { return tag == parameter; }
    bool isDeclaration() const { return tag == declaration; }

    const std::string& name() const {
        if(isParameter()) return param()->name;
        if(isDeclaration()) return decl()->name;
        LLVM_BUILTIN_UNREACHABLE;
    }

    std::vector<std::shared_ptr<Param>> paramList() const {
        if(isParameter()) return std::vector<std::shared_ptr<Param>>();
        if(isDeclaration()) return decl()->paramList;
        LLVM_BUILTIN_UNREACHABLE;
    }

    TypeRef typeRef() {
        if(isParameter()) return TypeRef(param()->value.type);
        if(isDeclaration()) return decl()->type;
        LLVM_BUILTIN_UNREACHABLE;
    }

    SourceRange range() {
        if(isDeclaration()) return decl()->range;
        return param()->range;
    }

    llvm::Value* codegenVal() {
        if(isParameter()) return param()->codegenVal;
        if(isDeclaration()) return decl()->codegenVal;
        LLVM_BUILTIN_UNREACHABLE;
    }

    bool isMut() const {
        if(isDeclaration()) return decl()->isMut;
        return false;
    }
    bool isExtern() const {
        if(isDeclaration()) return decl()->isExtern;
        return false;
    }
    bool isComputed() const {
        if(isDeclaration()) return decl()->isComputed();
        return false;
    }
};
