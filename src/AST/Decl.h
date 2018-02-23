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
    std::vector<Param> paramList;
    // This represents the actual type specified at a given location in a source file. This won't
    // be affected by the type checker. For the actual type of a decl, view Decl::getType()
    llvm::Optional<PhysicalTypeRef> physicalType;
    bool isMut;
    bool isExtern;

    AST_NODE_CTOR(DeclPrototype,
                  const std::string& name,
                  const std::vector<Param>& paramList,
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
