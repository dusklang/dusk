//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <memory>
#include <string>

#include "llvm/IR/Value.h"

#include "AST.h"
#include "Expr.h"
#include "Types.h"

struct Expr;

struct Decl final : public ASTNode {
    std::string name;
    TypeRef type;
    bool isMut;
    bool isExtern;

    std::vector<std::shared_ptr<Decl>> paramList;

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
                  TypeRef type,
                  bool isMut = false,
                  bool isExtern = false,
                  std::vector<std::shared_ptr<Decl>> paramList = std::vector<std::shared_ptr<Decl>>(),
                  std::shared_ptr<Expr> expression = nullptr),
    name(name), type(type), isMut(isMut), isExtern(isExtern), paramList(paramList),
    value(std::dynamic_pointer_cast<ASTNode>(expression)) {}

    AST_NODE_CTOR(Decl,
                  std::string name,
                  TypeRef type,
                  bool isMut,
                  bool isExtern,
                  std::vector<std::shared_ptr<Decl>> paramList,
                  std::shared_ptr<Scope> body),
    name(name), type(type), isMut(isMut), isExtern(isExtern), paramList(paramList),
    value(std::dynamic_pointer_cast<ASTNode>(body)) {}
    ~Decl() {}

    // Opposites:
    bool isStored() const { return (bool)std::dynamic_pointer_cast<Expr>(value); }
    bool isComputed() const {
        if(!hasDefinition() && isExtern) return true;
        return (bool)std::dynamic_pointer_cast<Scope>(value);
    }

    bool isParameterized() const { return !paramList.empty(); }
};
