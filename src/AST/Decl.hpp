//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <memory>
#include <string>
#include "AST.hpp"
#include "Expr.hpp"
#include "Types.hpp"

struct Expr;

// This is used in Decls, or on its own as a standalone prototype.
struct DeclPrototype final : public ASTNode {
    std::string name;
    std::vector<Param> paramList;
    llvm::Optional<TypeRef> type;
    bool isMut;
    bool isExtern;

    AST_NODE_CTOR(DeclPrototype,
                  const std::string& name,
                  const std::vector<Param>& paramList,
                  llvm::Optional<TypeRef> type,
                  bool isMut,
                  bool isExtern),
    name(name), paramList(paramList), type(type), isMut(isMut), isExtern(isExtern) {}
};

struct Decl final : public ASTNode {
private:
    std::shared_ptr<ASTNode> value;
public:
    DeclPrototype prototype;

    std::shared_ptr<Expr> expression() const { return std::dynamic_pointer_cast<Expr>(value); }
    std::shared_ptr<Scope> body() const {
        return std::dynamic_pointer_cast<Scope>(value);
    }

    AST_NODE_CTOR(Decl,
                  DeclPrototype prototype,
                  std::shared_ptr<Expr> expression),
    value(std::dynamic_pointer_cast<ASTNode>(expression)), prototype(prototype) {}

    AST_NODE_CTOR(Decl,
                  DeclPrototype prototype,
                  std::shared_ptr<Scope> body),
    value(std::dynamic_pointer_cast<ASTNode>(body)),
    prototype(prototype) {}
};
