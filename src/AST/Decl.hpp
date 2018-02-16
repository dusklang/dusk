//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <memory>
#include <string>
#include "AST.hpp"
#include "Expr.hpp"

struct Expr;

struct Decl final : public ASTNode {
private:
    std::shared_ptr<ASTNode> value;
public:
    DeclPrototype prototype;
    std::shared_ptr<Expr> type;

    std::shared_ptr<Expr> expression() const { return std::dynamic_pointer_cast<Expr>(value); }
    std::shared_ptr<ScopeNode> body() const {
        return std::dynamic_pointer_cast<ScopeNode>(value);
    }

    Decl(DeclPrototype prototype,
         std::shared_ptr<Expr> expression,
         std::shared_ptr<Expr> type)
      : value(std::dynamic_pointer_cast<ASTNode>(expression)),
        prototype(prototype),
        type(type) {}

    Decl(DeclPrototype prototype,
         std::shared_ptr<ScopeNode> body,
         std::shared_ptr<Expr> type)
      : value(std::dynamic_pointer_cast<ASTNode>(body)),
        prototype(prototype),
        type(type) {}
    std::string prettyPrint(int indentationLevel = 0) const override;
};
