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
         std::shared_ptr<Expr> type,
         std::shared_ptr<Expr> expression)
        : value(std::dynamic_pointer_cast<ASTNode>(expression)),
          prototype(prototype), type(type) {
        for(auto& params: prototype.paramLists) {
            assert(!params.empty() && "Encountered empty parameter list, which is not allowed.");
        }
    }

    Decl(DeclPrototype prototype,
         std::shared_ptr<Expr> type,
         std::shared_ptr<ScopeNode> body)
      : value(std::dynamic_pointer_cast<ASTNode>(body)),
       prototype(prototype), type(type) {
        for(auto& params: prototype.paramLists) {
            assert(!params.empty() && "Encountered empty parameter list, which is not allowed.");
        }
    }
    std::string prettyPrint(int indentationLevel = -1) const override;
};
