//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <memory>
#include <string>
#include "AST.hpp"

struct Expr;

struct Decl final : public ASTNode {
    DeclPrototype prototype;
    std::shared_ptr<Expr> type;
    bool isConstant;
    std::shared_ptr<Expr> expression;

    Decl(DeclPrototype prototype,
         std::shared_ptr<Expr> type,
         bool isConstant,
         std::shared_ptr<Expr> expression)
        : prototype(prototype), type(type), isConstant(isConstant), expression(expression) {
            for(auto& params: prototype.paramLists) {
                assert(!params.empty() && "Encountered empty parameter list, which is not allowed.");
            }
        }
    std::string prettyPrint() const override;
};
