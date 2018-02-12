//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include "llvm/ADT/SmallVector.h"

struct Expr;

// Abstract class from which each node in the AST inherits.
struct ASTNode {
    virtual std::string prettyPrint() const = 0;
};

struct Param final : public ASTNode {
    std::string name;
    std::shared_ptr<Expr> value;

    Param(const std::string& name, std::shared_ptr<Expr> value) : name(name), value(value) {}
    std::string prettyPrint() const override;
};

// This is used in Decls and DeclRefs.
struct DeclPrototype final : public ASTNode {
    std::string name;
    llvm::SmallVector<llvm::SmallVector<Param, 2>, 1> paramLists;

    DeclPrototype(const std::string& name,
                  const llvm::SmallVector<llvm::SmallVector<Param, 2>, 1>& paramLists)
            : name(name), paramLists(paramLists) {}

    std::string prettyPrint() const override;
};
