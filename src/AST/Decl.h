//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>

#include "llvm/IR/Value.h"

#include "AST.h"

struct Expr;

struct Decl: public ASTNode {
    std::string name;
    Type type;
    bool isVar;
    bool isExtern;

    // TODO: Store parameters contiguously.
    std::vector<Decl*> paramList;

    llvm::Value* codegenVal;
private:
    ASTNode* value;
public:
    Expr* expression() const;
    Scope* body() const;
    bool hasDefinition() const { return (bool)value; }

    Decl(SourceRange range,
         std::string name,
         Type type,
         bool isVar = false,
         bool isExtern = false,
         std::vector<Decl*> paramList = std::vector<Decl*>(),
         Expr* expression = nullptr);

    Decl(SourceRange range,
         std::string name,
         Type type,
         bool isVar,
         bool isExtern,
         std::vector<Decl*> paramList,
         Scope* body);

    ~Decl() {
        delete value;
        for (Decl* param: paramList) {
            delete param;
        }
    }

    // Opposites:
    bool isStored() const;
    bool isComputed() const;

    bool isParameterized() const { return !paramList.empty(); }
};
