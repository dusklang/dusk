//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <vector>

#include "AST.h"

struct Expr;
struct Scope;
namespace llvm {
    class Value;
}

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
        if(value) {
            delete value;
        }
        for (Decl* param: paramList) {
            delete param;
        }
    }

    // Opposites:
    bool isStored() const;
    bool isComputed() const;

    bool isParameterized() const { return !paramList.empty(); }
};

struct StructDecl: public ASTNode {
    std::string name;
    std::vector<Decl*> fields;

    StructDecl(SourceRange range, std::string name, std::vector<Decl*> fields) : ASTNode(NodeKind::StructDecl, range), name(name), fields(fields) {}
};
