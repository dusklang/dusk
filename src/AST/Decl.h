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
    std::optional<SourceRange> externRange;
    std::optional<SourceRange> keywordRange;
    Ident name;
    Type type;
    bool isVar;
    std::vector<Decl*> paramList;

    // FIXME: Store this information in CodeGenerator.
    llvm::Value* codegenVal;
private:
    // FIXME: Make this into a variant.
    ASTNode* value = nullptr;
public:
    Expr* expression() const;
    Scope* body() const;
    bool hasDefinition() const { return (bool)value; }

    /// Constructor for bare declarations, used for parameters and struct fields.
    Decl(Ident name, Type type, bool isVar) : ASTNode(NodeKind::Decl), name(name), type(type), isVar(isVar) {}

    /// Constructor for stored declarations.
    Decl(std::optional<SourceRange> externRange, SourceRange keywordRange, Ident name, Type type, bool isVar, std::vector<Decl*> paramList, Expr* expression) : ASTNode(NodeKind::Decl), externRange(externRange), keywordRange(keywordRange), name(name), type(type), isVar(isVar), paramList(paramList), value(expression) {}

    /// Constructor for computed declarations.
    Decl(std::optional<SourceRange> externRange, SourceRange keywordRange, Ident name, Type type, bool isVar, std::vector<Decl*> paramList, Scope* body) : ASTNode(NodeKind::Decl), externRange(externRange), keywordRange(keywordRange), name(name), type(type), isVar(isVar), paramList(paramList), value(body) {}

    /// Constructor for declaration prototypes.
    Decl(std::optional<SourceRange> externRange, SourceRange keywordRange, Ident name, Type type, bool isVar, std::vector<Decl*> paramList) : ASTNode(NodeKind::Decl), externRange(externRange), keywordRange(keywordRange), name(name), type(type), isVar(isVar), paramList(paramList) {}

    // Opposites:
    bool isStored() const;
    bool isComputed() const;

    bool isParameterized() const { return !paramList.empty(); }
    bool isExtern() const { return (bool)externRange; }
};

struct StructDecl: public ASTNode {
    SourceRange structRange;
    Ident name;
    std::vector<Decl*> fields;

    StructDecl(SourceRange structRange, Ident name, std::vector<Decl*> fields) : ASTNode(NodeKind::StructDecl), structRange(structRange), name(name), fields(fields) {}
};
