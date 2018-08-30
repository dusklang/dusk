//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <vector>

#include "General/SourceInfo.h"

#include "AST.h"

struct Expr;
struct Scope;

struct Decl final: public ASTNode {
    std::optional<SourceRange> externRange;
    std::optional<SourceRange> keywordRange;
    Ident name;
    Type type;
    bool isVar;
    std::vector<Decl*> paramList;
    enum {
        Unresolved, Resolving, Resolved
    } protoState = Unresolved;
private:
    // FIXME: Make this into a variant.
    ASTNode* value = nullptr;
public:
    Expr* expression() const;
    Scope* body() const;
    bool hasDefinition() const { return (bool)value; }

    /// Constructor for bare declarations, used for parameters and struct fields.
    Decl(Ident name, Type type, bool isVar) : name(name), type(type), isVar(isVar) {}

    /// Constructor for stored declarations.
    Decl(std::optional<SourceRange> externRange, SourceRange keywordRange, Ident name, Type type, bool isVar, std::vector<Decl*> paramList, Expr* expression) : externRange(externRange), keywordRange(keywordRange), name(name), type(type), isVar(isVar), paramList(paramList), value(expression) {}

    /// Constructor for computed declarations.
    Decl(std::optional<SourceRange> externRange, SourceRange keywordRange, Ident name, Type type, bool isVar, std::vector<Decl*> paramList, Scope* body) : externRange(externRange), keywordRange(keywordRange), name(name), type(type), isVar(isVar), paramList(paramList), value(body) {}

    /// Constructor for declaration prototypes.
    Decl(std::optional<SourceRange> externRange, SourceRange keywordRange, Ident name, Type type, bool isVar, std::vector<Decl*> paramList) : externRange(externRange), keywordRange(keywordRange), name(name), type(type), isVar(isVar), paramList(paramList) {}

    // Opposites:
    bool isStored() const;
    bool isComputed() const;

    bool isExtern() const { return (bool)externRange; }

    SourceRange protoRange() const {
        auto range = name.range;
        if(externRange) range += *externRange;
        if(keywordRange) range += *keywordRange;
        return range;
    }
    SourceRange totalRange() const override {
        // INCOMPLETE.
        return protoRange();
    }
};

struct StructDecl final: public ASTNode {
    SourceRange structRange;
    Ident name;
    std::vector<Decl*> fields;
    enum {
        Unresolved, Resolving, Resolved
    } state = Unresolved;

    StructDecl(SourceRange structRange, Ident name, std::vector<Decl*> fields) : structRange(structRange), name(name), fields(fields) {}
    SourceRange totalRange() const override {
        // INCOMPLETE.
        return structRange;
    }
};
