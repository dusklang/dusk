//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <memory>
#include <string>

#include "llvm/IR/Value.h"

#include "AST.h"
#include "Expr.h"
#include "Types.h"

struct Expr;
struct ParamDecl;

struct Decl: public ASTNode {
    std::string name;
    Type type;
    bool isMut;
    bool isExtern;

    std::vector<std::shared_ptr<ParamDecl>> paramList;

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
                  Type type,
                  bool isMut = false,
                  bool isExtern = false,
                  std::vector<std::shared_ptr<ParamDecl>> paramList = std::vector<std::shared_ptr<ParamDecl>>(),
                  std::shared_ptr<Expr> expression = nullptr),
    name(name), type(type), isMut(isMut), isExtern(isExtern), paramList(paramList),
    value(std::dynamic_pointer_cast<ASTNode>(expression)) {}

    AST_NODE_CTOR(Decl,
                  std::string name,
                  Type type,
                  bool isMut,
                  bool isExtern,
                  std::vector<std::shared_ptr<ParamDecl>> paramList,
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

// For a ParamDecl, the inherited name field represents the internal name of the parameter, while the
// label field represents the *external* label for the parameter. If only a name is specified, like so...
//     def myFunction(intParameter: i32): Void
// ...that name will be copied into label. If an underscore instead of a label precedes the name of
// the function...
//     def myFunction(_ intParameter: i32): Void
// ...label will be None.
struct ParamDecl: public Decl {
    llvm::Optional<std::string> label;

    ParamDecl(SourceRange range,
              llvm::Optional<std::string> label,
              std::string name,
              Type type) :
    Decl(range, name, type), label(label) {}
};
