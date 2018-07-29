//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "Expr.h"
#include "Decl.h"

Decl::Decl(SourceRange range,
     std::string name,
     Type type,
     bool isVar,
     bool isExtern,
     std::vector<Decl*> paramList,
     Expr* expression) :
ASTNode(NodeKind::Decl, range),
name(name), type(type), isVar(isVar), isExtern(isExtern), paramList(paramList),
value(expression) {}

Decl::Decl(SourceRange range,
           std::string name,
           Type type,
           bool isVar,
           bool isExtern,
           std::vector<Decl*> paramList,
           Scope* body) :
ASTNode(NodeKind::Decl, range),
name(name), type(type), isVar(isVar), isExtern(isExtern), paramList(paramList),
value(body) {}

Expr* Decl::expression() const {
    return dynamic_cast<Expr*>(value);
}

Scope* Decl::body() const {
    return dynamic_cast<Scope*>(value);
}

bool Decl::isStored() const {
    return (bool)dynamic_cast<Expr*>(value);
}

bool Decl::isComputed() const {
    if(!hasDefinition() && isExtern) return true;
    return (bool)dynamic_cast<Scope*>(value);
}
