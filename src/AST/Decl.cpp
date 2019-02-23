#include "Expr.h"
#include "Decl.h"

Expr* Decl::expression() const {
    return dynamic_cast<Expr*>(value);
}

Scope* Decl::body() const {
    return dynamic_cast<Scope*>(value);
}

bool Decl::isStored() const {
    return !isComputed();
}

bool Decl::isComputed() const {
    if(!hasDefinition() && isExtern() && !isVar) return true;
    return (bool)dynamic_cast<Scope*>(value);
}
