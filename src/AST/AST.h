//  Copyright © 2019 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <vector>
#include <map>
#include <optional>
#include <algorithm>
#include "General/SourceInfo.h"
#include "Type.h"

struct Expr;

// Abstract class from which each node in the AST inherits.
struct ASTNode {
    virtual ~ASTNode() {}
    virtual SourceRange totalRange() const = 0;
};

/// Represents a collection of other nodes.
struct Scope final : public ASTNode {
    SourceRange range;
    Array<ASTNode*> nodes;
    Expr* terminalExpr = nullptr;
    Scope(SourceRange range, Array<ASTNode*> nodes) : range(range), nodes(nodes) {}
    SourceRange totalRange() const override { return range; }
    Type terminalType() const;
    Expr* terminalValueExpr() const;
};
