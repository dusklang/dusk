//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <vector>
#include <map>
#include <optional>
#include <algorithm>
#include "General/SourceInfo.h"

// Abstract class from which each node in the AST inherits.
struct ASTNode {
    virtual ~ASTNode() {}
};

// A scope node represents a collection of other nodes.
struct Scope final : public ASTNode {
    SourceRange range;
    std::vector<ASTNode*> nodes;
    Scope(SourceRange range, std::vector<ASTNode*> nodes) : range(range), nodes(nodes) {}
};
