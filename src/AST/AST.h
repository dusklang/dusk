//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <string>
#include <vector>
#include <map>
#include "General/SourceLoc.h"

struct Expr;

enum class NodeKind {
    #define AST_NODE(name) name,
    #include "ASTNodes.def"
    NUM_NODES
};

// Abstract class from which each node in the AST inherits.
struct ASTNode {
    NodeKind kind;
    SourceRange range;
    ASTNode(NodeKind kind, SourceRange range) : kind(kind), range(range) {}
    virtual ~ASTNode() {}
};

// A scope node represents a collection of other nodes.
struct Scope final : public ASTNode {
    std::vector<ASTNode*> nodes;

    Scope(SourceRange range, std::vector<ASTNode*> nodes): ASTNode(NodeKind::Scope, range), nodes(nodes) {}

    ~Scope() override {
        for (ASTNode* node: nodes) {
            delete node;
        }
    }
};
