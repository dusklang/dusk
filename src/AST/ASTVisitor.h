//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include "General/General.h"

#include "AST.h"
#define AST_NODE(name) struct name;
#define EXPR_NODE(name) struct name##Expr;
#include "ASTNodes.def"

#include "Expr.h"
#include "Decl.h"

template<typename Impl,
         typename ASTNodeReturnTy = void,
         #define AST_NODE(name) typename name##RetTy = void,
         #include "ASTNodes.def"
         typename... Args>
class ASTVisitor {
public:
    ASTVisitor() {}
    ~ASTVisitor() {}

    ASTNodeReturnTy visit(ASTNode* node, Args... args) {
        switch(node->kind) {
            #define AST_NODE(name) case NodeKind::name: { \
                if constexpr(std::is_same<void, ASTNodeReturnTy>::value) { \
                    static_cast<Impl*>(this)->visit##name(dynamic_cast<name*>(node), std::forward<Args>(args)...); \
                    return; \
                } else { \
                    return static_cast<Impl*>(this)->visit##name(dynamic_cast<name*>(node), std::forward<Args>(args)...); \
                } \
            }
            #include "ASTNodes.def"
            default: break;
        }
        unreachable;
    }

    auto visit(std::vector<ASTNode*> nodes, Args... args) {
        if constexpr(std::is_same_v<void, ASTNodeReturnTy>) {
            for(auto node: nodes) {
                visit(node, std::forward(args)...);
            }
        } else {
            std::vector<ASTNodeReturnTy> retVal;
            for(auto node: nodes) {
                retVal.push_back(node, visit(std::forward(args)...));
            }
            return retVal;
        }
    }

    #define UNTYPED_AST_NODE(name) name##RetTy visit##name(name* node, Args... args) { \
        return static_cast<Impl*>(this)->visit##name(node, std::forward<Args>(args)...); \
    }
    #include "ASTNodes.def"

    ExprRetTy visitExpr(Expr* expr, Args... args) {
        switch(expr->exprKind) {
        #define EXPR_NODE(name) case ExprKind::name: \
            return static_cast<Impl*>(this)->visit##name##Expr(static_cast<name##Expr*>(expr), std::forward<Args>(args)...);
            #include "ASTNodes.def"
            default: break;
        }
        unreachable;
    }
};
