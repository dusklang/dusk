//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#define AST_NODE(name) struct name;
#define EXPR_NODE(name) struct name##Expr;
#include "ASTNodes.def"

#include <memory>
#include "Expr.h"
#include "Stmt.h"

template<typename Impl,
         typename ASTNodeReturnTy,
         #define AST_NODE(name) typename name##RetTy = void,
         #include "ASTNodes.def"

         typename... Args>
class ASTVisitor {
public:
    ASTVisitor() {}
    ~ASTVisitor() {}

    ASTNodeReturnTy visit(std::shared_ptr<ASTNode> node, Args... args) {
        switch(node->kind) {
            #define AST_NODE(name) case NodeKind::name: { \
                if constexpr(std::is_same<void, ASTNodeReturnTy>::value) { \
                    static_cast<Impl*>(this)->visit##name(std::dynamic_pointer_cast<name>(node), std::forward<Args>(args)...); \
                    return; \
                } else { \
                    return static_cast<Impl*>(this)->visit##name(std::dynamic_pointer_cast<name>(node), std::forward<Args>(args)...); \
                } \
            }
            #include "ASTNodes.def"
            default: break;
        }
        LLVM_BUILTIN_UNREACHABLE;
    }

    #define UNTYPED_AST_NODE(name) name##RetTy visit##name(std::shared_ptr<name> node, Args... args) { \
        return static_cast<Impl*>(this)->visit##name(node, std::forward<Args>(args)...); \
    }
    #include "ASTNodes.def"

    // It would be kind of nice to be able to "iterate" over AST_TYPED_NODEs, and "iterate" over
    // their kinds in a switch, but alas I don't think this is possible with C++ macros because we
    // can't #include or #define other macros inside of a macro definition. Maybe with some awful
    // template wizardry? Or a boilerplate generator? Or maybe I'm just putting way too much thought
    // into something that doesn't actually matter that much?
    ExprRetTy visitExpr(std::shared_ptr<Expr> expr, Args... args) {
        switch(expr->exprKind) {
        #define EXPR_NODE(name) case ExprKind::name: \
            return static_cast<Impl*>(this)->visit##name##Expr(std::static_pointer_cast<name##Expr>(expr), std::forward<Args>(args)...);
            #include "ASTNodes.def"
            default: break;
        }
        LLVM_BUILTIN_UNREACHABLE;
    }

    StmtRetTy visitStmt(std::shared_ptr<Stmt> stmt, Args... args) {
        switch(stmt->stmtKind) {
            #define STMT_NODE(name) case StmtKind::name: \
                return static_cast<Impl*>(this)->visit##name##Stmt(std::static_pointer_cast<name##Stmt>(stmt), std::forward<Args>(args)...);
            #include "ASTNodes.def"
            default: break;
        }
        LLVM_BUILTIN_UNREACHABLE;
    }
};
