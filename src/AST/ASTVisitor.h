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

    ASTNodeReturnTy visit(ASTNode* node, Args... args) {
            #define AST_NODE(name) if(auto val = dynamic_cast<name*>(node)) { \
                if constexpr(std::is_same<void, ASTNodeReturnTy>::value) { \
                    static_cast<Impl*>(this)->visit##name(val, std::forward<Args>(args)...); \
                    return; \
                } else { \
                    return static_cast<Impl*>(this)->visit##name(val, std::forward<Args>(args)...); \
                } \
            }
            #include "ASTNodes.def"
        unreachable;
    }

    auto visit(Array<ASTNode*> nodes, Args... args) {
        if constexpr(std::is_same_v<void, ASTNodeReturnTy>) {
            for(auto node: nodes) {
                visit(node, std::forward<Args>(args)...);
            }
        } else {
            Array<ASTNodeReturnTy> retVal;
            for(auto node: nodes) {
                retVal.append(node, visit(std::forward<Args>(args)...));
            }
            return retVal;
        }
    }

    ExprRetTy visitExpr(Expr* expr, Args... args) {
        #define EXPR_NODE(name) if(auto val = dynamic_cast<name##Expr*>(expr)) { \
            return static_cast<Impl*>(this)->visit##name##Expr(val, std::forward<Args>(args)...); \
        }
        #include "ASTNodes.def"
        unreachable;
    }
};
