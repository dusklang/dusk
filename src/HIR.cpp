#include <iostream>

#include "HIR.h"

using namespace hir;

ExprID Builder::intLit(uint64_t literal) {
    ExprID id(levels.count());
    levels.append(0);
    assert(!expressions.isEmpty());
    expressions.first()->append(Expr(ExprKind::IntLit, id));
    return id;
}
ExprID Builder::decLit(double literal) {
    ExprID id(levels.count());
    levels.append(0);
    expressions.first()->append(Expr(ExprKind::DecLit, id));
    return id;
}
ExprID Builder::binOp(BinOp op, ExprID lhs, ExprID rhs) {
    ExprID id(levels.count());
    size_t level = max(levels[lhs.id], levels[rhs.id]) + 1;
    levels.append(level);
    while(expressions.count() < level + 1) {
        expressions.append(Array<Expr>());
    }
    expressions[level].append(Expr(id, op, lhs, rhs));
    return id;
}

void Program::debugPrint() {
    for(auto [i, level]: zip(expressions.indices(), expressions)) {
        std::cout << "Level " << i << ":\n";
        for(auto expr: level) {
            std::cout << expr.id.id << ": ";
            switch(expr.kind) {
                case ExprKind::IntLit: {
                    std::cout << "integer literal";
                } break;
                case ExprKind::DecLit: {
                    std::cout << "decimal literal";
                } break;
                case ExprKind::BinOp: {
                    std::cout << "#" << expr.binOp.lhs.id << ' ';
                    std::cout << symbol(expr.binOp.op);
                    std::cout << " #" << expr.binOp.rhs.id;
                } break;
            }
            std::cout << '\n';
        }
    }
}
