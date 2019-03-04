#include <iostream>

#include "HIR.h"

using namespace hir;

ExprID Builder::intLit(uint64_t literal) {
    ExprID id(levels.count());
    levels.append(0);
    expressions.first()->append(Expr(ExprKind::IntLit, id));
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

void Builder::debugPrint() const {
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
                    std::cout << "#" << expr.binOp.lhs.id << " ";
                    switch(expr.binOp.op) {
                        case BinOp::Mult: {
                            std::cout << '*';
                        } break;
                        case BinOp::Div: {
                            std::cout << '/';
                        } break;
                        case BinOp::Mod: {
                            std::cout << '%';
                        } break;
                        case BinOp::Add: {
                            std::cout << '+';
                        } break;
                        case BinOp::Sub: {
                            std::cout << '-';
                        } break;
                        case BinOp::Less: {
                            std::cout << '<';
                        } break;
                        case BinOp::LessOrEq: {
                            std::cout << "<=";
                        } break;
                        case BinOp::Greater: {
                            std::cout << ">";
                        } break;
                        case BinOp::GreaterOrEq: {
                            std::cout << ">=";
                        } break;
                        case BinOp::Eq: {
                            std::cout << "==";
                        } break;
                        case BinOp::NotEq: {
                            std::cout << "!=";
                        } break;
                        case BinOp::BitwiseAnd: {
                            std::cout << '&';
                        } break;
                        case BinOp::BitwiseOr: {
                            std::cout << '|';
                        } break;
                        case BinOp::LogicalAnd: {
                            std::cout << "&&";
                        } break;
                        case BinOp::LogicalOr: {
                            std::cout << "||";
                        } break;
                        case BinOp::Assignment: {
                            std::cout << '=';
                        } break;
                    }
                    std::cout << " #" << expr.binOp.rhs.id;
                } break;
            }
        }
    }
}
