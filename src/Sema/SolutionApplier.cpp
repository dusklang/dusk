//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "SolutionApplier.h"

void SolutionApplier::visitDecl(std::shared_ptr<Decl> decl) {
    decl->type.substitute(solution.types);
    for(auto& param: decl->paramList) {
        param->type.substitute(solution.types);
    }
}
void SolutionApplier::visitScope(std::shared_ptr<Scope> scope) {
    for(auto& node: scope->nodes) {
        visit(node);
    }
}
void SolutionApplier::visitIntegerLiteralExpr(std::shared_ptr<IntegerLiteralExpr> expr) {
    expr->type.substitute(solution.types);
}
void SolutionApplier::visitDecimalLiteralExpr(std::shared_ptr<DecimalLiteralExpr> expr) {
    expr->type.substitute(solution.types);
}
void SolutionApplier::visitBooleanLiteralExpr(std::shared_ptr<BooleanLiteralExpr> expr) {
    expr->type.substitute(solution.types);
}
void SolutionApplier::visitCharLiteralExpr(std::shared_ptr<CharLiteralExpr> expr) {
    expr->type.substitute(solution.types);
}
void SolutionApplier::visitStringLiteralExpr(std::shared_ptr<StringLiteralExpr> expr) {
    expr->type.substitute(solution.types);
}
void SolutionApplier::visitDeclRefExpr(std::shared_ptr<DeclRefExpr> expr) {
    expr->type.substitute(solution.types);

    // Constrain arguments.
    for(auto& arg: expr->argList) {
        visitExpr(arg.value);
    }
}
void SolutionApplier::visitReturnStmt(std::shared_ptr<ReturnStmt> stmt) {
    if(!stmt->value) {
        visitExpr(stmt->value);
    }
}
void SolutionApplier::visitAssignmentStmt(std::shared_ptr<AssignmentStmt> stmt) {
    visitDeclRefExpr(stmt->lhs);
    visitExpr(stmt->rhs);
}
void SolutionApplier::visitIfStmt(std::shared_ptr<IfStmt> stmt) {
    visitExpr(stmt->condition);
    visitScope(stmt->thenScope);
    if(stmt->elseScope) {
        visitScope(*stmt->elseScope);
    }
}
void SolutionApplier::visitWhileStmt(std::shared_ptr<WhileStmt> stmt) {
    visitExpr(stmt->condition);
    visitScope(stmt->thenScope);
}
