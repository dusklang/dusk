//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <iostream>
#include <map>

#include "AST/ASTVisitor.h"
#include "Constraint.h"
#include "Solution.h"
#include "llvm/ADT/Optional.h"

using llvm::Optional;
using llvm::None;

struct SolutionApplier: public ASTVisitor<SolutionApplier> {
    Solution const solution;

    SolutionApplier(Solution const& solution) : solution(solution) {}

    void visitDecl(std::shared_ptr<Decl> decl);
    void visitScope(std::shared_ptr<Scope> scope);
    void visitArgument(std::shared_ptr<Argument> argument) {}
    void visitIntegerLiteralExpr(std::shared_ptr<IntegerLiteralExpr> expr);
    void visitDecimalLiteralExpr(std::shared_ptr<DecimalLiteralExpr> expr);
    void visitBooleanLiteralExpr(std::shared_ptr<BooleanLiteralExpr> expr);
    void visitCharLiteralExpr(std::shared_ptr<CharLiteralExpr> expr);
    void visitStringLiteralExpr(std::shared_ptr<StringLiteralExpr> expr);
    void visitDeclRefExpr(std::shared_ptr<DeclRefExpr> expr);

    void visitReturnStmt(std::shared_ptr<ReturnStmt> stmt);
    void visitAssignmentStmt(std::shared_ptr<AssignmentStmt> stmt);
    void visitIfStmt(std::shared_ptr<IfStmt> stmt);
    void visitWhileStmt(std::shared_ptr<WhileStmt> stmt);
};
