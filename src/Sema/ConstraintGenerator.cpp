//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "ConstraintGenerator.h"

void ConstraintGenerator::visitDecl(std::shared_ptr<Decl> decl) { }
void ConstraintGenerator::visitScope(std::shared_ptr<Scope> scope) { }
void ConstraintGenerator::visitArgument(std::shared_ptr<Argument> argument) { }
void ConstraintGenerator::visitIntegerLiteralExpr(std::shared_ptr<IntegerLiteralExpr> expr) { }
void ConstraintGenerator::visitDecimalLiteralExpr(std::shared_ptr<DecimalLiteralExpr> expr) { }
void ConstraintGenerator::visitBooleanLiteralExpr(std::shared_ptr<BooleanLiteralExpr> expr) { }
void ConstraintGenerator::visitCharLiteralExpr(std::shared_ptr<CharLiteralExpr> expr) { }
void ConstraintGenerator::visitStringLiteralExpr(std::shared_ptr<StringLiteralExpr> expr) { }
void ConstraintGenerator::visitDeclRefExpr(std::shared_ptr<DeclRefExpr> expr) { }

void ConstraintGenerator::visitReturnStmt(std::shared_ptr<ReturnStmt> stmt) { }
void ConstraintGenerator::visitAssignmentStmt(std::shared_ptr<AssignmentStmt> stmt) { }
