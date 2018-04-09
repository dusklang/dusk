//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include "ASTVisitor.h"

class ASTPrinter: public ASTVisitor<ASTPrinter,
                                    /*ASTNodeReturnTy*/ void,
                                    #define AST_NODE(_) void,
                                    #include "ASTNodes.def"
                                    /*Arguments*/int, std::ostream&>
{
public:
    void visitDecl(std::shared_ptr<Decl> decl, int indentationLevel, std::ostream& stream);
    void visitScope(std::shared_ptr<Scope> scope, int indentationLevel, std::ostream& stream);
    void visitArgument(std::shared_ptr<Argument> argument, int indentationLevel, std::ostream& stream);

    void visitIntegerLiteralExpr(std::shared_ptr<IntegerLiteralExpr> expr, int indentationLevel, std::ostream& stream);
    void visitDecimalLiteralExpr(std::shared_ptr<DecimalLiteralExpr> expr, int indentationLevel, std::ostream& stream);
    void visitBooleanLiteralExpr(std::shared_ptr<BooleanLiteralExpr> expr, int indentationLevel, std::ostream& stream);
    void visitCharLiteralExpr(std::shared_ptr<CharLiteralExpr> expr, int indentationLevel, std::ostream& stream);
    void visitStringLiteralExpr(std::shared_ptr<StringLiteralExpr> expr, int indentationLevel, std::ostream& stream);
    void visitDeclRefExpr(std::shared_ptr<DeclRefExpr> expr, int indentationLevel, std::ostream& stream);

    void visitReturnStmt(std::shared_ptr<ReturnStmt> stmt, int indentationLevel, std::ostream& stream);
    void visitAssignmentStmt(std::shared_ptr<AssignmentStmt> stmt, int indentationLevel, std::ostream& stream);
    void visitIfStmt(std::shared_ptr<IfStmt> stmt, int indentationLevel, std::ostream& stream, bool isIfElse = false);
    void visitWhileStmt(std::shared_ptr<WhileStmt> stmt, int indentationLevel, std::ostream& stream);
};
