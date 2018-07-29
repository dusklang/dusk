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
    void visitDecl(Decl* decl, int indentationLevel, std::ostream& stream);
    void visitScope(Scope* scope, int indentationLevel, std::ostream& stream);

    void visitIntegerLiteralExpr(IntegerLiteralExpr* expr, int indentationLevel, std::ostream& stream);
    void visitDecimalLiteralExpr(DecimalLiteralExpr* expr, int indentationLevel, std::ostream& stream);
    void visitBooleanLiteralExpr(BooleanLiteralExpr* expr, int indentationLevel, std::ostream& stream);
    void visitCharLiteralExpr(CharLiteralExpr* expr, int indentationLevel, std::ostream& stream);
    void visitStringLiteralExpr(StringLiteralExpr* expr, int indentationLevel, std::ostream& stream);
    void visitDeclRefExpr(DeclRefExpr* expr, int indentationLevel, std::ostream& stream);

    void visitReturnStmt(ReturnStmt* stmt, int indentationLevel, std::ostream& stream);
    void visitAssignmentStmt(AssignmentStmt* stmt, int indentationLevel, std::ostream& stream);
    void visitIfStmt(IfStmt* stmt, int indentationLevel, std::ostream& stream, bool isIfElse = false);
    void visitWhileStmt(WhileStmt* stmt, int indentationLevel, std::ostream& stream);
};
