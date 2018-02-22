//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include "ASTVisitor.hpp"

class ASTPrinter: public ASTVisitor<ASTPrinter,
                                    /*ASTNodeReturnTy*/ std::string,
                                    #define AST_NODE(_) std::string,
                                    #include "ASTNodes.def"
                                    int>
{
public:
    std::string visitDecl(Decl* decl, int indentationLevel);
    std::string visitDeclPrototype(DeclPrototype* prototype, int indentationLevel);
    std::string visitScope(Scope* scope, int indentationLevel);
    std::string visitParam(Param* param, int indentationLevel);
    std::string visitArgument(Argument* argument, int indentationLevel);

    std::string visitIntegerLiteralExpr(IntegerLiteralExpr* expr, int indentationLevel);
    std::string visitDecimalLiteralExpr(DecimalLiteralExpr* expr, int indentationLevel);
    std::string visitDeclRefExpr(DeclRefExpr* expr, int indentationLevel);
    std::string visitPlaceholderTypeRefExpr(PlaceholderTypeRefExpr* expr, int indentationLevel);

    std::string visitReturnStmt(ReturnStmt* stmt, int indentationLevel);
};
