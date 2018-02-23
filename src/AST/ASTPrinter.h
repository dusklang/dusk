//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include "ASTVisitor.h"

class ASTPrinter: public ASTVisitor<ASTPrinter,
                                    /*ASTNodeReturnTy*/ std::string,
                                    #define AST_NODE(_) std::string,
                                    #include "ASTNodes.def"
                                    int>
{
public:
    std::string visitDecl(std::shared_ptr<Decl> decl, int indentationLevel);
    std::string visitDeclPrototype(std::shared_ptr<DeclPrototype> prototype, int indentationLevel);
    std::string visitScope(std::shared_ptr<Scope> scope, int indentationLevel);
    std::string visitParam(std::shared_ptr<Param> param, int indentationLevel);
    std::string visitArgument(std::shared_ptr<Argument> argument, int indentationLevel);
    std::string visitPhysicalTypeRef(std::shared_ptr<PhysicalTypeRef> expr, int indentationLevel);

    std::string visitIntegerLiteralExpr(std::shared_ptr<IntegerLiteralExpr> expr, int indentationLevel);
    std::string visitDecimalLiteralExpr(std::shared_ptr<DecimalLiteralExpr> expr, int indentationLevel);
    std::string visitDeclRefExpr(std::shared_ptr<DeclRefExpr> expr, int indentationLevel);

    std::string visitReturnStmt(std::shared_ptr<ReturnStmt> stmt, int indentationLevel);
};
