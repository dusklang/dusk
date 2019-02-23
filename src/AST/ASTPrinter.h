#pragma once

#include "ASTVisitor.h"
#include "General/SourceInfo.h"

class ASTPrinter: public ASTVisitor<ASTPrinter,
                                    /*ASTNodeReturnTy*/ void,
                                    #define AST_NODE(_) void,
                                    #include "ASTNodes.def"
                                    /*Arguments*/int, std::ostream&>
{
public:
    void visitDecl(Decl* decl, int indentationLevel, std::ostream& stream);
    void visitStructDecl(StructDecl* decl, int indentationLevel, std::ostream& stream);
    void visitScope(Scope* scope, int indentationLevel, std::ostream& stream);

    void visitIntegerLiteralExpr(IntegerLiteralExpr* expr, int indentationLevel, std::ostream& stream);
    void visitDecimalLiteralExpr(DecimalLiteralExpr* expr, int indentationLevel, std::ostream& stream);
    void visitBooleanLiteralExpr(BooleanLiteralExpr* expr, int indentationLevel, std::ostream& stream);
    void visitCharLiteralExpr(CharLiteralExpr* expr, int indentationLevel, std::ostream& stream);
    void visitStringLiteralExpr(StringLiteralExpr* expr, int indentationLevel, std::ostream& stream);
    void visitPreOpExpr(PreOpExpr* expr, int indentationLevel, std::ostream& stream);
    void visitBinOpExpr(BinOpExpr* expr, int indentationLevel, std::ostream& stream);
    void visitCastExpr(CastExpr* expr, int indentationLevel, std::ostream& stream);
    void visitDeclRefExpr(DeclRefExpr* expr, int indentationLevel, std::ostream& stream);
    void visitMemberRefExpr(MemberRefExpr* expr, int indentationLevel, std::ostream& stream);
    void visitReturnExpr(ReturnExpr* expr, int indentationLevel, std::ostream& stream);
    void visitIfExpr(IfExpr* expr, int indentationLevel, std::ostream& stream, bool isIfElse = false);
    void visitWhileExpr(WhileExpr* expr, int indentationLevel, std::ostream& stream);
    void visitDoExpr(DoExpr* expr, int indentationLevel, std::ostream& stream);
};
