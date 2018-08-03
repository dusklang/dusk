//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <iostream>
#include "ASTPrinter.h"

int constexpr multiplier = 4;
inline void indent(int level, std::ostream& stream) {
    for(int i = 0; i < (level * multiplier); i++) stream << " ";
}

void ASTPrinter::visitDecl(Decl* decl, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    stream << (decl->isVar ? "var " : "") << (decl->isExtern ? "extern " : "");
    stream << decl->name;
    if(!decl->paramList.empty()) {
        stream << "(";
        bool first = true;
        for(auto& param: decl->paramList) {
            if(!first) stream << ", ";
            else first = false;

            stream << param->name << ": " << param->type.name();
        }
        stream << ")";
    }
    stream << ": " << decl->type.name();

    if(decl->expression()) {
        stream << " = ";
        visitExpr(decl->expression(), 0, stream);
    }
    else if(decl->body()) {
        stream << " {\n";
        visitScope(decl->body(), indentationLevel, stream);
        stream << '\n';
        indent(indentationLevel, stream);
        stream << '}';
    }
}

void ASTPrinter::visitScope(Scope* scope, int indentationLevel, std::ostream& stream) {
    for(int i = 0; i < scope->nodes.size(); ++i) {
        visit(scope->nodes[i], indentationLevel + 1, stream);
        if(i != scope->nodes.size() - 1) stream << '\n';
    }
}

void ASTPrinter::visitIntegerLiteralExpr(IntegerLiteralExpr* expr, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    stream << expr->literal;
}

void ASTPrinter::visitDecimalLiteralExpr(DecimalLiteralExpr* expr, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    stream << expr->literal;
}

void ASTPrinter::visitBooleanLiteralExpr(BooleanLiteralExpr* expr, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    stream << (expr->literal ? "true" : "false");
}

void ASTPrinter::visitCharLiteralExpr(CharLiteralExpr* expr, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    stream << expr->range.getSubstring();
}

void ASTPrinter::visitStringLiteralExpr(StringLiteralExpr* expr, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    stream << expr->range.getSubstring();
}

void ASTPrinter::visitPrefixOpExpr(PrefixOpExpr* expr, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    switch(expr->op) {
        #define TOKEN_OPERATOR(name, string) case OperatorKind::name: stream << string; break;
        #include "Parser/TokenKinds.def"
    }
    visitExpr(expr->operand, 0, stream);
}

void ASTPrinter::visitBinOpExpr(BinOpExpr* expr, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    visitExpr(expr->lhs, 0, stream);
    stream << " ";
    switch(expr->op) {
        #define TOKEN_OPERATOR(name, string) case OperatorKind::name: stream << string; break;
        #include "Parser/TokenKinds.def"
    }
    stream << " ";
    visitExpr(expr->rhs, 0, stream);
}

void ASTPrinter::visitDeclRefExpr(DeclRefExpr* expr, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    stream << expr->name;
    if(!expr->argList.empty()) {
        stream << '(';
        bool first = true;
        for(auto* arg: expr->argList) {
            if(!first) stream << ", ";
            else first = false;

            visitExpr(arg, 0, stream);
        }
        stream << ')';
    }
}

void ASTPrinter::visitReturnStmt(ReturnStmt* stmt, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    stream << "return";
    if(stmt->value) {
        stream << ' ';
        visitExpr(stmt->value, 0, stream);
    }
}

void ASTPrinter::visitIfStmt(IfStmt* stmt, int indentationLevel, std::ostream& stream, bool isIfElse) {
    if(!isIfElse) indent(indentationLevel, stream);
    stream << "if ";
    visitExpr(stmt->condition, 0, stream);
    stream << " {\n";
    visitScope(stmt->thenScope, indentationLevel, stream);
    stream << '\n';
    indent(indentationLevel, stream);
    stream << '}';
    if(stmt->elseScope) {
        stream << " else ";
        // Handle else if.
        if(stmt->elseScope->nodes.size() == 1) {
            if(auto elseIf = dynamic_cast<IfStmt*>(stmt->elseScope->nodes[0])) {
                visitIfStmt(elseIf, indentationLevel, stream, true);
            } else {
                // TODO: A goto really shouldn't be necessary here.
                goto notIfElse;
            }
        } else {
            notIfElse:
            stream << "{\n";
            visitScope(stmt->elseScope, indentationLevel, stream);
            stream << '\n';
            indent(indentationLevel, stream);
            stream << '}';
        }
    }
}

void ASTPrinter::visitWhileStmt(WhileStmt* stmt, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    stream << "while ";
    visitExpr(stmt->condition, 0, stream);
    stream << " {\n";
    visitScope(stmt->thenScope, indentationLevel, stream);
    stream << "\n";
    indent(indentationLevel, stream);
    stream << "}";
}
