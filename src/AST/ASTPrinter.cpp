//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "ASTPrinter.h"

int constexpr multiplier = 4;
inline void indent(int level, std::ostream& stream) {
    for(int i = 0; i < (level * multiplier); i++) stream << " ";
}

void ASTPrinter::visitDecl(std::shared_ptr<Decl> decl, int indentationLevel, std::ostream& stream) {
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

void ASTPrinter::visitScope(std::shared_ptr<Scope> scope, int indentationLevel, std::ostream& stream) {
    for(int i = 0; i < scope->nodes.size(); ++i) {
        visit(scope->nodes[i], indentationLevel + 1, stream);
        if(i != scope->nodes.size() - 1) stream << '\n';
    }
}

void ASTPrinter::visitArgument(std::shared_ptr<Argument> argument, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    visitExpr(argument->value, 0, stream);
}

void ASTPrinter::visitIntegerLiteralExpr(std::shared_ptr<IntegerLiteralExpr> expr, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    stream << expr->literal;
}

void ASTPrinter::visitDecimalLiteralExpr(std::shared_ptr<DecimalLiteralExpr> expr, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    stream << expr->literal;
}

void ASTPrinter::visitBooleanLiteralExpr(std::shared_ptr<BooleanLiteralExpr> expr, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    stream << (expr->literal ? "true" : "false");
}

void ASTPrinter::visitCharLiteralExpr(std::shared_ptr<CharLiteralExpr> expr, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    stream << expr->range.getSubstring();
}

void ASTPrinter::visitStringLiteralExpr(std::shared_ptr<StringLiteralExpr> expr, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    stream << expr->range.getSubstring();
}

void ASTPrinter::visitDeclRefExpr(std::shared_ptr<DeclRefExpr> expr, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    stream << expr->name;
    if(!expr->argList.empty()) {
        stream << '(';
        bool first = true;
        for(auto& arg: expr->argList) {
            if(!first) stream << ", ";
            else first = false;

            visitArgument(std::make_shared<Argument>(arg), 0, stream);
        }
        stream << ')';
    }
}

void ASTPrinter::visitReturnStmt(std::shared_ptr<ReturnStmt> stmt, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    stream << "return";
    if(stmt->value) {
        stream << ' ';
        visitExpr(stmt->value, 0, stream);
    }
}

void ASTPrinter::visitAssignmentStmt(std::shared_ptr<AssignmentStmt> stmt, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    visitExpr(stmt->lhs, 0, stream);
    stream << " = ";
    visitExpr(stmt->rhs, 0, stream);
}

void ASTPrinter::visitIfStmt(std::shared_ptr<IfStmt> stmt, int indentationLevel, std::ostream& stream, bool isIfElse) {
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
        if((*stmt->elseScope)->nodes.size() == 1) {
            if(auto elseIf = std::dynamic_pointer_cast<IfStmt>((*stmt->elseScope)->nodes[0])) {
                visitIfStmt(elseIf, indentationLevel, stream, true);
            } else {
                // TODO: A goto really shouldn't be necessary here.
                goto notIfElse;
            }
        } else {
            notIfElse:
            stream << "{\n";
            visitScope(*stmt->elseScope, indentationLevel, stream);
            stream << '\n';
            indent(indentationLevel, stream);
            stream << '}';
        }
    }
}

void ASTPrinter::visitWhileStmt(std::shared_ptr<WhileStmt> stmt, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    stream << "while ";
    visitExpr(stmt->condition, 0, stream);
    stream << " {\n";
    visitScope(stmt->thenScope, indentationLevel, stream);
    stream << '\n';
    indent(indentationLevel, stream);
}
