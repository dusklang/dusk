//  Copyright © 2019 Zach Wolfe. All rights reserved.

#include <iostream>
#include "ASTPrinter.h"
#include "mpark/patterns.hpp"
using namespace mpark::patterns;

int constexpr multiplier = 4;
inline void indent(int level, std::ostream& stream) {
    for(int i = 0; i < (level * multiplier); i++) stream << " ";
}

void ASTPrinter::visitDecl(Decl* decl, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    stream << (decl->isVar ? "var " : "") << (decl->isExtern() ? "extern " : "");
    stream << decl->name;
    if(!decl->paramList.isEmpty()) {
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

void ASTPrinter::visitStructDecl(StructDecl* decl, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    stream << "struct " << decl->name << " {";
    for(auto field: decl->fields) {
        visitDecl(field, indentationLevel + 1, stream);
    }
    indent(indentationLevel, stream);
    stream << "}\n";
}

void ASTPrinter::visitScope(Scope* scope, int indentationLevel, std::ostream& stream) {
    for(size_t i = 0; i < scope->nodes.count(); ++i) {
        visit(scope->nodes[i], indentationLevel + 1, stream);
        /*if(i != scope->nodes.size() - 1) */stream << '\n';
    }
    indent(indentationLevel + 1, stream);
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
    stream << '"' << expr->literal << '"';
}

void ASTPrinter::visitStringLiteralExpr(StringLiteralExpr* expr, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    stream << '"' << expr->literal << '"';
}

void ASTPrinter::visitPreOpExpr(PreOpExpr* expr, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    switch(expr->op) {
        case PreOp::Positive:
            stream << '+';
            break;
        case PreOp::Negative:
            stream << '-';
            break;
        case PreOp::Deref:
            stream << '*';
            break;
        case PreOp::Not:
            stream << '!';
            break;
        case PreOp::AddrOf:
            stream << '&';
            break;
    }
    visitExpr(expr->operand, 0, stream);
}

void ASTPrinter::visitBinOpExpr(BinOpExpr* expr, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    visitExpr(expr->lhs, 0, stream);
    stream << ' ';
    switch(expr->op) {
        case BinOp::Add:
            stream << '+';
            break;
        case BinOp::Sub:
            stream << '-';
            break;
        case BinOp::Mult:
            stream << '*';
            break;
        case BinOp::Div:
            stream << '/';
            break;
        case BinOp::Mod:
            stream << '%';
            break;
        case BinOp::BitwiseOr:
            stream << '|';
            break;
        case BinOp::Or:
            stream << "||";
            break;
        case BinOp::BitwiseAnd:
            stream << '&';
            break;
        case BinOp::And:
            stream << "&&";
            break;
        case BinOp::Assignment:
            stream << '=';
            break;
        case BinOp::AddAssignment:
            stream << "+=";
            break;
        case BinOp::SubAssignment:
            stream << "-=";
            break;
        case BinOp::MultAssignment:
            stream << "*=";
            break;
        case BinOp::DivAssignment:
            stream << "/=";
            break;
        case BinOp::ModAssignment:
            stream << "%=";
            break;
        case BinOp::AndAssignment:
            stream << "&=";
            break;
        case BinOp::OrAssignment:
            stream << "|=";
            break;
        case BinOp::Equal:
            stream << "==";
            break;
        case BinOp::NotEqual:
            stream << "!=";
            break;
        case BinOp::LessThan:
            stream << '<';
            break;
        case BinOp::LessThanOrEqual:
            stream << "<=";
            break;
        case BinOp::GreaterThan:
            stream << '>';
            break;
        case BinOp::GreaterThanOrEqual:
            stream << ">=";
            break;
    }
    stream << ' ';
    visitExpr(expr->rhs, 0, stream);
}

void ASTPrinter::visitCastExpr(CastExpr* expr, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    visitExpr(expr->operand, 0, stream);
    stream << " as " << expr->type.name();
}

void ASTPrinter::visitDeclRefExpr(DeclRefExpr* expr, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    stream << expr->name;
    if(!expr->argList.isEmpty()) {
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

void ASTPrinter::visitMemberRefExpr(MemberRefExpr* expr, int indentationLevel, std::ostream& stream) {
    visitExpr(expr->root, indentationLevel, stream);
    stream << '.' << expr->name;
}

void ASTPrinter::visitReturnExpr(ReturnExpr* expr, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    stream << "return";
    if(expr->value) {
        stream << ' ';
        visitExpr(expr->value, 0, stream);
    }
}

void ASTPrinter::visitIfExpr(IfExpr* expr, int indentationLevel, std::ostream& stream, bool isIfElse) {
    if(!isIfElse) indent(indentationLevel, stream);
    stream << "if ";
    visitExpr(expr->condition, 0, stream);
    stream << " {\n";
    visitScope(expr->thenScope, indentationLevel, stream);
    stream << '\n';
    indent(indentationLevel, stream);
    stream << '}';

    if(auto scope = expr->elseScope) {
        stream << " else {\n";
        visitScope(scope, indentationLevel, stream);
        stream << '\n';
        indent(indentationLevel, stream);
        stream << '}';
    }
}

void ASTPrinter::visitWhileExpr(WhileExpr* expr, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    stream << "while ";
    visitExpr(expr->condition, 0, stream);
    stream << " {\n";
    visitScope(expr->thenScope, indentationLevel, stream);
    stream << "\n";
    indent(indentationLevel, stream);
    stream << "}";
}

void ASTPrinter::visitDoExpr(DoExpr* expr, int indentationLevel, std::ostream& stream) {
    indent(indentationLevel, stream);
    stream << "do ";
    match(expr->value)(
        pattern(as<Expr*>(arg)) = [&](auto expr) { visitExpr(expr, 0, stream); },
        pattern(as<Scope*>(arg)) = [&](auto scope) {
            stream << "{\n";
            visitScope(scope, indentationLevel + 1, stream);
            stream << '\n';
            indent(indentationLevel + 1, stream);
            stream << '}';
        }
    );
}
