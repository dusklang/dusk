//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "ASTPrinter.h"

std::string indentation(int level) {
    const int multiplier = 4;
    std::string str;
    for(int i = 0; i < (level * multiplier); i++) str += " ";
    return str;
}

std::string ASTPrinter::visitDecl(std::shared_ptr<Decl> decl, int indentationLevel) {
    std::string str = indentation(indentationLevel);
    str += decl->isMut ? "mut " : "";
    str += decl->isExtern ? "extern " : "";
    str += decl->name;
    if(!decl->paramList.empty()) {
        str += "(";
        bool first = true;
        for(auto& param: decl->paramList) {
            if(!first) str += ", ";
            else first = false;

            str += param->name + ": " + param->type.name();
        }
        str += ")";
    }
    std::string typeStr;
    if(decl->type.isInferred()) {
        typeStr = "<inferred>";
    } else {
        typeStr = decl->type.name();
    }
    str += ": " + typeStr;

    if(decl->expression()) {
        str += " = " + visitExpr(decl->expression(), 0);
    }
    else if(decl->body()) {
        str += " {\n" + visitScope(decl->body(), indentationLevel) + "\n" + indentation(indentationLevel) + "}";
    }
    return str;
}

std::string ASTPrinter::visitScope(std::shared_ptr<Scope> scope, int indentationLevel) {
    std::string str;
    for(auto& node: scope->nodes)
        str += visit(node, indentationLevel + 1) + "\n";
    if(!str.empty()) str.pop_back();
    return str;
}

std::string ASTPrinter::visitArgument(std::shared_ptr<Argument> argument, int indentationLevel) {
    std::string str;
    if(argument->label) str += *argument->label + ": ";
    return str + visitExpr(argument->value, 0);
}

std::string ASTPrinter::visitIntegerLiteralExpr(std::shared_ptr<IntegerLiteralExpr> expr, int indentationLevel) {
    return indentation(indentationLevel) + expr->literal;
}

std::string ASTPrinter::visitDecimalLiteralExpr(std::shared_ptr<DecimalLiteralExpr> expr, int indentationLevel) {
    return indentation(indentationLevel) + expr->literal;
}

std::string ASTPrinter::visitBooleanLiteralExpr(std::shared_ptr<BooleanLiteralExpr> expr, int indentationLevel) {
    return indentation(indentationLevel) + std::string(expr->literal ? "true" : "false");
}

std::string ASTPrinter::visitCharLiteralExpr(std::shared_ptr<CharLiteralExpr> expr, int indentationLevel) {
    return indentation(indentationLevel) + expr->literal;
}

std::string ASTPrinter::visitDeclRefExpr(std::shared_ptr<DeclRefExpr> expr, int indentationLevel) {
    std::string str = expr->name;
    if(!expr->argList.empty()) {
        str += "(";
        bool first = true;
        for(auto& arg: expr->argList) {
            if(!first) str += ", ";
            else first = false;

            str += visitArgument(std::make_shared<Argument>(arg), 0);
        }
        str += ")";
    }
    return indentation(indentationLevel) + str;
}

std::string ASTPrinter::visitReturnStmt(std::shared_ptr<ReturnStmt> stmt, int indentationLevel) {
    std::string str = indentation(indentationLevel) + "return";
    if(stmt->value) {
        str += " " + visitExpr(stmt->value, 0);
    }
    return str;
}

std::string ASTPrinter::visitAssignmentStmt(std::shared_ptr<AssignmentStmt> stmt, int indentationLevel) {
    return indentation(indentationLevel) + visitExpr(stmt->lhs, 0) + " = " + visitExpr(stmt->rhs, 0);
}

std::string ASTPrinter::visitIfStmt(std::shared_ptr<IfStmt> stmt, int indentationLevel, bool isIfElse) {
    std::string str;
    if(!isIfElse) str = indentation(indentationLevel);
    str += "if " + visitExpr(stmt->condition, 0) + " {\n";
    str += visitScope(stmt->thenScope, indentationLevel);
    str += "\n" + indentation(indentationLevel) + "}";
    if(stmt->elseScope) {
        str += " else ";
        // Handle else if.
        if((*stmt->elseScope)->nodes.size() == 1) {
            if(auto elseIf = std::dynamic_pointer_cast<IfStmt>((*stmt->elseScope)->nodes[0])) {
                str += visitIfStmt(elseIf, indentationLevel, true);
            } else {
                // TODO: A goto really shouldn't be necessary here.
                goto notIfElse;
            }
        } else {
            notIfElse:
            str += "{\n";
            str += visitScope(*stmt->elseScope, indentationLevel);
            str += "\n" + indentation(indentationLevel) + "}";
        }
    }
    return str;
}

std::string ASTPrinter::visitWhileStmt(std::shared_ptr<WhileStmt> stmt, int indentationLevel) {
    std::string str = indentation(indentationLevel) + "while " + visitExpr(stmt->condition, 0) + " {\n";
    str += visitScope(stmt->thenScope, indentationLevel);
    str += "\n" + indentation(indentationLevel) + "}";
    return str;
}
