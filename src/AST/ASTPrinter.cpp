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

            str += param->name + ": " + getNameForBuiltinType(param->type.getType());
        }
        str += ")";
    }
    std::string typeStr;
    if(decl->type.isInferred()) {
        typeStr = "<inferred>";
    } else {
        typeStr = getNameForBuiltinType(decl->type.getType());
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
    return argument->name + ": " + visitExpr(argument->value, 0);
}

std::string ASTPrinter::visitPhysicalTypeRef(std::shared_ptr<PhysicalTypeRef> expr, int indentationLevel) {
    return "";
}

std::string ASTPrinter::visitIntegerLiteralExpr(std::shared_ptr<IntegerLiteralExpr> expr, int indentationLevel) {
    return indentation(indentationLevel) + expr->literal;
}

std::string ASTPrinter::visitDecimalLiteralExpr(std::shared_ptr<DecimalLiteralExpr> expr, int indentationLevel) {
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
