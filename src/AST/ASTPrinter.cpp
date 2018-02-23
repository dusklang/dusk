//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "ASTPrinter.hpp"

std::string indentation(int level) {
    const int multiplier = 4;
    std::string str;
    for(int i = 0; i < (level * multiplier); i++) str += " ";
    return str;
}

std::string ASTPrinter::visitDecl(Decl* decl, int indentationLevel) {
    std::string str = indentation(indentationLevel) + visitDeclPrototype(&decl->prototype, 0);
    if(decl->expression()) {
        str += " = " + visitExpr(decl->expression().get(), 0);
    }
    else str += " {\n" + visitScope(decl->body().get(), indentationLevel) + "\n" + indentation(indentationLevel) + "}";
    return str;
}

std::string ASTPrinter::visitDeclPrototype(DeclPrototype* prototype, int indentationLevel) {
    std::string str;
    str += prototype->isMut ? "mut " : "";
    str += prototype->isExtern ? "extern " : "";
    str += prototype->name;
    if(!prototype->paramList.empty()) {
        str += "(";
        bool first = true;
        for(auto& param: prototype->paramList) {
            if(!first) str += ", ";
            else first = false;

            str += visitParam(&param, 0);
        }
        str += ")";
    }
    std::string typeStr;
    if(prototype->type) {
        typeStr = visitTypeRef(&*prototype->type, 0);
    } else {
        typeStr = "<inferred>";
    }
    str += ": " + typeStr;
    return indentation(indentationLevel) + str;
}

std::string ASTPrinter::visitScope(Scope* scope, int indentationLevel) {
    std::string str;
    for(auto& node: scope->nodes)
        str += visit(node.get(), indentationLevel + 1) + "\n";
    if(!str.empty()) str.pop_back();
    return str;
}

std::string ASTPrinter::visitParam(Param* param, int indentationLevel) {
    return param->name + ": " + visitTypeRef(&param->value, 0);
}

std::string ASTPrinter::visitArgument(Argument* argument, int indentationLevel) {
    return argument->name + ": " + visitExpr(argument->value.get(), 0);
}

std::string ASTPrinter::visitTypeRef(TypeRef* expr, int indentationLevel) {
    switch(expr->type) {
        #define BUILTIN_TYPE(name) case BuiltinType::name: return indentation(indentationLevel) + #name;
        #include "BuiltinTypes.def"
    }
}

std::string ASTPrinter::visitIntegerLiteralExpr(IntegerLiteralExpr* expr, int indentationLevel) {
    return indentation(indentationLevel) + expr->literal;
}

std::string ASTPrinter::visitDecimalLiteralExpr(DecimalLiteralExpr* expr, int indentationLevel) {
    return indentation(indentationLevel) + expr->literal;
}

std::string ASTPrinter::visitDeclRefExpr(DeclRefExpr* expr, int indentationLevel) {
    std::string str = expr->name;
    if(!expr->argList.empty()) {
        str += "(";
        bool first = true;
        for(auto& arg: expr->argList) {
            if(!first) str += ", ";
            else first = false;

            str += visitArgument(&arg, 0);
        }
        str += ")";
    }
    return indentation(indentationLevel) + str;
}

std::string ASTPrinter::visitReturnStmt(ReturnStmt* stmt, int indentationLevel) {
    std::string str = indentation(indentationLevel) + "return";
    if(stmt->value) {
        str += " " + visitExpr(stmt->value.get(), 0);
    }
    return str;
}
