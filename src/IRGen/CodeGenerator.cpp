//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "CodeGenerator.hpp"

void CodeGenerator::visitDecl(Decl* decl, int indentationLevel) {
}
void CodeGenerator::visitDeclPrototype(DeclPrototype* prototype, int indentationLevel) {

}
void CodeGenerator::visitScope(Scope* scope, int indentationLevel) {

}
void CodeGenerator::visitParam(Param* param, int indentationLevel) {
    
}
void CodeGenerator::visitArgument(Argument* argument, int indentationLevel) {

}
llvm::Value* CodeGenerator::visitIntegerLiteralExpr(IntegerLiteralExpr* expr) {
    return llvm::ConstantInt::get(context, llvm::APInt(32, std::stoi(expr->literal)));
}
llvm::Value* CodeGenerator::visitDecimalLiteralExpr(DecimalLiteralExpr* expr) {
    return nullptr;
}
llvm::Value* CodeGenerator::visitDeclRefExpr(DeclRefExpr* expr) {
    return nullptr;
}
llvm::Value* CodeGenerator::visitPlaceholderTypeRefExpr(PlaceholderTypeRefExpr* expr) {
    return nullptr;
}
