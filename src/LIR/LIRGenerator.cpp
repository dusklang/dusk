//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "LIRGenerator.h"
#include "mpark/patterns.hpp"

using namespace mpark::patterns;

lir::Value* LIRGenerator::visitDecl(Decl* decl) {
    return nullptr;
}
lir::Value* LIRGenerator::visitScope(Scope* scope) {
    return nullptr;
}
lir::Value* LIRGenerator::visitStructDecl(StructDecl* decl) {
    return nullptr;
}
lir::Value* LIRGenerator::visitIntegerLiteralExpr(IntegerLiteralExpr* expr) {
    return nullptr;
}
lir::Value* LIRGenerator::visitDecimalLiteralExpr(DecimalLiteralExpr* expr) {
    return nullptr;
}
lir::Value* LIRGenerator::visitBooleanLiteralExpr(BooleanLiteralExpr* expr) {
    return nullptr;
}
lir::Value* LIRGenerator::visitCharLiteralExpr(CharLiteralExpr* expr) {
    return nullptr;
}
lir::Value* LIRGenerator::visitStringLiteralExpr(StringLiteralExpr* expr) {
    return nullptr;
}
lir::Value* LIRGenerator::visitPreOpExpr(PreOpExpr* expr) {
    return nullptr;
}
lir::Value* LIRGenerator::visitBinOpExpr(BinOpExpr* expr) {
    return nullptr;
}
lir::Value* LIRGenerator::visitCastExpr(CastExpr* expr) {
    return nullptr;
}
lir::Value* LIRGenerator::visitDeclRefExpr(DeclRefExpr* expr) {
    return nullptr;
}
lir::Value* LIRGenerator::visitMemberRefExpr(MemberRefExpr* expr) {
    return nullptr;
}
lir::Value* LIRGenerator::visitReturnExpr(ReturnExpr* expr) {
    return nullptr;
}
lir::Value* LIRGenerator::visitIfExpr(IfExpr* expr) {
    return nullptr;
}
lir::Value* LIRGenerator::visitWhileExpr(WhileExpr* expr) {
    return nullptr;
}

void LIRGenerator::printIR() const {}
