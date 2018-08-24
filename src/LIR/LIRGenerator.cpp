//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "LIRGenerator.h"
#include "mpark/patterns.hpp"

using namespace mpark::patterns;

lir::Reg LIRGenerator::visitDecl(Decl* decl) {
    return -1;
}
void LIRGenerator::visitScope(Scope* scope) { }
void LIRGenerator::visitStructDecl(StructDecl* decl) { }
lir::Reg LIRGenerator::visitIntegerLiteralExpr(IntegerLiteralExpr* expr) {
    return -1;
}
lir::Reg LIRGenerator::visitDecimalLiteralExpr(DecimalLiteralExpr* expr) {
    return -1;
}
lir::Reg LIRGenerator::visitBooleanLiteralExpr(BooleanLiteralExpr* expr) {
    return -1;
}
lir::Reg LIRGenerator::visitCharLiteralExpr(CharLiteralExpr* expr) {
    return -1;
}
lir::Reg LIRGenerator::visitStringLiteralExpr(StringLiteralExpr* expr) {
    return -1;
}
lir::Reg LIRGenerator::visitPreOpExpr(PreOpExpr* expr) {
    return -1;
}
lir::Reg LIRGenerator::visitBinOpExpr(BinOpExpr* expr) {
    return -1;
}
lir::Reg LIRGenerator::visitCastExpr(CastExpr* expr) {
    return -1;
}
lir::Reg LIRGenerator::visitDeclRefExpr(DeclRefExpr* expr) {
    return -1;
}
lir::Reg LIRGenerator::visitMemberRefExpr(MemberRefExpr* expr) {
    return -1;
}

lir::Reg LIRGenerator::visitReturnExpr(ReturnExpr* expr) {
    return -1;
}
lir::Reg LIRGenerator::visitIfExpr(IfExpr* expr) {
    return -1;
}

void LIRGenerator::printIR() const {
    for(auto& entryPoint: entryPoints) {
        std::cout << entryPoint.first << ":\n\n";
    }
}
