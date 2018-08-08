//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <vector>
#include <iostream>

#include "AST/Expr.h"
#include "AST/Stmt.h"
#include "AST/Decl.h"
#include "Parser.h"

#define EXPECT(token, message) if(current().isNot(token)) reportError(message)
#define EXPECT_NEXT(token, message) {\
    next();\
    EXPECT(token, message);\
}

#define TRY(result) [&]() {\
    lexer.saveState();\
    auto val = result;\
    if(!val) lexer.rollbackState();\
    return val;\
}()

// FIXME: Allocating a multi-dimensional array on the heap is
std::vector<std::vector<BinOp>> precedenceLevels {
    { BinOp::Mult, BinOp::Div, BinOp::Mod },

    { BinOp::Add, BinOp::Sub },

    {
        BinOp::LessThan,
        BinOp::LessThanOrEqual,
        BinOp::GreaterThan,
        BinOp::GreaterThanOrEqual
    },

    { BinOp::Equal, BinOp::NotEqual },

    { BinOp::And, BinOp::Or },

    {
        BinOp::Assignment,
        BinOp::AddAssignment,
        BinOp::SubAssignment,
        BinOp::MultAssignment,
        BinOp::DivAssignment,
        BinOp::ModAssignment
    }
};

std::optional<int> getPrecedence(BinOp op) {
    for(int level = 0; level < precedenceLevels.size(); ++level) {
        for(auto otherOp: precedenceLevels[level]) {
            if(op == otherOp) { return level; }
        }
    }
    return std::nullopt;
}

bool bothPrefixAndBinary(BinOp op) {
    switch(op) {
        case BinOp::Add:
        case BinOp::Sub:
        case BinOp::Mult:
            return true;
        default:
            return false;
    }
}

std::optional<BinOp> parseBinaryOperator(tok token) {
#define MATCH(tokName, opName) case tok::sym_ ## tokName: return BinOp::opName
    switch(token) {
        MATCH(add, Add);
        MATCH(subtract, Sub);
        MATCH(asterisk, Mult);
        MATCH(divide, Div);
        MATCH(modulo, Mod);
        MATCH(assignment, Assignment);
        MATCH(add_assignment, AddAssignment);
        MATCH(sub_assignment, SubAssignment);
        MATCH(mult_assignment, MultAssignment);
        MATCH(div_assignment, DivAssignment);
        MATCH(mod_assignment, ModAssignment);
        MATCH(equal, Equal);
        MATCH(not_equal, NotEqual);
        MATCH(less_than, LessThan);
        MATCH(less_than_or_equal, LessThanOrEqual);
        MATCH(greater_than, GreaterThan);
        MATCH(greater_than_or_equal, GreaterThanOrEqual);
        MATCH(b_or, Or);
        MATCH(b_and, And);
        default: return std::nullopt;
    }
#undef MATCH
}
std::optional<PreOp> parsePrefixOperator(tok token) {
#define MATCH(tokName, opName) case tok::sym_ ## tokName: return PreOp::opName
    switch(token) {
        MATCH(add, Positive);
        MATCH(subtract, Negative);
        MATCH(asterisk, Deref);
        MATCH(b_not, Not);
        default: return std::nullopt;
    }
#undef MATCH
}

#include "AST/ASTPrinter.h"
std::vector<ASTNode*> Parser::parseTopLevel() {
    std::vector<ASTNode*> nodes;
    while(true) {
        if(current().is(tok::eof)) break;
        if(current().is(tok::sym_right_curly)) {
            reportError("Extraneous closing brace '}'", current().getRange());
        }
        nodes.push_back(parseNode());
    }
    return nodes;
}

Scope* Parser::parseScope() {
    if(current().isNot(tok::sym_left_curly)) return nullptr;
    recordCurrentLoc();
    next();
    std::vector<ASTNode*> nodes;
    while(true) {
        if(current().is(tok::sym_right_curly)) { next(); break; }
        if(current().is(tok::eof)) reportError("Unexpected eof before end of scope", previous()->getRange());
        nodes.push_back(parseNode());
    }
    return new Scope(currentRange(), nodes);
}

ASTNode* Parser::parseNode() {
    if(auto stmt = TRY(parseStmt())) {
        return stmt;
    } else if(auto decl = TRY(parseDecl())) {
        return decl;
    } else if(auto decl = TRY(parseStructDecl())) {
        return decl;
    }
    recordCurrentLoc();
    auto expr = parseExpr();
    assert(expr && "Failed to parse node");
    return expr;
}

Type Parser::parseType() {
    recordCurrentLoc();
    if(current().is(tok::sym_asterisk)) {
        next();
        return Type::Pointer(parseType());
    }
    auto typeName = parseIdentifer();
    if(!typeName) reportError("Expected type name", current().getRange());

    if(*typeName == "i8") { return Type::I8(); }
    if(*typeName == "i16") { return Type::I16(); }
    if(*typeName == "i32") { return Type::I32(); }
    if(*typeName == "i64") { return Type::I64(); }

    if(*typeName == "u8") { return Type::U8(); }
    if(*typeName == "u16") { return Type::U16(); }
    if(*typeName == "u32") { return Type::U32(); }
    if(*typeName == "u64") { return Type::U64(); }

    if(*typeName == "f32") { return Type::Float(); }
    if(*typeName == "f64") { return Type::Double(); }

    if(*typeName == "bool") { return Type::Bool(); }
    if(*typeName == "void") { return Type::Void(); }

    return Type::Struct(*typeName);
}

Decl* Parser::parseDecl() {
    recordCurrentLoc();
    bool isVar;
    bool isExtern = false;
    if(current().is(tok::kw_extern)) {
        isExtern =  true;
        next();
    }
    if(current().is(tok::kw_var)) {
        isVar = true;
    } else if(current().is(tok::kw_def)) {
        isVar = false;
    } else {
        // Report an error only if we know for a fact that this was supposed to be a declaration.
        if(isExtern) {
            reportError("Expected def or var keyword to begin declaration");
        } else {
            return nullptr;
        }
    }
    EXPECT_NEXT(tok::identifier, "Expected identifier after def");
    auto name = current().getText();
    std::vector<Decl*> paramList;
    if(next().is(tok::sym_left_paren)) {
        do {
            recordCurrentLoc();
            EXPECT_NEXT(tok::identifier, "Expected parameter name");
            auto paramName = current().getText();
            EXPECT_NEXT(tok::sym_colon, "Expected colon after parameter name");
            next();
            paramList.push_back(new Decl(currentRange(), paramName, parseType()));
        } while(current().is(tok::sym_comma));
        EXPECT(tok::sym_right_paren, "Expected ')' after parameter list");
        if(paramList.empty()) reportError("Expected parameter list for parameterized declaration " + name + ", "
                                          "try removing the parentheses");
        next();
    }

    Type type = Type::Error();
    // Range of the "prototype", which includes everything from the extern keyword (if it exists) to
    // the type of the declaration (if it's specified).
    SourceRange protoRange;

    if(current().is(tok::sym_colon)) {
        next();

        next();
        protoRange = currentRange();
        previous();

        type = parseType();
    } else {
        protoRange = currentRange();
    }

    auto checkExtern = [&]() {
        if(isExtern) {
            reportError("'extern' declaration '" + name +
                        "' may not have a definition.",
                        protoRange);
        }
    };

    if(current().is(tok::sym_assignment)) {
        checkExtern();
        next();
        auto expr = parseExpr();
        if(!expr) reportError("Expected expression to assign to declaration " + name, current().getRange());
        auto range = rangeFrom(protoRange.begin, expr->range);
        return new Decl(range, name, type, isVar, isExtern, paramList, expr);
    } else if(auto scope = parseScope()) {
        checkExtern();
        auto range = rangeFrom(protoRange.begin, scope->range);
        return new Decl(range, name, type, isVar, isExtern, paramList, scope);
    } else {
        return new Decl(protoRange, name, type, isVar, isExtern, paramList);
    }
}

StructDecl* Parser::parseStructDecl() {
    recordCurrentLoc();
    if(current().isNot(tok::kw_struct)) {
        return nullptr;
    }
    next();
    auto name = parseIdentifer();
    if(!name) {
        reportError("Expected struct name");
    }
    if(current().isNot(tok::sym_left_curly)) {
        reportError("Expected opening curly brace to begin struct declaration");
    }
    next();
    std::vector<Decl*> fields;
    while(current().isNot(tok::sym_right_curly)) {
        auto field = parseDecl();
        if(!field) {
            reportError("Expected struct field");
        }
        if(field->isExtern) {
            reportError("Struct fields cannot be declared as `extern`");
        }
        if(field->isParameterized()) {
            reportError("Struct fields cannot have parameters");
        }
        if(field->hasDefinition()) {
            reportError("Struct fields cannot (yet) have definitions");
        }
        if(field->type == Type::Error()) {
            reportError("Struct fields must have explicit types");
        }
        fields.push_back(field);
    }
    next();
    return new StructDecl(currentRange(), *name, fields);
}

Stmt* Parser::parseStmt() {
    if(current().is(tok::kw_return)) {
        recordCurrentLoc();
        next();
        auto value = parseExpr();
        if(!value) return new ReturnStmt(currentRange(), nullptr);
        return new ReturnStmt(currentRange(), value);
    } else if(current().is(tok::kw_if)) {
        return parseIfStmt();
    } else {
        return parseWhileStmt();
    }
}

Stmt* Parser::parseIfStmt() {
    if(current().isNot(tok::kw_if)) return nullptr;
    recordCurrentLoc();
    next();
    auto conditionExpr = parseExpr();
    if(!conditionExpr) reportError("Expected condition expression for if statement");
    auto thenScope = parseScope();
    if(!thenScope) reportError("Expected opening curly brace for if statement");
    Scope* elseScope = nullptr;
    if(current().is(tok::kw_else)) {
        next();
        if(auto scope = parseScope()) {
            elseScope = scope;
        } else if(auto elseIf = parseIfStmt()) {
            elseScope = new Scope(
                elseIf->range,
                std::vector<ASTNode*> { elseIf }
            );
        }
    }
    return new IfStmt(currentRange(),
                      conditionExpr,
                      thenScope,
                      elseScope);
}

Stmt* Parser::parseWhileStmt() {
    if(current().isNot(tok::kw_while)) return nullptr;
    recordCurrentLoc();
    next();
    auto conditionExpr = parseExpr();
    if(!conditionExpr) reportError("Expected condition expression for while statement");
    auto thenScope = parseScope();
    if(!thenScope) reportError("Expected opening curly brace for while statement");
    return new WhileStmt(currentRange(),
                         conditionExpr,
                         thenScope);
}

Expr* Parser::parseDeclRefExpr() {
    if(current().isNot(tok::identifier)) return nullptr;

    recordCurrentLoc();
    auto name = current().getText();
    std::vector<Expr*> argList;
    if(next().is(tok::sym_left_paren)) {
        do {
            next();
            recordCurrentLoc();

            auto argument = parseExpr();
            if(!argument) reportError("Expected argument");
            argList.push_back(argument);

        } while(current().is(tok::sym_comma));
        EXPECT(tok::sym_right_paren, "Expected ')' after parameter and argument");
        next();
    }

    return new DeclRefExpr(currentRange(), name, argList);
}

Expr* Parser::parseExpr() {
    std::vector<Expr*> exprStack { parseTerm() };
    std::vector<BinOp> opStack;

    auto popStacks = [&]() {
        auto rhs = exprStack.back();
        exprStack.pop_back();
        auto lhs = exprStack.back();
        exprStack.pop_back();
        auto nextOp = opStack.back();
        opStack.pop_back();
        exprStack.push_back(new BinOpExpr(SourceRange(), lhs, rhs, nextOp));
    };

    while(true) {
        auto op = parseBinaryOperator(current().getKind());
        if(!op) { break; }
        next();

        while(!opStack.empty() && getPrecedence(opStack.back()) <= getPrecedence(*op)) {
            popStacks();
        }
        opStack.push_back(*op);
        exprStack.push_back(parseTerm());
    }
    while(!opStack.empty()) {
        popStacks();
    }
    return exprStack.front();
}

Expr* Parser::parseTerm() {
    recordCurrentLoc();
    Expr* retVal;
    if(current().is(tok::sym_left_paren)) {
        next();
        auto expr = parseExpr();
        EXPECT(tok::sym_right_paren, "Unclosed parentheses");
        next();
        retVal = expr;
    } else if(auto preOp = parsePrefixOperator(current().getKind())) {
        next();
        auto range = currentRange();
        retVal = new PreOpExpr(range, parseTerm(), *preOp);
    } else if(current().is(tok::kw_true)) {
        next();
        retVal = new BooleanLiteralExpr(currentRange(), true);
    } else if(current().is(tok::kw_false)) {
        next();
        retVal = new BooleanLiteralExpr(currentRange(), false);
    } else if(auto intVal = parseIntegerLiteral()) {
        retVal = new IntegerLiteralExpr(currentRange(), *intVal);
    } else if(auto decimalVal = parseDecimalLiteral()) {
        retVal = new DecimalLiteralExpr(currentRange(), *decimalVal);
    } else if(auto charVal = parseCharLiteral()) {
        retVal = new CharLiteralExpr(currentRange(), *charVal);
    } else if(auto stringVal = parseStringLiteral()) {
        retVal = new StringLiteralExpr(currentRange(), *stringVal);
    } else {
        retVal = TRY(parseDeclRefExpr());
    }

    if(retVal) {
        while(current().is(tok::sym_dot)) {
            next();
            auto memberName = parseIdentifer();
            if(!memberName) {
                reportError("Expected member name after '.'");
            }
            retVal = new MemberRefExpr(currentRange(), retVal, *memberName);
        }
        while(current().is(tok::kw_as)) {
            next();
            auto destType = parseType();
            retVal = new CastExpr(currentRange(), retVal, destType);
        }
    }

    return retVal;
}
