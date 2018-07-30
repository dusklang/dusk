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

std::vector<std::vector<OperatorKind>> precedenceLevels {
    { OperatorKind::asterisk, OperatorKind::divide, OperatorKind::modulo },
    { OperatorKind::add, OperatorKind::subtract },
    {
        OperatorKind::less_than,
        OperatorKind::less_than_or_equal,
        OperatorKind::greater_than,
        OperatorKind::greater_than_or_equal
    },
    { OperatorKind::equal, OperatorKind::not_equal },
    { OperatorKind::b_and, OperatorKind::b_or }
};

std::optional<int> getPrecedence(OperatorKind op) {
    for(int level = 0; level < precedenceLevels.size(); ++level) {
        for(auto otherOp: precedenceLevels[level]) {
            if(op == otherOp) { return level; }
        }
    }
    return std::nullopt;
}

std::optional<OperatorKind> parseOperator(tok token) {
    switch(token) {
        #define TOKEN_OPERATOR(name, string) case tok::op_ ## name: return OperatorKind::name;
        #include "TokenKinds.def"
        default: return std::nullopt;
    }
}
bool isBinary(OperatorKind op) {
    return op != OperatorKind::b_not;
}

#include "AST/ASTPrinter.h"
std::vector<ASTNode*> Parser::parseTopLevel() {
    std::vector<ASTNode*> nodes;
    while(true) {
        if(current().is(tok::eof)) break;
        if(current().is(tok::sep_right_curly)) reportError("Extraneous closing brace '}'", current().getRange());
        nodes.push_back(parseNode());
    }
    return nodes;
}

Scope* Parser::parseScope() {
    if(current().isNot(tok::sep_left_curly)) return nullptr;
    recordCurrentLoc();
    next();
    std::vector<ASTNode*> nodes;
    while(true) {
        if(current().is(tok::sep_right_curly)) { next(); break; }
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
    }
    recordCurrentLoc();
    auto expr = parseExpr();
    assert(expr && "Failed to parse expression");
    // Parse AssignmentStmt
    // TODO: Maybe put this into it's own method someday
    if(current().is(tok::sep_equal)) {
        next();
        auto rhsExpr = parseExpr();
        if(!rhsExpr) reportError("Expected right-hand expression in assignment statement");

        if(auto declRefLHS = dynamic_cast<DeclRefExpr*>(expr)) {
            return new AssignmentStmt(currentRange(), declRefLHS, rhsExpr);
        } else {
            reportError("Cannot assign to non-declaration reference");
        }
    } else {
        currentRange();
        return expr;
    }
    __builtin_unreachable();
}

Type Parser::parseType() {
    recordCurrentLoc();
    if(current().is(tok::op_asterisk)) {
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

    reportError("Invalid type name \"" + *typeName + '"', previous()->getRange());
    __builtin_unreachable();
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
    if(next().is(tok::sep_left_paren)) {
        do {
            recordCurrentLoc();
            EXPECT_NEXT(tok::identifier, "Expected parameter name");
            auto paramName = current().getText();
            EXPECT_NEXT(tok::sep_colon, "Expected colon after parameter name");
            next();
            paramList.push_back(new Decl(currentRange(), paramName, parseType()));
        } while(current().is(tok::sep_comma));
        EXPECT(tok::sep_right_paren, "Expected ')' after parameter list");
        if(paramList.empty()) reportError("Expected parameter list for parameterized declaration " + name + ", "
                                          "try removing the parentheses");
        next();
    }

    Type type = Type::Error();
    // Range of the "prototype", which includes everything from the extern keyword (if it exists) to
    // the type of the declaration (if it's specified).
    SourceRange protoRange;

    if(current().is(tok::sep_colon)) {
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

    if(current().is(tok::sep_equal)) {
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
    if(next().is(tok::sep_left_paren)) {
        do {
            next();
            recordCurrentLoc();

            auto argument = parseExpr();
            if(!argument) reportError("Expected argument");
            argList.push_back(argument);

        } while(current().is(tok::sep_comma));
        EXPECT(tok::sep_right_paren, "Expected ')' after parameter and argument");
        next();
    }

    return new DeclRefExpr(currentRange(), name, argList);
}

Expr* Parser::parseExpr() {
    std::vector<Expr*> exprStack { parseTerm() };
    std::vector<OperatorKind> opStack;

    auto popStacks = [&]() {
        auto rhs = exprStack.back();
        exprStack.pop_back();
        auto lhs = exprStack.back();
        exprStack.pop_back();
        auto nextOp = opStack.back();
        opStack.pop_back();
        switch(nextOp) {
            case OperatorKind::asterisk:
            case OperatorKind::divide:
            case OperatorKind::modulo:
            case OperatorKind::add:
            case OperatorKind::subtract:
            case OperatorKind::equal:
            case OperatorKind::not_equal:
            case OperatorKind::less_than:
            case OperatorKind::less_than_or_equal:
            case OperatorKind::greater_than:
            case OperatorKind::greater_than_or_equal:
            case OperatorKind::b_and:
            case OperatorKind::b_or:
                exprStack.push_back(new BinOpExpr(SourceRange(), lhs, rhs, nextOp));
            default: __builtin_unreachable();
        }
    };

    while(true) {
        std::cout << "tast\n";
        auto kind = current().getKind();
        std::cout << "test\n";
        auto op = parseOperator(kind);
        std::cout << "tast\n";
        if(!op) { break; }
        next();
        std::cout << "test\n";
        if(!isBinary(*op)) {
            reportError("Expected binary operator");
        }
        while(!opStack.empty() && getPrecedence(opStack.back()) <= getPrecedence(*op)) {
            popStacks();
        }
        opStack.push_back(*op);
        exprStack.push_back(parseTerm());

        ASTPrinter printer;
        printer.visitExpr(exprStack.back(), 0, std::cout);
        std::cout << "\n";
    }
    while(!opStack.empty()) {
        popStacks();
    }
    return exprStack.front();
}

Expr* Parser::parseTerm() {
    recordCurrentLoc();
    if(current().is(tok::sep_left_paren)) {
        next();
        auto expr = parseExpr();
        EXPECT(tok::sep_right_paren, "Unclosed parentheses");
        next();
        return expr;
    } else if(current().is(tok::op_asterisk)) {
        next();
        return new PrefixOpExpr(currentRange(), parseTerm(), OperatorKind::asterisk);
    } else if(current().is(tok::op_add)) {
        next();
        return new PrefixOpExpr(currentRange(), parseTerm(), OperatorKind::add);
    } else if(current().is(tok::op_subtract)) {
        next();
        return new PrefixOpExpr(currentRange(), parseTerm(), OperatorKind::subtract);
    } else if(current().is(tok::op_b_not)) {
        next();
        return new PrefixOpExpr(currentRange(), parseTerm(), OperatorKind::b_not);
    } else if(current().is(tok::kw_true)) {
        next();
        return new BooleanLiteralExpr(currentRange(), true);
    } else if(current().is(tok::kw_false)) {
        next();
        return new BooleanLiteralExpr(currentRange(), false);
    } else if(auto intVal = parseIntegerLiteral()) {
        return new IntegerLiteralExpr(currentRange(), *intVal);
    } else if(auto decimalVal = parseDecimalLiteral()) {
        return new DecimalLiteralExpr(currentRange(), *decimalVal);
    } else if(auto charVal = parseCharLiteral()) {
        return new CharLiteralExpr(currentRange(), *charVal);
    } else if(auto stringVal = parseStringLiteral()) {
        return new StringLiteralExpr(currentRange(), *stringVal);
    }

    // Reset the stack.
    next();
    currentRange();
    previous();
    return TRY(parseDeclRefExpr());
}
