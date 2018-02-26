//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include "Parser.h"
#include "AST/Expr.h"
#include <vector>
#include <memory>
#include <iostream>

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

std::vector<std::shared_ptr<ASTNode>> Parser::parseTopLevel() {
    std::vector<std::shared_ptr<ASTNode>> nodes;
    while(true) {
        if(current().is(tok::eof)) break;
        if(current().is(tok::sep_right_curly)) reportError("Extraneous closing brace '}'", current().getRange());
        nodes.push_back(parseNode());
    }
    return nodes;
}

llvm::Optional<std::shared_ptr<Scope>> Parser::parseScope() {
    if(current().isNot(tok::sep_left_curly)) return llvm::None;
    recordCurrentLoc();
    next();
    std::vector<std::shared_ptr<ASTNode>> nodes;
    while(true) {
        if(current().is(tok::sep_right_curly)) { next(); break; }
        if(current().is(tok::eof)) reportError("Unexpected eof before end of scope", previous()->getRange());
        nodes.push_back(parseNode());
    }
    return std::make_shared<Scope>(currentRange(), nodes);
}

std::shared_ptr<ASTNode> Parser::parseNode() {
    if(auto stmt = TRY(parseStmt())) {
        return std::dynamic_pointer_cast<ASTNode>(*stmt);
    } else if(auto decl = TRY(parseDecl())) {
        return std::dynamic_pointer_cast<ASTNode>(std::make_shared<Decl>(*decl));
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

        if(auto declRefLHS = std::dynamic_pointer_cast<DeclRefExpr>(*expr)) {
            return std::make_shared<AssignmentStmt>(currentRange(), declRefLHS, *rhsExpr);
        } else {
            reportError("Cannot assign to non-declaration reference");
        }
    } else {
        currentRange();
        return std::dynamic_pointer_cast<ASTNode>(*expr);
    }
    LLVM_BUILTIN_UNREACHABLE;
}

PhysicalTypeRef Parser::parseTypeRef() {
    recordCurrentLoc();
    auto typeName = parseIdentifer();
    if(!typeName) reportError("Expected type name", current().getRange());
    #define BUILTIN_TYPE(name) if(*typeName == #name) { return PhysicalTypeRef(currentRange(), BuiltinType::name); }
    #include "AST/BuiltinTypes.def"
    reportError("Invalid type name \"" + *typeName + '"', previous()->getRange());
    LLVM_BUILTIN_UNREACHABLE;
}

llvm::Optional<Decl> Parser::parseDecl() {
    recordCurrentLoc();
    bool isMut;
    bool isExtern = false;
    if(current().is(tok::kw_extern)) {
        isExtern =  true;
        next();
    }
    if(current().is(tok::kw_var)) {
        isMut = true;
    } else if(current().is(tok::kw_def)) {
        isMut = false;
    } else {
        // Report an error only if we know for a fact that this was supposed to be a declaration.
        if(isExtern) {
            reportError("Expected def or var keyword to begin declaration");
        } else {
            return llvm::None;
        }
    }
    EXPECT_NEXT(tok::identifier, "Expected identifier after def");
    auto name = current().getText();
    std::vector<std::shared_ptr<Decl>> paramList;
    if(next().is(tok::sep_left_paren)) {
        do {
            recordCurrentLoc();
            EXPECT_NEXT(tok::identifier, "Expected parameter name");
            auto param = current().getText();
            EXPECT_NEXT(tok::sep_colon, "Expected colon after parameter name");
            next();
            paramList.push_back(std::make_shared<Decl>(currentRange(), param, parseTypeRef()));
        } while(current().is(tok::sep_comma));
        EXPECT(tok::sep_right_paren, "Expected ')' after parameter list");
        if(paramList.empty()) reportError("Expected parameter list for parameterized declaration " + name + ", "
                                          "try removing the parentheses");
        next();
    }

    TypeRef type;
    // Range of the "prototype", which includes everything from the extern keyword (if it exists) to
    // the type of the declaration (if it's specified).
    SourceRange protoRange;

    if(current().is(tok::sep_colon)) {
        next();

        next();
        protoRange = currentRange();
        previous();

        type = parseTypeRef();
    } else {
        protoRange = currentRange();
        type = TypeRef();
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
        auto range = rangeFrom(protoRange.begin, (*expr)->range);
        return Decl(range, name, type, isMut, isExtern, paramList, *expr);
    } else if(auto scope = parseScope()) {
        checkExtern();
        auto range = rangeFrom(protoRange.begin, (*scope)->range);
        return Decl(range, name, type, isMut, isExtern, paramList, *scope);
    } else {
        return Decl(protoRange, name, type, isMut, isExtern, paramList);
    }
}

llvm::Optional<std::shared_ptr<Stmt>> Parser::parseStmt() {
    if(current().is(tok::kw_return)) {
        recordCurrentLoc();
        next();
        auto value = parseExpr();
        if(!value) return std::dynamic_pointer_cast<Stmt>(std::make_shared<ReturnStmt>(currentRange(), nullptr));
        return std::dynamic_pointer_cast<Stmt>(std::make_shared<ReturnStmt>(currentRange(), *value));
    }

    return parseIfStmt();
}

llvm::Optional<std::shared_ptr<Stmt>> Parser::parseIfStmt() {
    if(current().isNot(tok::kw_if)) return llvm::None;
    recordCurrentLoc();
    next();
    auto conditionExpr = parseExpr();
    if(!conditionExpr) reportError("Expected condition expression for if statement");
    auto thenScope = parseScope();
    if(!thenScope) reportError("Expected opening curly brace for if statement");
    llvm::Optional<std::shared_ptr<Scope>> elseScope;
    if(current().is(tok::kw_else)) {
        next();
        if(auto scope = parseScope()) {
            elseScope = *scope;
        } else if(auto elseIf = parseIfStmt()) {
            elseScope = std::make_shared<Scope>(
                (*elseIf)->range,
                std::vector<std::shared_ptr<ASTNode>> { std::dynamic_pointer_cast<ASTNode>(*elseIf) }
            );
        }
    }
    return std::dynamic_pointer_cast<Stmt>(std::make_shared<IfStmt>(currentRange(),
                                                                    *conditionExpr,
                                                                    *thenScope,
                                                                    elseScope));
}

llvm::Optional<std::shared_ptr<Expr>> Parser::parseDeclRefExpr() {
    if(current().isNot(tok::identifier)) return llvm::None;

    recordCurrentLoc();
    auto name = current().getText();
    std::vector<Argument> argList;
    if(next().is(tok::sep_left_paren)) {
        do {
            recordCurrentLoc();
            EXPECT_NEXT(tok::identifier, "Expected parameter name");
            auto param = current().getText();
            EXPECT_NEXT(tok::sep_colon, "Expected colon after parameter name");
            next();

            auto argument = parseExpr();
            if(!argument) reportError("Expected argument after parameter name");
            argList.push_back(Argument(currentRange(), param, *argument));

        } while(current().is(tok::sep_comma));
        EXPECT(tok::sep_right_paren, "Expected ')' after parameter and argument");
        next();
    }

    return std::dynamic_pointer_cast<Expr>(std::make_shared<DeclRefExpr>(currentRange(), name, argList));
}

llvm::Optional<std::shared_ptr<Expr>> Parser::parseExpr() {
    recordCurrentLoc();
    if(current().is(tok::kw_true)) {
        next();
        return std::dynamic_pointer_cast<Expr>(std::make_shared<BooleanLiteralExpr>(currentRange(),
                                                                                    true));
    } else if(current().is(tok::kw_false)) {
        next();
        return std::dynamic_pointer_cast<Expr>(std::make_shared<BooleanLiteralExpr>(currentRange(),
                                                                                    false));
    } else if(auto intVal = TRY(parseIntegerLiteral())) {
        return std::dynamic_pointer_cast<Expr>(std::make_shared<IntegerLiteralExpr>(currentRange(), *intVal));
    } else if(auto decimalVal = TRY(parseDecimalLiteral())) {
        return std::dynamic_pointer_cast<Expr>(std::make_shared<DecimalLiteralExpr>(currentRange(), *decimalVal));
    }

    // Reset the stack.
    next();
    currentRange();
    previous();
    return TRY(parseDeclRefExpr());
}
