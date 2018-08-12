//  Copyright © 2018 Zach Wolfe. All rights reserved.

#include <vector>
#include <iostream>
#include <optional>
#include <utility> // for std::pair

#include "AST/Expr.h"
#include "AST/Stmt.h"
#include "AST/Decl.h"
#include "Parser.h"

#define ERR(message) Diagnostic(Diagnostic::Error, file, message)

#define EXPECT(token, message) if(cur().isNot(token)) { \
    reportDiag( \
        ERR(message).primaryRange(cur().getRange())\
    ); \
}
#define EXPECT_NEXT(token, message) {\
    next();\
    EXPECT(token, message);\
}

#define TRY(result) [&]() {\
    saveState();\
    auto val = result;\
    if(!val) recallState();\
    return val;\
}()

// FIXME: Allocating a multi-dimensional array on the heap for this is silly.
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

    { BinOp::BitwiseAnd, BinOp::BitwiseOr },

    { BinOp::And, BinOp::Or },

    {
        BinOp::Assignment,
        BinOp::AddAssignment,
        BinOp::SubAssignment,
        BinOp::MultAssignment,
        BinOp::DivAssignment,
        BinOp::ModAssignment,
        BinOp::AndAssignment,
        BinOp::OrAssignment
    }
};

int getPrecedence(BinOp op) {
    for(int level = 0; level < precedenceLevels.size(); ++level) {
        for(auto otherOp: precedenceLevels[level]) {
            if(op == otherOp) { return level; }
        }
    }
    assert(false && "Undefined precedence for binary operator");
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
        MATCH(and_assignment, AndAssignment);
        MATCH(or_assignment, OrAssignment);
        MATCH(equal, Equal);
        MATCH(not_equal, NotEqual);
        MATCH(less_than, LessThan);
        MATCH(less_than_or_equal, LessThanOrEqual);
        MATCH(greater_than, GreaterThan);
        MATCH(greater_than_or_equal, GreaterThanOrEqual);
        MATCH(b_or, Or);
        MATCH(b_and, And);
        MATCH(pipe, BitwiseOr);
        MATCH(ampersand, BitwiseAnd);
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
        MATCH(ampersand, AddrOf);
        default: return std::nullopt;
    }
    #undef MATCH
}

#include "AST/ASTPrinter.h"
std::vector<ASTNode*> Parser::parseTopLevel() {
    std::vector<ASTNode*> nodes;
    while(true) {
        if(cur().is(tok::eof)) break;
        if(cur().is(tok::sym_right_curly)) {
            reportDiag(ERR("extraneous closing brace '}'").primaryRange(cur().getRange()));
        }
        nodes.push_back(parseNode());
    }
    return nodes;
}

Scope* Parser::parseScope() {
    if(cur().isNot(tok::sym_left_curly)) return nullptr;
    Token openingCurly = cur();
    std::optional<Token> closingCurly;
    next();
    std::vector<ASTNode*> nodes;
    while(true) {
        if(cur().is(tok::sym_right_curly)) {
            closingCurly = cur();
            next();
            break;
        }
        if(cur().is(tok::eof)) {
            auto prevTok = prev();
            reportDiag(
                ERR("unexpected eof before end of scope").range(openingCurly.getRange(), "scope begins here")
            );
        }
        nodes.push_back(parseNode());
    }
    return new Scope(openingCurly.getRange() + closingCurly->getRange(), nodes);
}

ASTNode* Parser::parseNode() {
    if(auto stmt = TRY(parseStmt())) {
        return stmt;
    } else if(auto decl = TRY(parseDecl())) {
        return decl;
    } else if(auto decl = TRY(parseStructDecl())) {
        return decl;
    }
    auto expr = parseExpr();
    assert(expr && "Failed to parse node");
    return expr;
}

Type Parser::parseType() {
    if(cur().is(tok::sym_asterisk)) {
        auto asteriskRange = cur().getRange();
        next();
        auto pointedTy = parseType();
        return Type(PointerTy::get(pointedTy), asteriskRange + pointedTy.range);
    }
    auto range = cur().getRange();
    auto typeName = parseIdentifier();
    if(!typeName) reportDiag(ERR("expected type name").primaryRange(cur().getRange()));

    Type::DataType data;
    if(*typeName == "i8") { data = IntTy::I8(); }
    else if(*typeName == "i16") { data = IntTy::I16(); }
    else if(*typeName == "i32") { data = IntTy::I32(); }
    else if(*typeName == "i64") { data = IntTy::I64(); }

    else if(*typeName == "u8") { data = IntTy::U8(); }
    else if(*typeName == "u16") { data = IntTy::U16(); }
    else if(*typeName == "u32") { data = IntTy::U32(); }
    else if(*typeName == "u64") { data = IntTy::U64(); }

    else if(*typeName == "f32") { data = FloatTy(); }
    else if(*typeName == "f64") { data = DoubleTy(); }

    else if(*typeName == "bool") { data = BoolTy(); }
    else if(*typeName == "void") { data = VoidTy(); }

    else {
        data = StructTy::get(*typeName);
    }

    return Type(data, range);
}

Decl* Parser::parseDecl() {
    bool isVar;
    std::optional<SourceRange> externRange;
    if(cur().is(tok::kw_extern)) {
        externRange = cur().getRange();
        next();
    }
    SourceRange keywordRange;
    if(cur().is(tok::kw_var)) {
        isVar = true;
        keywordRange = cur().getRange();
    } else if(cur().is(tok::kw_def)) {
        isVar = false;
        keywordRange = cur().getRange();
    } else {
        // Report an error only if we know for a fact that this was supposed to be a declaration. Otherwise return.
        if(externRange) {
            reportDiag(ERR("expected def or var keyword to begin declaration").primaryRange(cur().getRange()));
        } else {
            return nullptr;
        }
    }
    next();
    auto name = parseIdentifier();
    if(!name) {
        reportDiag(
            ERR("expected declaration name").primaryRange(cur().getRange())
        );
    }
    std::vector<Decl*> paramList;
    if(cur().is(tok::sym_left_paren)) {
        auto leftParen = cur();
        bool isFirst = true;
        do {
            next();
            auto paramName = parseIdentifier();
            if(!paramName) {
                auto diag = ERR("expected parameter name");
                if(isFirst && cur().is(tok::sym_right_paren)) {
                    diag.primaryRange(leftParen.getRange() + cur().getRange(), "try removing empty parameter list");
                } else {
                    diag.primaryRange(cur().getRange());
                }
                reportDiag(diag);
            }
            if(isFirst) {
                isFirst = false;
            }
            EXPECT(tok::sym_colon, "expected colon after parameter name");
            next();
            paramList.push_back(new Decl(*paramName, parseType(), /* isVar */ false));
        } while(cur().is(tok::sym_comma));
        EXPECT(tok::sym_right_paren, "expected ')' after parameter list");
        next();
    }

    Type type = ErrorTy();
    if(cur().is(tok::sym_colon)) {
        next();
        type = parseType();
    }

    auto definitionBeginTok = cur();
    auto checkExtern = [&]() {
        if(externRange) {
            reportDiag(ERR("'extern' declaration '" + name->getText() +
                           "' may not have a definition.").primaryRange(definitionBeginTok.getRange()));
        }
    };

    if(cur().is(tok::sym_assignment)) {
        checkExtern();
        next();
        auto expr = parseExpr();
        if(!expr) {
            reportDiag(ERR("expected expression to assign to declaration `" + name->getText() + '`')
                .primaryRange(cur().getRange()));
        }
        return new Decl(externRange, keywordRange, *name, type, isVar, paramList, expr);
    } else if(auto scope = parseScope()) {
        checkExtern();
        return new Decl(externRange, keywordRange, *name, type, isVar, paramList, scope);
    } else {
        return new Decl(externRange, keywordRange, *name, type, isVar, paramList);
    }
}

StructDecl* Parser::parseStructDecl() {
    if(cur().isNot(tok::kw_struct)) {
        return nullptr;
    }
    next();
    auto name = parseIdentifier();
    if(!name) {
        reportDiag(ERR("expected struct name").primaryRange(cur().getRange()));
    }
    EXPECT(tok::sym_left_curly, "expected opening curly brace to begin struct declaration");
    next();
    std::vector<Decl*> fields;
    while(cur().isNot(tok::sym_right_curly)) {
        auto fieldName = parseIdentifier();
        if(!fieldName) {
            reportDiag(ERR("expected struct field name").primaryRange(cur().getRange()));
        }
        EXPECT(tok::sym_colon, "expected colon in struct field");
        next();
        auto fieldType = parseType();
        fields.push_back(new Decl(*fieldName, fieldType, /* isVar */ true));
    }
    next();
    return new StructDecl(currentRange(), *name, fields);
}

Stmt* Parser::parseStmt() {
    if(cur().is(tok::kw_return)) {
        auto returnTok = cur();
        next();
        auto value = parseExpr();
        if(!value) return new ReturnStmt(returnTok.getRange(), nullptr);
        return new ReturnStmt(returnTok.getRange(), value);
    } else if(cur().is(tok::kw_if)) {
        return parseIfStmt();
    } else {
        return parseWhileStmt();
    }
}

IfStmt* Parser::parseIfStmt() {
    if(cur().isNot(tok::kw_if)) return nullptr;
    auto ifTok = cur();
    next();
    auto conditionExpr = parseExpr();
    if(!conditionExpr) {
        reportDiag(
           ERR("expected condition expression for if statement")
               .primaryRange(cur().getRange())
               .range(ifTok.getRange(), "if statement begins here")
        );
    }
    auto thenScope = parseScope();
    if(!thenScope) {
        reportDiag(
            ERR("expected opening curly brace for if statement")
                .primaryRange(cur().getRange())
                .range(ifTok.getRange(), "if statement begins here")
        );
    }
    std::optional<std::variant<Scope*, IfStmt*>> elseNode = std::nullopt;
    if(cur().is(tok::kw_else)) {
        next();
        if(auto scope = parseScope()) {
            elseNode = scope;
        } else if(auto elseIf = parseIfStmt()) {
            elseNode = static_cast<IfStmt*>(elseIf);
        }
    }
    return new IfStmt(ifTok.getRange(),
                      conditionExpr,
                      thenScope,
                      elseNode);
}

Stmt* Parser::parseWhileStmt() {
    if(cur().isNot(tok::kw_while)) return nullptr;
    auto whileTok = cur();
    next();
    auto conditionExpr = parseExpr();
    if(!conditionExpr) {
        reportDiag(
            ERR("expected condition expression for while statement")
                .primaryRange(cur().getRange())
                   .range(whileTok.getRange(), "while statement begins here")
        );
    }
    auto thenScope = parseScope();
    if(!thenScope) {
        reportDiag(
           ERR("expected opening curly brace for while statement")
               .primaryRange(cur().getRange())
               .range(whileTok.getRange(), "while statement begins here")
        );
    }
    return new WhileStmt(whileTok.getRange(),
                         conditionExpr,
                         thenScope);
}

Expr* Parser::parseDeclRefExpr() {
    auto name = parseIdentifier();
    if(!name) return nullptr;

    std::vector<Expr*> argList;
    std::optional<std::pair<SourceRange, SourceRange>> _parenRanges;
    if(cur().is(tok::sym_left_paren)) {
        std::pair<SourceRange, SourceRange> parenRanges;
        parenRanges.first = cur().getRange();
        do {
            next();
            auto argument = parseExpr();
            if(!argument) reportDiag(ERR("expected argument").primaryRange(cur().getRange()));
            argList.push_back(argument);

        } while(cur().is(tok::sym_comma));
        EXPECT(tok::sym_right_paren, "expected ')' after argument");
        parenRanges.second = cur().getRange();
        next();

        _parenRanges = parenRanges;
    }

    return new DeclRefExpr(_parenRanges, *name, argList);
}

Expr* Parser::parseExpr() {
    std::vector<Expr*> exprStack { parseTerm() };
    std::vector<std::pair<BinOp, SourceRange>> opStack;

    auto popStacks = [&]() {
        auto rhs = exprStack.back();
        exprStack.pop_back();
        auto lhs = exprStack.back();
        exprStack.pop_back();
        auto nextOp = opStack.back();
        opStack.pop_back();
        exprStack.push_back(new BinOpExpr(nextOp.second, lhs, rhs, nextOp.first));
    };

    while(true) {
        auto op = parseBinaryOperator(cur().getKind());
        auto opRange = cur().getRange();
        if(!op) { break; }
        next();

        while(!opStack.empty() && getPrecedence(opStack.back().first) <= getPrecedence(*op)) {
            popStacks();
        }
        opStack.push_back({*op, opRange});
        exprStack.push_back(parseTerm());
    }
    while(!opStack.empty()) {
        popStacks();
    }
    return exprStack.front();
}

Expr* Parser::parseTerm() {
    Expr* retVal;
    if(cur().is(tok::sym_left_paren)) {
        next();
        auto expr = parseExpr();
        EXPECT(tok::sym_right_paren, "unclosed parentheses");
        next();
        retVal = expr;
    } else if(auto preOp = parsePrefixOperator(cur().getKind())) {
        auto opRange = cur().getRange();
        next();
        retVal = new PreOpExpr(opRange, parseTerm(), *preOp);
    } else if(cur().is(tok::kw_true)) {
        auto range = cur().getRange();
        next();
        retVal = new BooleanLiteralExpr(range, true);
    } else if(cur().is(tok::kw_false)) {
        auto range = cur().getRange();
        next();
        retVal = new BooleanLiteralExpr(range, false);
    } else if(auto intVal = parseIntegerLiteral()) {
        retVal = new IntegerLiteralExpr(intVal->getRange(), intVal->getText());
    } else if(auto decimalVal = parseDecimalLiteral()) {
        retVal = new DecimalLiteralExpr(decimalVal->getRange(), decimalVal->getText());
    } else if(auto charVal = parseCharLiteral()) {
        retVal = new CharLiteralExpr(charVal->getRange(), charVal->getText()[0]);
    } else if(auto stringVal = parseStringLiteral()) {
        retVal = new StringLiteralExpr(stringVal->getRange(), stringVal->getText());
    } else {
        retVal = TRY(parseDeclRefExpr());
    }

    if(retVal) {
        while(cur().is(tok::sym_dot)) {
            next();
            auto memberName = parseIdentifier();
            if(!memberName) {
                reportDiag(ERR("expected member name after '.'").primaryRange(cur().getRange()));
            }
            retVal = new MemberRefExpr(currentRange(), retVal, *memberName);
        }
        while(cur().is(tok::kw_as)) {
            next();
            auto destType = parseType();
            retVal = new CastExpr(currentRange(), retVal, destType);
        }
    }

    return retVal;
}

std::string Token::prettyPrint() {
    #define TOKEN(name) case tok::name: return #name " " + getText();
    switch(kind) {
        #include "TokenKinds.def"
        default: return "undefined token";
    }
}
