//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <optional>
#include <stack>

#include "AST/AST.h"
#include "AST/Type.h"
#include "General/SourceInfo.h"
#include "General/Diagnostics.h"
#include "Lexer.h"

struct Expr;
struct Stmt;
struct IfStmt;
struct Decl;

class Parser final {
    SourceFile const& file;
    std::vector<Token> tokens;
    uint32_t curTok = -1;
    std::stack<uint32_t> savedState;

    Token cur() {
        return tokens[curTok];
    }
    Token nextIncludingInsignificant() {
        return tokens[++curTok];
    }
    Token next() {
        while(true) {
            auto next = nextIncludingInsignificant();
            if(next.isSignificant())
                return next;
        }
    }
    std::optional<Token> prevIncludingInsignificant() {
        if(curTok == 1) return std::nullopt;
        return tokens[--curTok];
    }
    std::optional<Token> prev() {
        while(auto prev = prevIncludingInsignificant()) {
            if(prev->isSignificant()) return prev;
        }
        return std::nullopt;
    }
    void saveState() {
        savedState.push(curTok);
    }
    void recallState() {
        assert(!savedState.empty());
        curTok = savedState.top();
        savedState.pop();
    }

    SourceRange currentRange() const {
        return SourceRange(0, 0);
    }

    #define PARSE_METHOD(name) \
    std::optional<Token> parse ## name() { \
        if(cur().is ## name()) { \
            auto curTok = cur(); \
            next(); \
            return curTok; \
        } else { \
            return std::nullopt; \
        } \
    }
    PARSE_METHOD(IntegerLiteral)
    PARSE_METHOD(DecimalLiteral)
    PARSE_METHOD(CharLiteral)
    PARSE_METHOD(StringLiteral)
    std::optional<std::string> parseIdentifier() {
        auto val = cur().getIdentifier();
        if(val) next();
        return val;
    }

    void reportDiag(Diagnostic diag) {
        diag.print(std::cout);
        switch(diag.kind) {
            case Diagnostic::Error:
                exit(1);
            case Diagnostic::Warning:
                break;
        }
    }
public:
    Parser(SourceFile* file) : file(*file) {
        tokens = lex(file);

        next();
    }

    std::vector<ASTNode*> parseTopLevel();
    Scope* parseScope();
    ASTNode* parseNode();
    Type parseType();
    Decl* parseDecl();
    StructDecl* parseStructDecl();
    Stmt* parseStmt();
    IfStmt* parseIfStmt();
    Stmt* parseWhileStmt();
    Expr* parseDeclRefExpr();
    Expr* parseExpr();
    Expr* parseTerm();
};
