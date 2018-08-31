//  Copyright © 2018 Zach Wolfe. All rights reserved.

#pragma once

#include <optional>
#include <stack>

#include "AST/AST.h"
#include "AST/Type.h"
#include "General/SourceInfo.h"
#include "General/Diagnostics.h"
#include "General/General.h"
#include "General/Array.h"
#include "Lexer.h"

struct Expr;
struct IfExpr;
struct WhileExpr;
struct DoExpr;
struct DeclRefExpr;
struct Decl;

class Parser final {
    SourceFile const& file;
    Array<Token> tokens;
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
        assertTrue(!savedState.empty());
        curTok = savedState.top();
        savedState.pop();
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
    std::optional<Ident> parseIdentifier() {
        auto val = cur().getIdentifier();
        auto range = cur().getRange();
        if(val) {
            next();
            return Ident(*val, range);
        } else {
            return std::nullopt;
        }
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
    IfExpr* parseIfExpr();
    WhileExpr* parseWhileExpr();
    DoExpr* parseDoExpr();
    DeclRefExpr* parseDeclRefExpr();
    Expr* parseExpr();
    Expr* parseTerm();
};
