#include <optional>

#include "Parser.h"
#include "Diagnostics.h"

namespace {
    struct Parser {
        SourceFile const& file;
        Slice<Token> tokens;
        hir::Builder builder;

        uint32_t curTok = -1;
    };
}

#define EXPECT(token, message) if(cur(parser).isNot(token)) { \
    ERR(message, parser->file).primaryRange(cur(parser).getRange()) \
        .report(); \
}


static Token const& cur(Parser const* parser) {
    return parser->tokens[parser->curTok];
}

static Token const& nextIncludingInsignificant(Parser* parser) {
    return parser->tokens[++parser->curTok];
}

static Token const& next(Parser* parser) {
    while(true) {
        auto const& next = nextIncludingInsignificant(parser);
        if(next.isSignificant()) return next;
    }
}

namespace {
    using namespace hir;
    Array<Array<BinOp>> precedenceLevels {
        { BinOp::Mult, BinOp::Div, BinOp::Mod },

        { BinOp::Add, BinOp::Sub },

        {
            BinOp::Less,
            BinOp::LessOrEq,
            BinOp::Greater,
            BinOp::GreaterOrEq
        },

        { BinOp::Eq, BinOp::NotEq },

        { BinOp::BitwiseAnd, BinOp::BitwiseOr },

        { BinOp::LogicalAnd, BinOp::LogicalOr },

        {
            BinOp::Assignment,
            BinOp::AddAssignment,
            BinOp::SubAssignment,
            BinOp::MultAssignment,
            BinOp::DivAssignment,
            BinOp::ModAssignment,
            BinOp::BitwiseAndAssignment,
            BinOp::BitwiseOrAssignment
        }
    };
};

static int getPrecedence(hir::BinOp op) {
    for(size_t level: precedenceLevels.indices()) {
        for(auto otherOp: precedenceLevels[level]) {
            if(op == otherOp) { return level; }
        }
    }
    panic("Undefined precedence for binary operator");
}


static std::optional<hir::BinOp> parseBinaryOperator(tok token) {
#define MATCH(tokName, opName) case tok::sym_ ## tokName: return hir::BinOp::opName
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
            MATCH(and_assignment, BitwiseAndAssignment);
            MATCH(or_assignment, BitwiseOrAssignment);
            MATCH(equal, Eq);
            MATCH(not_equal, NotEq);
            MATCH(less_than, Less);
            MATCH(less_than_or_equal, LessOrEq);
            MATCH(greater_than, Greater);
            MATCH(greater_than_or_equal, GreaterOrEq);
            MATCH(b_or, LogicalOr);
            MATCH(b_and, LogicalAnd);
            MATCH(pipe, BitwiseOr);
            MATCH(ampersand, BitwiseAnd);
        default: return std::nullopt;
    }
    #undef MATCH
}

static hir::ExprID parseExpr(Parser* parser);
static hir::ExprID parseTerm(Parser* parser) {
    if(cur(parser).is(tok::sym_left_paren)) {
        next(parser);
        auto expr = parseExpr(parser);
        EXPECT(tok::sym_right_paren, "unclosed parentheses");
        next(parser);
        return expr;
    } else if(cur(parser).is(tok::integer_literal)) {
        // TODO: actually, y'know, parse the number.
        auto lit = parser->builder.intLit(2400);
        next(parser);
        return lit;
    } else if(cur(parser).is(tok::decimal_literal)) {
        // TODO: actually, y'know, parse the number.
        auto lit = parser->builder.decLit(1.3);
        next(parser);
        return lit;
    } else {
        panic("failed to parse term");
    }
}

static hir::ExprID parseExpr(Parser* parser) {
    // TODO: is 1000 too many/not enough?
    Array<hir::ExprID> exprStack(1000);
    Array<hir::BinOp> opStack(1000);
    exprStack.append(parseTerm(parser));

    auto popStacks = [&]() {
        auto rhs = exprStack.removeLast();
        auto lhs = exprStack.removeLast();
        auto nextOp = opStack.removeLast();
        exprStack.append(parser->builder.binOp(nextOp, lhs, rhs));
    };

    while(true) {
        auto op = parseBinaryOperator(cur(parser).getKind());
        if(!op) { break; }
        next(parser);
        while(!opStack.isEmpty() && getPrecedence(*opStack.last()) <= getPrecedence(*op)) {
            popStacks();
        }
        opStack.append(*op);
        exprStack.append(parseTerm(parser));
    }
    while(!opStack.isEmpty()) {
        popStacks();
    }
    hir::ExprID id = *exprStack.first();
    exprStack.destroy();
    opStack.destroy();
    return id;

}

static void parseNode(Parser* parser) {
    parseExpr(parser);
}

static void parseTopLevel(Parser* parser) {
    next(parser);
    while(true) {
        if(cur(parser).is(tok::eof)) break;
        if(cur(parser).is(tok::sym_right_curly)) {
            ERR("extraneous closing brace '}'", parser->file)
                .primaryRange(cur(parser).getRange())
                .report();
        }
        parseNode(parser);
    }
}

hir::Program parse(SourceFile const& file, Slice<Token> tokens) {
    Parser parser { file, tokens };
    parseTopLevel(&parser);
    return { parser.builder.expressions, file, parser.builder.levels.count() };
}
