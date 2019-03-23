use crate::token::{TokenVec, TokenKind, Token};
use crate::hir::{Program, Builder, ExprID, BinOp};
use crate::error::Error;
use crate::source_info::SourceRange;

#[inline]
pub fn parse(toks: TokenVec) -> (Program, Vec<Error>) {
    Parser::parse(toks)
}

struct Parser {
    toks: TokenVec,
    builder: Builder,
    cur: usize,
    errs: Vec<Error>,
}

impl Parser {
    fn parse(toks: TokenVec) -> (Program, Vec<Error>) {
        let mut p = Parser {
            toks,
            builder: Builder::new(),
            cur: 0,
            errs: Vec::new(),
        };

        loop {
            match p.cur().kind {
                TokenKind::Eof => break,
                TokenKind::CloseCurly => {
                    p.errs.push(
                        Error::new("Extraneous closing brace '}'")
                            .adding_primary_range(p.cur().range.clone(), "brace here")
                    );
                },
                _ => p.parse_node()
            }
        }

        (p.builder.program(), p.errs)
    }

    fn parse_binary_operator(&self) -> Option<BinOp> {
        Some(
            match self.cur().kind {
                TokenKind::Add => BinOp::Add,
                TokenKind::Sub => BinOp::Sub,
                TokenKind::Asterisk => BinOp::Mult,
                TokenKind::Div => BinOp::Mod,
                TokenKind::Assign => BinOp::Assign,
                TokenKind::AddAssign => BinOp::AddAssign,
                TokenKind::SubAssign => BinOp::SubAssign,
                TokenKind::MultAssign => BinOp::MultAssign,
                TokenKind::DivAssign => BinOp::DivAssign,
                TokenKind::ModAssign => BinOp::ModAssign,
                TokenKind::BitwiseAndAssign => BinOp::BitwiseAndAssign,
                TokenKind::BitwiseOrAssign => BinOp::BitwiseOrAssign,
                TokenKind::Equal => BinOp::Eq,
                TokenKind::NotEqual => BinOp::NotEq,
                TokenKind::LT => BinOp::Less,
                TokenKind::LTE => BinOp::LessOrEq,
                TokenKind::GT => BinOp::Greater,
                TokenKind::GTE => BinOp::GreaterOrEq,
                TokenKind::LogicalOr => BinOp::LogicalOr,
                TokenKind::LogicalAnd => BinOp::LogicalAnd,
                TokenKind::Pipe => BinOp::BitwiseOr,
                TokenKind::Ampersand => BinOp::BitwiseAnd,
                _ => return None,
            }
        )
    }

    fn parse_term(&mut self) -> ExprID {
        use TokenKind::*;
        println!("tok: {:#?}", self.cur().kind);
        match self.cur().kind {
            LeftParen => {
                self.next();
                let expr = self.parse_expr();
                if let RightParen = self.cur().kind {}
                else {
                    self.errs.push(
                        Error::new("unclosed parentheses")
                            .adding_primary_range(self.cur().range.clone(), "paren here")
                    );
                }
                self.next();
                expr
            },
            IntLit(val) => {
                let lit = self.builder.int_lit(*val);
                self.next();
                lit
            },
            DecLit(val) => {
                let lit = self.builder.dec_lit(*val);
                self.next();
                lit
            },
            x => panic!("UNHANDLED TERM {:#?}", &x)
        }
    }

    fn parse_expr(&mut self) -> ExprID {
        let mut expr_stack = Vec::new();
        let mut op_stack: Vec<BinOp> = Vec::new();
        expr_stack.push(self.parse_term());

        // It's kind of silly that this is a macro, but I'm not aware of any other
        // way to do it without upsetting the borrow checker?
        macro_rules! pop_stacks {
            () => {
                let rhs = expr_stack.pop().unwrap();
                let lhs = expr_stack.pop().unwrap();
                let next_op = op_stack.pop().unwrap();
                expr_stack.push(self.builder.bin_op(next_op, lhs, rhs));
            }
        }

        while let Some(op) = self.parse_binary_operator() {
            self.next();
            while let Some(other_op) = op_stack.last() {
                if other_op.precedence() > op.precedence() {
                    break;
                }

                pop_stacks!();
            }
            op_stack.push(op);
            expr_stack.push(self.parse_term());
        }
        while !op_stack.is_empty() { pop_stacks!(); }

        expr_stack.pop().unwrap()
    }

    fn parse_node(&mut self) {
        self.parse_expr();
    }

    fn cur(&self) -> Token {
        self.toks.at(self.cur)
    }

    fn next_including_insignificant(&mut self) -> Token {
        self.cur += 1;
        self.cur()
    }

    fn next(&mut self) -> Token {
        loop {
            self.next_including_insignificant();
            if self.cur().kind.is_significant() {
                return self.cur();
            }
        }
    }
}
