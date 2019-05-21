use crate::token::{TokenVec, TokenKind, Token};
use crate::hir::{Program, Builder, ItemId, BinOp};
use crate::ty::Type;
use crate::error::Error;
use crate::source_info;

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
        p.builder.begin_computed_decl(
            "do_something_really_cool".to_string(),
            vec!["something".to_string()],
            vec![Type::i32()],
            Type::i32(),
            0..0
        );

        p.skip_insignificant();
        loop {
            match p.cur().kind {
                TokenKind::Eof => break,
                TokenKind::CloseCurly => {
                    p.errs.push(
                        Error::new("Extraneous closing brace '}'")
                            .adding_primary_range(p.cur().range.clone(), "brace here")
                    );
                },
                _ => p.parse_node(),
            }
        }
        p.builder.end_computed_decl();

        (p.builder.program(), p.errs)
    }

    fn parse_binary_operator(&self) -> Option<BinOp> {
        Some(
            match self.cur().kind {
                TokenKind::Add => BinOp::Add,
                TokenKind::Sub => BinOp::Sub,
                TokenKind::Asterisk => BinOp::Mult,
                TokenKind::Div => BinOp::Div,
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

    fn parse_term(&mut self) -> ItemId {
        use TokenKind::*;
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
            &IntLit(val) => {
                let lit = self.builder.int_lit(val, self.cur().range.clone());
                self.next();
                lit
            },
            &DecLit(val) => {
                let lit = self.builder.dec_lit(val, self.cur().range.clone());
                self.next();
                lit
            },
            Ident(name) => {
                let name = name.clone();
                let name_range = self.cur().range.clone();
                let mut args = Vec::new();
                let mut end_range = name_range.clone();
                if let TokenKind::LeftParen = self.next().kind {
                    self.next();
                    loop {
                        // TODO: actually implement proper comma and newline handling like I've thought about
                        match self.cur().kind {
                            TokenKind::RightParen => {
                                end_range = self.next().range.clone();
                                break;
                            }
                            TokenKind::Comma => { self.next(); }
                            TokenKind::Eof => {
                                panic!("Reached eof in middle of decl ref");
                            }
                            _ => { args.push(self.parse_expr()); }
                        }
                    }
                }
                let decl_ref = self.builder.decl_ref(
                    name,
                    args,
                    source_info::concat(
                        name_range,
                        end_range,
                    )
                );
                decl_ref
            },
            x => panic!("UNHANDLED TERM {:#?}", &x)
        }
    }

    fn parse_expr(&mut self) -> ItemId {
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
                let range = source_info::concat(
                    self.builder.get_range(lhs),
                    self.builder.get_range(rhs),
                );
                expr_stack.push(self.builder.bin_op(next_op, lhs, rhs, range));
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
        match self.cur().kind {
            TokenKind::Ident(name) => {
                if let TokenKind::Colon = self.peek_next().kind {
                    // TODO: Intern strings so we don't have to copy here
                    let name = name.clone();
                    self.parse_decl(name);
                } else {
                    self.parse_expr();
                }
            },
            _ => { self.parse_expr(); }
        }
    }

    fn parse_decl(&mut self, name: String) {
        let name_range = self.cur().range.clone();
        // Skip to colon, get range.
        let colon_range = self.next().range.clone();
        let mut found_separator = true;
        let mutable = match self.next().kind {
            TokenKind::Assign => true,
            TokenKind::Colon => false,
            _ => {
                self.errs.push(
                    Error::new("expected '=' or ':' after ':' when parsing declaration")
                        .adding_primary_range(colon_range, "':' here")
                );
                found_separator = false;

                true
            }
        };

        if found_separator {
            self.next();
        }

        let root = self.parse_expr();
        let root_range = self.builder.get_range(root);
        self.builder.stored_decl(name, root, source_info::concat(name_range, root_range));
    }

    fn cur(&self) -> Token {
        self.toks.at(self.cur)
    }

    fn skip_insignificant(&mut self) {
        while self.cur().kind.is_insignificant() {
            self.next_including_insignificant();
        }
    }

    fn next_including_insignificant(&mut self) -> Token {
        self.cur += 1;
        self.cur()
    }

    fn next(&mut self) -> Token {
        self.next_including_insignificant();
        self.skip_insignificant();
        self.cur()
    }

    fn peek_next(&self) -> Token {
        for i in (self.cur+1)..self.toks.len() {
            let cur = self.toks.at(i);
            if cur.kind.is_significant() {
                return cur
            }
        }
        panic!("No significant token found");
    }
}
