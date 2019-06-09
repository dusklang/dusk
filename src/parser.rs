use string_interner::{DefaultStringInterner, Sym};

use crate::token::{TokenVec, TokenKind, Token};
use crate::builder::{ExprId, BinOp, Builder};
use crate::ty::Type;
use crate::error::Error;
use crate::source_info::{self, SourceRange};

#[inline]
pub fn parse<B: Builder>(toks: TokenVec, interner: DefaultStringInterner) -> (B::Output, Vec<Error>) {
    Parser::<B>::parse(toks, interner)
}

struct Parser<B: Builder> {
    toks: TokenVec,
    builder: B,
    cur: usize,
    errs: Vec<Error>,
}

impl<B: Builder> Parser<B> {
    fn parse(toks: TokenVec, interner: DefaultStringInterner) -> (B::Output, Vec<Error>) {
        let mut p = Parser {
            toks,
            builder: B::new(interner),
            cur: 0,
            errs: Vec::new(),
        };
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
                _ => { p.parse_node(); }
            }
        }

        (p.builder.output(), p.errs)
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

    fn try_parse_term(&mut self) -> Result<ExprId, TokenKind> {
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
                Ok(expr)
            },
            &IntLit(val) => {
                let lit = self.builder.int_lit(val, self.cur().range.clone());
                self.next();
                Ok(lit)
            },
            &DecLit(val) => {
                let lit = self.builder.dec_lit(val, self.cur().range.clone());
                self.next();
                Ok(lit)
            },
            &Ident(name) => {
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
                Ok(decl_ref)
            },
            TokenKind::Do => {
                self.next();
                let terminal_expr = self.parse_scope().unwrap_or(self.builder.void_expr());

                Ok(terminal_expr)
            },
            TokenKind::If => Ok(self.parse_if()),
            TokenKind::Return => {
                self.next();
                let ret_expr = self.try_parse_expr().unwrap_or_else(|_| self.builder.void_expr());
                Ok(self.builder.ret(ret_expr, 0..0))
            },
            x => Err(x.clone())
        }
    }

    fn try_parse_expr(&mut self) -> Result<ExprId, TokenKind> {
        let mut expr_stack = Vec::new();
        let mut op_stack: Vec<BinOp> = Vec::new();
        expr_stack.push(self.try_parse_term()?);

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
            expr_stack.push(self.try_parse_term()?);
        }
        while !op_stack.is_empty() { pop_stacks!(); }

        Ok(expr_stack.pop().unwrap())
    }

    fn parse_expr(&mut self) -> ExprId {
        self.try_parse_expr().unwrap_or_else(|tok| panic!("UNHANDLED TERM: {:#?}", tok))
    }

    fn parse_if(&mut self) -> ExprId {
        assert_eq!(self.cur().kind, &TokenKind::If);
        self.next();
        let condition = self.parse_expr();
        let then_expr = self.parse_scope().unwrap_or(self.builder.void_expr());
        let else_expr = if let TokenKind::Else = self.cur().kind {
            match self.next().kind {
                TokenKind::If => self.parse_if(),
                TokenKind::OpenCurly => self.parse_scope().unwrap_or(self.builder.void_expr()),
                _ => panic!("Expected '{' or 'if' after 'else'"),
            }
        } else {
            self.builder.void_expr()
        };
        self.builder.if_expr(condition, then_expr, else_expr, 0..0)
    }

    /// Parses any node. Iff the node is an expression, returns its ExprId.
    fn parse_node(&mut self) -> Option<ExprId> {
        match self.cur().kind {
            &TokenKind::Ident(name) => {
                if let TokenKind::Colon = self.peek_next().kind {
                    self.parse_decl(name);
                    None
                } else {
                    Some(self.parse_expr())
                }
            },
            TokenKind::Fn => {
                self.parse_comp_decl();
                None
            }
            _ => Some(self.parse_expr())
        }
    }

    fn parse_decl(&mut self, name: Sym) {
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

    // Parses an open curly brace, then a list of nodes, then a closing curly brace
    // Returns the id of the final node if it exists and is an expression. Otherwise returns `None`.
    fn parse_scope(&mut self) -> Option<ExprId> {
        self.builder.begin_scope();
        let mut stmts = Vec::new();
        let mut last_was_expr = false;
        assert_eq!(self.cur().kind, &TokenKind::OpenCurly);
        self.next();
        loop {
            match self.cur().kind {
                TokenKind::Eof => panic!("Unexpected eof while parsing scope"),
                TokenKind::CloseCurly => {
                    self.next();
                    break;
                },
                _ => {
                    let node = self.parse_node();

                    // If the node was a standalone expression, make it a statement
                    if let Some(expr) = node {
                        last_was_expr = true;
                        stmts.push(expr);
                    } else {
                        last_was_expr = false;
                    }
                }
            }
        }
        let terminal_expr = if last_was_expr {
            stmts.pop()
        } else {
            None
        };
        self.builder.stmts(&stmts);
        self.builder.end_scope();
        terminal_expr
    }

    fn parse_comp_decl(&mut self) {
        assert_eq!(self.cur().kind, &TokenKind::Fn);
        let mut proto_range = self.cur().range.clone();
        let name = if let &TokenKind::Ident(name) = self.next().kind {
            name
        } else {
            panic!("expected identifier after 'fn'")
        };
        proto_range = source_info::concat(proto_range, self.cur().range.clone());
        let mut param_names = Vec::new();
        let mut param_tys = Vec::new();
        if let TokenKind::LeftParen = self.next().kind {
            self.next();
            while let &TokenKind::Ident(name) = self.cur().kind {
                param_names.push(name);
                assert_eq!(self.next().kind, &TokenKind::Colon);
                self.next();
                let (ty, _range) = self.parse_type();
                param_tys.push(ty);
                while let TokenKind::Comma = self.cur().kind {
                    self.next();
                }
            }
            assert_eq!(self.cur().kind, &TokenKind::RightParen);
            proto_range = source_info::concat(proto_range, self.cur().range.clone());
            self.next();
        }
        let ty = if let TokenKind::Colon = self.cur().kind {
            self.next();
            let (ty, range) = self.parse_type();
            proto_range = source_info::concat(proto_range, range);
            ty
        } else {
            Type::Void
        };
        assert_eq!(self.cur().kind, &TokenKind::OpenCurly);
        self.builder.begin_computed_decl(name, param_names, param_tys, ty.clone(), proto_range);

        let terminal_expr = self.parse_scope().unwrap_or_else(|| {
            assert_eq!(ty, Type::Void, "no expression to return in non-void computed decl");
            self.builder.void_expr()
        });
        self.builder.ret(terminal_expr, 0..0);
        self.builder.end_computed_decl();
    }

    fn parse_type(&mut self) -> (Type, SourceRange) {
        let i = self.builder.interner();
        let val = (
            match self.cur().kind {
                &TokenKind::Ident(ident) => match i.resolve(ident).unwrap() {
                    "i8" => Type::i8(),
                    "i16" => Type::i16(),
                    "i32" => Type::i32(),
                    "i64" => Type::i64(),
                    "u8" => Type::u8(),
                    "u16" => Type::u16(),
                    "u32" => Type::u32(),
                    "u64" => Type::u64(),
                    "f32" => Type::f32(),
                    "f64" => Type::f64(),
                    "never" => Type::Never,
                    "bool" => Type::Bool,
                    "void" => Type::Void,
                    _ => {
                        self.errs.push(
                            Error::new("Unrecognized type")
                                .adding_primary_range(self.cur().range.clone(), "")
                        );
                        Type::Error
                    }
                },
                _ => {
                    self.errs.push(
                        Error::new("Unrecognized type")
                            .adding_primary_range(self.cur().range.clone(), "")
                    );
                    Type::Error
                }
            }, 
            self.cur().range.clone()
        );
        self.next();
        val
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
