use smallvec::{SmallVec, smallvec};

use string_interner::Sym;

use crate::driver::Driver;
use crate::token::{TokenKind, Token};
use crate::builder::{ExprId, ScopeId, BinOp, UnOp, OpPlacement, Intrinsic};
use crate::ty::Type;
use crate::error::Error;
use crate::source_info::{self, SourceRange};

struct Parser { cur: usize }

impl Driver {
    pub fn parse(&mut self) {
        let mut p = Parser { cur: 0 };

        // Add intrinsics
        {
            // Integers, floats and bool
            let values = &[
                Type::u8(), Type::u16(), Type::u32(), Type::u64(), Type::usize(),
                Type::i8(), Type::i16(), Type::i32(), Type::i64(), Type::isize(),
                Type::f32(), Type::f64(), Type::Bool
            ];
            let numerics = &values[0..12];
            let signed_numerics = &numerics[5..];
            let integers = &numerics[0..10];

            use Intrinsic::*;
            for &intr in &[Mult, Div, Mod, Add, Sub] {
                for ty in numerics {
                    self.add_intrinsic(intr, smallvec![ty.clone(), ty.clone()], ty.clone());
                }
            }
            for &intr in &[Less, LessOrEq, Greater, GreaterOrEq] {
                for ty in numerics {
                    self.add_intrinsic(intr, smallvec![ty.clone(), ty.clone()], Type::Bool);
                }
            }
            for &intr in &[Eq, NotEq] {
                for ty in values {
                    self.add_intrinsic(intr, smallvec![ty.clone(), ty.clone()], Type::Bool);
                }
            }
            for &intr in &[BitwiseAnd, BitwiseOr] {
                for ty in integers {
                    self.add_intrinsic(intr, smallvec![ty.clone(), ty.clone()], ty.clone());
                }
            }
            for &intr in &[LogicalAnd, LogicalOr] {
                self.add_intrinsic(intr, smallvec![Type::Bool, Type::Bool], Type::Bool);
            }
            for ty in signed_numerics {
                self.add_intrinsic(Neg, smallvec![ty.clone()], ty.clone());
            }
            for ty in numerics {
                self.add_intrinsic(Pos, smallvec![ty.clone()], ty.clone());
            }
            self.add_intrinsic(LogicalNot, smallvec![Type::Bool], Type::Bool);

            self.add_intrinsic(Panic, SmallVec::new(), Type::Never);
            self.add_intrinsic(Panic, smallvec![Type::u8().ptr()], Type::Never);

            self.add_intrinsic(Malloc, smallvec![Type::usize()], Type::Void.mut_ptr());
            self.add_intrinsic(Free, smallvec![Type::Void.mut_ptr()], Type::Void);

            self.add_intrinsic(Print, smallvec![Type::u8().ptr()], Type::Void);
            self.add_intrinsic(Print, smallvec![Type::u8()], Type::Void);
            self.add_intrinsic(PrintType, smallvec![Type::Ty], Type::Void);

            macro_rules! types {
                ($($ty:ident),+) => {
                    $(self.add_intrinsic($ty, SmallVec::new(), Type::Ty);)+
                };
            }
            types!(
                I8, I16, I32, I64, Isize,
                U8, U16, U32, U64, Usize,
                F32, F64,
                Never, Bool, Void, Ty
            );
        }

        self.skip_insignificant(&mut p);
        loop {
            match self.cur(&p).kind {
                TokenKind::Eof => break,
                TokenKind::CloseCurly => {
                    self.errors.push(
                        Error::new("Extraneous closing brace '}'")
                            .adding_primary_range(self.cur(&p).range.clone(), "brace here")
                    );
                },
                _ => { self.parse_node(&mut p); }
            }
        }
    }

    fn parse_binary_operator(&mut self, p: &mut Parser) -> Option<BinOp> {
        let op = match self.cur(p).kind {
            TokenKind::Add => BinOp::Add,
            TokenKind::Sub => BinOp::Sub,
            TokenKind::Asterisk => BinOp::Mult,
            TokenKind::Div => BinOp::Div,
            TokenKind::Mod => BinOp::Mod,
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
            TokenKind::Ampersand if self.peek_next(p).kind != &TokenKind::Mut => BinOp::BitwiseAnd,
            _ => return None,
        };
        // Don't bother checking for postfix placement; it's assumed if this were a postfix operator,
        // this method (`parse_binary_operator`) would never be called.
        let placement = op.placement();
        debug_assert!(placement.contains(OpPlacement::INFIX));
        if placement.contains(OpPlacement::PREFIX) {
            let lhs_whitespace = self.peek_prev_including_insignificant(p).kind.is_insignificant();
            let rhs_whitespace = !self.peek_next_including_insignificant(p).kind.could_begin_expression();
            if lhs_whitespace && !rhs_whitespace { return None; }
        }
        self.next(p);
        Some(op)
    }

    fn parse_prefix_operator(&mut self, p: &mut Parser) -> Option<(UnOp, SourceRange)> {
        let tok = self.cur(p);
        let mut range = tok.range.clone();
        let op = match tok.kind {
            TokenKind::Sub        => UnOp::Neg,
            TokenKind::Add        => UnOp::Plus,
            TokenKind::LogicalNot => UnOp::Not,
            TokenKind::Asterisk   => UnOp::Deref,
            TokenKind::Ampersand  => if let TokenKind::Mut = self.peek_next(p).kind {
                let mut_range = self.next(p).range.clone();
                range = source_info::concat(range, mut_range);
                UnOp::AddrOfMut
            } else {
                UnOp::AddrOf
            },
            _ => return None,
        };
        debug_assert!(op.placement().contains(OpPlacement::PREFIX));
        self.next(p);
        Some((op, range))
    }

    fn parse_postfix_operator(&mut self, p: &mut Parser) -> Option<(UnOp, SourceRange)> {
        let lhs_whitespace = self.peek_prev_including_insignificant(p).kind.is_insignificant();
        let tok = self.cur(p);
        let mut range = tok.range.clone();
        let op = match tok.kind {
            TokenKind::Asterisk => if let TokenKind::Mut = self.peek_next(p).kind {
                let mut_range = self.next(p).range.clone();
                range = source_info::concat(range, mut_range);
                UnOp::PointerMut
            } else {
                UnOp::Pointer
            },
            _ => return None,
        };
        let placement = op.placement();
        debug_assert!(placement.contains(OpPlacement::POSTFIX));
        if placement.contains(OpPlacement::PREFIX) {
            debug_assert!(
                placement.contains(OpPlacement::INFIX),
                "Operators that can be prefix and postfix but not infix are not supported! See https://github.com/zachrwolfe/meda/issues/14"
            );
            let rhs_whitespace = !self.peek_next_including_insignificant(p).kind.could_begin_expression();
            if lhs_whitespace || !rhs_whitespace { return None; }
        }
        self.next(p);

        Some((op, range))
    }

    fn try_parse_term(&mut self, p: &mut Parser) -> Result<ExprId, TokenKind> {
        let mut term = self.try_parse_non_cast_term(p)?;
        let mut range = self.hir.get_range(term);
        while let TokenKind::As = self.cur(p).kind {
            self.next(p);
            let (ty, ty_range) = self.parse_type(p);
            range = source_info::concat(range, ty_range);
            term = self.hir.cast(term, ty, range.clone());
        }
        Ok(term)
    }

    fn try_parse_non_cast_term(&mut self, p: &mut Parser) -> Result<ExprId, TokenKind> {
        if let Some((op, op_range)) = self.parse_prefix_operator(p) {
            let term = self.try_parse_non_cast_term(p)
                .unwrap_or_else(|tok| panic!("Expected expression after unary operator, found {:?}", tok));
            let term_range = self.hir.get_range(term);
            return Ok(self.un_op(op, term, source_info::concat(op_range, term_range)));
        }

        match self.cur(p).kind {
            TokenKind::LeftParen => {
                let open_paren_range = self.cur(p).range.clone();
                self.next(p);
                let expr = self.parse_expr(p);
                if let TokenKind::RightParen = self.cur(p).kind {}
                else {
                    self.errors.push(
                        Error::new("unclosed parentheses")
                            .adding_primary_range(self.cur(p).range.clone(), "paren here")
                    );
                }
                let close_paren_range = self.cur(p).range.clone();
                self.hir.set_range(expr, source_info::concat(open_paren_range, close_paren_range));
                self.next(p);
                Ok(expr)
            },
            &TokenKind::IntLit(val) => {
                let lit = self.hir.int_lit(val, self.cur(p).range.clone());
                self.next(p);
                Ok(lit)
            },
            &TokenKind::DecLit(val) => {
                let lit = self.hir.dec_lit(val, self.cur(p).range.clone());
                self.next(p);
                Ok(lit)
            },
            TokenKind::StrLit(val) => {
                let val = val.clone();
                let lit = self.hir.str_lit(val, self.cur(p).range.clone());
                self.next(p);
                Ok(lit)
            },
            &TokenKind::CharLit(val) => {
                let lit = self.hir.char_lit(val, self.cur(p).range.clone());
                self.next(p);
                Ok(lit)
            },
            &TokenKind::Ident(name) => {
                let name_range = self.cur(p).range.clone();
                let mut args = SmallVec::new();
                let mut end_range = name_range.clone();
                if let TokenKind::LeftParen = self.next(p).kind {
                    self.next(p);
                    loop {
                        // TODO: actually implement proper comma and newline handling like I've thought about
                        let Token { kind, range } = self.cur(p);
                        match kind {
                            TokenKind::RightParen => {
                                end_range = range.clone();
                                self.next(p);
                                break;
                            }
                            TokenKind::Comma => { self.next(p); }
                            TokenKind::Eof => {
                                panic!("Reached eof in middle of decl ref");
                            }
                            _ => { args.push(self.parse_expr(p)); }
                        }
                    }
                }
                let decl_ref = self.hir.decl_ref(
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
                let do_range = self.cur(p).range.clone();
                self.next(p);
                let (scope, scope_range) = self.parse_scope(p);
                Ok(self.hir.do_expr(scope, source_info::concat(do_range, scope_range)))
            },
            TokenKind::If => Ok(self.parse_if(p)),
            TokenKind::While => {
                let while_range = self.cur(p).range.clone();
                self.next(p);
                let condition = self.parse_expr(p);
                let (scope, scope_range) = self.parse_scope(p);
                Ok(self.hir.while_expr(condition, scope, source_info::concat(while_range, scope_range)))
            },
            TokenKind::Return => {
                let ret_range = self.cur(p).range.clone();
                self.next(p);
                let ret_expr = self.try_parse_expr(p).unwrap_or_else(|_| self.hir.void_expr);
                let expr_range = self.hir.get_range(ret_expr);
                Ok(self.hir.ret(ret_expr, source_info::concat(ret_range, expr_range)))
            },
            x => Err(x.clone()),
        }.map(|expr| match self.parse_postfix_operator(p) {
            None => expr,
            Some((op, mut range)) => {
                range = source_info::concat(range, self.hir.get_range(expr));
                self.un_op(op, expr, range)
            }
        })
    }

    fn try_parse_expr(&mut self, p: &mut Parser) -> Result<ExprId, TokenKind> {
        const INLINE: usize = 5;
        let mut expr_stack = SmallVec::<[ExprId; INLINE]>::new();
        let mut op_stack = SmallVec::<[BinOp; INLINE]>::new();
        expr_stack.push(self.try_parse_term(p)?);

        // It's kind of silly that this is a macro, but I'm not aware of any other
        // way to do it without upsetting the borrow checker?
        macro_rules! pop_stacks {
            () => {
                let rhs = expr_stack.pop().unwrap();
                let lhs = expr_stack.pop().unwrap();
                let next_op = op_stack.pop().unwrap();
                let range = source_info::concat(
                    self.hir.get_range(lhs),
                    self.hir.get_range(rhs),
                );
                expr_stack.push(self.bin_op(next_op, lhs, rhs, range));
            }
        }

        while let Some(op) = self.parse_binary_operator(p) {
            while let Some(other_op) = op_stack.last() {
                if other_op.precedence() > op.precedence() {
                    break;
                }

                pop_stacks!();
            }
            op_stack.push(op);
            expr_stack.push(self.try_parse_term(p)?);
        }
        while !op_stack.is_empty() { pop_stacks!(); }

        Ok(expr_stack.pop().unwrap())
    }

    fn parse_expr(&mut self, p: &mut Parser) -> ExprId {
        self.try_parse_expr(p).unwrap_or_else(|tok| panic!("UNHANDLED TERM: {:#?}", tok))
    }

    fn parse_if(&mut self, p: &mut Parser) -> ExprId {
        let Token { kind, range: if_range } = self.cur(p);
        let if_range = if_range.clone();
        assert_eq!(kind, &TokenKind::If);
        self.next(p);
        let condition = self.parse_expr(p);
        let (then_scope, then_range) = self.parse_scope(p);
        let mut range = source_info::concat(if_range, then_range);
        let else_scope = if let TokenKind::Else = self.cur(p).kind {
            match self.next(p).kind {
                TokenKind::If => {
                    let scope = self.hir.begin_scope();
                    let if_expr = self.parse_if(p);
                    let if_range = self.hir.get_range(if_expr);
                    range = source_info::concat(range, if_range);
                    self.hir.stmt(if_expr);
                    self.hir.end_scope(true);
                    Some(scope)
                },
                TokenKind::OpenCurly => {
                    let (else_scope, else_range) = self.parse_scope(p);
                    range = source_info::concat(range, else_range);
                    Some(else_scope)
                },
                _ => panic!("Expected '{' or 'if' after 'else'"),
            }
        } else {
            None
        };
        self.hir.if_expr(condition, then_scope, else_scope, range)
    }

    /// Parses any node. Iff the node is an expression, returns its ExprId.
    fn parse_node(&mut self, p: &mut Parser) -> Option<ExprId> {
        match self.cur(p).kind {
            &TokenKind::Ident(name) => {
                if let TokenKind::Colon = self.peek_next(p).kind {
                    self.parse_decl(name, p);
                    None
                } else {
                    Some(self.parse_expr(p))
                }
            },
            TokenKind::Fn => {
                self.parse_comp_decl(p);
                None
            }
            _ => Some(self.parse_expr(p))
        }
    }

    fn parse_decl(&mut self, name: Sym, p: &mut Parser) {
        let name_range = self.cur(p).range.clone();
        // Skip to colon, get range.
        let colon_range = self.next(p).range.clone();
        let mut found_separator = true;
        let explicit_ty = match self.next(p).kind {
            TokenKind::Ident(_) => Some(self.parse_type(p).0),
            _ => None,
        };
        let is_mut = match self.cur(p).kind {
            TokenKind::Assign => true,
            TokenKind::Colon => false,
            _ => {
                self.errors.push(
                    Error::new("expected '=' or ':' after ':' when parsing declaration")
                        .adding_primary_range(colon_range, "':' here")
                );
                found_separator = false;

                true
            }
        };

        if found_separator {
            self.next(p);
        }

        let root = self.parse_expr(p);
        let root_range = self.hir.get_range(root);
        self.hir.stored_decl(name, explicit_ty, is_mut, root, source_info::concat(name_range, root_range));
    }

    // Parses an open curly brace, then a list of nodes, then a closing curly brace.
    fn parse_scope(&mut self, p: &mut Parser) -> (ScopeId, SourceRange) {
        let scope = self.hir.begin_scope();
        let mut last_was_expr = false;
        let Token { kind, range: open_curly_range } = self.cur(p);
        let open_curly_range = open_curly_range.clone();
        assert_eq!(kind, &TokenKind::OpenCurly);
        self.next(p);
        let close_curly_range = loop {
            match self.cur(p).kind {
                TokenKind::Eof => panic!("Unexpected eof while parsing scope"),
                TokenKind::CloseCurly => {
                    let close_curly_range = self.cur(p).range.clone();
                    self.next(p);
                    break close_curly_range;
                },
                _ => {
                    let node = self.parse_node(p);

                    // If the node was a standalone expression, make it a statement
                    if let Some(expr) = node {
                        last_was_expr = true;
                        self.hir.stmt(expr);
                    } else {
                        last_was_expr = false;
                    }
                }
            }
        };
        self.hir.end_scope(last_was_expr);
        (scope, source_info::concat(open_curly_range, close_curly_range))
    }

    fn parse_comp_decl(&mut self, p: &mut Parser) {
        assert_eq!(self.cur(p).kind, &TokenKind::Fn);
        let mut proto_range = self.cur(p).range.clone();
        let name = if let TokenKind::Ident(name) = *self.next(p).kind {
            name
        } else {
            panic!("expected function name after 'fn'")
        };
        proto_range = source_info::concat(proto_range, self.cur(p).range.clone());
        let mut param_names = SmallVec::new();
        let mut param_tys = SmallVec::new();
        if let TokenKind::LeftParen = self.next(p).kind {
            self.next(p);
            while let TokenKind::Ident(name) = *self.cur(p).kind {
                param_names.push(name);
                assert_eq!(self.next(p).kind, &TokenKind::Colon);
                self.next(p);
                let (ty, _range) = self.parse_type(p);
                param_tys.push(ty);
                while let TokenKind::Comma = self.cur(p).kind {
                    self.next(p);
                }
            }
            assert_eq!(self.cur(p).kind, &TokenKind::RightParen);
            proto_range = source_info::concat(proto_range, self.cur(p).range.clone());
            self.next(p);
        }
        let ty = match self.cur(p).kind {
            TokenKind::Colon => {
                self.next(p);
                let (ty, range) = self.parse_type(p);
                proto_range = source_info::concat(proto_range, range);
                Some(ty)
            },
            TokenKind::OpenCurly => Some(self.hir.void_ty),
            TokenKind::Assign => None,
            tok => panic!("Invalid token {:?}", tok),
        };
        self.hir.begin_computed_decl(name, param_names, param_tys, ty.clone(), proto_range);
        match self.cur(p).kind {
            TokenKind::OpenCurly => {
                self.parse_scope(p);
            },
            TokenKind::Assign => {
                self.next(p);
                self.hir.begin_scope();
                let assigned_expr = self.parse_expr(p);
                self.hir.stmt(assigned_expr);
                self.hir.end_scope(true);
            },
            tok => panic!("Invalid token {:?}", tok),
        }
        self.hir.end_computed_decl();
    }

    fn parse_type(&mut self, p: &mut Parser) -> (ExprId, SourceRange) {
        let begin_range = self.cur(p).range.clone();
        // This is a term and not an expression because assignments are valid expressions.
        //     For example: `foo: SomeType = ...` <- the parser would think you were assigning `...` to `SomeType` and
        //     taking the `void` result of that assignment as the type of variable declaration `foo`
        // TODO: add statements as a slight superset of expressions which includes assignments.
        let ty = self.try_parse_term(p).unwrap();
        let end_range = self.cur(p).range.clone();
        let range = begin_range.start..end_range.start;

        (ty, range)
    }

    fn cur(&self, p: &Parser) -> Token {
        self.toks.at(p.cur)
    }

    // TODO: come up with a better term for whitespace and comments than "insignificant".
    // If they really were insignificant, we wouldn't be lexing them. :)
    fn skip_insignificant(&mut self, p: &mut Parser) {
        while self.cur(p).kind.is_insignificant() {
            self.next_including_insignificant(p);
        }
    }

    fn next_including_insignificant(&mut self, p: &mut Parser) -> Token {
        p.cur += 1;
        self.cur(p)
    }

    fn next(&mut self, p: &mut Parser) -> Token {
        self.next_including_insignificant(p);
        self.skip_insignificant(p);
        self.cur(p)
    }

    fn peek_next_including_insignificant(&self, p: &Parser) -> Token {
        self.toks.at(p.cur+1)
    }

    fn peek_prev_including_insignificant(&self, p: &Parser) -> Token {
        self.toks.at(p.cur - 1)
    }

    fn peek_next(&self, p: &Parser) -> Token {
        for i in (p.cur+1)..self.toks.len() {
            let cur = self.toks.at(i);
            if cur.kind.is_significant() {
                return cur
            }
        }
        panic!("No significant token found");
    }
}
