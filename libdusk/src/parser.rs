use std::collections::HashMap;

use arrayvec::ArrayVec;
use smallvec::{SmallVec, smallvec};
use string_interner::DefaultSymbol as Sym;
use derivative::Derivative;

use dusk_dire::hir::{self, ExprId, DeclId, ConditionNsId, Item, ImperScopeId, Intrinsic, Attribute, FieldAssignment, Ident, Pattern, PatternKind, SwitchCase, ImperScopedDecl, ExternMod, ERROR_EXPR, ERROR_TYPE, VOID_TYPE};
use dusk_dire::ty::Type;
use dusk_dire::source_info::{self, SourceFileId, SourceRange};

use crate::driver::Driver;
use crate::hir::{ConditionKind, GenericParamList};
use crate::token::{TokenKind, Token};
use crate::builder::{BinOp, UnOp, OpPlacement};
use crate::error::Error;
use crate::autopop::{AutoPopStack, AutoPopStackEntry};
use crate::dvd::{self, Message as DvdMessage};

use dusk_proc_macros::*;

struct Parser {
    file: SourceFileId,
    cur: usize,
    list_stack: AutoPopStack<ListState>,
    list_counter: usize,
}

#[derive(Debug, Clone)]
struct TokenBeginLoc {
    /// zero-based line number
    line: usize,
    /// number of bytes the token is away from the beginning of the line
    offset: usize,

    range: SourceRange,
}

#[derive(Derivative)]
#[derivative(Debug)]
#[derive(Clone)]
struct ListState {
    id: usize,
    first_token_loc: TokenBeginLoc,
    separators: ArrayVec<TokenKind, 2>,
    terminator: Option<TokenKind>,
    last_item_had_separator: bool,
    #[derivative(Debug="ignore")]
    could_begin_item_proc: fn(&TokenKind) -> bool,
}

impl PartialEq<ListState> for usize {
    fn eq(&self, other: &ListState) -> bool {
        *self == other.id
    }
}

#[derive(Debug)]
pub enum ParseError {
    UnexpectedToken(TokenKind),
    UnableToLex,
    Eof,
}
pub type ParseResult<T> = Result<T, ParseError>;

#[derive(Debug)]
enum SeparatorResult {
    Implicit,
    Explicit(TokenKind),
    Terminator,
    Eof,
    None,
}

impl Driver {
    pub fn parse_added_files(&mut self) -> ParseResult<()> {
        // TODO: this is pretty dumb! I think the unparsed_files set always consists of contiguous values, so I should
        // just make it a Range.
        let mut unparsed_files: Vec<_> = self.src_map.unparsed_files.iter().copied().collect();
        unparsed_files.sort();
        for file in unparsed_files {
            dvd::send(|| DvdMessage::WillBeginParsingInputFile(self.src_map.files[file].location.clone()));
            self.src_map.unparsed_files.remove(&file);
            self.parse_single_file(file)?;
        }
        Ok(())
    }

    fn get_tok_begin_loc(&self, range: SourceRange) -> TokenBeginLoc {
        let (_, line, offset) = self.lookup_file_line_and_offset(range.start);
        TokenBeginLoc {
            line,
            offset,
            range,
        }
    }

    fn get_cur_begin_loc(&self, p: &Parser) -> TokenBeginLoc {
        let range = self.cur(p).range;
        self.get_tok_begin_loc(range)
    }

    fn begin_list(&mut self, p: &mut Parser, could_begin_item_proc: fn(&TokenKind) -> bool, separators: impl IntoIterator<Item=TokenKind>, terminator: Option<TokenKind>) -> AutoPopStackEntry<ListState, usize> {
        let id = p.list_counter;
        p.list_counter += 1;
        let first_token_loc = self.get_cur_begin_loc(p);
        let separators: ArrayVec<TokenKind, 2> = separators.into_iter().collect();
        if let Some(terminator) = &terminator {
            debug_assert!(!separators.contains(terminator));
        }
        debug_assert!(separators.iter().all(|sep| sep.pretty_print_separator().is_some()));
        p.list_stack.push(
            id,
            ListState {
                id,
                first_token_loc,
                separators,
                terminator,
                last_item_had_separator: true,
                could_begin_item_proc,
            }
        )
    }

    fn start_next_list_item(&mut self, p: &mut Parser, id: usize) {
        let new_loc = self.get_cur_begin_loc(p);
        let (begin_range, had_separator) = p.list_stack.peek_mut(|state| {
            let state = state.unwrap();
            debug_assert_eq!(id, state.id);
            let begin_range = state.first_token_loc.range;
            let had_separator = state.last_item_had_separator;
            state.last_item_had_separator = true;
            state.first_token_loc = new_loc;
            (begin_range, had_separator)
        });

        let state = p.list_stack.peek().unwrap();
        if !had_separator {
            let range = self.cur(p).range;
            let mut msg = "expected ".to_string();
            for sep in &state.separators {
                msg.push('\'');
                msg.push_str(sep.pretty_print_separator().unwrap());
                msg.push_str("', ");
            }
            msg.push_str("or a newline followed by no less whitespace than the beginning of the previous item");
            self.diag.report_error("no separator found", range, msg)
                .adding_secondary_range_with_msg(begin_range.first_char_only(), "if on a separate line, the next item must start no later than this column");
        }
    }

    fn has_implicit_separator_impl(&self, p: &Parser, cur: Token, prev_range: SourceRange) -> bool {
        if let Some(state) = p.list_stack.peek() {
            let prev_loc = self.get_tok_begin_loc(prev_range);
            let loc = self.get_tok_begin_loc(cur.range);
            loc.line > prev_loc.line && loc.offset <= state.first_token_loc.offset && (state.could_begin_item_proc)(cur.kind)
        } else {
            false
        }
    }

    fn has_implicit_separator(&self, p: &Parser) -> bool {
        let cur = self.cur(p);
        let prev_range = self.peek_prev(p).range;
        self.has_implicit_separator_impl(p, cur, prev_range)
    }

    fn next_tok_has_implicit_separator(&self, p: &Parser) -> bool {
        let cur = self.peek_next(p);
        let prev_range = self.cur(p).range;
        self.has_implicit_separator_impl(p, cur, prev_range)
    }

    fn eat_separators(&mut self, p: &mut Parser) -> SeparatorResult {
        let list_state = p.list_stack.peek().expect("invalid call to expect_separator()");
        let found_implicit = self.has_implicit_separator(p);
        let mut found_explicit = None;
        let mut first_extraneous_separator = None;
        let mut num_separators_found = 0;
        loop {
            let Token { kind: cur_tok, range } = self.cur(p);
            if Some(cur_tok) == list_state.terminator.as_ref() {
                if num_separators_found == 0 {
                    return SeparatorResult::Terminator;
                } else {
                    break;
                }
            } else if matches!(cur_tok, TokenKind::Eof) {
                return SeparatorResult::Eof;
            } else if list_state.separators.contains(cur_tok) {
                num_separators_found += 1;
                if num_separators_found == 1 {
                    found_explicit = Some((cur_tok.clone(), range));
                } else if num_separators_found == 2 {
                    first_extraneous_separator = Some(range);
                }
                self.next(p);
            } else {
                break;
            }
        }

        if let Some(first_extraneous_separator) = first_extraneous_separator {
            let ess = if num_separators_found > 2 {
                "s"
            } else {
                ""
            };
            
            self.diag.report_warning_no_range_msg(format!("extraneous separator{}", ess), first_extraneous_separator)
                .adding_secondary_range_with_msg(found_explicit.as_ref().unwrap().1, "first separator here is sufficient");
        }

        if let Some((tok, _)) = found_explicit {
            SeparatorResult::Explicit(tok)
        } else if found_implicit {
            SeparatorResult::Implicit
        } else {
            p.list_stack.peek_mut(|state| state.unwrap().last_item_had_separator = false);
            SeparatorResult::None
        }
    }

    fn parse_single_file(&mut self, file: SourceFileId) -> ParseResult<()> {
        self.lex(file).map_err(|_| ParseError::UnableToLex)?;
        let _new_file = self.start_new_file(file);
        let mut p = Parser { file, cur: 0, list_stack: Default::default(), list_counter: 0 };

        self.skip_insignificant(&mut p);
        let item_list = self.begin_list(&mut p, TokenKind::could_begin_statement, [TokenKind::Semicolon], None);
        loop {
            match self.cur(&p).kind {
                TokenKind::Eof => break,
                TokenKind::CloseCurly => {
                    self.diag.push(
                        Error::new("Extraneous closing brace '}'")
                            .adding_primary_range(self.cur(&p).range, "brace here")
                    );
                    self.next(&mut p);
                },
                _ => {
                    self.start_next_list_item(&mut p, item_list.id());
                    self.parse_item(&mut p)?;
                    self.eat_separators(&mut p);
                }
            }
        }
        Ok(())
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
            TokenKind::XorAssign => BinOp::XorAssign,
            TokenKind::LeftShiftAssign => BinOp::LeftShiftAssign,
            TokenKind::RightShiftAssign => BinOp::RightShiftAssign,
            TokenKind::Caret => BinOp::BitwiseXor,
            TokenKind::LeftShift => BinOp::LeftShift,
            TokenKind::RightShift => BinOp::RightShift,
            TokenKind::Equal => BinOp::Eq,
            TokenKind::NotEqual => BinOp::NotEq,
            TokenKind::Lt => BinOp::Less,
            TokenKind::Lte => BinOp::LessOrEq,
            TokenKind::GT => BinOp::Greater,
            TokenKind::Gte => BinOp::GreaterOrEq,
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
        let mut range = tok.range;
        let op = match tok.kind {
            TokenKind::Sub        => UnOp::Neg,
            TokenKind::Add        => UnOp::Plus,
            TokenKind::LogicalNot => UnOp::Not,
            TokenKind::Tilde      => UnOp::BitwiseNot,
            TokenKind::Asterisk   => UnOp::Deref,
            TokenKind::Ampersand  => if let TokenKind::Mut = self.peek_next(p).kind {
                let mut_range = self.next(p).range;
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
        let mut range = tok.range;
        let op = match tok.kind {
            TokenKind::Asterisk => if let TokenKind::Mut = self.peek_next(p).kind {
                let mut_range = self.next(p).range;
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
                "Operators that can be prefix and postfix but not infix are not supported. See https://github.com/dusk-lang/dusk/issues/14"
            );
            let rhs_whitespace = !self.peek_next_including_insignificant(p).kind.could_begin_expression();
            if lhs_whitespace || !rhs_whitespace { return None; }
        }
        self.next(p);

        Some((op, range))
    }

    fn try_parse_term(&mut self, p: &mut Parser, parse_struct_lits: bool) -> ParseResult<ExprId> {
        let mut term = self.try_parse_restricted_term(p)?;
        let range = self.get_range(term);
        if parse_struct_lits {
            if let TokenKind::OpenCurly = self.cur(p).kind {
                self.next(p);
                let mut fields = Vec::new();
                let field_list = self.begin_list(p, TokenKind::could_begin_struct_literal_field, [TokenKind::Comma], Some(TokenKind::CloseCurly));
                let close_curly_range = loop {
                    match self.cur(p).kind {
                        TokenKind::Eof => panic!("Unexpected eof while parsing struct literal"),
                        TokenKind::CloseCurly => {
                            let close_curly_range = self.cur(p).range;
                            self.next(p);
                            break close_curly_range;
                        },
                        _ => {
                            if let &TokenKind::Ident(name) = self.cur(p).kind {
                                self.start_next_list_item(p, field_list.id());
                                self.next(p);
                                self.eat_tok(p, TokenKind::Colon)?;
                                let expr = self.parse_expr(p).unwrap_or(ERROR_TYPE);
                                fields.push(
                                    FieldAssignment { name, expr }
                                );
                                self.eat_separators(p);
                            } else {
                                let range = self.cur(p).range;
                                self.diag.report_error("unexpected token", range, "expected field name");
                                self.next(p);
                            }
                        }
                    }
                };
                let lit_range = source_info::concat(range, close_curly_range);
                term = self.struct_lit(term, fields, lit_range);
            }
        }
        while let TokenKind::As = self.cur(p).kind {
            let as_range = self.cur(p).range;
            self.next(p);
            let (ty, ty_range) = self.parse_type(p);
            let range = source_info::concat(as_range, ty_range);
            term = self.cast(term, ty, range);
        }
        Ok(term)
    }

    fn parse_decl_ref(&mut self, p: &mut Parser, base_expr: Option<ExprId>, name: Sym) -> ParseResult<ExprId> {
        let name_range = self.cur(p).range;
        self.next(p);
        if !self.has_implicit_separator(p) && matches!(self.cur(p).kind, TokenKind::OpenSquareBracket) {
            let open_square_bracket_range = self.cur(p).range;
            self.next(p);
            let generic_arg_list = self.begin_list(p, TokenKind::could_begin_expression, [TokenKind::Comma], Some(TokenKind::CloseSquareBracket));
            let mut args = Vec::new();
            loop {
                let kind = self.cur(p).kind;
                match kind {
                    TokenKind::CloseSquareBracket => {
                        self.next(p);
                        break;
                    }
                    TokenKind::Eof => {
                        self.diag.push(
                            Error::new("unclosed generic argument list")
                                .adding_primary_range(open_square_bracket_range, "generic argument list began here")
                        );
                        return Err(ParseError::Eof);
                    }
                    _ => {
                        self.start_next_list_item(p, generic_arg_list.id());
                        if let Ok(arg) = self.parse_expr(p) {
                            args.push(arg);
                            self.eat_separators(p);
                        }
                    }
                }
            }
        }
        let generic_ctx = self.begin_decl_ref_generic_ctx();
        let mut args = SmallVec::new();
        let mut has_parens = false;
        if !self.has_implicit_separator(p) && matches!(self.cur(p).kind, TokenKind::LeftParen) {
            has_parens = true;
            self.next(p);
            let arg_list = self.begin_list(p, TokenKind::could_begin_expression, [TokenKind::Comma], Some(TokenKind::RightParen));
            loop {
                match self.cur(p).kind {
                    TokenKind::RightParen => {
                        self.next(p);
                        break;
                    }
                    TokenKind::Eof => {
                        panic!("Reached eof in middle of decl ref");
                    }
                    _ => {
                        self.start_next_list_item(p, arg_list.id());
                        if let Ok(arg) = self.parse_expr(p) {
                            args.push(arg);
                            self.eat_separators(p);
                        }
                    }
                }
            }
        }
        Ok(
            self.decl_ref(
                base_expr,
                name,
                args,
                has_parens,
                name_range,
                generic_ctx,
            )
        )
    }

    fn parse_while(&mut self, p: &mut Parser, label: Option<Ident>) -> ParseResult<ExprId> {
        let while_range = self.eat_tok(p, TokenKind::While)?;
        let condition = self.parse_non_struct_lit_expr(p);
        let looop = self.begin_loop(label);
        let loop_id = looop.id();
        let (scope, scope_range) = self.parse_scope(p, &[])?;
        self.end_loop(looop);
        Ok(self.while_expr(loop_id, condition, scope, source_info::concat(while_range, scope_range)))
    }

    fn parse_for(&mut self, p: &mut Parser, label: Option<Ident>) -> ParseResult<ExprId> {
        let for_range = self.eat_tok(p, TokenKind::For)?;
        let is_mut = matches!(self.cur(p).kind, TokenKind::Mut);
        if is_mut {
            self.next(p);
        }
        let var_name = self.eat_ident(p);
        let var_ty = matches!(self.cur(p).kind, TokenKind::Colon).then(|| {
            self.next(p);
            self.parse_type(p).0
        });
        self.eat_tok(p, TokenKind::In)?;
        let lower_bound = self.parse_expr(p).unwrap_or_else(|err| err);
        self.eat_tok(p, TokenKind::DoubleDot)?;
        let upper_bound = self.parse_non_struct_lit_expr(p);
        let stored_decl_id = self.next_stored_decl();
        let binding_decl = self.add_decl(hir::Decl::LoopBinding { id: stored_decl_id, is_mut }, var_name.symbol, var_ty, var_name.range);
        let binding_decl = ImperScopedDecl {
            name: var_name.symbol,
            num_params: 0,
            id: binding_decl,
        };
        let looop = self.begin_loop(label);
        let loop_id = looop.id();
        let (scope, _scope_range) = self.parse_scope(p, &[binding_decl])?;
        self.end_loop(looop);
        Ok(self.for_expr(loop_id, binding_decl.id, lower_bound, upper_bound, scope, for_range))
    }

    /// A restricted term doesn't include cast or struct literal expressions
    fn try_parse_restricted_term(&mut self, p: &mut Parser) -> ParseResult<ExprId> {
        if let Some((op, op_range)) = self.parse_prefix_operator(p) {
            let term = self.try_parse_restricted_term(p)
                .unwrap_or_else(|tok| panic!("Expected expression after unary operator, found {:?}", tok));
            let term_range = self.get_range(term);
            return Ok(self.un_op(op, term, source_info::concat(op_range, term_range)));
        }

        match self.cur(p).kind {
            TokenKind::LeftParen => {
                let open_paren_range = self.cur(p).range;
                self.next(p);
                let expr = self.parse_expr(p).unwrap_or_else(|err| err);
                if let TokenKind::RightParen = self.cur(p).kind {}
                else {
                    self.diag.push(
                        Error::new("unclosed parentheses")
                            .adding_primary_range(self.cur(p).range, "paren here")
                    );
                }
                let close_paren_range = self.cur(p).range;
                ef!(expr.range) = source_info::concat(open_paren_range, close_paren_range);
                self.next(p);
                Ok(expr)
            },
            TokenKind::DebugMark => {
                self.next(p);
                self.eat_tok(p, TokenKind::LeftParen)?;
                let expr = if let Ok(expr) = self.parse_expr(p) {
                    self.debug_mark_expr(expr);
                    expr
                } else {
                    ERROR_EXPR
                };
                self.eat_tok(p, TokenKind::RightParen)?;
                Ok(expr)
            },
            &TokenKind::IntLit(val) => {
                let lit = self.int_lit(val, self.cur(p).range);
                self.next(p);
                Ok(lit)
            },
            &TokenKind::DecLit(val) => {
                let lit = self.dec_lit(val, self.cur(p).range);
                self.next(p);
                Ok(lit)
            },
            TokenKind::StrLit(val) => {
                let val = val.clone();
                let lit = self.str_lit(val, self.cur(p).range);
                self.next(p);
                Ok(lit)
            },
            &TokenKind::CharLit(val) => {
                let lit = self.char_lit(val, self.cur(p).range);
                self.next(p);
                Ok(lit)
            },
            TokenKind::True => {
                let lit = self.bool_lit(true, self.cur(p).range);
                self.next(p);
                Ok(lit)
            },
            TokenKind::False => {
                let lit = self.bool_lit(false, self.cur(p).range);
                self.next(p);
                Ok(lit)
            },
            &TokenKind::Ident(name) => Ok(self.parse_decl_ref(p, None, name)?),
            TokenKind::Do => {
                let do_range = self.eat_tok(p, TokenKind::Do)?;
                let (scope, scope_range) = self.parse_scope(p, &[])?;
                Ok(self.do_expr(scope, source_info::concat(do_range, scope_range)))
            },
            TokenKind::Module => self.parse_module(p),
            TokenKind::ExternModule => self.parse_extern_module(p),
            TokenKind::Struct => self.parse_struct(p),
            TokenKind::Enum => self.parse_enum(p),
            TokenKind::If => self.parse_if(p),
            TokenKind::While => self.parse_while(p, None),
            TokenKind::For => self.parse_for(p, None),
            TokenKind::Colon => {
                self.eat_tok(p, TokenKind::Colon)?;
                let label = self.eat_ident(p);
                match self.cur(p).kind {
                    TokenKind::While => self.parse_while(p, Some(label)),
                    TokenKind::For => self.parse_for(p, Some(label)),
                    unexpected_token => {
                        let unexpected_token = unexpected_token.clone();
                        let range = self.cur(p).range;
                        self.diag.push(
                            Error::new("unexpected token")
                                .adding_primary_range(range, "expected `while` or `for` instead")
                        );
                        Err(ParseError::UnexpectedToken(unexpected_token))
                    },
                }
            },
            TokenKind::Switch => self.parse_switch(p),
            TokenKind::Return => {
                let ret_range = self.cur(p).range;
                self.next(p);
                let ret_expr = self.try_parse_expr(p, true).unwrap_or(hir::VOID_EXPR);
                let expr_range = self.get_range(ret_expr);
                Ok(self.ret(ret_expr, source_info::concat(ret_range, expr_range)))
            },
            TokenKind::Break => {
                let range = self.cur(p).range;
                self.next(p);
                let label = matches!(self.cur(p).kind, TokenKind::Colon).then(|| {
                    self.next(p);
                    self.eat_ident(p)
                });
                Ok(self.break_expr(range, label))
            },
            TokenKind::Continue => {
                let range = self.cur(p).range;
                self.next(p);
                let label = matches!(self.cur(p).kind, TokenKind::Colon).then(|| {
                    self.next(p);
                    self.eat_ident(p)
                });
                Ok(self.continue_expr(range, label))
            },
            TokenKind::Fn => {
                let fn_range = self.cur(p).range;
                self.next(p);
                self.eat_tok(p, TokenKind::LeftParen)?;
                let mut param_tys = Vec::new();
                let param_ty_list = self.begin_list(p, TokenKind::could_begin_expression, [TokenKind::Comma], Some(TokenKind::RightParen));
                loop {
                    match self.cur(p).kind {
                        TokenKind::Eof => panic!("Unexpected eof while parsing function type"),
                        TokenKind::RightParen => break,
                        _ => {
                            self.start_next_list_item(p, param_ty_list.id());
                            param_tys.push(self.parse_type(p).0);
                            self.eat_separators(p);
                        },
                    }
                }
                self.eat_tok(p, TokenKind::RightParen)?;
                let ret_ty = if matches!(self.cur(p).kind, TokenKind::ReturnArrow) {
                    self.next(p);
                    self.parse_type(p).0
                } else {
                    VOID_TYPE
                };
                Ok(self.fn_type(param_tys, ret_ty, fn_range))
            },
            x => Err(ParseError::UnexpectedToken(x.clone())),
        }.and_then(|mut expr| {
            // Parse arbitrary sequence of postfix operators and member refs
            loop {
                let mut modified = false;
                while let Some((op, mut range)) = self.parse_postfix_operator(p) {
                    range = source_info::concat(range, self.get_range(expr));
                    expr = self.un_op(op, expr, range);

                    modified = true;
                }
                while self.cur(p).kind == &TokenKind::Dot && !self.has_implicit_separator(p) {
                    let dot_range = self.cur(p).range;
                    let name = match self.next(p).kind {
                        &TokenKind::Ident(name) => name,
                        TokenKind::CloseSquareBracket | TokenKind::CloseCurly | TokenKind::RightParen | TokenKind::Comma => {
                            let range = self.cur(p).range;
                            self.diag.report_error("expected identifier after '.'", dot_range, "'.' here")
                                .adding_secondary_range_with_msg(range, "note: found this instead");
                            return Ok(expr)
                        },
                        _ => {
                            self.diag.push(
                                Error::new("expected identifier after '.'")
                                    .adding_primary_range(dot_range, "'.' here")
                                    .adding_secondary_range(self.cur(p).range, "note: found this instead")
                            );
                            self.next(p);
                            return Ok(expr)
                        }
                    };
                    expr = self.parse_decl_ref(p, Some(expr), name)?;

                    modified = true;
                }
                if !modified { break; }
            }
            Ok(expr)
        })
    }

    fn try_parse_expr(&mut self, p: &mut Parser, parse_struct_lits: bool) -> ParseResult<ExprId> {
        const INLINE: usize = 5;
        let mut expr_stack = SmallVec::<[ExprId; INLINE]>::new();
        let mut op_stack = SmallVec::<[BinOp; INLINE]>::new();
        expr_stack.push(self.try_parse_term(p, parse_struct_lits)?);

        // It's kind of silly that this is a macro, but I'm not aware of any other
        // way to do it without upsetting the borrow checker?
        macro_rules! pop_stacks {
            () => {
                let rhs = expr_stack.pop().unwrap();
                let lhs = expr_stack.pop().unwrap();
                let next_op = op_stack.pop().unwrap();
                let range = source_info::concat(
                    self.get_range(lhs),
                    self.get_range(rhs),
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
            expr_stack.push(self.try_parse_term(p, parse_struct_lits)?);
        }
        while !op_stack.is_empty() { pop_stacks!(); }

        Ok(expr_stack.pop().unwrap())
    }

    fn parse_non_struct_lit_expr(&mut self, p: &mut Parser) -> ExprId {
        self.try_parse_expr(p, false)
            .unwrap_or_else(|tok| panic!("UNHANDLED TERM: {:#?}", tok))
    }
    // Difference between try_parse_expr and parse_expr:
    // It is possible for try_parse_expr to return Err, even in a perfectly legal program.
    // If parse_expr() return Err, it always signals an invalid program.
    fn parse_expr(&mut self, p: &mut Parser) -> Result<ExprId, ExprId> {
        match self.try_parse_expr(p, true) {
            Ok(expr) => Ok(expr),
            Err(_token) => {
                self.diag.push(
                    Error::new("unrecognized term")
                        .adding_primary_range(self.cur(p).range, "term here")
                );
                // try_parse_expr() does not advance on failure. So move to the next token in order to avoid an infinite loop
                let range = self.cur(p).range;
                self.next(p);
                Err(self.error_expr(range))
            }
        }
    }

    fn parse_if(&mut self, p: &mut Parser) -> ParseResult<ExprId> {
        let if_range = self.eat_tok(p, TokenKind::If)?;
        let condition = self.parse_non_struct_lit_expr(p);
        let (then_scope, then_range) = self.parse_scope(p, &[])?;
        let mut range = source_info::concat(if_range, then_range);
        let else_scope = match self.cur(p).kind {
            TokenKind::Else => match self.next(p).kind {
                TokenKind::If => {
                    let scope = self.begin_imper_scope();
                    let scope_id = scope.id();
                    let if_expr = self.parse_if(p)?;
                    let if_range = self.get_range(if_expr);
                    range = source_info::concat(range, if_range);
                    self.stmt(if_expr, false);
                    self.end_imper_scope(scope, true);
                    Some(scope_id)
                },
                TokenKind::OpenCurly => {
                    let (else_scope, else_range) = self.parse_scope(p, &[])?;
                    range = source_info::concat(range, else_range);
                    Some(else_scope)
                },
                _ => panic!("{}", "Expected '{' or 'if' after 'else'"),
            },
            _ => None,
        };
        Ok(self.if_expr(condition, then_scope, else_scope, range))
    }

    fn parse_switch(&mut self, p: &mut Parser) -> ParseResult<ExprId> {
        let switch_range = self.eat_tok(p, TokenKind::Switch)?;
        let scrutinee = self.parse_non_struct_lit_expr(p);
        self.eat_tok(p, TokenKind::OpenCurly)?;
        let case_list = self.begin_list(p, TokenKind::could_begin_pattern, [TokenKind::Comma, TokenKind::Semicolon], Some(TokenKind::CloseCurly));
        let mut cases = Vec::new();
        let close_curly_range = loop {
            match self.cur(p).kind {
                TokenKind::Eof => {
                    self.diag.report_error("Unexpected end of file while parsing `switch` body", switch_range, "`switch` started here");
                    return Err(ParseError::Eof);
                },
                TokenKind::CloseCurly => {
                    let close_curly_range = self.cur(p).range;
                    self.next(p);
                    break close_curly_range;
                },
                _ => {
                    self.start_next_list_item(p, case_list.id());
                    let pattern_kind = self.parse_pattern(p);
                    let (bindings, binding_ids) = self.get_pattern_bindings(&pattern_kind, scrutinee);
                    let pattern = Pattern { kind: pattern_kind, bindings: binding_ids };
                    self.eat_tok(p, TokenKind::Colon)?;
                    let (scope, scope_range) = match self.cur(p).kind {
                        TokenKind::OpenCurly => {
                            let scope = self.parse_scope(p, &bindings)?;
                            self.eat_separators(p);
                            scope
                        },
                        _ => {
                            let scope = self.begin_imper_scope();
                            let scope_id = scope.id();
                            let case_expr = self.parse_expr(p).unwrap_or_else(|err| err);
                            let separators = self.eat_separators(p);
                            let has_semicolon = matches!(separators, SeparatorResult::Explicit(TokenKind::Semicolon));
                            self.stmt(case_expr, has_semicolon);
                            self.end_imper_scope(scope, !has_semicolon);
                            let scope_range = self.get_range(case_expr);
                            (scope_id, scope_range)
                        }
                    };
                    let switch_case = SwitchCase {
                        pattern,
                        scope,
                        scope_range,
                    };
                    cases.push(switch_case);
                }
            }
        };

        let range = source_info::concat(switch_range, close_curly_range);
        Ok(self.switch_expr(scrutinee, cases, range))
    }

    fn parse_attribute(&mut self, p: &mut Parser, condition_ns: &mut Option<ConditionNsId>) -> ParseResult<Attribute> {
        let at_range = self.eat_tok(p, TokenKind::AtSign)?;

        let Token { kind, range: ident_range } = self.cur(p);
        let attr = match kind {
            &TokenKind::Ident(sym) => sym,
            _ => panic!("Unexpected token when parsing attribute"),
        };
        let is_requires = attr == self.hir.known_idents.requires;
        let is_guarantees = attr == self.hir.known_idents.guarantees;
        let is_comptime = attr == self.hir.known_idents.comptime;
        let is_condition = is_requires || is_guarantees;

        if !is_condition && !is_comptime {
            self.diag.push(
                Error::new(format!("unrecognized attribute '{}'", self.interner.resolve(attr).unwrap()))
                    .adding_primary_range(ident_range, "")
            );
        }
        let (arg, final_tok_range) = match self.next(p).kind {
            TokenKind::LeftParen => {
                let left_paren_range = self.cur(p).range;
                self.next(p);
                if matches!(self.cur(p).kind, TokenKind::RightParen) {
                    let paren_range = left_paren_range + self.cur(p).range;
                    self.diag.push(
                        Error::new("unexpected empty argument list on attribute")
                            .adding_primary_range(paren_range, "try removing these parentheses")
                    );
                    self.next(p);
                    (None, self.cur(p).range)
                } else {
                    // Enter condition namespace
                    let _condition_entry = is_condition.then(|| {
                        let condition_kind = if is_requires {
                            ConditionKind::Requirement
                        } else if is_guarantees {
                            ConditionKind::Guarantee
                        } else {
                            unreachable!();
                        };
                        // Lazily create condition namespace if necessary
                        let ns = if let &mut Some(condition_ns) = condition_ns {
                            condition_ns
                        } else {
                            let ns = self.create_condition_namespace();
                            *condition_ns = Some(ns);
                            ns
                        };
                        Some(self.enter_condition_namespace(ns, condition_kind))
                    });
                    let arg = self.parse_expr(p).unwrap_or_else(|err| err);
                    let paren_range = self.eat_tok(p, TokenKind::RightParen)?;
    
                    (Some(arg), paren_range)
                }
            },
            _ => (None, ident_range),
        };
        let range = source_info::concat(at_range, final_tok_range);

        if let &Some(arg) = &arg {
            if is_comptime {
                self.diag.push(
                    Error::new("argument passed to @comptime attribute")
                        .adding_primary_range(arg, "consider removing this expression and surrounding parentheses")
                );
            }
        }

        Ok(Attribute { attr, arg, range })
    }

    fn parse_pattern(&mut self, p: &mut Parser) -> PatternKind {
        let initial_tok = self.cur(p);
        let initial_range = initial_tok.range;
        match initial_tok.kind {
            TokenKind::Dot => {
                self.next(p);
                let name = self.eat_ident(p);
                let range = source_info::concat(initial_range, name.range);
                PatternKind::ContextualMember { name, range }
            },
            &TokenKind::Ident(name) => {
                self.next(p);
                if name == self.hir.known_idents.underscore {
                    PatternKind::AnonymousCatchAll(initial_range)
                } else {
                    let name = Ident { symbol: name, range: initial_range };
                    PatternKind::NamedCatchAll(name)
                }
            },
            &TokenKind::IntLit(value) => {
                self.next(p);
                PatternKind::IntLit { value, range: initial_range }
            },
            _ => panic!("unexpected token"),
        }   
    }

    fn eat_tok(&mut self, p: &mut Parser, kind: TokenKind) -> ParseResult<SourceRange> {
        let Token { kind: cur_kind, range } = self.cur(p);
        if cur_kind != &kind {
            let cur_kind = cur_kind.clone();
            self.diag.push(
                Error::new("unexpected token")
                    .adding_primary_range(range, format!("expected {:?} instead", kind)) // TODO: user-facing pretty-printing of token kinds
            );
            Err(ParseError::UnexpectedToken(cur_kind))
        } else {
            self.next(p);
            Ok(range)
        }
    }

    fn eat_ident(&mut self, p: &mut Parser) -> Ident {
        let Token { kind, range } = self.cur(p);
        if let &TokenKind::Ident(symbol) = kind {
            self.next(p);
            Ident { symbol, range }
        } else {
            panic!("unexpected token {:?}, expected identifier", kind)
        }
    }
}

#[derive(Debug)]
enum AmbiguousGenericListKind {
    Ambiguous(Vec<Ident>),
    Arguments(Vec<ExprId>),
}

/// This exists because when we see the following tokens:
/// ```dusk
///     ident[
/// ```
/// 
/// There's no way to tell if it is the start of a generic constant, like this:
/// ```dusk
///     Array[Element] :: struct { ... }
/// ```
/// 
/// Or the start of a generic decl ref with explicit arguments, like this:
/// ```dusk
///     Array[u8]
/// ```
/// 
/// Through the process of parsing the generic list, we may come across tokens
/// that make it clear it could only be one of the two possibilities, or we
/// may not. So, there needs to be a way to store elements of the generic list
/// without altering the HIR state until we know for sure what kind of list we're
/// dealing with. Once we do, we need to convert the "ambiguous" elements into the
/// proper format (either generic parameters or generic arguments).
#[derive(Debug)]
struct AmbiguousGenericList {
    kind: AmbiguousGenericListKind,
    /// Range from the opening square bracket to the closing one (or the last included token,
    /// if there was a parse error) 
    range: SourceRange,
}

impl Driver {
    fn convert_ambiguous_generic_list_to_arguments(&mut self, idents: &Vec<Ident>) -> Vec<ExprId> {
        let mut arguments = Vec::new();
        for ident in idents {
            let generic_ctx = self.begin_decl_ref_generic_ctx();
            let ty = self.decl_ref(None, ident.symbol, SmallVec::new(), false, ident.range, generic_ctx);
            arguments.push(ty);
        }
        arguments
    }

    fn convert_ambiguous_generic_list_to_params(&mut self, idents: &Vec<Ident>) -> GenericParamList {
        let mut generic_params = GenericParamList::default();
        generic_params.ids.start = self.hir.generic_params.peek_next_idx();
        for ident in idents {
            // Claim a GenericParamId for yourself, then set the `end` value to be one past the end
            let generic_param = self.hir.generic_params.next_idx();
            // Make sure nobody interrupts this loop and creates an unrelated generic param
            debug_assert_eq!(generic_params.ids.end, generic_param);
            generic_params.ids.end = generic_param + 1;

            generic_params.names.push(ident.symbol);
            generic_params.ranges.push(ident.range);
        }

        generic_params
    }

    fn parse_ambiguous_generic_list(&mut self, p: &mut Parser) -> ParseResult<AmbiguousGenericList> {
        let open_square_bracket_range = self.eat_tok(p, TokenKind::OpenSquareBracket)?;
        let mut list = AmbiguousGenericList {
            kind: AmbiguousGenericListKind::Ambiguous(Vec::new()),
            range: open_square_bracket_range
        };

        loop {
            match &mut list.kind {
                AmbiguousGenericListKind::Ambiguous(idents) => {
                    if let &TokenKind::Ident(symbol) = self.cur(p).kind {
                        if matches!(self.peek_next(p).kind, TokenKind::Comma) || matches!(self.peek_next(p).kind, TokenKind::CloseSquareBracket) {
                            // Still ambiguous; just an identifier
                            let range = self.cur(p).range;
                            idents.push(Ident { symbol, range  });
                            list.range = source_info::concat(list.range, range);
                            self.next(p);
                            while let TokenKind::Comma = self.cur(p).kind {
                                list.range = source_info::concat(list.range, self.cur(p).range);
                                self.next(p);
                            }
                            if let TokenKind::CloseSquareBracket = self.cur(p).kind {
                                break;
                            }
                        } else {
                            // Convert all previous items into expressions and let the next iteration loop handle the current item
                            let args = self.convert_ambiguous_generic_list_to_arguments(idents);
                            list.kind = AmbiguousGenericListKind::Arguments(args);
                        }
                    } else {
                        // Convert all previous items into expressions and let the next iteration loop handle the current item
                        let args = self.convert_ambiguous_generic_list_to_arguments(idents);
                        list.kind = AmbiguousGenericListKind::Arguments(args);
                    }
                },
                AmbiguousGenericListKind::Arguments(args) => {
                    let arg = self.parse_expr(p).unwrap_or_else(|err| err);
                    args.push(arg);
                    while let TokenKind::Comma = self.cur(p).kind {
                        list.range = source_info::concat(list.range, self.cur(p).range);
                        self.next(p);
                    }
                    if let TokenKind::CloseSquareBracket = self.cur(p).kind {
                        break;
                    }
                }
            }
        }

        let bracket_range = self.eat_tok(p, TokenKind::CloseSquareBracket)?;
        list.range = source_info::concat(list.range, bracket_range);

        Ok(list)
    }

    /// Parses any item. Used at the top-level, in modules, and within computed declaration scopes.
    fn parse_item(&mut self, p: &mut Parser) -> ParseResult<Item> {
        match self.cur(p).kind {
            &TokenKind::Ident(name) => {
                let name = Ident { symbol: name, range: self.cur(p).range };
                if self.next_tok_has_implicit_separator(p) {
                    let expr = self.parse_expr(p).unwrap_or_else(|err| err);
                    Ok(Item::Expr(expr))
                } else if let TokenKind::OpenSquareBracket = self.peek_next(p).kind {
                    self.next(p);
                    let list = self.parse_ambiguous_generic_list(p)?;
                    if self.next_tok_has_implicit_separator(p) {
                        // THIS TODO IS DUPLICATED BELOW
                        todo!("explicit generic arguments at the top-level scope are not yet supported");
                    } else if let TokenKind::Colon = self.peek_next(p).kind {
                        let params = match list.kind {
                            AmbiguousGenericListKind::Ambiguous(idents) =>
                                self.convert_ambiguous_generic_list_to_params(&idents),
                            AmbiguousGenericListKind::Arguments(_args) => {
                                self.diag.push(
                                    Error::new("invalid syntax in generic parameter list")
                                        .adding_primary_range(list.range, "expected comma-separated list of identifiers")
                                );
                                GenericParamList::default()
                            }
                        };
                        let decl = self.parse_decl(name, params, p)?;
                        Ok(Item::Decl(decl))
                    } else {
                        // THIS TODO IS DUPLICATED ABOVE
                        todo!("explicit generic arguments at the top-level scope are not yet supported");
                    }
                } else if let TokenKind::Colon = self.peek_next(p).kind {
                    self.next(p);
                    let decl = self.parse_decl(name, GenericParamList::default(), p)?;
                    Ok(Item::Decl(decl))
                } else {
                    let expr = self.parse_expr(p).unwrap_or_else(|err| err);
                    Ok(Item::Expr(expr))
                }
            },
            TokenKind::Fn => {
                let decl = self.parse_comp_decl(p)?;
                Ok(Item::Decl(decl))
            },
            TokenKind::AtSign => {
                let mut attributes = Vec::new();

                let mut condition_ns = None;
                let decl = loop {
                    let attr = self.parse_attribute(p, &mut condition_ns)?;
                    attributes.push(attr);
                    if self.cur(p).kind != &TokenKind::AtSign {
                        match self.parse_item(p)? {
                            Item::Decl(decl) => break decl,
                            Item::Expr(_) => panic!("Attributes on expressions are unsupported!"),
                        }
                    }
                };
                if let Some(condition_ns) = condition_ns {
                    self.code.hir.condition_ns[condition_ns].func = decl;
                }
                if let Some(attr) = attributes.iter().find(|attr| attr.attr == self.hir.known_idents.comptime) {
                    if !matches!(df!(decl.hir), hir::Decl::Computed { .. }) {
                        self.diag.push(
                            Error::new("unexpected @comptime attribute")
                                .adding_primary_range(attr.range, "can only be applied to function declarations")
                        );
                    }
                }
                self.code.hir.decl_attributes.entry(decl).or_default()
                    .extend(attributes);
                Ok(Item::Decl(decl))
            },
            _ => {
                let expr = self.parse_expr(p).unwrap_or_else(|err| err);
                Ok(Item::Expr(expr))
            },
        }
    }

    fn parse_decl(&mut self, name: Ident, generic_param_list: GenericParamList, p: &mut Parser) -> ParseResult<DeclId> {
        let colon_range = self.eat_tok(p, TokenKind::Colon)?;
        let mut found_separator = true;
        let explicit_ty = match self.cur(p).kind {
            TokenKind::Ident(_) => Some(self.parse_type(p).0),
            _ => None,
        };
        
        let is_mut = match self.cur(p).kind {
            TokenKind::Assign => true,
            TokenKind::Colon => false,
            _ => {
                self.diag.push(
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

        let generic_params = self.create_decls_for_generic_param_list(&generic_param_list);
        let ns = self.begin_generic_context(generic_params);
        let root = self.parse_expr(p).unwrap_or_else(|err| err);
        drop(ns);
        Ok(self.stored_decl(name.symbol, generic_param_list, explicit_ty, is_mut, root, name.range))
    }

    fn parse_module(&mut self, p: &mut Parser) -> ParseResult<ExprId> {
        let mod_range = self.eat_tok(p, TokenKind::Module)?;

        let (_mod_entry, module) = self.begin_module(None, mod_range);
        self.eat_tok(p, TokenKind::OpenCurly)?;
        let item_list = self.begin_list(p, TokenKind::could_begin_statement, [TokenKind::Semicolon], Some(TokenKind::CloseCurly));
        loop {
            match self.cur(p).kind {
                TokenKind::Eof => panic!("Unexpected eof while parsing scope"),
                TokenKind::CloseCurly => {
                    self.next(p);
                    break;
                },
                _ => {
                    self.start_next_list_item(p, item_list.id());
                    let item = self.parse_item(p)?;
                    if let Item::Expr(expr) = item {
                        self.diag.report_error("expressions are not allowed in the top-level of a module", expr, "delete this");
                    }
                    self.eat_separators(p);
                }
            }
        }
        Ok(module)
    }

    fn parse_extern_module(&mut self, p: &mut Parser) -> ParseResult<ExprId> {
        let mod_range = self.eat_tok(p, TokenKind::ExternModule)?;
        self.eat_tok(p, TokenKind::LeftParen)?;
        let library_path = self.parse_expr(p).unwrap_or_else(|err| err);
        self.eat_tok(p, TokenKind::RightParen)?;

        let extern_mod = self.code.hir.extern_mods.push(ExternMod::new(library_path));

        let (_module_entry, module) = self.begin_module(Some(extern_mod), mod_range);
        self.eat_tok(p, TokenKind::OpenCurly)?;
        let item_list = self.begin_list(p, TokenKind::could_begin_statement, [TokenKind::Semicolon], Some(TokenKind::CloseCurly));
        loop {
            match self.cur(p).kind {
                TokenKind::Eof => panic!("Unexpected eof while parsing scope"),
                TokenKind::CloseCurly => {
                    self.next(p);
                    break;
                },
                _ => {
                    self.start_next_list_item(p, item_list.id());
                    let item = self.parse_item(p)?;
                    if let Item::Expr(expr) = item {
                        self.diag.report_error("expressions are not allowed in the top-level of a module", expr, "delete this");
                    }
                    self.eat_separators(p);
                }
            }
        }
        Ok(module)
    }

    // From open curly to close curly
    fn parse_struct_body(&mut self, p: &mut Parser, additional_range: Option<SourceRange>) -> ParseResult<ExprId> {
        let open_curly_range = self.eat_tok(p, TokenKind::OpenCurly)?;

        let mut fields = Vec::new();
        let (expr, strukt) = self.reserve_struct();
        let mut used_names = HashMap::new();
        let field_list = self.begin_list(p, TokenKind::could_begin_struct_field, [TokenKind::Comma], Some(TokenKind::CloseCurly));
        let close_curly_range = loop {
            match self.cur(p).kind {
                TokenKind::Eof => panic!("Unexpected eof while parsing struct expression"),
                TokenKind::CloseCurly => {
                    let close_curly_range = self.cur(p).range;
                    self.next(p);
                    break close_curly_range;
                },
                _ => {
                    self.start_next_list_item(p, field_list.id());
                    if let Token { kind: &TokenKind::Ident(name), range: ident_range } = self.cur(p) {
                        self.next(p);
                        self.eat_tok(p, TokenKind::Colon)?;
                        let (ty, ty_range) = self.parse_type(p);
                        let index = fields.len();
                        let range = source_info::concat(ident_range, ty_range);
                        fields.push(self.field_decl(name, strukt, ty, index, range));
                        if let Some(first_range) = used_names.get(&name).copied() {
                            let name_str = self.interner.resolve(name).unwrap();
                            self.diag.report_error_no_range_msg(format!("field with name '{}' already exists", name_str), ident_range)
                                .adding_secondary_range_with_msg(first_range, "first field with that name here");
                        } else {
                            used_names.insert(name, ident_range);
                        }
                        self.eat_separators(p);
                    } else {
                        let range = self.cur(p).range;
                        self.diag.report_error("unexpected token", range, "expected field name");
                        self.next(p);
                    }
                }
            }
        };
        let mut range = source_info::concat(open_curly_range, close_curly_range);
        if let Some(additional_range) = additional_range {
            range = source_info::concat(range, additional_range);
        }
        self.finish_struct(fields, range, expr, strukt);

        Ok(expr)
    }

    fn parse_struct(&mut self, p: &mut Parser) -> ParseResult<ExprId> {
        let struct_range = self.eat_tok(p, TokenKind::Struct)?;
        self.parse_struct_body(p, Some(struct_range))
    }

    fn parse_enum(&mut self, p: &mut Parser) -> ParseResult<ExprId> {
        let enum_range = self.eat_tok(p, TokenKind::Enum)?;
        self.eat_tok(p, TokenKind::OpenCurly)?;

        let (expr, enuum) = self.reserve_enum();

        let mut variants = Vec::new();
        let variant_list = self.begin_list(p, TokenKind::could_begin_variant_decl, [TokenKind::Comma], Some(TokenKind::CloseCurly));
        let close_curly_range = loop {
            match self.cur(p).kind {
                TokenKind::Eof => panic!("Unexpected eof while parsing struct expression"),
                TokenKind::CloseCurly => {
                    let close_curly_range = self.cur(p).range;
                    self.next(p);
                    break close_curly_range;
                },
                _ => {
                    self.start_next_list_item(p, variant_list.id());
                    if let Token { kind: &TokenKind::Ident(name), range: ident_range } = self.cur(p) {
                        self.next(p);
                        let index = variants.len();
                        let payload_ty = match self.cur(p).kind {
                            TokenKind::LeftParen => {
                                self.next(p);
                                let (payload_ty, _) = self.parse_type(p);
                                self.eat_tok(p, TokenKind::RightParen)?;
                                Some(payload_ty)
                            },
                            TokenKind::OpenCurly => {
                                Some(self.parse_struct_body(p, None)?)
                            }
                            _ => None,
                        };
                        variants.push(self.variant_decl(name, expr, enuum, index, payload_ty, ident_range));
                        self.eat_separators(p);
                    } else {
                        let range = self.cur(p).range;
                        self.diag.report_error("unexpected token", range, "expected variant name");
                        self.next(p);
                    }
                }
            }
        };
        self.finish_enum(variants, source_info::concat(enum_range, close_curly_range), expr, enuum);

        // Add intrinsics for comparison of enums
        let boool = self.add_const_ty(Type::Bool);
        self.add_intrinsic(Intrinsic::Eq, smallvec![expr, expr], boool, true);
        self.add_intrinsic(Intrinsic::NotEq, smallvec![expr, expr], boool, true);
        Ok(expr)
    }

    // Parses an open curly brace, then a list of Items, then a closing curly brace.
    fn parse_scope(&mut self, p: &mut Parser, additional_imper_decls: &[ImperScopedDecl]) -> ParseResult<(ImperScopeId, SourceRange)> {
        let scope = self.begin_imper_scope();
        let scope_id = scope.id();
        for &decl in additional_imper_decls {
            self.imper_scoped_decl(decl);
        }
        let mut last_was_expr = false;
        let open_curly_range = self.eat_tok(p, TokenKind::OpenCurly)?;
        let statement_list = self.begin_list(p, TokenKind::could_begin_statement, [TokenKind::Semicolon], Some(TokenKind::CloseCurly));
        let close_curly_range = loop {
            match self.cur(p).kind {
                TokenKind::Eof => {
                    self.diag.report_error("unclosed brace", open_curly_range, "opening brace was here");
                    return Err(ParseError::Eof)
                },
                TokenKind::CloseCurly => {
                    let close_curly_range = self.cur(p).range;
                    self.next(p);
                    break close_curly_range;
                },
                _ => {
                    self.start_next_list_item(p, statement_list.id());
                    let item = self.parse_item(p)?;

                    // If the item was a standalone expression, make it a statement
                    if let Item::Expr(expr) = item {
                        let separators = self.eat_separators(p);
                        let has_semicolon = matches!(separators, SeparatorResult::Explicit(TokenKind::Semicolon));
                        last_was_expr = !has_semicolon;
                        self.stmt(expr, has_semicolon);
                    } else {
                        last_was_expr = false;
                        self.eat_separators(p);
                    }
                }
            }
        };
        self.end_imper_scope(scope, last_was_expr);
        Ok((scope_id, source_info::concat(open_curly_range, close_curly_range)))
    }

    fn check_for_fn_equal(&mut self, p: &mut Parser) -> ParseResult<bool> {
        if matches!(self.cur(p).kind, TokenKind::Assign) {
            self.diag.push(
                Error::new("assigned functions are not yet supported")
                    .adding_primary_range(self.cur(p).range, "")
            );
            self.next(p);
            self.parse_expr(p).unwrap();
            Ok(true)
        } else {
            Ok(false)
        }
    }

    fn parse_comp_decl(&mut self, p: &mut Parser) -> ParseResult<DeclId> {
        // Parse fn {name}
        let mut proto_range = self.eat_tok(p, TokenKind::Fn)?;
        let name = if let TokenKind::Ident(name) = *self.cur(p).kind {
            name
        } else {
            self.diag.push(
                Error::new("expected function name after 'fn'")
                    .adding_primary_range(proto_range, "'fn' here")
                    .adding_secondary_range(self.cur(p).range, "note: found this instead")
            );
            return Err(ParseError::UnexpectedToken(self.cur(p).kind.clone()));
        };
        let name_range = self.cur(p).range;
        proto_range = source_info::concat(proto_range, name_range);

        // Parse optional [T, U, V, ...]
        let mut generic_param_list = GenericParamList::default();
        if let TokenKind::OpenSquareBracket = self.next(p).kind {
            let open_square_bracket_range = self.cur(p).range;
            self.next(p);
            let generic_param_syntax_list = self.begin_list(p, TokenKind::could_begin_generic_parameter, [TokenKind::Comma], Some(TokenKind::CloseSquareBracket));
            if matches!(self.cur(p).kind, TokenKind::Ident(_)) {
                generic_param_list.ids.start = self.hir.generic_params.peek_next_idx();
                generic_param_list.ids.end = generic_param_list.ids.start;
                while let TokenKind::Ident(name) = *self.cur(p).kind {
                    self.start_next_list_item(p, generic_param_syntax_list.id());
                    // Claim a GenericParamId for yourself, then set the `end` value to be one past the end
                    let generic_param = self.hir.generic_params.next_idx();
                    // Make sure nobody interrupts this loop and creates an unrelated generic param
                    debug_assert_eq!(generic_param_list.ids.end, generic_param);
                    generic_param_list.ids.end = generic_param + 1;

                    let param_range = self.cur(p).range;
                    generic_param_list.names.push(name);
                    self.next(p);
                    generic_param_list.ranges.push(param_range);
                    self.eat_separators(p);
                }
            } else {
                self.diag.report_error("expected at least one parameter in generic parameter list", open_square_bracket_range, "list starts here");
            }
            let bracket_range = self.eat_tok(p, TokenKind::CloseSquareBracket)?;
            proto_range = source_info::concat(proto_range, bracket_range);
        }

        let generic_params = self.create_decls_for_generic_param_list(&generic_param_list);
        let generic_ctx = self.begin_computed_decl_generic_ctx(generic_param_list.clone());

        // Parse (param_name: param_ty, param2_name: param2_ty, ...)
        let mut param_names = SmallVec::new();
        let mut param_tys = SmallVec::new();
        let mut param_ranges = SmallVec::new();
        let ns = self.begin_generic_context(generic_params.clone());
        if let TokenKind::LeftParen = self.cur(p).kind {
            self.next(p);
            let parameter_list = self.begin_list(p, TokenKind::could_begin_parameter, [TokenKind::Comma], Some(TokenKind::RightParen));
            while let TokenKind::Ident(name) = *self.cur(p).kind {
                self.start_next_list_item(p, parameter_list.id());
                let param_range = self.cur(p).range;
                param_names.push(name);
                self.next(p);
                self.eat_tok(p, TokenKind::Colon)?;
                let (ty, _ty_range) = self.parse_type(p);
                param_ranges.push(param_range);
                param_tys.push(ty);
                self.eat_separators(p);
            }
            let paren_range = self.eat_tok(p, TokenKind::RightParen)?;
            proto_range = source_info::concat(proto_range, paren_range);
        } else {
            self.diag.push(
                Error::new("function declaration must have parentheses")
                    .adding_primary_range(name_range, "")
                    .adding_secondary_range(
                        SourceRange::from_single_char(name_range.end),
                        "add '()' here"
                    )
            );
        }

        // Parse ": ty" or "{"
        let ty = match self.cur(p).kind {
            TokenKind::Colon => {
                self.next(p);
                let (ty, range) = self.parse_type(p);
                proto_range = source_info::concat(proto_range, range);
                ty
            },
            TokenKind::OpenCurly => hir::VOID_TYPE,
            _ => {
                self.check_for_fn_equal(p)?;
                assert_eq!(generic_param_list.names.len(), 0, "generic parameters on a function prototype are not allowed");
                drop(ns);
                return Ok(self.comp_decl_prototype(name, param_tys, param_ranges, hir::VOID_TYPE, proto_range));
            },
        };

        // TODO: end this later, after the ImperScope, so that generic parameters can be referred to from
        // inside the function as well.
        drop(ns);

        let decl_id = match self.cur(p).kind {
            TokenKind::OpenCurly => {
                let decl_id = self.begin_computed_decl(name, param_names, param_tys, param_ranges, generic_params, ty, proto_range);
                self.parse_scope(p, &[])?;
                self.end_computed_decl();
                decl_id
            },
            _ => {
                self.check_for_fn_equal(p)?;
                assert_eq!(generic_param_list.names.len(), 0, "generic parameters on a function prototype are not allowed");
                self.comp_decl_prototype(name, param_tys, param_ranges, ty, proto_range)
            }
        };
        drop(generic_ctx); // explicitly pop generic ctx id from the stack

        Ok(decl_id)
    }

    fn try_parse_type(&mut self, p: &mut Parser) -> Option<(ExprId, SourceRange)> {
        // This is a term and not an expression because assignments are valid expressions.
        //     For example: `foo: SomeType = ...` <- the parser would think you were assigning `...` to `SomeType` and
        //     taking the `void` result of that assignment as the type of variable declaration `foo`
        // TODO: add statements as a slight superset of expressions which includes assignments.
        self.try_parse_restricted_term(p)
            .ok()
            .map(|ty| (ty, self.get_range(ty)))
    }

    fn parse_type(&mut self, p: &mut Parser) -> (ExprId, SourceRange) {
        // TODO: report errors
        self.try_parse_type(p).unwrap()
    }

    fn cur(&self, p: &Parser) -> Token {
        self.toks[p.file].at(p.cur)
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
        self.toks[p.file].at(p.cur+1)
    }

    fn peek_prev_including_insignificant(&self, p: &Parser) -> Token {
        self.toks[p.file].at(p.cur - 1)
    }

    fn find_first_significant(&self, p: &Parser, token_range: impl Iterator<Item=usize>) -> Option<Token> {
        let toks = &self.toks[p.file];
        for i in token_range {
            let cur = toks.at(i);
            if cur.kind.is_significant() {
                return Some(cur)
            }
        }
        None
    }

    fn peek_prev(&self, p: &Parser) -> Token {
        self.find_first_significant(p, (0..p.cur).rev()).unwrap()
    }

    fn peek_next(&self, p: &Parser) -> Token {
        self.find_first_significant(p, (p.cur+1)..self.toks[p.file].len()).unwrap()
    }
}
