use string_interner::{DefaultStringInterner, Sym};
use smallvec::SmallVec;

use crate::index_vec::Idx;
use crate::source_info::SourceRange;
use crate::ty::Type;

newtype_index!(ExprId pub);
newtype_index!(DeclRefId pub);
newtype_index!(GlobalDeclId pub);
newtype_index!(LocalDeclId pub);
newtype_index!(ScopeId pub);

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum DeclId {
    Global(GlobalDeclId),
    Local(LocalDeclId),
}

#[derive(Clone, Copy, Debug)]
pub enum BinOp {
    Mult, Div, Mod,
    Add, Sub,
    Less, LessOrEq, Greater, GreaterOrEq,
    Eq, NotEq,
    BitwiseAnd, BitwiseOr,
    LogicalAnd, LogicalOr,
    Assign,
    MultAssign, DivAssign, ModAssign,
    AddAssign, SubAssign,
    BitwiseAndAssign, BitwiseOrAssign,
}

#[derive(Clone, Copy, Debug)]
pub enum UnOp {
    Not, Deref, AddrOf, Neg, Plus, AddrOfMut
}

impl BinOp {
    pub fn symbol(self) -> &'static str {
        use BinOp::*;
        match self {
            Mult => "*",
            Div => "/",
            Mod => "%",
            Add => "+",
            Sub => "-",
            Less => "<",
            LessOrEq => "<=",
            Greater => ">",
            GreaterOrEq => ">=",
            Eq => "==",
            NotEq => "!=",
            BitwiseAnd => "&",
            BitwiseOr => "|",
            LogicalAnd => "&&",
            LogicalOr => "||",
            Assign => "=",
            MultAssign => "*=",
            DivAssign => "/=",
            ModAssign => "%=",
            AddAssign => "+=",
            SubAssign => "-=",
            BitwiseAndAssign => "&=",
            BitwiseOrAssign => "|=",
        }
    }

    pub fn precedence(self) -> u8 {
        use BinOp::*;
        match self {
            Mult | Div | Mod => 0,
            Add | Sub => 1,
            Less | LessOrEq | Greater | GreaterOrEq => 2,
            Eq | NotEq => 3,
            BitwiseAnd | BitwiseOr => 4,
            LogicalAnd | LogicalOr => 5,
            Assign | AddAssign | SubAssign | MultAssign |
                DivAssign | ModAssign | BitwiseAndAssign | 
                BitwiseOrAssign => 6
        }
    }
}

impl UnOp {
    pub fn symbol(self) -> &'static str {
        use UnOp::*;
        match self {
            Not => "!",
            Deref => "*",
            Neg => "-",
            Plus => "+",
            AddrOf => "&",
            AddrOfMut => "&mut",
        }
    }
}

pub trait Builder<'a> {
    type Output;
    fn new(interner: &'a mut DefaultStringInterner) -> Self;
    fn interner(&self) -> &DefaultStringInterner;
    fn void_expr(&self) -> ExprId;
    fn int_lit(&mut self, lit: u64, range: SourceRange) -> ExprId;
    fn dec_lit(&mut self, lit: f64, range: SourceRange) -> ExprId;
    fn bin_op(&mut self, op: BinOp, lhs: ExprId, rhs: ExprId, range: SourceRange) -> ExprId;
    fn un_op(&mut self, op: UnOp, expr: ExprId, range: SourceRange) -> ExprId;
    fn stored_decl(&mut self, name: Sym, is_mut: bool, root_expr: ExprId, range: SourceRange);
    fn ret(&mut self, expr: ExprId, range: SourceRange) -> ExprId;
    fn if_expr(&mut self, condition: ExprId, then_scope: ScopeId, else_scope: Option<ScopeId>, range: SourceRange) -> ExprId;
    fn stmt(&mut self, expr: ExprId);
    fn do_expr(&mut self, scope: ScopeId) -> ExprId;
    fn begin_scope(&mut self) -> ScopeId;
    fn end_scope(&mut self, has_terminal_expr: bool);
    fn begin_computed_decl(&mut self, name: Sym, param_names: SmallVec<[Sym; 2]>, param_tys: SmallVec<[Type; 2]>, ret_ty: Type, proto_range: SourceRange);
    fn end_computed_decl(&mut self);
    fn decl_ref(&mut self, name: Sym, arguments: SmallVec<[ExprId; 2]>, range: SourceRange) -> ExprId;
    fn get_range(&self, id: ExprId) -> SourceRange;
    fn get_terminal_expr(&self, scope: ScopeId) -> ExprId;
    fn output(self) -> Self::Output;
}
