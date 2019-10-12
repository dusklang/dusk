use std::ffi::CString;

use bitflags::bitflags;
use smallvec::SmallVec;

use crate::index_vec::Idx;
use crate::source_info::SourceRange;
use crate::ty::Type;

newtype_index!(ExprId pub);
newtype_index!(DeclRefId pub);
newtype_index!(DeclId pub);
newtype_index!(ScopeId pub);
newtype_index!(CastId pub);

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
    /// Prefix
    Not, Deref, AddrOf, Neg, Plus, AddrOfMut,
    
    /// Postfix
    Pointer, PointerMut,
}

bitflags! {
    pub struct OpPlacement: u8 {
        const PREFIX  = 1 << 0;
        const POSTFIX = 1 << 1;
        const INFIX   = 1 << 2;
    }
}

impl BinOp {
    pub fn placement(self) -> OpPlacement {
        match self {
            BinOp::Mult | BinOp::Add | BinOp::Sub | BinOp::BitwiseAnd => OpPlacement::INFIX | OpPlacement::PREFIX,
            _ => OpPlacement::INFIX,
        }
    }
}

impl UnOp {
    pub fn placement(self) -> OpPlacement {
        match self {
            UnOp::Deref | UnOp::AddrOf | UnOp::Neg | UnOp::Plus => OpPlacement::PREFIX | OpPlacement::INFIX,
            UnOp::Not | UnOp::AddrOfMut => OpPlacement::PREFIX,
            UnOp::Pointer => OpPlacement::PREFIX | OpPlacement::INFIX | OpPlacement::POSTFIX,
            UnOp::PointerMut => OpPlacement::POSTFIX,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Intrinsic {
    Mult,
    Div,
    Mod,
    Add,
    Sub,
    Less,
    LessOrEq,
    Greater,
    GreaterOrEq,
    Eq,
    NotEq,
    BitwiseAnd,
    BitwiseOr,
    LogicalAnd,
    LogicalOr,
    LogicalNot,
    Neg,
    Pos,
    Panic,
    Print,
    Malloc,
    Free,

    // Named types
    I8,
    I16,
    I32,
    I64,
    Isize,
    U8,
    U16,
    U32,
    U64,
    Usize,
    F32,
    F64,
    Never,
    Bool,
    Void,
    Ty,
}

impl Intrinsic {
    pub fn name(&self) -> &str {
        use Intrinsic::*;
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
            LogicalNot => "!",
            Neg => "-",
            Pos => "+",
            Panic => "panic",
            Print => "print",
            Malloc => "malloc",
            Free => "free",
            I8 => "i8",
            I16 => "i16",
            I32 => "i32",
            I64 => "i64",
            Isize => "isize",
            U8 => "u8",
            U16 => "u16",
            U32 => "u32",
            U64 => "u64",
            Usize => "usize",
            F32 => "f32",
            F64 => "f64",
            Never => "never",
            Bool => "bool",
            Void => "void",
            Ty => "type",
        }
    }
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
            MultAssign => "*=",
            DivAssign => "/=",
            ModAssign => "%=",
            AddAssign => "+=",
            SubAssign => "-=",
            BitwiseAndAssign => "&=",
            BitwiseOrAssign => "|=",
            Assign => panic!("operator has no symbol"),
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
            Neg => "-",
            Plus => "+",
            Deref | AddrOf | AddrOfMut | Pointer | PointerMut => panic!("operator has no symbol")
        }
    }
}

pub trait Builder<'src> {
    type Output;
    fn void_expr(&self) -> ExprId;
    fn int_lit(&mut self, lit: u64, range: SourceRange) -> ExprId;
    fn dec_lit(&mut self, lit: f64, range: SourceRange) -> ExprId;
    fn str_lit(&mut self, lit: CString, range: SourceRange) -> ExprId;
    fn char_lit(&mut self, lit: i8, range: SourceRange) -> ExprId;
    fn bin_op(&mut self, op: BinOp, lhs: ExprId, rhs: ExprId, range: SourceRange) -> ExprId;
    fn cast(&mut self, expr: ExprId, ty: Type, range: SourceRange) -> ExprId;
    fn un_op(&mut self, op: UnOp, expr: ExprId, range: SourceRange) -> ExprId;
    fn stored_decl(&mut self, name: &'src str, explicit_ty: Option<Type>, is_mut: bool, root_expr: ExprId, range: SourceRange);
    fn ret(&mut self, expr: ExprId, range: SourceRange) -> ExprId;
    fn implicit_ret(&mut self, expr: ExprId);
    fn if_expr(&mut self, condition: ExprId, then_scope: ScopeId, else_scope: Option<ScopeId>, range: SourceRange) -> ExprId;
    fn while_expr(&mut self, condition: ExprId, scope: ScopeId, range: SourceRange) -> ExprId;
    fn stmt(&mut self, expr: ExprId);
    fn do_expr(&mut self, scope: ScopeId, range: SourceRange) -> ExprId;
    fn begin_scope(&mut self) -> ScopeId;
    fn end_scope(&mut self, has_terminal_expr: bool);
    fn begin_computed_decl(&mut self, name: &'src str, param_names: SmallVec<[&'src str; 2]>, param_tys: SmallVec<[Type; 2]>, ret_ty: Option<Type>, proto_range: SourceRange);
    fn end_computed_decl(&mut self);
    fn add_intrinsic(&mut self, intrinsic: Intrinsic, param_tys: SmallVec<[Type; 2]>, ret_ty: Type);
    fn decl_ref(&mut self, name: &'src str, arguments: SmallVec<[ExprId; 2]>, range: SourceRange) -> ExprId;
    fn get_range(&self, id: ExprId) -> SourceRange;
    fn set_range(&mut self, id: ExprId, range: SourceRange);
    fn get_terminal_expr(&self, scope: ScopeId) -> ExprId;
    fn enter_type_ctx(&mut self);
    fn exit_type_ctx(&mut self);
    fn HACK_convert_expr_to_type(&self, expr: ExprId) -> Type;
    fn output(self) -> Self::Output;
}
