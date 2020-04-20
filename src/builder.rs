// TODO: Rename this file, or move its contents elsewhere. Used to contain the builder trait, which no longer exists.

use bitflags::bitflags;

use crate::index_vec::Idx;

newtype_index!(ItemId pub);
newtype_index!(ExprId pub);
newtype_index!(DeclRefId pub);
newtype_index!(DeclId pub);
newtype_index!(ScopeId pub);
newtype_index!(CastId pub);
newtype_index!(ModId pub);

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
    PrintType,
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
            PrintType => "print_type",
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