// TODO: Rename this file, or move its contents elsewhere. Used to contain the builder trait, which no longer exists.

use bitflags::bitflags;

#[derive(Clone, Copy, Debug)]
pub enum BinOp {
    Mult, Div, Mod,
    Add, Sub,
    Less, LessOrEq, Greater, GreaterOrEq,
    Eq, NotEq,
    BitwiseAnd, BitwiseOr, BitwiseXor,
    LogicalAnd, LogicalOr,
    Assign,
    MultAssign, DivAssign, ModAssign,
    AddAssign, SubAssign,
    BitwiseAndAssign, BitwiseOrAssign,
    LeftShiftAssign, RightShiftAssign,
    XorAssign,
    LeftShift, RightShift,
}

#[derive(Clone, Copy, Debug)]
pub enum UnOp {
    /// Prefix
    Not, BitwiseNot, Deref, AddrOf, Neg, Plus, AddrOfMut,
    
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
            UnOp::Not | UnOp::BitwiseNot | UnOp::AddrOfMut => OpPlacement::PREFIX,
            UnOp::Pointer => OpPlacement::PREFIX | OpPlacement::INFIX | OpPlacement::POSTFIX,
            UnOp::PointerMut => OpPlacement::POSTFIX,
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
            XorAssign => "^=",
            LeftShiftAssign => "<<=",
            RightShiftAssign => ">>=",
            BitwiseXor => "^",
            LeftShift => "<<",
            RightShift => ">>",
            Assign => panic!("operator has no symbol"),
        }
    }

    pub fn precedence(self) -> u8 {
        use BinOp::*;
        match self {
            Mult | Div | Mod | LeftShift | RightShift => 0,
            Add | Sub => 1,
            Less | LessOrEq | Greater | GreaterOrEq => 2,
            Eq | NotEq => 3,
            BitwiseAnd | BitwiseOr | BitwiseXor => 4,
            LogicalAnd | LogicalOr => 5,
            Assign | AddAssign | SubAssign | MultAssign |
                DivAssign | ModAssign | BitwiseAndAssign | 
                BitwiseOrAssign | XorAssign | LeftShiftAssign | RightShiftAssign => 6,
        }
    }
}

impl UnOp {
    pub fn symbol(self) -> &'static str {
        use UnOp::*;
        match self {
            BitwiseNot => "~",
            Not => "!",
            Neg => "-",
            Plus => "+",
            Deref | AddrOf | AddrOfMut | Pointer | PointerMut => panic!("operator has no symbol")
        }
    }
}