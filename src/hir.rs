use std::cmp::max;
use crate::dependent_vec::DependentVec;

pub type ItemId = usize;

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
    Not, Deref, Neg, Plus
}

#[derive(Debug)]
pub enum ItemKind {
    IntLit,
    DecLit,
    BinOp { op: BinOp, lhs: ItemId, rhs: ItemId },
}

/// An expression or declaration
#[derive(Debug)]
pub struct Item {
    pub kind: ItemKind,
    pub id: ItemId,
}

#[derive(Debug)]
pub struct Program {
    /// All the items in the entire program
    pub items: DependentVec<Item>,
    /// Number of items in the entire program
    pub num_items: usize,
    /// Number of operator expressions in the entire program
    pub num_operator_exprs: usize,
}

pub struct Builder {
    /// All the items in the entire program so far
    items: DependentVec<Item>,
    /// The levels of each item so far
    levels: Vec<u32>,
    /// Number of operator expressions so far
    num_operator_exprs: usize,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            items: DependentVec::new(),
            levels: Vec::new(),
            num_operator_exprs: 0,
        }
    }

    pub fn int_lit(&mut self, lit: u64) -> ItemId {
        let id = self.levels.len();
        let level = self.items.insert(
            &[],
            Item { 
                kind: ItemKind::IntLit,
                id,
            },
        );
        self.levels.push(level);
        id
    }

    pub fn dec_lit(&mut self, lit: f64) -> ItemId {
        let id = self.levels.len();
        let level = self.items.insert(
            &[],
            Item {
                kind: ItemKind::DecLit,
                id,
            },
        );
        self.levels.push(level);

        id
    }

    pub fn bin_op(&mut self, op: BinOp, lhs: ItemId, rhs: ItemId) -> ItemId {
        let id = self.levels.len();
        let level = self.items.insert(
            &[self.levels[lhs], self.levels[rhs]],
            Item {
                kind: ItemKind::BinOp { op, lhs, rhs },
                id,
            },
        );
        self.levels.push(level);
        self.num_operator_exprs += 1;

        id
    }

    pub fn program(self) -> Program {
        Program {
            items: self.items,
            num_items: self.levels.len(),
            num_operator_exprs: self.num_operator_exprs,
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
        }
    }
}

