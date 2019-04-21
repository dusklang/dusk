use std::cmp::max;

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
    /// Lists of items in the entire program, ordered by typechecking dependency
    pub items: Vec<Vec<Item>>,
    /// Number of items in the entire program
    pub num_items: usize,
    /// Number of operator expressions in the entire program
    pub num_operator_exprs: usize,
}

pub struct Builder {
    /// Lists of items in the entire program so far, ordered by typechecking dependency
    items: Vec<Vec<Item>>,
    /// The levels of each item so far
    levels: Vec<usize>,
    /// Number of operator expressions so far
    num_operator_exprs: usize,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            items: vec![Vec::new()],
            levels: Vec::new(),
            num_operator_exprs: 0,
        }
    }

    pub fn int_lit(&mut self, lit: u64) -> ItemId {
        let id = self.levels.len();
        self.levels.push(0);
        self.items.first_mut().unwrap().push(
            Item {
                kind: ItemKind::IntLit,
                id,
            }
        );

        id
    }

    pub fn dec_lit(&mut self, lit: f64) -> ItemId {
        let id = self.levels.len();
        self.levels.push(0);
        self.items.first_mut().unwrap().push(
            Item {
                kind: ItemKind::DecLit,
                id,
            }
        );

        id
    }

    pub fn bin_op(&mut self, op: BinOp, lhs: ItemId, rhs: ItemId) -> ItemId {
        let id = self.levels.len();
        let level = max(self.levels[lhs], self.levels[rhs]) + 1;
        self.levels.push(level);
        while self.items.len() < level + 1 {
            self.items.push(Vec::new());
        }
        self.items[level].push(
            Item {
                kind: ItemKind::BinOp { op, lhs, rhs },
                id,
            }
        );
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

