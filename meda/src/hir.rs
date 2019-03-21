use std::cmp::max;

pub type ExprID = usize;

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
pub enum ExprKind {
    IntLit,
    DecLit,
    BinOp { lhs: ExprID, rhs: ExprID },
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub id: ExprID,
}

#[derive(Debug)]
pub struct Program {
    /// List of expressions in the entire program, order
    pub expressions: Vec<Vec<Expr>>,
    pub num_expressions: usize,
}

pub struct Builder {
    expressions: Vec<Vec<Expr>>,
    levels: Vec<usize>,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            expressions: vec![Vec::new()],
            levels: Vec::new(),
        }
    }

    pub fn int_lit(&mut self, lit: u64) -> ExprID {
        let id = self.levels.len();
        self.levels.push(0);
        self.expressions.first_mut().unwrap()
            .push(Expr {
                kind: ExprKind::IntLit,
                id: id,
            });

        id
    }

    pub fn dec_lit(&mut self, lit: f64) -> ExprID {
        let id = self.levels.len();
        self.levels.push(0);
        self.expressions.first_mut().unwrap()
            .push(Expr {
                kind: ExprKind::DecLit,
                id: id,
            });

        id
    }

    pub fn bin_op(&mut self, op: BinOp, lhs: ExprID, rhs: ExprID) -> ExprID {
        let id = self.levels.len();
        let level = max(self.levels[lhs], self.levels[rhs]);
        self.levels.push(level);
        while(self.expressions.len() < level + 1) {
            self.expressions.push(Vec::new());
        }
        self.expressions.first_mut().unwrap()
            .push(Expr {
                kind: ExprKind::BinOp { lhs, rhs },
                id: id,
            });

        id
    }

    pub fn program(self) -> Program {
        Program {
            expressions: self.expressions,
            num_expressions: self.levels.len(),
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

