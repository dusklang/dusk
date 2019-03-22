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
    BinOp { op: BinOp, lhs: ExprID, rhs: ExprID },
}

#[derive(Debug)]
pub struct Expr {
    pub kind: ExprKind,
    pub id: ExprID,
}

#[derive(Debug)]
pub struct Program {
    /// Lists of expressions in the entire program, ordered by typechecking dependency
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
        self.expressions.first_mut().unwrap().push(
            Expr {
                kind: ExprKind::IntLit,
                id: id,
            }
        );

        id
    }

    pub fn dec_lit(&mut self, lit: f64) -> ExprID {
        let id = self.levels.len();
        self.levels.push(0);
        self.expressions.first_mut().unwrap().push(
            Expr {
                kind: ExprKind::DecLit,
                id: id,
            }
        );

        id
    }

    pub fn bin_op(&mut self, op: BinOp, lhs: ExprID, rhs: ExprID) -> ExprID {
        let id = self.levels.len();
        let level = max(self.levels[lhs], self.levels[rhs]) + 1;
        println!("Level is {}", level);
        self.levels.push(level);
        while self.expressions.len() < level + 1 {
            self.expressions.push(Vec::new());
        }
        self.expressions[level].push(
            Expr {
                kind: ExprKind::BinOp { op, lhs, rhs },
                id: id,
            }
        );

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

