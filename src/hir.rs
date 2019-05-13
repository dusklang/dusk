use crate::dep_vec::DepVec;
use crate::source_info::SourceRange;
use crate::index_vec::{Idx, IdxVec};
use crate::ty::Type;

use arrayvec::ArrayVec;

newtype_index!(ItemId);
newtype_index!(OpId);
newtype_index!(DeclId);

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
    BinOp { op: BinOp, lhs: ItemId, rhs: ItemId, op_id: OpId },
    StoredDecl { name: String, root_expr: ItemId },
    DeclRef { decl: Option<ItemId> }
}

/// An expression or declaration
#[derive(Debug)]
pub struct Item {
    pub kind: ItemKind,
    pub id: ItemId,
}

#[derive(Debug)]
pub struct ComputedDecl {
    pub name: String,
    pub param_tys: Vec<Type>,
    pub ret_ty: Type,
}

impl ComputedDecl {
    fn new(name: String, param_tys: Vec<Type>, ret_ty: Type) -> ComputedDecl {
        ComputedDecl { name, param_tys, ret_ty }
    }
}

#[derive(Debug)]
pub struct Program {
    /// All items in the entire program
    pub items: DepVec<Item>,
    /// The source ranges of each item in the entire program
    pub source_ranges: IdxVec<SourceRange, ItemId>,
    /// The global declarations in the entire program
    pub decls: IdxVec<ComputedDecl, DeclId>,
    /// Number of items in the entire program
    pub num_items: usize,
    /// Number of operator expressions in the entire program
    pub num_operator_exprs: usize,
}

struct StoredDecl {
    name: String,
    decl: ItemId,
}

pub struct Builder {
    /// All items in the entire program so far
    items: DepVec<Item>,
    /// The source ranges of each item so far
    source_ranges: IdxVec<SourceRange, ItemId>,
    /// The levels of each item so far
    levels: IdxVec<u32, ItemId>,
    /// The global declarations so far
    decls: IdxVec<ComputedDecl, DeclId>,
    /// Number of operator expressions so far
    num_operator_exprs: usize,

    /// All stored declarations in the current scope (not indexed like the other collections; just for convenient lookup of declarations)
    stored_decls: Vec<StoredDecl>,
}

impl Builder {
    pub fn new() -> Self {
        let mut decls: IdxVec<ComputedDecl, DeclId> = IdxVec::new();
        // Integers, floats and bool
        let values = &[
            Type::i8(), Type::i16(), Type::i32(), Type::i64(),
            Type::u8(), Type::u16(), Type::u32(), Type::u64(),
            Type::f32(), Type::f64(), Type::Bool
        ];
        let numerics = &values[0..10];
        let integers = &numerics[0..8];
        for op in &["*", "/", "%", "+", "-"] {
            for ty in numerics {
                decls.push(ComputedDecl::new(op.to_string(), vec![ty.clone(), ty.clone()], ty.clone()));
            }
        }
        for op in &["<", "<=", ">", ">="] {
            for ty in numerics {
                decls.push(ComputedDecl::new(op.to_string(), vec![ty.clone(), ty.clone()], Type::Bool));
            }
        }
        for op in &["==", "!="] {
            for ty in values {
                decls.push(ComputedDecl::new(op.to_string(), vec![ty.clone(), ty.clone()], Type::Bool));
            }
        }
        for op in &["&", "|"] {
            for ty in integers {
                decls.push(ComputedDecl::new(op.to_string(), vec![ty.clone(), ty.clone()], ty.clone()));
            }
        }
        for op in &["&=", "|="] {
            for ty in integers {
                decls.push(ComputedDecl::new(op.to_string(), vec![ty.clone(), ty.clone()], Type::Void));
            }
        }
        for op in &["&&", "||"] {
            decls.push(ComputedDecl::new(op.to_string(), vec![Type::Bool, Type::Bool], Type::Bool));
        }
        for ty in values {
            decls.push(ComputedDecl::new("=".to_string(), vec![ty.clone(), ty.clone()], Type::Void));
        }
        Self {
            items: DepVec::new(),
            source_ranges: IdxVec::new(),
            levels: IdxVec::new(),
            decls,
            num_operator_exprs: 0,
            stored_decls: Vec::new(),
        }
    }

    pub fn int_lit(&mut self, lit: u64, range: SourceRange) -> ItemId {
        let id = ItemId::new(self.levels.len());
        let level = self.items.insert(
            &[],
            Item { 
                kind: ItemKind::IntLit,
                id,
            },
        );
        self.levels.push(level);
        self.source_ranges.push(range);
        id
    }

    pub fn dec_lit(&mut self, lit: f64, range: SourceRange) -> ItemId {
        let id = ItemId::new(self.levels.len());
        let level = self.items.insert(
            &[],
            Item {
                kind: ItemKind::DecLit,
                id,
            },
        );
        self.levels.push(level);
        self.source_ranges.push(range);

        id
    }

    pub fn bin_op(&mut self, op: BinOp, lhs: ItemId, rhs: ItemId, range: SourceRange) -> ItemId {
        let id = ItemId::new(self.levels.len());
        let level = self.items.insert(
            &[self.levels[lhs], self.levels[rhs]],
            Item {
                kind: ItemKind::BinOp { op, lhs, rhs, op_id: OpId::new(self.num_operator_exprs as usize) },
                id,
            },
        );
        self.levels.push(level);
        self.source_ranges.push(range);
        self.num_operator_exprs += 1;

        id
    }

    pub fn stored_decl(&mut self, name: String, root_expr: ItemId, range: SourceRange) -> ItemId {
        let id = ItemId::new(self.levels.len());
        let level = self.items.insert(
            &[self.levels[root_expr]],
            Item {
                kind: ItemKind::StoredDecl { name: name.clone(), root_expr },
                id,
            },
        );
        self.levels.push(level);
        self.source_ranges.push(range);

        self.stored_decls.push(StoredDecl { name, decl: id });
        
        id
    }

    pub fn decl_ref(&mut self, name: String, range: SourceRange) -> ItemId {
        let id = ItemId::new(self.levels.len());

        let mut decl: Option<ItemId> = None;
        for &StoredDecl { name: ref other_name, decl: other_decl } in self.stored_decls.iter().rev() {
            if &name == other_name {
                decl = Some(other_decl);
                break;
            }
        }

        let mut deps = ArrayVec::<[u32; 1]>::new();
        if let Some(decl) = decl { deps.push(self.levels[decl]); }
        let level = self.items.insert(
            &deps[..],
            Item {
                kind: ItemKind::DeclRef { decl },
                id,
            },
        );
        self.levels.push(level);
        self.source_ranges.push(range);

        id
    }

    pub fn get_range(&self, id: ItemId) -> SourceRange {
        self.source_ranges[id].clone()
    }

    pub fn program(self) -> Program {
        Program {
            items: self.items,
            source_ranges: self.source_ranges,
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

