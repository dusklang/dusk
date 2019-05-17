use crate::dep_vec::DepVec;
use crate::source_info::SourceRange;
use crate::index_vec::{Idx, IdxVec};
use crate::ty::Type;

use arrayvec::ArrayVec;

newtype_index!(ItemId);
newtype_index!(DeclRefId);
newtype_index!(GlobalDeclId);
newtype_index!(LocalDeclId);

#[derive(Copy, Clone, Debug)]
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
    Not, Deref, Neg, Plus
}

#[derive(Debug)]
pub enum ItemKind {
    IntLit,
    DecLit,
    StoredDecl { root_expr: ItemId, id: DeclId },
    DeclRef { args: Vec<ItemId>, id: DeclRefId },
}

/// An expression or declaration
#[derive(Debug)]
pub struct Item {
    pub kind: ItemKind,
    pub id: ItemId,
}

#[derive(Debug)]
pub struct Decl {
    pub name: String,
    pub param_tys: Vec<Type>,
    pub ret_ty: Type,
}

impl Decl {
    fn new(name: String, param_tys: Vec<Type>, ret_ty: Type) -> Self {
        Decl { name, param_tys, ret_ty }
    }
}

#[derive(Debug)]
pub struct Program {
    /// All items in the entire program
    pub items: DepVec<Item>,
    /// The source ranges of each item in the entire program
    pub source_ranges: IdxVec<SourceRange, ItemId>,
    /// The global declarations in the entire program
    pub global_decls: IdxVec<Decl, GlobalDeclId>,
    /// The local declarations in the entire program
    pub local_decls: IdxVec<Decl, LocalDeclId>,
    /// Each declref's overload choices
    pub overloads: IdxVec<Vec<DeclId>, DeclRefId>,
    /// Number of items in the entire program
    pub num_items: usize,
}

impl Program {
    pub fn decl(&self, id: DeclId) -> &Decl { 
        match id {
            DeclId::Global(id) => &self.global_decls[id],
            DeclId::Local(id) => &self.local_decls[id],
        }
    }

    pub fn decl_mut(&mut self, id: DeclId) -> &mut Decl { 
        match id {
            DeclId::Global(id) => &mut self.global_decls[id],
            DeclId::Local(id) => &mut self.local_decls[id],
        }
    }
}

struct StoredDecl {
    name: String,
    item: ItemId,
    decl: DeclId,
}

struct GlobalDeclRef {
    id: DeclRefId,
    name: String,
    num_arguments: u32,
}

pub struct Builder {
    /// All items in the entire program so far
    items: DepVec<Item>,
    /// The source ranges of each item so far
    source_ranges: IdxVec<SourceRange, ItemId>,
    /// The levels of each item so far
    levels: IdxVec<u32, ItemId>,
    /// The global declarations so far
    global_decls: IdxVec<Decl, GlobalDeclId>,
    /// The local declarations so far
    local_decls: IdxVec<Decl, LocalDeclId>,
    /// Each declref's overload choices so far
    overloads: IdxVec<Vec<DeclId>, DeclRefId>,

    /// The names and arities of the global declaration references so far
    global_decl_refs: Vec<GlobalDeclRef>,
    /// All stored declarations in the current scope
    stored_decls: Vec<StoredDecl>,
}

impl Builder {
    pub fn new() -> Self {
        let mut global_decls: IdxVec<Decl, GlobalDeclId> = IdxVec::new();
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
                global_decls.push(Decl::new(op.to_string(), vec![ty.clone(), ty.clone()], ty.clone()));
            }
        }
        for op in &["<", "<=", ">", ">="] {
            for ty in numerics {
                global_decls.push(Decl::new(op.to_string(), vec![ty.clone(), ty.clone()], Type::Bool));
            }
        }
        for op in &["==", "!="] {
            for ty in values {
                global_decls.push(Decl::new(op.to_string(), vec![ty.clone(), ty.clone()], Type::Bool));
            }
        }
        for op in &["&", "|"] {
            for ty in integers {
                global_decls.push(Decl::new(op.to_string(), vec![ty.clone(), ty.clone()], ty.clone()));
            }
        }
        for op in &["&=", "|="] {
            for ty in integers {
                global_decls.push(Decl::new(op.to_string(), vec![ty.clone(), ty.clone()], Type::Void));
            }
        }
        for op in &["&&", "||"] {
            global_decls.push(Decl::new(op.to_string(), vec![Type::Bool, Type::Bool], Type::Bool));
        }
        for ty in values {
            global_decls.push(Decl::new("=".to_string(), vec![ty.clone(), ty.clone()], Type::Void));
        }
        Self {
            items: DepVec::new(),
            source_ranges: IdxVec::new(),
            levels: IdxVec::new(),
            global_decls,
            local_decls: IdxVec::new(),
            overloads: IdxVec::new(),
            global_decl_refs: Vec::new(),
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
        let decl_ref_id = self.overloads.push(Vec::new());
        self.global_decl_refs.push(GlobalDeclRef { id: decl_ref_id, name: op.symbol().to_string(), num_arguments: 2 });
        let level = self.items.insert(
            &[self.levels[lhs], self.levels[rhs]],
            Item {
                kind: ItemKind::DeclRef { args: vec![lhs, rhs], id: decl_ref_id },
                id,
            },
        );
        self.levels.push(level);
        self.source_ranges.push(range);


        id
    }

    pub fn stored_decl(&mut self, name: String, root_expr: ItemId, range: SourceRange) -> ItemId {
        let id = ItemId::new(self.levels.len());
        let decl_id = self.local_decls.push(Decl { name: name.clone(), param_tys: Vec::new(), ret_ty: Type::Error });
        let decl_id = DeclId::Local(decl_id);
        let level = self.items.insert(
            &[self.levels[root_expr]],
            Item {
                kind: ItemKind::StoredDecl { root_expr, id: decl_id },
                id,
            },
        );
        self.levels.push(level);
        self.source_ranges.push(range);

        self.stored_decls.push(StoredDecl { name, item: id, decl: decl_id });

        id
    }

    pub fn decl_ref(&mut self, name: String, range: SourceRange) -> ItemId {
        let id = ItemId::new(self.levels.len());

        let mut decl: Option<(ItemId, DeclId)> = None;
        for &StoredDecl { name: ref other_name, item: other_item, decl: other_decl } in self.stored_decls.iter().rev() {
            if &name == other_name {
                decl = Some((other_item, other_decl));
                break;
            }
        }

        let mut deps = ArrayVec::<[u32; 1]>::new();
        let decl_ref = if let Some((item, decl)) = decl {
            deps.push(self.levels[item]);
            self.overloads.push(vec![decl])
        } else {
            self.overloads.push(Vec::new())
        };
        let level = self.items.insert(
            &deps[..],
            Item {
                kind: ItemKind::DeclRef { args: Vec::new(), id: decl_ref },
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

    pub fn program(mut self) -> Program {
        assert_eq!(self.stored_decls.len(), self.local_decls.len());
        for decl_ref in self.global_decl_refs {
            self.overloads[decl_ref.id] = self.global_decls.indices_satisfying(|decl| {
                decl.name == decl_ref.name && decl.param_tys.len() == decl_ref.num_arguments as usize
            }).iter().map(|&i| DeclId::Global(i)).collect();
        }

        Program {
            items: self.items,
            source_ranges: self.source_ranges,
            local_decls: self.local_decls,
            global_decls: self.global_decls,
            overloads: self.overloads,
            num_items: self.levels.len(),
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

