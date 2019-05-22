use std::ops::{Deref, DerefMut};

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
pub struct IntLit;
#[derive(Debug)]
pub struct DecLit;
#[derive(Debug)]
pub struct Stmt { pub root_expr: ItemId }
#[derive(Debug)]
pub struct AssignedDecl { pub root_expr: ItemId, pub decl_id: DeclId }
#[derive(Debug)]
pub struct DeclRef { pub args: Vec<ItemId>, pub decl_ref_id: DeclRefId }

#[derive(Debug)]
pub struct Item<T> {
    pub id: ItemId,
    pub data: T,
}

impl<T> Deref for Item<T> {
    type Target = T;
    fn deref(&self) -> &T { &self.data }
}

impl<T> DerefMut for Item<T> {
    fn deref_mut(&mut self) -> &mut T { &mut self.data }
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
    /// All integer literals in the entire program
    pub int_lits: Vec<Item<IntLit>>,
    /// All decimal literals in the entire program
    pub dec_lits: Vec<Item<DecLit>>,
    /// All assigned decls in the entire program
    pub assigned_decls: DepVec<AssignedDecl>,
    /// All decl refs in the entire program
    pub decl_refs: DepVec<Item<DeclRef>>,
    /// All statements in the entire program
    pub stmts: DepVec<Stmt>,

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

#[derive(Clone)]
struct LocalDecl {
    name: String,
    level: u32,
    decl: DeclId,
}

struct LocalDeclList {
    decls: Vec<LocalDecl>,
}

impl LocalDeclList {
    fn new() -> Self {
        Self { decls: Vec::new() }
    }
}

struct GlobalDeclRef {
    id: DeclRefId,
    name: String,
    num_arguments: u32,
}

pub struct Builder {
    /// All integer literals in the entire program so far
    int_lits: Vec<Item<IntLit>>,
    /// All decimal literals in the entire program so far
    dec_lits: Vec<Item<DecLit>>,
    /// All assigned decls in the entire program so far
    assigned_decls: DepVec<AssignedDecl>,
    /// All decl refs in the entire program so far
    decl_refs: DepVec<Item<DeclRef>>,
    /// All statements in the entire program so far
    stmts: DepVec<Stmt>,

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
    /// List of local declarations for each nested function
    local_decl_stack: Vec<LocalDeclList>,
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
        global_decls.push(Decl::new("pi".to_string(), Vec::new(), Type::f64()));
        global_decls.push(Decl::new("abs".to_string(), vec![Type::f32()], Type::f64()));
        Self {
            int_lits: Vec::new(),
            dec_lits: Vec::new(),
            assigned_decls: DepVec::new(),
            decl_refs: DepVec::new(),
            stmts: DepVec::new(),
            source_ranges: IdxVec::new(),
            levels: IdxVec::new(),
            global_decls,
            local_decls: IdxVec::new(),
            overloads: IdxVec::new(),
            global_decl_refs: Vec::new(),
            local_decl_stack: vec![LocalDeclList::new()],
        }
    }

    pub fn int_lit(&mut self, lit: u64, range: SourceRange) -> ItemId {
        let id = ItemId::new(self.levels.len());
        self.int_lits.push(Item { id, data: IntLit });
        self.levels.push(0);
        self.source_ranges.push(range);
        id
    }

    pub fn dec_lit(&mut self, lit: f64, range: SourceRange) -> ItemId {
        let id = ItemId::new(self.levels.len());
        self.dec_lits.push(Item { id, data: DecLit });
        self.levels.push(0);
        self.source_ranges.push(range);

        id
    }

    pub fn bin_op(&mut self, op: BinOp, lhs: ItemId, rhs: ItemId, range: SourceRange) -> ItemId {
        let id = ItemId::new(self.levels.len());
        let decl_ref_id = self.overloads.push(Vec::new());
        self.global_decl_refs.push(GlobalDeclRef { id: decl_ref_id, name: op.symbol().to_string(), num_arguments: 2 });
        let level = self.decl_refs.insert(
            &[self.levels[lhs], self.levels[rhs]],
            Item { id, data: DeclRef { args: vec![lhs, rhs], decl_ref_id } },
        );
        self.levels.push(level);
        self.source_ranges.push(range);

        id
    }

    pub fn stored_decl(&mut self, name: String, root_expr: ItemId, range: SourceRange) -> ItemId {
        let id = ItemId::new(self.levels.len());
        let decl_id = self.local_decls.push(Decl { name: name.clone(), param_tys: Vec::new(), ret_ty: Type::Error });
        let decl_id = DeclId::Local(decl_id);
        let level = self.assigned_decls.insert(&[self.levels[root_expr]], AssignedDecl { root_expr, decl_id });
        self.source_ranges.push(range);

        self.local_decl_stack.last_mut().unwrap().decls.push(LocalDecl { name, level, decl: decl_id });

        id
    }

    pub fn stmt(&mut self, root_expr: ItemId) {
        self.stmts.insert(&[self.levels[root_expr]], Stmt { root_expr });
    }

    pub fn begin_computed_decl(&mut self, name: String, param_names: Vec<String>, param_tys: Vec<Type>, ret_ty: Type, proto_range: SourceRange) {
        assert_eq!(param_names.len(), param_tys.len());
        let decl_id = self.local_decls.push(Decl { name: name.clone(), param_tys: param_tys.clone(), ret_ty: ret_ty.clone() });
        let decl_id = DeclId::Local(decl_id);
        let local_decl = LocalDecl { name: name.clone(), level: 0, decl: decl_id };
        // Add decl to enclosing scope
        self.local_decl_stack.last_mut().unwrap().decls.push(local_decl.clone());
        let mut decl_list = LocalDeclList::new();
        // Add decl to its own scope to enable recursion
        decl_list.decls.push(local_decl);
        // Add parameters to scope
        for (name, ty) in param_names.iter().zip(&param_tys) {
            let id = self.local_decls.push(Decl { name: name.clone(), param_tys: Vec::new(), ret_ty: ty.clone() });
            decl_list.decls.push(LocalDecl { name: name.clone(), level: 0, decl: DeclId::Local(id) });
        }
        self.local_decl_stack.push(decl_list);
    }

    pub fn end_computed_decl(&mut self) {
        self.local_decl_stack.pop().unwrap();
    }

    pub fn decl_ref(&mut self, name: String, arguments: Vec<ItemId>, range: SourceRange) -> ItemId {
        let id = ItemId::new(self.levels.len());

        let mut decl: Option<(u32, DeclId)> = None;
        for &LocalDecl { name: ref other_name, level: other_level, decl: other_decl } in self.local_decl_stack.last().unwrap().decls.iter().rev() {
            if &name == other_name {
                decl = Some((other_level, other_decl));
                break;
            }
        }

        let mut deps = ArrayVec::<[u32; 2]>::new();
        deps.push(arguments.iter().map(|&id| self.levels[id]).max().unwrap_or_else(|| 0));
        let decl_ref_id = if let Some((level, decl)) = decl {
            // Local decl

            // Local decls must be stored atm, so if there are arguments it's a compile error (or right now, a panic)
            deps.push(level);
            self.overloads.push(vec![decl])
        } else {
            // Global decl
            let decl_ref_id = self.overloads.push(Vec::new());
            self.global_decl_refs.push(GlobalDeclRef { id: decl_ref_id, name: name, num_arguments: arguments.len() as u32 });
            decl_ref_id
        };
        let level = self.decl_refs.insert(
            &deps[..],
            Item { id, data: DeclRef { args: arguments, decl_ref_id } },
        );
        self.levels.push(level);
        self.source_ranges.push(range);

        id
    }

    pub fn get_range(&self, id: ItemId) -> SourceRange {
        self.source_ranges[id].clone()
    }

    pub fn program(mut self) -> Program {
        for decl_ref in self.global_decl_refs {
            self.overloads[decl_ref.id] = self.global_decls.indices_satisfying(|decl| {
                decl.name == decl_ref.name && decl.param_tys.len() == decl_ref.num_arguments as usize
            }).iter().map(|&i| DeclId::Global(i)).collect();
        }

        Program {
            int_lits: self.int_lits,
            dec_lits: self.dec_lits,
            assigned_decls: self.assigned_decls,
            decl_refs: self.decl_refs,
            stmts: self.stmts,
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

