use std::ops::{Deref, DerefMut};
use smallvec::SmallVec;
use string_interner::Sym;

use crate::builder::*;
use crate::dep_vec::DepVec;
use crate::hir;
use crate::index_vec::{Idx, IdxVec, IdxSlice};
use crate::source_info::SourceRange;
use crate::ty::{Type, QualType};

newtype_index!(TreeId pub);
newtype_index!(RetGroupId);

#[derive(Debug)]
pub struct RetGroup { pub ty: Type, pub exprs: SmallVec<[ExprId; 1]> }
#[derive(Debug)]
pub struct Cast { pub expr: ExprId, pub ty: Type, pub cast_id: CastId }
#[derive(Debug)]
pub struct AddrOf { pub expr: ExprId, pub is_mut: bool }
#[derive(Debug)]
pub struct Dereference { pub expr: ExprId }
#[derive(Debug)]
pub struct Stmt { pub root_expr: ExprId }
#[derive(Debug)]
pub struct Do { pub terminal_expr: ExprId }
#[derive(Debug)]
pub struct AssignedDecl { pub explicit_ty: Option<Type>, pub root_expr: ExprId, pub decl_id: DeclId }
#[derive(Debug)]
pub struct Assignment { pub lhs: ExprId, pub rhs: ExprId }
#[derive(Debug)]
pub struct DeclRef { pub args: SmallVec<[ExprId; 2]>, pub decl_ref_id: DeclRefId }
#[derive(Debug)]
pub struct If { pub condition: ExprId, pub then_expr: ExprId, pub else_expr: ExprId }
#[derive(Debug)]
pub struct While { pub condition: ExprId }

impl RetGroup {
    pub fn new(ty: Type) -> RetGroup {
        RetGroup { ty, exprs: SmallVec::new() }
    }
}

/// State machine to prevent cycles at the global scope. For example:
///     fn foo = bar
///     fn bar = foo
#[derive(Copy, Clone)]
enum Level {
    Unresolved,
    Resolving,
    Resolved(u32),
}

impl Level {
    fn unwrap(self) -> u32 {
        match self {
            Level::Resolved(level) => level,
            _ => panic!("attempt to unwrap unresolved level"),
        }
    }
}

#[derive(Debug)]
pub struct Expr<T> {
    pub id: ExprId,
    pub data: T,
}

impl<T> Deref for Expr<T> {
    type Target = T;
    fn deref(&self) -> &T { &self.data }
}

impl<T> DerefMut for Expr<T> {
    fn deref_mut(&mut self) -> &mut T { &mut self.data }
}

#[derive(Debug)]
pub struct Decl {
    pub param_tys: SmallVec<[Type; 2]>,
    pub ret_ty: QualType,
}

#[derive(Debug)]
pub struct Program<'hir> {
    pub int_lits: Vec<ExprId>,
    pub dec_lits: Vec<ExprId>,
    pub str_lits: Vec<ExprId>,
    pub char_lits: Vec<ExprId>,
    pub stmts: Vec<Stmt>,
    pub explicit_rets: Vec<ExprId>,
    pub ret_groups: Vec<RetGroup>,
    pub casts: Vec<Expr<Cast>>,
    pub whiles: Vec<Expr<While>>,
    pub dos: DepVec<Expr<Do>>,
    pub assigned_decls: DepVec<AssignedDecl>,
    pub assignments: DepVec<Expr<Assignment>>,
    pub decl_refs: DepVec<Expr<DeclRef>>,
    pub addr_ofs: DepVec<Expr<AddrOf>>,
    pub derefs: DepVec<Expr<Dereference>>,
    pub ifs: DepVec<Expr<If>>,
    /// An expression to uniquely represent the void value
    pub void_expr: ExprId,
    pub source_ranges: IdxSlice<'hir, SourceRange, ExprId>,
    pub decls: IdxVec<Decl, DeclId>,
    /// Each declref's overload choices
    pub overloads: IdxVec<Vec<DeclId>, DeclRefId>,
    /// Number of expressions in the entire program
    pub num_exprs: usize,
}

struct GlobalDecl {
    id: DeclId,
    num_params: usize,
}

struct GlobalDeclGroup {
    name: Sym,
    decls: Vec<GlobalDecl>,
}

struct LocalDecl {
    name: Sym,
    id: DeclId,
}

struct CompDeclState {
    local_decls: Vec<LocalDecl>,
    /// The size of `local_decls` before the current scope was started
    scope_stack: Vec<usize>,

    /// All the expressions to be put into this comp decl's RetGroup
    returned_expressions: SmallVec<[ExprId; 1]>,
}

impl CompDeclState {
    fn new() -> Self {
        Self {
            local_decls: Vec::new(),
            scope_stack: vec![0],
            returned_expressions: SmallVec::new(),
        }
    }

    fn open_scope(&mut self) {
        self.scope_stack.push(self.local_decls.len());
    }

    fn close_scope(&mut self) {
        let new_len = self.scope_stack.pop().unwrap();
        debug_assert!(new_len <= self.local_decls.len());
        self.local_decls.truncate(new_len);
    }
}

pub struct Builder<'hir> {
    int_lits: Vec<ExprId>,
    dec_lits: Vec<ExprId>,
    str_lits: Vec<ExprId>,
    char_lits: Vec<ExprId>,
    stmts: Vec<Stmt>,
    explicit_rets: Vec<ExprId>,
    ret_groups: IdxVec<RetGroup, RetGroupId>,
    casts: Vec<Expr<Cast>>,
    whiles: Vec<Expr<While>>,
    dos: DepVec<Expr<Do>>,
    assigned_decls: DepVec<AssignedDecl>,
    assignments: DepVec<Expr<Assignment>>,
    decl_refs: DepVec<Expr<DeclRef>>,
    addr_ofs: DepVec<Expr<AddrOf>>,
    derefs: DepVec<Expr<Dereference>>,
    ifs: DepVec<Expr<If>>,
    // An expression to uniquely represent the void value
    void_expr: ExprId,

    hir: &'hir hir::Program,
    expr_levels: IdxVec<u32, ExprId>,
    decl_levels: IdxVec<Level, DeclId>,
    decls: IdxVec<Decl, DeclId>,
    global_decls: Vec<GlobalDeclGroup>,
    comp_decl_stack: Vec<CompDeclState>,
    /// Each declref's overload choices
    overloads: IdxVec<Vec<DeclId>, DeclRefId>,

    /// The terminal expression for each scope so far 
    /// (or the void expression if there is no terminal expression)
    terminal_exprs: IdxVec<ExprId, ScopeId>,
}

impl<'hir> Builder<'hir> {
    pub fn new(hir: &'hir hir::Program) -> Self {
        // Create the void expression
        let mut expr_levels = IdxVec::new();
        let void_expr = expr_levels.push(0);

        Self {
            int_lits: Vec::new(),
            dec_lits: Vec::new(),
            str_lits: Vec::new(),
            char_lits: Vec::new(),
            stmts: Vec::new(),
            explicit_rets: Vec::new(),
            ret_groups: IdxVec::new(),
            casts: Vec::new(),
            whiles: Vec::new(),
            dos: DepVec::new(),
            assigned_decls: DepVec::new(),
            assignments: DepVec::new(),
            decl_refs: DepVec::new(),
            addr_ofs: DepVec::new(),
            derefs: DepVec::new(),
            ifs: DepVec::new(),
            void_expr,
            hir,
            expr_levels,
            decl_levels: IdxVec::new(),
            decls: IdxVec::new(),
            global_decls: Vec::new(),
            comp_decl_stack: Vec::new(),
            overloads: IdxVec::new(),
            terminal_exprs: IdxVec::new(),
        }
    }

    fn open_comp_decl(&mut self) {
        self.comp_decl_stack.push(CompDeclState::new());
    }

    fn build_expr(&mut self, id: ExprId) -> u32 {
        let mut level = 0u32;

        level
    }

    fn insert<T: Item>(&mut self, value: T) {
        let level = value.compute_level(self);
        T::storage(self).insert(level, value);
    }

    fn build_decl(&mut self, id: DeclId) -> u32 {
        match self.decl_levels[id] {
            Level::Unresolved => self.decl_levels[id] = Level::Resolving,
            Level::Resolving => panic!("Cycle detected!"),
            Level::Resolved(level) => return level,
        }
        let mut level = 0u32;
        match self.hir.decls[id] {
            hir::Decl::Computed { ref param_tys, ref params, scope } => {
                self.comp_decl_stack.push(CompDeclState::new());

                let scope = &self.hir.scopes[scope];
                for &item in &scope.items {
                    match item {
                        hir::Item::Stmt(expr) => { self.build_expr(expr); },
                        hir::Item::StoredDecl { id, root_expr } => {
                            // TODO: add decl id to hir so we can actually do something here
                        },
                        // TODO: add computed decls as items to hir so we can actually do something here
                    }
                }
                level = self.build_expr(scope.terminal_expr);
                let ret_exprs = &mut self.comp_decl_stack.last_mut().unwrap().returned_expressions;
                ret_exprs.push(scope.terminal_expr);
                match &self.hir.explicit_tys[id] {
                    Some(ty) => {
                        self.ret_groups.push(RetGroup { ty: ty.clone(), exprs: ret_exprs.clone() });
                    },
                    None => {
                        assert_eq!(ret_exprs.len(), 1, "multiple returns from assigned functions not allowed");
                        let root_expr = ret_exprs[0];
                        self.insert(AssignedDecl { explicit_ty: None, root_expr, decl_id: id });
                    },
                }

                self.comp_decl_stack.pop().unwrap();
            },
            hir::Decl::Const(expr) => {},
            hir::Decl::Intrinsic { intr, ref param_tys } => {},
            hir::Decl::Parameter { index } => {},
            hir::Decl::Static(expr) => {},
            hir::Decl::Stored { id, is_mut } => {},
        }

        level
    }

    pub fn build(mut self) -> Program<'hir> {
        // Populate `self.decls`
        for (i, decl) in self.hir.decls.iter().enumerate() {
            let id = DeclId::new(i);
            let ty = self.hir.explicit_tys[id].as_ref()
                .map(|ty| ty.clone())
                .unwrap_or(Type::Error);
            let (ret_ty, param_tys) = match *decl {
                hir::Decl::Computed { ref param_tys, .. } => (
                    ty.into(),
                    param_tys.clone(),
                ),
                hir::Decl::Const(expr) => (
                    ty.into(),
                    SmallVec::new(),
                ),
                hir::Decl::Intrinsic { intr, ref param_tys, } => (
                    ty.into(),
                    param_tys.clone(),
                ),
                hir::Decl::Parameter { .. } => (
                    ty.into(),
                    SmallVec::new(),
                ),
                hir::Decl::Static(expr) => (
                    QualType { ty, is_mut: true },
                    SmallVec::new(),
                ),
                hir::Decl::Stored { id, is_mut } => (
                    QualType { ty, is_mut },
                    SmallVec::new(),
                ),
            };
            self.decls.push(Decl { param_tys, ret_ty });
        }

        // Populate `self.global_decls`
        for &id in &self.hir.global_decls {
            let name = self.hir.names[id];
            match self.global_decls.iter_mut().find(|group| group.name == name) {
                Some(group) => group,
                None => {
                    self.global_decls.push(GlobalDeclGroup { name, decls: Vec::new() });
                    self.global_decls.last_mut().unwrap()
                },
            }.decls.push(GlobalDecl { id, num_params: self.decls[id].param_tys.len() });
        }

        // Build global decls, which will recursively build all expressions and local declarations
        for &id in &self.hir.global_decls {
            self.build_decl(id);
        }

        Program {
            int_lits: self.int_lits,
            dec_lits: self.dec_lits,
            str_lits: self.str_lits,
            char_lits: self.char_lits,
            stmts: self.stmts,
            explicit_rets: self.explicit_rets,
            ret_groups: self.ret_groups.raw,
            casts: self.casts,
            whiles: self.whiles,
            dos: self.dos,
            assigned_decls: self.assigned_decls,
            assignments: self.assignments,
            decl_refs: self.decl_refs,
            addr_ofs: self.addr_ofs,
            derefs: self.derefs,
            ifs: self.ifs,
            void_expr: self.void_expr,
            source_ranges: self.hir.source_ranges.as_idx_slice(),
            decls: self.decls,
            overloads: self.overloads,
            num_exprs: self.expr_levels.len(),
        }
    }
}

macro_rules! level {
    ($b:expr, $($level:expr),+) => {{
        [$($b.expr_levels[$level]),+].iter().max().unwrap() + 1
    }}
}

pub trait Item: Sized {
    fn compute_level<'hir>(&'hir self, builder: &'hir Builder<'hir>) -> u32;
    fn storage<'a, 'hir>(builder: &'a mut Builder<'hir>) -> &'a mut DepVec<Self>;
}

impl Item for Expr<Do> {
    fn compute_level<'hir>(&'hir self, b: &'hir Builder<'hir>) -> u32 {
        level!(b, self.terminal_expr)
    }
    fn storage<'a, 'hir>(builder: &'a mut Builder<'hir>) -> &'a mut DepVec<Self> { &mut builder.dos }
}

impl Item for Expr<Assignment> {
    fn compute_level<'hir>(&'hir self, b: &'hir Builder<'hir>) -> u32 {
        level!(b, self.lhs, self.rhs)
    }
    fn storage<'a, 'hir>(builder: &'a mut Builder<'hir>) -> &'a mut DepVec<Self> { &mut builder.assignments }
}

impl Item for Expr<DeclRef> {
    fn compute_level<'hir>(&'hir self, b: &'hir Builder<'hir>) -> u32 {
        self.args.iter().map(|&id| b.expr_levels[id]).max().unwrap() + 1
    }
    fn storage<'a, 'hir>(builder: &'a mut Builder<'hir>) -> &'a mut DepVec<Self> { &mut builder.decl_refs }
}

impl Item for Expr<AddrOf> {
    fn compute_level<'hir>(&'hir self, b: &'hir Builder<'hir>) -> u32 {
        level!(b, self.expr)
    }
    fn storage<'a, 'hir>(builder: &'a mut Builder<'hir>) -> &'a mut DepVec<Self> { &mut builder.addr_ofs }
}

impl Item for Expr<Dereference> {
    fn compute_level<'hir>(&'hir self, b: &'hir Builder<'hir>) -> u32 {
        level!(b, self.expr)
    }
    fn storage<'a, 'hir>(builder: &'a mut Builder<'hir>) -> &'a mut DepVec<Self> { &mut builder.derefs }
}

impl Item for Expr<If> {
    fn compute_level<'hir>(&'hir self, b: &'hir Builder<'hir>) -> u32 {
        level!(b, self.condition, self.then_expr, self.else_expr)
    }
    fn storage<'a, 'hir>(builder: &'a mut Builder<'hir>) -> &'a mut DepVec<Self> { &mut builder.ifs }
}

impl Item for AssignedDecl {
    fn compute_level<'hir>(&'hir self, b: &'hir Builder<'hir>) -> u32 {
        level!(b, self.root_expr)
    }
    fn storage<'a, 'hir>(builder: &'a mut Builder<'hir>) -> &'a mut DepVec<Self> { &mut builder.assigned_decls }
}