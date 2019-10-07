use std::ops::{Deref, DerefMut};
use std::collections::HashMap;

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

/// State machine to prevent cycles at the global scope. For example:
///     fn foo = bar
///     fn bar = foo
#[derive(Debug, Copy, Clone)]
enum Level {
    Unresolved,
    Resolving,
    Resolved(u32),
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

#[derive(Debug)]
struct GlobalDecl {
    id: DeclId,
    num_params: usize,
}

#[derive(Debug)]
struct GlobalDeclGroup {
    name: Sym,
    decls: Vec<GlobalDecl>,
}

#[derive(Debug)]
struct LocalDecl {
    name: Sym,
    id: DeclId,
}

#[derive(Debug)]
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
            scope_stack: Vec::new(),
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

    staged_decls: HashMap<DeclId, StagedDecl>,
}

enum StagedDecl {
    AssignedDecl(AssignedDecl),
    RetGroup(RetGroup),
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

            staged_decls: HashMap::new(),
        }
    }

    fn prebuild_expr(&mut self, id: ExprId) -> u32 {
        let level = match self.hir.exprs[id] {
            hir::Expr::AddrOf { expr, is_mut } => {
                self.prebuild_expr(expr);
                self.get_expr_level(id, AddrOf { expr, is_mut }, 0)
            },
            hir::Expr::Cast { expr, .. } => {
                self.prebuild_expr(expr);
                0
            },
            hir::Expr::CharLit { .. } | hir::Expr::DecLit { .. } | hir::Expr::IntLit { .. } | hir::Expr::StrLit { .. } => 0,
            hir::Expr::DeclRef { name, ref arguments, id: decl_ref_id } => {
                for &arg in arguments {
                    self.prebuild_expr(arg);
                }

                let mut overloads = Vec::new();
                if let Some(state) = self.comp_decl_stack.last() {
                    if let Some(decl) = state.local_decls.iter().rev().find(|decl| decl.name == name) {
                        overloads = vec![decl.id];
                    }
                }
                if overloads.is_empty() {
                    if let Some(group) = self.global_decls.iter().find(|group| group.name == name) {
                        overloads = group.decls.iter()
                            .filter_map(|decl| if decl.num_params == arguments.len() {
                                Some(decl.id)
                            } else {
                                None
                            }).collect();
                    }
                }
                let deepest_overload = overloads.iter()
                    .map(|&overload| self.prebuild_decl(overload))
                    .max().unwrap();
                self.overloads[decl_ref_id] = overloads;
                // TODO: would be nice if we didn't clone and then immediately throw away the arguments
                self.get_expr_level(id, DeclRef { args: arguments.clone(), decl_ref_id }, deepest_overload)
            },
            hir::Expr::Deref(expr) => {
                self.prebuild_expr(expr);
                self.get_expr_level(id, Dereference { expr }, 0)
            },
            hir::Expr::Do { scope } => {
                let terminal_expr = self.prebuild_scope(scope);
                self.get_expr_level(id, Do { terminal_expr }, 0)
            },
            hir::Expr::If { condition, then_scope, else_scope } => {
                self.prebuild_expr(condition);
                let then_expr = self.prebuild_scope(then_scope);
                let else_expr = else_scope.map_or(self.void_expr, |scope| self.prebuild_scope(scope));
                self.get_expr_level(id, If { condition, then_expr, else_expr }, 0)
            },
            hir::Expr::Ret { expr } => {
                self.prebuild_expr(expr);
                self.comp_decl_stack.last_mut()
                    .expect("Found return expression outside of comp decl")
                    .returned_expressions
                    .push(expr);
                0
            },
            hir::Expr::Set { lhs, rhs } => {
                self.prebuild_expr(lhs);
                self.prebuild_expr(rhs);
                self.get_expr_level(id, Assignment { lhs, rhs }, 0)
            },
            hir::Expr::Void => 0,
            hir::Expr::While { condition, scope } => {
                self.prebuild_expr(condition);
                self.prebuild_scope(scope);
                0
            },
        };
        self.expr_levels[id] = level;
        level
    }

    fn get_level<T: Item>(&self, value: T, additional_level: u32) -> u32 {
        std::cmp::max(additional_level, value.compute_level(self))
    }

    fn get_expr_level<T>(&self, id: ExprId, data: T, additional_level: u32) -> u32 where Expr<T>: Item {
        self.get_level(Expr { id, data }, additional_level)
    }

    fn insert_expr<T>(&mut self, id: ExprId, data: T) where Expr<T>: Item {
        let level = self.expr_levels[id];
        Expr::<T>::storage(self).insert(level, Expr { id, data });
    }

    /// Returns terminal expression
    fn prebuild_scope(&mut self, id: ScopeId) -> ExprId {
        self.comp_decl_stack.last_mut().unwrap().open_scope();

        let scope = &self.hir.scopes[id];
        for &item in &scope.items {
            match item {
                hir::Item::Stmt(expr) => { self.prebuild_expr(expr); },
                hir::Item::StoredDecl { decl_id, root_expr, .. } => {
                    let name = self.hir.names[decl_id];
                    let explicit_ty = self.hir.explicit_tys[decl_id].clone();

                    let level = self.prebuild_expr(root_expr) + 1;
                    self.staged_decls.insert(decl_id, StagedDecl::AssignedDecl(AssignedDecl { explicit_ty, root_expr, decl_id }));

                    self.comp_decl_stack.last_mut().unwrap().local_decls.push(LocalDecl { name, id: decl_id });

                    self.decl_levels[decl_id] = Level::Resolved(level);
                },
                hir::Item::ComputedDecl(decl_id) => {
                    self.prebuild_decl(decl_id);

                    let name = self.hir.names[decl_id];
                    self.comp_decl_stack.last_mut().unwrap().local_decls.push(LocalDecl { name, id: decl_id });
                },
            }
        }
        self.prebuild_expr(scope.terminal_expr);

        self.comp_decl_stack.last_mut().unwrap().close_scope();

        scope.terminal_expr
    }

    fn prebuild_decl(&mut self, id: DeclId) -> u32 {
        match self.decl_levels[id] {
            Level::Unresolved => self.decl_levels[id] = Level::Resolving,
            Level::Resolving => panic!("Cycle detected on decl {}!", self.hir.interner.resolve(self.hir.names[id]).unwrap()),
            Level::Resolved(level) => return level,
        }
        let level = match self.hir.decls[id] {
            hir::Decl::Computed { ref params, scope, .. } => {
                // Resolve computed decls with explicit tys before prebuilding their scope
                if self.hir.explicit_tys[id].is_some() {
                    self.decl_levels[id] = Level::Resolved(0);
                }

                let mut comp_decl_state = CompDeclState::new();
                // Add function name to local scope to enable recursion
                let name = self.hir.names[id];
                comp_decl_state.local_decls.push(LocalDecl { name, id });
                comp_decl_state.local_decls.reserve(params.len());
                for &id in params {
                    let name = self.hir.names[id];
                    comp_decl_state.local_decls.push(LocalDecl { name, id });
                }
                self.comp_decl_stack.push(comp_decl_state);

                let terminal_expr = self.prebuild_scope(scope);
                let ret_exprs = &mut self.comp_decl_stack.last_mut().unwrap().returned_expressions;
                ret_exprs.push(terminal_expr);
                let level = match &self.hir.explicit_tys[id] {
                    Some(ty) => {
                        self.staged_decls.insert(id, StagedDecl::RetGroup(RetGroup { ty: ty.clone(), exprs: ret_exprs.clone() }));
                        0
                    },
                    None => {
                        assert_eq!(ret_exprs.len(), 1, "multiple returns from assigned functions not allowed");
                        let root_expr = ret_exprs[0];
                        self.staged_decls.insert(id, StagedDecl::AssignedDecl(AssignedDecl { explicit_ty: None, root_expr, decl_id: id }));
                        self.expr_levels[root_expr] + 1
                    },
                };

                self.comp_decl_stack.pop().unwrap();

                level
            },
            hir::Decl::Const(expr) | hir::Decl::Static(expr) => {
                self.prebuild_expr(expr);
                self.staged_decls.insert(id, StagedDecl::AssignedDecl(AssignedDecl { explicit_ty: self.hir.explicit_tys[id].clone(), root_expr: expr, decl_id: id }));
                self.expr_levels[expr] + 1
            },
            hir::Decl::Intrinsic { .. } | hir::Decl::Parameter { .. } => 0,
            hir::Decl::Stored { .. } => panic!("Should've already been handled in prebuild_scope"),
        };
        self.decl_levels[id] = Level::Resolved(level);
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
                hir::Decl::Const(_) => (
                    ty.into(),
                    SmallVec::new(),
                ),
                hir::Decl::Intrinsic { ref param_tys, .. } => (
                    ty.into(),
                    param_tys.clone(),
                ),
                hir::Decl::Parameter { .. } => (
                    ty.into(),
                    SmallVec::new(),
                ),
                hir::Decl::Static(_) => (
                    QualType { ty, is_mut: true },
                    SmallVec::new(),
                ),
                hir::Decl::Stored { is_mut, .. } => (
                    QualType { ty, is_mut },
                    SmallVec::new(),
                ),
            };
            self.decls.push(Decl { param_tys, ret_ty });
            self.decl_levels.push(Level::Unresolved);
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

        debug_assert!(self.overloads.is_empty());
        debug_assert!(self.expr_levels.len() == 1);
        self.overloads.resize_with(self.hir.num_decl_refs, || Vec::new());
        self.expr_levels.resize_with(self.hir.exprs.len(), || std::u32::MAX);

        // Prebuild global decls, which will recursively compute the typechecking dependencies of all declarations and expressions
        for &id in &self.hir.global_decls {
            self.prebuild_decl(id);
        }

        // Build expressions
        for (i, expr) in self.hir.exprs.iter().enumerate() {
            let id = ExprId::new(i);
            if self.hir.exprs_in_type_ctx.contains(&id) { continue; }

            match *expr {
                hir::Expr::AddrOf { expr, is_mut } => self.insert_expr(id, AddrOf { expr, is_mut }),
                hir::Expr::Cast { expr, ref ty, cast_id } => self.casts.push(Expr { id, data: Cast { expr, ty: ty.clone(), cast_id } }),
                hir::Expr::CharLit { .. } => self.char_lits.push(id),
                hir::Expr::DecLit { .. } => self.dec_lits.push(id),
                hir::Expr::IntLit { .. } => self.int_lits.push(id),
                hir::Expr::StrLit { .. } => self.str_lits.push(id),
                hir::Expr::DeclRef { ref arguments, id: decl_ref_id, .. } => self.insert_expr(id, DeclRef { args: arguments.clone(), decl_ref_id }),
                hir::Expr::Deref(expr) => self.insert_expr(id, Dereference { expr }),
                hir::Expr::Do { scope } => self.insert_expr(id, Do { terminal_expr: self.hir.scopes[scope].terminal_expr }),
                hir::Expr::If { condition, then_scope, else_scope } => {
                    let then_expr = self.hir.scopes[then_scope].terminal_expr;
                    let else_expr = else_scope.map_or(self.void_expr, |scope| self.hir.scopes[scope].terminal_expr);
                    self.insert_expr(id, If { condition, then_expr, else_expr});
                },
                // Handled with ret groups
                hir::Expr::Ret { .. } => {},
                hir::Expr::Set { lhs, rhs } => self.insert_expr(id, Assignment { lhs, rhs }),
                hir::Expr::Void => {},
                hir::Expr::While { condition, .. } => self.whiles.push(Expr { id, data: While { condition } }),
            }
        }

        // Build declarations
        for i in 0..self.hir.decls.len() {
            let id = DeclId::new(i);
            if let Some(decl) = self.staged_decls.remove(&id) {
                match decl {
                    StagedDecl::AssignedDecl(assigned_decl) => {
                        let level = match self.decl_levels[id] {
                            Level::Resolved(level) => level,
                            _ => panic!("failed to get level"),
                        };
                        self.assigned_decls.insert(level, assigned_decl);
                    },
                    StagedDecl::RetGroup(ret_group) => {
                        self.ret_groups.push(ret_group);
                    }
                }
            }
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

macro_rules! item_impl {
    ($ty:ty, $storage:ident; $($level:ident),+) => {
        impl Item for $ty {
            fn compute_level<'hir>(&'hir self, b: &'hir Builder<'hir>) -> u32 {
                [$(b.expr_levels[self.$level]),+].iter().max().unwrap() + 1
            }
            fn storage<'a, 'hir>(builder: &'a mut Builder<'hir>) -> &'a mut DepVec<Self> { &mut builder.$storage }
        }
    }
}

pub trait Item: Sized {
    fn compute_level<'hir>(&'hir self, builder: &'hir Builder<'hir>) -> u32;
    fn storage<'a, 'hir>(builder: &'a mut Builder<'hir>) -> &'a mut DepVec<Self>;
}

item_impl!(Expr<Do>, dos; terminal_expr);
item_impl!(Expr<Assignment>, assignments; lhs, rhs);
item_impl!(Expr<AddrOf>, addr_ofs; expr);
item_impl!(Expr<Dereference>, derefs; expr);
item_impl!(Expr<If>, ifs; condition, then_expr, else_expr);
item_impl!(AssignedDecl, assigned_decls; root_expr);

impl Item for Expr<DeclRef> {
    fn compute_level<'hir>(&'hir self, b: &'hir Builder<'hir>) -> u32 {
        self.args.iter().map(|&id| b.expr_levels[id]).max().unwrap_or(0) + 1
    }
    fn storage<'a, 'hir>(builder: &'a mut Builder<'hir>) -> &'a mut DepVec<Self> { &mut builder.decl_refs }
}