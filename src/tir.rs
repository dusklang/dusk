use std::cmp::max;
use std::ops::{Deref, DerefMut};

use arrayvec::ArrayVec;
use smallvec::SmallVec;
use string_interner::Sym;

use crate::driver::Driver;
use crate::builder::*;
use crate::dep_vec::DepVec;
use crate::hir;
use crate::index_vec::{Idx, IdxVec};

newtype_index!(TreeId pub);
newtype_index!(RetGroupId);
newtype_index!(SpVarId);

#[derive(Debug)]
pub struct RetGroup { pub ty: ExprId, pub exprs: SmallVec<[ExprId; 1]> }
#[derive(Debug)]
pub struct Cast { pub expr: ExprId, pub ty: ExprId, pub cast_id: CastId }
#[derive(Debug)]
pub struct AddrOf { pub expr: ExprId, pub is_mut: bool }
#[derive(Debug)]
pub struct Dereference { pub expr: ExprId }
#[derive(Debug)]
pub struct Pointer { pub expr: ExprId }
#[derive(Debug)]
pub struct Stmt { pub root_expr: ExprId }
#[derive(Debug)]
pub struct Do { pub terminal_expr: ExprId }
#[derive(Debug)]
pub struct AssignedDecl { pub explicit_ty: Option<ExprId>, pub root_expr: ExprId, pub decl_id: DeclId }
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
    Resolved(u32, SpVarId),
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
    pub param_tys: SmallVec<[ExprId; 2]>,
    pub is_mut: bool,
}

#[derive(Debug)]
pub struct Subprogram {
    pub int_lits: Vec<ExprId>,
    pub dec_lits: Vec<ExprId>,
    pub str_lits: Vec<ExprId>,
    pub char_lits: Vec<ExprId>,
    pub const_tys: Vec<ExprId>,
    pub stmts: Vec<Stmt>,
    pub explicit_rets: Vec<ExprId>,
    // Not public because RetGroupIds are not exposed outside of TIR. Instead, you get a slice via the `ret_groups()` method on `Subprogram`.
    ret_groups: IdxVec<RetGroup, RetGroupId>,
    pub casts: Vec<Expr<Cast>>,
    pub whiles: Vec<Expr<While>>,
    pub dos: DepVec<Expr<Do>>,
    pub assigned_decls: DepVec<AssignedDecl>,
    pub assignments: DepVec<Expr<Assignment>>,
    pub decl_refs: DepVec<Expr<DeclRef>>,
    pub addr_ofs: DepVec<Expr<AddrOf>>,
    pub derefs: DepVec<Expr<Dereference>>,
    pub pointers: DepVec<Expr<Pointer>>,
    pub ifs: DepVec<Expr<If>>,

    /// The expressions in this subprogram that later subprograms have eval dependencies on
    pub eval_dependees: Vec<ExprId>,
}

impl Subprogram {
    fn new() -> Self {
        Self {
            int_lits: Vec::new(),
            dec_lits: Vec::new(),
            str_lits: Vec::new(),
            char_lits: Vec::new(),
            const_tys: Vec::new(),
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
            pointers: DepVec::new(),
            ifs: DepVec::new(),

            eval_dependees: Vec::new(),
        }
    }
    pub fn ret_groups(&self) -> &[RetGroup] {
        &self.ret_groups.raw[..]
    }
}

#[derive(Debug)]
struct SubprogramVar {
    weak: Vec<SpVarId>,
    eval: Vec<SpVarId>,
    codegen: Vec<SpVarId>,
    has_eval_dependers: bool,

    value: Option<u32>,
}

impl SubprogramVar {
    fn new() -> Self { 
        Self {
            weak: Vec::new(),
            eval: Vec::new(),
            codegen: Vec::new(),
            has_eval_dependers: false,

            value: None,
        }
    }
}

#[derive(Debug)]
pub struct Builder {
    pub sub_progs: Vec<Subprogram>,
    /// An expression to uniquely represent the void value
    pub void_expr: ExprId,
    pub num_casts: usize,
    pub decls: IdxVec<Decl, DeclId>,
    /// Each declref's overload choices
    pub overloads: IdxVec<Vec<DeclId>, DeclRefId>,

    expr_levels: IdxVec<u32, ExprId>,
    decl_levels: IdxVec<Level, DeclId>,

    sp_vars: IdxVec<SubprogramVar, SpVarId>,

    expr_sp_vars: IdxVec<SpVarId, ExprId>,
    decl_sp_vars: IdxVec<SpVarId, DeclId>,

    expr_sub_progs: IdxVec<u32, ExprId>,
    decl_sub_progs: IdxVec<u32, DeclId>,

    global_decls: Vec<GlobalDeclGroup>,
    comp_decl_stack: Vec<CompDeclState>,

    staged_ret_groups: Vec<(DeclId, RetGroup)>,
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

impl Builder {
    pub fn new() -> Self {
        // Create the void expression
        let mut expr_levels = IdxVec::new();
        let void_expr = expr_levels.push(0);

        let mut sp_vars = IdxVec::new();
        let void_sp_var = sp_vars.push(SubprogramVar::new());
        let mut expr_sp_vars = IdxVec::new();
        expr_sp_vars.push(void_sp_var);

        Self {
            sub_progs: Vec::new(),
            void_expr,
            num_casts: 0,
            expr_levels,
            decl_levels: IdxVec::new(),

            expr_sp_vars,
            decl_sp_vars: IdxVec::new(),

            expr_sub_progs: IdxVec::new(),
            decl_sub_progs: IdxVec::new(),

            sp_vars,

            decls: IdxVec::new(),
            global_decls: Vec::new(),
            comp_decl_stack: Vec::new(),
            overloads: IdxVec::new(),

            staged_ret_groups: Vec::new(),
        }
    }

    pub fn num_exprs(&self) -> usize {
        self.expr_levels.len()
    }
}

impl Driver {
    pub fn build_tir(&mut self) {
        // Populate `self.decls`
        for decl in &self.hir.decls {
            let (is_mut, param_tys) = match *decl {
                hir::Decl::Computed { ref param_tys, .. } => (
                    false,
                    param_tys.clone(),
                ),
                hir::Decl::Const(_) => (
                    false,
                    SmallVec::new(),
                ),
                hir::Decl::Intrinsic { ref param_tys, .. } => (
                    false,
                    param_tys.clone(),
                ),
                hir::Decl::Parameter { .. } => (
                    false,
                    SmallVec::new(),
                ),
                hir::Decl::Static(_) => (
                    true,
                    SmallVec::new(),
                ),
                hir::Decl::Stored { is_mut, .. } => (
                    is_mut,
                    SmallVec::new(),
                ),
            };
            self.tir.decls.push(Decl { param_tys, is_mut });
            self.tir.decl_levels.push(Level::Unresolved);
        }

        // Populate `self.tir.global_decls`
        for &id in &self.hir.global_decls {
            let name = self.hir.names[id];
            match self.tir.global_decls.iter_mut().find(|group| group.name == name) {
                Some(group) => group,
                None => {
                    self.tir.global_decls.push(GlobalDeclGroup { name, decls: Vec::new() });
                    self.tir.global_decls.last_mut().unwrap()
                },
            }.decls.push(GlobalDecl { id, num_params: self.tir.decls[id].param_tys.len() });
        }

        debug_assert!(self.tir.overloads.is_empty());
        debug_assert_eq!(self.tir.expr_levels.len(), 1);
        self.tir.overloads.resize_with(self.hir.num_decl_refs, || Vec::new());
        self.tir.expr_levels.resize_with(self.hir.exprs.len(), || std::u32::MAX);

        self.tir.expr_sp_vars.resize_with(self.hir.exprs.len(), || SpVarId::new(std::usize::MAX));
        self.tir.decl_sp_vars.resize_with(self.hir.decls.len(), || SpVarId::new(std::usize::MAX));

        // Prebuild global decls, which will recursively compute the typechecking dependencies of all declarations and expressions
        for i in 0..self.hir.global_decls.len() {
            self.prebuild_decl(self.hir.global_decls[i], None);
        }

        // Solve system of subprogram variables
        for i in 0..self.tir.sp_vars.len() {
            let id = SpVarId::new(i);
            self.solve_sp_var(id);
        }

        // Apply subprogram solution to expressions:
        self.tir.expr_sub_progs.reserve(self.hir.exprs.len());
        for i in 0..self.hir.exprs.len() {
            let id = ExprId::new(i);

            let var = self.tir.expr_sp_vars[id];
            let val = self.tir.sp_vars[var].value.unwrap();
            self.tir.expr_sub_progs.push(val);
        }

        // Apply subprogram solution to declarations:
        self.tir.decl_sub_progs.reserve(self.hir.decls.len());
        for i in 0..self.hir.decls.len() {
            let id = DeclId::new(i);
            let var = self.tir.decl_sp_vars[id];
            let val = self.tir.sp_vars[var].value.unwrap();
            self.tir.decl_sub_progs.push(val);
        }

        let sp_var = self.tir.expr_sp_vars[ExprId::new(525)];
        // Build expressions
        for i in 0..self.hir.exprs.len() {
            let id = ExprId::new(i);
            let expr = &self.hir.exprs[id];
            let sp = self.tir.expr_sub_progs[id];
            let sp_usize = sp as usize;
            // TODO: store the maximum subprogram number and move this out of the loop!
            let new_len = max(self.tir.sub_progs.len(), sp_usize + 1);
            self.tir.sub_progs.resize_with(new_len, || Subprogram::new());

            let sp_var = self.tir.expr_sp_vars[id];
            if self.tir.sp_vars[sp_var].has_eval_dependers {
                self.tir.sub_progs[sp_usize].eval_dependees.push(id);
            }

            match *expr {
                hir::Expr::AddrOf { expr, is_mut } => self.insert_expr(sp, id, AddrOf { expr, is_mut }),
                hir::Expr::Pointer { expr, .. } => self.insert_expr(sp, id, Pointer { expr }),
                hir::Expr::Cast { expr, ref ty, cast_id } => {
                    self.tir.num_casts += 1;
                    let ty = ty.clone();
                    self.insert_expr(sp, id, Cast { expr, ty, cast_id })
                },
                hir::Expr::CharLit { .. } => self.tir.sub_progs[sp_usize].char_lits.push(id),
                hir::Expr::DecLit { .. } => self.tir.sub_progs[sp_usize].dec_lits.push(id),
                hir::Expr::IntLit { .. } => self.tir.sub_progs[sp_usize].int_lits.push(id),
                hir::Expr::StrLit { .. } => self.tir.sub_progs[sp_usize].str_lits.push(id),
                hir::Expr::ConstTy(_) => self.tir.sub_progs[sp_usize].const_tys.push(id),
                hir::Expr::DeclRef { ref arguments, id: decl_ref_id, .. } => {
                    let args = arguments.clone();
                    self.insert_expr(sp, id, DeclRef { args, decl_ref_id })
                },
                hir::Expr::Deref(expr) => self.insert_expr(sp, id, Dereference { expr }),
                hir::Expr::Do { scope } => self.insert_expr(sp, id, Do { terminal_expr: self.hir.scopes[scope].terminal_expr }),
                hir::Expr::If { condition, then_scope, else_scope } => {
                    let then_expr = self.hir.scopes[then_scope].terminal_expr;
                    let else_expr = else_scope.map_or(self.tir.void_expr, |scope| self.hir.scopes[scope].terminal_expr);
                    self.insert_expr(sp, id, If { condition, then_expr, else_expr});
                },
                // Handled with ret groups
                hir::Expr::Ret { .. } => {},
                hir::Expr::Set { lhs, rhs } => self.insert_expr(sp, id, Assignment { lhs, rhs }),
                hir::Expr::Void => {},
                hir::Expr::While { condition, .. } => self.insert_expr(sp, id, While { condition }),
            }
        }

        // Build declarations
        for i in 0..self.hir.decls.len() {
            let id = DeclId::new(i);
            let level = match self.tir.decl_levels[id] {
                Level::Resolved(level, _) => level,
                _ => panic!("failed to get level"),
            };
            let sub_prog = self.tir.decl_sub_progs[id] as usize;
            // TODO: store the maximum subprogram number and move this out of the loop! (out of the expression loop above as well)
            let new_len = max(self.tir.sub_progs.len(), sub_prog + 1);
            self.tir.sub_progs.resize_with(new_len, || Subprogram::new());
            match self.hir.decls[id] {
                hir::Decl::Stored { root_expr, .. } => {
                    let explicit_ty = self.hir.explicit_tys[id].clone();
                    self.tir.sub_progs[sub_prog].assigned_decls.insert(level, AssignedDecl { explicit_ty, root_expr, decl_id: id });
                },
                hir::Decl::Computed { scope, .. } => if self.hir.explicit_tys[id].is_none() {
                    let root_expr = self.hir.scopes[scope].terminal_expr;
                    self.tir.sub_progs[sub_prog].assigned_decls.insert(level, AssignedDecl { explicit_ty: None, root_expr, decl_id: id });
                },
                hir::Decl::Const(expr) | hir::Decl::Static(expr) => {
                    let explicit_ty = self.hir.explicit_tys[id].clone();
                    self.tir.sub_progs[sub_prog].assigned_decls.insert(level, AssignedDecl { explicit_ty, root_expr: expr, decl_id: id });
                },
                hir::Decl::Parameter { .. } | hir::Decl::Intrinsic { .. } => {},
            }
        }
        let staged_ret_groups = std::mem::replace(&mut self.tir.staged_ret_groups, Vec::new());
        for (decl, ret_group) in staged_ret_groups {
            let sub_prog = self.tir.decl_sub_progs[decl] as usize;
            self.tir.sub_progs[sub_prog].ret_groups.push(ret_group);
        }
    }

    fn solve_sp_var(&mut self, sp_var: SpVarId) -> u32 {
        if let Some(value) = self.tir.sp_vars[sp_var].value {
            return value;
        }

        macro_rules! maximize {
            ($dep_kind:ident + $constraint:expr) => {{
                let mut result = 0;
                for i in 0..self.tir.sp_vars[sp_var].$dep_kind.len() {
                    let dep_var = self.tir.sp_vars[sp_var].$dep_kind[i];
                    let val = self.solve_sp_var(dep_var);
                    result = max(result, val + $constraint);
                }
                result
            }};
        }
        let weak = maximize!(weak + 0);
        let eval = maximize!(eval + 1);
        let codegen = maximize!(codegen + 0);
        let value = max(weak, max(codegen, eval));
        self.tir.sp_vars[sp_var].value = Some(value);

        value
    }

    fn pb_deps(&mut self, sp_var: SpVarId, normal: &[ExprId], weak: &[DeclId], codegen: &[ScopeId]) -> u32 {
        // IMPORTANT NOTE: the terminal expressions of the "codegen" scopes MUST be treated as normal dependencies!
        let normal = normal.iter()
            .map(|&expr| self.prebuild_expr(expr, Some(sp_var)))
            .max().unwrap_or(0);
        let weak = weak.iter()
            .map(|&decl| self.prebuild_decl(decl, Some(sp_var)))
            .max().unwrap_or(0);
        let codegen = codegen.iter()
            .map(|&scope| {
                let expr = self.prebuild_scope(scope, sp_var);
                self.tir.expr_levels[expr]
            })
            .max().unwrap_or(0);
        max(normal, max(weak, codegen)) + 1
    }

    fn new_sp_var(&mut self) -> SpVarId {
        self.tir.sp_vars.push(SubprogramVar::new())
    }

    fn prebuild_expr(&mut self, id: ExprId, sp_var: Option<SpVarId>) -> u32 {
        let sp_var = sp_var.unwrap_or_else(|| self.new_sp_var());
        self.tir.expr_sp_vars[id] = sp_var;
        let level = match self.hir.exprs[id] {
            hir::Expr::AddrOf { expr, .. } => self.pb_deps(sp_var, &[expr], &[], &[]),
            hir::Expr::Cast { expr, ty, .. } => {
                let ty_sp_var = self.new_sp_var();
                self.prebuild_expr(ty, Some(ty_sp_var));
                self.add_eval_dep(sp_var, ty_sp_var);
                self.pb_deps(sp_var, &[expr], &[], &[])
            },
            hir::Expr::CharLit { .. } | hir::Expr::DecLit { .. } |
                hir::Expr::IntLit { .. } | hir::Expr::StrLit { .. } | hir::Expr::ConstTy(_) => self.pb_deps(sp_var, &[], &[], &[]),
            hir::Expr::DeclRef { name, ref arguments, id: decl_ref_id } => {
                // TODO: Would be nice to not have to clone the arguments here. Difficult to do though because of the borrow checker.
                let arguments = arguments.clone();
                let mut overloads = Vec::new();
                if let Some(state) = self.tir.comp_decl_stack.last() {
                    if let Some(decl) = state.local_decls.iter().rev().find(|decl| decl.name == name) {
                        overloads = vec![decl.id];
                    }
                }
                if overloads.is_empty() {
                    if let Some(group) = self.tir.global_decls.iter().find(|group| group.name == name) {
                        overloads = group.decls.iter()
                            .filter_map(|decl| if decl.num_params == arguments.len() {
                                Some(decl.id)
                            } else {
                                None
                            }).collect();
                    }
                }
                let level = self.pb_deps(sp_var, &arguments[..], &overloads[..], &[]);
                self.tir.overloads[decl_ref_id] = overloads;
                level
            },
            hir::Expr::Deref(expr) => self.pb_deps(sp_var, &[expr], &[], &[]),
            hir::Expr::Pointer { expr, .. } => self.pb_deps(sp_var, &[expr], &[], &[]),
            hir::Expr::Do { scope } => self.pb_deps(sp_var, &[], &[], &[scope]),
            hir::Expr::If { condition, then_scope, else_scope } => {
                let mut scopes = ArrayVec::<[ScopeId; 2]>::new();
                scopes.push(then_scope);
                if let Some(else_scope) = else_scope { scopes.push(else_scope); }
                self.pb_deps(sp_var, &[condition], &[], &scopes[..])
            },
            hir::Expr::Ret { expr } => {
                self.tir.comp_decl_stack.last_mut()
                    .expect("Found return expression outside of comp decl")
                    .returned_expressions
                    .push(expr);
                self.pb_deps(sp_var, &[expr], &[], &[])
            },
            hir::Expr::Set { lhs, rhs } => self.pb_deps(sp_var, &[lhs, rhs], &[], &[]),
            hir::Expr::Void => 0,
            hir::Expr::While { condition, scope } => self.pb_deps(sp_var, &[condition], &[], &[scope]),
        };
        self.tir.expr_levels[id] = level;
        level
    }

    fn insert_expr<T>(&mut self, sp: u32, id: ExprId, data: T) where Expr<T>: Item {
        let level = self.tir.expr_levels[id];
        Expr { id, data }.insert(&mut self.tir, (sp, level));
    }

    fn add_weak_dep(&mut self, dependent: SpVarId, dependee: SpVarId) {
        self.tir.sp_vars[dependent].weak.push(dependee);
    }

    fn add_eval_dep(&mut self, dependent: SpVarId, dependee: SpVarId) {
        self.tir.sp_vars[dependent].eval.push(dependee);
        self.tir.sp_vars[dependee].has_eval_dependers = true;
    }

    fn add_codegen_dep(&mut self, dependent: SpVarId, dependee: SpVarId) {
        assert_ne!(dependent, dependee);
        self.tir.sp_vars[dependent].codegen.push(dependee);
    }

    /// Returns terminal expression
    ///     `dependent_sp_var`: the subprogram variable that has a codegen dependency on everything in this scope except for its terminal expression,
    ///                         on which it has a normal dependency
    fn prebuild_scope(&mut self, id: ScopeId, dependent_sp_var: SpVarId) -> ExprId {
        self.tir.comp_decl_stack.last_mut().unwrap().open_scope();

        for i in 0..self.hir.scopes[id].items.len() {
            match self.hir.scopes[id].items[i] {
                hir::Item::Stmt(expr) => {
                    let sp_var = self.new_sp_var();
                    self.prebuild_expr(expr, Some(sp_var));
                    self.add_codegen_dep(dependent_sp_var, sp_var);
                },
                hir::Item::StoredDecl { decl_id, root_expr, .. } => {
                    let sp_var = self.new_sp_var();
                    self.tir.decl_sp_vars[decl_id] = sp_var;
                    let level = self.prebuild_expr(root_expr, Some(sp_var)) + 1;
                    self.add_codegen_dep(dependent_sp_var, sp_var);

                    if let Some(explicit_ty) = self.hir.explicit_tys[decl_id] {
                        let ty_sp_var = self.new_sp_var();
                        self.prebuild_expr(explicit_ty, Some(ty_sp_var));
                        self.add_eval_dep(sp_var, ty_sp_var);
                    }

                    let name = self.hir.names[decl_id];
                    self.tir.comp_decl_stack.last_mut().unwrap().local_decls.push(LocalDecl { name, id: decl_id });

                    self.tir.decl_levels[decl_id] = Level::Resolved(level, sp_var);
                },
                hir::Item::ComputedDecl(decl_id) => {
                    self.prebuild_decl(decl_id, None);

                    let name = self.hir.names[decl_id];
                    self.tir.comp_decl_stack.last_mut().unwrap().local_decls.push(LocalDecl { name, id: decl_id });
                },
            }
        }
        self.prebuild_expr(self.hir.scopes[id].terminal_expr, Some(dependent_sp_var));

        self.tir.comp_decl_stack.last_mut().unwrap().close_scope();

        self.hir.scopes[id].terminal_expr
    }

    ///     `dependent_sp_var`: the subprogram variable which has a weak normal dependency on this declaration.
    ///                         can be different each call
    fn prebuild_decl(&mut self, id: DeclId, dependent_sp_var: Option<SpVarId>) -> u32 {
        let (sp_var, resolved_level) = match self.tir.decl_levels[id] {
            Level::Unresolved => {
                self.tir.decl_levels[id] = Level::Resolving;
                (self.new_sp_var(), None)
            },
            Level::Resolving => panic!("Cycle detected on decl {}!", self.interner.resolve(self.hir.names[id]).unwrap()),
            Level::Resolved(level, sp_var) => (sp_var, Some(level)),
        };
        if let Some(dependent_sp_var) = dependent_sp_var {
            if self.hir.explicit_tys[id].is_some() {
                // If there's an explicit type, add a codegen dependency because level doesn't matter
                self.add_codegen_dep(dependent_sp_var, sp_var);
            } else {
                // Otherwise add a weak dependency, because level does matter
                self.add_weak_dep(dependent_sp_var, sp_var);
            }
        }

        self.tir.decl_sp_vars[id] = sp_var;
        if let Some(explicit_ty) = self.hir.explicit_tys[id] {
            let ty_sp_var = self.new_sp_var();
            self.prebuild_expr(explicit_ty, Some(ty_sp_var));
            self.add_eval_dep(sp_var, ty_sp_var);
        }

        if let Some(level) = resolved_level { return level; }
        let level = match self.hir.decls[id] {
            hir::Decl::Computed { ref param_tys, ref params, scope } => {
                // Resolve computed decls with explicit tys before prebuilding their scopes
                if self.hir.explicit_tys[id].is_some() {
                    self.tir.decl_levels[id] = Level::Resolved(0, sp_var);
                }

                let mut comp_decl_state = CompDeclState::new();
                // Add function name to local scope to enable recursion
                let name = self.hir.names[id];
                comp_decl_state.local_decls.push(LocalDecl { name, id });
                comp_decl_state.local_decls.reserve(params.end.idx() - params.start.idx());
                for id in params.start.idx()..params.end.idx() {
                    let id = DeclId::new(id);
                    let name = self.hir.names[id];
                    comp_decl_state.local_decls.push(LocalDecl { name, id });

                    // Set the sp var for the param decl to be the same as the parent decl
                    self.tir.decl_sp_vars[id] = sp_var;
                }
                let param_tys = param_tys.clone();
                let params = params.clone();
                for (&ty, id) in param_tys.iter().zip(params.start.idx()..params.end.idx()) {
                    let id = DeclId::new(id);
                    let ty_sp_var = self.new_sp_var();
                    self.prebuild_expr(ty, Some(ty_sp_var));
                    self.tir.decl_levels[id] = Level::Resolved(0, sp_var);
                    self.add_eval_dep(sp_var, ty_sp_var);
                }
                self.tir.comp_decl_stack.push(comp_decl_state);

                let mut level = self.pb_deps(sp_var, &[], &[], &[scope]);
                let mut exprs = self.tir.comp_decl_stack.pop().unwrap().returned_expressions;
                let terminal_expr = self.hir.scopes[scope].terminal_expr;
                exprs.push(terminal_expr);
                match &self.hir.explicit_tys[id] {
                    Some(ty) => {
                        self.tir.staged_ret_groups.push((id, RetGroup { ty: ty.clone(), exprs }));
                        level = 0;
                    },
                    None => assert_eq!(exprs.len(), 1, "multiple returns from assigned functions not allowed"),
                };

                level
            },
            hir::Decl::Const(expr) | hir::Decl::Static(expr) => self.pb_deps(sp_var, &[expr], &[], &[]),
            hir::Decl::Intrinsic { ref param_tys, .. } => {
                let param_tys = param_tys.clone();
                for ty in param_tys {
                    let ty_sp_var = self.new_sp_var();
                    self.prebuild_expr(ty, Some(ty_sp_var));
                    self.add_eval_dep(sp_var, ty_sp_var);
                }
                0
            },
            hir::Decl::Parameter { .. } => panic!("prebuilding parameter should've been handled elsewhere!"),
            hir::Decl::Stored { .. } => panic!("Should've already been handled in prebuild_scope"),
        };
        self.tir.decl_levels[id] = Level::Resolved(level, sp_var);
        level
    }
}


pub trait Item: Sized {
    fn insert(self, builder: &mut Builder, level: (u32, u32));
}

macro_rules! item_impl {
    ($ty:ty, $storage:ident) => {
        impl Item for Expr<$ty> {
            fn insert(self, builder: &mut Builder, (sub_prog, level): (u32, u32)) { builder.sub_progs[sub_prog as usize].$storage.insert(level, self); }
        }
    }
}

macro_rules! item_vec_impl {
    ($ty:ty, $storage:ident) => {
        impl Item for Expr<$ty> {
            fn insert(self, builder: &mut Builder, (sub_prog, _): (u32, u32)) { builder.sub_progs[sub_prog as usize].$storage.push(self); }
        }
    }
}

item_vec_impl!(Cast, casts);
item_vec_impl!(While, whiles);

item_impl!(Do, dos);
item_impl!(Assignment, assignments);
item_impl!(AddrOf, addr_ofs);
item_impl!(Dereference, derefs);
item_impl!(Pointer, pointers);
item_impl!(If, ifs);
item_impl!(DeclRef, decl_refs);