use std::cmp::max;
use std::ops::{Deref, DerefMut};

use arrayvec::ArrayVec;
use smallvec::SmallVec;
use string_interner::Sym;

use crate::driver::Driver;
use crate::builder::*;
use crate::dep_vec::DepVec;
use crate::hir::{self, Namespace};
use crate::index_vec::{Idx, IdxVec};

mod graph;
use graph::Graph;

newtype_index!(TreeId pub);
newtype_index!(RetGroupId);

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
pub struct Builder {
    pub sub_progs: Vec<Subprogram>,
    /// An expression to uniquely represent the void value
    pub void_expr: ExprId,
    pub num_casts: usize,
    pub decls: IdxVec<Decl, DeclId>,
    /// Each declref's overload choices
    pub overloads: IdxVec<Vec<DeclId>, DeclRefId>,

    expr_levels: IdxVec<u32, ExprId>,
    decl_levels: IdxVec<u32, DeclId>,

    global_decls: Vec<GlobalDeclGroup>,

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

impl Builder {
    pub fn new() -> Self {
        // Create the void expression
        let mut expr_levels = IdxVec::new();
        let void_expr = expr_levels.push(0);

        Self {
            sub_progs: Vec::new(),
            void_expr,
            num_casts: 0,
            expr_levels,
            decl_levels: IdxVec::new(),

            decls: IdxVec::new(),
            global_decls: Vec::new(),
            overloads: IdxVec::new(),

            staged_ret_groups: Vec::new(),
        }
    }

    pub fn num_exprs(&self) -> usize {
        self.expr_levels.len()
    }
}

impl Driver {
    fn find_overloads(&self, decl_ref: &hir::DeclRef) -> Vec<DeclId> {
        let mut overloads = Vec::new();

        if let Some(mut namespace) = decl_ref.namespace {
            let mut scope = &self.hir.decl_scopes[namespace.scope];
            loop {
                let result = scope.decls[0..namespace.end_offset].iter()
                    .rev()
                    .find(|&decl| decl.name == decl_ref.name && decl.num_params == decl_ref.num_arguments);
                if let Some(decl) = result {
                    overloads.push(decl.id);
                    return overloads;
                }
                if let Some(parent) = scope.parent {
                    namespace = parent;
                    scope = &self.hir.decl_scopes[namespace.scope];
                } else {
                    break;
                }
            }
        }

        if let Some(group) = self.tir.global_decls.iter().find(|group| group.name == decl_ref.name) {
            overloads.extend(group.decls.iter().map(|decl| decl.id));
        }

        overloads
    }

    fn add_type3_scope_dep(&self, graph: &mut Graph, a: impl graph::Item, b: ScopeId) {
        let scope = &self.hir.scopes[b];
        for &item in &scope.items {
            match item {
                hir::Item::Stmt(expr) => graph.add_type3_dep(a, expr),
                hir::Item::StoredDecl { decl_id, root_expr, .. } => graph.add_type3_dep(a, decl_id),
                hir::Item::ComputedDecl(_) => {},
            }
        }
    }

    pub fn build_tir(&mut self) {
        // Populate `decls`
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
        }

        // Populate `global_decls`
        for &id in &self.hir.global_decls {
            let name = self.hir.names[id];
            let num_params = self.tir.decls[id].param_tys.len();
            let group = match self.tir.global_decls.iter_mut().find(|group| group.name == name) {
                Some(group) => group,
                None => {
                    self.tir.global_decls.push(GlobalDeclGroup { name, decls: Vec::new() });
                    self.tir.global_decls.last_mut().unwrap()
                },
            };
            group.decls.push(GlobalDecl { id, num_params });
        }

        debug_assert!(self.tir.overloads.is_empty());
        self.tir.overloads.reserve(self.hir.decl_refs.len());
        for i in 0..self.hir.decl_refs.len() {
            let id = DeclRefId::new(i);
            let decl_ref = &self.hir.decl_refs[id];
            let overloads = self.find_overloads(decl_ref);
            self.tir.overloads.push(overloads);
        }

        // Add dependencies to the graph
        let mut graph = self.create_graph();
        for i in 0..self.hir.decls.len() {
            let id = DeclId::new(i);
            match self.hir.decls[id] {
                hir::Decl::Parameter { .. } => {},
                hir::Decl::Intrinsic { ref param_tys, .. } => {
                    for &ty in param_tys {
                        graph.add_type4_dep(id, ty);
                    }
                },
                hir::Decl::Static(assigned_expr) | hir::Decl::Const(assigned_expr) => graph.add_type1_dep(id, assigned_expr),
                hir::Decl::Computed { scope, ref param_tys, .. } => {
                    self.add_type3_scope_dep(&mut graph, id, scope);
                    let terminal_expr = self.hir.scopes[scope].terminal_expr;
                    graph.add_type1_dep(id, terminal_expr);
                    for &ty in param_tys {
                        graph.add_type4_dep(id, ty);
                    }
                    if self.hir.explicit_tys[id].is_none() {
                        graph.add_type4_dep(id, self.hir.void_ty);
                    }
                },
                hir::Decl::Stored { root_expr, .. } => {
                    graph.add_type1_dep(id, root_expr);
                },
            }
            if let Some(ty) = self.hir.explicit_tys[id] {
                graph.add_type4_dep(id, ty);
            }
        }
        for i in 0..self.hir.exprs.len() {
            let id = ExprId::new(i);
            match self.hir.exprs[id] {
                hir::Expr::Void | hir::Expr::IntLit { .. } | hir::Expr::DecLit { .. } | hir::Expr::StrLit { .. }
                    | hir::Expr::CharLit { .. } | hir::Expr::ConstTy(_) => {},
                hir::Expr::AddrOf { expr, .. } | hir::Expr::Deref(expr) | hir::Expr::Pointer { expr, .. }
                    => graph.add_type1_dep(id, expr),
                hir::Expr::Cast { expr, ty, .. } => {
                    graph.add_type1_dep(id, expr);
                    graph.add_type4_dep(id, ty);
                },
                hir::Expr::Ret { expr, decl } => {
                    graph.add_type1_dep(id, expr);
                    let ty = decl
                        .and_then(|decl| self.hir.explicit_tys[decl])
                        .unwrap_or(self.hir.void_ty);
                    graph.add_type4_dep(id, ty);
                }
                hir::Expr::DeclRef { ref arguments, id: decl_ref_id } => {
                    for &overload in &self.tir.overloads[decl_ref_id] {
                        match self.hir.decls[overload] {
                            hir::Decl::Computed { ref param_tys, .. } => {
                                let ty = self.hir.explicit_tys[overload].unwrap_or(self.hir.void_ty);
                                graph.add_type4_dep(id, ty);
                                for &ty in param_tys {
                                    graph.add_type4_dep(id, ty);
                                }
                                graph.add_type3_dep(id, overload);
                            },
                            _ => {
                                println!("before! {}", self.interner.resolve(self.hir.names[overload]).unwrap());
                                graph.add_type2_dep(id, overload);
                                println!("after! {}", self.interner.resolve(self.hir.names[overload]).unwrap());
                            },
                        }
                    }
                    for &arg in arguments {
                        graph.add_type1_dep(id, arg);
                    }
                },
                hir::Expr::Set { lhs, rhs } => {
                    graph.add_type1_dep(id, lhs);
                    graph.add_type1_dep(id, rhs);
                },
                hir::Expr::Do { scope } => {
                    self.add_type3_scope_dep(&mut graph, id, scope);
                    let terminal_expr = self.hir.scopes[scope].terminal_expr;
                    graph.add_type1_dep(id, terminal_expr);
                },
                hir::Expr::If { condition, then_scope, else_scope } => {
                    graph.add_type1_dep(id, condition);

                    self.add_type3_scope_dep(&mut graph, id, then_scope);
                    let then_expr = self.hir.scopes[then_scope].terminal_expr;
                    let else_expr = if let Some(else_scope) = else_scope {
                        self.add_type3_scope_dep(&mut graph, id, else_scope);
                        self.hir.scopes[else_scope].terminal_expr
                    } else {
                        self.hir.void_expr
                    };
                    graph.add_type1_dep(id, then_expr);
                    graph.add_type1_dep(id, else_expr);
                }
                hir::Expr::While { condition, scope } => {
                    graph.add_type1_dep(id, condition);
                    self.add_type3_scope_dep(&mut graph, id, scope);
                    let terminal_expr = self.hir.scopes[scope].terminal_expr;
                    graph.add_type1_dep(id, terminal_expr);
                },
            }
        }
        graph.split();
        graph.find_units();
        self.print_graph(&graph).unwrap();
    }
}