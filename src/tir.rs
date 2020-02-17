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

        Self {
            sub_progs: Vec::new(),
            void_expr,
            num_casts: 0,
            expr_levels,
            decl_levels: IdxVec::new(),

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
        let mut graph = self.create_graph();
        
        // Add type 1 dependencies to the graph
        for i in 0..self.hir.decls.len() {
            let id = DeclId::new(i);
            match self.hir.decls[id] {
                hir::Decl::Parameter { .. } | hir::Decl::Intrinsic { .. } => {},
                hir::Decl::Static(assigned_expr) | hir::Decl::Const(assigned_expr) => graph.add_type1_dep(id, assigned_expr),
                hir::Decl::Computed { scope, .. } => {
                    let terminal_expr = self.hir.scopes[scope].terminal_expr;
                    graph.add_type1_dep(id, terminal_expr);
                },
                hir::Decl::Stored { root_expr, .. } => {
                    graph.add_type1_dep(id, root_expr);
                },
            }
        }
        for i in 0..self.hir.exprs.len() {
            let id = ExprId::new(i);
            match self.hir.exprs[id] {
                hir::Expr::Void | hir::Expr::IntLit { .. } | hir::Expr::DecLit { .. } | hir::Expr::StrLit { .. }
                    | hir::Expr::CharLit { .. } | hir::Expr::ConstTy(_) => {},
                hir::Expr::AddrOf { expr, .. } | hir::Expr::Deref(expr) | hir::Expr::Pointer { expr, .. } | hir::Expr::Cast { expr, .. }
                    | hir::Expr::Ret { expr } => graph.add_type1_dep(id, expr),

                hir::Expr::DeclRef { ref arguments, .. } => {
                    for &arg in arguments {
                        graph.add_type1_dep(id, arg);
                    }
                },
                hir::Expr::Set { lhs, rhs } => {
                    graph.add_type1_dep(id, lhs);
                    graph.add_type1_dep(id, rhs);
                },
                hir::Expr::Do { scope } => {
                    let terminal_expr = self.hir.scopes[scope].terminal_expr;
                    graph.add_type1_dep(id, terminal_expr);
                },
                hir::Expr::If { condition, then_scope, else_scope } => {
                    graph.add_type1_dep(id, condition);

                    let then_expr = self.hir.scopes[then_scope].terminal_expr;
                    let else_expr = if let Some(else_scope) = else_scope {
                        self.hir.scopes[else_scope].terminal_expr
                    } else {
                        self.hir.void_expr
                    };
                    graph.add_type1_dep(id, then_expr);
                    graph.add_type1_dep(id, else_expr);
                }
                hir::Expr::While { condition, scope } => {
                    graph.add_type1_dep(id, condition);
                    let terminal_expr = self.hir.scopes[scope].terminal_expr;
                    graph.add_type1_dep(id, terminal_expr);
                },
            }
        }

        self.print_graph(&graph).unwrap();
    }
}