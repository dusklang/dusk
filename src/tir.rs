use std::ops::{Deref, DerefMut};
use std::collections::HashMap;

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
pub struct Unit {
    pub int_lits: Vec<ExprId>,
    pub dec_lits: Vec<ExprId>,
    pub str_lits: Vec<ExprId>,
    pub char_lits: Vec<ExprId>,
    pub const_tys: Vec<ExprId>,
    pub stmts: Vec<Stmt>,
    pub explicit_rets: Vec<ExprId>,
    pub ret_groups: Vec<RetGroup>,
    pub casts: Vec<Expr<Cast>>,
    pub whiles: Vec<Expr<While>>,
    pub modules: Vec<ExprId>,
    pub dos: DepVec<Expr<Do>>,
    pub assigned_decls: DepVec<AssignedDecl>,
    pub assignments: DepVec<Expr<Assignment>>,
    pub decl_refs: DepVec<Expr<DeclRef>>,
    pub addr_ofs: DepVec<Expr<AddrOf>>,
    pub derefs: DepVec<Expr<Dereference>>,
    pub pointers: DepVec<Expr<Pointer>>,
    pub ifs: DepVec<Expr<If>>,

    /// The expressions in this unit that later units have eval dependencies on
    pub eval_dependees: Vec<ExprId>,
}

impl Unit {
    fn new() -> Self {
        Self {
            int_lits: Vec::new(),
            dec_lits: Vec::new(),
            str_lits: Vec::new(),
            char_lits: Vec::new(),
            const_tys: Vec::new(),
            stmts: Vec::new(),
            explicit_rets: Vec::new(),
            ret_groups: Vec::new(),
            casts: Vec::new(),
            whiles: Vec::new(),
            modules: Vec::new(),
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
}

#[derive(Debug)]
pub struct Builder {
    pub units: Vec<Unit>,
    pub decls: IdxVec<Decl, DeclId>,
    /// Each declref's overload choices
    pub overloads: IdxVec<Vec<DeclId>, DeclRefId>,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            units: Vec::new(),
            decls: IdxVec::new(),
            overloads: IdxVec::new(),
        }
    }
}

impl Driver {
    // Returns whether or not we found the last/only overload
    fn find_overloads_from_namespace(&self, ns: Namespace, decl_ref: &hir::DeclRef, last_was_imperative: bool, overloads: &mut Vec<DeclId>) -> bool {
        match ns {
            Namespace::Imper { scope, end_offset } => {
                if !last_was_imperative { return true; }

                let namespace = &self.hir.imper_ns[scope];
                let result = namespace.decls[0..end_offset].iter()
                    .rev()
                    .find(|&decl| decl.name == decl_ref.name && decl.num_params == decl_ref.num_arguments);
                if let Some(decl) = result {
                    overloads.push(decl.id);
                    return true;
                }
            },
            Namespace::Mod(scope) => {
                let scope = self.hir.mod_ns[scope].scope;
                if let Some(group) = self.hir.mod_scopes[scope].decl_groups.get(&decl_ref.name) {
                    overloads.extend(
                        group.iter()
                            .filter(|decl| decl.num_params == decl_ref.num_arguments)
                            .map(|decl| decl.id)
                    );
                }
            }
        }
        false
    }

    fn find_overloads(&self, decl_ref: &hir::DeclRef) -> Vec<DeclId> {
        let mut overloads = Vec::new();

        let mut last_was_imperative = true;
        let mut namespace = Some(decl_ref.namespace);
        while let Some(ns) = namespace {
            if self.find_overloads_from_namespace(ns, decl_ref, last_was_imperative, &mut overloads) {
                break;
            }
            namespace = match ns {
                Namespace::Imper { scope, .. } => {
                    last_was_imperative = true;
                    self.hir.imper_ns[scope].parent
                },
                Namespace::Mod(scope) => {
                    last_was_imperative = false;
                    self.hir.mod_ns[scope].parent
                },
            };
        }

        overloads
    }

    fn add_type3_scope_dep(&self, graph: &mut Graph, a: ItemId, b: ImperScopeId) {
        let scope = &self.hir.imper_scopes[b];
        for &item in &scope.items {
            match item {
                hir::ScopeItem::Stmt(expr) => graph.add_type3_dep(a, self.hir.expr_to_items[expr]),
                hir::ScopeItem::StoredDecl { decl_id, root_expr, .. } => graph.add_type3_dep(a, self.hir.decl_to_items[decl_id]),
                hir::ScopeItem::ComputedDecl(_) => {},
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
        // TODO: do something better than an array of bools :(
        let mut depended_on = IdxVec::<bool, ExprId>::new();
        depended_on.resize_with(self.hir.exprs.len(), || false);
        macro_rules! ei {
            ($a:expr) => { self.hir.expr_to_items[$a] }
        }
        macro_rules! di {
            ($a:expr) => { self.hir.decl_to_items[$a] }
        }
        macro_rules! add_eval_dep {
            ($a:expr, $b:expr) => {{
                graph.add_type4_dep($a, ei!($b));
                depended_on[$b] = true;
            }}
        }
        for i in 0..self.hir.decls.len() {
            let decl_id = DeclId::new(i);
            let id = di!(decl_id);
            match self.hir.decls[decl_id] {
                hir::Decl::Parameter { .. } => {},
                hir::Decl::Intrinsic { ref param_tys, .. } => {
                    for &ty in param_tys {
                        add_eval_dep!(id, ty);
                    }
                },
                hir::Decl::Static(assigned_expr) | hir::Decl::Const(assigned_expr) => graph.add_type1_dep(id, ei!(assigned_expr)),
                hir::Decl::Computed { scope, ref param_tys, .. } => {
                    self.add_type3_scope_dep(&mut graph, id, scope);
                    let terminal_expr = self.hir.imper_scopes[scope].terminal_expr;
                    graph.add_type1_dep(id, ei!(terminal_expr));
                    for &ty in param_tys {
                        add_eval_dep!(id, ty);
                    }
                    let ty = self.hir.explicit_tys[decl_id].unwrap_or(self.hir.void_ty);
                    add_eval_dep!(id, ty);
                },
                hir::Decl::Stored { root_expr, .. } => {
                    graph.add_type1_dep(id, ei!(root_expr));
                },
            }
            if let Some(ty) = self.hir.explicit_tys[decl_id] {
                add_eval_dep!(id, ty);
            }
        }
        for i in 0..self.hir.exprs.len() {
            let expr_id = ExprId::new(i);
            let id = ei!(expr_id);
            match self.hir.exprs[expr_id] {
                hir::Expr::Void | hir::Expr::IntLit { .. } | hir::Expr::DecLit { .. } | hir::Expr::StrLit { .. }
                    | hir::Expr::CharLit { .. } | hir::Expr::ConstTy(_) => {},
                hir::Expr::AddrOf { expr, .. } | hir::Expr::Deref(expr) | hir::Expr::Pointer { expr, .. }
                    => graph.add_type1_dep(id, ei!(expr)),
                hir::Expr::Cast { expr, ty, .. } => {
                    graph.add_type1_dep(id, ei!(expr));
                    add_eval_dep!(id, ty);
                },
                hir::Expr::Ret { expr, decl } => {
                    graph.add_type1_dep(id, ei!(expr));
                    let ty = decl
                        .and_then(|decl| self.hir.explicit_tys[decl])
                        .unwrap_or(self.hir.void_ty);
                    add_eval_dep!(id, ty);
                }
                hir::Expr::DeclRef { ref arguments, id: decl_ref_id } => {
                    for &overload in &self.tir.overloads[decl_ref_id] {
                        match self.hir.decls[overload] {
                            hir::Decl::Computed { ref param_tys, .. } => {
                                let ty = self.hir.explicit_tys[overload].unwrap_or(self.hir.void_ty);
                                add_eval_dep!(id, ty);
                                for &ty in param_tys {
                                    add_eval_dep!(id, ty);
                                }
                                graph.add_type3_dep(id, di!(overload));
                            },
                            _ => graph.add_type2_dep(id, di!(overload)),
                        }
                    }
                    for &arg in arguments {
                        graph.add_type1_dep(id, ei!(arg));
                    }
                },
                hir::Expr::Set { lhs, rhs } => {
                    graph.add_type1_dep(id, ei!(lhs));
                    graph.add_type1_dep(id, ei!(rhs));
                },
                hir::Expr::Do { scope } => {
                    self.add_type3_scope_dep(&mut graph, id, scope);
                    let terminal_expr = self.hir.imper_scopes[scope].terminal_expr;
                    graph.add_type1_dep(id, ei!(terminal_expr));
                },
                hir::Expr::If { condition, then_scope, else_scope } => {
                    graph.add_type1_dep(id, ei!(condition));

                    self.add_type3_scope_dep(&mut graph, id, then_scope);
                    let then_expr = self.hir.imper_scopes[then_scope].terminal_expr;
                    let else_expr = if let Some(else_scope) = else_scope {
                        self.add_type3_scope_dep(&mut graph, id, else_scope);
                        self.hir.imper_scopes[else_scope].terminal_expr
                    } else {
                        self.hir.void_expr
                    };
                    graph.add_type1_dep(id, ei!(then_expr));
                    graph.add_type1_dep(id, ei!(else_expr));
                }
                hir::Expr::While { condition, scope } => {
                    graph.add_type1_dep(id, ei!(condition));
                    self.add_type3_scope_dep(&mut graph, id, scope);
                    let terminal_expr = self.hir.imper_scopes[scope].terminal_expr;
                    graph.add_type1_dep(id, ei!(terminal_expr));
                },
                hir::Expr::Mod { id: mod_id } => {
                    for decl_group in self.hir.mod_scopes[mod_id].decl_groups.values() {
                        for decl in decl_group {
                            graph.add_type4_dep(id, di!(decl.id));
                        }
                    }
                }
            }
        }

        // Solve for the unit and level of each item
        graph.split();
        graph.find_units();
        let levels = graph.solve();
        self.tir.units.resize_with(levels.num_units as usize, || Unit::new());

        let mut staged_ret_groups = HashMap::<DeclId, SmallVec<[ExprId; 1]>>::new();

        // Finally, convert HIR items to TIR and add them to the correct spot
        for (i, expr) in self.hir.exprs.iter().enumerate() {
            let id = ExprId::new(i);
            let item_id = ei!(id);
            let level = levels.levels[item_id];
            let unit = levels.units[item_id];
            let unit = &mut self.tir.units[unit as usize];
            if depended_on[id] { unit.eval_dependees.push(id); }

            macro_rules! insert_item {
                ($depvec:ident, $item:expr) => {{
                    unit.$depvec.insert(
                        level,
                        Expr { id, data: $item }
                    );
                }}
            };

            match expr {
                hir::Expr::Void => {},
                hir::Expr::IntLit { .. } => unit.int_lits.push(id),
                hir::Expr::DecLit { .. } => unit.dec_lits.push(id),
                hir::Expr::StrLit { .. } => unit.str_lits.push(id),
                hir::Expr::CharLit { .. } => unit.char_lits.push(id),
                hir::Expr::ConstTy(_) => unit.const_tys.push(id),
                &hir::Expr::AddrOf { expr, is_mut } => insert_item!(addr_ofs, AddrOf { expr, is_mut }),
                &hir::Expr::Deref(expr) => insert_item!(derefs, Dereference { expr }),
                &hir::Expr::Pointer { expr, .. } => insert_item!(pointers, Pointer { expr }),
                &hir::Expr::Cast { expr, ty, cast_id } => unit.casts.push(Expr { id, data: Cast { expr, ty, cast_id } }),
                &hir::Expr::Ret { expr, decl } => {
                    let decl = decl.expect("returning outside of a computed decl is invalid!");
                    staged_ret_groups.entry(decl).or_default().push(expr);
                    unit.explicit_rets.push(expr);
                }
                &hir::Expr::DeclRef { ref arguments, id: decl_ref_id } => insert_item!(decl_refs, DeclRef { args: arguments.clone(), decl_ref_id }),
                &hir::Expr::Set { lhs, rhs } => insert_item!(assignments, Assignment { lhs, rhs }),
                &hir::Expr::Do { scope } => insert_item!(dos, Do { terminal_expr: self.hir.imper_scopes[scope].terminal_expr }),
                &hir::Expr::If { condition, then_scope, else_scope } => {
                    let then_expr = self.hir.imper_scopes[then_scope].terminal_expr;
                    let else_expr = if let Some(else_scope) = else_scope {
                        self.hir.imper_scopes[else_scope].terminal_expr
                    } else {
                        self.hir.void_expr
                    };
                    insert_item!(ifs, If { condition, then_expr, else_expr });
                },
                &hir::Expr::While { condition, .. } => unit.whiles.push(Expr { id, data: While { condition } }),
                hir::Expr::Mod { .. } => unit.modules.push(id),
            }
        }
        for (i, decl) in self.hir.decls.iter().enumerate() {
            let id = DeclId::new(i);
            let item_id = di!(id);
            let level = levels.levels[item_id];
            let unit = levels.units[item_id];
            let unit = &mut self.tir.units[unit as usize];

            match decl {
                // TODO: Add a parameter TIR item for (at least) checking that the type of the param is valid
                hir::Decl::Parameter { .. } => {},
                hir::Decl::Intrinsic { ref param_tys, .. } => {},
                &hir::Decl::Static(root_expr) | &hir::Decl::Const(root_expr) | &hir::Decl::Stored { root_expr, .. } => {
                    let explicit_ty = self.hir.explicit_tys[id];
                    unit.assigned_decls.insert(level, AssignedDecl { explicit_ty, root_expr, decl_id: id });
                },
                &hir::Decl::Computed { scope, ref param_tys, .. } => {
                    let terminal_expr = self.hir.imper_scopes[scope].terminal_expr;
                    let mut exprs = staged_ret_groups.remove(&id).unwrap_or_default();
                    exprs.push(terminal_expr);
                    if let Some(ty) = self.hir.explicit_tys[id] {
                        unit.ret_groups.push(
                            RetGroup { ty, exprs }
                        );
                    } else {
                        assert_eq!(exprs.len(), 1, "explicit return statements are not allowed in assigned functions (yet?)");
                        unit.assigned_decls.insert(level, AssignedDecl { explicit_ty: None, root_expr: terminal_expr, decl_id: id });
                    }
                },
            }
        }
        for scope in &self.hir.imper_scopes {
            for &item in &scope.items {
                match item {
                    hir::ScopeItem::Stmt(expr) => {
                        let unit = levels.units[ei!(expr)];
                        let unit = &mut self.tir.units[unit as usize];
                        unit.stmts.push(Stmt { root_expr: expr });
                    },
                    hir::ScopeItem::ComputedDecl(_) | hir::ScopeItem::StoredDecl { .. } => {},
                }
            }
        }
    }
}