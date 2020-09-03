use std::ops::{Deref, DerefMut};
use std::collections::{HashMap, HashSet};

use smallvec::SmallVec;

use crate::driver::Driver;
use crate::builder::*;
use crate::dep_vec::{self, DepVec};
use crate::hir::{self, Namespace};
use crate::index_vec::{Idx, IdxVec};

mod graph;
use graph::{Graph, Levels};

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

struct Subprogram {
    units: Vec<Unit>,
    levels: Levels,
}

#[derive(Default, Debug)]
pub struct UnitItems {
    pub int_lits: Vec<ExprId>,
    pub dec_lits: Vec<ExprId>,
    pub str_lits: Vec<ExprId>,
    pub char_lits: Vec<ExprId>,
    pub const_tys: Vec<ExprId>,
    pub stmts: Vec<Stmt>,
    pub explicit_rets: DepVec<ExprId>,
    pub ret_groups: DepVec<RetGroup>,
    pub casts: DepVec<Expr<Cast>>,
    pub whiles: DepVec<Expr<While>>,
    pub modules: DepVec<ExprId>,
    pub dos: DepVec<Expr<Do>>,
    pub assigned_decls: DepVec<AssignedDecl>,
    pub assignments: DepVec<Expr<Assignment>>,
    pub decl_refs: DepVec<Expr<DeclRef>>,
    pub addr_ofs: DepVec<Expr<AddrOf>>,
    pub derefs: DepVec<Expr<Dereference>>,
    pub pointers: DepVec<Expr<Pointer>>,
    pub ifs: DepVec<Expr<If>>,
}

impl UnitItems {
    fn unify_sizes(&mut self) {
        dep_vec::unify_sizes(&mut [
            &mut self.assigned_decls, &mut self.assignments, &mut self.decl_refs, 
            &mut self.addr_ofs, &mut self.derefs, &mut self.pointers, &mut self.ifs,
            &mut self.dos, &mut self.ret_groups, &mut self.casts, &mut self.whiles,
            &mut self.explicit_rets, &mut self.modules,
        ]);
    }
}

#[derive(Debug)]
pub struct MetaDependee {
    pub dependee: ExprId,
    pub items: UnitItems,
}

#[derive(Debug)]
pub struct LevelMetaDependees {
    pub level: u32,
    pub meta_dependees: Vec<MetaDependee>,
}

#[derive(Debug, Default)]
pub struct Unit {
    pub items: UnitItems,

    /// The expressions in this unit that later units have eval dependencies on
    pub eval_dependees: Vec<ExprId>,

    pub meta_dependees: Vec<LevelMetaDependees>,
}

/// The namespace "inside" an expression,
/// i.e. what are all the declarations that you might be able to access as members of an expression
#[derive(Debug)]
pub enum ExprNamespace {
    Mod(ModScopeId)
}

#[derive(Debug)]
pub struct Builder {
    pub decls: IdxVec<Decl, DeclId>,
    pub expr_namespaces: HashMap<ExprId, Vec<ExprNamespace>>,
    graph: Graph,
    depended_on: IdxVec<bool, ExprId>,

    staged_ret_groups: HashMap<DeclId, SmallVec<[ExprId; 1]>>,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            decls: IdxVec::new(),
            expr_namespaces: HashMap::new(),
            graph: Graph::default(),
            depended_on: IdxVec::new(),
            staged_ret_groups: HashMap::new(),
        }
    }
}

macro_rules! ei_injector {
    ($self:expr, $name:ident) => { 
        macro_rules! $name {
            ($a: expr) => { $self.hir.expr_to_items[$a] }
        }
    }
}
macro_rules! di_injector {
    ($self:expr, $name:ident) => { 
        macro_rules! $name {
            ($a: expr) => { $self.hir.decl_to_items[$a] }
        }
    }
}
macro_rules! add_eval_dep_injector {
    ($self:expr, $name: ident) => {
        macro_rules! $name {
            ($a:expr, $b:expr) => {{
                ei_injector!($self, ei_inner);
                $self.tir.graph.add_type4_dep($a, ei_inner!($b));
                $self.tir.depended_on[$b] = true;
            }}
        }
    }
}

impl Driver {
    fn find_overloads_in_mod(&self, decl_ref: &hir::DeclRef, scope: ModScopeId, overloads: &mut HashSet<DeclId>) {
        if let Some(group) = self.hir.mod_scopes[scope].decl_groups.get(&decl_ref.name) {
            overloads.extend(
                group.iter()
                    .filter(|decl| decl.num_params == decl_ref.num_arguments)
                    .map(|decl| decl.id)
            );
        }
    }
    // Returns the overloads for a declref, if they are known (they won't be if it's a member ref)
    pub fn find_overloads(&self, decl_ref: &hir::DeclRef) -> Vec<DeclId> {
        let mut overloads = HashSet::new();

        let mut started_at_mod_scope = false;
        let mut root_namespace = true;
        let mut namespace = Some(decl_ref.namespace);
        while let Some(ns) = namespace {
            namespace = match ns {
                Namespace::Imper { scope, end_offset } => {
                    if !started_at_mod_scope {
                        let namespace = &self.hir.imper_ns[scope];
                        let result = namespace.decls[0..end_offset].iter()
                            .rev()
                            .find(|&decl| decl.name == decl_ref.name && decl.num_params == decl_ref.num_arguments);
                        if let Some(decl) = result {
                            overloads.insert(decl.id);
                            break;
                        }
                    }

                    self.hir.imper_ns[scope].parent
                },
                Namespace::Mod(scope_ns) => {
                    let scope = self.hir.mod_ns[scope_ns].scope;
                    self.find_overloads_in_mod(decl_ref, scope, &mut overloads);

                    if root_namespace { started_at_mod_scope = true; }
                    self.hir.mod_ns[scope_ns].parent
                },
                Namespace::MemberRef { base_expr } => {
                    assert!(root_namespace, "member refs currently must be at the root of a namespace hierarchy");

                    if let Some(expr_namespaces) = self.tir.expr_namespaces.get(&base_expr) {
                        for ns in expr_namespaces {
                            match *ns {
                                ExprNamespace::Mod(scope) => self.find_overloads_in_mod(decl_ref, scope, &mut overloads),
                            }
                        }
                    }

                    break;
                },
            };

            root_namespace = false;
        }

        overloads.into_iter().collect()
    }

    fn add_types_2_to_4_deps_to_member_ref(&mut self, id: ItemId, decl_ref_id: DeclRefId) {
        add_eval_dep_injector!(self, add_eval_dep);
        ei_injector!(self, ei);
        di_injector!(self, di);
        let decl_ref = &self.hir.decl_refs[decl_ref_id];
        if let hir::Namespace::MemberRef { base_expr } = decl_ref.namespace {
            self.tir.graph.add_meta_dep(id, ei!(base_expr));
        }
        let overloads = self.find_overloads(decl_ref);
        for overload in overloads {
            match self.hir.decls[overload] {
                hir::Decl::Computed { ref param_tys, .. } => {
                    let ty = self.hir.explicit_tys[overload].unwrap_or(self.hir.void_ty);
                    add_eval_dep!(id, ty);
                    for &ty in param_tys {
                        add_eval_dep!(id, ty);
                    }
                    self.tir.graph.add_type3_dep(id, di!(overload));
                },
                _ => self.tir.graph.add_type2_dep(id, di!(overload)),
            }
        }
    }

    /// IMPORTANT NOTE: When/if we stop adding type3 deps to all items in a function's scope,
    /// we will need to bring back the original idea of meta-dependencies:
    /// https://github.com/zachrwolfe/meda/issues/58
    fn add_type3_scope_dep(&mut self, a: ItemId, b: ImperScopeId) {
        let scope = &self.hir.imper_scopes[b];
        for &item in &scope.items {
            match item {
                hir::ScopeItem::Stmt(expr) => self.tir.graph.add_type3_dep(a, self.hir.expr_to_items[expr]),
                hir::ScopeItem::StoredDecl { decl_id, .. } => self.tir.graph.add_type3_dep(a, self.hir.decl_to_items[decl_id]),
                hir::ScopeItem::ComputedDecl(_) => {},
            }
        }
    }

    fn flush_staged_ret_groups(&mut self, sp: &mut Subprogram) {
        let staged_ret_groups = std::mem::replace(&mut self.tir.staged_ret_groups, HashMap::new());
        for (decl, exprs) in staged_ret_groups {
            assert!(matches!(self.hir.decls[decl], hir::Decl::Computed { .. }));

            let ty = self.hir.explicit_tys[decl].expect("explicit return statements are not allowed in assigned functions (yet?)");

            let item = self.hir.decl_to_items[decl];
            let unit_id = sp.levels.item_to_units[item];
            let level = sp.levels.item_to_levels[item];
            let unit = &mut sp.units[unit_id as usize];
            unit.items.ret_groups.insert(
                level,
                RetGroup { ty, exprs },
            );
        }
    }

    fn build_tir_expr(&mut self, unit: &mut UnitItems, level: u32, id: ExprId) {
        macro_rules! insert_item {
            ($depvec:ident, $item:expr) => {{
                unit.$depvec.insert(
                    level, $item,
                );
            }}
        };
        macro_rules! insert_expr {
            ($depvec:ident, $expr:expr) => {{
                insert_item!($depvec, Expr { id, data: $expr, });
            }}
        };
        macro_rules! flat_insert_item {
            ($vec:ident, $item:expr) => {{
                assert_eq!(level, 0);
                unit.$vec.push($item);
            }}
        }
        match &self.hir.exprs[id] {
            hir::Expr::Void => {},
            hir::Expr::IntLit { .. } => flat_insert_item!(int_lits, id),
            hir::Expr::DecLit { .. } => flat_insert_item!(dec_lits, id),
            hir::Expr::StrLit { .. } => flat_insert_item!(str_lits, id),
            hir::Expr::CharLit { .. } => flat_insert_item!(char_lits, id),
            hir::Expr::ConstTy(_) => flat_insert_item!(const_tys, id),
            &hir::Expr::AddrOf { expr, is_mut } => insert_expr!(addr_ofs, AddrOf { expr, is_mut }),
            &hir::Expr::Deref(expr) => insert_expr!(derefs, Dereference { expr }),
            &hir::Expr::Pointer { expr, .. } => insert_expr!(pointers, Pointer { expr }),
            &hir::Expr::Cast { expr, ty, cast_id } => insert_expr!(casts, Cast { expr, ty, cast_id }),
            &hir::Expr::Ret { expr, decl } => {
                let decl = decl.expect("returning outside of a computed decl is invalid!");
                self.tir.staged_ret_groups.entry(decl).or_default().push(expr);
                insert_item!(explicit_rets, id);
            }
            &hir::Expr::DeclRef { ref arguments, id: decl_ref_id } => insert_expr!(decl_refs, DeclRef { args: arguments.clone(), decl_ref_id }),
            &hir::Expr::Set { lhs, rhs } => insert_expr!(assignments, Assignment { lhs, rhs }),
            &hir::Expr::Do { scope } => insert_expr!(dos, Do { terminal_expr: self.hir.imper_scopes[scope].terminal_expr }),
            &hir::Expr::If { condition, then_scope, else_scope } => {
                let then_expr = self.hir.imper_scopes[then_scope].terminal_expr;
                let else_expr = if let Some(else_scope) = else_scope {
                    self.hir.imper_scopes[else_scope].terminal_expr
                } else {
                    self.hir.void_expr
                };
                insert_expr!(ifs, If { condition, then_expr, else_expr });
            },
            &hir::Expr::While { condition, .. } => insert_expr!(whiles, While { condition }),
            hir::Expr::Mod { .. } => insert_item!(modules, id),
        }
    }

    fn build_tir_decl(&mut self, unit: &mut UnitItems, level: u32, id: DeclId) {
        match &self.hir.decls[id] {
            // TODO: Add a parameter TIR item for (at least) checking that the type of the param is valid
            hir::Decl::Parameter { .. } => {},
            hir::Decl::Intrinsic { .. } => {},
            &hir::Decl::Static(root_expr) | &hir::Decl::Const(root_expr) | &hir::Decl::Stored { root_expr, .. } => {
                let explicit_ty = self.hir.explicit_tys[id];
                unit.assigned_decls.insert(level, AssignedDecl { explicit_ty, root_expr, decl_id: id });
            },
            &hir::Decl::Computed { scope, .. } => {
                let terminal_expr = self.hir.imper_scopes[scope].terminal_expr;
                if let Some(_) = self.hir.explicit_tys[id] {
                    self.tir.staged_ret_groups.entry(id).or_default().push(terminal_expr);
                } else {
                    unit.assigned_decls.insert(level, AssignedDecl { explicit_ty: None, root_expr: terminal_expr, decl_id: id });
                }
            },
        }
    }

    pub fn initialize_tir(&mut self) {
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

        self.initialize_graph();
        ei_injector!(self, ei);
        di_injector!(self, di);
        add_eval_dep_injector!(self, add_eval_dep);
        // Add type 1 dependencies to the graph
        for i in 0..self.hir.decls.len() {
            let decl_id = DeclId::new(i);
            let id = di!(decl_id);
            match self.hir.decls[decl_id] {
                hir::Decl::Parameter { .. } | hir::Decl::Intrinsic { .. } => {},
                hir::Decl::Static(expr) | hir::Decl::Const(expr) | hir::Decl::Stored { root_expr: expr, .. } => self.tir.graph.add_type1_dep(id, ei!(expr)),
                hir::Decl::Computed { scope, .. } => {
                    let terminal_expr = self.hir.imper_scopes[scope].terminal_expr;
                    self.tir.graph.add_type1_dep(id, ei!(terminal_expr));
                },
            }
        }
        for i in 0..self.hir.exprs.len() {
            let expr_id = ExprId::new(i);
            let id = ei!(expr_id);
            match self.hir.exprs[expr_id] {
                hir::Expr::Void | hir::Expr::IntLit { .. } | hir::Expr::DecLit { .. } | hir::Expr::StrLit { .. }
                    | hir::Expr::CharLit { .. } | hir::Expr::ConstTy(_) | hir::Expr::Mod { .. } => {},
                hir::Expr::AddrOf { expr, .. } | hir::Expr::Deref(expr) | hir::Expr::Pointer { expr, .. }
                    | hir::Expr::Cast { expr, .. } | hir::Expr::Ret { expr, .. } => self.tir.graph.add_type1_dep(id, ei!(expr)),
                hir::Expr::DeclRef { ref arguments, id: decl_ref_id } => {
                    let decl_ref = &self.hir.decl_refs[decl_ref_id];
                    if let hir::Namespace::MemberRef { base_expr } = decl_ref.namespace {
                        self.tir.graph.add_type1_dep(id, ei!(base_expr));
                    }
                    for &arg in arguments {
                        self.tir.graph.add_type1_dep(id, ei!(arg));
                    }
                },
                hir::Expr::Set { lhs, rhs } => {
                    self.tir.graph.add_type1_dep(id, ei!(lhs));
                    self.tir.graph.add_type1_dep(id, ei!(rhs));
                },
                hir::Expr::Do { scope } => {
                    let terminal_expr = self.hir.imper_scopes[scope].terminal_expr;
                    self.tir.graph.add_type1_dep(id, ei!(terminal_expr));
                },
                hir::Expr::If { condition, then_scope, else_scope } => {
                    self.tir.graph.add_type1_dep(id, ei!(condition));
                    let then_expr = self.hir.imper_scopes[then_scope].terminal_expr;
                    let else_expr = if let Some(else_scope) = else_scope {
                        self.hir.imper_scopes[else_scope].terminal_expr
                    } else {
                        self.hir.void_expr
                    };
                    self.tir.graph.add_type1_dep(id, ei!(then_expr));
                    self.tir.graph.add_type1_dep(id, ei!(else_expr));
                }
                hir::Expr::While { condition, scope } => {
                    self.tir.graph.add_type1_dep(id, ei!(condition));
                    let terminal_expr = self.hir.imper_scopes[scope].terminal_expr;
                    self.tir.graph.add_type1_dep(id, ei!(terminal_expr));
                },
            }
        }

        // Split the graph into components
        self.tir.graph.split();

        // TODO: do something better than an array of bools :(
        self.tir.depended_on.resize_with(self.hir.exprs.len(), || false);

        // Add types 2-4 dependencies to graph
        for i in 0..self.hir.decls.len() {
            let decl_id = DeclId::new(i);
            let id = di!(decl_id);
            match self.hir.decls[decl_id] {
                hir::Decl::Parameter { .. } | hir::Decl::Static(_) | hir::Decl::Const(_) | hir::Decl::Stored { .. } => {},
                hir::Decl::Intrinsic { ref param_tys, .. } => {
                    for &ty in param_tys {
                        add_eval_dep!(id, ty);
                    }
                },
                hir::Decl::Computed { scope, ref param_tys, .. } => {
                    for &ty in param_tys {
                        add_eval_dep!(id, ty);
                    }
                    self.add_type3_scope_dep(id, scope);
                    let ty = self.hir.explicit_tys[decl_id].unwrap_or(self.hir.void_ty);
                    add_eval_dep!(id, ty);
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
                    | hir::Expr::CharLit { .. } | hir::Expr::ConstTy(_) | hir::Expr::AddrOf { .. } | hir::Expr::Deref(_)
                    | hir::Expr::Pointer { .. } | hir::Expr::Set { .. } => {},
                hir::Expr::Mod { id: mod_scope_id } => {
                    for decl_group in self.hir.mod_scopes[mod_scope_id].decl_groups.values() {
                        for decl in decl_group {
                            self.tir.graph.add_type3_dep(id, di!(decl.id));
                        }
                    }
                },
                hir::Expr::Cast { ty, .. } => {
                    add_eval_dep!(id, ty);
                },
                hir::Expr::Ret { decl, .. } => {
                    let ty = decl
                        .and_then(|decl| self.hir.explicit_tys[decl])
                        .unwrap_or(self.hir.void_ty);
                    add_eval_dep!(id, ty);
                }
                hir::Expr::DeclRef { id: decl_ref_id, .. } => {
                    self.add_types_2_to_4_deps_to_member_ref(id, decl_ref_id);
                },
                hir::Expr::Do { scope } => {
                    self.add_type3_scope_dep(id, scope);
                },
                hir::Expr::If { then_scope, else_scope, .. } => {
                    self.add_type3_scope_dep(id, then_scope);
                    if let Some(else_scope) = else_scope {
                        self.add_type3_scope_dep(id, else_scope);
                    }
                }
                hir::Expr::While { scope, .. } => {
                    self.add_type3_scope_dep(id, scope);
                }
            }
        }
    }

    pub fn build_more_tir(&mut self) -> Vec<Unit> {
        ei_injector!(self, ei);

        // Solve for the unit and level of each item
        let levels = self.tir.graph.solve();

        let mut sp = Subprogram { units: Vec::new(), levels };
        sp.units.resize_with(sp.levels.units.len(), || Unit::default());

        // Finally, convert HIR items to TIR and add them to the correct spot
        for unit_id in 0..sp.levels.units.len() {
            let unit = &mut sp.units[unit_id];
            for i in 0..sp.levels.units[unit_id].items.len() {
                let item_id = sp.levels.units[unit_id].items[i];
                let level = sp.levels.item_to_levels[item_id];
                match self.hir.items[item_id] {
                    hir::Item::Decl(id) => {
                        self.build_tir_decl(&mut unit.items, level, id);
                    }
                    hir::Item::Expr(id) => {
                        if self.tir.depended_on[id] { unit.eval_dependees.push(id); }
                        self.build_tir_expr(&mut unit.items, level, id);
                    }
                }
            }

            for level_dep in &sp.levels.units[unit_id].meta_dependees {
                let mut meta_dependees = Vec::new();
                for dep in &level_dep.meta_dependees {
                    let expr = match self.hir.items[dep.item] {
                        hir::Item::Expr(id) => id,
                        hir::Item::Decl(_) => panic!("Can't have metadependency on a declaration!"),
                    };
                    let mut items = UnitItems::default();
                    let level = sp.levels.item_to_levels[dep.item];
                    self.build_tir_expr(&mut items, level, expr);
                    for &item_id in &dep.deps {
                        let level = sp.levels.item_to_levels[item_id];
                        match self.hir.items[item_id] {
                            hir::Item::Decl(id) => {
                                self.build_tir_decl(&mut items, level, id);
                            }
                            hir::Item::Expr(id) => {
                                self.build_tir_expr(&mut items, level, id);
                            }
                        }
                    }
                    items.unify_sizes();
                    meta_dependees.push(
                        MetaDependee {
                            dependee: expr,
                            items,
                        }
                    );
                }
                sp.units[unit_id].meta_dependees.push(
                    LevelMetaDependees {
                        level: level_dep.level,
                        meta_dependees,
                    }
                )
            }
        }
        self.flush_staged_ret_groups(&mut sp);
        for scope in &self.hir.imper_scopes {
            for &item in &scope.items {
                match item {
                    hir::ScopeItem::Stmt(expr) => {
                        let unit = sp.levels.item_to_units[ei!(expr)];
                        let unit = &mut sp.units[unit as usize];
                        unit.items.stmts.push(Stmt { root_expr: expr });
                    },
                    hir::ScopeItem::ComputedDecl(_) | hir::ScopeItem::StoredDecl { .. } => {},
                }
            }
        }

        for unit in &mut sp.units {
            unit.items.unify_sizes();
        }

        sp.units
    }
}