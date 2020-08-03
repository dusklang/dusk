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

#[derive(Debug)]
pub struct Unit {
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

    /// The expressions in this unit that later units have eval dependencies on
    pub eval_dependees: Vec<ExprId>,
}

impl Unit {
    fn clear_up_to(&mut self, level: u32) {
        self.int_lits.clear();
        self.dec_lits.clear();
        self.str_lits.clear();
        self.char_lits.clear();
        self.const_tys.clear();

        // Intentionally don't clear stmts; they are added from scopes, not items, and they are processed at the end of pass 1

        self.explicit_rets.clear_up_to(level);
        self.ret_groups.clear_up_to(level);
        self.casts.clear_up_to(level);
        self.whiles.clear_up_to(level);
        self.modules.clear_up_to(level);
        self.dos.clear_up_to(level);
        self.assigned_decls.clear_up_to(level);
        self.assignments.clear_up_to(level);
        self.decl_refs.clear_up_to(level);
        self.addr_ofs.clear_up_to(level);
        self.derefs.clear_up_to(level);
        self.pointers.clear_up_to(level);
        self.ifs.clear_up_to(level);
    }
    fn new() -> Self {
        Self {
            int_lits: Vec::new(),
            dec_lits: Vec::new(),
            str_lits: Vec::new(),
            char_lits: Vec::new(),
            const_tys: Vec::new(),
            stmts: Vec::new(),
            explicit_rets: DepVec::new(),
            ret_groups: DepVec::new(),
            casts: DepVec::new(),
            whiles: DepVec::new(),
            modules: DepVec::new(),
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

    graph: Graph,
    depended_on: IdxVec<bool, ExprId>,

    levels: Levels,
    staged_ret_groups: HashMap<DeclId, SmallVec<[ExprId; 1]>>,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            units: Vec::new(),
            decls: IdxVec::new(),
            overloads: IdxVec::new(),
            graph: Graph::default(),
            depended_on: IdxVec::new(),
            levels: Levels::default(),
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
    // Returns the overloads for a declref, if they are known (they won't be if it's a member ref)
    fn find_overloads(&self, decl_ref: &hir::DeclRef) -> Option<Vec<DeclId>> {
        let mut overloads = Vec::new();

        let mut last_was_imperative = true;
        let mut root_namespace = true;
        let mut namespace = Some(decl_ref.namespace);
        while let Some(ns) = namespace {
            namespace = match ns {
                Namespace::Imper { scope, end_offset } => {
                    if !last_was_imperative { break; }
    
                    let namespace = &self.hir.imper_ns[scope];
                    let result = namespace.decls[0..end_offset].iter()
                        .rev()
                        .find(|&decl| decl.name == decl_ref.name && decl.num_params == decl_ref.num_arguments);
                    if let Some(decl) = result {
                        overloads.push(decl.id);
                        break;
                    }

                    last_was_imperative = true;
                    self.hir.imper_ns[scope].parent
                },
                Namespace::Mod(scope_ns) => {
                    let scope = self.hir.mod_ns[scope_ns].scope;
                    if let Some(group) = self.hir.mod_scopes[scope].decl_groups.get(&decl_ref.name) {
                        overloads.extend(
                            group.iter()
                                .filter(|decl| decl.num_params == decl_ref.num_arguments)
                                .map(|decl| decl.id)
                        );
                    }

                    last_was_imperative = false;
                    self.hir.mod_ns[scope_ns].parent
                },
    
                // TODO: get the overloads
                Namespace::MemberRef { .. } => {
                    assert!(root_namespace, "member refs currently must be at the root of a namespace hierarchy");

                    return None;
                },
            };

            root_namespace = false;
        }

        Some(overloads)
    }

    fn add_types_2_to_4_deps_to_member_ref(&mut self, id: ItemId, arguments: &[ExprId], decl_ref_id: DeclRefId) {
        add_eval_dep_injector!(self, add_eval_dep);
        ei_injector!(self, ei);
        di_injector!(self, di);
        let decl_ref = &self.hir.decl_refs[decl_ref_id];
        if let hir::Namespace::MemberRef { base_expr } = decl_ref.namespace {
            self.tir.graph.add_meta_dep(id, ei!(base_expr));
        }
        let overloads = self.find_overloads(decl_ref);
        self.tir.overloads[decl_ref_id] = overloads.unwrap_or_default();
        for &overload in &self.tir.overloads[decl_ref_id] {
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

    fn add_type3_scope_dep(&mut self, a: ItemId, b: ImperScopeId) {
        let scope = &self.hir.imper_scopes[b];
        for &item in &scope.items {
            match item {
                hir::ScopeItem::Stmt(expr) => self.tir.graph.add_type3_dep(a, self.hir.expr_to_items[expr]),
                hir::ScopeItem::StoredDecl { decl_id, root_expr, .. } => self.tir.graph.add_type3_dep(a, self.hir.decl_to_items[decl_id]),
                hir::ScopeItem::ComputedDecl(_) => {},
            }
        }
    }

    // Returns a new unit with the passed-in `items` and their dependencies if any are in the current `unit`. Otherwise, returns `None`.
    pub fn add_dynamic_type4_dependency(&mut self, unit: u32, items: &[ItemId]) -> Option<Unit> {
        let items: Vec<_> = items.iter()
            .map(|item| *item)
            .filter(|&item| self.tir.levels.item_to_units[item] == unit)
            .collect();
        if items.is_empty() { return None; }

        let removed_items = self.tir.levels.split_unit(unit, &items);

        let max_level = items.iter().map(|&item| self.tir.levels.item_to_levels[item]).max().unwrap();
        self.tir.units[unit as usize].clear_up_to(max_level);
        let mut new_unit = Unit::new();

        // Recreate the portion of the old unit that we want
        for i in 0..self.tir.levels.units[unit as usize].len() {
            let id = self.tir.levels.units[unit as usize][i];
            self.build_tir_item(id, None);
        }
        self.flush_staged_ret_groups(None);

        for id in removed_items {
            self.build_tir_item(id, Some(&mut new_unit));
        }
        self.flush_staged_ret_groups(Some(&mut new_unit));

        Some(new_unit)
    }

    fn flush_staged_ret_groups(&mut self, mut unit: Option<&mut Unit>) {
        let staged_ret_groups = std::mem::replace(&mut self.tir.staged_ret_groups, HashMap::new());
        for (decl, exprs) in staged_ret_groups {
            let scope = if let hir::Decl::Computed { scope, .. } = self.hir.decls[decl] {
                scope
            } else {
                panic!("Invalid staged ret group")
            };

            let ty = self.hir.explicit_tys[decl].expect("explicit return statements are not allowed in assigned functions (yet?)");

            let item = self.hir.decl_to_items[decl];
            let unit_id = self.tir.levels.item_to_units[item];
            let level = self.tir.levels.item_to_levels[item];
            let unit = if let Some(unit) = &mut unit {
                unit
            } else {
                &mut self.tir.units[unit_id as usize]
            };
            unit.ret_groups.insert(
                level,
                RetGroup { ty, exprs },
            );
        }
    }

    fn build_tir_item(&mut self, id: ItemId, unit: Option<&mut Unit>) {
        match self.hir.items[id] {
            hir::Item::Expr(expr_id) => self.build_tir_expr(id, expr_id, unit),
            hir::Item::Decl(decl_id) => self.build_tir_decl(id, decl_id, unit),
        }
    }

    fn build_tir_expr(&mut self, item_id: ItemId, id: ExprId, unit: Option<&mut Unit>) {
        let level = self.tir.levels.item_to_levels[item_id];
        let unit = if let Some(unit) = unit {
            unit
        } else {
            let unit = self.tir.levels.item_to_units[item_id];
            &mut self.tir.units[unit as usize]
        };
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

    fn build_tir_decl(&mut self, item_id: ItemId, id: DeclId, unit: Option<&mut Unit>) {
        let level = self.tir.levels.item_to_levels[item_id];
        let unit = if let Some(unit) = unit {
            unit
        } else {
            let unit = self.tir.levels.item_to_units[item_id];
            &mut self.tir.units[unit as usize]
        };

        match &self.hir.decls[id] {
            // TODO: Add a parameter TIR item for (at least) checking that the type of the param is valid
            hir::Decl::Parameter { .. } => {},
            hir::Decl::Intrinsic { ref param_tys, .. } => {},
            &hir::Decl::Static(root_expr) | &hir::Decl::Const(root_expr) | &hir::Decl::Stored { root_expr, .. } => {
                let explicit_ty = self.hir.explicit_tys[id];
                unit.assigned_decls.insert(level, AssignedDecl { explicit_ty, root_expr, decl_id: id });
            },
            &hir::Decl::Computed { scope, ref param_tys, .. } => {
                let terminal_expr = self.hir.imper_scopes[scope].terminal_expr;
                if let Some(ty) = self.hir.explicit_tys[id] {
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

        debug_assert!(self.tir.overloads.is_empty());
        self.tir.overloads.reserve(self.hir.decl_refs.len());

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
        self.tir.overloads.resize_with(self.hir.decl_refs.len(), Default::default);

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
                    | hir::Expr::Pointer { .. } | hir::Expr::Set { .. } | hir::Expr::Mod { .. } => {},
                hir::Expr::Cast { expr, ty, .. } => {
                    add_eval_dep!(id, ty);
                },
                hir::Expr::Ret { expr, decl } => {
                    let ty = decl
                        .and_then(|decl| self.hir.explicit_tys[decl])
                        .unwrap_or(self.hir.void_ty);
                    add_eval_dep!(id, ty);
                }
                hir::Expr::DeclRef { ref arguments, id: decl_ref_id } => {
                    // Yay borrow-checker
                    let arguments = arguments.clone();
                    self.add_types_2_to_4_deps_to_member_ref(id, &arguments, decl_ref_id);
                },
                hir::Expr::Do { scope } => {
                    self.add_type3_scope_dep(id, scope);
                },
                hir::Expr::If { condition, then_scope, else_scope } => {
                    self.add_type3_scope_dep(id, then_scope);
                    if let Some(else_scope) = else_scope {
                        self.add_type3_scope_dep(id, else_scope);
                    }
                }
                hir::Expr::While { condition, scope } => {
                    self.add_type3_scope_dep(id, scope);
                }
            }
        }
    }

    pub fn build_more_tir(&mut self) {
        ei_injector!(self, ei);
        // Imagined typechecking flow:
        // - Driver calls a TIR generation method, which does the following:
        //   - Gets the set of declrefs for which we now have namespace info (if any), and adds types 2-4 dependencies to them (thus resolving the metadependencies)
        //   - Find and solve for the next few units
        //   - Build TIR for each item in each unit (including mock TIR units for any meta-dependees)
        // - Driver calls typechecker on the units it got from tir.rs (which also should figure out all member ref namepsaces via the mock units)
        // - Driver repeats from the beginning until there are no more items, or nothing happened in the previous iteration (possible?)

        // Solve for the unit and level of each item
        self.tir.graph.find_units();
        self.print_graph().unwrap();
        let graph = std::mem::replace(&mut self.tir.graph, Graph::default());
        self.tir.levels = graph.solve();
        self.tir.units.resize_with(self.tir.levels.units.len(), || Unit::new());

        // Finally, convert HIR items to TIR and add them to the correct spot
        for unit in 0..self.tir.levels.units.len() {
            for i in 0..self.tir.levels.units[unit].len() {
                let item_id = self.tir.levels.units[unit][i];
                match self.hir.items[item_id] {
                    hir::Item::Decl(id) => {
                        self.build_tir_decl(item_id, id, None);
                    }
                    hir::Item::Expr(id) => {
                        let unit = &mut self.tir.units[unit];
                        if self.tir.depended_on[id] { unit.eval_dependees.push(id); }
                        self.build_tir_expr(item_id, id, None);
                    }
                }
            }
        }
        self.flush_staged_ret_groups(None);
        for scope in &self.hir.imper_scopes {
            for &item in &scope.items {
                match item {
                    hir::ScopeItem::Stmt(expr) => {
                        let unit = self.tir.levels.item_to_units[ei!(expr)];
                        let unit = &mut self.tir.units[unit as usize];
                        unit.stmts.push(Stmt { root_expr: expr });
                    },
                    hir::ScopeItem::ComputedDecl(_) | hir::ScopeItem::StoredDecl { .. } => {},
                }
            }
        }
    }
}