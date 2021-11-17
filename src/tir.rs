use std::ops::{Deref, DerefMut};
use std::collections::{HashMap, HashSet};

use smallvec::SmallVec;
use index_vec::define_index_type;

use dire::hir::{self, Item, Namespace, FieldAssignment, ExprId, DeclId, DeclRefId, StructLitId, ModScopeId, StructId, ItemId, ImperScopeId, CastId, GenericParamId, RETURN_VALUE_DECL};

use crate::driver::Driver;
use crate::dep_vec::{self, DepVec, AnyDepVec};
use crate::index_vec::*;
use crate::TirGraphOutput;

mod graph;
use graph::{Graph, Levels};

define_index_type!(pub struct TreeId = u32;);

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
pub struct Struct { pub field_tys: SmallVec<[ExprId; 2]>, }
#[derive(Debug)]
pub struct StructLit { pub ty: ExprId, pub fields: Vec<FieldAssignment>, pub struct_lit_id: StructLitId, }
#[derive(Debug)]
pub struct ExplicitRet;
#[derive(Debug)]
pub struct Module;
#[derive(Debug)]
pub struct Import;
#[derive(Debug)]
pub struct IntLit;
#[derive(Debug)]
pub struct DecLit;
#[derive(Debug)]
pub struct StrLit;
#[derive(Debug)]
pub struct CharLit;
#[derive(Debug)]
pub struct ConstTy;
#[derive(Debug)]
pub struct GenericParam { pub id: DeclId }

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
    pub generic_params: SmallVec<[GenericParamId; 1]>,
    pub is_mut: bool,
}

struct Subprogram {
    units: Vec<Unit>,
    mock_units: Vec<MockUnit>,
    levels: Levels,
}

#[derive(Default, Debug)]
pub struct UnitItems {
    pub int_lits: Vec<Expr<IntLit>>,
    pub dec_lits: Vec<Expr<DecLit>>,
    pub str_lits: Vec<Expr<StrLit>>,
    pub char_lits: Vec<Expr<CharLit>>,
    pub const_tys: Vec<Expr<ConstTy>>,
    pub generic_params: Vec<GenericParam>,
    pub stmts: Vec<Stmt>,
    pub explicit_rets: DepVec<Expr<ExplicitRet>>,
    pub ret_groups: DepVec<RetGroup>,
    pub casts: DepVec<Expr<Cast>>,
    pub whiles: DepVec<Expr<While>>,
    pub modules: DepVec<Expr<Module>>,
    pub imports: DepVec<Expr<Import>>,
    pub dos: DepVec<Expr<Do>>,
    pub assigned_decls: DepVec<AssignedDecl>,
    pub assignments: DepVec<Expr<Assignment>>,
    pub decl_refs: DepVec<Expr<DeclRef>>,
    pub addr_ofs: DepVec<Expr<AddrOf>>,
    pub derefs: DepVec<Expr<Dereference>>,
    pub pointers: DepVec<Expr<Pointer>>,
    pub ifs: DepVec<Expr<If>>,
    pub structs: DepVec<Expr<Struct>>,
    pub struct_lits: DepVec<Expr<StructLit>>,
}

impl UnitItems {
    pub fn num_levels(&self) -> u32 {
        // Assumption: all DepVecs in the unit have the same number of levels!
        self.assigned_decls.num_levels()
    }
}

impl UnitItems {
    fn unify_sizes(&mut self) {
        dep_vec::unify_sizes(&mut [
            &mut self.assigned_decls, &mut self.assignments, &mut self.decl_refs, 
            &mut self.addr_ofs, &mut self.derefs, &mut self.pointers, &mut self.ifs,
            &mut self.dos, &mut self.ret_groups, &mut self.casts, &mut self.whiles,
            &mut self.explicit_rets, &mut self.modules, &mut self.imports, &mut self.structs,
            &mut self.struct_lits,
        ]);
    }
}

#[derive(Debug, Default)]
pub struct Unit {
    pub items: UnitItems,

    /// The expressions in this unit that later units have eval dependencies on
    pub eval_dependees: Vec<ExprId>,
}

#[derive(Debug)]
pub struct MockUnit {
    pub main_expr: ExprId,
    pub items: UnitItems,
}

#[derive(Debug, Default)]
pub struct Units {
    pub units: Vec<Unit>,
    pub mock_units: Vec<MockUnit>,
}

/// The namespace "inside" an expression,
/// i.e. what are all the declarations that you might be able to access as members of an expression
#[derive(Debug)]
pub enum ExprNamespace {
    Mod(ModScopeId),
    Struct(StructId),
}

#[derive(Debug, Default)]
pub struct Builder {
    pub decls: IndexVec<DeclId, Decl>,
    pub expr_namespaces: HashMap<ExprId, Vec<ExprNamespace>>,
    graph: Graph,
    depended_on: IndexVec<ExprId, bool>,

    staged_ret_groups: HashMap<DeclId, SmallVec<[ExprId; 1]>>,
}

macro_rules! ei_injector {
    ($self:expr, $name:ident) => { 
        macro_rules! $name {
            ($a: expr) => { $self.code.hir_code.expr_to_items[$a] }
        }
    }
}
macro_rules! di_injector {
    ($self:expr, $name:ident) => { 
        macro_rules! $name {
            ($a: expr) => { $self.code.hir_code.decl_to_items[$a] }
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
        if let Some(group) = self.code.hir_code.mod_scopes[scope].decl_groups.get(&decl_ref.name) {
            overloads.extend(
                group.iter()
                    .filter(|decl| decl.num_params == decl_ref.num_arguments)
                    .map(|decl| decl.id)
            );
        }
    }
    fn find_overloads_in_struct(&self, decl_ref: &hir::DeclRef, strukt: StructId, overloads: &mut HashSet<DeclId>) {
        for &field in &self.code.hir_code.structs[strukt].fields {
            let field = &self.code.hir_code.field_decls[field];
            if field.name == decl_ref.name {
                overloads.insert(field.decl);
                return;
            }
        }
    }
    fn find_overloads_in_function_parameters(&self, decl_ref: &hir::DeclRef, func: DeclId, overloads: &mut HashSet<DeclId>) {
        match &self.code.hir_code.decls[func] {
            hir::Decl::Computed { params, .. } => {
                for i in params.start.index()..params.end.index() {
                    let decl = DeclId::new(i);
                    let param_name = self.code.hir_code.names[decl];
                    if decl_ref.name == param_name {
                        overloads.insert(decl);
                    }
                }
            },
            _ => panic!("Can only have requirements clause on computed decls"),
        }
    }
    // Returns the overloads for a declref, if they are known (they won't be if it's an unresolved member ref)
    pub fn find_overloads(&self, decl_ref: &hir::DeclRef) -> Vec<DeclId> {
        let mut overloads = HashSet::new();

        let mut started_at_mod_scope = false;
        let mut root_namespace = true;
        let mut namespace = Some(decl_ref.namespace);
        'find_overloads: while let Some(ns) = namespace {
            namespace = match ns {
                Namespace::Imper { scope, end_offset } => {
                    if !started_at_mod_scope {
                        let namespace = &self.code.hir_code.imper_ns[scope];
                        let result = namespace.decls[0..end_offset].iter()
                            .rev()
                            .find(|&decl| decl.name == decl_ref.name && decl.num_params == decl_ref.num_arguments);
                        if let Some(decl) = result {
                            overloads.insert(decl.id);
                            break;
                        }
                    }

                    self.code.hir_code.imper_ns[scope].parent
                },
                Namespace::Mod(scope_ns) => {
                    let scope = self.code.hir_code.mod_ns[scope_ns].scope;
                    self.find_overloads_in_mod(decl_ref, scope, &mut overloads);

                    if root_namespace { started_at_mod_scope = true; }
                    self.code.hir_code.mod_ns[scope_ns].parent
                },
                Namespace::MemberRef { base_expr } => {
                    assert!(root_namespace, "member refs currently must be at the root of a namespace hierarchy");

                    if let Some(expr_namespaces) = self.tir.expr_namespaces.get(&base_expr) {
                        for ns in expr_namespaces {
                            match *ns {
                                ExprNamespace::Mod(scope) => self.find_overloads_in_mod(decl_ref, scope, &mut overloads),
                                ExprNamespace::Struct(id) => self.find_overloads_in_struct(decl_ref, id, &mut overloads),
                            }
                        }
                    }

                    break;
                },
                Namespace::CompDeclParams(ns_id) => {
                    let comp_decl_params_ns = &self.code.hir_code.comp_decl_params_ns[ns_id];
                    if let hir::Decl::Computed { generic_params, .. } = &self.code.hir_code.decls[comp_decl_params_ns.func] {
                        for i in generic_params.start.index()..generic_params.end.index() {
                            let decl = DeclId::new(i);
                            let param_name = self.code.hir_code.names[decl];
                            if decl_ref.name == param_name {
                                overloads.insert(decl);
                                break 'find_overloads;
                            }
                        }
                    } else {
                        panic!("expected computed decl");
                    };
                    comp_decl_params_ns.parent
                },
                Namespace::Requirement(ns_id) => {
                    let condition_ns = &self.code.hir_code.condition_ns[ns_id];
                    self.find_overloads_in_function_parameters(decl_ref, condition_ns.func, &mut overloads);
                    condition_ns.parent
                },
                Namespace::Guarantee(ns_id) => {
                    let condition_ns = &self.code.hir_code.condition_ns[ns_id];
                    self.find_overloads_in_function_parameters(decl_ref, condition_ns.func, &mut overloads);
                    if decl_ref.name == self.hir.return_value_sym && overloads.is_empty() {
                        overloads.insert(RETURN_VALUE_DECL);
                    }
                    condition_ns.parent
                },
            };

            root_namespace = false;
        }

        overloads.into_iter().collect()
    }

    fn add_types_2_to_4_deps_to_member_ref(&mut self, id: ItemId, decl_ref_id: DeclRefId) {
        add_eval_dep_injector!(self, add_eval_dep);
        di_injector!(self, di);
        let decl_ref = &self.code.hir_code.decl_refs[decl_ref_id];
        let overloads = self.find_overloads(decl_ref);
        for overload in overloads {
            match self.code.hir_code.decls[overload] {
                hir::Decl::Computed { ref param_tys, .. } => {
                    let ty = self.code.hir_code.explicit_tys[overload].unwrap_or(hir::VOID_TYPE);
                    add_eval_dep!(id, ty);
                    for &ty in param_tys {
                        add_eval_dep!(id, ty);
                    }
                    self.tir.graph.add_type3_dep(id, di!(overload));
                },
                hir::Decl::ReturnValue => {
                    let decl_ref = &self.code.hir_code.decl_refs[decl_ref_id];
                    match decl_ref.namespace {
                        Namespace::Guarantee(condition_ns_id) => {
                            let func = self.code.hir_code.condition_ns[condition_ns_id].func;
                            self.tir.graph.add_type3_dep(id, di!(func));
                        },
                        _ => panic!("invalid namespace for `return_value`"),
                    }
                },
                _ => self.tir.graph.add_type2_dep(id, di!(overload)),
            }
        }
    }

    /// IMPORTANT NOTE: When/if we stop adding type3 deps to all items in a function's scope,
    /// we will need to bring back the original idea of meta-dependencies:
    /// https://github.com/dusk-lang/dusk/issues/58
    fn add_type3_scope_dep(&mut self, a: ItemId, b: ImperScopeId) {
        let block = self.code.hir_code.imper_scopes[b].block;
        for &op in &self.code.blocks[block].ops {
            let op = &self.code.ops[op];
            let item = op.as_hir_item().unwrap();
            match item {
                Item::Expr(expr) => self.tir.graph.add_type3_dep(a, self.code.hir_code.expr_to_items[expr]),
                Item::Decl(decl) => match self.code.hir_code.decls[decl] {
                    hir::Decl::Stored { .. } => self.tir.graph.add_type3_dep(a, self.code.hir_code.decl_to_items[decl]),
                    hir::Decl::Computed { .. } => {},
                    _ => panic!("Invalid scope item"),
                },
            }
        }
    }

    fn flush_staged_ret_groups(&mut self, sp: &mut Subprogram) {
        let staged_ret_groups = std::mem::replace(&mut self.tir.staged_ret_groups, HashMap::new());
        for (decl, exprs) in staged_ret_groups {
            assert!(matches!(self.code.hir_code.decls[decl], hir::Decl::Computed { .. }));

            let ty = self.code.hir_code.explicit_tys[decl].expect("explicit return statements are not allowed in assigned functions (yet?)");

            let item = self.code.hir_code.decl_to_items[decl];
            let unit_id = sp.levels.item_to_units[&item];
            let level = sp.levels.item_to_levels[&item];
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
        }
        macro_rules! insert_expr {
            ($depvec:ident, $expr:expr) => {{
                insert_item!($depvec, Expr { id, data: $expr, });
            }}
        }
        macro_rules! flat_insert_item {
            ($vec:ident, $item:expr) => {{
                assert_eq!(level, 0);
                unit.$vec.push($item);
            }}
        }
        macro_rules! flat_insert_expr {
            ($vec:ident, $expr:expr) => {{
                flat_insert_item!($vec, Expr { id, data: $expr, });
            }}
        }
        match &self.code.hir_code.exprs[id] {
            hir::Expr::Void => {},
            hir::Expr::IntLit { .. } => flat_insert_expr!(int_lits, IntLit),
            hir::Expr::DecLit { .. } => flat_insert_expr!(dec_lits, DecLit),
            hir::Expr::StrLit { .. } => flat_insert_expr!(str_lits, StrLit),
            hir::Expr::CharLit { .. } => flat_insert_expr!(char_lits, CharLit),
            hir::Expr::ConstTy(_) => flat_insert_expr!(const_tys, ConstTy),
            &hir::Expr::AddrOf { expr, is_mut } => insert_expr!(addr_ofs, AddrOf { expr, is_mut }),
            &hir::Expr::Deref(expr) => insert_expr!(derefs, Dereference { expr }),
            &hir::Expr::Pointer { expr, .. } => insert_expr!(pointers, Pointer { expr }),
            &hir::Expr::Cast { expr, ty, cast_id } => insert_expr!(casts, Cast { expr, ty, cast_id }),
            &hir::Expr::Ret { expr, decl } => {
                let decl = decl.expect("returning outside of a computed decl is invalid!");
                self.tir.staged_ret_groups.entry(decl).or_default().push(expr);
                insert_expr!(explicit_rets, ExplicitRet)
            }
            &hir::Expr::DeclRef { ref arguments, id: decl_ref_id } => insert_expr!(decl_refs, DeclRef { args: arguments.clone(), decl_ref_id }),
            &hir::Expr::Set { lhs, rhs } => insert_expr!(assignments, Assignment { lhs, rhs }),
            &hir::Expr::Do { scope } => insert_expr!(dos, Do { terminal_expr: self.code.hir_code.imper_scopes[scope].terminal_expr }),
            &hir::Expr::If { condition, then_scope, else_scope } => {
                let then_expr = self.code.hir_code.imper_scopes[then_scope].terminal_expr;
                let else_expr = if let Some(else_scope) = else_scope {
                    self.code.hir_code.imper_scopes[else_scope].terminal_expr
                } else {
                    hir::VOID_EXPR
                };
                insert_expr!(ifs, If { condition, then_expr, else_expr });
            },
            &hir::Expr::While { condition, .. } => insert_expr!(whiles, While { condition }),
            hir::Expr::Mod { .. } => insert_expr!(modules, Module),
            hir::Expr::Import { .. } => insert_expr!(imports, Import),
            &hir::Expr::Struct(struct_id) => {
                let field_tys = self.code.hir_code.structs[struct_id].fields.iter().map(|&id| self.code.hir_code.field_decls[id].ty).collect();
                insert_expr!(structs, Struct { field_tys })
            },
            &hir::Expr::StructLit { ty, ref fields, id } => {
                insert_expr!(struct_lits, StructLit { ty, fields: fields.clone(), struct_lit_id: id })
            }
        }
    }

    fn build_tir_decl(&mut self, unit: &mut UnitItems, level: u32, id: DeclId) {
        match self.code.hir_code.decls[id] {
            // TODO: Add parameter and field TIR items for (at least) checking that the type of the param is valid
            hir::Decl::Parameter { .. } | hir::Decl::Field(_) | hir::Decl::ReturnValue | hir::Decl::Intrinsic { .. } => {},
            hir::Decl::GenericParam(_) => {
                assert_eq!(level, 0);
                unit.generic_params.push(GenericParam { id });
            },
            hir::Decl::Static(root_expr) | hir::Decl::Const(root_expr) | hir::Decl::Stored { root_expr, .. } => {
                let explicit_ty = self.code.hir_code.explicit_tys[id];
                unit.assigned_decls.insert(level, AssignedDecl { explicit_ty, root_expr, decl_id: id });
            },
            hir::Decl::Computed { scope, .. } => {
                let terminal_expr = self.code.hir_code.imper_scopes[scope].terminal_expr;
                if let Some(_) = self.code.hir_code.explicit_tys[id] {
                    self.tir.staged_ret_groups.entry(id).or_default().push(terminal_expr);
                } else {
                    unit.assigned_decls.insert(level, AssignedDecl { explicit_ty: None, root_expr: terminal_expr, decl_id: id });
                }
            },
        }
    }

    pub fn initialize_tir(&mut self) {
        // Populate `decls`
        for decl in &self.code.hir_code.decls {
            let mut generic_params = SmallVec::new();
            let (is_mut, param_tys) = match *decl {
                hir::Decl::Computed { ref param_tys, generic_params: ref og_generic_params, .. } => {
                    for i in og_generic_params.start.index()..og_generic_params.end.index() {
                        let decl = DeclId::new(i);
                        let generic_param_id = match self.code.hir_code.decls[decl] {
                            hir::Decl::GenericParam(id) => id,
                            _ => panic!("COMPILER BUG: expected generic parameter"),
                        };
                        generic_params.push(generic_param_id);
                    }

                    (false, param_tys.clone())
                },
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
                hir::Decl::Field(_) => (
                    true,
                    SmallVec::new()
                ),
                hir::Decl::ReturnValue => (
                    false,
                    SmallVec::new(),
                ),
                hir::Decl::GenericParam(_) => (
                    false,
                    SmallVec::new(),
                ),
            };
            self.tir.decls.push(Decl { param_tys, is_mut, generic_params });
        }

        self.initialize_graph();
        ei_injector!(self, ei);
        di_injector!(self, di);
        // Add type 1 dependencies to the graph
        for i in 0..self.code.hir_code.decls.len() {
            let decl_id = DeclId::new(i);
            let id = di!(decl_id);
            match self.code.hir_code.decls[decl_id] {
                hir::Decl::Parameter { .. } | hir::Decl::Intrinsic { .. } | hir::Decl::Field(_) | hir::Decl::ReturnValue | hir::Decl::GenericParam(_) => {},
                hir::Decl::Static(expr) | hir::Decl::Const(expr) | hir::Decl::Stored { root_expr: expr, .. } => self.tir.graph.add_type1_dep(id, ei!(expr)),
                hir::Decl::Computed { scope, .. } => {
                    let terminal_expr = self.code.hir_code.imper_scopes[scope].terminal_expr;
                    self.tir.graph.add_type1_dep(id, ei!(terminal_expr));
                },
            }
        }
        for i in 0..self.code.hir_code.exprs.len() {
            let expr_id = ExprId::new(i);
            let id = ei!(expr_id);
            match self.code.hir_code.exprs[expr_id] {
                hir::Expr::Void | hir::Expr::IntLit { .. } | hir::Expr::DecLit { .. } | hir::Expr::StrLit { .. }
                    | hir::Expr::CharLit { .. } | hir::Expr::ConstTy(_) | hir::Expr::Mod { .. } | hir::Expr::Import { .. } => {},
                hir::Expr::AddrOf { expr, .. } | hir::Expr::Deref(expr) | hir::Expr::Pointer { expr, .. }
                    | hir::Expr::Cast { expr, .. } | hir::Expr::Ret { expr, .. } => self.tir.graph.add_type1_dep(id, ei!(expr)),
                hir::Expr::DeclRef { ref arguments, id: decl_ref_id } => {
                    let decl_ref = &self.code.hir_code.decl_refs[decl_ref_id];
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
                    let terminal_expr = self.code.hir_code.imper_scopes[scope].terminal_expr;
                    self.tir.graph.add_type1_dep(id, ei!(terminal_expr));
                },
                hir::Expr::If { condition, then_scope, else_scope } => {
                    self.tir.graph.add_type1_dep(id, ei!(condition));
                    let then_expr = self.code.hir_code.imper_scopes[then_scope].terminal_expr;
                    let else_expr = if let Some(else_scope) = else_scope {
                        self.code.hir_code.imper_scopes[else_scope].terminal_expr
                    } else {
                        hir::VOID_EXPR
                    };
                    self.tir.graph.add_type1_dep(id, ei!(then_expr));
                    self.tir.graph.add_type1_dep(id, ei!(else_expr));
                }
                hir::Expr::While { condition, scope } => {
                    self.tir.graph.add_type1_dep(id, ei!(condition));
                    let terminal_expr = self.code.hir_code.imper_scopes[scope].terminal_expr;
                    self.tir.graph.add_type1_dep(id, ei!(terminal_expr));
                },
                hir::Expr::Struct(struct_id) => {
                    for &field in &self.code.hir_code.structs[struct_id].fields {
                        let field = &self.code.hir_code.field_decls[field];
                        self.tir.graph.add_type1_dep(id, ei!(field.ty));
                    }
                },
                hir::Expr::StructLit { ref fields, .. } => {
                    for field in fields {
                        self.tir.graph.add_type1_dep(id, ei!(field.expr));
                    }
                },
            }
        }

        // Split the graph into components
        self.tir.graph.split();

        // TODO: do something better than an array of bools :(
        self.tir.depended_on.resize_with(self.code.hir_code.exprs.len(), || false);

        // Add meta-dependees to graph
        for decl_ref in &self.code.hir_code.decl_refs {
            if let hir::Namespace::MemberRef { base_expr } = decl_ref.namespace {
                self.tir.graph.add_meta_dep(ei!(decl_ref.expr), ei!(base_expr));
            }
        }
    }

    pub fn build_more_tir(&mut self, output: Option<TirGraphOutput>) -> Option<Units> {
        if !self.tir.graph.has_outstanding_components() { return None; }

        add_eval_dep_injector!(self, add_eval_dep);
        ei_injector!(self, ei);

        let items_that_need_dependencies = self.tir.graph.get_items_that_need_dependencies();
        
        for id in items_that_need_dependencies {
            match self.code.hir_code.items[id] {
                hir::Item::Decl(decl_id) => {
                    match self.code.hir_code.decls[decl_id] {
                        hir::Decl::Parameter { .. } | hir::Decl::Static(_) | hir::Decl::Const(_) | hir::Decl::Stored { .. } | hir::Decl::Field(_) | hir::Decl::ReturnValue => {},
                        hir::Decl::GenericParam(_) => {
                            add_eval_dep!(id, hir::TYPE_TYPE);
                        },
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
                            // NOTE: the Some case is handled below this match expression
                            if self.code.hir_code.explicit_tys[decl_id].is_none() {
                                add_eval_dep!(id, hir::VOID_TYPE);
                            }
                        },
                    }
        
                    // NOTE: The computed decl case in the above match expression depends on this!
                    if let Some(ty) = self.code.hir_code.explicit_tys[decl_id] {
                        add_eval_dep!(id, ty);
                    }
                }
                hir::Item::Expr(expr_id) => {
                    match self.code.hir_code.exprs[expr_id] {
                        hir::Expr::Void | hir::Expr::IntLit { .. } | hir::Expr::DecLit { .. } | hir::Expr::StrLit { .. }
                            | hir::Expr::CharLit { .. } | hir::Expr::ConstTy(_) | hir::Expr::AddrOf { .. } | hir::Expr::Deref(_)
                            | hir::Expr::Pointer { .. } | hir::Expr::Set { .. } | hir::Expr::Mod { .. } |  hir::Expr::Import { .. }
                            | hir::Expr::Struct(_) => {},
                        hir::Expr::Cast { ty, .. } => {
                            add_eval_dep!(id, ty);
                        },
                        hir::Expr::StructLit { ty, .. } => {
                            add_eval_dep!(id, ty);
                        },
                        hir::Expr::Ret { decl, .. } => {
                            let ty = decl
                                .and_then(|decl| self.code.hir_code.explicit_tys[decl])
                                .unwrap_or(hir::VOID_TYPE);
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
                        },
                    }
                }
            }
        }

        // Solve for the unit and level of each item
        let levels = self.tir.graph.solve();

        // Output TIR graph if necessary
        if let Some(output) = output {
            self.print_graph(output, &levels).unwrap();
        }

        let mut sp = Subprogram { units: Vec::new(), mock_units: Vec::new(), levels };
        sp.units.resize_with(sp.levels.units.len(), Default::default);

        // Finally, convert HIR items to TIR and add them to the correct spot
        for unit_id in 0..sp.levels.units.len() {
            let unit = &mut sp.units[unit_id];
            for i in 0..sp.levels.units[unit_id].items.len() {
                let item_id = sp.levels.units[unit_id].items[i];
                let level = sp.levels.item_to_levels[&item_id];
                match self.code.hir_code.items[item_id] {
                    hir::Item::Decl(id) => {
                        self.build_tir_decl(&mut unit.items, level, id);
                    }
                    hir::Item::Expr(id) => {
                        if self.tir.depended_on[id] { unit.eval_dependees.push(id); }
                        self.build_tir_expr(&mut unit.items, level, id);
                    }
                }
            }
        }
        for mock_id in 0..sp.levels.mock_units.len() {
            let mock_unit = &sp.levels.mock_units[mock_id];
            let main_expr = match self.code.hir_code.items[mock_unit.item] {
                hir::Item::Expr(id) => id,
                hir::Item::Decl(_) => panic!("Can't have metadependency on a declaration!"),
            };
            let mut items = UnitItems::default();
            self.build_tir_expr(&mut items, mock_unit.item_level, main_expr);
            for &item_id in &mock_unit.deps {
                let level = sp.levels.item_to_levels[&item_id];
                match self.code.hir_code.items[item_id] {
                    hir::Item::Decl(id) => {
                        self.build_tir_decl(&mut items, level, id);
                    }
                    hir::Item::Expr(id) => {
                        self.build_tir_expr(&mut items, level, id);
                    }
                }
            }
            items.unify_sizes();
            sp.mock_units.push(
                MockUnit {
                    main_expr,
                    items,
                },
            );
        }
        self.flush_staged_ret_groups(&mut sp);
        for scope in &self.code.hir_code.imper_scopes {
            for &op in &self.code.blocks[scope.block].ops {
                let op = &self.code.ops[op];
                let item = op.as_hir_item().unwrap();
                match item {
                    // TODO: This is a horrible hack! Instead of looping through all imperative scopes, I should somehow
                    // associate statements with their unit
                    Item::Expr(expr) => if let Some(&unit) = sp.levels.item_to_units.get(&ei!(expr)) {
                        let unit = &mut sp.units[unit as usize];
                        unit.items.stmts.push(Stmt { root_expr: expr });
                    },
                    Item::Decl(decl) => assert!(matches!(self.code.hir_code.decls[decl], hir::Decl::Stored { .. } | hir::Decl::Computed { .. }), "Invalid scope item"),
                }
            }
        }

        for unit in &mut sp.units {
            unit.items.unify_sizes();
        }

        Some(
            Units { units: sp.units, mock_units: sp.mock_units }
        )
    }
}