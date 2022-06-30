use std::ops::{Deref, DerefMut};
use std::collections::{HashMap, HashSet};

use smallvec::SmallVec;
use index_vec::define_index_type;

use dire::source_info::SourceRange;
use dire::hir::{self, Item, Namespace, FieldAssignment, ExprId, DeclId, EnumId, DeclRefId, StructLitId, ModScopeId, StructId, ItemId, ImperScopeId, CastId, GenericParamId, PatternBindingDeclId, Pattern, RETURN_VALUE_DECL};
use dire::{internal_fields, internal_field_decls, InternalField, InternalFieldDecls, InternalNamespace};
use dire::ty::Type;

use crate::driver::Driver;
use crate::dep_vec::{self, DepVec, AnyDepVec};
use crate::index_vec::*;
use crate::TirGraphOutput;
use crate::debug::{self, Message as DvdMessage};

mod graph;
use graph::{Graph, Levels};

pub use graph::CompId;

use dusk_proc_macros::*;

define_index_type!(pub struct TreeId = u32;);

#[derive(Debug)]
pub struct SwitchCase {
    // TODO: might want a different, typechecker-specific data structure for patterns here at some point
    pub pattern: Pattern,
    pub terminal_expr: ExprId,
}


#[derive(Debug)]
pub struct RetGroup { pub ty: ExprId, pub exprs: SmallVec<[ExprId; 1]>, pub decl_range: SourceRange }
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
pub struct ErrorExpr;
#[derive(Debug)]
pub struct Do { pub terminal_expr: ExprId }
#[derive(Debug)]
pub struct AssignedDecl { pub explicit_ty: Option<ExprId>, pub root_expr: ExprId, pub decl_id: DeclId }
#[derive(Debug)]
pub struct PatternBinding { pub binding_id: PatternBindingDeclId, pub scrutinee: ExprId, pub decl_id: DeclId }
#[derive(Debug)]
pub struct Assignment { pub lhs: ExprId, pub rhs: ExprId }
#[derive(Debug)]
pub struct DeclRef { pub decl_ref_id: DeclRefId }
#[derive(Debug)]
pub struct Call { pub callee: ExprId, pub args: SmallVec<[ExprId; 2]> }
#[derive(Debug)]
pub struct FunctionTy { pub param_tys: Vec<ExprId>, pub ret_ty: ExprId }
#[derive(Debug)]
pub struct If { pub condition: ExprId, pub then_expr: ExprId, pub else_expr: ExprId }
#[derive(Debug)]
pub struct While { pub condition: ExprId }
#[derive(Debug)]
pub struct Switch { pub scrutinee: ExprId, pub cases: Vec<SwitchCase> }
#[derive(Debug)]
pub struct Struct { pub field_tys: SmallVec<[ExprId; 2]>, }
#[derive(Debug)]
pub struct StructLit { pub ty: ExprId, pub fields: Vec<FieldAssignment>, pub struct_lit_id: StructLitId, }
#[derive(Debug)]
pub struct Enum { pub variant_payload_tys: SmallVec<[ExprId; 2]> }
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
pub struct BoolLit;
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
    pub bool_lits: Vec<Expr<BoolLit>>,
    pub const_tys: Vec<Expr<ConstTy>>,
    pub error_exprs: Vec<Expr<ErrorExpr>>,
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
    pub pattern_bindings: DepVec<PatternBinding>,
    pub assignments: DepVec<Expr<Assignment>>,
    pub decl_refs: DepVec<Expr<DeclRef>>,
    pub calls: DepVec<Expr<Call>>,
    pub function_tys: DepVec<Expr<FunctionTy>>,
    pub addr_ofs: DepVec<Expr<AddrOf>>,
    pub derefs: DepVec<Expr<Dereference>>,
    pub pointers: DepVec<Expr<Pointer>>,
    pub ifs: DepVec<Expr<If>>,
    pub switches: DepVec<Expr<Switch>>,
    pub structs: DepVec<Expr<Struct>>,
    pub struct_lits: DepVec<Expr<StructLit>>,
    pub enums: DepVec<Expr<Enum>>,
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
            &mut self.assigned_decls, &mut self.assignments, &mut self.decl_refs, &mut self.calls,
            &mut self.addr_ofs, &mut self.derefs, &mut self.pointers, &mut self.ifs, &mut self.dos,
            &mut self.ret_groups, &mut self.casts, &mut self.whiles, &mut self.explicit_rets,
            &mut self.modules, &mut self.imports, &mut self.structs, &mut self.struct_lits,
            &mut self.switches, &mut self.enums, &mut self.pattern_bindings, &mut self.function_tys,
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
    Enum(EnumId),
    Internal(InternalNamespace),
}

#[derive(Debug, Default)]
pub struct Builder {
    pub decls: IndexVec<DeclId, Decl>,
    pub expr_namespaces: HashMap<ExprId, Vec<ExprNamespace>>,
    graph: Graph,
    depended_on: IndexVec<ExprId, bool>,

    staged_ret_groups: HashMap<DeclId, SmallVec<[ExprId; 1]>>,
}

macro_rules! add_eval_dep_injector {
    ($self:expr, $name: ident) => {
        macro_rules! $name {
            ($a:expr, $b:expr) => {{
                $self.tir.graph.add_type4_dep($a, ef!($self, $b.item));
                $self.tir.depended_on[$b] = true;
            }}
        }
    }
}


macro_rules! define_internal_types_internal {
    ($(struct $name:ident {
        $($field_name:ident: $ty:expr),*$(,)?
    })*) => {
        impl Driver {
            pub fn register_internal_fields(&mut self) {
                self.internal_field_decls = InternalFieldDecls {
                    $(
                        $name: internal_field_decls::$name {
                            $(
                                $field_name: self.internal_field(
                                    InternalField::$name(internal_fields::$name::$field_name),
                                    stringify!($field_name),
                                    $ty
                                )
                            ),*
                        }
                    ),*  
                };
            }
            fn find_overloads_in_internal(&self, decl_ref: &hir::DeclRef, ns: InternalNamespace, overloads: &mut HashSet<DeclId>) {
                match ns {
                    $(
                        InternalNamespace::$name => {
                            $({
                                let decl = self.internal_field_decls.$name.$field_name;
                                let name = self.code.hir.names[decl];
                                if name == decl_ref.name {
                                    overloads.insert(decl);
                                }   
                            })*
                        }
                    ),*
                }
            }
        }
    }
}
dire::define_internal_types!(define_internal_types_internal);

impl Driver {
    fn find_overloads_in_mod(&self, decl_ref: &hir::DeclRef, scope: ModScopeId, overloads: &mut HashSet<DeclId>) {
        if let Some(group) = self.code.hir.mod_scopes[scope].decl_groups.get(&decl_ref.name) {
            overloads.extend(
                group.iter()
                    .map(|decl| decl.id)
            );
        }
    }
    fn find_overloads_in_struct(&self, decl_ref: &hir::DeclRef, strukt: StructId, overloads: &mut HashSet<DeclId>) {
        for field in &self.code.hir.structs[strukt].fields {
            if field.name == decl_ref.name {
                overloads.insert(field.decl);
                return;
            }
        }
    }
    fn find_overloads_in_enum(&self, decl_ref: &hir::DeclRef, id: EnumId, overloads: &mut HashSet<DeclId>) {
        for variant in &self.code.hir.enums[id].variants {
            if variant.name == decl_ref.name {
                overloads.insert(variant.decl);
                return;
            }
        }
    }
    fn find_overloads_in_function_parameters(&self, decl_ref: &hir::DeclRef, func: DeclId, overloads: &mut HashSet<DeclId>) {
        match &df!(func.hir) {
            hir::Decl::Computed { params, .. } => {
                for i in params.start.index()..params.end.index() {
                    let decl = DeclId::new(i);
                    let param_name = self.code.hir.names[decl];
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
                        let namespace = &self.code.hir.imper_ns[scope];
                        let result = namespace.decls[0..end_offset].iter()
                            .rev()
                            .find(|&decl| decl.name == decl_ref.name);
                        if let Some(decl) = result {
                            overloads.insert(decl.id);
                            break;
                        }
                    }

                    self.code.hir.imper_ns[scope].parent
                },
                Namespace::Mod(scope_ns) => {
                    let scope = self.code.hir.mod_ns[scope_ns].scope;
                    self.find_overloads_in_mod(decl_ref, scope, &mut overloads);

                    if root_namespace { started_at_mod_scope = true; }
                    self.code.hir.mod_ns[scope_ns].parent
                },
                Namespace::MemberRef { base_expr } => {
                    assert!(root_namespace, "member refs currently must be at the root of a namespace hierarchy");

                    if let Some(expr_namespaces) = self.tir.expr_namespaces.get(&base_expr) {
                        for ns in expr_namespaces {
                            match *ns {
                                ExprNamespace::Mod(scope) => self.find_overloads_in_mod(decl_ref, scope, &mut overloads),
                                ExprNamespace::Struct(id) => self.find_overloads_in_struct(decl_ref, id, &mut overloads),
                                ExprNamespace::Enum(id) => self.find_overloads_in_enum(decl_ref, id, &mut overloads),
                                ExprNamespace::Internal(internal) => self.find_overloads_in_internal(decl_ref, internal, &mut overloads),
                            }
                        }
                    }

                    break;
                },
                Namespace::GenericContext(ns_id) => {
                    let generic_context_ns = &self.code.hir.generic_context_ns[ns_id];
                    let generic_params = &generic_context_ns.generic_params;
                    for i in generic_params.start.index()..generic_params.end.index() {
                        let decl = DeclId::new(i);
                        let param_name = self.code.hir.names[decl];
                        if decl_ref.name == param_name {
                            overloads.insert(decl);
                            break 'find_overloads;
                        }
                    }
                    generic_context_ns.parent
                },
                Namespace::Requirement(ns_id) => {
                    let condition_ns = &self.code.hir.condition_ns[ns_id];
                    self.find_overloads_in_function_parameters(decl_ref, condition_ns.func, &mut overloads);
                    condition_ns.parent
                },
                Namespace::Guarantee(ns_id) => {
                    let condition_ns = &self.code.hir.condition_ns[ns_id];
                    self.find_overloads_in_function_parameters(decl_ref, condition_ns.func, &mut overloads);
                    if decl_ref.name == self.hir.known_idents.return_value && overloads.is_empty() {
                        overloads.insert(RETURN_VALUE_DECL);
                    }
                    condition_ns.parent
                },
                Namespace::Invalid => {
                    panic!("internal compiler error: invalid namespace");
                }
            };

            root_namespace = false;
        }

        overloads.into_iter().collect()
    }

    fn add_types_2_to_4_deps_to_member_ref(&mut self, id: ItemId, decl_ref_id: DeclRefId) {
        add_eval_dep_injector!(self, add_eval_dep);

        let decl_ref = &self.code.hir.decl_refs[decl_ref_id];
        let overloads = self.find_overloads(decl_ref);
        for overload in overloads {
            match df!(overload.hir) {
                hir::Decl::Computed { ref param_tys, .. } => {
                    let ty = self.code.hir.explicit_tys[overload].unwrap_or(hir::VOID_TYPE);
                    add_eval_dep!(id, ty);
                    for &ty in param_tys {
                        add_eval_dep!(id, ty);
                    }
                    self.tir.graph.add_type3_dep(id, df!(overload.item));
                },
                hir::Decl::ReturnValue => {
                    let decl_ref = &self.code.hir.decl_refs[decl_ref_id];
                    match decl_ref.namespace {
                        Namespace::Guarantee(condition_ns_id) => {
                            let func = self.code.hir.condition_ns[condition_ns_id].func;
                            self.tir.graph.add_type3_dep(id, df!(func.item));
                        },
                        _ => panic!("invalid namespace for `return_value`"),
                    }
                },
                hir::Decl::Variant { payload_ty, .. } => {
                    if let Some(payload_ty) = payload_ty {
                        add_eval_dep!(id, payload_ty);
                    }
                    self.tir.graph.add_type3_dep(id, df!(overload.item));
                },
                _ => self.tir.graph.add_type2_dep(id, df!(overload.item)),
            }
        }
    }

    /// IMPORTANT NOTE: When/if we stop adding type3 deps to all items in a function's scope,
    /// we will need to bring back the original idea of meta-dependencies:
    /// https://github.com/dusk-lang/dusk/issues/58
    fn add_type3_scope_dep(&mut self, a: ItemId, b: ImperScopeId) {
        let block = self.code.hir.imper_scopes[b].block;
        for &op in &self.code.blocks[block].ops {
            let op = &self.code.ops[op];
            let item = op.as_hir_item().unwrap();
            match item {
                Item::Expr(expr) => self.tir.graph.add_type3_dep(a, self.code.hir.expr_to_items[expr]),
                Item::Decl(decl) => match df!(decl.hir) {
                    hir::Decl::Stored { .. } => self.tir.graph.add_type3_dep(a, self.code.hir.decl_to_items[decl]),
                    hir::Decl::Computed { .. } => {},
                    _ => panic!("Invalid scope item"),
                },
            }
        }
    }

    fn flush_staged_ret_groups(&mut self, sp: &mut Subprogram) {
        let staged_ret_groups = std::mem::take(&mut self.tir.staged_ret_groups);
        for (decl, exprs) in staged_ret_groups {
            assert!(matches!(df!(decl.hir), hir::Decl::Computed { .. }));

            let ty = self.code.hir.explicit_tys[decl].expect("explicit return statements are not allowed in assigned functions (yet?)");

            let item = self.code.hir.decl_to_items[decl];
            let unit_id = sp.levels.item_to_units[&item];
            let level = sp.levels.item_to_levels[&item];
            let unit = &mut sp.units[unit_id as usize];
            let decl_range = df!(decl.range);
            unit.items.ret_groups.insert(
                level,
                RetGroup { ty, exprs, decl_range },
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
        match &ef!(id.hir) {
            hir::Expr::Void => {},
            hir::Expr::Error => flat_insert_expr!(error_exprs, ErrorExpr),
            hir::Expr::IntLit { .. } => flat_insert_expr!(int_lits, IntLit),
            hir::Expr::DecLit { .. } => flat_insert_expr!(dec_lits, DecLit),
            hir::Expr::StrLit { .. } => flat_insert_expr!(str_lits, StrLit),
            hir::Expr::CharLit { .. } => flat_insert_expr!(char_lits, CharLit),
            hir::Expr::BoolLit { .. } => flat_insert_expr!(bool_lits, BoolLit),
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
            &hir::Expr::DeclRef { id: decl_ref_id } => insert_expr!(decl_refs, DeclRef { decl_ref_id }),
            &hir::Expr::Call { callee, ref arguments } => insert_expr!(calls, Call { callee, args: arguments.clone() }),
            &hir::Expr::FunctionTy { ref param_tys, ret_ty } => insert_expr!(function_tys, FunctionTy { param_tys: param_tys.clone(), ret_ty }),
            &hir::Expr::Set { lhs, rhs } => insert_expr!(assignments, Assignment { lhs, rhs }),
            &hir::Expr::Do { scope } => insert_expr!(dos, Do { terminal_expr: self.code.hir.imper_scopes[scope].terminal_expr }),
            &hir::Expr::If { condition, then_scope, else_scope } => {
                let then_expr = self.code.hir.imper_scopes[then_scope].terminal_expr;
                let else_expr = if let Some(else_scope) = else_scope {
                    self.code.hir.imper_scopes[else_scope].terminal_expr
                } else {
                    hir::VOID_EXPR
                };
                insert_expr!(ifs, If { condition, then_expr, else_expr });
            },
            &hir::Expr::While { condition, .. } => insert_expr!(whiles, While { condition }),
            &hir::Expr::Switch { scrutinee, ref cases, } => {
                let mut tir_cases = Vec::with_capacity(cases.len());
                for case in cases {
                    let case = SwitchCase {
                        pattern: case.pattern.clone(),
                        terminal_expr: self.code.hir.imper_scopes[case.scope].terminal_expr,
                    };
                    tir_cases.push(case);
                }
                insert_expr!(switches, Switch { scrutinee, cases: tir_cases })
            },
            hir::Expr::Mod { .. } => insert_expr!(modules, Module),
            hir::Expr::Import { .. } => insert_expr!(imports, Import),
            &hir::Expr::Struct(struct_id) => {
                let field_tys = self.code.hir.structs[struct_id].fields.iter().map(|field| field.ty).collect();
                insert_expr!(structs, Struct { field_tys })
            },
            &hir::Expr::Enum(enum_id) => {
                let mut variant_payload_tys = SmallVec::new();
                for variant in &self.code.hir.enums[enum_id].variants {
                    if let Some(payload) = variant.payload_ty {
                        variant_payload_tys.push(payload);
                    }
                }
                insert_expr!(enums, Enum { variant_payload_tys })
            },
            &hir::Expr::StructLit { ty, ref fields, id } => {
                insert_expr!(struct_lits, StructLit { ty, fields: fields.clone(), struct_lit_id: id })
            }
        }
    }

    fn build_tir_decl(&mut self, unit: &mut UnitItems, level: u32, id: DeclId) {
        match df!(id.hir) {
            // TODO: Add parameter and field TIR items for (at least) checking that the type of the param is valid
            hir::Decl::Parameter { .. } | hir::Decl::Field { .. } | hir::Decl::Variant { .. } | hir::Decl::ReturnValue | hir::Decl::Intrinsic { .. } | hir::Decl::ComputedPrototype { .. } | hir::Decl::InternalField(_) => {},
            hir::Decl::GenericParam(_) => {
                assert_eq!(level, 0);
                unit.generic_params.push(GenericParam { id });
            },
            hir::Decl::Static(root_expr) | hir::Decl::Const(root_expr) | hir::Decl::Stored { root_expr, .. } => {
                let explicit_ty = self.code.hir.explicit_tys[id];
                unit.assigned_decls.insert(level, AssignedDecl { explicit_ty, root_expr, decl_id: id });
            },
            hir::Decl::PatternBinding { id: binding_id, .. } => {
                let scrutinee = self.code.hir.pattern_binding_decls[binding_id].scrutinee;
                unit.pattern_bindings.insert(level, PatternBinding { binding_id, scrutinee, decl_id: id });
            },
            hir::Decl::Computed { scope, .. } => {
                let terminal_expr = self.code.hir.imper_scopes[scope].terminal_expr;
                self.tir.staged_ret_groups.entry(id).or_default().push(terminal_expr);
            },
        }
    }

    pub fn initialize_tir(&mut self) {
        // Populate `decls`
        for decl in &self.code.hir.decls {
            let mut generic_params = SmallVec::new();
            let (is_mut, param_tys) = match *decl {
                hir::Decl::Computed { ref param_tys, generic_params: ref og_generic_params, .. } => {
                    for i in og_generic_params.start.index()..og_generic_params.end.index() {
                        let decl = DeclId::new(i);
                        let generic_param_id = match df!(decl.hir) {
                            hir::Decl::GenericParam(id) => id,
                            _ => panic!("COMPILER BUG: expected generic parameter"),
                        };
                        generic_params.push(generic_param_id);
                    }

                    (false, param_tys.clone())
                },
                hir::Decl::ComputedPrototype { ref param_tys, .. } => (
                    false,
                    param_tys.clone()
                ),
                hir::Decl::Const(_) | hir::Decl::Parameter { .. } | hir::Decl::ReturnValue | hir::Decl::GenericParam(_) => (
                    false,
                    SmallVec::new(),
                ),
                hir::Decl::Intrinsic { ref param_tys, .. } => (
                    false,
                    param_tys.clone(),
                ),
                hir::Decl::Static(_) => (
                    true,
                    SmallVec::new(),
                ),
                hir::Decl::Stored { is_mut, .. } | hir::Decl::PatternBinding { is_mut, .. } => (
                    is_mut,
                    SmallVec::new(),
                ),
                hir::Decl::Field { .. } => (
                    true,
                    SmallVec::new()
                ),
                hir::Decl::InternalField(_) => (
                    false, // TODO: allow this to be changed per-field
                    SmallVec::new()
                ),
                hir::Decl::Variant { payload_ty, .. } => (
                    false,
                    payload_ty.iter().cloned().collect(),
                ),
            };
            debug::send(|| DvdMessage::DidInitializeTirForDecl { id: DeclId::new(self.tir.decls.len()), is_mut, param_tys: param_tys.iter().cloned().collect() });
            self.tir.decls.push(Decl { param_tys, is_mut, generic_params });
        }

        self.initialize_graph();
        // Add type 1 dependencies to the graph
        for i in 0..self.code.hir.decls.len() {
            let decl_id = DeclId::new(i);
            let id = df!(decl_id.item);
            match df!(decl_id.hir) {
                hir::Decl::Parameter { .. } | hir::Decl::Intrinsic { .. } | hir::Decl::Field { .. } | hir::Decl::ReturnValue | hir::Decl::GenericParam(_) | hir::Decl::Variant { .. } | hir::Decl::ComputedPrototype { .. } | hir::Decl::InternalField(_) => {},
                hir::Decl::PatternBinding { id: _binding_id, .. } => {
                    // let scrutinee = self.code.hir.pattern_binding_decls[binding_id].scrutinee;

                    // // TODO: find out why this seems to cause an infinite loop if it's moved to build_more_tir() and changed to a type 2 dependency
                    // self.tir.graph.add_type1_dep(id, ef!(scrutinee.item));
                },
                hir::Decl::Static(expr) | hir::Decl::Const(expr) | hir::Decl::Stored { root_expr: expr, .. } => self.tir.graph.add_type1_dep(id, ef!(expr.item)),
                hir::Decl::Computed { scope, .. } => {
                    let terminal_expr = self.code.hir.imper_scopes[scope].terminal_expr;
                    self.tir.graph.add_type1_dep(id, ef!(terminal_expr.item));
                },
            }
        }
        for i in 0..self.code.hir.exprs.len() {
            let expr_id = ExprId::new(i);
            let id = ef!(expr_id.item);
            match ef!(expr_id.hir) {
                hir::Expr::Void | hir::Expr::Error | hir::Expr::IntLit { .. } | hir::Expr::DecLit { .. } | hir::Expr::StrLit { .. }
                    | hir::Expr::CharLit { .. } | hir::Expr::BoolLit { .. } | hir::Expr::ConstTy(_) | hir::Expr::Mod { .. }
                    | hir::Expr::Import { .. } => {},
                hir::Expr::AddrOf { expr, .. } | hir::Expr::Deref(expr) | hir::Expr::Pointer { expr, .. }
                    | hir::Expr::Cast { expr, .. } | hir::Expr::Ret { expr, .. } => self.tir.graph.add_type1_dep(id, ef!(expr.item)),
                hir::Expr::DeclRef { id: decl_ref_id, .. } => {
                    let decl_ref = &self.code.hir.decl_refs[decl_ref_id];
                    if let hir::Namespace::MemberRef { base_expr } = decl_ref.namespace {
                        self.tir.graph.add_type1_dep(id, ef!(base_expr.item));
                    }
                },
                hir::Expr::Call { callee, ref arguments } => {
                    self.tir.graph.add_type1_dep(id, ef!(callee.item));
                    for &arg in arguments {
                        self.tir.graph.add_type1_dep(id, ef!(arg.item));

                        // NOTE: if the callee is a declref, it is important for it to have a type 1 dependency on all
                        // arguments, because declrefs do not lock their generic argument values in until pass 2, when
                        // the caller's pass 2 has already passed. This is important because declrefs need to
                        // substitute all generic arguments for the appropriate generic parameter types.
                        self.tir.graph.add_type1_dep(ef!(callee.item), ef!(arg.item));
                    }
                },
                hir::Expr::FunctionTy { ref param_tys, ret_ty } => {
                    for &param_ty in param_tys {
                        self.tir.graph.add_type1_dep(id, ef!(param_ty.item));
                    }
                    self.tir.graph.add_type1_dep(id, ef!(ret_ty.item));
                },
                hir::Expr::Set { lhs, rhs } => {
                    self.tir.graph.add_type1_dep(id, ef!(lhs.item));
                    self.tir.graph.add_type1_dep(id, ef!(rhs.item));
                },
                hir::Expr::Do { scope } => {
                    let terminal_expr = self.code.hir.imper_scopes[scope].terminal_expr;
                    self.tir.graph.add_type1_dep(id, ef!(terminal_expr.item));
                },
                hir::Expr::If { condition, then_scope, else_scope } => {
                    self.tir.graph.add_type1_dep(id, ef!(condition.item));
                    let then_expr = self.code.hir.imper_scopes[then_scope].terminal_expr;
                    let else_expr = if let Some(else_scope) = else_scope {
                        self.code.hir.imper_scopes[else_scope].terminal_expr
                    } else {
                        hir::VOID_EXPR
                    };
                    self.tir.graph.add_type1_dep(id, ef!(then_expr.item));
                    self.tir.graph.add_type1_dep(id, ef!(else_expr.item));
                },
                hir::Expr::Switch { scrutinee, ref cases } => {
                    self.tir.graph.add_type1_dep(id, ef!(scrutinee.item));
                    for case in cases.clone() {
                        let terminal = self.code.hir.imper_scopes[case.scope].terminal_expr;
                        self.tir.graph.add_type1_dep(id, ef!(terminal.item));
                    }
                },
                hir::Expr::While { condition, scope } => {
                    self.tir.graph.add_type1_dep(id, ef!(condition.item));
                    let terminal_expr = self.code.hir.imper_scopes[scope].terminal_expr;
                    self.tir.graph.add_type1_dep(id, ef!(terminal_expr.item));
                },
                hir::Expr::Struct(struct_id) => {
                    for field in &self.code.hir.structs[struct_id].fields {
                        self.tir.graph.add_type1_dep(id, ef!(field.ty.item));
                    }
                },
                hir::Expr::Enum(enum_id) => {
                    for variant in &self.code.hir.enums[enum_id].variants {
                        if let Some(payload_ty) = variant.payload_ty {
                            self.tir.graph.add_type1_dep(id, ef!(payload_ty.item));
                        }
                    }
                },
                hir::Expr::StructLit { ref fields, .. } => {
                    for field in fields {
                        self.tir.graph.add_type1_dep(id, ef!(field.expr.item));
                    }
                },
            }
        }

        // Split the graph into components
        self.tir.graph.split();

        // TODO: do something better than an array of bools :(
        self.tir.depended_on.resize_with(self.code.hir.exprs.len(), || false);

        // Add meta-dependees to graph
        for decl_ref in &self.code.hir.decl_refs {
            if let hir::Namespace::MemberRef { base_expr } = decl_ref.namespace {
                self.tir.graph.add_meta_dep(ef!(decl_ref.expr.item), ef!(base_expr.item));
            }
        }
    }

    pub fn build_more_tir(&mut self, output: Option<TirGraphOutput>) -> Option<Units> {
        debug::send(|| DvdMessage::WillBuildMoreTir);
        if !self.tir.graph.has_outstanding_components() {
            debug::send(|| DvdMessage::DidBuildMoreTir);
            return None;
        }

        add_eval_dep_injector!(self, add_eval_dep);

        let items_that_need_dependencies = self.tir.graph.get_items_that_need_dependencies();
        debug::send(|| DvdMessage::WillAddTirDependencies);
        for id in items_that_need_dependencies {
            match self.code.hir.items[id] {
                hir::Item::Decl(decl_id) => {
                    match df!(decl_id.hir) {
                        hir::Decl::Parameter { .. } | hir::Decl::Static(_) | hir::Decl::Const(_) | hir::Decl::Stored { .. } | hir::Decl::Field { .. } | hir::Decl::ReturnValue | hir::Decl::InternalField(_) /*  | hir::Decl::PatternBinding { .. }*/ => {},
                        hir::Decl::PatternBinding { id: binding_id, .. } => {
                            let scrutinee = self.code.hir.pattern_binding_decls[binding_id].scrutinee;
        
                            // TODO: find out why this seems to cause an infinite loop if it's moved to build_more_tir() and changed to a type 2 dependency
                            self.tir.graph.add_type2_dep(id, ef!(scrutinee.item));
                        },
                        hir::Decl::GenericParam(_) => {
                            add_eval_dep!(id, hir::TYPE_TYPE);
                        },
                        hir::Decl::Intrinsic { ref param_tys, .. } => {
                            for &ty in param_tys {
                                add_eval_dep!(id, ty);
                            }
                        },
                        hir::Decl::Variant { payload_ty, .. } => {
                            if let Some(payload_ty) = payload_ty {
                                add_eval_dep!(id, payload_ty);
                            }
                        }
                        hir::Decl::Computed { scope, ref param_tys, .. } => {
                            for &ty in param_tys {
                                add_eval_dep!(id, ty);
                            }
                            self.add_type3_scope_dep(id, scope);
                            // NOTE: the Some case is handled below this match expression
                            if self.code.hir.explicit_tys[decl_id].is_none() {
                                add_eval_dep!(id, hir::VOID_TYPE);
                            }
                        },
                        hir::Decl::ComputedPrototype { ref param_tys, .. } => {
                            for &ty in param_tys {
                                add_eval_dep!(id, ty);
                            }
                            // NOTE: the Some case is handled below this match expression
                            if self.code.hir.explicit_tys[decl_id].is_none() {
                                add_eval_dep!(id, hir::VOID_TYPE);
                            }
                        },
                    }
        
                    // NOTE: The computed decl case in the above match expression depends on this!
                    if let Some(ty) = self.code.hir.explicit_tys[decl_id] {
                        add_eval_dep!(id, ty);
                    }
                }
                hir::Item::Expr(expr_id) => {
                    match ef!(expr_id.hir) {
                        hir::Expr::Void | hir::Expr::Error | hir::Expr::IntLit { .. } | hir::Expr::DecLit { .. } | hir::Expr::StrLit { .. }
                            | hir::Expr::CharLit { .. } | hir::Expr::BoolLit { .. } | hir::Expr::ConstTy(_) | hir::Expr::AddrOf { .. }
                            | hir::Expr::Deref(_) | hir::Expr::Pointer { .. } | hir::Expr::Set { .. } | hir::Expr::Mod { .. }
                            | hir::Expr::Import { .. } | hir::Expr::Struct(_) | hir::Expr::Enum(_) | hir::Expr::Call { .. }
                            | hir::Expr::FunctionTy { .. } => {},
                        hir::Expr::Cast { ty, .. } => {
                            add_eval_dep!(id, ty);
                        },
                        hir::Expr::StructLit { ty, .. } => {
                            add_eval_dep!(id, ty);
                        },
                        hir::Expr::Ret { decl, .. } => {
                            let ty = decl
                                .and_then(|decl| self.code.hir.explicit_tys[decl])
                                .unwrap_or(hir::VOID_TYPE);
                            add_eval_dep!(id, ty);
                        }
                        hir::Expr::DeclRef { id: decl_ref_id, .. } => {
                            self.add_types_2_to_4_deps_to_member_ref(id, decl_ref_id);
                        },
                        hir::Expr::Do { scope } => {
                            self.add_type3_scope_dep(id, scope);
                        },
                        hir::Expr::Switch { ref cases, .. } => {
                            for case in cases.clone() {
                                self.add_type3_scope_dep(id, case.scope);
                            }
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
        debug::send(|| DvdMessage::DidAddTirDependencies);

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
                match self.code.hir.items[item_id] {
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
            let main_expr = match self.code.hir.items[mock_unit.item] {
                hir::Item::Expr(id) => id,
                hir::Item::Decl(_) => panic!("Can't have metadependency on a declaration!"),
            };
            let mut items = UnitItems::default();
            self.build_tir_expr(&mut items, mock_unit.item_level, main_expr);
            for &item_id in &mock_unit.deps {
                let level = sp.levels.item_to_levels[&item_id];
                match self.code.hir.items[item_id] {
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
        for scope in &self.code.hir.imper_scopes {
            for &op in &self.code.blocks[scope.block].ops {
                let op = &self.code.ops[op];
                let item = op.as_hir_item().unwrap();
                match item {
                    // TODO: This is a horrible hack! Instead of looping through all imperative scopes, I should somehow
                    // associate statements with their unit
                    Item::Expr(expr) => if let Some(&unit) = sp.levels.item_to_units.get(&ef!(expr.item)) {
                        let unit = &mut sp.units[unit as usize];
                        unit.items.stmts.push(Stmt { root_expr: expr });
                    },
                    Item::Decl(decl) => assert!(matches!(df!(decl.hir), hir::Decl::Stored { .. } | hir::Decl::Computed { .. }), "Invalid scope item"),
                }
            }
        }

        for unit in &mut sp.units {
            unit.items.unify_sizes();
        }

        debug::send(|| DvdMessage::DidBuildMoreTir);
        Some(
            Units { units: sp.units, mock_units: sp.mock_units }
        )
    }
}