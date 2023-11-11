use std::ops::{Range, Deref, DerefMut};
use std::collections::{HashMap, HashSet};
use std::mem;

use smallvec::SmallVec;
use index_vec::define_index_type;
use string_interner::DefaultSymbol as Sym;

use crate::source_info::SourceRange;
use crate::ast::{self, Item, Namespace, FieldAssignment, ExprId, DeclId, DeclRefId, StructLitId, ItemId, ImperScopeId, CastId, GenericParamId, PatternBindingDeclId, Pattern, NewNamespaceId, RETURN_VALUE_DECL, ParamList, StructId};
use crate::internal_types::{internal_fields, internal_field_decls, InternalField, InternalFieldDecls, InternalNamespace};
use crate::ty::Type;
use crate::ty::StructType;

use crate::dvd::{Message as DvdMessage, self};

use crate::driver::Driver;
use crate::dep_vec::{self, DepVec, AnyDepVec};
use crate::index_vec::*;
use crate::new_code::NewCode;

mod graph;
use graph::{Graph, Levels};

use dusk_proc_macros::*;

define_index_type!(pub struct TreeId = u32;);
define_index_type!(pub struct CompId = u32;);

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
pub struct Stmt { pub root_expr: ExprId, pub has_semicolon: bool }
#[derive(Debug)]
pub struct ErrorExpr;
#[derive(Debug)]
pub struct Do { pub terminal_expr: ExprId }
#[derive(Debug)]
pub struct AssignedDecl { pub explicit_ty: Option<ExprId>, pub root_expr: ExprId, pub decl_id: DeclId }
#[derive(Debug)]
pub struct PatternBinding { pub binding_id: PatternBindingDeclId, pub scrutinee: ExprId, pub decl_id: DeclId }
#[derive(Debug)]
pub struct For { pub binding_decl: DeclId, pub binding_explicit_ty: Option<ExprId>, pub lower_bound: ExprId, pub upper_bound: ExprId }
#[derive(Debug)]
pub struct Assignment { pub lhs: ExprId, pub rhs: ExprId }
#[derive(Debug)]
pub struct DeclRef { pub decl_ref_id: DeclRefId, pub explicit_generic_args: Option<Vec<ExprId>> }
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
pub struct Module {
    pub extern_library_path: Option<ExprId>,
}
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
pub struct Break;
#[derive(Debug)]
pub struct Continue;
#[derive(Debug)]
pub struct ConstExpr(pub Type);
#[derive(Debug)]
pub struct GenericParam { pub id: DeclId }
#[derive(Debug)]
pub struct FunctionDecl { pub id: DeclId }

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
    pub param_list: ParamList,
    pub generic_params: Range<GenericParamId>,
    pub is_mut: bool,
}

struct Subprogram {
    units: Vec<Unit>,
    mock_units: Vec<MockUnit>,
    levels: Levels,
}

#[derive(Debug)]
pub enum TirError {
    DependencyCycle,
}

#[derive(Default, Debug)]
pub struct UnitItems {
    pub int_lits: Vec<Expr<IntLit>>,
    pub dec_lits: Vec<Expr<DecLit>>,
    pub str_lits: Vec<Expr<StrLit>>,
    pub char_lits: Vec<Expr<CharLit>>,
    pub bool_lits: Vec<Expr<BoolLit>>,
    pub breaks: Vec<Expr<Break>>,
    pub continues: Vec<Expr<Continue>>,
    pub consts: Vec<Expr<ConstExpr>>,
    pub error_exprs: Vec<Expr<ErrorExpr>>,
    pub generic_params: Vec<GenericParam>,
    pub stmts: Vec<Stmt>,
    pub func_decls: Vec<FunctionDecl>,
    pub explicit_rets: DepVec<Expr<ExplicitRet>>,
    pub ret_groups: DepVec<RetGroup>,
    pub casts: DepVec<Expr<Cast>>,
    pub whiles: DepVec<Expr<While>>,
    pub fors: DepVec<Expr<For>>,
    pub modules: DepVec<Expr<Module>>,
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
    fn unify_sizes(&mut self) {
        dep_vec::unify_sizes(&mut [
            &mut self.assigned_decls, &mut self.assignments, &mut self.decl_refs, &mut self.calls,
            &mut self.addr_ofs, &mut self.derefs, &mut self.pointers, &mut self.ifs, &mut self.dos,
            &mut self.ret_groups, &mut self.casts, &mut self.whiles, &mut self.fors, &mut self.explicit_rets,
            &mut self.modules, &mut self.structs, &mut self.struct_lits, &mut self.switches, &mut self.enums,
            &mut self.pattern_bindings, &mut self.function_tys,
        ]);
    }
}

impl UnitItems {
    pub fn num_levels(&self) -> u32 {
        // Assumption: all DepVecs in the unit have the same number of levels!
        self.assigned_decls.num_levels()
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

#[derive(Debug, Clone, Copy)]
pub enum NewNamespaceRefKind {
    Instance,
    Static,
}

/// The namespace "inside" an expression,
/// i.e. what are all the declarations that you might be able to access as members of an expression
#[derive(Debug)]
pub enum ExprNamespace {
    // TODO: merge this with `New(_,_)` variant (requires some additional behaviour)
    Mod(NewNamespaceId),
    Internal(InternalNamespace),
    // TODO: it would be really nice if this could be expanded to subsume not only all other variants here, but also
    // the complicated web of `*Ns`, `*NsId` and `Namespace` values
    New(NewNamespaceId, NewNamespaceRefKind),
    Error,
}

#[derive(Debug)]
pub enum ExprMacroInfo {
    Struct(StructType),
}

#[derive(Debug, Default)]
pub struct Builder {
    pub decls: IndexVec<DeclId, Decl>,
    pub expr_namespaces: HashMap<ExprId, Vec<ExprNamespace>>,
    // E.g., in `expr { }`, maps `expr` to the possible "macros" it might refer to. Currently this only works with
    // struct literals, but could be expanded to support arbitrary procedural macros in the future.
    pub expr_macro_info: HashMap<ExprId, Vec<ExprMacroInfo>>,
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


macro_rules! define_legacy_internal_types_internal {
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
            fn find_overloads_in_internal(&self, lookup: &NameLookup, ns: InternalNamespace, overloads: &mut HashSet<FoundOverload>) {
                match ns {
                    $(
                        InternalNamespace::$name => {
                            $({
                                let decl = self.internal_field_decls.$name.$field_name;
                                let name = self.code.ast.names[decl];
                                if self.name_matches(lookup, name) {
                                    overloads.insert(decl.into());
                                }   
                            })*
                        }
                    ),*
                }
            }
        }
    }
}
define_legacy_internal_types!(define_legacy_internal_types_internal);

pub enum NameLookup {
    Exact(Sym),
    // TODO: should we actually only look up the beginning of symbols?
    Beginning(String),
}

#[derive(Hash, PartialEq, Eq)]
pub enum OverloadScope {
    Normal,

    Member { base: ExprId },
    Field { base: ExprId, struct_id: StructId, field_index: usize },
}

#[derive(Hash, PartialEq, Eq)]
pub struct FoundOverload {
    pub decl: DeclId,
    pub scope: OverloadScope,
}

impl From<DeclId> for FoundOverload {
    fn from(value: DeclId) -> Self {
        Self { decl: value, scope: OverloadScope::Normal }
    }
}

impl Driver {
    // See comment about `should_traverse_blanket_uses` later in this file
    fn find_overloads_in_mod(&self, name: &NameLookup, scope: NewNamespaceId, should_traverse_blanket_uses: bool, overloads: &mut HashSet<FoundOverload>) -> bool {
        let scope = &self.code.ast.new_namespaces[scope];
        for decl in &scope.static_decls {
            if self.name_matches(name, decl.name) {
                overloads.insert(decl.decl.into());
            }
        }
        if should_traverse_blanket_uses {
            let mut all_unresolved = !scope.blanket_uses.is_empty();
            for &used_namespace in &scope.blanket_uses {
                all_unresolved &= !self.find_overloads_in_namespace(name, used_namespace, true, overloads);
            }
            !all_unresolved
        } else {
            true
        }
    }
    fn find_overloads_in_new_namespace(&self, name: &NameLookup, base: ExprId, id: NewNamespaceId, kind: NewNamespaceRefKind, overloads: &mut HashSet<FoundOverload>) {
        let ns = &self.code.ast.new_namespaces[id];
        
        match kind {
            NewNamespaceRefKind::Instance => for decl in &ns.instance_decls {
                let decl_name = self.code.ast.names[decl.decl];
                if self.name_matches(name, decl_name) {
                    let scope = if let Some(field_info) = &decl.field_info {
                        OverloadScope::Field { base, struct_id: field_info.struct_id, field_index: field_info.field_index }
                    } else {
                        OverloadScope::Member { base }
                    };
                    let overload = FoundOverload {
                        decl: decl.decl,
                        scope,
                    };
                    overloads.insert(overload);
                    return;
                }
            },
            NewNamespaceRefKind::Static => for decl in &ns.static_decls {
                let decl_name = self.code.ast.names[decl.decl];
                if self.name_matches(name, decl_name) {
                    overloads.insert(decl.decl.into());
                    return;
                }
            }
        };
    }
    fn find_overloads_in_function_parameters(&self, name: &NameLookup, func: DeclId, overloads: &mut HashSet<FoundOverload>) {
        match &df!(func.ast) {
            ast::Decl::Computed { params, .. } => {
                for decl in range_iter(params.clone()) {
                    let param_name = self.code.ast.names[decl];
                    
                    if self.name_matches(name, param_name) {
                        overloads.insert(decl.into());
                    }
                }
            },
            _ => panic!("Can only have requirements clause on computed decls"),
        }
    }

    // `should_traverse_blanket_uses` keeps track of whether we should look for overloads in blanket uses (which
    // currently don't have a syntax, and are only used for the prelude). The reason for this is, code in a module X
    // wants to be able to traverse its own namespace tree up to and including the blanket use of the prelude. However,
    // code outside module X does *not* want to see items from the prelude re-exported. E.g. mod{}.print("Hello, world")
    //
    // TODO: more robust support for access control
    fn find_overloads_in_namespace(&self, name: &NameLookup, namespace: Namespace, mut should_traverse_blanket_uses: bool, overloads: &mut HashSet<FoundOverload>) -> bool {
        let mut started_at_mod_scope = false;
        let mut root_namespace = true;
        let mut namespace = Some(namespace);
        'find_overloads: while let Some(ns) = namespace {
            namespace = match ns {
                Namespace::Imper { scope, end_offset } => {
                    if !started_at_mod_scope {
                        let namespace = &self.code.ast.imper_ns[scope];
                        let result = namespace.decls[..end_offset].iter()
                            .rev()
                            .find(|&decl| self.name_matches(name, decl.name));
                        if let Some(decl) = result {
                            overloads.insert(decl.id.into());
                            break;
                        }
                    }

                    self.code.ast.imper_ns[scope].parent
                },
                Namespace::Mod(scope_ns) => {
                    let scope = self.code.ast.mod_ns[scope_ns].scope;
                    self.find_overloads_in_mod(name, scope, should_traverse_blanket_uses, overloads);

                    if root_namespace { started_at_mod_scope = true; }
                    self.code.ast.mod_ns[scope_ns].parent
                },
                Namespace::MemberRef { base_expr } => {
                    assert!(root_namespace, "member refs currently must be at the root of a namespace hierarchy");

                    should_traverse_blanket_uses = false;

                    if let Some(expr_namespaces) = self.tir.expr_namespaces.get(&base_expr) {
                        for ns in expr_namespaces {
                            match *ns {
                                ExprNamespace::Mod(scope) => {
                                    if !self.find_overloads_in_mod(name, scope, should_traverse_blanket_uses, overloads) {
                                        return false
                                    }
                                },
                                ExprNamespace::Internal(internal) => self.find_overloads_in_internal(name, internal, overloads),
                                ExprNamespace::New(id, kind) => self.find_overloads_in_new_namespace(name, base_expr, id, kind, overloads),
                                ExprNamespace::Error => return false,
                            }
                        }
                    }

                    break;
                },
                Namespace::GenericContext(ns_id) => {
                    let generic_context_ns = &self.code.ast.generic_context_ns[ns_id];
                    let generic_params = &generic_context_ns.generic_params;
                    for decl in range_iter(generic_params.clone()) {
                        let param_name = self.code.ast.names[decl];
                        if self.name_matches(name, param_name) {
                            overloads.insert(decl.into());
                            break 'find_overloads;
                        }
                    }
                    generic_context_ns.parent
                },
                Namespace::Requirement(ns_id) => {
                    let condition_ns = &self.code.ast.condition_ns[ns_id];
                    self.find_overloads_in_function_parameters(name, condition_ns.func, overloads);
                    condition_ns.parent
                },
                Namespace::Guarantee(ns_id) => {
                    let condition_ns = &self.code.ast.condition_ns[ns_id];
                    self.find_overloads_in_function_parameters(name, condition_ns.func, overloads);
                    if self.name_matches(name, self.ast.known_idents.return_value) && overloads.is_empty() {
                        overloads.insert(RETURN_VALUE_DECL.into());
                    }
                    condition_ns.parent
                },
                Namespace::Invalid => {
                    panic!("internal compiler error: invalid namespace");
                }
            };

            root_namespace = false;
        }
        true
    }
    fn name_matches(&self, lookup: &NameLookup, actual: Sym) -> bool {
        match lookup {
            NameLookup::Beginning(beginning) => {
                let actual = self.interner.resolve(actual).unwrap();
                actual.starts_with(beginning)
            },
            &NameLookup::Exact(name) => name == actual,
        }
    }

    /// Returns the overloads for a name in a namespace, if they are known (they won't be if it's an unresolved member ref)
    /// Returns None if the namespace comes from a memberref AND the memberref base expression has an error (e.g., invalid variable reference)
    pub fn find_overloads(&self, namespace: Namespace, name: &NameLookup) -> Option<Vec<FoundOverload>> {
        let mut overloads = HashSet::new();
        self.find_overloads_in_namespace(name, namespace, true, &mut overloads)
            .then(|| overloads.into_iter().collect())
    }

    fn add_types_2_to_4_deps_to_member_ref(&mut self, id: ItemId, decl_ref_id: DeclRefId) {
        add_eval_dep_injector!(self, add_eval_dep);

        let decl_ref = &self.code.ast.decl_refs[decl_ref_id];
        let overloads = self.find_overloads(decl_ref.namespace, &NameLookup::Exact(decl_ref.name)).unwrap_or_default();
        for overload in overloads {
            match df!(overload.decl.ast) {
                ast::Decl::Computed { ref param_tys, .. } => {
                    let ty = self.code.ast.explicit_tys[overload.decl].unwrap_or(ast::VOID_TYPE);
                    add_eval_dep!(id, ty);
                    for &ty in param_tys {
                        add_eval_dep!(id, ty);
                    }
                    self.tir.graph.add_type3_dep(id, df!(overload.decl.item));
                },
                ast::Decl::ReturnValue => {
                    let decl_ref = &self.code.ast.decl_refs[decl_ref_id];
                    match decl_ref.namespace {
                        Namespace::Guarantee(condition_ns_id) => {
                            let func = self.code.ast.condition_ns[condition_ns_id].func;
                            self.tir.graph.add_type3_dep(id, df!(func.item));
                        },
                        _ => panic!("invalid namespace for `return_value`"),
                    }
                },
                ast::Decl::Variant { payload_ty, .. } => {
                    if let Some(payload_ty) = payload_ty {
                        add_eval_dep!(id, payload_ty);
                    }
                    self.tir.graph.add_type3_dep(id, df!(overload.decl.item));
                },
                _ => self.tir.graph.add_type2_dep(id, df!(overload.decl.item)),
            }
        }
    }

    /// IMPORTANT NOTE: When/if we stop adding type3 deps to all items in a function's scope,
    /// we will need to bring back the original idea of meta-dependencies:
    /// https://github.com/dusklang/dusk/issues/58
    /// 
    /// Update on Apr. 20, 2023: I feel like we already have the "original idea of meta-dependencies", and I don't
    /// see how they help in this case? The actual issue (as I understand it now, years later) is that we currently
    /// conservatively add type3 deps from each function decl to all items in its scope, because if we need to generate
    /// MIR for and then call the function in the interpreter, it's important for us to have already typechecked all of
    /// the items in its scope. The reason this is overly conservative is we might not need to call the function at
    /// compile-time at all, and even if we do, there is no reason to defer typechecking of the function declaration
    /// until after its items. In other words, the call is what truly depends on the items having been typechecked; not
    /// the function itself. I think/hope this could also be a cause for some of the mysterious dependency cycles
    /// that have plagued the compiler for some time (though frankly I don't have a good reason to think that).
    fn add_type3_scope_dep(&mut self, a: ItemId, b: ImperScopeId) {
        let block = self.code.ast.imper_scopes[b].block;
        for &op in &self.code.blocks[block].ops {
            let op = &self.code.ops[op];
            let item = op.as_ast_item().unwrap();
            match item {
                Item::Expr(expr) => self.tir.graph.add_type3_dep(a, self.code.ast.expr_to_items[expr]),
                Item::Decl(decl) => match df!(decl.ast) {
                    ast::Decl::Stored { .. } => self.tir.graph.add_type3_dep(a, self.code.ast.decl_to_items[decl]),
                    ast::Decl::Computed { .. } => {},
                    _ => panic!("Invalid scope item"),
                },
            }
        }
    }

    fn flush_staged_ret_groups(&mut self, sp: &mut Subprogram) {
        let staged_ret_groups = mem::take(&mut self.tir.staged_ret_groups);
        for (decl, exprs) in staged_ret_groups {
            assert!(matches!(df!(decl.ast), ast::Decl::Computed { .. }));

            let ty = self.code.ast.explicit_tys[decl].expect("explicit return statements are not allowed in assigned functions (yet?)");

            let item = self.code.ast.decl_to_items[decl];
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
        match &ef!(id.ast) {
            ast::Expr::Void => {},
            ast::Expr::Error => flat_insert_expr!(error_exprs, ErrorExpr),
            ast::Expr::IntLit { .. } => flat_insert_expr!(int_lits, IntLit),
            ast::Expr::DecLit { .. } => flat_insert_expr!(dec_lits, DecLit),
            ast::Expr::StrLit { .. } => flat_insert_expr!(str_lits, StrLit),
            ast::Expr::CharLit { .. } => flat_insert_expr!(char_lits, CharLit),
            ast::Expr::BoolLit { .. } => flat_insert_expr!(bool_lits, BoolLit),
            ast::Expr::Break(_) => flat_insert_expr!(breaks, Break),
            ast::Expr::Continue(_) => flat_insert_expr!(continues, Continue),
            ast::Expr::Const(val) => flat_insert_expr!(consts, ConstExpr(val.ty())),
            &ast::Expr::AddrOf { expr, is_mut } => insert_expr!(addr_ofs, AddrOf { expr, is_mut }),
            &ast::Expr::Deref(expr) => insert_expr!(derefs, Dereference { expr }),
            &ast::Expr::Pointer { expr, .. } => insert_expr!(pointers, Pointer { expr }),
            &ast::Expr::Cast { expr, ty, cast_id } => insert_expr!(casts, Cast { expr, ty, cast_id }),
            &ast::Expr::Ret { expr, decl } => {
                if let Some(decl) = decl {
                    self.tir.staged_ret_groups.entry(decl).or_default().push(expr);
                }
                insert_expr!(explicit_rets, ExplicitRet)
            }
            &ast::Expr::DeclRef { id: decl_ref_id, ref explicit_generic_args } => insert_expr!(decl_refs, DeclRef { decl_ref_id, explicit_generic_args: explicit_generic_args.clone() }),
            &ast::Expr::Call { callee, ref arguments } => insert_expr!(calls, Call { callee, args: arguments.clone() }),
            &ast::Expr::FunctionTy { ref param_tys, ret_ty, .. } => insert_expr!(function_tys, FunctionTy { param_tys: param_tys.clone(), ret_ty }),
            &ast::Expr::Set { lhs, rhs } => insert_expr!(assignments, Assignment { lhs, rhs }),
            &ast::Expr::Do { scope } => insert_expr!(dos, Do { terminal_expr: self.code.ast.imper_scopes[scope].terminal_expr }),
            &ast::Expr::If { condition, then_scope, else_scope } => {
                let then_expr = self.code.ast.imper_scopes[then_scope].terminal_expr;
                let else_expr = if let Some(else_scope) = else_scope {
                    self.code.ast.imper_scopes[else_scope].terminal_expr
                } else {
                    ast::VOID_EXPR
                };
                insert_expr!(ifs, If { condition, then_expr, else_expr });
            },
            &ast::Expr::While { condition, .. } => insert_expr!(whiles, While { condition }),
            &ast::Expr::For { binding, lower_bound, upper_bound, .. } => {
                let explicit_ty = self.code.ast.explicit_tys[binding];
                insert_expr!(fors, For { binding_decl: binding, binding_explicit_ty: explicit_ty, lower_bound, upper_bound })
            },
            &ast::Expr::Switch { scrutinee, ref cases, } => {
                let mut tir_cases = Vec::with_capacity(cases.len());
                for case in cases {
                    let case = SwitchCase {
                        pattern: case.pattern.clone(),
                        terminal_expr: self.code.ast.imper_scopes[case.scope].terminal_expr,
                    };
                    tir_cases.push(case);
                }
                insert_expr!(switches, Switch { scrutinee, cases: tir_cases })
            },
            &ast::Expr::Mod { extern_library_path, .. } => insert_expr!(modules, Module { extern_library_path }),
            &ast::Expr::Struct(struct_id) => {
                let field_tys = self.code.ast.structs[struct_id].fields.iter().map(|field| field.ty).collect();
                insert_expr!(structs, Struct { field_tys })
            },
            &ast::Expr::Enum(enum_id) => {
                let mut variant_payload_tys = SmallVec::new();
                for variant in &self.code.ast.enums[enum_id].variants {
                    if let Some(payload) = variant.payload_ty {
                        variant_payload_tys.push(payload);
                    }
                }
                insert_expr!(enums, Enum { variant_payload_tys })
            },
            &ast::Expr::StructLit { ty, ref fields, id } => {
                insert_expr!(struct_lits, StructLit { ty, fields: fields.clone(), struct_lit_id: id })
            }
        }
    }

    fn build_tir_decl(&mut self, unit: &mut UnitItems, level: u32, id: DeclId) {
        match df!(id.ast) {
            // TODO: Add parameter and field TIR items for (at least) checking that the type of the param is valid
            // LoopBinding doesn't require a TIR decl because it is current checked by its parent `For` expr
            ast::Decl::Parameter { .. } | ast::Decl::Field { .. } | ast::Decl::Variant { .. } |
                ast::Decl::ReturnValue | ast::Decl::LegacyIntrinsic { .. } | ast::Decl::Intrinsic(_) | ast::Decl::MethodIntrinsic(_) | ast::Decl::InternalField(_) | ast::Decl::LoopBinding { .. } | ast::Decl::ObjcClassRef { .. } => {},
            ast::Decl::GenericParam(_) => {
                assert_eq!(level, 0);
                unit.generic_params.push(GenericParam { id });
            },
            ast::Decl::Static(root_expr) | ast::Decl::Const { assigned_expr: root_expr, .. } | ast::Decl::Stored { root_expr, .. } => {
                let explicit_ty = self.code.ast.explicit_tys[id];
                unit.assigned_decls.insert(level, AssignedDecl { explicit_ty, root_expr, decl_id: id });
            },
            ast::Decl::PatternBinding { id: binding_id, .. } => {
                let scrutinee = self.code.ast.pattern_binding_decls[binding_id].scrutinee;
                unit.pattern_bindings.insert(level, PatternBinding { binding_id, scrutinee, decl_id: id });
            },
            ast::Decl::Computed { scope, .. } => {
                let terminal_expr = self.code.ast.imper_scopes[scope].terminal_expr;
                self.tir.staged_ret_groups.entry(id).or_default().push(terminal_expr);
                unit.func_decls.push(FunctionDecl { id });
            },
            ast::Decl::ComputedPrototype { .. } => {
                unit.func_decls.push(FunctionDecl { id });
            },
        }
    }

    pub fn initialize_tir(&mut self, new_code: &NewCode) {
        // Populate `decls`
        for id in range_iter(new_code.decls.clone()) {
            let decl = &self.code.ast.decls[id];
            let mut generic_params = empty_range();
            let (is_mut, param_list) = match *decl {
                ast::Decl::Computed { ref param_tys, generic_params: ref og_generic_params, .. } => {
                    generic_params = og_generic_params.clone();

                    (false, ParamList { param_tys: param_tys.clone(), has_c_variadic_param: false })
                },
                ast::Decl::Const { generic_params: ref og_generic_params, .. } => {
                    generic_params = og_generic_params.clone();
                    (
                        false,
                        ParamList::default(),
                    )
                },
                ast::Decl::ComputedPrototype { ref param_list, .. } => (
                    false,
                    param_list.clone(),
                ),
                ast::Decl::Parameter { .. } | ast::Decl::ReturnValue | ast::Decl::GenericParam(_) | ast::Decl::ObjcClassRef { .. } => (
                    false,
                    ParamList::default(),
                ),
                ast::Decl::LegacyIntrinsic { ref param_tys, .. } => (
                    false,
                    ParamList { param_tys: param_tys.clone(), has_c_variadic_param: false },
                ),
                ast::Decl::Intrinsic(id) => {
                    let param_tys = &self.code.ast.intrinsics[id].param_tys;
                    (false, ParamList { param_tys: param_tys.clone(), has_c_variadic_param: false })
                },
                ast::Decl::MethodIntrinsic(id) => {
                    let param_tys = SmallVec::from(&self.code.ast.intrinsics[id].param_tys[1..]);
                    (false, ParamList { param_tys, has_c_variadic_param: false })
                },
                ast::Decl::Static(_) => (
                    true,
                    ParamList::default(),
                ),
                ast::Decl::Stored { is_mut, .. } | ast::Decl::PatternBinding { is_mut, .. } => (
                    is_mut,
                    ParamList::default(),
                ),
                ast::Decl::Field { .. } => (
                    true,
                    ParamList::default(),
                ),
                ast::Decl::InternalField(_) => (
                    false, // TODO: allow this to be changed per-field
                    ParamList::default(),
                ),
                ast::Decl::Variant { payload_ty, .. } => (
                    false,
                    ParamList { param_tys: payload_ty.iter().cloned().collect(), has_c_variadic_param: false },
                ),
                ast::Decl::LoopBinding { is_mut, .. } => (
                    is_mut,
                    ParamList::default(),
                ),
            };
            dvd::send(|| DvdMessage::DidInitializeTirForDecl { id, is_mut, param_tys: param_list.param_tys.iter().cloned().collect() });
            self.tir.decls.push_at(id, Decl { param_list, is_mut, generic_params });
        }

        self.initialize_graph();

        // Add type 1 dependencies and meta-dependencies to the graph
        dvd::send(|| DvdMessage::WillAddType1Dependencies);
        for decl_id in range_iter(new_code.decls.clone()) {
            let id = df!(decl_id.item);
            match df!(decl_id.ast) {
                // NOTE: type 1 dependencies are currently added to LoopBinding by its parent `for` loop; see below.
                ast::Decl::Parameter { .. } | ast::Decl::LegacyIntrinsic { .. } | ast::Decl::Intrinsic(_) | ast::Decl::MethodIntrinsic(_) | ast::Decl::Field { .. } | ast::Decl::ReturnValue | ast::Decl::GenericParam(_) | ast::Decl::Variant { .. } | ast::Decl::ComputedPrototype { .. } | ast::Decl::InternalField(_) | ast::Decl::LoopBinding { .. } | ast::Decl::ObjcClassRef { .. } => {},
                ast::Decl::PatternBinding { id: _binding_id, .. } => {
                    // let scrutinee = self.code.ast.pattern_binding_decls[binding_id].scrutinee;

                    // // TODO: find out why this seems to cause an infinite loop if it's moved to build_more_tir() and changed to a type 2 dependency
                    // self.tir.graph.add_type1_dep(id, ef!(scrutinee.item));
                },
                ast::Decl::Static(expr) | ast::Decl::Const { assigned_expr: expr, .. } | ast::Decl::Stored { root_expr: expr, .. } => self.tir.graph.add_type1_dep(id, ef!(expr.item)),
                ast::Decl::Computed { scope, .. } => {
                    let terminal_expr = self.code.ast.imper_scopes[scope].terminal_expr;
                    self.tir.graph.add_type1_dep(id, ef!(terminal_expr.item));
                },
            }
        }
        for expr_id in range_iter(new_code.exprs.clone()) {
            let id = ef!(expr_id.item);
            match ef!(expr_id.ast) {
                ast::Expr::Void | ast::Expr::Error | ast::Expr::IntLit { .. } | ast::Expr::DecLit { .. } | ast::Expr::StrLit { .. }
                    | ast::Expr::CharLit { .. } | ast::Expr::BoolLit { .. } | ast::Expr::Const(_) | ast::Expr::Break(_) | ast::Expr::Continue(_) => {},
                ast::Expr::Mod { extern_library_path, .. } => {
                    if let Some(extern_library_path) = extern_library_path {
                        self.tir.graph.add_type1_dep(id, ef!(extern_library_path.item));
                    }
                },
                ast::Expr::AddrOf { expr, .. } | ast::Expr::Deref(expr) | ast::Expr::Pointer { expr, .. }
                    | ast::Expr::Cast { expr, .. } | ast::Expr::Ret { expr, .. } => self.tir.graph.add_type1_dep(id, ef!(expr.item)),
                ast::Expr::DeclRef { id: decl_ref_id, .. } => {
                    let decl_ref = &self.code.ast.decl_refs[decl_ref_id];
                    if let ast::Namespace::MemberRef { base_expr } = decl_ref.namespace {
                        self.tir.graph.add_type1_dep(id, ef!(base_expr.item));
                        self.tir.graph.add_meta_dep(id, ef!(base_expr.item));
                    }
                },
                ast::Expr::Call { callee, ref arguments } => {
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
                ast::Expr::FunctionTy { ref param_tys, ret_ty, .. } => {
                    for &param_ty in param_tys {
                        self.tir.graph.add_type1_dep(id, ef!(param_ty.item));
                    }
                    self.tir.graph.add_type1_dep(id, ef!(ret_ty.item));
                },
                ast::Expr::Set { lhs, rhs } => {
                    self.tir.graph.add_type1_dep(id, ef!(lhs.item));
                    self.tir.graph.add_type1_dep(id, ef!(rhs.item));
                },
                ast::Expr::Do { scope } => {
                    let terminal_expr = self.code.ast.imper_scopes[scope].terminal_expr;
                    self.tir.graph.add_type1_dep(id, ef!(terminal_expr.item));
                },
                ast::Expr::If { condition, then_scope, else_scope } => {
                    self.tir.graph.add_type1_dep(id, ef!(condition.item));
                    let then_expr = self.code.ast.imper_scopes[then_scope].terminal_expr;
                    let else_expr = if let Some(else_scope) = else_scope {
                        self.code.ast.imper_scopes[else_scope].terminal_expr
                    } else {
                        ast::VOID_EXPR
                    };
                    self.tir.graph.add_type1_dep(id, ef!(then_expr.item));
                    self.tir.graph.add_type1_dep(id, ef!(else_expr.item));
                },
                ast::Expr::Switch { scrutinee, ref cases } => {
                    self.tir.graph.add_type1_dep(id, ef!(scrutinee.item));
                    for case in cases.clone() {
                        let terminal = self.code.ast.imper_scopes[case.scope].terminal_expr;
                        self.tir.graph.add_type1_dep(id, ef!(terminal.item));
                    }
                },
                ast::Expr::While { condition, scope, .. } => {
                    self.tir.graph.add_type1_dep(id, ef!(condition.item));
                    let terminal_expr = self.code.ast.imper_scopes[scope].terminal_expr;
                    self.tir.graph.add_type1_dep(id, ef!(terminal_expr.item));
                },
                ast::Expr::Struct(struct_id) => {
                    for field in &self.code.ast.structs[struct_id].fields {
                        self.tir.graph.add_type1_dep(id, ef!(field.ty.item));
                    }
                },
                ast::Expr::Enum(enum_id) => {
                    for variant in &self.code.ast.enums[enum_id].variants {
                        if let Some(payload_ty) = variant.payload_ty {
                            self.tir.graph.add_type1_dep(id, ef!(payload_ty.item));
                        }
                    }
                },
                ast::Expr::StructLit { ref fields, ty, .. } => {
                    self.tir.graph.add_type1_dep(id, ef!(ty.item));
                    self.tir.graph.add_meta_dep(id, ef!(ty.item));
                    for field in fields {
                        self.tir.graph.add_type1_dep(id, ef!(field.expr.item));
                    }
                },
                ast::Expr::For { binding, lower_bound, upper_bound, .. } => {
                    self.tir.graph.add_type1_dep(id, df!(binding.item));

                    // THIS IS KIND OF A HACK. The AST variants for loop binding variables do not currently keep track
                    // of the lower_bound or upper_bound, so I just add the dependencies here.
                    self.tir.graph.add_type1_dep(df!(binding.item), ef!(lower_bound.item));
                    self.tir.graph.add_type1_dep(df!(binding.item), ef!(upper_bound.item));
                },
            }
        }

        // Split the graph into components
        self.tir.graph.split(new_code);

        // TODO: do something better than an array of bools :(
        self.tir.depended_on.resize_with(self.code.ast.exprs.len(), || false);
    }

    pub fn build_more_tir(&mut self) -> Result<Option<Units>, TirError> {
        dvd::send(|| DvdMessage::WillBuildMoreTir);
        if !self.tir.graph.has_outstanding_components() {
            dvd::send(|| DvdMessage::DidBuildMoreTir { no_outstanding_components: true });
            return Ok(None);
        }

        add_eval_dep_injector!(self, add_eval_dep);

        // Add types 2-4 dependencies
        let items_that_need_dependencies = self.tir.graph.get_items_that_need_dependencies();
        dvd::send(|| DvdMessage::WillAddTirDependencies { items_that_need_dependencies: items_that_need_dependencies.clone() });
        for id in items_that_need_dependencies {
            match self.code.ast.items[id] {
                ast::Item::Decl(decl_id) => {
                    match df!(decl_id.ast) {
                        ast::Decl::Parameter { .. } | ast::Decl::Static(_) | ast::Decl::Const { .. } | ast::Decl::Stored { .. } | ast::Decl::Field { .. } | ast::Decl::ReturnValue | ast::Decl::InternalField(_) | ast::Decl::LoopBinding { .. } /*  | ast::Decl::PatternBinding { .. }*/ => {},
                        ast::Decl::PatternBinding { id: binding_id, .. } => {
                            let scrutinee = self.code.ast.pattern_binding_decls[binding_id].scrutinee;
        
                            // TODO: find out why this seems to cause an infinite loop if it's moved to build_more_tir() and changed to a type 2 dependency
                            self.tir.graph.add_type2_dep(id, ef!(scrutinee.item));
                        },
                        ast::Decl::GenericParam(_) => {
                            add_eval_dep!(id, ast::TYPE_TYPE);
                        },
                        ast::Decl::LegacyIntrinsic { ref param_tys, .. } => {
                            for &ty in param_tys {
                                add_eval_dep!(id, ty);
                            }
                        },
                        ast::Decl::Intrinsic(intr) | ast::Decl::MethodIntrinsic(intr) => {
                            let param_tys = self.code.ast.intrinsics[intr].param_tys.clone();
                            for ty in param_tys {
                                add_eval_dep!(id, ty);
                            }
                        },
                        ast::Decl::Variant { payload_ty, .. } => {
                            if let Some(payload_ty) = payload_ty {
                                add_eval_dep!(id, payload_ty);
                            }
                        }
                        ast::Decl::Computed { scope, ref param_tys, .. } => {
                            for &ty in param_tys {
                                add_eval_dep!(id, ty);
                            }
                            self.add_type3_scope_dep(id, scope);
                            // NOTE: the Some case is handled below this match expression
                            if self.code.ast.explicit_tys[decl_id].is_none() {
                                add_eval_dep!(id, ast::VOID_TYPE);
                            }
                        },
                        ast::Decl::ComputedPrototype { ref param_list, extern_func } => {
                            for &ty in &param_list.param_tys {
                                add_eval_dep!(id, ty);
                            }
                            // NOTE: the Some case is handled below this match expression
                            if self.code.ast.explicit_tys[decl_id].is_none() {
                                add_eval_dep!(id, ast::VOID_TYPE);
                            }

                            if let Some(extern_func) = extern_func {
                                let extern_library_path = self.code.ast.extern_mods[extern_func.extern_mod].library_path;
                                add_eval_dep!(id, extern_library_path);
                            }
                        },
                        ast::Decl::ObjcClassRef { extern_mod, .. } => {
                            let extern_library_path = self.code.ast.extern_mods[extern_mod].library_path;
                            add_eval_dep!(id, extern_library_path);
                        },
                    }
        
                    // NOTE: The computed decl case in the above match expression depends on this!
                    if let Some(ty) = self.code.ast.explicit_tys[decl_id] {
                        add_eval_dep!(id, ty);
                    }
                }
                ast::Item::Expr(expr_id) => {
                    match ef!(expr_id.ast) {
                        ast::Expr::Void | ast::Expr::Error | ast::Expr::IntLit { .. } | ast::Expr::DecLit { .. } | ast::Expr::StrLit { .. }
                            | ast::Expr::CharLit { .. } | ast::Expr::BoolLit { .. } | ast::Expr::Const(_) | ast::Expr::AddrOf { .. }
                            | ast::Expr::Deref(_) | ast::Expr::Pointer { .. } | ast::Expr::Set { .. } | ast::Expr::Mod { .. }
                            | ast::Expr::Struct(_) | ast::Expr::Enum(_) | ast::Expr::Call { .. } | ast::Expr::FunctionTy { .. }
                            | ast::Expr::Break(_) | ast::Expr::Continue(_) | ast::Expr::StructLit { .. } => {},
                        ast::Expr::Cast { ty, .. } => {
                            add_eval_dep!(id, ty);
                        },
                        ast::Expr::Ret { decl, .. } => {
                            let ty = decl
                                .and_then(|decl| self.code.ast.explicit_tys[decl])
                                .unwrap_or(ast::VOID_TYPE);
                            add_eval_dep!(id, ty);
                        }
                        ast::Expr::DeclRef { id: decl_ref_id, ref explicit_generic_args, .. } => {
                            if let Some(generic_args) = explicit_generic_args {
                                for &arg in generic_args {
                                    add_eval_dep!(id, arg);
                                }
                            }
                            self.add_types_2_to_4_deps_to_member_ref(id, decl_ref_id);
                        },
                        ast::Expr::Do { scope } => {
                            self.add_type3_scope_dep(id, scope);
                        },
                        ast::Expr::Switch { ref cases, .. } => {
                            for case in cases.clone() {
                                self.add_type3_scope_dep(id, case.scope);
                            }
                        },
                        ast::Expr::If { then_scope, else_scope, .. } => {
                            self.add_type3_scope_dep(id, then_scope);
                            if let Some(else_scope) = else_scope {
                                self.add_type3_scope_dep(id, else_scope);
                            }
                        }
                        ast::Expr::While { scope, .. } => {
                            self.add_type3_scope_dep(id, scope);
                        },
                        ast::Expr::For { scope, .. } => {
                            self.add_type3_scope_dep(id, scope);
                        },
                    }
                }
            }
        }
        dvd::send(|| DvdMessage::DidAddTirDependencies);

        // Solve for the unit and level of each item
        let levels = self.tir.graph.solve().map_err(|err| {
            match err {
                TirError::DependencyCycle => {
                    self.diag.report_error_no_range("dependency cycle found. this is most likely a Dusk compiler bug.");
                }
            }
            dvd::send(|| DvdMessage::DidBuildMoreTir { no_outstanding_components: false });
            err
        })?;

        let mut sp = Subprogram { units: Vec::new(), mock_units: Vec::new(), levels };
        sp.units.resize_with(sp.levels.units.len(), Default::default);

        // Finally, convert AST items to TIR and add them to the correct spot
        for (unit, levels_unit) in sp.units.iter_mut().zip(&sp.levels.units) {
            for &item_id in &levels_unit.items {
                let level = sp.levels.item_to_levels[&item_id];
                match self.code.ast.items[item_id] {
                    ast::Item::Decl(id) => {
                        self.build_tir_decl(&mut unit.items, level, id);
                    }
                    ast::Item::Expr(id) => {
                        if self.tir.depended_on[id] { unit.eval_dependees.push(id); }
                        self.build_tir_expr(&mut unit.items, level, id);
                    }
                }
            }
        }
        for mock_unit in &sp.levels.mock_units {
            let main_expr = match self.code.ast.items[mock_unit.item] {
                ast::Item::Expr(id) => id,
                ast::Item::Decl(_) => panic!("Can't have metadependency on a declaration!"),
            };
            let mut items = UnitItems::default();
            self.build_tir_expr(&mut items, mock_unit.item_level, main_expr);
            for &item_id in &mock_unit.deps {
                let level = sp.levels.item_to_levels[&item_id];
                match self.code.ast.items[item_id] {
                    ast::Item::Decl(id) => {
                        self.build_tir_decl(&mut items, level, id);
                    }
                    ast::Item::Expr(id) => {
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
        for scope in &self.code.ast.imper_scopes {
            for &op in &self.code.blocks[scope.block].ops {
                let op = &self.code.ops[op];
                let item = op.as_ast_item().unwrap();
                match item {
                    // TODO: This is a horrible hack! Instead of looping through all imperative scopes, I should somehow
                    // associate statements with their unit
                    Item::Expr(expr) => if let Some(&unit) = sp.levels.item_to_units.get(&ef!(expr.item)) {
                        let has_semicolon = op.has_semicolon();
                        let unit = &mut sp.units[unit as usize];
                        unit.items.stmts.push(Stmt { root_expr: expr, has_semicolon });
                    },
                    Item::Decl(decl) => assert!(matches!(df!(decl.ast), ast::Decl::Stored { .. } | ast::Decl::Computed { .. }), "Invalid scope item"),
                }
            }
        }

        for unit in &mut sp.units {
            unit.items.unify_sizes();
        }

        dvd::send(|| DvdMessage::DidBuildMoreTir { no_outstanding_components: false });
        Ok(
            Some(
                Units { units: sp.units, mock_units: sp.mock_units }
            )
        )
    }
}
