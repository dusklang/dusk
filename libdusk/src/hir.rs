use std::ffi::CString;
use std::fmt::Debug;
use std::ops::Range;
use std::collections::HashSet;

use display_adapter::display_adapter;
use dusk_dire::mir::Const;
use smallvec::{SmallVec, smallvec};
use string_interner::{DefaultSymbol as Sym, Symbol, StringInterner};

use dusk_dire::{Op, Block};
use dusk_dire::hir::*;
use dusk_dire::index_counter::IndexCounter;
use dusk_dire::ty::Type;
use dusk_dire::source_info::{SourceFileId, SourceRange};
use dusk_dire::InternalField;

use crate::dvd::{Message as DvdMessage, self};
use crate::autopop::{AutoPopStack, AutoPopStackEntry};
use crate::driver::Driver;
use crate::error::Error;
use crate::index_vec::*;
use crate::builder::{BinOp, UnOp};
use crate::source_info::ToSourceRange;

use dusk_proc_macros::*;

// TODO: switch to AOS here
// TODO: move to dire, perhaps
#[derive(Debug, Clone)]
pub struct GenericParamList {
    pub names: SmallVec<[Sym; 1]>,
    pub ids: Range<GenericParamId>,
    pub ranges: SmallVec<[SourceRange; 1]>,
}

impl Driver {
    pub fn create_decls_for_generic_param_list(&mut self, list: &GenericParamList) -> Range<DeclId> {
        assert_eq!(list.names.len(), list.ids.end - list.ids.start);
        assert_eq!(list.names.len(), list.ranges.len());
        self.code.hir.decls.reserve(list.names.len());
        let first_generic_param = self.code.hir.decls.next_idx();
        for ((param, &name), &range) in range_iter(list.ids.clone()).zip(&list.names).zip(&list.ranges) {
            self.add_decl(Decl::GenericParam(param), name, Some(VOID_TYPE), range);
        }
        let last_generic_param = self.code.hir.decls.next_idx();
        first_generic_param..last_generic_param
    }
}

impl Default for GenericParamList {
    fn default() -> Self {
        Self {
            names: SmallVec::new(),
            ids: GenericParamId::new(0)..GenericParamId::new(0),
            ranges: SmallVec::new(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BufferedStmt {
    expr: ExprId,
    has_semicolon: bool,
}

#[derive(Debug, Clone, Copy)]
pub enum ScopeState {
    Imper {
        id: ImperScopeId,
        // TODO: choose `namespace` or `ns` and use it consistently for all variants of this enum
        namespace: ImperScopeNsId,
        stmt_buffer: Option<BufferedStmt>,
    },
    Mod {
        id: ModScopeId,
        namespace: ModScopeNsId,
        extern_mod: Option<ExternModId>,
    },
    GenericContext(GenericContextNsId),
    Condition {
        ns: ConditionNsId,
        condition_kind: ConditionKind,
    },
}

#[derive(Debug)]
struct CompDeclState {
    has_scope: Option<ImperScopeId>,
    params: Range<DeclId>,
    generic_params: Range<DeclId>,
    id: DeclId,
    imper_scope_stack: u32,
    stored_decl_counter: IndexCounter<StoredDeclId>,
    // TODO: support loops outside of comp decls by moving this elsewhere.
    loop_stack: AutoPopStack<LoopState>,
    loop_counter: IndexCounter<LoopId>,
}

#[derive(Debug)]
pub struct Builder {
    comp_decl_stack: Vec<CompDeclState>,
    scope_stack: AutoPopStack<ScopeState>,
    generic_ctx_stack: AutoPopStack<GenericCtxId>,
    debug_marked_exprs: HashSet<ExprId>,
    pub prelude_namespace: Option<ModScopeNsId>,
    pub generic_params: IndexCounter<GenericParamId>,

    pub known_idents: KnownIdents,
}

macro_rules! declare_known_idents {
    ($($name:ident $(= $assignment:expr)?),*) => {
        #[derive(Debug)]
        pub struct KnownIdents {
            $(pub $name: Sym),*
        }

        impl KnownIdents {
            fn uninit() -> Self {
                KnownIdents {
                    $($name: Sym::try_from_usize((u32::MAX - 1) as usize).unwrap()),*
                }
            }
        
            fn init(&mut self, interner: &mut StringInterner) {
                *self = KnownIdents {
                    $(
                        $name: declare_known_idents!(@init interner, $name $(= $assignment)?)
                    ),*
                };
            }
        }
    };
    (@init $interner:expr, $name:ident = $assignment:expr) => {
        $interner.get_or_intern($assignment)
    };
    (@init $interner:expr, $name:ident) => {
        $interner.get_or_intern(stringify!($name))
    };
}
declare_known_idents!(requires, guarantees, comptime, return_value, invalid_declref, underscore = "_");

impl Default for Builder {
    fn default() -> Self {
        Builder {
            comp_decl_stack: Default::default(),
            scope_stack: Default::default(),
            generic_ctx_stack: Default::default(),
            debug_marked_exprs: Default::default(),

            generic_params: IndexCounter::new(),

            prelude_namespace: None,

            // Gets initialized in Driver::initialize_hir() below
            known_idents: KnownIdents::uninit(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LoopState {
    id: LoopId,
    name: Option<Ident>,
    used: bool, // whether the loop name has been used by a continue or break
}

impl PartialEq<ScopeState> for ConditionNsId {
    fn eq(&self, other: &ScopeState) -> bool {
        match other {
            ScopeState::Condition { ns, .. } => ns == self,
            _ => false,
        }
    }
}
impl PartialEq<ScopeState> for GenericContextNsId {
    fn eq(&self, other: &ScopeState) -> bool {
        match other {
            ScopeState::GenericContext(ns) => ns == self,
            _ => false,
        }
    }
}
impl PartialEq<ScopeState> for ModScopeNsId {
    fn eq(&self, other: &ScopeState) -> bool {
        match other {
            ScopeState::Mod { namespace: ns, .. } => ns == self,
            _ => false,
        }
    }
}
impl PartialEq<ScopeState> for ImperScopeId {
    fn eq(&self, other: &ScopeState) -> bool {
        match other {
            ScopeState::Imper { id, .. } => id == self,
            _ => false,
        }
    }
}
impl PartialEq<LoopState> for LoopId {
    fn eq(&self, other: &LoopState) -> bool {
        other.id == *self
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ConditionKind {
    Requirement, Guarantee,
}

impl Driver {
    pub fn initialize_hir(&mut self) {
        dvd::send(|| DvdMessage::WillBeginAddingBuiltins);
        self.hir.generic_ctx_stack.push(BLANK_GENERIC_CTX, BLANK_GENERIC_CTX).make_permanent();
        self.add_expr(Expr::Void, SourceRange::default());
        self.add_expr(Expr::Error, SourceRange::default());
        self.add_const_ty(Type::Void);
        self.add_const_ty(Type::Ty);
        self.add_const_ty(Type::Error);
        assert_eq!(self.code.hir.exprs.len(), 5);

        self.hir.known_idents.init(&mut self.interner);
        self.add_decl(Decl::ReturnValue, self.hir.known_idents.return_value, None, SourceRange::default());

        self.register_internal_fields();
        self.add_prelude();
    }

    pub fn finalize_hir(&mut self) {
        // In the parser, in between pushing calls and popping calls, it is hypothetically possible to fail in a
        // recoverable way, leaving the generic ctx stack in an invalid state. As long as this assertion passes
        // (and there are no other panics before we even get here), we're fine for now.
        // TODO: fix this for real.
        assert_eq!(self.hir.generic_ctx_stack.stack.lock().unwrap().borrow().len(), 1);
    }

    #[allow(unused)]
    #[display_adapter]
    pub fn dump_scope_stack(&self, w: &mut Formatter) {
        for scope in self.hir.scope_stack.stack.lock().unwrap().borrow().iter().rev() {
            writeln!(w, "{:?}", scope)?;
        }
        Ok(())
    }

    pub fn debug_mark_expr(&mut self, expr: ExprId) {
        self.hir.debug_marked_exprs.insert(expr);
    }

    #[allow(unused)]
    pub fn expr_is_debug_marked(&self, expr: ExprId) -> bool {
        self.hir.debug_marked_exprs.contains(&expr)
    }

    fn add_expr(&mut self, expr: Expr, range: SourceRange) -> ExprId {
        // TODO: I used to require callers to explicitly pass in the generic ctx id, but it seems fine to just take it
        // from the top of the stack. Is there some major downside to this approach that I have since forgotten?
        // NOTE: also, see add_decl()
        let generic_ctx = self.hir.generic_ctx_stack.peek().unwrap();
        let expr_id = self.code.hir.exprs.push(expr);
        let item_id = self.code.hir.items.push(Item::Expr(expr_id));
        self.code.hir.item_generic_ctxs.push_at(item_id, generic_ctx);
        self.code.hir.expr_to_items.push_at(expr_id, item_id);
        self.code.hir.source_ranges.push_at(item_id, range);

        dvd::send(|| {
            let text = self.display_item(expr_id).to_string();
            DvdMessage::DidAddExpr { id: expr_id, item_id, text }
        });

        expr_id
    }

    pub fn add_decl(&mut self, decl: Decl, name: Sym, explicit_ty: Option<ExprId>, range: SourceRange) -> DeclId {
        // TODO: I used to require callers to explicitly pass in the generic ctx id, but it seems fine to just take it
        // from the top of the stack. Is there some major downside to this approach that I have since forgotten?
        // NOTE: also, see add_expr()
        let generic_ctx = self.hir.generic_ctx_stack.peek().unwrap();
        let decl_id = self.code.hir.decls.push(decl);
        self.code.hir.explicit_tys.push_at(decl_id, explicit_ty);
        self.code.hir.names.push_at(decl_id, name);

        let item_id = self.code.hir.items.push(Item::Decl(decl_id));
        self.code.hir.decl_to_items.push_at(decl_id, item_id);

        self.code.hir.source_ranges.push_at(item_id, range);
        self.code.hir.item_generic_ctxs.push_at(item_id, generic_ctx);

        dvd::send(|| {
            let text = self.display_item(decl_id).to_string();
            DvdMessage::DidAddDecl { id: decl_id, item_id, text }
        });

        decl_id
    }

    pub fn int_lit(&mut self, lit: u64, range: SourceRange) -> ExprId {
        self.add_expr(Expr::IntLit { lit }, range)
    }
    pub fn dec_lit(&mut self, lit: f64, range: SourceRange) -> ExprId { 
        self.add_expr(Expr::DecLit { lit }, range)
    }
    pub fn str_lit(&mut self, lit: CString, range: SourceRange) -> ExprId { 
        self.add_expr(Expr::StrLit { lit }, range)
    }
    pub fn char_lit(&mut self, lit: i8, range: SourceRange) -> ExprId { 
        self.add_expr(Expr::CharLit { lit }, range)
    }
    pub fn bool_lit(&mut self, lit: bool, range: SourceRange) -> ExprId { 
        self.add_expr(Expr::BoolLit { lit }, range)
    }
    pub fn cast(&mut self, expr: ExprId, ty: ExprId, range: SourceRange) -> ExprId {
        let cast_id = self.code.hir.cast_counter.next_idx();
        self.add_expr(Expr::Cast { expr, ty, cast_id }, range)
    }
    pub fn next_stored_decl(&mut self) -> StoredDeclId {
        let decl = self.hir.comp_decl_stack.last_mut().unwrap();
        decl.stored_decl_counter.next_idx()
    }
    pub fn stored_decl(&mut self, name: Sym, _generic_params: GenericParamList, explicit_ty: Option<ExprId>, is_mut: bool, root_expr: ExprId, range: SourceRange) -> DeclId {
        self.flush_stmt_buffer();
        match self.hir.scope_stack.peek().unwrap() {
            ScopeState::Imper { .. } => {
                let id = self.next_stored_decl();

                let decl_id = self.add_decl(Decl::Stored { id, is_mut, root_expr }, name, explicit_ty, range);
                self.scope_item(Item::Decl(decl_id), false);
                self.imper_scoped_decl(
                    ImperScopedDecl {
                        name,
                        num_params: 0,
                        id: decl_id,
                    }
                );
                decl_id
            }
            ScopeState::Mod { .. } => {
                let decl_id = self.add_decl(
                    if is_mut {
                        Decl::Static(root_expr)
                    } else {
                        Decl::Const(root_expr)
                    },
                    name,
                    explicit_ty,
                    range,
                );
                self.mod_scoped_decl(
                    name,
                    ModScopedDecl {
                        num_params: 0,
                        id: decl_id
                    }
                );
                decl_id
            },
            ScopeState::Condition { .. } | ScopeState::GenericContext(_) => panic!("Stored decl unsupported in this position"),
        }
    }
    pub fn ret(&mut self, expr: ExprId, range: SourceRange) -> ExprId {
        let decl = self.hir.comp_decl_stack.last().map(|decl| decl.id);
        self.add_expr(Expr::Ret { expr, decl }, range)
    }
    fn lookup_loop_by_label(&mut self, label: Option<Ident>) -> Option<LoopId> {
        let loop_stack = self.hir.comp_decl_stack.last().unwrap().loop_stack.clone();
        let loop_stack = loop_stack.stack.lock().unwrap();
        let mut loop_stack = loop_stack.borrow_mut();
        if let Some(label) = label {
            for looop in loop_stack.iter_mut().rev() {
                if looop.name.map(|ident| ident.symbol) == Some(label.symbol) {
                    looop.used = true;
                    return Some(looop.id);
                }
            }
            let label_str = self.interner.resolve(label.symbol).unwrap().to_owned();
            self.diag.report_error_no_range_msg(
                format!("unable to find loop label `{}`", label_str),
                label.range,
            );
            None
        } else {
            loop_stack.last().map(|state| state.id)
        }
    }
    fn control_flow_outside_loop_error(&mut self, kind: &str, range: SourceRange) {
        self.diag.push(
            Error::new(format!("`{}` expression found outside of a loop", kind))
                .adding_primary_range(range, "only valid inside a `for` or `while` loop")
        )
    }
    pub fn break_expr(&mut self, range: SourceRange, label: Option<Ident>) -> ExprId {
        let id = self.lookup_loop_by_label(label);
        if label.is_none() && id.is_none() {
            self.control_flow_outside_loop_error("break", range);
        }
        self.add_expr(Expr::Break(id), range)
    }
    pub fn continue_expr(&mut self, range: SourceRange, label: Option<Ident>) -> ExprId {
        let id = self.lookup_loop_by_label(label);
        if label.is_none() && id.is_none() {
            self.control_flow_outside_loop_error("break", range);
        }
        self.add_expr(Expr::Continue(id), range)
    }
    pub fn if_expr(&mut self, condition: ExprId, then_scope: ImperScopeId, else_scope: Option<ImperScopeId>, range: SourceRange) -> ExprId {
        self.add_expr(
            Expr::If { condition, then_scope, else_scope },
            range,
        )
    }
    pub fn while_expr(&mut self, loop_id: LoopId, condition: ExprId, scope: ImperScopeId, range: SourceRange) -> ExprId {
        self.add_expr(Expr::While { loop_id, condition, scope }, range)
    }
    pub fn for_expr(&mut self, loop_id: LoopId, binding: DeclId, lower_bound: ExprId, upper_bound: ExprId, scope: ImperScopeId, range: SourceRange) -> ExprId {
        self.add_expr(Expr::For { loop_id, binding, lower_bound, upper_bound, scope }, range)
    }
    pub fn switch_expr(&mut self, scrutinee: ExprId, cases: Vec<SwitchCase>, range: SourceRange) -> ExprId {
        self.add_expr(Expr::Switch { scrutinee, cases }, range)
    }
    pub fn do_expr(&mut self, scope: ImperScopeId, range: SourceRange) -> ExprId {
        self.add_expr(Expr::Do { scope }, range)
    }
    pub fn field_decl(&mut self, name: Sym, strukt: StructId, ty: ExprId, index: usize, range: SourceRange) -> FieldDecl {
        let decl = self.add_decl(Decl::Field { strukt, index }, name, Some(ty), range);
        FieldDecl { decl, name, ty }
    }
    pub fn variant_decl(&mut self, name: Sym, enuum: ExprId, enum_id: EnumId, index: usize, payload_ty: Option<ExprId>, range: SourceRange) -> VariantDecl {
        let decl = self.add_decl(Decl::Variant { enuum: enum_id, index, payload_ty }, name, Some(enuum), range);
        VariantDecl { decl, name, enuum, payload_ty }
    }
    pub fn reserve_struct(&mut self) -> (ExprId, StructId) {
        let strukt = self.code.hir.structs.push(Struct { fields: Vec::new() });
        let expr = self.add_expr(Expr::Struct(strukt), Default::default());
        (expr, strukt)
    }
    pub fn finish_struct(&mut self, fields: Vec<FieldDecl>, range: SourceRange, expr: ExprId, strukt: StructId) {
        self.code.hir.structs[strukt] = Struct { fields };
        ef!(expr.range) = range;
    }
    pub fn reserve_enum(&mut self) -> (ExprId, EnumId) {
        let enuum = self.code.hir.enums.push(Enum { variants: Vec::new() });
        let expr = self.add_expr(Expr::Enum(enuum), Default::default());
        (expr, enuum)
    }
    pub fn finish_enum(&mut self, variants: Vec<VariantDecl>, range: SourceRange, expr: ExprId, enuum: EnumId) {
        self.code.hir.enums[enuum] = Enum { variants };
        ef!(expr.range) = range;
    }
    pub fn struct_lit(&mut self, ty: ExprId, fields: Vec<FieldAssignment>, range: SourceRange) -> ExprId {
        let id = self.code.hir.struct_lits.next_idx();
        self.add_expr(Expr::StructLit { ty, fields, id }, range)
    }
    pub fn error_expr(&mut self, range: SourceRange) -> ExprId {
        self.add_expr(Expr::Error, range)
    }
    pub fn get_pattern_bindings(&mut self, pattern: &PatternKind, scrutinee: ExprId) -> (Vec<ImperScopedDecl>, Vec<PatternBindingDeclId>) {
        let mut decls = Vec::new();
        let mut bindings = Vec::new();
        match pattern {
            PatternKind::ContextualMember { .. } | PatternKind::AnonymousCatchAll(_) | PatternKind::IntLit { .. } => {},
            PatternKind::NamedCatchAll(name) => {
                let paths = vec![
                    PatternBindingPath::identity()
                ];
                let binding_decl = PatternBindingDecl { paths, scrutinee };
                let id = self.code.hir.pattern_binding_decls.push(binding_decl);
                let decl = self.add_decl(Decl::PatternBinding { id, is_mut: false }, name.symbol, None, name.range);
                let decl = ImperScopedDecl { name: name.symbol, num_params: 0, id: decl };
                decls.push(decl);
                bindings.push(id);
            },
        }
        (decls, bindings)
    }
    pub fn push_to_scope_stack<Id: PartialEq<ScopeState> + Debug + Copy>(&mut self, id: Id, state: ScopeState) -> AutoPopStackEntry<ScopeState, Id> {
        self.hir.scope_stack.push(id, state)
    }
    /// unchecked invariant: must call end_loop after this
    pub fn begin_loop(&mut self, name: Option<Ident>) -> AutoPopStackEntry<LoopState, LoopId> {
        // TODO: there's no reason loops shouldn't be allowed outside of comp decls
        let comp_decl_state = self.hir.comp_decl_stack.last_mut().unwrap();
        let id = comp_decl_state.loop_counter.next_idx();
        let mut loop_stack = comp_decl_state.loop_stack.clone();
        {
            let loop_stack = loop_stack.stack.lock().unwrap();
            let loop_stack = loop_stack.borrow();
            if let Some(name) = name {
                for state in &*loop_stack {
                    if state.name.map(|ident| ident.symbol) == Some(name.symbol) {
                        // AFAICT this is the first time I have emitted an error from inside the HIR generator instead
                        // of the parser. This makes me a little uncomfortable. On the other hand, diagnosing this
                        // inside the parser would require exposing more state to the parser, which I also don't love.
                        let name_str = self.interner.resolve(name.symbol).unwrap();
                        self.diag.push(
                            Error::new(format!("loop with label `{}` already exists", name_str))
                                .adding_primary_range(name.range, "")
                                .adding_secondary_range(state.name.unwrap().range, "")
                        );
                        break;
                    }
                }
                
            }
        }

        loop_stack.push(id, LoopState { id, name, used: false })
    }
    pub fn end_loop(&mut self, entry: AutoPopStackEntry<LoopState, LoopId>) {
        let state = entry.stack.peek().unwrap();
        if let Some(name) = state.name {
            if !state.used {
                let name_str = self.interner.resolve(name.symbol).unwrap().to_string();
                self.diag.report_warning_no_range_msg(
                    format!("loop label `{}` never used", name_str),
                    name.range
                );
            }
        }
    }
    pub fn create_condition_namespace(&mut self) -> ConditionNsId {
        let parent = self.cur_namespace();
        self.code.hir.condition_ns.push(ConditionNs { func: DeclId::from_raw(u32::MAX), parent: Some(parent) })
    }
    pub fn enter_condition_namespace(&mut self, ns: ConditionNsId, condition_kind: ConditionKind) -> AutoPopStackEntry<ScopeState, ConditionNsId> {
        // This condition kind is just a placeholder which will be reset by each attribute. It is done this way so I can share a single
        // condition namespace across all condition attributes on a single function.
        self.push_to_scope_stack(ns, ScopeState::Condition { ns, condition_kind })
    }
    pub fn begin_generic_context(&mut self, generic_params: Range<DeclId>) -> AutoPopStackEntry<ScopeState, GenericContextNsId> {
        let parent = self.cur_namespace();
        let ns = self.code.hir.generic_context_ns.push(GenericContextNs { generic_params, parent: Some(parent) });
        self.push_to_scope_stack(ns, ScopeState::GenericContext(ns))
    }
    pub fn begin_module(&mut self, extern_mod: Option<ExternModId>, range: SourceRange) -> (AutoPopStackEntry<ScopeState, ModScopeNsId>, ExprId) {
        let parent = self.cur_namespace();
        let id = self.code.hir.mod_scopes.push(ModScope::default());
        let namespace = self.code.hir.mod_ns.push(
            ModScopeNs {
                scope: id, parent: Some(parent)
            }
        );
        let entry = self.push_to_scope_stack(namespace, ScopeState::Mod { id, namespace, extern_mod });
        let extern_library_path = extern_mod.map(|mawd| self.code.hir.extern_mods[mawd].library_path);
        let expr = self.add_expr(Expr::Mod { id, extern_library_path }, range);
        (entry, expr)
    }
    fn push_generic_ctx(&mut self, ctx: impl FnOnce(GenericCtxId) -> GenericCtx) -> AutoPopStackEntry<GenericCtxId> {
        let parent = self.hir.generic_ctx_stack.peek().unwrap();
        let generic_ctx = self.code.hir.generic_ctxs.push(ctx(parent));
        self.hir.generic_ctx_stack.push(generic_ctx, generic_ctx)
    }
    pub fn begin_computed_decl_generic_ctx(&mut self, generic_param_list: GenericParamList) -> AutoPopStackEntry<GenericCtxId> {
        let generic_param_ids = range_iter(generic_param_list.ids.clone())
            .collect();
        self.push_generic_ctx(|parent| GenericCtx::Decl { parameters: generic_param_ids, parent })
    }
    pub fn begin_computed_decl(&mut self, name: Sym, param_names: SmallVec<[Sym; 2]>, param_tys: SmallVec<[ExprId; 2]>, param_ranges: SmallVec<[SourceRange; 2]>, generic_params: Range<DeclId>, return_ty: ExprId, proto_range: SourceRange) -> DeclId {
        // This is a placeholder value that gets replaced once the parameter declarations get allocated.
        let id = self.add_decl(Decl::Const(ExprId::new(u32::MAX as usize)), name, Some(return_ty), proto_range);

        assert_eq!(param_names.len(), param_tys.len());
        self.code.hir.decls.reserve(param_tys.len());
        let first_param = self.code.hir.decls.next_idx();
        param_tys.iter()
            .enumerate()
            .zip(&param_names)
            .zip(&param_ranges)
            .for_each(|(((index, ty), &name), &range)| {
                self.add_decl(Decl::Parameter { index }, name, Some(*ty), range);
            });
        let last_param = self.code.hir.decls.next_idx();
        let params = first_param..last_param;

        // `end_computed_decl` will attach the real scope to this decl; we don't have it yet
        df!(id.hir) = Decl::Computed {
            param_tys,
            params: params.clone(),
            scope: ImperScopeId::new(u32::MAX as usize),
            generic_params: generic_params.clone(),
        };
        match self.hir.scope_stack.peek().unwrap() {
            ScopeState::Imper { .. } => {
                self.flush_stmt_buffer();
                self.scope_item(Item::Decl(id), false);
            },
            ScopeState::Mod { .. } => {
                self.mod_scoped_decl(name, ModScopedDecl { num_params: param_names.len(), id });
            },
            ScopeState::Condition { .. } | ScopeState::GenericContext(_) => panic!("Computed decls are not supported in this position"),
        }
        self.hir.comp_decl_stack.push(
            CompDeclState {
                has_scope: None,
                params,
                generic_params,
                id,
                imper_scope_stack: 0,
                stored_decl_counter: IndexCounter::new(),
                loop_stack: Default::default(),
                loop_counter: Default::default(),
            }
        );

        id
    }
    pub fn comp_decl_prototype(&mut self, name: Sym, param_tys: SmallVec<[ExprId; 2]>, _param_ranges: SmallVec<[SourceRange; 2]>, return_ty: ExprId, range: SourceRange) -> DeclId {
        // This is a placeholder value that gets replaced once the parameter declarations get allocated.
        let num_params = param_tys.len();
        let extern_func = match self.hir.scope_stack.peek().unwrap() {
            ScopeState::Mod { extern_mod: Some(extern_mod), .. } => {
                let funcs = &mut self.code.hir.extern_mods[extern_mod].imported_functions;
                let index = funcs.len();
                let name = self.interner.resolve(name).unwrap();
                funcs.push(ExternFunction { name: name.to_string(), param_tys: param_tys.iter().cloned().collect(), return_ty });
                Some(
                    ExternFunctionRef {
                        extern_mod,
                        index
                    }
                )
            },
            _ => None,
        };
        let id = self.add_decl(Decl::ComputedPrototype { param_tys, extern_func }, name, Some(return_ty), range);
        match self.hir.scope_stack.peek().unwrap() {
            ScopeState::Imper { .. } => {
                self.flush_stmt_buffer();
                self.scope_item(Item::Decl(id), false);
            },
            ScopeState::Mod { .. } => {
                self.mod_scoped_decl(name, ModScopedDecl { num_params, id });
            },
            ScopeState::Condition { .. } | ScopeState::GenericContext(_) => panic!("Computed decls are not supported in this position"),
        }

        id
    }
    pub fn begin_decl_ref_generic_ctx(&mut self) -> AutoPopStackEntry<GenericCtxId> {
        let id = self.code.hir.decl_refs.push(
            DeclRef {
                name: self.hir.known_idents.invalid_declref,
                namespace: Namespace::Invalid,
                expr: ERROR_EXPR,
            }
        );
        self.push_generic_ctx(|parent| GenericCtx::DeclRef { id, parent })
    }
    pub fn decl_ref(&mut self, base_expr: Option<ExprId>, name: Sym, arguments: SmallVec<[ExprId; 2]>, has_parens: bool, range: SourceRange, generic_ctx: AutoPopStackEntry<GenericCtxId>) -> ExprId {
        let namespace = match base_expr {
            Some(base_expr) => {
                Namespace::MemberRef { base_expr }
            },
            None => self.cur_namespace(),
        };
        let id = if let GenericCtx::DeclRef { id, .. } = self.code.hir.generic_ctxs[generic_ctx.id()] {
            id
        } else {
            panic!("Invalid generic context passed in");
        };
        let expr = self.add_expr(Expr::DeclRef { id }, range);
        self.code.hir.decl_refs[id] = DeclRef {
            name,
            namespace,
            expr,
        };
        if has_parens {
            self.add_expr(Expr::Call { callee: expr, arguments }, range)
        } else {
            expr
        }
    }
    // TODO: intern constant expressions
    pub fn add_const_expr(&mut self, value: Const) -> ExprId {
        self.add_expr(Expr::Const(value), SourceRange::default())
    }
    pub fn add_const_ty(&mut self, ty: Type) -> ExprId {
        self.add_const_expr(Const::Ty(ty))
    }
    pub fn add_intrinsic(&mut self, intrinsic: Intrinsic, param_tys: SmallVec<[ExprId; 2]>, ret_ty: ExprId, function_like: bool) {
        let name = self.interner.get_or_intern(intrinsic.name());
        let num_params = param_tys.len();
        let id = self.add_decl(Decl::Intrinsic { intr: intrinsic, param_tys, function_like }, name, Some(ret_ty), SourceRange::default());
        self.mod_scoped_decl(
            name,
            ModScopedDecl { num_params, id }
        );
    }
    pub fn internal_field(&mut self, field: InternalField, name: &str, ty: Type) -> DeclId {
        let name = self.interner.get_or_intern(name);
        let ty = self.add_const_ty(ty);
        self.add_decl(
            Decl::InternalField(field), name, Some(ty), SourceRange::default()
        )
    }
    pub fn bin_op(&mut self, op: BinOp, lhs: ExprId, rhs: ExprId, range: SourceRange) -> ExprId {
        match op {
            BinOp::Assign => self.add_expr(Expr::Set { lhs, rhs }, range),
            _ => {
                let name = self.interner.get_or_intern(op.symbol());
                // TODO: create generic context before parsing operands
                let generic_ctx = self.begin_decl_ref_generic_ctx();
                self.decl_ref(None, name, smallvec![lhs, rhs], true, range, generic_ctx)
            }
        }
    }
    pub fn un_op(&mut self, op: UnOp, expr: ExprId, range: SourceRange) -> ExprId {
        match op {
            UnOp::Deref      => self.add_expr(Expr::Deref(expr), range),
            UnOp::AddrOf     => self.add_expr(Expr::AddrOf  { expr, is_mut: false }, range),
            UnOp::AddrOfMut  => self.add_expr(Expr::AddrOf  { expr, is_mut: true  }, range),
            UnOp::Pointer    => self.add_expr(Expr::Pointer { expr, is_mut: false }, range),
            UnOp::PointerMut => self.add_expr(Expr::Pointer { expr, is_mut: true  }, range),
            _ => {
                let name = self.interner.get_or_intern(op.symbol());
                // TODO: create generic context before parsing operand
                let generic_ctx = self.begin_decl_ref_generic_ctx();
                self.decl_ref(None, name, smallvec![expr], true, range, generic_ctx)
            },
        }
    }
    pub fn fn_type(&mut self, param_tys: Vec<ExprId>, ret_ty: ExprId, range: SourceRange) -> ExprId {
        self.add_expr(Expr::FunctionTy { param_tys, ret_ty }, range)
    }
    pub fn start_new_file(&mut self, file: SourceFileId) -> AutoPopStackEntry<ScopeState, ModScopeNsId> {
        let mut global_scope = ModScope::default();
        // Use all of prelude
        global_scope.blanket_uses.push(Namespace::Mod(self.hir.prelude_namespace.unwrap()));
        let global_scope = self.code.hir.mod_scopes.push(global_scope);
        let global_namespace = self.code.hir.mod_ns.push(
            ModScopeNs {
                scope: global_scope,
                parent: None
            }
        );
        self.code.hir.global_scopes.insert(file, global_scope);
        self.push_to_scope_stack(global_namespace, ScopeState::Mod { id: global_scope, namespace: global_namespace, extern_mod: None })
    }

    fn flush_stmt_buffer(&mut self) {
        self.hir.scope_stack.peek_mut(|state| {
            if let Some(ScopeState::Imper { id, stmt_buffer, .. }) = state {
                if let Some(stmt) = *stmt_buffer {
                    let block = self.code.hir.imper_scopes[*id].block;
                    let op = self.code.ops.push(Op::HirItem { item: Item::Expr(stmt.expr), has_semicolon: stmt.has_semicolon });
                    self.code.blocks[block].ops.push(op);
                    *stmt_buffer = None;
                }
            }
        });
    }

    fn scope_item(&mut self, item: Item, has_semicolon: bool) {
        if let ScopeState::Imper { id, .. } = self.hir.scope_stack.peek().unwrap() {
            let block = self.code.hir.imper_scopes[id].block;
            let op = self.code.ops.push(Op::HirItem { item, has_semicolon });
            self.code.blocks[block].ops.push(op);
        }
    }

    pub fn imper_scoped_decl(&mut self, decl: ImperScopedDecl) {
        if let Some(ScopeState::Imper { namespace, .. }) = self.hir.scope_stack.peek() {
            self.code.hir.imper_ns[namespace].decls.push(decl);
        } else {
            panic!("tried to add imperative-scoped declaration in a non-imperative scope");
        }
    }

    pub fn mod_scoped_decl(&mut self, name: Sym, decl: ModScopedDecl) {
        match self.hir.scope_stack.peek().unwrap() {
            ScopeState::Mod { id, .. } => {
                self.code.hir.mod_scopes[id].decl_groups.entry(name).or_default().push(decl);
            },

            // This is a hack to allow intrinsics to be added for comparing enum types
            state @ ScopeState::GenericContext(id) => {
                match self.code.hir.generic_context_ns[id].parent {
                    Some(Namespace::Mod(id)) => {
                        let id = self.code.hir.mod_ns[id].scope;
                        self.code.hir.mod_scopes[id].decl_groups.entry(name).or_default().push(decl);
                    }
                    _ => panic!("tried to add module-scoped declaration in a non-module scope {:?}", state),
                }
            },
            ref state => panic!("tried to add module-scoped declaration in a non-module scope {:?}", state),
        }
    }

    pub fn stmt(&mut self, expr: ExprId, has_semicolon: bool) {
        self.flush_stmt_buffer();
        self.hir.scope_stack.peek_mut(|state| {
            if let Some(ScopeState::Imper { stmt_buffer, .. }) = state {
                *stmt_buffer = Some(
                    BufferedStmt {
                        expr,
                        has_semicolon,
                    }
                );
            }
        });
        if has_semicolon {
            self.flush_stmt_buffer();
        }
    }
    pub fn begin_imper_scope(&mut self) -> AutoPopStackEntry<ScopeState, ImperScopeId> {
        let parent = self.cur_namespace();

        let block = self.code.blocks.push(Block::default());
        let id = self.code.hir.imper_scopes.push(
            ImperScope {
                block,
                terminal_expr: VOID_EXPR,
            }
        );
        let namespace = self.code.hir.imper_ns.push(
            ImperScopeNs {
                decls: Vec::new(),
                parent: Some(parent),
            }
        );
        let entry = self.push_to_scope_stack(
            id,
            ScopeState::Imper {
                id, namespace, stmt_buffer: None,
            }
        );
        
        if let Some(comp_decl) = self.hir.comp_decl_stack.last_mut() {
            assert!(comp_decl.imper_scope_stack > 0 || comp_decl.has_scope.is_none(), "Can't add multiple top-level scopes to a computed decl");
            let is_first_scope = comp_decl.imper_scope_stack == 0;
            if is_first_scope {
                comp_decl.has_scope = Some(id);
            }
            comp_decl.imper_scope_stack += 1;

            if is_first_scope {
                let name = self.code.hir.names[comp_decl.id];
                let num_params = comp_decl.params.end.index() - comp_decl.params.start.index();
                let id = comp_decl.id;
    
                let params = comp_decl.params.clone();
                let generic_params = comp_decl.generic_params.clone();
    
                // Add the current comp decl to the decl scope, to enable recursion
                self.imper_scoped_decl(
                    ImperScopedDecl {
                        name,
                        num_params,
                        id
                    }
                );
    
                // Add parameters to decl scope
                for id in range_iter(params.clone()) {
                    self.imper_scoped_decl(
                        ImperScopedDecl {
                            name: self.code.hir.names[id],
                            num_params: 0,
                            id,
                        }
                    );
                }
    
                // Add generic parameters to decl scope
                for id in range_iter(generic_params.clone()) {
                    self.imper_scoped_decl(
                        ImperScopedDecl {
                            name: self.code.hir.names[id],
                            num_params: 0,
                            id,
                        }
                    );
                }
            }
        }

        entry
    }
    pub fn end_imper_scope(&mut self, _entry: AutoPopStackEntry<ScopeState, ImperScopeId>, has_terminal_expr: bool) {
        if let Some(ScopeState::Imper { id, stmt_buffer, .. }) = self.hir.scope_stack.peek() {
            if has_terminal_expr {
                let terminal_expr = stmt_buffer.expect("must pass terminal expression via Builder::stmt()");
                self.code.hir.imper_scopes[id].terminal_expr = terminal_expr.expr;
            }
        } else {
            panic!("tried to end imperative scope, but the top scope in the stack is not an imperative scope");
        }
    }
    pub fn end_computed_decl(&mut self) {
        let decl_state = self.hir.comp_decl_stack.pop().unwrap();
        if let Decl::Computed { ref mut scope, .. } = df!(decl_state.id.hir) {
            *scope = decl_state.has_scope.unwrap();
        } else {
            panic!("Unexpected decl kind when ending computed decl!");
        }
    }
    fn cur_namespace(&self) -> Namespace {
        match self.hir.scope_stack.peek().unwrap() {
            ScopeState::Imper { namespace, .. } => {
                let end_offset = self.code.hir.imper_ns[namespace].decls.len();
                Namespace::Imper { scope: namespace, end_offset }
            },
            ScopeState::Mod { namespace, .. } => Namespace::Mod(namespace),
            ScopeState::GenericContext(ns) => Namespace::GenericContext(ns),
            ScopeState::Condition { ns, condition_kind: ConditionKind::Requirement } => Namespace::Requirement(ns),
            ScopeState::Condition { ns, condition_kind: ConditionKind::Guarantee } => Namespace::Guarantee(ns),
        }
    }
    pub fn get_range(&self, item: impl Into<ToSourceRange>) -> SourceRange {
        match item.into() {
            ToSourceRange::Item(item) => match item {
                Item::Expr(expr) => ef!(expr.range),
                Item::Decl(decl) => df!(decl.range),
            }
            ToSourceRange::Op(op) => *self.code.mir.source_ranges.get(&op).unwrap(),
            ToSourceRange::SourceRange(range) => range,
        }
    }
}