use std::ffi::CString;
use std::ops::Range;
use std::collections::HashSet;

use smallvec::{SmallVec, smallvec};
use string_interner::{DefaultSymbol as Sym, Symbol};

use dire::{Op, Block};
use dire::hir::*;
use dire::index_counter::IndexCounter;
use dire::ty::Type;
use dire::source_info::{SourceFileId, SourceRange};

use crate::driver::Driver;
use crate::index_vec::*;
use crate::builder::{BinOp, UnOp};
use crate::source_info::ToSourceRange;
use crate::debug::{self, Message as DvdMessage};

use dusk_proc_macros::*;

// TODO: switch to AOS here
// TODO: move to dire, perhaps
#[derive(Debug)]
pub struct GenericParamList {
    pub names: SmallVec<[Sym; 1]>,
    pub ids: Range<GenericParamId>,
    pub ranges: SmallVec<[SourceRange; 1]>,
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

#[derive(Debug)]
pub enum ScopeState {
    Imper {
        id: ImperScopeId,
        namespace: ImperScopeNsId,
        stmt_buffer: Option<ExprId>,
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
}

#[derive(Debug)]
pub struct Builder {
    comp_decl_stack: Vec<CompDeclState>,
    scope_stack: Vec<ScopeState>,
    debug_marked_exprs: HashSet<ExprId>,
    pub generic_params: IndexCounter<GenericParamId>,

    pub requires_sym: Sym,
    pub guarantees_sym: Sym,
    pub return_value_sym: Sym,
    pub underscore_sym: Sym,
}

impl Default for Builder {
    fn default() -> Self {
        Builder {
            comp_decl_stack: Default::default(),
            scope_stack: Default::default(),
            debug_marked_exprs: Default::default(),

            generic_params: IndexCounter::new(),

            // Note: gets initialized in Driver::initialize_hir() below
            requires_sym: Sym::try_from_usize((u32::MAX - 1) as usize).unwrap(),
            guarantees_sym: Sym::try_from_usize((u32::MAX - 1) as usize).unwrap(),
            return_value_sym: Sym::try_from_usize((u32::MAX - 1) as usize).unwrap(),
            underscore_sym: Sym::try_from_usize((u32::MAX - 1) as usize).unwrap(),
        }
    }
}

#[derive(Debug)]
pub enum ConditionKind {
    Requirement, Guarantee,
}


impl Driver {
    pub fn initialize_hir(&mut self) {
        self.push_expr(Expr::Void, SourceRange::default());
        self.push_expr(Expr::Error, SourceRange::default());
        self.push_expr(Expr::ConstTy(Type::Void), SourceRange::default());
        self.push_expr(Expr::ConstTy(Type::Ty), SourceRange::default());
        self.push_expr(Expr::ConstTy(Type::Error), SourceRange::default());
        assert_eq!(self.code.hir_code.exprs.len(), 5);

        let return_value_sym = self.interner.get_or_intern_static("return_value");
        self.decl(Decl::ReturnValue, return_value_sym, None, SourceRange::default());

        self.hir.requires_sym = self.interner.get_or_intern_static("requires");
        self.hir.guarantees_sym = self.interner.get_or_intern_static("guarantees");
        self.hir.return_value_sym = return_value_sym;
        self.hir.underscore_sym = self.interner.get_or_intern_static("_");
    }

    pub fn debug_mark_expr(&mut self, expr: ExprId) {
        self.hir.debug_marked_exprs.insert(expr);
    }

    #[allow(unused)]
    pub fn expr_is_debug_marked(&self, expr: ExprId) -> bool {
        self.hir.debug_marked_exprs.contains(&expr)
    }

    fn push_expr(&mut self, expr: Expr, range: SourceRange) -> ExprId {
        let expr_id = self.code.hir_code.exprs.push(expr);
        let item_id = self.code.hir_code.items.push(Item::Expr(expr_id));
        self.code.hir_code.expr_to_items.push_at(expr_id, item_id);
        self.code.hir_code.source_ranges.push_at(item_id, range);

        debug::send(|| DvdMessage::DidAddExpr { id: expr_id, item_id, text: None });

        expr_id
    }

    pub fn import(&mut self, file: SourceFileId, range: SourceRange) -> ExprId {
        self.push_expr(Expr::Import { file }, range)
    }

    pub fn decl(&mut self, decl: Decl, name: Sym, explicit_ty: Option<ExprId>, range: SourceRange) -> DeclId {
        let decl_id = self.code.hir_code.decls.push(decl);
        self.code.hir_code.explicit_tys.push_at(decl_id, explicit_ty);
        self.code.hir_code.names.push_at(decl_id, name);

        let item_id = self.code.hir_code.items.push(Item::Decl(decl_id));
        self.code.hir_code.decl_to_items.push_at(decl_id, item_id);

        self.code.hir_code.source_ranges.push_at(item_id, range);

        debug::send(|| DvdMessage::DidAddDecl { id: decl_id, item_id, text: None });

        decl_id
    }

    pub fn int_lit(&mut self, lit: u64, range: SourceRange) -> ExprId {
        self.push_expr(Expr::IntLit { lit }, range)
    }
    pub fn dec_lit(&mut self, lit: f64, range: SourceRange) -> ExprId { 
        self.push_expr(Expr::DecLit { lit }, range)
    }
    pub fn str_lit(&mut self, lit: CString, range: SourceRange) -> ExprId { 
        self.push_expr(Expr::StrLit { lit }, range)
    }
    pub fn char_lit(&mut self, lit: i8, range: SourceRange) -> ExprId { 
        self.push_expr(Expr::CharLit { lit }, range)
    }
    pub fn bool_lit(&mut self, lit: bool, range: SourceRange) -> ExprId { 
        self.push_expr(Expr::BoolLit { lit }, range)
    }
    pub fn cast(&mut self, expr: ExprId, ty: ExprId, range: SourceRange) -> ExprId {
        let cast_id = self.code.hir_code.cast_counter.next();
        self.push_expr(Expr::Cast { expr, ty, cast_id }, range)
    }
    pub fn stored_decl(&mut self, name: Sym, _generic_params: GenericParamList, explicit_ty: Option<ExprId>, is_mut: bool, root_expr: ExprId, range: SourceRange) -> DeclId {
        self.flush_stmt_buffer();
        match self.hir.scope_stack.last().unwrap() {
            ScopeState::Imper { .. } => {
                let decl = self.hir.comp_decl_stack.last_mut().unwrap();
                let id = decl.stored_decl_counter.next();

                let decl_id = self.decl(Decl::Stored { id, is_mut, root_expr }, name, explicit_ty, range);
                self.scope_item(Item::Decl(decl_id));
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
                let decl_id = self.decl(
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
        self.push_expr(Expr::Ret { expr, decl }, range)
    }
    pub fn if_expr(&mut self, condition: ExprId, then_scope: ImperScopeId, else_scope: Option<ImperScopeId>, range: SourceRange) -> ExprId {
        self.push_expr(
            Expr::If { condition, then_scope, else_scope },
            range,
        )
    }
    pub fn while_expr(&mut self, condition: ExprId, scope: ImperScopeId, range: SourceRange) -> ExprId {
        self.push_expr(Expr::While { condition, scope }, range)
    }
    pub fn switch_expr(&mut self, scrutinee: ExprId, cases: Vec<SwitchCase>, range: SourceRange) -> ExprId {
        self.push_expr(Expr::Switch { scrutinee, cases }, range)
    }
    pub fn do_expr(&mut self, scope: ImperScopeId, range: SourceRange) -> ExprId {
        self.push_expr(Expr::Do { scope }, range)
    }
    pub fn field_decl(&mut self, name: Sym, strukt: StructId, ty: ExprId, index: usize, range: SourceRange) -> FieldDecl {
        let decl = self.decl(Decl::Field { strukt, index }, name, Some(ty), range);
        FieldDecl { decl, name, ty }
    }
    pub fn variant_decl(&mut self, name: Sym, enuum: ExprId, enum_id: EnumId, index: usize, payload_ty: Option<ExprId>, range: SourceRange) -> VariantDecl {
        let decl = self.decl(Decl::Variant { enuum: enum_id, index, payload_ty }, name, Some(enuum), range);
        VariantDecl { decl, name, enuum, payload_ty }
    }
    pub fn reserve_struct(&mut self) -> (ExprId, StructId) {
        let strukt = self.code.hir_code.structs.push(Struct { fields: Vec::new() });
        let expr = self.push_expr(Expr::Struct(strukt), Default::default());
        (expr, strukt)
    }
    pub fn finish_struct(&mut self, fields: Vec<FieldDecl>, range: SourceRange, expr: ExprId, strukt: StructId) {
        self.code.hir_code.structs[strukt] = Struct { fields };
        ef!(expr.range) = range;
    }
    pub fn reserve_enum(&mut self) -> (ExprId, EnumId) {
        let enuum = self.code.hir_code.enums.push(Enum { variants: Vec::new() });
        let expr = self.push_expr(Expr::Enum(enuum), Default::default());
        (expr, enuum)
    }
    pub fn finish_enum(&mut self, variants: Vec<VariantDecl>, range: SourceRange, expr: ExprId, enuum: EnumId) {
        self.code.hir_code.enums[enuum] = Enum { variants };
        ef!(expr.range) = range;
    }
    pub fn struct_lit(&mut self, ty: ExprId, fields: Vec<FieldAssignment>, range: SourceRange) -> ExprId {
        let id = self.code.hir_code.struct_lits.next();
        self.push_expr(Expr::StructLit { ty, fields, id }, range)
    }
    pub fn error_expr(&mut self, range: SourceRange) -> ExprId {
        self.push_expr(Expr::Error, range)
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
                let id = self.code.hir_code.pattern_binding_decls.push(binding_decl);
                let decl = self.decl(Decl::PatternBinding { id, is_mut: false }, name.symbol, None, name.range);
                let decl = ImperScopedDecl { name: name.symbol, num_params: 0, id: decl };
                decls.push(decl);
                bindings.push(id);
            },
        }
        (decls, bindings)
    }
    pub fn begin_condition_namespace(&mut self) -> ConditionNsId {
        let parent = self.cur_namespace();
        let ns = self.code.hir_code.condition_ns.push(ConditionNs { func: DeclId::from_raw(u32::MAX), parent: Some(parent) });
        // This condition kind is just a placeholder which will be reset by each attribute. It is done this way so I can share a single
        // condition namespace across all condition attributes on a single function.
        self.hir.scope_stack.push(ScopeState::Condition { ns, condition_kind: ConditionKind::Requirement });

        ns
    }
    pub fn set_condition_kind(&mut self, desired_ns: ConditionNsId, desired_condition_kind: ConditionKind) {
        if let Some(ScopeState::Condition { ns, condition_kind }) = self.hir.scope_stack.last_mut() {
            assert_eq!(&*ns, &desired_ns, "tried to set condition_kind, but the current condition scope doesn't match");
            *condition_kind = desired_condition_kind;
        } else {
            panic!("tried to set condition_kind, but the top of the scope stack is not a condition namespace");
        }
    }
    pub fn end_condition_namespace(&mut self, desired_ns: ConditionNsId) {
        if let Some(&ScopeState::Condition { ns, .. }) = self.hir.scope_stack.last() {
            assert_eq!(ns, desired_ns, "tried to end condition namespace, but the current condition scope doesn't match");
            self.hir.scope_stack.pop();
        } else {
            panic!("Tried to end condition namespace, but the top of the scope stack is not a condition namespace");
        }
    }
    pub fn begin_generic_context(&mut self, generic_params: Range<DeclId>) -> GenericContextNsId {
        let parent = self.cur_namespace();
        let ns = self.code.hir_code.generic_context_ns.push(GenericContextNs { generic_params, parent: Some(parent) });
        self.hir.scope_stack.push(ScopeState::GenericContext(ns));

        ns
    }
    pub fn end_generic_context(&mut self, desired_ns: GenericContextNsId) {
        if let Some(&ScopeState::GenericContext(ns)) = self.hir.scope_stack.last() {
            assert_eq!(ns, desired_ns, "tried to end generic context, but the current condition scope doesn't match");
            self.hir.scope_stack.pop();
        } else {
            panic!("Tried to end generic context, but the top of the scope stack is not a generic context namespace");
        }
    }
    pub fn begin_module(&mut self, extern_mod: Option<ExternModId>) -> ExprId {
        let parent = self.cur_namespace();
        let id = self.code.hir_code.mod_scopes.push(ModScope::default());
        let namespace = self.code.hir_code.mod_ns.push(
            ModScopeNs {
                scope: id, parent: Some(parent)
            }
        );
        self.hir.scope_stack.push(ScopeState::Mod { id, namespace, extern_mod });
        self.push_expr(Expr::Mod { id }, SourceRange::default())
    }
    pub fn begin_computed_decl(&mut self, name: Sym, param_names: SmallVec<[Sym; 2]>, param_tys: SmallVec<[ExprId; 2]>, param_ranges: SmallVec<[SourceRange; 2]>, generic_params: Range<DeclId>, return_ty: ExprId, proto_range: SourceRange) -> DeclId {
        // This is a placeholder value that gets replaced once the parameter declarations get allocated.
        let id = self.decl(Decl::Const(ExprId::new(u32::MAX as usize)), name, Some(return_ty), proto_range);

        assert_eq!(param_names.len(), param_tys.len());
        self.code.hir_code.decls.reserve(param_tys.len());
        let first_param = DeclId::new(self.code.hir_code.decls.len());
        param_tys.iter()
            .enumerate()
            .zip(&param_names)
            .zip(&param_ranges)
            .for_each(|(((index, ty), &name), &range)| {
                self.decl(Decl::Parameter { index }, name, Some(ty.clone()), range);
            });
        let last_param = DeclId::new(self.code.hir_code.decls.len());
        let params = first_param..last_param;

        // `end_computed_decl` will attach the real scope to this decl; we don't have it yet
        df!(id.hir) = Decl::Computed {
            param_tys,
            params: params.clone(),
            scope: ImperScopeId::new(u32::MAX as usize),
            generic_params: generic_params.clone(),
        };
        match self.hir.scope_stack.last().unwrap() {
            ScopeState::Imper { .. } => {
                self.flush_stmt_buffer();
                self.scope_item(Item::Decl(id));
            },
            &ScopeState::Mod { .. } => {
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
            }
        );

        id
    }
    pub fn comp_decl_prototype(&mut self, name: Sym, param_tys: SmallVec<[ExprId; 2]>, _param_ranges: SmallVec<[SourceRange; 2]>, return_ty: ExprId, range: SourceRange) -> DeclId {
        // This is a placeholder value that gets replaced once the parameter declarations get allocated.
        let num_params = param_tys.len();
        let extern_func = if let &ScopeState::Mod { extern_mod: Some(extern_mod), .. } = self.hir.scope_stack.last().unwrap() {
            let funcs = &mut self.code.hir_code.extern_mods[extern_mod].imported_functions;
            let index = funcs.len();
            let name = self.interner.resolve(name).unwrap();
            funcs.push(ExternFunction { name: name.to_string(), param_tys: param_tys.iter().cloned().collect(), return_ty });
            Some(
                ExternFunctionRef {
                    extern_mod,
                    index
                }
            )
        } else {
            None
        };
        let id = self.decl(Decl::ComputedPrototype { param_tys, extern_func }, name, Some(return_ty), range);
        match self.hir.scope_stack.last().unwrap() {
            ScopeState::Imper { .. } => {
                self.flush_stmt_buffer();
                self.scope_item(Item::Decl(id));
            },
            &ScopeState::Mod { .. } => {
                self.mod_scoped_decl(name, ModScopedDecl { num_params, id });
            },
            ScopeState::Condition { .. } | ScopeState::GenericContext(_) => panic!("Computed decls are not supported in this position"),
        }

        id
    }
    pub fn decl_ref(&mut self, base_expr: Option<ExprId>, name: Sym, arguments: SmallVec<[ExprId; 2]>, has_parens: bool, range: SourceRange) -> ExprId {
        let namespace = match base_expr {
            Some(base_expr) => Namespace::MemberRef { base_expr },
            None => self.cur_namespace(),
        };
        let id = self.code.hir_code.decl_refs.next_idx();
        let expr = self.push_expr(Expr::DeclRef { id }, range);
        self.code.hir_code.decl_refs.push_at(
            id,
            DeclRef {
                name,
                namespace,
                expr,
            }
        );
        if has_parens {
            self.push_expr(Expr::Call { callee: expr, arguments, decl_ref_id: id }, range)
        } else {
            expr
        }
    }
    // TODO: intern constant type expressions
    pub fn add_const_ty(&mut self, ty: Type) -> ExprId {
        self.push_expr(Expr::ConstTy(ty), SourceRange::default())
    }
    pub fn add_intrinsic(&mut self, intrinsic: Intrinsic, param_tys: SmallVec<[ExprId; 2]>, ret_ty: ExprId, function_like: bool) {
        let name = self.interner.get_or_intern(intrinsic.name());
        let num_params = param_tys.len();
        let id = self.decl(Decl::Intrinsic { intr: intrinsic, param_tys, function_like }, name, Some(ret_ty), SourceRange::default());
        assert_eq!(self.hir.scope_stack.len(), 1, "cannot add intrinsic anywhere except global scope");
        self.mod_scoped_decl(
            name,
            ModScopedDecl { num_params, id }
        );
    }
    pub fn bin_op(&mut self, op: BinOp, lhs: ExprId, rhs: ExprId, range: SourceRange) -> ExprId {
        match op {
            BinOp::Assign => self.push_expr(Expr::Set { lhs, rhs }, range),
            _ => {
                let name = self.interner.get_or_intern(op.symbol());
                self.decl_ref(None, name, smallvec![lhs, rhs], true, range)
            }
        }
    }
    pub fn un_op(&mut self, op: UnOp, expr: ExprId, range: SourceRange) -> ExprId {
        match op {
            UnOp::Deref      => self.push_expr(Expr::Deref(expr), range),
            UnOp::AddrOf     => self.push_expr(Expr::AddrOf  { expr, is_mut: false }, range),
            UnOp::AddrOfMut  => self.push_expr(Expr::AddrOf  { expr, is_mut: true  }, range),
            UnOp::Pointer    => self.push_expr(Expr::Pointer { expr, is_mut: false }, range),
            UnOp::PointerMut => self.push_expr(Expr::Pointer { expr, is_mut: true  }, range),
            _ => {
                let name = self.interner.get_or_intern(op.symbol());
                self.decl_ref(None, name, smallvec![expr], true, range)
            },
        }
    }
    pub fn fn_type(&mut self, param_tys: Vec<ExprId>, ret_ty: ExprId, range: SourceRange) -> ExprId {
        self.push_expr(Expr::FunctionTy { param_tys, ret_ty }, range)
    }
    pub fn start_new_file(&mut self, file: SourceFileId) {
        let global_scope = self.code.hir_code.mod_scopes.push(ModScope::default());
        let global_namespace = self.code.hir_code.mod_ns.push(
            ModScopeNs {
                scope: global_scope,
                parent: None
            }
        );
        self.hir.scope_stack = vec![ScopeState::Mod { id: global_scope, namespace: global_namespace, extern_mod: None }];
        self.code.hir_code.global_scopes.push_at(file, global_scope);
    }

    fn flush_stmt_buffer(&mut self) {
        if let Some(ScopeState::Imper { id, stmt_buffer, .. }) = self.hir.scope_stack.last_mut() {
            if let Some(stmt) = *stmt_buffer {
                let block = self.code.hir_code.imper_scopes[*id].block;
                let op = self.code.ops.push(Op::HirItem(Item::Expr(stmt)));
                self.code.blocks[block].ops.push(op);
                *stmt_buffer = None;
            }
        }
    }

    fn scope_item(&mut self, item: Item) {
        if let &ScopeState::Imper { id, .. } = self.hir.scope_stack.last().unwrap() {
            let block = self.code.hir_code.imper_scopes[id].block;
            let op = self.code.ops.push(Op::HirItem(item));
            self.code.blocks[block].ops.push(op);
        }
    }

    pub fn imper_scoped_decl(&mut self, decl: ImperScopedDecl) {
        if let Some(&ScopeState::Imper { namespace, .. }) = self.hir.scope_stack.last() {
            self.code.hir_code.imper_ns[namespace].decls.push(decl);
        } else {
            panic!("tried to add imperative-scoped declaration in a non-imperative scope");
        }
    }

    fn mod_scoped_decl(&mut self, name: Sym, decl: ModScopedDecl) {
        if let Some(&ScopeState::Mod { id, .. }) = self.hir.scope_stack.last() {
            self.code.hir_code.mod_scopes[id].decl_groups.entry(name).or_default().push(decl);
        } else {
            panic!("tried to add module-scoped declaration in a non-module scope");
        }
    }

    pub fn stmt(&mut self, expr: ExprId) {
        self.flush_stmt_buffer();
        if let Some(ScopeState::Imper { stmt_buffer, .. }) = self.hir.scope_stack.last_mut() {
            *stmt_buffer = Some(expr);
        }
    }
    pub fn begin_imper_scope(&mut self) -> ImperScopeId {
        let parent = self.cur_namespace();

        let comp_decl = self.hir.comp_decl_stack.last_mut().unwrap();
        assert!(comp_decl.imper_scope_stack > 0 || comp_decl.has_scope.is_none(), "Can't add multiple top-level scopes to a computed decl");
        let block = self.code.blocks.push(Block::default());
        let id = self.code.hir_code.imper_scopes.push(
            ImperScope {
                block,
                terminal_expr: VOID_EXPR,
            }
        );
        let namespace = self.code.hir_code.imper_ns.push(
            ImperScopeNs {
                decls: Vec::new(),
                parent: Some(parent),
            }
        );
        
        let is_first_scope = comp_decl.imper_scope_stack == 0;
        if is_first_scope {
            comp_decl.has_scope = Some(id);
        }
        comp_decl.imper_scope_stack += 1;

        self.hir.scope_stack.push(
            ScopeState::Imper {
                id, namespace, stmt_buffer: None,
            }
        );

        if is_first_scope {
            let name = self.code.hir_code.names[comp_decl.id];
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
            for i in params.start.index()..params.end.index() {
                let id = DeclId::new(i);
                self.imper_scoped_decl(
                    ImperScopedDecl {
                        name: self.code.hir_code.names[id],
                        num_params: 0,
                        id,
                    }
                );
            }

            // Add generic parameters to decl scope
            for i in generic_params.start.index()..generic_params.end.index() {
                let id = DeclId::new(i);
                self.imper_scoped_decl(
                    ImperScopedDecl {
                        name: self.code.hir_code.names[id],
                        num_params: 0,
                        id,
                    }
                );
            }
        }

        id
    }
    pub fn end_imper_scope(&mut self, has_terminal_expr: bool) {
        if let Some(&ScopeState::Imper { id, stmt_buffer, .. }) = self.hir.scope_stack.last() {
            if has_terminal_expr {
                let terminal_expr = stmt_buffer.expect("must pass terminal expression via Builder::stmt()");
                self.code.hir_code.imper_scopes[id].terminal_expr = terminal_expr;
            }
            self.hir.scope_stack.pop().unwrap();
        } else {
            panic!("tried to end imperative scope, but the top scope in the stack is not an imperative scope");
        }
    }
    pub fn end_module(&mut self, mod_expr: ExprId, range: SourceRange) {
        debug_assert!(matches!(ef!(mod_expr.hir), Expr::Mod { .. }));

        if let Some(ScopeState::Mod { .. }) = self.hir.scope_stack.last() {
            ef!(mod_expr.range) = range;
            self.hir.scope_stack.pop().unwrap();
        } else {
            panic!("tried to end the module, but the top scope in the stack is not a module scope");
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
        match *self.hir.scope_stack.last().unwrap() {
            ScopeState::Imper { namespace, .. } => {
                let end_offset = self.code.hir_code.imper_ns[namespace].decls.len();
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
            ToSourceRange::Op(op) => self.code.mir_code.source_ranges.get(&op).unwrap().clone(),
            ToSourceRange::SourceRange(range) => range,
        }
    }
}