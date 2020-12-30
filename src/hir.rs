use std::ffi::CString;
use std::ops::Range;

use smallvec::{SmallVec, smallvec};
use string_interner::DefaultSymbol as Sym;
use mire::hir::*;
use mire::index_counter::IndexCounter;
use mire::ty::Type;
use mire::source_info::SourceFileId;

use crate::driver::Driver;
use crate::index_vec::IdxVec;
use crate::builder::{BinOp, UnOp};
use crate::source_info::SourceRange;

#[derive(Debug)]
enum ScopeState {
    Imper {
        id: ImperScopeId,
        namespace: ImperScopeNsId,
        stmt_buffer: Option<ExprId>,
    },
    Mod {
        id: ModScopeId,
        namespace: ModScopeNsId,
    }
}

#[derive(Debug)]
struct CompDeclState {
    has_scope: Option<ImperScopeId>,
    params: Range<DeclId>,
    id: DeclId,
    imper_scope_stack: u32,
    stored_decl_counter: IndexCounter<StoredDeclId>,
}

#[derive(Debug)]
pub struct Builder {
    pub items: IdxVec<ItemId, Item>,
    pub exprs: IdxVec<ExprId, Expr>,
    pub decl_refs: IdxVec<DeclRefId, DeclRef>,
    pub decls: IdxVec<DeclId, Decl>,
    pub expr_to_items: IdxVec<ExprId, ItemId>,
    pub decl_to_items: IdxVec<DeclId, ItemId>,
    pub names: IdxVec<DeclId, Sym>,
    pub explicit_tys: IdxVec<DeclId, Option<ExprId>>,
    pub global_scopes: IdxVec<SourceFileId, ModScopeId>,
    pub imper_scopes: IdxVec<ImperScopeId, ImperScope>,
    pub mod_scopes: IdxVec<ModScopeId, ModScope>,
    pub imper_ns: IdxVec<ImperScopeNsId, ImperScopeNs>,
    pub mod_ns: IdxVec<ModScopeNsId, ModScopeNs>,
    pub void_expr: ExprId,
    pub void_ty: ExprId,
    pub source_ranges: IdxVec<ItemId, SourceRange>,
    pub cast_counter: IndexCounter<CastId>,
    pub structs: IdxVec<StructId, Struct>,
    pub field_decls: IdxVec<FieldDeclId, FieldDecl>,
    pub struct_lits: IndexCounter<StructLitId>,

    comp_decl_stack: Vec<CompDeclState>,
    scope_stack: Vec<ScopeState>,
}

impl Builder {
    pub fn new() -> Self {
        let mut b = Self {
            items: IdxVec::new(),
            exprs: IdxVec::new(),
            decl_refs: IdxVec::new(),
            decls: IdxVec::new(),
            expr_to_items: IdxVec::new(),
            decl_to_items: IdxVec::new(),
            names: IdxVec::new(),
            explicit_tys: IdxVec::new(),
            global_scopes: IdxVec::new(),
            imper_scopes: IdxVec::new(),
            mod_scopes: IdxVec::new(),
            imper_ns: IdxVec::new(),
            mod_ns: IdxVec::new(),
            void_expr: ExprId::new(0),
            void_ty: ExprId::new(1),
            source_ranges: IdxVec::new(),
            cast_counter: IndexCounter::new(),
            structs: IdxVec::new(),
            struct_lits: IndexCounter::new(),
            field_decls: IdxVec::new(),
            comp_decl_stack: Vec::new(),
            scope_stack: Vec::new(),
        };
        b.push(Expr::Void, SourceRange::default());
        b.push(Expr::ConstTy(Type::Void), SourceRange::default());
        b
    }

    pub fn start_new_file(&mut self, file: SourceFileId) {
        let global_scope = self.mod_scopes.push(ModScope::default());
        let global_namespace = self.mod_ns.push(
            ModScopeNs {
                scope: global_scope,
                parent: None
            }
        );
        self.scope_stack = vec![ScopeState::Mod { id: global_scope, namespace: global_namespace }];
        self.global_scopes.push_at(file, global_scope);
    }

    pub fn import(&mut self, file: SourceFileId, range: SourceRange) -> ExprId {
        self.push(Expr::Import { file }, range)
    }

    fn push(&mut self, expr: Expr, range: SourceRange) -> ExprId {
        let expr_id = self.exprs.push(expr);
        let item_id = self.items.push(Item::Expr(expr_id));
        self.expr_to_items.push_at(expr_id, item_id);
        self.source_ranges.push_at(item_id, range);

        expr_id
    }

    fn flush_stmt_buffer(&mut self) {
        if let Some(ScopeState::Imper { id, stmt_buffer, .. }) = self.scope_stack.last_mut() {
            if let Some(stmt) = *stmt_buffer {
                self.imper_scopes[*id].items.push(ScopeItem::Stmt(stmt));
                *stmt_buffer = None;
            }
        }
    }

    fn scope_item(&mut self, item: ScopeItem) {
        if let &ScopeState::Imper { id, .. } = self.scope_stack.last().unwrap() {
            self.imper_scopes[id].items.push(item);
        }
    }

    fn decl(&mut self, decl: Decl, name: Sym, explicit_ty: Option<ExprId>, range: SourceRange) -> DeclId {
        let decl_id = self.decls.push(decl);
        self.explicit_tys.push_at(decl_id, explicit_ty);
        self.names.push_at(decl_id, name);

        let item_id = self.items.push(Item::Decl(decl_id));
        self.decl_to_items.push_at(decl_id, item_id);

        self.source_ranges.push_at(item_id, range);

        decl_id
    }

    pub fn int_lit(&mut self, lit: u64, range: SourceRange) -> ExprId {
        self.push(Expr::IntLit { lit }, range)
    }
    pub fn dec_lit(&mut self, lit: f64, range: SourceRange) -> ExprId { 
        self.push(Expr::DecLit { lit }, range)
    }
    pub fn str_lit(&mut self, lit: CString, range: SourceRange) -> ExprId { 
        self.push(Expr::StrLit { lit }, range)
    }
    pub fn char_lit(&mut self, lit: i8, range: SourceRange) -> ExprId { 
        self.push(Expr::CharLit { lit }, range)
    }
    pub fn cast(&mut self, expr: ExprId, ty: ExprId, range: SourceRange) -> ExprId {
        let cast_id = self.cast_counter.next();
        self.push(Expr::Cast { expr, ty, cast_id }, range)
    }

    fn imper_scoped_decl(&mut self, decl: ImperScopedDecl) {
        if let Some(&ScopeState::Imper { namespace, .. }) = self.scope_stack.last() {
            self.imper_ns[namespace].decls.push(decl);
        } else {
            panic!("tried to add imperative-scoped declaration in a non-imperative scope");
        }
    }

    fn mod_scoped_decl(&mut self, name: Sym, decl: ModScopedDecl) {
        if let Some(&ScopeState::Mod { id, .. }) = self.scope_stack.last() {
            self.mod_scopes[id].decl_groups.entry(name).or_default().push(decl);
        } else {
            panic!("tried to add module-scoped declaration in a non-module scope");
        }
    }

    pub fn stored_decl(&mut self, name: Sym, explicit_ty: Option<ExprId>, is_mut: bool, root_expr: ExprId, range: SourceRange) {
        self.flush_stmt_buffer();
        match self.scope_stack.last().unwrap() {
            ScopeState::Imper { .. } => {
                let decl = self.comp_decl_stack.last_mut().unwrap();
                let id = decl.stored_decl_counter.next();

                let decl_id = self.decl(Decl::Stored { id, is_mut, root_expr }, name, explicit_ty, range);
                self.scope_item(ScopeItem::StoredDecl { decl_id, id, root_expr });
                self.imper_scoped_decl(
                    ImperScopedDecl {
                        name,
                        num_params: 0,
                        id: decl_id,
                    }
                );
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
            },
        }
    }
    pub fn ret(&mut self, expr: ExprId, range: SourceRange) -> ExprId {
        let decl = self.comp_decl_stack.last().map(|decl| decl.id);
        self.push(Expr::Ret { expr, decl }, range)
    }
    pub fn if_expr(&mut self, condition: ExprId, then_scope: ImperScopeId, else_scope: Option<ImperScopeId>, range: SourceRange) -> ExprId {
        self.push(
            Expr::If { condition, then_scope, else_scope },
            range,
        )
    }
    pub fn while_expr(&mut self, condition: ExprId, scope: ImperScopeId, range: SourceRange) -> ExprId {
        self.push(Expr::While { condition, scope }, range)
    }
    pub fn stmt(&mut self, expr: ExprId) {
        self.flush_stmt_buffer();
        if let Some(ScopeState::Imper { stmt_buffer, .. }) = self.scope_stack.last_mut() {
            *stmt_buffer = Some(expr);
        }
    }
    pub fn do_expr(&mut self, scope: ImperScopeId, range: SourceRange) -> ExprId {
        self.push(Expr::Do { scope }, range)
    }
    pub fn begin_imper_scope(&mut self) -> ImperScopeId {
        let parent = self.cur_namespace();

        let comp_decl = self.comp_decl_stack.last_mut().unwrap();
        assert!(comp_decl.imper_scope_stack > 0 || comp_decl.has_scope.is_none(), "Can't add multiple top-level scopes to a computed decl");
        let id = self.imper_scopes.push(
            ImperScope {
                items: Vec::new(),
                terminal_expr: self.void_expr,
            }
        );
        let namespace = self.imper_ns.push(
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

        self.scope_stack.push(
            ScopeState::Imper {
                id, namespace, stmt_buffer: None,
            }
        );

        if is_first_scope {
            let name = self.names[comp_decl.id];
            let num_params = comp_decl.params.end.index() - comp_decl.params.start.index();
            let id = comp_decl.id;

            let params = comp_decl.params.clone();

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
                        name: self.names[id],
                        num_params: 0,
                        id,
                    }
                );
            }
        }

        id
    }
    pub fn end_imper_scope(&mut self, has_terminal_expr: bool) {
        if let Some(&ScopeState::Imper { id, stmt_buffer, .. }) = self.scope_stack.last() {
            if has_terminal_expr {
                let terminal_expr = stmt_buffer.expect("must pass terminal expression via Builder::stmt()");
                self.imper_scopes[id].terminal_expr = terminal_expr;
            }
            self.scope_stack.pop().unwrap();
        } else {
            panic!("tried to end imperative scope, but the top scope in the stack is not an imperative scope");
        }
    }
    pub fn field_decl(&mut self, name: Sym, ty: ExprId, index: usize, range: SourceRange) -> FieldDeclId {
        let field = self.field_decls.next_id();
        let decl = self.decl(Decl::Field(field), name, Some(ty), range);
        self.field_decls.push_at(field, FieldDecl { decl, name, ty, index });
        field
    }
    pub fn strukt(&mut self, fields: Vec<FieldDeclId>, range: SourceRange) -> ExprId {
        let strukt = self.structs.push(Struct { fields });
        self.push(Expr::Struct(strukt), range)
    }
    pub fn struct_lit(&mut self, ty: ExprId, fields: Vec<FieldAssignment>, range: SourceRange) -> ExprId {
        let id = self.struct_lits.next();
        self.push(Expr::StructLit { ty, fields, id }, range)
    }
    pub fn begin_module(&mut self) -> ExprId {
        let parent = self.cur_namespace();
        let id = self.mod_scopes.push(ModScope::default());
        let namespace = self.mod_ns.push(
            ModScopeNs {
                scope: id, parent: Some(parent)
            }
        );
        self.scope_stack.push(ScopeState::Mod { id, namespace });
        self.push(Expr::Mod { id }, SourceRange::default())
    }
    pub fn end_module(&mut self, mod_expr: ExprId, range: SourceRange) {
        debug_assert!(matches!(self.exprs[mod_expr], Expr::Mod { .. }));

        if let Some(ScopeState::Mod { .. }) = self.scope_stack.last() {
            self.source_ranges[self.expr_to_items[mod_expr]] = range;
            self.scope_stack.pop().unwrap();
        } else {
            panic!("tried to end the module, but the top scope in the stack is not a module scope");
        }
    }
    pub fn begin_computed_decl(&mut self, name: Sym, param_names: SmallVec<[Sym; 2]>, param_tys: SmallVec<[ExprId; 2]>, param_ranges: SmallVec<[SourceRange; 2]>, explicit_ty: Option<ExprId>, proto_range: SourceRange) {
        // This is a placeholder value that gets replaced once the parameter declarations get allocated.
        let id = self.decl(Decl::Const(ExprId::new(usize::MAX)), name, explicit_ty, proto_range);
        assert_eq!(param_names.len(), param_tys.len());
        self.decls.reserve(param_tys.len());
        let first_param = DeclId::new(self.decls.len());
        param_tys.iter()
            .enumerate()
            .zip(&param_names)
            .zip(&param_ranges)
            .for_each(|(((index, ty), &name), &range)| {
                self.decl(Decl::Parameter { index }, name, Some(ty.clone()), range);
            });
        let last_param = DeclId::new(self.decls.len());
        let params = first_param..last_param;
        // `end_computed_decl` will attach the real scope to this decl; we don't have it yet
        self.decls[id] = Decl::Computed {
            param_tys,
            params: params.clone(),
            scope: ImperScopeId::new(usize::MAX)
        };
        match self.scope_stack.last().unwrap() {
            ScopeState::Imper { .. } => {
                self.flush_stmt_buffer();
                self.scope_item(ScopeItem::ComputedDecl(id));
            },
            &ScopeState::Mod { .. } => {
                self.mod_scoped_decl(name, ModScopedDecl { num_params: param_names.len(), id });
            }
        }
        self.comp_decl_stack.push(
            CompDeclState {
                has_scope: None,
                params,
                id,
                imper_scope_stack: 0,
                stored_decl_counter: IndexCounter::new(),
            }
        );
    }
    pub fn end_computed_decl(&mut self) {
        let decl_state = self.comp_decl_stack.pop().unwrap();
        if let Decl::Computed { ref mut scope, .. } = self.decls[decl_state.id] {
            *scope = decl_state.has_scope.unwrap();
        } else {
            panic!("Unexpected decl kind when ending computed decl!");
        }
    }
    fn cur_namespace(&self) -> Namespace {
        match *self.scope_stack.last().unwrap() {
            ScopeState::Imper { namespace, .. } => {
                let end_offset = self.imper_ns[namespace].decls.len();
                Namespace::Imper { scope: namespace, end_offset }
            },
            ScopeState::Mod { namespace, .. } => Namespace::Mod(namespace),
        }
    }
    pub fn decl_ref(&mut self, base_expr: Option<ExprId>, name: Sym, arguments: SmallVec<[ExprId; 2]>, has_parens: bool, range: SourceRange) -> ExprId {
        let namespace = match base_expr {
            Some(base_expr) => Namespace::MemberRef { base_expr },
            None => self.cur_namespace(),
        };
        let id = self.decl_refs.next_id();
        let num_arguments = arguments.len();
        let expr = self.push(Expr::DeclRef { arguments, id }, range);
        self.decl_refs.push_at(
            id,
            DeclRef {
                name,
                namespace,
                num_arguments,
                has_parens,
                expr,
            }
        );
        expr
    }
    pub fn get_range(&self, id: ExprId) -> SourceRange { self.source_ranges[self.expr_to_items[id]].clone() }
    pub fn set_range(&mut self, id: ExprId, range: SourceRange) { self.source_ranges[self.expr_to_items[id]] = range; }
}

impl Driver {
    pub fn add_const_ty(&mut self, ty: Type) -> ExprId {
        self.hir.push(Expr::ConstTy(ty), SourceRange::default())
    }
    pub fn add_intrinsic(&mut self, intrinsic: Intrinsic, param_tys: SmallVec<[ExprId; 2]>, ret_ty: ExprId, function_like: bool) {
        let name = self.interner.get_or_intern(intrinsic.name());
        let num_params = param_tys.len();
        let id = self.hir.decl(Decl::Intrinsic { intr: intrinsic, param_tys, function_like }, name, Some(ret_ty), SourceRange::default());
        assert_eq!(self.hir.scope_stack.len(), 1, "cannot add intrinsic anywhere except global scope");
        self.hir.mod_scoped_decl(
            name,
            ModScopedDecl { num_params, id }
        );
    }
    pub fn bin_op(&mut self, op: BinOp, lhs: ExprId, rhs: ExprId, range: SourceRange) -> ExprId {
        match op {
            BinOp::Assign => self.hir.push(Expr::Set { lhs, rhs }, range),
            _ => {
                let name = self.interner.get_or_intern(op.symbol());
                self.hir.decl_ref(None, name, smallvec![lhs, rhs], false, range)
            }
        }
    }
    pub fn un_op(&mut self, op: UnOp, expr: ExprId, range: SourceRange) -> ExprId {
        match op {
            UnOp::Deref      => self.hir.push(Expr::Deref(expr), range),
            UnOp::AddrOf     => self.hir.push(Expr::AddrOf  { expr, is_mut: false }, range),
            UnOp::AddrOfMut  => self.hir.push(Expr::AddrOf  { expr, is_mut: true  }, range),
            UnOp::Pointer    => self.hir.push(Expr::Pointer { expr, is_mut: false }, range),
            UnOp::PointerMut => self.hir.push(Expr::Pointer { expr, is_mut: true  }, range),
            _ => {
                let name = self.interner.get_or_intern(op.symbol());
                self.hir.decl_ref(None, name, smallvec![expr], false, range)
            },
        }
    }
}
