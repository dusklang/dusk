use std::ffi::CString;
use std::marker::PhantomData;
use std::rc::Rc;

use smallvec::{SmallVec, smallvec};
use string_interner::{DefaultStringInterner as Interner, Sym};

use crate::index_vec::{Idx, IdxVec};
use crate::builder::{self, BinOp, UnOp, ExprId, DeclId, ScopeId, DeclRefId, CastId, Intrinsic};
use crate::source_info::SourceRange;
use crate::ty::Type;

newtype_index!(StoredDeclId pub);

#[derive(Debug)]
pub enum Expr {
    Void,
    IntLit { lit: u64 },
    DecLit { lit: f64 },
    StrLit { lit: CString },
    CharLit { lit: i8 },
    DeclRef { name: Sym, arguments: SmallVec<[ExprId; 2]>, id: DeclRefId },
    AddrOf(ExprId),
    Deref(ExprId),
    Set { lhs: ExprId, rhs: ExprId },
    Do { scope: ScopeId },
    If { condition: ExprId, then_scope: ScopeId, else_scope: Option<ScopeId> },
    While { condition: ExprId, scope: ScopeId },
    Cast { expr: ExprId, ty: Type, cast_id: CastId },
    Ret { expr: ExprId },
}

#[derive(Debug)]
struct ScopeState {
    id: ScopeId,
    stmt_buffer: Option<ExprId>,
}

#[derive(Debug)]
struct CompDeclState {
    has_scope: Option<ScopeId>,
    id: DeclId,
    scope_stack: Vec<ScopeState>,
    num_stored_decls: usize,
}

#[derive(Copy, Clone, Debug)]
pub enum Item {
    Stmt(ExprId),
    StoredDecl { decl_id: DeclId, id: StoredDeclId, root_expr: ExprId },
    ComputedDecl(DeclId),
}

#[derive(Debug)]
pub struct Scope {
    pub items: Vec<Item>,
    pub terminal_expr: ExprId,
}

#[derive(Debug)]
pub enum Decl {
    Computed {
        param_tys: SmallVec<[Type; 2]>,
        params: SmallVec<[DeclId; 2]>,
        scope: ScopeId,
    },
    Stored { id: StoredDeclId, is_mut: bool },
    Parameter {
        /// Parameter index within the function
        index: usize,
    },
    Intrinsic { intr: Intrinsic, param_tys: SmallVec<[Type; 2]>, },
    Static(ExprId),
    Const(ExprId),
}

#[derive(Debug)]
pub struct Program {
    pub exprs: IdxVec<Expr, ExprId>,
    pub num_decl_refs: usize,
    pub decls: IdxVec<Decl, DeclId>,
    pub names: IdxVec<Sym, DeclId>,
    pub explicit_tys: IdxVec<Option<Type>, DeclId>,
    /// The subset of decls that are in the global scope
    pub global_decls: Vec<DeclId>,
    pub scopes: IdxVec<Scope, ScopeId>,
    pub void_expr: ExprId,
    pub source_ranges: IdxVec<SourceRange, ExprId>,
    pub interner: Rc<Interner>,
}

#[derive(Debug)]
pub struct Builder<'src> {
    exprs: IdxVec<Expr, ExprId>,
    num_decl_refs: usize,
    num_casts: usize,

    decls: IdxVec<Decl, DeclId>,
    names: IdxVec<Sym, DeclId>,
    explicit_tys: IdxVec<Option<Type>, DeclId>,
    /// The subset of decls that are in the global scope
    global_decls: Vec<DeclId>,
    scopes: IdxVec<Scope, ScopeId>,
    comp_decl_stack: Vec<CompDeclState>,
    source_ranges: IdxVec<SourceRange, ExprId>,
    void_expr: ExprId,
    interner: Interner,
    
    _phantom_src_lifetime: PhantomData<&'src str>,
}

impl<'src> Builder<'src> {
    pub fn new() -> Self {
        let mut b = Self {
            exprs: IdxVec::new(),
            num_decl_refs: 0,
            num_casts: 0,
            decls: IdxVec::new(),
            names: IdxVec::new(),
            explicit_tys: IdxVec::new(),
            global_decls: Vec::new(),
            scopes: IdxVec::new(),
            comp_decl_stack: Vec::new(),
            source_ranges: IdxVec::new(),
            void_expr: ExprId::new(0),
            interner: Interner::new(),
            _phantom_src_lifetime: PhantomData,
        };
        b.push(Expr::Void, 0..0);
        b
    }

    fn flush_stmt_buffer(&mut self) {
        let comp_decl = self.comp_decl_stack.last_mut();
        if comp_decl.is_none() { return }
        let scope_state = comp_decl.unwrap().scope_stack.last_mut();
        if scope_state.is_none() { return }
        let scope_state = scope_state.unwrap();
        if let Some(stmt) = scope_state.stmt_buffer {
            self.scopes[scope_state.id].items.push(Item::Stmt(stmt));
            scope_state.stmt_buffer = None;
        }
    }

    fn item(&mut self, item: Item) {
        let scope = self.comp_decl_stack.last().unwrap().scope_stack.last().unwrap().id;
        self.scopes[scope].items.push(item);
    }

    fn allocate_decl_ref_id(&mut self) -> DeclRefId {
        let decl_ref_id = DeclRefId::new(self.num_decl_refs);
        self.num_decl_refs += 1;
        decl_ref_id
    }

    fn push(&mut self, expr: Expr, range: SourceRange) -> ExprId {
        self.exprs.push(expr);
        self.source_ranges.push(range)
    }

    fn decl(&mut self, decl: Decl, name: Sym, explicit_ty: Option<Type>) -> DeclId {
        let id1 = self.decls.push(decl);
        let id2 = self.explicit_tys.push(explicit_ty);
        let id3 = self.names.push(name);
        debug_assert_eq!(id1, id2);
        debug_assert_eq!(id2, id3);

        id1
    }

    fn global_decl(&mut self, decl: Decl, name: Sym, explicit_ty: Option<Type>) {
        let id = self.decl(decl, name, explicit_ty);
        self.global_decls.push(id);
    }
}

impl<'src> builder::Builder<'src> for Builder<'src> {
    type Output = Program;
    fn add_intrinsic(&mut self, intrinsic: Intrinsic, param_tys: SmallVec<[Type; 2]>, ret_ty: Type) {
        let name = self.interner.get_or_intern(intrinsic.name());
        self.global_decl(Decl::Intrinsic { intr: intrinsic, param_tys }, name, Some(ret_ty));
    }
    fn void_expr(&self) -> ExprId { self.void_expr }
    fn int_lit(&mut self, lit: u64, range: SourceRange) -> ExprId {
        self.push(Expr::IntLit { lit }, range)
    }
    fn dec_lit(&mut self, lit: f64, range: SourceRange) -> ExprId { 
        self.push(Expr::DecLit { lit }, range)
    }
    fn str_lit(&mut self, lit: CString, range: SourceRange) -> ExprId { 
        self.push(Expr::StrLit { lit }, range)
    }
    fn char_lit(&mut self, lit: i8, range: SourceRange) -> ExprId { 
        self.push(Expr::CharLit { lit }, range)
    }
    fn bin_op(&mut self, op: BinOp, lhs: ExprId, rhs: ExprId, range: SourceRange) -> ExprId {
        match op {
            BinOp::Assign => self.push(Expr::Set { lhs, rhs }, range),
            _ => self.decl_ref(op.symbol(), smallvec![lhs, rhs], range),
        }
    }
    fn cast(&mut self, expr: ExprId, ty: Type, range: SourceRange) -> ExprId {
        let cast_id = CastId::new(self.num_casts);
        self.num_casts += 1;
        self.push(Expr::Cast { expr, ty, cast_id }, range)
    }
    fn un_op(&mut self, op: UnOp, expr: ExprId, range: SourceRange) -> ExprId {
        match op {
            UnOp::Deref => self.push(Expr::Deref(expr), range),
            UnOp::AddrOf | UnOp::AddrOfMut => self.push(Expr::AddrOf(expr), range),
            _ => self.decl_ref(op.symbol(), smallvec![expr], range),
        }
    }
    fn stored_decl(&mut self, name: &'src str, explicit_ty: Option<Type>, is_mut: bool, root_expr: ExprId, _range: SourceRange) {
        self.flush_stmt_buffer();
        let name = self.interner.get_or_intern(name);
        if self.comp_decl_stack.is_empty() {
            self.global_decl(
                if is_mut {
                    Decl::Static(root_expr)
                } else {
                    Decl::Const(root_expr)
                },
                name,
                explicit_ty,
            );
        } else {
            let decl = self.comp_decl_stack.last_mut().unwrap();
            let id = StoredDeclId::new(decl.num_stored_decls);
            decl.num_stored_decls += 1;

            let decl_id = self.decl(Decl::Stored { id, is_mut }, name, explicit_ty);
            self.item(Item::StoredDecl { decl_id, id, root_expr });
        }
    }
    fn ret(&mut self, expr: ExprId, range: SourceRange) -> ExprId {
        self.push(Expr::Ret { expr }, range)
    }
    fn implicit_ret(&mut self, _expr: ExprId) {}
    fn if_expr(&mut self, condition: ExprId, then_scope: ScopeId, else_scope: Option<ScopeId>, range: SourceRange) -> ExprId {
        self.push(
            Expr::If { condition, then_scope, else_scope },
            range,
        )
    }
    fn while_expr(&mut self, condition: ExprId, scope: ScopeId, range: SourceRange) -> ExprId {
        self.push(Expr::While { condition, scope }, range)
    }
    fn stmt(&mut self, expr: ExprId) {
        self.flush_stmt_buffer();
        let scope = self.comp_decl_stack.last_mut().unwrap().scope_stack.last_mut().unwrap();
        scope.stmt_buffer = Some(expr);
    }
    fn do_expr(&mut self, scope: ScopeId, range: SourceRange) -> ExprId {
        self.push(Expr::Do { scope }, range)
    }
    fn begin_scope(&mut self) -> ScopeId { 
        let id = self.scopes.push(
            Scope {
                items: Vec::new(),
                terminal_expr: self.void_expr(),
            }
        );
        let comp_decl = self.comp_decl_stack.last_mut().unwrap();
        assert!(!comp_decl.scope_stack.is_empty() || comp_decl.has_scope.is_none(), "Can't add multiple top-level scopes to a computed decl");
        if comp_decl.scope_stack.is_empty() {
            comp_decl.has_scope = Some(id);
        }

        comp_decl.scope_stack.push(
            ScopeState {
                id,
                stmt_buffer: None,
            }
        );

        id
    }
    fn end_scope(&mut self, has_terminal_expr: bool) {
        let scope = self.comp_decl_stack.last_mut().unwrap().scope_stack.pop().unwrap();
        if has_terminal_expr {
            let terminal_expr = scope.stmt_buffer.expect("must pass terminal expression via Builder::stmt()");
            self.scopes[scope.id].terminal_expr = terminal_expr;
        }
    }
    fn begin_computed_decl(&mut self, name: &'src str, param_names: SmallVec<[&'src str; 2]>, param_tys: SmallVec<[Type; 2]>, explicit_ty: Option<Type>, _proto_range: SourceRange) {
        let name = self.interner.get_or_intern(name);
        // This is a placeholder value that gets replaced once the parameter declarations get allocated.
        let id = self.decl(Decl::Const(ExprId::new(std::usize::MAX)), name, explicit_ty);
        assert_eq!(param_names.len(), param_tys.len());
        self.decls.reserve(param_tys.len());
        let params = param_tys.iter()
            .enumerate()
            .zip(&param_names)
            .map(|((index, ty), &name)| {
                let name = self.interner.get_or_intern(name);
                self.decl(Decl::Parameter { index }, name, Some(ty.clone()))
            })
            .collect();
        // `end_computed_decl` will attach the real scope to this decl; we don't have it yet
        self.decls[id] = Decl::Computed {
            param_tys,
            params,
            scope: ScopeId::new(std::usize::MAX)
        };
        if self.comp_decl_stack.is_empty() {
            self.global_decls.push(id);
        } else {
            self.flush_stmt_buffer();
            self.item(Item::ComputedDecl(id));
        }
        self.comp_decl_stack.push(
            CompDeclState {
                has_scope: None,
                id,
                scope_stack: Vec::new(),
                num_stored_decls: 0,
            }
        );
    }
    fn end_computed_decl(&mut self) {
        let decl_state = self.comp_decl_stack.pop().unwrap();
        if let Decl::Computed { ref mut scope, .. } = self.decls[decl_state.id] {
            *scope = decl_state.has_scope.unwrap();
        } else {
            panic!("Unexpected decl kind when ending computed decl!");
        }
    }
    fn decl_ref(&mut self, name: &'src str, arguments: SmallVec<[ExprId; 2]>, range: SourceRange) -> ExprId {
        let decl_ref_id = self.allocate_decl_ref_id();
        let name = self.interner.get_or_intern(name);
        self.push(Expr::DeclRef { name, arguments, id: decl_ref_id }, range)
    }
    fn get_range(&self, id: ExprId) -> SourceRange { self.source_ranges[id].clone() }
    fn set_range(&mut self, id: ExprId, range: SourceRange) { self.source_ranges[id] = range; }
    fn get_terminal_expr(&self, scope: ScopeId) -> ExprId { 
        self.scopes[scope].terminal_expr
    }
    fn output(self) -> Program {
        Program {
            exprs: self.exprs,
            num_decl_refs: self.num_decl_refs,
            decls: self.decls,
            names: self.names,
            explicit_tys: self.explicit_tys,
            global_decls: self.global_decls,
            source_ranges: self.source_ranges,
            scopes: self.scopes,
            void_expr: self.void_expr,
            interner: Rc::new(self.interner),
        }
    }
}
