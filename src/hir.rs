use std::ffi::CString;
use std::marker::PhantomData;

use smallvec::{SmallVec, smallvec};

use crate::index_vec::{Idx, IdxVec};
use crate::builder::{self, BinOp, UnOp, ExprId, DeclId, ScopeId, DeclRefId, CastId, Intrinsic};
use crate::source_info::SourceRange;
use crate::ty::Type;

#[derive(Debug)]
pub enum Expr {
    Void,
    IntLit { lit: u64 },
    DecLit { lit: f64 },
    StrLit { lit: CString },
    CharLit { lit: i8 },
    DeclRef { arguments: SmallVec<[ExprId; 2]>, id: DeclRefId },
    LogicalOr { lhs: ExprId, rhs: ExprId },
    LogicalAnd { lhs: ExprId, rhs: ExprId },
    LogicalNot(ExprId),
    AddrOf(ExprId),
    Deref(ExprId),
    Set { lhs: ExprId, rhs: ExprId },
    Do { scope: ScopeId },
    If { condition: ExprId, then_scope: ScopeId, else_scope: Option<ScopeId> },
    While { condition: ExprId, scope: ScopeId },
    Cast { expr: ExprId, ty: Type, cast_id: CastId },
    Ret { expr: ExprId }
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
}

#[derive(Copy, Clone, Debug)]
pub enum Item {
    Stmt(ExprId),
    StoredDecl { id: DeclId, root_expr: ExprId },
}

#[derive(Debug)]
pub struct Scope {
    pub items: Vec<Item>,
    pub terminal_expr: ExprId,
}

#[derive(Debug)]
pub enum Decl {
    Computed { name: String, params: SmallVec<[DeclId; 2]>, scope: ScopeId },
    Stored,
    Parameter,
    Intrinsic(Intrinsic),
    Static(ExprId),
    Const(ExprId),
}

#[derive(Debug)]
pub struct Program {
    pub exprs: IdxVec<Expr, ExprId>,
    pub num_decl_refs: usize,
    pub decls: IdxVec<Decl, DeclId>,
    pub scopes: IdxVec<Scope, ScopeId>,
    pub void_expr: ExprId,
}

#[derive(Debug)]
pub struct Builder<'src> {
    exprs: IdxVec<Expr, ExprId>,
    num_decl_refs: usize,
    num_casts: usize,

    decls: IdxVec<Decl, DeclId>,
    scopes: IdxVec<Scope, ScopeId>,
    comp_decl_stack: Vec<CompDeclState>,
    void_expr: ExprId,
    
    _phantom_src_lifetime: PhantomData<&'src str>,
}

impl<'src> Builder<'src> {
    pub fn new() -> Self {
        let mut exprs = IdxVec::new();
        let void_expr = exprs.push(Expr::Void);
        Self {
            exprs,
            num_decl_refs: 0,
            num_casts: 0,
            decls: IdxVec::new(),
            scopes: IdxVec::new(),
            comp_decl_stack: Vec::new(),
            void_expr,
            _phantom_src_lifetime: PhantomData,
        }
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

    fn decl_ref_no_name(&mut self, arguments: SmallVec<[ExprId; 2]>, _range: SourceRange) -> ExprId {
        let decl_ref_id = self.allocate_decl_ref_id();
        self.exprs.push(Expr::DeclRef { arguments, id: decl_ref_id })
    }

    /// Allocates a new DeclRefId, then pushes `expr`.
    /// Some operators are represented as decl refs in TIR but are built-in in HIR. So this is a
    /// hack to preserve synchronization between the two representations.
    fn push_op_expr(&mut self, expr: Expr) -> ExprId {
        self.allocate_decl_ref_id();
        self.exprs.push(expr)
    }
}

impl<'src> builder::Builder<'src> for Builder<'src> {
    type Output = Program;
    fn add_intrinsic(&mut self, intrinsic: Intrinsic, _param_tys: SmallVec<[Type; 2]>, _ret_ty: Type) {
        self.decls.push(Decl::Intrinsic(intrinsic));
    }
    fn void_expr(&self) -> ExprId { self.void_expr }
    fn int_lit(&mut self, lit: u64, _range: SourceRange) -> ExprId {
        self.exprs.push(Expr::IntLit { lit })
    }
    fn dec_lit(&mut self, lit: f64, _range: SourceRange) -> ExprId { 
        self.exprs.push(Expr::DecLit { lit })
    }
    fn str_lit(&mut self, lit: CString, _range: SourceRange) -> ExprId { 
        self.exprs.push(Expr::StrLit { lit })
    }
    fn char_lit(&mut self, lit: i8, _range: SourceRange) -> ExprId { 
        self.exprs.push(Expr::CharLit { lit })
    }
    fn bin_op(&mut self, op: BinOp, lhs: ExprId, rhs: ExprId, range: SourceRange) -> ExprId {
        match op {
            BinOp::Assign => self.exprs.push(Expr::Set { lhs, rhs }),
            BinOp::LogicalAnd => self.push_op_expr(Expr::LogicalAnd { lhs, rhs }),
            BinOp::LogicalOr => self.push_op_expr(Expr::LogicalOr { lhs, rhs }),
            _ => self.decl_ref_no_name(smallvec![lhs, rhs], range),
        }
    }
    fn cast(&mut self, expr: ExprId, ty: Type, _range: SourceRange) -> ExprId {
        let cast_id = CastId::new(self.num_casts);
        self.num_casts += 1;
        self.exprs.push(Expr::Cast { expr, ty, cast_id })
    }
    fn un_op(&mut self, op: UnOp, expr: ExprId, range: SourceRange) -> ExprId {
        match op {
            UnOp::Plus => {
                self.allocate_decl_ref_id();
                // Unary plus is a no-op
                expr
            },
            UnOp::Deref => self.exprs.push(Expr::Deref(expr)),
            UnOp::AddrOf | UnOp::AddrOfMut => self.exprs.push(Expr::AddrOf(expr)),
            UnOp::Not => self.push_op_expr(Expr::LogicalNot(expr)),
            _ => self.decl_ref_no_name(smallvec![expr], range),
        }
    }
    fn stored_decl(&mut self, _name: &'src str, _explicit_ty: Option<Type>, is_mut: bool, root_expr: ExprId, _range: SourceRange) {
        self.flush_stmt_buffer();
        if self.comp_decl_stack.is_empty() {
            self.decls.push(
                if is_mut {
                    Decl::Static(root_expr)
                } else {
                    Decl::Const(root_expr)
                }
            );
        } else {
            let id = self.decls.push(Decl::Stored);
            self.item(Item::StoredDecl { id, root_expr });
        }
    }
    fn ret(&mut self, expr: ExprId, _range: SourceRange) -> ExprId {
        self.exprs.push(Expr::Ret { expr })
    }
    fn implicit_ret(&mut self, _expr: ExprId) {}
    fn if_expr(&mut self, condition: ExprId, then_scope: ScopeId, else_scope: Option<ScopeId>, _range: SourceRange) -> ExprId {
        self.exprs.push(
            Expr::If { condition, then_scope, else_scope }
        )
    }
    fn while_expr(&mut self, condition: ExprId, scope: ScopeId, _range: SourceRange) -> ExprId {
        self.exprs.push(Expr::While { condition, scope })
    }
    fn stmt(&mut self, expr: ExprId) {
        self.flush_stmt_buffer();
        let scope = self.comp_decl_stack.last_mut().unwrap().scope_stack.last_mut().unwrap();
        scope.stmt_buffer = Some(expr);
    }
    fn do_expr(&mut self, scope: ScopeId) -> ExprId {
        self.exprs.push(Expr::Do { scope })
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
    fn begin_computed_decl(&mut self, name: &'src str, param_names: SmallVec<[&'src str; 2]>, param_tys: SmallVec<[Type; 2]>, _ret_ty: Option<Type>, _proto_range: SourceRange) {
        self.flush_stmt_buffer();
        // This is a placeholder value that gets replaced once the parameter declarations are allocated.
        let id = self.decls.push(Decl::Stored);
        assert_eq!(param_names.len(), param_tys.len());
        self.decls.reserve(param_tys.len());
        let params = param_tys.iter()
            .map(|_| self.decls.push(Decl::Parameter))
            .collect();
        // `end_computed_decl` will attach the real scope to this decl; we don't have it yet
        self.decls[id] = Decl::Computed {
            name: name.to_owned(),
            params,
            scope: ScopeId::new(std::usize::MAX)
        };
        self.comp_decl_stack.push(
            CompDeclState {
                has_scope: None,
                id,
                scope_stack: Vec::new(),
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
    fn decl_ref(&mut self, _name: &'src str, arguments: SmallVec<[ExprId; 2]>, range: SourceRange) -> ExprId {
        self.decl_ref_no_name(arguments, range)
    }
    // TODO: Refactor so this method doesn't need to be exposed by HIR
    fn get_range(&self, _id: ExprId) -> SourceRange { 0..0 }
    fn get_terminal_expr(&self, scope: ScopeId) -> ExprId { 
        self.scopes[scope].terminal_expr
    }
    fn output(self) -> Program {
        Program {
            exprs: self.exprs,
            num_decl_refs: self.num_decl_refs,
            decls: self.decls,
            scopes: self.scopes,
            void_expr: self.void_expr,
        }
    }
}
