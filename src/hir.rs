use string_interner::{DefaultStringInterner, Sym};
use smallvec::{SmallVec, smallvec};

use crate::index_vec::{Idx, IdxVec};
use crate::builder::{self, BinOp, ExprId, ScopeId, DeclRefId};
use crate::source_info::SourceRange;
use crate::ty::Type;

pub enum Expr {
    Void,
    IntLit { lit: u64 },
    DecLit { lit: f64 },
    DeclRef { arguments: SmallVec<[ExprId; 2]>, id: DeclRefId },
    If { condition: ExprId, then_scope: ScopeId, else_scope: Option<ScopeId> },
    Ret { expr: ExprId }
}

pub enum Instr {

}

pub struct Program {}

struct ScopeState {
    id: ScopeId,
    stmt_buffer: Option<ExprId>,
}

struct CompDeclState {
    scope_stack: Vec<ScopeState>,
}

pub struct Builder {
    exprs: IdxVec<Expr, ExprId>,
    num_decl_refs: usize,
    terminal_exprs: IdxVec<ExprId, ScopeId>,
    comp_decl_stack: Vec<CompDeclState>,
    void_expr: ExprId,
    interner: DefaultStringInterner,
}

impl builder::Builder for Builder {
    type Output = Program;
    fn new(interner: DefaultStringInterner) -> Self {
        let mut exprs = IdxVec::new();
        let void_expr = exprs.push(Expr::Void);
        Self {
            exprs: IdxVec::new(),
            num_decl_refs: 0,
            terminal_exprs: IdxVec::new(),
            comp_decl_stack: Vec::new(),
            void_expr,
            interner,
        }
    }
    fn interner(&self) -> &DefaultStringInterner { &self.interner }
    fn void_expr(&self) -> ExprId { self.void_expr }
    fn int_lit(&mut self, lit: u64, range: SourceRange) -> ExprId {
        self.exprs.push(Expr::IntLit { lit })
    }
    fn dec_lit(&mut self, lit: f64, range: SourceRange) -> ExprId { 
        self.exprs.push(Expr::DecLit { lit })
    }
    fn bin_op(&mut self, op: BinOp, lhs: ExprId, rhs: ExprId, range: SourceRange) -> ExprId {
        let name = self.interner.get_or_intern(op.symbol());
        self.decl_ref(name, smallvec![lhs, rhs], range)
    }
    fn stored_decl(&mut self, name: Sym, root_expr: ExprId, range: SourceRange) {}
    fn ret(&mut self, expr: ExprId, range: SourceRange) -> ExprId {
        self.exprs.push(Expr::Ret { expr })
    }
    fn if_expr(&mut self, condition: ExprId, then_scope: ScopeId, else_scope: Option<ScopeId>, range: SourceRange) -> ExprId {
        self.exprs.push(
            Expr::If { condition, then_scope, else_scope }
        )
    }
    fn stmt(&mut self, expr: ExprId) {
        let scope = self.comp_decl_stack.last_mut().unwrap().scope_stack.last_mut().unwrap();
        if let Some(stmt) = scope.stmt_buffer {
            // Do something with the statement
        }
        scope.stmt_buffer = Some(expr);
    }
    fn begin_scope(&mut self) -> ScopeId { 
        let id = self.terminal_exprs.push(self.void_expr());
        self.comp_decl_stack.last_mut().unwrap().scope_stack.push(
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
            self.terminal_exprs[scope.id] = terminal_expr;
        }
    }
    fn begin_computed_decl(&mut self, name: Sym, param_names: SmallVec<[Sym; 2]>, param_tys: SmallVec<[Type; 2]>, ret_ty: Type, proto_range: SourceRange) {
        self.comp_decl_stack.push(
            CompDeclState {
                scope_stack: Vec::new(),
            }
        );
    }
    fn end_computed_decl(&mut self) {
        self.comp_decl_stack.pop();
    }
    fn decl_ref(&mut self, name: Sym, arguments: SmallVec<[ExprId; 2]>, range: SourceRange) -> ExprId {
        let decl_ref_id = DeclRefId::new(self.num_decl_refs);
        self.num_decl_refs += 1;
        self.exprs.push(Expr::DeclRef { arguments, id: decl_ref_id })
    }
    // TODO: Refactor so this method doesn't need to be exposed by HIR
    fn get_range(&self, id: ExprId) -> SourceRange { 0..0 }
    fn get_terminal_expr(&self, scope: ScopeId) -> ExprId { 
        self.terminal_exprs[scope]
    }
    fn output(self) -> Program {
        Program { }
    }
}
