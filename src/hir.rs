use string_interner::{DefaultStringInterner, Sym};

use crate::index_vec::IdxVec;
use crate::builder::{self, BinOp, ExprId, ScopeId, DeclRefId};
use crate::source_info::SourceRange;
use crate::ty::Type;

pub enum Expr {
    Void,
    IntLit { lit: u64 },
    DecLit { lit: f64 },
    DeclRef { args: Vec<ExprId>, id: DeclRefId },
    If { condition: ExprId, then_scope: ScopeId, else_scope: ScopeId }
}

pub enum Instr {

}

pub struct Program {}

pub struct Builder {
    exprs: IdxVec<Expr, ExprId>,
    interner: DefaultStringInterner,

}

impl builder::Builder for Builder {
    type Output = Program;
    fn new(interner: DefaultStringInterner) -> Self {
        Self {
            exprs: IdxVec::new(),
            interner,
        }
    }
    fn interner(&self) -> &DefaultStringInterner { &self.interner }
    fn void_expr(&self) -> ExprId { panic!() }
    fn int_lit(&mut self, lit: u64, range: SourceRange) -> ExprId { panic!() }
    fn dec_lit(&mut self, lit: f64, range: SourceRange) -> ExprId { panic!() }
    fn bin_op(&mut self, op: BinOp, lhs: ExprId, rhs: ExprId, range: SourceRange) -> ExprId { panic!() }
    fn stored_decl(&mut self, name: Sym, root_expr: ExprId, range: SourceRange) -> ExprId { panic!() }
    fn ret(&mut self, expr: ExprId, range: SourceRange) -> ExprId { panic!() }
    fn if_expr(&mut self, condition: ExprId, then_scope: ScopeId, else_scope: Option<ScopeId>, range: SourceRange) -> ExprId { panic!() }
    fn begin_scope(&mut self) -> ScopeId { panic!() }
    fn end_scope(&mut self, stmts: &[ExprId], terminal_expr: ExprId) { panic!() }
    fn begin_computed_decl(&mut self, name: Sym, param_names: Vec<Sym>, param_tys: Vec<Type>, ret_ty: Type, proto_range: SourceRange) { panic!() }
    fn end_computed_decl(&mut self) { panic!() }
    fn decl_ref(&mut self, name: Sym, arguments: Vec<ExprId>, range: SourceRange) -> ExprId { panic!() }
    fn get_range(&self, id: ExprId) -> SourceRange { panic!() }
    fn terminal_expr(&self, scope: ScopeId) -> ExprId { panic!() }
    fn output(self) -> Program {
        Program { }
    }
}
