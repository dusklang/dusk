use string_interner::{DefaultStringInterner, Sym};
use smallvec::{SmallVec, smallvec};

use crate::index_vec::{Idx, IdxVec};
use crate::builder::{self, BinOp, ExprId, DeclId, ScopeId, LocalDeclId, DeclRefId};
use crate::source_info::SourceRange;
use crate::ty::Type;

newtype_index!(InstrId);
newtype_index!(BasicBlockId);
newtype_index!(TerminationId);

pub enum Expr {
    Void,
    IntLit { lit: u64 },
    DecLit { lit: f64 },
    DeclRef { arguments: SmallVec<[ExprId; 2]>, id: DeclRefId },
    If { condition: ExprId, then_scope: ScopeId, else_scope: Option<ScopeId> },
    Ret { expr: ExprId }
}

/// An action to perform with a value
pub enum ValueAction {
    Set { arguments: SmallVec<[InstrId; 2]>, id: DeclRefId },
    Ret,
}

pub enum Instr {
    IntConst { lit: u64, expr: ExprId },
    FloatConst { lit: f64, expr: ExprId },
    Alloca { root_value: InstrId, decl: DeclId },
    Get { arguments: SmallVec<[InstrId; 2]>, id: DeclRefId },
    ValueAction { action: ValueAction, value: InstrId, expr: ExprId },
    Terminate { action: TerminationId, value: InstrId, expr: ExprId },
    CondBr { condition: InstrId, true_bb: BasicBlockId, false_bb: BasicBlockId },
}

pub struct Program {}

struct ScopeState {
    id: ScopeId,
    stmt_buffer: Option<ExprId>,
}

struct CompDeclState {
    has_scope: Option<ScopeId>,
    id: DeclId,
    scope_stack: Vec<ScopeState>,
}

enum Item {
    Stmt(ExprId),
    // TODO: make do expressions real, actual expressions instead of just passthrough for the terminal expression contained within their scope
    HACK_AbsorbItemsFromDoScope(ScopeId),
    StoredDecl(DeclId, ExprId),
}

struct Scope {
    items: Vec<Item>,
    terminal_expr: ExprId,
}

struct CompDecl {
    code: IdxVec<Instr, InstrId>,
    basic_blocks: IdxVec<InstrId, BasicBlockId>,
}

pub struct Builder {
    exprs: IdxVec<Expr, ExprId>,
    num_decl_refs: usize,
    num_local_decls: usize,
    comp_decls: Vec<CompDecl>,
    scopes: IdxVec<Scope, ScopeId>,
    comp_decl_stack: Vec<CompDeclState>,
    void_expr: ExprId,
    interner: DefaultStringInterner,
}

impl Builder {
    fn flush_stmt_buffer(&mut self) {
        let scope_state = self.comp_decl_stack.last_mut().unwrap().scope_stack.last_mut().unwrap();
        if let Some(stmt) = scope_state.stmt_buffer {
            self.scopes[scope_state.id].items.push(Item::Stmt(stmt));
            scope_state.stmt_buffer = None;
        }
    }
    fn item(&mut self, item: Item) {
        let scope = self.comp_decl_stack.last().unwrap().scope_stack.last().unwrap().id;
        self.scopes[scope].items.push(item);
    }

    fn gen_comp_decl(&self, scope: ScopeId) -> CompDecl {
        let mut func = CompDecl {
            code: IdxVec::new(),
            basic_blocks: IdxVec::new(),
        };



        func
    }
}

impl builder::Builder for Builder {
    type Output = Program;
    fn new(interner: DefaultStringInterner) -> Self {
        let mut exprs = IdxVec::new();
        let void_expr = exprs.push(Expr::Void);
        Self {
            exprs: IdxVec::new(),
            num_decl_refs: 0,
            num_local_decls: 0,
            comp_decls: Vec::new(),
            scopes: IdxVec::new(),
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
    fn stored_decl(&mut self, name: Sym, root_expr: ExprId, range: SourceRange) {
        self.flush_stmt_buffer();
        let id = LocalDeclId::new(self.num_local_decls);
        self.num_local_decls += 1;
        self.item(Item::StoredDecl(DeclId::Local(id), root_expr));
    }
    fn ret(&mut self, expr: ExprId, range: SourceRange) -> ExprId {
        self.exprs.push(Expr::Ret { expr })
    }
    fn if_expr(&mut self, condition: ExprId, then_scope: ScopeId, else_scope: Option<ScopeId>, range: SourceRange) -> ExprId {
        self.exprs.push(
            Expr::If { condition, then_scope, else_scope }
        )
    }
    fn stmt(&mut self, expr: ExprId) {
        self.flush_stmt_buffer();
        let scope = self.comp_decl_stack.last_mut().unwrap().scope_stack.last_mut().unwrap();
        scope.stmt_buffer = Some(expr);
    }
    fn do_expr(&mut self, scope: ScopeId) {
        self.flush_stmt_buffer();
        self.item(Item::HACK_AbsorbItemsFromDoScope(scope));
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
        comp_decl.has_scope = Some(id);

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
    fn begin_computed_decl(&mut self, name: Sym, param_names: SmallVec<[Sym; 2]>, param_tys: SmallVec<[Type; 2]>, ret_ty: Type, proto_range: SourceRange) {
        let id = LocalDeclId::new(self.num_local_decls);
        self.num_local_decls += 1;
        let id = DeclId::Local(id);

        self.num_local_decls += param_names.len();
        self.comp_decl_stack.push(
            CompDeclState {
                has_scope: None,
                id,
                scope_stack: Vec::new(),
            }
        );
    }
    fn end_computed_decl(&mut self) {
        let scope = self.comp_decl_stack.pop().unwrap().has_scope.unwrap();
        self.comp_decls.push(self.gen_comp_decl(scope));
    }
    fn decl_ref(&mut self, name: Sym, arguments: SmallVec<[ExprId; 2]>, range: SourceRange) -> ExprId {
        let decl_ref_id = DeclRefId::new(self.num_decl_refs);
        self.num_decl_refs += 1;
        self.exprs.push(Expr::DeclRef { arguments, id: decl_ref_id })
    }
    // TODO: Refactor so this method doesn't need to be exposed by HIR
    fn get_range(&self, id: ExprId) -> SourceRange { 0..0 }
    fn get_terminal_expr(&self, scope: ScopeId) -> ExprId { 
        self.scopes[scope].terminal_expr
    }
    fn output(self) -> Program {
        Program { }
    }
}
