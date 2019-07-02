use string_interner::{DefaultStringInterner, Sym};
use smallvec::{SmallVec, smallvec};

use crate::index_vec::{Idx, IdxVec};
use crate::builder::{self, BinOp, UnOp, ExprId, DeclId, ScopeId, LocalDeclId, DeclRefId};
use crate::source_info::SourceRange;
use crate::ty::Type;

newtype_index!(InstrId pub);
newtype_index!(BasicBlockId pub);
newtype_index!(TerminationId pub);

#[derive(Debug)]
pub enum Expr {
    Void,
    IntLit { lit: u64 },
    DecLit { lit: f64 },
    DeclRef { arguments: SmallVec<[ExprId; 2]>, id: DeclRefId },
    LogicalOr { lhs: ExprId, rhs: ExprId },
    LogicalAnd { lhs: ExprId, rhs: ExprId },
    Set { lhs: ExprId, rhs: ExprId },
    Do { scope: ScopeId },
    If { condition: ExprId, then_scope: ScopeId, else_scope: Option<ScopeId> },
    Ret { expr: ExprId }
}

#[derive(Debug)]
pub enum Instr {
    Void,
    IntConst { lit: u64, expr: ExprId },
    FloatConst { lit: f64, expr: ExprId },
    BoolConst(bool),
    Alloca { expr: ExprId },
    Get { arguments: SmallVec<[InstrId; 2]>, id: DeclRefId },
    Set { arguments: SmallVec<[InstrId; 2]>, id: DeclRefId, value: InstrId, expr: ExprId },
    Load { location: InstrId, expr: ExprId },
    Store { location: InstrId, value: InstrId, expr: ExprId },
    Ret { value: InstrId, expr: ExprId },
    Br(BasicBlockId),
    CondBr { condition: InstrId, true_bb: BasicBlockId, false_bb: BasicBlockId },
}

#[derive(Debug)]
pub struct Program {
    comp_decls: Vec<CompDecl>,
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

// TODO: store IdxVec of these
#[derive(Debug)]
enum LocalDecl {
    Stored { location: InstrId },
    Computed
}

#[derive(Copy, Clone, Debug)]
enum Item {
    Stmt(ExprId),
    StoredDecl(ExprId),
}

#[derive(Debug)]
struct Scope {
    items: Vec<Item>,
    terminal_expr: ExprId,
}

#[derive(Debug)]
struct CompDecl {
    code: IdxVec<Instr, InstrId>,
    basic_blocks: IdxVec<InstrId, BasicBlockId>,
}

#[derive(Debug)]
pub struct Builder<'a> {
    exprs: IdxVec<Expr, ExprId>,
    num_decl_refs: usize,
    num_local_decls: usize,
    comp_decls: Vec<CompDecl>,
    scopes: IdxVec<Scope, ScopeId>,
    comp_decl_stack: Vec<CompDeclState>,
    void_expr: ExprId,
    interner: &'a mut DefaultStringInterner,
}

/// What to do with a value
#[derive(Clone)]
enum DataDest {
    /// This value needs to be returned from the current function
    Ret,
    /// A particular value needs to be assigned to this value
    Receive { value: InstrId, expr: ExprId },
    /// This value needs to be assigned to a particular expression
    Set { dest: ExprId },
    /// This value needs to be written to a particular memory location
    Store { location: InstrId },
    /// This value just needs to be read
    Read,
    /// This value is a statement and therefore will never be used
    Stmt,
    /// If this value is true, branch to the first basic block, otherwise branch to the second
    Branch(BasicBlockId, BasicBlockId),
}

/// Where to go after the current value is computed (whether implicitly or explicitly, such as via a `break` in a loop)
#[derive(Clone)]
enum ControlDest {
    Continue,
    Unreachable,
    Block(BasicBlockId),
}

#[derive(Clone)]
struct Context {
    data: DataDest,
    control: ControlDest,
}

impl Context {
    fn new(data: DataDest, control: ControlDest) -> Context {
        Context { data, control }
    }

    fn redirect(&self, read: Option<InstrId>, kontinue: Option<BasicBlockId>) -> Context {
        Context::new(
            match (&self.data, read) {
                (DataDest::Read, Some(location)) => DataDest::Store { location },
                (x, _) => x.clone(),
            },
            match(&self.control, kontinue) {
                (ControlDest::Continue, Some(block)) => ControlDest::Block(block),
                (x, _) => x.clone(),
            }
        )
    }
}

struct CompDeclBuilder<'a> {
    builder: &'a Builder<'a>,
    void_instr: InstrId,
    code: IdxVec<Instr, InstrId>,
    basic_blocks: IdxVec<InstrId, BasicBlockId>,
}

impl<'a> CompDeclBuilder<'a> {
    fn new(builder: &'a Builder) -> Self {
        let mut code = IdxVec::new();
        let void_instr = code.push(Instr::Void);
        let mut basic_blocks = IdxVec::new();
        basic_blocks.push(InstrId::new(0));
        CompDeclBuilder::<'a> {
            builder,
            void_instr,
            code,
            basic_blocks,
        }
    }

    fn item(&mut self, item: Item) {
        match item {
            Item::Stmt(expr) => {
                self.expr(expr, Context::new(DataDest::Stmt, ControlDest::Continue));
            },
            Item::StoredDecl(expr) => {
                let location = self.code.push(Instr::Alloca { expr });
                self.expr(expr, Context::new(DataDest::Store { location }, ControlDest::Continue));
            },
        }
    }

    fn void_instr(&self) -> InstrId { self.void_instr }

    fn scope(&mut self, scope: ScopeId, ctx: Context) -> InstrId {
        let scope = &self.builder.scopes[scope];
        for &item in &scope.items {
            self.item(item);
        }
        self.expr(scope.terminal_expr, ctx)
    }

    fn expr(&mut self, expr: ExprId, ctx: Context) -> InstrId {
        // HACK!!!!
        let mut should_allow_set = false;

        let instr = match self.builder.exprs[expr] {
            Expr::Void => self.void_instr(),
            Expr::IntLit { lit } => self.code.push(Instr::IntConst { lit, expr }),
            Expr::DecLit { lit } => self.code.push(Instr::FloatConst { lit, expr }),
            Expr::Set { lhs, rhs } => {
                return self.expr(
                    rhs,
                    Context::new(DataDest::Set { dest: lhs }, ctx.control.clone()),
                )
            },
            Expr::DeclRef { ref arguments, id } => {
                should_allow_set = true;
                let mut instr_args = SmallVec::new();
                instr_args.reserve(arguments.len());
                for &argument in arguments {
                    instr_args.push(
                        self.expr(argument, Context::new(DataDest::Read, ControlDest::Continue)),
                    );
                }
                self.code.push(
                    match ctx.data {
                        DataDest::Receive { value, expr } => 
                            Instr::Set { arguments: instr_args, id, value, expr },
                        _ => Instr::Get { arguments: instr_args, id },
                    }
                )
            },
            Expr::LogicalAnd { lhs, rhs } => {
                let left_true_bb = self.basic_blocks.push(InstrId::new(0));
                let location = if let DataDest::Read = ctx.data {
                    Some(self.code.push(Instr::Alloca { expr }))
                } else {
                    None
                };
                if let DataDest::Branch(true_bb, false_bb) = ctx.data {
                    self.expr(
                        lhs,
                        Context::new(DataDest::Branch(left_true_bb, false_bb), ControlDest::Continue),
                    );

                    self.basic_blocks[left_true_bb] = InstrId::new(self.code.len());
                    return self.expr(
                        rhs,
                        Context::new(DataDest::Branch(true_bb, false_bb), ControlDest::Continue),
                    );
                } else {
                    let left_false_bb = self.basic_blocks.push(InstrId::new(0));
                    let after_bb = self.basic_blocks.push(InstrId::new(0));
                    self.expr(
                        lhs,
                        Context::new(DataDest::Branch(left_true_bb, left_false_bb), ControlDest::Continue),
                    );

                    self.basic_blocks[left_true_bb] = InstrId::new(self.code.len());
                    // No further branching required, because (true && foo) <=> foo
                    let branch_ctx = ctx.redirect(location, Some(after_bb));
                    self.expr(rhs, branch_ctx.clone());

                    self.basic_blocks[left_false_bb] = InstrId::new(self.code.len());
                    let false_val = self.code.push(Instr::BoolConst(false));
                    self.handle_context(false_val, expr, branch_ctx, false);

                    self.basic_blocks[after_bb] = InstrId::new(self.code.len());
                    if let Some(location) = location {
                        self.code.push(Instr::Load { location, expr })
                    } else {
                        return self.void_instr()
                    }
                }
            },
            Expr::LogicalOr { lhs, rhs } => {
                self.void_instr()
            }
            Expr::Do { scope } => return self.scope(scope, ctx),
            Expr::If { condition, then_scope, else_scope } => {
                // At this point it's impossible to know where these basic blocks are supposed to begin, so make them 0 for now
                let true_bb = self.basic_blocks.push(InstrId::new(0));
                let false_bb = self.basic_blocks.push(InstrId::new(0));
                let post_bb = if else_scope.is_some() {
                    self.basic_blocks.push(InstrId::new(0))
                } else {
                    false_bb
                };

                let result_location = match (&ctx.data, else_scope) {
                    (DataDest::Read, Some(_)) => Some(
                        self.code.push(Instr::Alloca { expr })
                    ),
                    _ => None,
                };
                self.expr(
                    condition,
                    Context::new(DataDest::Branch(true_bb, false_bb), ControlDest::Continue),
                );
                self.basic_blocks[true_bb] = InstrId::new(self.code.len());
                let scope_ctx = ctx.redirect(result_location, Some(post_bb));
                self.scope(then_scope, scope_ctx.clone());
                if let Some(else_scope) = else_scope {
                    self.basic_blocks[false_bb] = InstrId::new(self.code.len());
                    self.scope(else_scope, scope_ctx);
                }

                self.basic_blocks[post_bb] = InstrId::new(self.code.len());
                if let Some(location) = result_location {
                    return self.code.push(Instr::Load { location, expr })
                } else {
                    self.void_instr()
                }
            },
            Expr::Ret { expr } => {
                return self.expr(
                    expr,
                    Context::new(DataDest::Ret, ctx.control),
                );
            }
        };
        
        self.handle_context(instr, expr, ctx, should_allow_set)
    }

    fn handle_context(&mut self, instr: InstrId, expr: ExprId, ctx: Context, should_allow_set: bool) -> InstrId {
        match ctx.data {
            DataDest::Read => return instr,
            DataDest::Ret => return self.code.push(Instr::Ret { value: instr, expr }),
            DataDest::Branch(true_bb, false_bb)
                => return self.code.push(Instr::CondBr { condition: instr, true_bb, false_bb }),
            DataDest::Receive { .. } => {
                assert!(should_allow_set, "can't set constant expression!");
            },
            DataDest::Store { location } => {
                self.code.push(Instr::Store { location, value: instr, expr });
            },
            DataDest::Set { dest } => {
                return self.expr(
                    dest,
                    Context::new(DataDest::Receive { value: instr, expr }, ctx.control.clone()),
                );
            }
            DataDest::Stmt => {},
        }

        match ctx.control {
            ControlDest::Block(block) => self.code.push(Instr::Br(block)),
            ControlDest::Continue => instr,
            ControlDest::Unreachable => self.void_instr(),
        }
    }

    fn build(mut self, scope: ScopeId) -> CompDecl {
        self.scope(scope, Context::new(DataDest::Ret, ControlDest::Unreachable));
        CompDecl {
            code: self.code,
            basic_blocks: self.basic_blocks,
        }
    }
}


impl<'a> Builder<'a> {
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
        CompDeclBuilder::new(&self).build(scope)
    }

    fn decl_ref_no_name(&mut self, arguments: SmallVec<[ExprId; 2]>, range: SourceRange) -> ExprId {
        let decl_ref_id = DeclRefId::new(self.num_decl_refs);
        self.num_decl_refs += 1;
        self.exprs.push(Expr::DeclRef { arguments, id: decl_ref_id })
    }
}

impl<'a> builder::Builder<'a> for Builder<'a> {
    type Output = Program;
    fn new(interner: &'a mut DefaultStringInterner) -> Self {
        let mut exprs = IdxVec::new();
        let void_expr = exprs.push(Expr::Void);
        Self {
            exprs,
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
        match op {
            BinOp::Assign => self.exprs.push(Expr::Set { lhs, rhs }),
            BinOp::LogicalAnd => self.exprs.push(Expr::LogicalAnd { lhs, rhs }),
            BinOp::LogicalOr => self.exprs.push(Expr::LogicalOr { lhs, rhs }),
            _ => self.decl_ref_no_name(smallvec![lhs, rhs], range),
        }
    }
    fn un_op(&mut self, op: UnOp, expr: ExprId, range: SourceRange) -> ExprId {
        match op {
            _ => self.decl_ref_no_name(smallvec![expr], range),
        }
    }
    fn stored_decl(&mut self, name: Sym, is_mut: bool, root_expr: ExprId, range: SourceRange) {
        self.flush_stmt_buffer();
        let id = LocalDeclId::new(self.num_local_decls);
        self.num_local_decls += 1;
        self.item(Item::StoredDecl(root_expr));
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
        self.decl_ref_no_name(arguments, range)
    }
    // TODO: Refactor so this method doesn't need to be exposed by HIR
    fn get_range(&self, id: ExprId) -> SourceRange { 0..0 }
    fn get_terminal_expr(&self, scope: ScopeId) -> ExprId { 
        self.scopes[scope].terminal_expr
    }
    fn output(self) -> Program {
        Program {
            comp_decls: self.comp_decls,
        }
    }
}
