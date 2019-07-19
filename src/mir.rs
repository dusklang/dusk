use smallvec::SmallVec;

use crate::index_vec::{Idx, IdxVec};
use crate::builder::{ExprId, DeclRefId, ScopeId};
use crate::hir::{self, Expr, Item, Decl};

newtype_index!(InstrId pub);
newtype_index!(BasicBlockId pub);
newtype_index!(TerminationId pub);

#[derive(Debug)]
pub enum Instr {
    Void,
    IntConst { lit: u64, expr: ExprId },
    FloatConst { lit: f64, expr: ExprId },
    BoolConst(bool),
    Alloca { expr: ExprId },
    LogicalNot(InstrId),
    Get { arguments: SmallVec<[InstrId; 2]>, id: DeclRefId },
    Set { arguments: SmallVec<[InstrId; 2]>, id: DeclRefId, value: InstrId, expr: ExprId },
    Modify { arguments: SmallVec<[InstrId; 2]>, id: DeclRefId },
    Load { location: InstrId, expr: ExprId },
    Store { location: InstrId, value: InstrId, expr: ExprId },
    Ret { value: InstrId, expr: ExprId },
    Br(BasicBlockId),
    CondBr { condition: InstrId, true_bb: BasicBlockId, false_bb: BasicBlockId },
}

// TODO: store IdxVec of these
#[derive(Debug)]
enum LocalDecl {
    Stored { location: InstrId },
    Computed
}

#[derive(Debug)]
struct Function {
    code: IdxVec<Instr, InstrId>,
    basic_blocks: IdxVec<InstrId, BasicBlockId>,
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
    /// The number of levels of indirection we are removed from the type of the expression
    /// Positive numbers indicate more layers of indirection, negative numbers indicate more
    /// dereferences
    indirection: i8,
    data: DataDest,
    control: ControlDest,
}

impl Context {
    fn new(indirection: i8, data: DataDest, control: ControlDest) -> Context {
        Context { indirection, data, control }
    }

    fn redirect(&self, read: Option<InstrId>, kontinue: Option<BasicBlockId>) -> Context {
        Context::new(
            self.indirection,
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

#[derive(Debug)]
pub struct Program {
    comp_decls: Vec<Function>,
}

impl Program {
    pub fn build(prog: &hir::Program) -> Self {
        Program {
            comp_decls: prog.local_decls.iter().chain(&prog.global_decls)
                .filter_map(|decl| if let &Decl::Computed(scope) = decl {
                    Some(FunctionBuilder::new(prog, scope).build())
                } else {
                    None
                }).collect()
        }
    }
}

struct FunctionBuilder<'a> {
    prog: &'a hir::Program,
    scope: ScopeId,
    void_instr: InstrId,
    code: IdxVec<Instr, InstrId>,
    basic_blocks: IdxVec<InstrId, BasicBlockId>,
}

impl<'a> FunctionBuilder<'a> {
    fn new(prog: &'a hir::Program, scope: ScopeId) -> Self {
        let mut code = IdxVec::new();
        let void_instr = code.push(Instr::Void);
        let mut basic_blocks = IdxVec::new();
        basic_blocks.push(InstrId::new(0));
        FunctionBuilder::<'a> {
            prog,
            scope,
            void_instr,
            code,
            basic_blocks,
        }
    }

    fn item(&mut self, item: Item) {
        match item {
            Item::Stmt(expr) => {
                self.expr(expr, Context::new(0, DataDest::Stmt, ControlDest::Continue));
            },
            Item::StoredDecl(expr) => {
                let location = self.code.push(Instr::Alloca { expr });
                self.expr(expr, Context::new(0, DataDest::Store { location }, ControlDest::Continue));
            },
        }
    }

    fn void_instr(&self) -> InstrId { self.void_instr }

    fn scope(&mut self, scope: ScopeId, ctx: Context) -> InstrId {
        let scope = &self.prog.scopes[scope];
        for &item in &scope.items {
            self.item(item);
        }
        self.expr(scope.terminal_expr, ctx)
    }

    fn new_bb(&mut self) -> BasicBlockId {
        self.basic_blocks.push(InstrId::new(0))
    }

    fn begin_bb(&mut self, bb: BasicBlockId) {
        self.basic_blocks[bb] = InstrId::new(self.code.len())
    }

    fn expr(&mut self, expr: ExprId, mut ctx: Context) -> InstrId {
        // HACK!!!!
        let mut should_allow_set = false;

        let instr = match self.prog.exprs[expr] {
            Expr::Void => self.void_instr(),
            Expr::IntLit { lit } => self.code.push(Instr::IntConst { lit, expr }),
            Expr::DecLit { lit } => self.code.push(Instr::FloatConst { lit, expr }),
            Expr::Set { lhs, rhs } => {
                self.expr(
                    rhs,
                    Context::new(0, DataDest::Set { dest: lhs }, ctx.control.clone()),
                );
                // Because we override the data destination above, we need to handle it ourselves
                return match ctx.data {
                    DataDest::Ret => self.code.push(Instr::Ret { value: self.void_instr(), expr }),
                    _ => self.void_instr(),
                };
            },
            Expr::DeclRef { ref arguments, id } => {
                should_allow_set = true;
                let arguments = arguments.iter().map(|&argument|
                    self.expr(argument, Context::new(0, DataDest::Read, ControlDest::Continue))
                ).collect();

                let instr = if ctx.indirection < 0 {
                    ctx.indirection += 1;
                    Instr::Modify { arguments, id }
                } else {
                    match ctx.data {
                        DataDest::Receive { value, expr } => if ctx.indirection > 0 {
                            let mut location = self.code.push(Instr::Get { arguments, id });
                            location = self.handle_indirection(location, expr, ctx.indirection, 1);
                            ctx.indirection = 0;
                            Instr::Store { location, value, expr }
                        } else {
                            Instr::Set { arguments, id, value, expr }
                        },
                        _ => Instr::Get { arguments, id },
                    }
                };
                self.code.push(instr)
            },
            Expr::LogicalAnd { lhs, rhs } => {
                assert_eq!(ctx.indirection, 0);
                let left_true_bb = self.new_bb();
                let location = if let DataDest::Read = ctx.data {
                    Some(self.code.push(Instr::Alloca { expr }))
                } else {
                    None
                };
                if let DataDest::Branch(true_bb, false_bb) = ctx.data {
                    self.expr(
                        lhs,
                        Context::new(0, DataDest::Branch(left_true_bb, false_bb), ControlDest::Continue),
                    );

                    self.begin_bb(left_true_bb);
                    return self.expr(
                        rhs,
                        Context::new(0, DataDest::Branch(true_bb, false_bb), ControlDest::Continue),
                    );
                } else {
                    let left_false_bb = self.new_bb();
                    let after_bb = self.new_bb();
                    self.expr(
                        lhs,
                        Context::new(0, DataDest::Branch(left_true_bb, left_false_bb), ControlDest::Continue),
                    );

                    self.begin_bb(left_true_bb);
                    // No further branching required, because (true && foo) <=> foo
                    let branch_ctx = ctx.redirect(location, Some(after_bb));
                    self.expr(rhs, branch_ctx.clone());

                    self.begin_bb(left_false_bb);
                    let false_val = self.code.push(Instr::BoolConst(false));
                    self.handle_context(false_val, expr, branch_ctx, false);

                    self.begin_bb(after_bb);
                    if let Some(location) = location {
                        self.code.push(Instr::Load { location, expr })
                    } else {
                        return self.void_instr()
                    }
                }
            },
            Expr::LogicalOr { lhs, rhs } => {
                assert_eq!(ctx.indirection, 0);
                let left_false_bb = self.new_bb();
                if let DataDest::Branch(true_bb, false_bb) = ctx.data {
                    self.expr(
                        lhs,
                        Context::new(0, DataDest::Branch(true_bb, left_false_bb), ControlDest::Continue),
                    );

                    self.begin_bb(left_false_bb);
                    return self.expr(
                        rhs,
                        Context::new(0, DataDest::Branch(true_bb, false_bb), ControlDest::Continue),
                    );
                } else {
                    let left_true_bb = self.new_bb();
                    let after_bb = self.new_bb();
                    let location = if let DataDest::Read = ctx.data {
                        Some(self.code.push(Instr::Alloca { expr }))
                    } else {
                        None
                    };
                    self.expr(
                        lhs,
                        Context::new(0, DataDest::Branch(left_true_bb, left_false_bb), ControlDest::Continue),
                    );

                    self.begin_bb(left_true_bb);
                    let true_val = self.code.push(Instr::BoolConst(true));
                    let branch_ctx = ctx.redirect(location, Some(after_bb));
                    self.handle_context(true_val, expr, branch_ctx.clone(), false);

                    self.begin_bb(left_false_bb);
                    self.expr(rhs, branch_ctx);

                    self.begin_bb(after_bb);
                    if let Some(location) = location {
                        self.code.push(Instr::Load { location, expr })
                    } else {
                        return self.void_instr()
                    }
                }
            },
            Expr::LogicalNot(operand) => {
                assert_eq!(ctx.indirection, 0);
                if let DataDest::Branch(true_bb, false_bb) = ctx.data {
                    return self.expr(operand, Context::new(0, DataDest::Branch(false_bb, true_bb), ctx.control))
                } else {
                    let operand = self.expr(operand, Context::new(0, DataDest::Read, ControlDest::Continue));
                    self.code.push(Instr::LogicalNot(operand))
                }
            },
            Expr::AddrOf(operand) => return self.expr(operand, Context::new(ctx.indirection - 1, ctx.data, ctx.control)),
            Expr::Deref(operand) => return self.expr(operand, Context::new(ctx.indirection + 1, ctx.data, ctx.control)),
            Expr::Do { scope } => return self.scope(scope, ctx),
            Expr::If { condition, then_scope, else_scope } => {
                // At this point it's impossible to know where these basic blocks are supposed to begin, so make them 0 for now
                let true_bb = self.new_bb();
                let false_bb = self.new_bb();
                let post_bb = if else_scope.is_some() {
                    self.new_bb()
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
                    Context::new(0, DataDest::Branch(true_bb, false_bb), ControlDest::Continue),
                );
                self.begin_bb(true_bb);
                let scope_ctx = ctx.redirect(result_location, Some(post_bb));
                self.scope(then_scope, scope_ctx.clone());
                if let Some(else_scope) = else_scope {
                    self.begin_bb(false_bb);
                    self.scope(else_scope, scope_ctx);
                }

                self.begin_bb(post_bb);
                return if let Some(location) = result_location {
                    self.code.push(Instr::Load { location, expr })
                } else {
                    self.handle_control(self.void_instr(), ctx.control)
                };
            },
            Expr::Ret { expr } => {
                return self.expr(
                    expr,
                    Context::new(0, DataDest::Ret, ctx.control),
                );
            }
        };
        
        self.handle_context(instr, expr, ctx, should_allow_set)
    }

    fn handle_indirection(&mut self, mut instr: InstrId, expr: ExprId, mut indirection: i8, target: i8) -> InstrId {
        if indirection > target {
            while indirection > target {
                // TODO: expr below is supposed to correspond to the dereference expression, not the root expression.
                // So right now this ends up with totally the wrong type!
                instr = self.code.push(Instr::Load { location: instr, expr });
                indirection -= 1;
            }
        } else if indirection < target {
            while indirection < target {
                // TODO: same as above!
                let location = self.code.push(Instr::Alloca { expr });
                // TODO: same as above!
                self.code.push(Instr::Store { location, value: instr, expr });
                instr = location;
                indirection += 1;
            }
        }
        instr
    }

    fn handle_control(&mut self, instr: InstrId, control: ControlDest) -> InstrId {
        match control {
            ControlDest::Block(block) => self.code.push(Instr::Br(block)),
            ControlDest::Continue => instr,
            ControlDest::Unreachable => self.void_instr(),
        }
    }

    fn handle_context(&mut self, instr: InstrId, expr: ExprId, ctx: Context, should_allow_set: bool) -> InstrId {
        let instr = self.handle_indirection(instr, expr, ctx.indirection, 0);
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
                    Context::new(0, DataDest::Receive { value: instr, expr }, ctx.control.clone()),
                );
            }
            DataDest::Stmt => {},
        }

        self.handle_control(instr, ctx.control)
    }

    fn build(mut self) -> Function {
        self.scope(self.scope, Context::new(0, DataDest::Ret, ControlDest::Unreachable));
        Function {
            code: self.code,
            basic_blocks: self.basic_blocks,
        }
    }
}