use smallvec::SmallVec;

use crate::ty::Type;
use crate::type_checker as tc;
use crate::index_vec::{Idx, IdxVec};
use crate::builder::{DeclId, LocalDeclId, GlobalDeclId, ExprId, DeclRefId, ScopeId, Intrinsic};
use crate::hir::{self, Expr, Item};

newtype_index!(InstrId pub);
newtype_index!(BasicBlockId pub);
newtype_index!(TerminationId pub);
newtype_index!(FuncId pub);
newtype_index!(StrId pub);

#[derive(Debug)]
pub enum Instr {
    Void,
    IntConst { lit: u64, ty: Type },
    FloatConst { lit: f64, ty: Type },
    StringConst { id: StrId, ty: Type },
    BoolConst(bool),
    Alloca(Type),
    LogicalNot(InstrId),
    Call { arguments: SmallVec<[InstrId; 2]>, func: FuncId },
    Intrinsic { arguments: SmallVec<[InstrId; 2]>, intr: Intrinsic },
    Load(InstrId),
    Store { location: InstrId, value: InstrId },
    Ret(InstrId),
    Br(BasicBlockId),
    CondBr { condition: InstrId, true_bb: BasicBlockId, false_bb: BasicBlockId },
    /// Only valid at the beginning of a function, right after the void instruction
    Parameter(Type),
}

#[derive(Debug)]
enum Decl {
    Stored { location: InstrId },
    Computed { get: FuncId },
    LocalConst { value: InstrId },
    Intrinsic(Intrinsic),
}

#[derive(Debug)]
struct Function {
    name: String,
    code: IdxVec<Instr, InstrId>,
    basic_blocks: IdxVec<InstrId, BasicBlockId>,
}

/// What to do with a value
#[derive(Clone, Debug)]
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
    /// This value will never be used
    Void,
    /// If this value is true, branch to the first basic block, otherwise branch to the second
    Branch(BasicBlockId, BasicBlockId),
}

/// Where to go after the current value is computed (whether implicitly or explicitly, such as via a `break` in a loop)
#[derive(Clone, Debug)]
enum ControlDest {
    Continue,
    Unreachable,
    Block(BasicBlockId),
}

#[derive(Clone, Debug)]
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
    comp_decls: IdxVec<Function, FuncId>,
    strings: IdxVec<String, StrId>,
}

impl Program {
    pub fn build(prog: &hir::Program, tc: &tc::Program) -> Self {
        let mut local_decls = IdxVec::<Decl, LocalDeclId>::new();
        let mut global_decls = IdxVec::<Decl, GlobalDeclId>::new();
        let mut num_functions = 0usize;

        for decl in &prog.local_decls {
            local_decls.push(
                match decl {
                    hir::Decl::Computed { .. } => {
                        num_functions += 1;
                        Decl::Computed { get: FuncId::new(num_functions - 1) }
                    },
                    hir::Decl::Stored => Decl::Stored { location: InstrId::new(std::usize::MAX) },
                    hir::Decl::Parameter(_) => Decl::LocalConst { value: InstrId::new(std::usize::MAX) },
                    hir::Decl::Intrinsic(_) => panic!("Unexpected local intrinisic"),
                }
            );
        }
        for decl in &prog.global_decls {
            global_decls.push(
                match decl {
                    hir::Decl::Computed { .. } => {
                        num_functions += 1;
                        Decl::Computed { get: FuncId::new(num_functions - 1) }
                    },
                    hir::Decl::Stored => panic!("globals not yet supported!"),
                    hir::Decl::Parameter(_) => panic!("global parameters are invalid!"),
                    &hir::Decl::Intrinsic(intr) => Decl::Intrinsic(intr),
                }
            );
        }
        let mut comp_decls = IdxVec::<Function, FuncId>::new();
        let mut strings = IdxVec::<String, StrId>::new();
        for decl in prog.local_decls.iter().chain(&prog.global_decls) {
            if let &hir::Decl::Computed { ref name, ref params, scope } = decl {
                comp_decls.push(
                    FunctionBuilder::new(
                        prog,
                        tc,
                        &mut local_decls,
                        &mut global_decls,
                        &mut strings,
                        name.clone(),
                        scope,
                        &params[..]
                    ).build()
                );
            }
        }
        assert_eq!(num_functions, comp_decls.len());
        Program { comp_decls, strings }
    }
}

struct FunctionBuilder<'a> {
    prog: &'a hir::Program,
    tc: &'a tc::Program,
    local_decls: &'a mut IdxVec<Decl, LocalDeclId>,
    global_decls: &'a mut IdxVec<Decl, GlobalDeclId>,
    strings: &'a mut IdxVec<String, StrId>,
    name: String,
    scope: ScopeId,
    void_instr: InstrId,
    code: IdxVec<Instr, InstrId>,
    basic_blocks: IdxVec<InstrId, BasicBlockId>,
}

impl<'a> FunctionBuilder<'a> {
    fn new(
        prog: &'a hir::Program,
        tc: &'a tc::Program,
        local_decls: &'a mut IdxVec<Decl, LocalDeclId>,
        global_decls: &'a mut IdxVec<Decl, GlobalDeclId>,
        strings: &'a mut IdxVec<String, StrId>,
        name: String,
        scope: ScopeId,
        params: &[LocalDeclId]
    ) -> Self {
        let mut code = IdxVec::new();
        let void_instr = code.push(Instr::Void);
        for &param in params {
            if let hir::Decl::Parameter(ty) = &prog.local_decls[param] {
                let value = code.push(Instr::Parameter(ty.clone()));
                local_decls[param] = Decl::LocalConst { value };
            } else {
                panic!("unexpected non-parameter as parameter decl");
            }
        }
        let mut basic_blocks = IdxVec::new();
        basic_blocks.push(InstrId::new(0));
        FunctionBuilder::<'a> {
            prog,
            tc,
            local_decls,
            global_decls,
            strings,
            name,
            scope,
            void_instr,
            code,
            basic_blocks,
        }
    }

    fn type_of(&self, expr: ExprId) -> Type {
        self.tc.types[expr].clone()
    }

    fn item(&mut self, item: Item) {
        match item {
            Item::Stmt(expr) => {
                self.expr(expr, Context::new(0, DataDest::Void, ControlDest::Continue));
            },
            Item::StoredDecl { id, root_expr } => {
                let ty = self.type_of(root_expr);
                let location = self.code.push(Instr::Alloca(ty));
                self.local_decls[id] = Decl::Stored { location };
                self.expr(root_expr, Context::new(0, DataDest::Store { location }, ControlDest::Continue));
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

    fn get_decl(&self, id: DeclId) -> &Decl {
        match id {
            DeclId::Global(id) => &self.global_decls[id],
            DeclId::Local(id) => &self.local_decls[id],
        }
    }

    fn get(&mut self, arguments: SmallVec<[InstrId; 2]>, id: DeclRefId) -> InstrId {
        let id = self.tc.overloads[id].expect("No overload found!");
        let instr = match self.get_decl(id) {
            &Decl::Computed { get } => Instr::Call { arguments, func: get },
            &Decl::Stored { location } => {
                assert!(arguments.is_empty());
                Instr::Load(location)
            },
            &Decl::LocalConst { value } => return value,
            &Decl::Intrinsic(intr) => Instr::Intrinsic { arguments, intr }
        };
        self.code.push(instr)
    }

    fn set(&mut self, arguments: SmallVec<[InstrId; 2]>, id: DeclRefId, value: InstrId) -> InstrId {
        let id = self.tc.overloads[id].expect("No overload found!");
        let instr = match self.get_decl(id) {
            &Decl::Computed { .. } => panic!("setters not yet implemented!"),
            &Decl::Stored { location } => {
                assert!(arguments.is_empty());
                Instr::Store { location, value }
            },
            &Decl::LocalConst { .. } => panic!("can't set a constant!"),
            &Decl::Intrinsic(_) => panic!("can't set an intrinsic! (yet?)"),
        };
        self.code.push(instr)
    }

    fn modify(&mut self, arguments: SmallVec<[InstrId; 2]>, id: DeclRefId) -> InstrId {
        let id = self.tc.overloads[id].expect("No overload found!");
        let decl = self.get_decl(id);
        match decl {
            &Decl::Computed { .. } => panic!("modify accessors not yet implemented!"),
            &Decl::Stored { location } => {
                assert!(arguments.is_empty());
                location
            },
            &Decl::LocalConst { .. } => panic!("can't modify a constant!"),
            &Decl::Intrinsic(_) => panic!("can't modify an intrinsic! (yet?)"),
        }
    }

    fn expr(&mut self, expr: ExprId, mut ctx: Context) -> InstrId {
        // HACK!!!!
        let mut should_allow_set = false;

        let ty = self.type_of(expr);

        let instr = match self.prog.exprs[expr] {
            Expr::Void => self.void_instr(),
            Expr::IntLit { lit } => self.code.push(Instr::IntConst { lit, ty: ty.clone() }),
            Expr::DecLit { lit } => self.code.push(Instr::FloatConst { lit, ty: ty.clone() }),
            Expr::StrLit { ref lit } => {
                let id = self.strings.push(lit.clone());
                self.code.push(Instr::StringConst { id, ty: ty.clone() })
            },
            Expr::CharLit { lit } => {
                match ty {
                    Type::Int { .. } => self.code.push(Instr::IntConst { lit: lit as u64, ty: ty.clone() }),
                    Type::Pointer(_) => {
                        let id = self.strings.push(String::from_utf8(vec![lit as u8]).unwrap());
                        self.code.push(Instr::StringConst { id, ty: ty.clone() })
                    },
                    _ => panic!("unexpected type for character")
                }
            },
            Expr::Set { lhs, rhs } => {
                self.expr(
                    rhs,
                    Context::new(0, DataDest::Set { dest: lhs }, ctx.control.clone()),
                );
                // Because we override the data destination above, we need to handle it ourselves
                return match ctx.data {
                    DataDest::Ret => self.code.push(Instr::Ret(self.void_instr())),
                    _ => self.void_instr(),
                };
            },
            Expr::DeclRef { ref arguments, id } => {
                should_allow_set = true;
                let arguments = arguments.iter().map(|&argument|
                    self.expr(argument, Context::new(0, DataDest::Read, ControlDest::Continue))
                ).collect();

                if ctx.indirection < 0 {
                    ctx.indirection += 1;
                    self.modify(arguments, id)
                } else {
                    match ctx.data {
                        DataDest::Receive { value, .. } => if ctx.indirection > 0 {
                            let mut location = self.get(arguments, id);
                            location = self.handle_indirection(location, ty.clone(), ctx.indirection, 1);
                            ctx.indirection = 0;
                            self.code.push(Instr::Store { location, value })
                        } else {
                            self.set(arguments, id, value)
                        },
                        _ => self.get(arguments, id),
                    }
                }
            },
            Expr::LogicalAnd { lhs, rhs } => {
                assert_eq!(ctx.indirection, 0);
                let left_true_bb = self.new_bb();
                let location = if let DataDest::Read = ctx.data {
                    Some(self.code.push(Instr::Alloca(ty.clone())))
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
                    self.handle_context(false_val, expr, Type::Bool, branch_ctx, false);

                    self.begin_bb(after_bb);
                    if let Some(location) = location {
                        self.code.push(Instr::Load(location))
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
                        Some(self.code.push(Instr::Alloca(ty.clone())))
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
                    self.handle_context(true_val, expr, Type::Bool, branch_ctx.clone(), false);

                    self.begin_bb(left_false_bb);
                    self.expr(rhs, branch_ctx);

                    self.begin_bb(after_bb);
                    if let Some(location) = location {
                        self.code.push(Instr::Load(location))
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
                let true_bb = self.new_bb();
                let false_bb = self.new_bb();
                let post_bb = if else_scope.is_some() {
                    self.new_bb()
                } else {
                    false_bb
                };

                let result_location = match (&ctx.data, else_scope) {
                    (DataDest::Read, Some(_)) => Some(
                        // TODO: this will be the wrong type if indirection != 0
                        self.code.push(Instr::Alloca(ty.clone()))
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
                if let Some(location) = result_location {
                    return self.code.push(Instr::Load(location))
                } else if else_scope.is_some() {
                    return self.handle_control(self.void_instr(), ctx.control)
                } else {
                    self.void_instr()
                }
            },
            Expr::While { condition, scope } => {
                assert_eq!(ctx.indirection, 0);
                let test_bb = self.new_bb();
                let loop_bb = self.new_bb();
                let post_bb = match ctx.control {
                    ControlDest::Continue | ControlDest::Unreachable => self.new_bb(),
                    ControlDest::Block(block) => block,
                };

                self.begin_bb(test_bb);
                self.expr(condition, Context::new(0, DataDest::Branch(loop_bb, post_bb), ControlDest::Continue));

                self.begin_bb(loop_bb);
                self.scope(scope, Context::new(0, DataDest::Read, ControlDest::Block(test_bb)))
            },
            Expr::Ret { expr } => {
                return self.expr(
                    expr,
                    Context::new(0, DataDest::Ret, ctx.control),
                );
            }
        };
        
        self.handle_context(instr, expr, ty, ctx, should_allow_set)
    }

    fn handle_indirection(&mut self, mut instr: InstrId, ty: Type, mut indirection: i8, target: i8) -> InstrId {
        if indirection > target {
            while indirection > target {
                instr = self.code.push(Instr::Load(instr));
                indirection -= 1;
            }
        } else if indirection < target {
            while indirection < target {
                // TODO: this will be the wrong type if indirection != 0
                let location = self.code.push(Instr::Alloca(ty.clone()));
                self.code.push(Instr::Store { location, value: instr });
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

    fn handle_context(&mut self, instr: InstrId, expr: ExprId, ty: Type, ctx: Context, should_allow_set: bool) -> InstrId {
        let instr = self.handle_indirection(instr, ty, ctx.indirection, 0);
        match ctx.data {
            DataDest::Read => return instr,
            DataDest::Ret => return self.code.push(Instr::Ret(instr)),
            DataDest::Branch(true_bb, false_bb)
                => return self.code.push(Instr::CondBr { condition: instr, true_bb, false_bb }),
            DataDest::Receive { .. } => {
                assert!(should_allow_set, "can't set constant expression!");
            },
            DataDest::Store { location } => {
                self.code.push(Instr::Store { location, value: instr });
            },
            DataDest::Set { dest } => {
                return self.expr(
                    dest,
                    Context::new(0, DataDest::Receive { value: instr, expr }, ctx.control.clone()),
                );
            }
            DataDest::Void => {},
        }

        self.handle_control(instr, ctx.control)
    }

    fn build(mut self) -> Function {
        self.scope(self.scope, Context::new(0, DataDest::Ret, ControlDest::Unreachable));
        Function {
            name: self.name,
            code: self.code,
            basic_blocks: self.basic_blocks,
        }
    }
}
