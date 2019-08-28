use std::ffi::CString;
use std::fmt;

use smallvec::SmallVec;

use crate::arch::Arch;
use crate::ty::Type;
use crate::type_checker as tc;
use tc::CastMethod;
use crate::index_vec::{Idx, IdxVec};
use crate::builder::{DeclId, ExprId, DeclRefId, ScopeId, Intrinsic};
use crate::hir::{self, Expr, Item, StoredDeclId};
use crate::interpreter::Interpreter;

newtype_index!(InstrId pub);
newtype_index!(BasicBlockId pub);
newtype_index!(TerminationId pub);
newtype_index!(FuncId pub);
newtype_index!(StrId pub);
newtype_index!(StaticId pub);

#[derive(Clone, Debug, PartialEq)]
pub enum Const {
    Int { lit: u64, ty: Type },
    Float { lit: f64, ty: Type },
    Str { id: StrId, ty: Type },
    Bool(bool),
}

impl Const {
    pub fn ty(&self) -> Type {
        match self {
            Const::Int { ty, .. } => ty.clone(),
            Const::Float { ty, .. } => ty.clone(),
            Const::Str { ty, .. } => ty.clone(),
            Const::Bool(_) => Type::Bool,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Instr {
    Void,
    Const(Const),
    Alloca(Type),
    LogicalNot(InstrId),
    Call { arguments: SmallVec<[InstrId; 2]>, func: FuncId },
    Intrinsic { arguments: SmallVec<[InstrId; 2]>, ty: Type, intr: Intrinsic },
    Reinterpret(InstrId, Type),
    Truncate(InstrId, Type),
    SignExtend(InstrId, Type),
    ZeroExtend(InstrId, Type),
    FloatCast(InstrId, Type),
    FloatToInt(InstrId, Type),
    IntToFloat(InstrId, Type),
    Load(InstrId),
    Store { location: InstrId, value: InstrId },
    AddressOfStatic(StaticId),
    Ret(InstrId),
    Br(BasicBlockId),
    CondBr { condition: InstrId, true_bb: BasicBlockId, false_bb: BasicBlockId },
    /// Only valid at the beginning of a function, right after the void instruction
    Parameter(Type),
}

#[derive(Debug)]
enum Decl {
    Stored(StoredDeclId),
    Computed { get: FuncId },
    LocalConst { value: InstrId },
    Intrinsic(Intrinsic, Type),
    Static(StaticId),
    Const(Const),
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub ret_ty: Type,
    pub code: IdxVec<Instr, InstrId>,
    pub basic_blocks: IdxVec<InstrId, BasicBlockId>,
}

impl Function {
    pub fn num_parameters(&self) -> usize {
        let raw_code = &self.code.raw;
        assert_eq!(raw_code[0], Instr::Void);
        let mut num_parameters = 0;
        for i in 1..raw_code.len() {
            match &raw_code[i] {
                Instr::Parameter(_) => num_parameters += 1,
                _ => break,
            }
        }
        num_parameters
    }
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
    pub functions: IdxVec<Function, FuncId>,
    pub strings: IdxVec<CString, StrId>,
    pub statics: IdxVec<Const, StaticId>,
    pub arch: Arch,
}

fn expr_to_const(expr: &Expr, ty: Type, strings: &mut IdxVec<CString, StrId>) -> Const {
    match *expr {
        Expr::IntLit { lit } => {
            match ty {
                Type::Int { .. } => Const::Int { lit, ty },
                Type::Float(_)   => Const::Float { lit: lit as f64, ty },
                _ => panic!("Unrecognized integer literal type {:?}", ty),
            }
        },
        Expr::DecLit { lit } => Const::Float { lit, ty },
        Expr::StrLit { ref lit } => {
            let id = strings.push(lit.clone());
            Const::Str { id, ty }
        },
        Expr::CharLit { lit } => {
            match ty {
                Type::Int { .. } => Const::Int { lit: lit as u64, ty },
                Type::Pointer(_) => {
                    let id = strings.push(CString::new([lit as u8].as_ref()).unwrap());
                    Const::Str { id, ty }
                },
                _ => panic!("unexpected type for character")
            }
        },
        _ => panic!("Cannot convert expression to constant: {:#?}", expr),
    }
}

pub enum FunctionRef {
    Id(FuncId),
    Ref(Function),
}

pub trait MirProvider {
    fn arch(&self) -> Arch;
    fn string(&self, id: StrId) -> &CString;
    fn new_string(&mut self, val: CString) -> StrId;
    fn num_statics(&self) -> usize;
    fn statik(&self, id: usize) -> Const;
    fn function(&self, id: FuncId) -> &Function;
    fn function_by_ref<'a>(&'a self, func_ref: &'a FunctionRef) -> &'a Function {
        match func_ref {
            &FunctionRef::Id(id) => self.function(id),
            FunctionRef::Ref(func) => func,
        }
    }
    fn type_of(&self, instr: InstrId, func_ref: &FunctionRef) -> Type {
        let func = self.function_by_ref(func_ref);
        match &func.code[instr] {
            Instr::Void | Instr::Store { .. } => Type::Void,
            Instr::Const(konst) => konst.ty(),
            Instr::Alloca(ty) => ty.clone().mut_ptr(),
            Instr::LogicalNot(_) => Type::Bool,
            &Instr::Call { func, .. } => self.function(func).ret_ty.clone(),
            Instr::Intrinsic { ty, .. } => ty.clone(),
            Instr::Reinterpret(_, ty) | Instr::Truncate(_, ty) | Instr::SignExtend(_, ty)
                | Instr::ZeroExtend(_, ty) | Instr::FloatCast(_, ty) | Instr::FloatToInt(_, ty)
                | Instr::IntToFloat(_, ty)
                  => ty.clone(),
            &Instr::Load(instr) => match self.type_of(instr, func_ref) {
                Type::Pointer(pointee) => pointee.ty,
                _ => Type::Error,
            },
            &Instr::AddressOfStatic(statik) => self.statik(statik.idx()).ty().mut_ptr(),
            Instr::Ret(_) | Instr::Br(_) | Instr::CondBr { .. } => Type::Never,
            Instr::Parameter(ty) => ty.clone(),
        }
    }
}

impl<'a> MirProvider for Builder<'a> {
    fn arch(&self) -> Arch { self.arch }
    fn string(&self, id: StrId) -> &CString { &self.strings[id] }
    fn new_string(&mut self, val: CString) -> StrId { self.strings.push(val) }
    fn num_statics(&self) -> usize { self.static_inits.len() }
    fn statik(&self, _id: usize) -> Const { panic!("can't get static from mir builder"); }
    fn function(&self, id: FuncId) -> &Function { &self.functions[id] }
}

impl MirProvider for Program {
    fn arch(&self) -> Arch { self.arch }
    fn string(&self, id: StrId) -> &CString { &self.strings[id] }
    fn new_string(&mut self, val: CString) -> StrId { self.strings.push(val) }
    fn num_statics(&self) -> usize { self.statics.len() }
    fn statik(&self, id: usize) -> Const { self.statics[StaticId::new(id)].clone() }
    fn function(&self, id: FuncId) -> &Function { &self.functions[id] }
}

pub struct Builder<'a> {
    hir: &'a hir::Program,
    tc: &'a tc::Program,
    arch: Arch,
    decls: IdxVec<Decl, DeclId>,
    strings: IdxVec<CString, StrId>,
    static_inits: IdxVec<Function, StaticId>,
    functions: IdxVec<Function, FuncId>,
}

impl<'a> Builder<'a> {
    pub fn new(hir: &'a hir::Program, tc: &'a tc::Program, arch: Arch) -> Self {
        let mut decls = IdxVec::<Decl, DeclId>::new();
        let mut strings = IdxVec::<CString, StrId>::new();
        let mut num_functions = 0usize;
        let mut num_statics = 0usize;

        for (i, decl) in hir.decls.iter().enumerate() {
            let id = DeclId::new(i);
            decls.push(
                match *decl {
                    hir::Decl::Computed { .. } => {
                        num_functions += 1;
                        Decl::Computed { get: FuncId::new(num_functions - 1) }
                    },
                    hir::Decl::Stored(index) => Decl::Stored(index),
                    hir::Decl::Parameter { index } => Decl::LocalConst { value: InstrId::new(index + 1) },
                    hir::Decl::Intrinsic(intr) => Decl::Intrinsic(intr, tc.decl_types[id].clone()),
                    hir::Decl::Static(_) => {
                        num_statics += 1;
                        Decl::Static(StaticId::new(num_statics - 1))
                    },
                    hir::Decl::Const(root_expr) => {
                        let konst = expr_to_const(&hir.exprs[root_expr], tc.types[root_expr].clone(), &mut strings);
                        Decl::Const(konst)
                    },
                }
            );
        }

        Self { 
            hir,
            tc,
            arch,
            decls,
            strings,
            static_inits: IdxVec::new(),
            functions: IdxVec::new(),
        }
    }

    pub fn build_decl(&mut self, id: DeclId) {
        match self.hir.decls[id] {
            hir::Decl::Computed { ref name, ref params, scope } => {
                self.functions.push(
                    FunctionBuilder::new(
                        &self.hir,
                        &self.tc,
                        &self.decls,
                        &mut self.strings,
                        name.clone(),
                        self.tc.decl_types[id].clone(),
                        FunctionBody::Scope(scope),
                        &params[..],
                        self.arch,
                    ).build()
                );
            },
            hir::Decl::Static(expr) => {
                self.static_inits.push(
                    FunctionBuilder::new(
                        self.hir,
                        self.tc,
                        &self.decls,
                        &mut self.strings,
                        String::from("static_init"),
                        self.tc.decl_types[id].clone(),
                        FunctionBody::Expr(expr),
                        &[],
                        self.arch,
                    ).build()
                );
            },
            _ => {},
        }
    }

    pub fn build(mut self) -> Program {
        for i in 0..self.hir.decls.len() {
            self.build_decl(DeclId::new(i));
        }

        let mut prog = Program {
            functions: self.functions,
            strings: self.strings,
            statics: IdxVec::new(),
            arch: self.arch,
        };
        // TODO: creating a new variable for statics here is a hack to prevent userspace code from being able to access static variables
        // There should be a flag passed to the interpreter to solve this problem (with a nice error message) instead.
        let mut statics = IdxVec::new();

        for statik in self.static_inits.raw {
            let ty = statik.ret_ty.clone();
            let konst = Interpreter::new(&prog)
                .call(FunctionRef::Ref(statik), Vec::new())
                .to_const(self.arch, ty, &mut prog.strings);
            statics.push(konst);
        }
        prog.statics = statics;

        prog
    }
}

fn fmt_const(f: &mut fmt::Formatter, konst: &Const, prog: &Program) -> fmt::Result {
    match *konst {
        Const::Bool(val) => writeln!(f, "{}", val),
        Const::Float { lit, ref ty } => writeln!(f, "{} as {:?}", lit, ty),
        Const::Int { lit, ref ty } => writeln!(f, "{} as {:?}", lit, ty),
        Const::Str { id, ref ty } => writeln!(f, "%str{} ({:?}) as {:?}", id.idx(), prog.strings[id], ty),
    }
}

impl fmt::Display for Program {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !self.statics.raw.is_empty() {
            for (i, statik) in self.statics.iter().enumerate() {
                write!(f, "%static{} = ", i)?;
                fmt_const(f, statik, self)?;
            }
            writeln!(f)?;
        }

        for (i, func) in self.functions.iter().enumerate() {
            write!(f, "fn {}", &func.name)?;
            assert_eq!(&func.code.raw[0], &Instr::Void);
            let mut first = true;
            for i in 1..func.code.len() {
                if let Instr::Parameter(ty) = &func.code.raw[i] {
                    if first {
                        write!(f, "(")?;
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "%{}: {:?}", i, ty)?;
                } else {
                    if !first {
                        write!(f, ")")?;
                    }
                    break;
                }
            }
            writeln!(f, ": {:?} {{", func.ret_ty)?;
            struct BB {
                id: BasicBlockId,
                instr: InstrId,
            }
            let mut basic_blocks: Vec<BB> = func.basic_blocks.iter().enumerate()
                .map(|(id, &instr)| BB { id: BasicBlockId::new(id), instr })
                .collect();
            basic_blocks.sort_by_key(|bb| bb.instr.idx());
            for i in 0..basic_blocks.len() {
                let lower_bound = basic_blocks[i].instr.idx();
                let upper_bound = if i + 1 < basic_blocks.len() {
                    basic_blocks[i + 1].instr.idx()
                } else {
                    func.code.len()
                };
                writeln!(f, "%bb{}:", basic_blocks[i].id.idx())?;
                if lower_bound == upper_bound { continue }
                
                for i in lower_bound..upper_bound {
                    let instr = &func.code.raw[i];
                    write!(f, "    ")?;
                    macro_rules! write_args {
                        ($args:expr) => {{
                            let mut first = true;
                            for arg in $args {
                                if first {
                                    write!(f, "(")?;
                                    first = false;
                                } else {
                                    write!(f, ", ")?;
                                }
                                write!(f, "%{}", arg.idx())?;
                            }
                            if !first {
                                write!(f, ")")?;
                            }
                            writeln!(f)
                        }}
                    }
                    match instr {
                        Instr::Alloca(ty) => writeln!(f, "%{} = alloca {:?}", i, ty)?,
                        Instr::Br(block) => writeln!(f, "br %bb{}", block.idx())?,
                        &Instr::CondBr { condition, true_bb, false_bb }
                            => writeln!(f, "condbr %{}, %bb{}, %bb{}", condition.idx(), true_bb.idx(), false_bb.idx())?,
                        &Instr::Call { ref arguments, func: callee } => {
                            write!(f, "%{} = call `{}`", i, self.functions[callee].name)?;
                            write_args!(arguments)?
                        },
                        Instr::Const(konst) => {
                            write!(f, "%{} = ", i)?;
                            fmt_const(f, konst, self)?;
                        },
                        Instr::Intrinsic { arguments, intr, .. } => {
                            write!(f, "%{} = intrinsic `{}`", i, intr.name())?;
                            write_args!(arguments)?
                        },
                        Instr::Load(location) => writeln!(f, "%{} = load %{}", i, location.idx())?,
                        Instr::LogicalNot(op) => writeln!(f, "%{} = not %{}", i, op.idx())?,
                        Instr::Ret(val) => writeln!(f,  "return %{}", val.idx())?,
                        Instr::Store { location, value } => writeln!(f, "store %{} in %{}", value.idx(), location.idx())?,
                        Instr::AddressOfStatic(statik) => writeln!(f, "%{} = address of %static{}", i, statik.idx())?,
                        &Instr::Reinterpret(val, ref ty) => writeln!(f, "%{} = reinterpret %{} as {:?}", i, val.idx(), ty)?,
                        &Instr::SignExtend(val, ref ty) => writeln!(f, "%{} = sign-extend %{} as {:?}", i, val.idx(), ty)?,
                        &Instr::ZeroExtend(val, ref ty) => writeln!(f, "%{} = zero-extend %{} as {:?}", i, val.idx(), ty)?,
                        &Instr::Truncate(val, ref ty) => writeln!(f, "%{} = truncate %{} as {:?}", i, val.idx(), ty)?,
                        &Instr::FloatCast(val, ref ty) => writeln!(f, "%{} = floatcast %{} as {:?}", i, val.idx(), ty)?,
                        &Instr::IntToFloat(val, ref ty) => writeln!(f, "%{} = inttofloat %{} as {:?}", i, val.idx(), ty)?,
                        &Instr::FloatToInt(val, ref ty) => writeln!(f, "%{} = floattoint %{} as {:?}", i, val.idx(), ty)?,
                        Instr::Parameter(_) => panic!("unexpected parameter!"),
                        Instr::Void => panic!("unexpected void!"),
                    };
                }
            }
            write!(f, "}}")?;
            if i + 1 < self.functions.len() {
                writeln!(f, "\n")?;
            }
        }
        Ok(())
    }
}

#[derive(Copy, Clone)]
enum FunctionBody {
    Scope(ScopeId),
    Expr(ExprId),
}

struct FunctionBuilder<'a> {
    prog: &'a hir::Program,
    tc: &'a tc::Program,
    decls: &'a IdxVec<Decl, DeclId>,
    strings: &'a mut IdxVec<CString, StrId>,
    name: String,
    ret_ty: Type,
    body: FunctionBody,
    arch: Arch,
    void_instr: InstrId,
    code: IdxVec<Instr, InstrId>,
    basic_blocks: IdxVec<InstrId, BasicBlockId>,
    stored_decl_locs: IdxVec<InstrId, StoredDeclId>,
}

impl<'a> FunctionBuilder<'a> {
    fn new(
        prog: &'a hir::Program,
        tc: &'a tc::Program,
        decls: &'a IdxVec<Decl, DeclId>,
        strings: &'a mut IdxVec<CString, StrId>,
        name: String,
        ret_ty: Type,
        body: FunctionBody,
        params: &[DeclId],
        arch: Arch,
    ) -> Self {
        let mut code = IdxVec::new();
        let void_instr = code.push(Instr::Void);
        for &param in params {
            if let hir::Decl::Parameter { .. } = prog.decls[param] {
                code.push(Instr::Parameter(tc.decl_types[param].clone()));
            } else {
                panic!("unexpected non-parameter as parameter decl");
            }
        }
        let mut basic_blocks = IdxVec::new();
        basic_blocks.push(InstrId::new(code.len()));
        FunctionBuilder::<'a> {
            prog,
            tc,
            decls,
            strings,
            name,
            ret_ty,
            body,
            void_instr,
            code,
            basic_blocks,
            arch,

            stored_decl_locs: IdxVec::new(),
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
                assert_eq!(self.stored_decl_locs.len(), id.idx());
                self.stored_decl_locs.push(location);
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
        let last_instr = self.code.raw.last().unwrap();
        assert!(
            match last_instr {
                Instr::Void | Instr::Parameter(_) | Instr::Br(_) | Instr::CondBr { .. } | Instr::Ret { .. } | Instr::Intrinsic { intr: Intrinsic::Panic, .. } => true,
                _ => false,
            },
            "expected terminal instruction before moving on to next block, found {:?}",
            last_instr,
        );
        assert_eq!(self.basic_blocks[bb].idx(), 0);

        self.basic_blocks[bb] = InstrId::new(self.code.len())
    }

    fn get(&mut self, arguments: SmallVec<[InstrId; 2]>, id: DeclRefId) -> InstrId {
        let id = self.tc.overloads[id].expect("No overload found!");
        match self.decls[id] {
            Decl::Computed { get } => self.code.push(Instr::Call { arguments, func: get }),
            Decl::Stored(id) => {
                assert!(arguments.is_empty());
                let location = self.stored_decl_locs[id];
                self.code.push(Instr::Load(location))
            },
            Decl::LocalConst { value } => value,
            Decl::Intrinsic(intr, ref ty) => {
                self.code.push(Instr::Intrinsic { arguments, ty: ty.clone(), intr })
            },
            Decl::Const(ref konst) => self.code.push(Instr::Const(konst.clone())),
            Decl::Static(statik) => {
                let location = self.code.push(Instr::AddressOfStatic(statik));
                self.code.push(Instr::Load(location))
            },
        }
    }

    fn set(&mut self, arguments: SmallVec<[InstrId; 2]>, id: DeclRefId, value: InstrId) -> InstrId {
        let id = self.tc.overloads[id].expect("No overload found!");
        match &self.decls[id] {
            Decl::Computed { .. } => panic!("setters not yet implemented!"),
            &Decl::Stored(id) => {
                assert!(arguments.is_empty());
                let location = self.stored_decl_locs[id];
                self.code.push(Instr::Store { location, value })
            },
            Decl::LocalConst { .. } | Decl::Const(_) => panic!("can't set a constant!"),
            Decl::Intrinsic(_, _) => panic!("can't set an intrinsic! (yet?)"),
            &Decl::Static(statik) => {
                let location = self.code.push(Instr::AddressOfStatic(statik));
                self.code.push(Instr::Store { location, value })
            }
        }
    }

    fn modify(&mut self, arguments: SmallVec<[InstrId; 2]>, id: DeclRefId) -> InstrId {
        let id = self.tc.overloads[id].expect("No overload found!");
        let decl = &self.decls[id];
        match decl {
            Decl::Computed { .. } => panic!("modify accessors not yet implemented!"),
            &Decl::Stored(id) => {
                assert!(arguments.is_empty());
                self.stored_decl_locs[id]
            },
            Decl::LocalConst { .. } | Decl::Const(_) => panic!("can't modify a constant!"),
            Decl::Intrinsic(_, _) => panic!("can't modify an intrinsic! (yet?)"),
            &Decl::Static(statik) => self.code.push(Instr::AddressOfStatic(statik)),
        }
    }

    fn expr(&mut self, expr: ExprId, mut ctx: Context) -> InstrId {
        // HACK!!!!
        let mut should_allow_set = false;

        let ty = self.type_of(expr);

        let instr = match self.prog.exprs[expr] {
            Expr::Void => self.void_instr(),
            Expr::IntLit { .. } | Expr::DecLit { .. } | Expr::StrLit { .. } | Expr::CharLit { .. } => {
                let konst = expr_to_const(&self.prog.exprs[expr], ty.clone(), &mut self.strings);
                self.code.push(Instr::Const(konst))
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
                    let false_val = self.code.push(Instr::Const(Const::Bool(false)));
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
                    let true_val = self.code.push(Instr::Const(Const::Bool(true)));
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
            Expr::Cast { expr, ty: ref dest_ty, cast_id } => match &self.tc.cast_methods[cast_id] {
                CastMethod::Noop => return self.expr(expr, ctx),
                CastMethod::Reinterpret => {
                    let value = self.expr(expr, Context::new(0, DataDest::Read, ControlDest::Continue));
                    self.code.push(Instr::Reinterpret(value, dest_ty.clone()))
                },
                CastMethod::Int => {
                    let (src_width, _src_is_signed, dest_width, dest_is_signed) = match (&self.tc.types[expr], dest_ty) {
                        (&Type::Int { width: ref src_width, is_signed: src_is_signed }, &Type::Int { width: ref dest_width, is_signed: dest_is_signed })
                            => (src_width.clone(), src_is_signed, dest_width.clone(), dest_is_signed),
                        _ => panic!("Internal compiler error: found invalid cast types while generating MIR")
                    };
                    let (src_bit_width, dest_bit_width) = (src_width.bit_width(self.arch), dest_width.bit_width(self.arch));
                    let value = self.expr(expr, Context::new(0, DataDest::Read, ControlDest::Continue));

                    if src_bit_width == dest_bit_width {
                        // TODO: Bounds checking
                        self.code.push(Instr::Reinterpret(value, dest_ty.clone()))
                    } else if src_bit_width < dest_bit_width {
                        if dest_is_signed {
                            // TODO: Bounds checking
                            self.code.push(Instr::SignExtend(value, dest_ty.clone()))
                        } else {
                            // TODO: Bounds checking
                            self.code.push(Instr::ZeroExtend(value, dest_ty.clone()))
                        }
                    } else if src_bit_width > dest_bit_width {
                        // TODO: Bounds checking
                        self.code.push(Instr::Truncate(value, dest_ty.clone()))
                    } else {
                        unreachable!()
                    }
                },
                CastMethod::Float => {
                    let value = self.expr(expr, Context::new(0, DataDest::Read, ControlDest::Continue));
                    self.code.push(Instr::FloatCast(value, dest_ty.clone()))
                },
                CastMethod::FloatToInt => {
                    let value = self.expr(expr, Context::new(0, DataDest::Read, ControlDest::Continue));
                    self.code.push(Instr::FloatToInt(value, dest_ty.clone()))
                },
                CastMethod::IntToFloat => {
                    let value = self.expr(expr, Context::new(0, DataDest::Read, ControlDest::Continue));
                    self.code.push(Instr::IntToFloat(value, dest_ty.clone()))
                },
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

                self.code.push(Instr::Br(test_bb));
                self.begin_bb(test_bb);
                self.expr(condition, Context::new(0, DataDest::Branch(loop_bb, post_bb), ControlDest::Continue));

                self.begin_bb(loop_bb);
                let val = self.scope(scope, Context::new(0, DataDest::Void, ControlDest::Block(test_bb)));

                match ctx.control {
                    ControlDest::Continue | ControlDest::Unreachable => {
                        self.begin_bb(post_bb);
                        val
                    },
                    ControlDest::Block(_) => return val,
                }
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
        let ctx = Context::new(0, DataDest::Ret, ControlDest::Unreachable);
        match self.body {
            FunctionBody::Expr(expr) => self.expr(expr, ctx),
            FunctionBody::Scope(scope) => self.scope(scope, ctx),
        };
        Function {
            name: self.name,
            ret_ty: self.ret_ty,
            code: self.code,
            basic_blocks: self.basic_blocks,
        }
    }
}
