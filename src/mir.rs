use std::collections::HashMap;
use std::ffi::CString;
use std::fmt;
use std::rc::Rc;

use smallvec::SmallVec;
use string_interner::{Sym, DefaultStringInterner as Interner};

use crate::arch::Arch;
use crate::ty::Type;
use crate::type_checker as tc;
use tc::CastMethod;
use crate::index_vec::{Idx, IdxVec};
use crate::builder::{DeclId, ExprId, DeclRefId, ScopeId, Intrinsic};
use crate::hir::{self, Expr, Item, StoredDeclId};
use crate::interpreter::{Interpreter, InterpMode};

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
    Pointer { op: InstrId, is_mut: bool },
    Ret(InstrId),
    Br(BasicBlockId),
    CondBr { condition: InstrId, true_bb: BasicBlockId, false_bb: BasicBlockId },
    /// Only valid at the beginning of a function, right after the void instruction
    Parameter(Type),
}

#[derive(Clone, Debug)]
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
    pub name: Option<Sym>,
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
    fn statics(&self) -> &[Const];
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
            Instr::Pointer { .. } => Type::Ty,
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
            &Instr::AddressOfStatic(statik) => self.statics()[statik.idx()].ty().mut_ptr(),
            Instr::Ret(_) | Instr::Br(_) | Instr::CondBr { .. } => Type::Never,
            Instr::Parameter(ty) => ty.clone(),
        }
    }
}

impl<'a> MirProvider for Builder<'a> {
    fn arch(&self) -> Arch { self.arch }
    fn string(&self, id: StrId) -> &CString { &self.strings[id] }
    fn new_string(&mut self, val: CString) -> StrId { self.strings.push(val) }
    fn statics(&self) -> &[Const] { &self.statics.raw }
    fn function(&self, id: FuncId) -> &Function { &self.functions[id] }
}

pub struct Builder<'a> {
    hir: &'a hir::Program,
    tc: &'a tc::Program,
    decls: HashMap<DeclId, Decl>,
    static_inits: IdxVec<Function, StaticId>,
    pub arch: Arch,
    pub strings: IdxVec<CString, StrId>,
    pub functions: IdxVec<Function, FuncId>,
    pub statics: IdxVec<Const, StaticId>,
}

impl<'a> Builder<'a> {
    pub fn new(hir: &'a hir::Program, tc: &'a tc::Program, arch: Arch) -> Self {
        Self { 
            hir,
            tc,
            decls: HashMap::new(),
            static_inits: IdxVec::new(),
            arch,
            strings: IdxVec::new(),
            functions: IdxVec::new(),
            statics: IdxVec::new(),
        }
    }

    fn get_decl(&mut self, id: DeclId) -> Decl {
        if let Some(decl) = self.decls.get(&id) { return decl.clone(); }
        match self.hir.decls[id] {
            hir::Decl::Computed { ref params, scope, .. } => {
                let get = FuncId::new(self.functions.len());
                let decl = Decl::Computed { get };
                self.decls.insert(id, decl.clone());
                let func = FunctionBuilder::new(
                    self,
                    Some(self.hir.names[id]),
                    self.tc.decl_types[id].clone(),
                    FunctionBody::Scope(scope),
                    &params[..],
                    self.arch,
                ).build();
                self.functions.push(func);
                decl
            },
            hir::Decl::Stored { id: index, .. } => {
                let decl = Decl::Stored(index);
                self.decls.insert(id, decl.clone());
                decl
            },
            hir::Decl::Parameter { index } => {
                let decl = Decl::LocalConst { value: InstrId::new(index + 1) };
                self.decls.insert(id, decl.clone());
                decl
            },
            hir::Decl::Intrinsic { intr, .. } => {
                let decl = Decl::Intrinsic(intr, self.tc.decl_types[id].clone());
                self.decls.insert(id, decl.clone());
                decl
            },
            hir::Decl::Static(expr) => {
                let decl = Decl::Static(StaticId::new(self.static_inits.len()));
                self.decls.insert(id, decl.clone());
                let func = FunctionBuilder::new(
                    self,
                    None,
                    self.tc.decl_types[id].clone(),
                    FunctionBody::Expr(expr),
                    &[],
                    self.arch,
                ).build();
                self.static_inits.push(func);
                decl
            },
            hir::Decl::Const(root_expr) => {
                let ty = self.tc.decl_types[id].clone();
                let function = FunctionBuilder::new(
                    self,
                    None,
                    ty.clone(),
                    FunctionBody::Expr(root_expr),
                    &[],
                    self.arch,
                ).build();
                let konst = Interpreter::new(&*self, InterpMode::CompileTime)
                    .call(FunctionRef::Ref(function), Vec::new())
                    .to_const(self.arch, ty, &mut self.strings);
                
                // TODO: Deal with cycles!
                let decl = Decl::Const(konst);
                self.decls.insert(id, decl.clone());
                decl
            },
        }
    }

    pub fn build(&mut self) {
        for i in 0..self.hir.decls.len() {
            self.get_decl(DeclId::new(i));
        }

        let static_inits = std::mem::replace(&mut self.static_inits, IdxVec::new());
        for statik in static_inits.raw {
            let ty = statik.ret_ty.clone();
            let konst = Interpreter::new(self, InterpMode::CompileTime)
                .call(FunctionRef::Ref(statik), Vec::new())
                .to_const(self.arch, ty, &mut self.strings);
            self.statics.push(konst);
        }
    }
}

impl<'a> Builder<'a> {
    fn fmt_const(&self, f: &mut fmt::Formatter, konst: &Const) -> fmt::Result {
        match *konst {
            Const::Bool(val) => writeln!(f, "{}", val),
            Const::Float { lit, ref ty } => writeln!(f, "{} as {:?}", lit, ty),
            Const::Int { lit, ref ty } => writeln!(f, "{} as {:?}", lit, ty),
            Const::Str { id, ref ty } => writeln!(f, "%str{} ({:?}) as {:?}", id.idx(), self.strings[id], ty),
        }
    }

    fn fn_name(&self, name: Option<Sym>) -> &str {
        match name {
            Some(name) => self.hir.interner.as_ref().resolve(name).unwrap(),
            None => "{anonymous}",
        }
    }
}

impl<'a> fmt::Display for Builder<'a> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !self.statics.raw.is_empty() {
            for (i, statik) in self.statics.iter().enumerate() {
                write!(f, "%static{} = ", i)?;
                self.fmt_const(f, statik)?;
            }
            writeln!(f)?;
        }

        for (i, func) in self.functions.iter().enumerate() {
            write!(f, "fn {}", self.fn_name(func.name))?;
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
                            write!(f, "%{} = call `{}`", i, self.fn_name(self.functions[callee].name))?;
                            write_args!(arguments)?
                        },
                        Instr::Const(konst) => {
                            write!(f, "%{} = ", i)?;
                            self.fmt_const(f, konst)?;
                        },
                        Instr::Intrinsic { arguments, intr, .. } => {
                            write!(f, "%{} = intrinsic `{}`", i, intr.name())?;
                            write_args!(arguments)?
                        },
                        &Instr::Pointer { op, is_mut } => {
                            write!(f, "%{} = %{} *", i, op.idx())?;
                            if is_mut {
                                writeln!(f, "mut")?
                            } else {
                                writeln!(f)?
                            }
                        }
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

struct FunctionBuilder<'a, 'mir: 'a> {
    b: &'a mut Builder<'mir>,
    name: Option<Sym>,
    ret_ty: Type,
    body: FunctionBody,
    arch: Arch,
    void_instr: InstrId,
    code: IdxVec<Instr, InstrId>,
    basic_blocks: IdxVec<InstrId, BasicBlockId>,
    stored_decl_locs: IdxVec<InstrId, StoredDeclId>,
}

impl<'a, 'mir: 'a> FunctionBuilder<'a, 'mir> {
    fn new(
        b: &'a mut Builder<'mir>,
        name: Option<Sym>,
        ret_ty: Type,
        body: FunctionBody,
        params: &[DeclId],
        arch: Arch,
    ) -> Self {
        let mut code = IdxVec::new();
        let void_instr = code.push(Instr::Void);
        for &param in params {
            if let hir::Decl::Parameter { .. } = b.hir.decls[param] {
                code.push(Instr::Parameter(b.tc.decl_types[param].clone()));
            } else {
                panic!("unexpected non-parameter as parameter decl");
            }
        }
        let mut basic_blocks = IdxVec::new();
        basic_blocks.push(InstrId::new(code.len()));
        FunctionBuilder::<'a, 'mir> {
            b,
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
        self.b.tc.types[expr].clone()
    }

    fn item(&mut self, item: Item) {
        match item {
            Item::Stmt(expr) => {
                self.expr(expr, Context::new(0, DataDest::Void, ControlDest::Continue));
            },
            Item::StoredDecl { id, root_expr, .. } => {
                let ty = self.type_of(root_expr);
                let location = self.code.push(Instr::Alloca(ty));
                assert_eq!(self.stored_decl_locs.len(), id.idx());
                self.stored_decl_locs.push(location);
                self.expr(root_expr, Context::new(0, DataDest::Store { location }, ControlDest::Continue));
            },
            // No need to give local computed decls special treatment at the MIR level
            Item::ComputedDecl(_) => {},
        }
    }

    fn void_instr(&self) -> InstrId { self.void_instr }

    fn scope(&mut self, scope: ScopeId, ctx: Context) -> InstrId {
        let scope = &self.b.hir.scopes[scope];
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
        let id = self.b.tc.overloads[id].expect("No overload found!");
        match self.b.get_decl(id) {
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
        let id = self.b.tc.overloads[id].expect("No overload found!");
        match self.b.get_decl(id) {
            Decl::Computed { .. } => panic!("setters not yet implemented!"),
            Decl::Stored(id) => {
                assert!(arguments.is_empty());
                let location = self.stored_decl_locs[id];
                self.code.push(Instr::Store { location, value })
            },
            Decl::LocalConst { .. } | Decl::Const(_) => panic!("can't set a constant!"),
            Decl::Intrinsic(_, _) => panic!("can't set an intrinsic! (yet?)"),
            Decl::Static(statik) => {
                let location = self.code.push(Instr::AddressOfStatic(statik));
                self.code.push(Instr::Store { location, value })
            }
        }
    }

    fn modify(&mut self, arguments: SmallVec<[InstrId; 2]>, id: DeclRefId) -> InstrId {
        let id = self.b.tc.overloads[id].expect("No overload found!");
        match self.b.get_decl(id) {
            Decl::Computed { .. } => panic!("modify accessors not yet implemented!"),
            Decl::Stored(id) => {
                assert!(arguments.is_empty());
                self.stored_decl_locs[id]
            },
            Decl::LocalConst { .. } | Decl::Const(_) => panic!("can't modify a constant!"),
            Decl::Intrinsic(_, _) => panic!("can't modify an intrinsic! (yet?)"),
            Decl::Static(statik) => self.code.push(Instr::AddressOfStatic(statik)),
        }
    }

    fn expr(&mut self, expr: ExprId, mut ctx: Context) -> InstrId {
        // HACK!!!!
        let mut should_allow_set = false;

        let ty = self.type_of(expr);

        let instr = match self.b.hir.exprs[expr] {
            Expr::Void => self.void_instr(),
            Expr::IntLit { .. } | Expr::DecLit { .. } | Expr::StrLit { .. } | Expr::CharLit { .. } => {
                let konst = expr_to_const(&self.b.hir.exprs[expr], ty.clone(), &mut self.b.strings);
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
            // This isn't really a loop! It's just a control flow hack to get around the fact
            // that you can't chain `if let`s in Rust.
            Expr::DeclRef { ref arguments, id, .. } => loop {
                // Check if the declaration is an intrinsic
                let decl_id = self.b.tc.overloads[id].unwrap();
                if let hir::Decl::Intrinsic { intr, .. } = self.b.hir.decls[decl_id] {
                    // Check if we need to special case the intrinsic
                    match intr {
                        Intrinsic::LogicalAnd => break {
                            assert_eq!(ctx.indirection, 0);
                            assert_eq!(arguments.len(), 2);
                            let (lhs, rhs) = (arguments[0], arguments[1]);
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
                        Intrinsic::LogicalOr => break {
                            assert_eq!(ctx.indirection, 0);
                            assert_eq!(arguments.len(), 2);
                            let (lhs, rhs) = (arguments[0], arguments[1]);
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
                        Intrinsic::LogicalNot => break {
                            assert_eq!(arguments.len(), 1);
                            let operand = arguments[0];
                            if let DataDest::Branch(true_bb, false_bb) = ctx.data {
                                return self.expr(operand, Context::new(0, DataDest::Branch(false_bb, true_bb), ctx.control))
                            } else {
                                let operand = self.expr(operand, Context::new(0, DataDest::Read, ControlDest::Continue));
                                self.code.push(Instr::LogicalNot(operand))
                            }
                        },
                        _ => {},
                    }
                }

                // Otherwise, just handle the general case
                should_allow_set = true;
                let arguments = arguments.iter().map(|&argument|
                    self.expr(argument, Context::new(0, DataDest::Read, ControlDest::Continue))
                ).collect();

                break if ctx.indirection < 0 {
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
            Expr::Cast { expr, ty: ref dest_ty, cast_id } => match &self.b.tc.cast_methods[cast_id] {
                CastMethod::Noop => return self.expr(expr, ctx),
                CastMethod::Reinterpret => {
                    let value = self.expr(expr, Context::new(0, DataDest::Read, ControlDest::Continue));
                    self.code.push(Instr::Reinterpret(value, dest_ty.clone()))
                },
                CastMethod::Int => {
                    let (src_width, _src_is_signed, dest_width, dest_is_signed) = match (&self.b.tc.types[expr], dest_ty) {
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
            Expr::AddrOf { expr: operand, .. } => return self.expr(operand, Context::new(ctx.indirection - 1, ctx.data, ctx.control)),
            Expr::Pointer { expr, is_mut } => {
                assert_eq!(ctx.indirection, 0);
                let op = self.expr(expr, Context::new(0, DataDest::Read, ControlDest::Continue));
                self.code.push(Instr::Pointer { op, is_mut })
            },
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
