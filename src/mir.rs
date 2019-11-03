use std::collections::HashMap;
use std::ffi::CString;
use std::fmt;
use std::ops::Range;

use smallvec::SmallVec;
use string_interner::Sym;

use crate::arch::Arch;
use crate::driver::Driver;
use crate::ty::Type;
use crate::type_checker as tc;
use tc::CastMethod;
use crate::index_vec::{Idx, IdxVec};
use crate::builder::{DeclId, ExprId, DeclRefId, ScopeId, Intrinsic};
use crate::hir::{self, Expr, Item, StoredDeclId};

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
    Ty(Type),
}

impl Const {
    pub fn ty(&self) -> Type {
        match self {
            Const::Int { ty, .. } => ty.clone(),
            Const::Float { ty, .. } => ty.clone(),
            Const::Str { ty, .. } => ty.clone(),
            Const::Bool(_) => Type::Bool,
            Const::Ty(_) => Type::Ty,
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

pub struct Builder {
    decls: HashMap<DeclId, Decl>,
    static_inits: IdxVec<ExprId, StaticId>,
    pub arch: Arch,
    pub strings: IdxVec<CString, StrId>,
    pub functions: IdxVec<Function, FuncId>,
    pub statics: IdxVec<Const, StaticId>,
}

impl Builder {
    pub fn new(arch: Arch) -> Self {
        Self {
            decls: HashMap::new(),
            static_inits: IdxVec::new(),
            arch,
            strings: IdxVec::new(),
            functions: IdxVec::new(),
            statics: IdxVec::new(),
        }
    }
}

impl Builder {
    pub fn function_by_ref<'b>(&'b self, func_ref: &'b FunctionRef) -> &'b Function {
        match func_ref {
            &FunctionRef::Id(id) => &self.functions[id],
            FunctionRef::Ref(func) => func,
        }
    }

    pub fn type_of(&self, instr: InstrId, func_ref: &FunctionRef) -> Type {
        let func = self.function_by_ref(func_ref);
        match &func.code[instr] {
            Instr::Void | Instr::Store { .. } => Type::Void,
            Instr::Pointer { .. } => Type::Ty,
            Instr::Const(konst) => konst.ty(),
            Instr::Alloca(ty) => ty.clone().mut_ptr(),
            Instr::LogicalNot(_) => Type::Bool,
            &Instr::Call { func, .. } => self.functions[func].ret_ty.clone(),
            Instr::Intrinsic { ty, .. } => ty.clone(),
            Instr::Reinterpret(_, ty) | Instr::Truncate(_, ty) | Instr::SignExtend(_, ty)
                | Instr::ZeroExtend(_, ty) | Instr::FloatCast(_, ty) | Instr::FloatToInt(_, ty)
                | Instr::IntToFloat(_, ty)
                  => ty.clone(),
            &Instr::Load(instr) => match self.type_of(instr, func_ref) {
                Type::Pointer(pointee) => pointee.ty,
                _ => Type::Error,
            },
            &Instr::AddressOfStatic(statik) => self.statics[statik].ty().mut_ptr(),
            Instr::Ret(_) | Instr::Br(_) | Instr::CondBr { .. } => Type::Never,
            Instr::Parameter(ty) => ty.clone(),
        }
    }
}

impl Driver {
    pub fn build_mir(&mut self) {
        for i in 0..self.hir.decls.len() {
            self.get_decl(DeclId::new(i));
        }

        for i in 0..self.mir.static_inits.len() {
            let id = StaticId::new(i);
            let init = self.mir.static_inits[id];
            let konst = self.eval_expr(init);
            self.mir.statics.push(konst);
        }
    }

    pub fn build_standalone_expr(&mut self, expr: ExprId) -> Function {
        self.build_function(None, self.tc.types[expr].clone(), FunctionBody::Expr(expr), DeclId::new(0)..DeclId::new(0))
    }

    fn get_decl(&mut self, id: DeclId) -> Decl {
        if let Some(decl) = self.mir.decls.get(&id) { return decl.clone(); }
        match self.hir.decls[id] {
            hir::Decl::Computed { ref params, scope, .. } => {
                let get = FuncId::new(self.mir.functions.len());
                let decl = Decl::Computed { get };
                self.mir.decls.insert(id, decl.clone());
                let params = params.clone();
                let func = self.build_function(Some(self.hir.names[id]), self.decl_type(id).clone(), FunctionBody::Scope(scope), params.clone());
                self.mir.functions.push(func);
                decl
            },
            hir::Decl::Stored { id: index, .. } => {
                let decl = Decl::Stored(index);
                self.mir.decls.insert(id, decl.clone());
                decl
            },
            hir::Decl::Parameter { index } => {
                let decl = Decl::LocalConst { value: InstrId::new(index + 1) };
                self.mir.decls.insert(id, decl.clone());
                decl
            },
            hir::Decl::Intrinsic { intr, .. } => {
                let decl = Decl::Intrinsic(intr, self.decl_type(id).clone());
                self.mir.decls.insert(id, decl.clone());
                decl
            },
            hir::Decl::Static(expr) => {
                let decl = Decl::Static(StaticId::new(self.mir.static_inits.len()));
                self.mir.decls.insert(id, decl.clone());
                self.mir.static_inits.push(expr);
                decl
            },
            hir::Decl::Const(root_expr) => {
                let konst = self.eval_expr(root_expr);

                // TODO: Deal with cycles!
                let decl = Decl::Const(konst);
                self.mir.decls.insert(id, decl.clone());
                decl
            },
        }
    }

    #[allow(dead_code)]
    fn fmt_const(&self, f: &mut fmt::Formatter, konst: &Const) -> fmt::Result {
        match *konst {
            Const::Bool(val) => writeln!(f, "{}", val),
            Const::Float { lit, ref ty } => writeln!(f, "{} as {:?}", lit, ty),
            Const::Int { lit, ref ty } => writeln!(f, "{} as {:?}", lit, ty),
            Const::Str { id, ref ty } => writeln!(f, "%str{} ({:?}) as {:?}", id.idx(), self.mir.strings[id], ty),
            Const::Ty(ref ty) => writeln!(f, "`{:?}`", ty),
        }
    }

    #[allow(dead_code)]
    fn fn_name(&self, name: Option<Sym>) -> &str {
        match name {
            Some(name) => self.interner.resolve(name).unwrap(),
            None => "{anonymous}",
        }
    }

    #[allow(dead_code)]
    pub fn fmt_mir(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !self.mir.statics.raw.is_empty() {
            for (i, statik) in self.mir.statics.iter().enumerate() {
                write!(f, "%static{} = ", i)?;
                self.fmt_const(f, statik)?;
            }
            writeln!(f)?;
        }

        for (i, func) in self.mir.functions.iter().enumerate() {
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
                            write!(f, "%{} = call `{}`", i, self.fn_name(self.mir.functions[callee].name))?;
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
            if i + 1 < self.mir.functions.len() {
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

struct FunctionBuilder {
    name: Option<Sym>,
    ret_ty: Type,
    void_instr: InstrId,
    code: IdxVec<Instr, InstrId>,
    basic_blocks: IdxVec<InstrId, BasicBlockId>,
    stored_decl_locs: IdxVec<InstrId, StoredDeclId>,
}

impl FunctionBuilder {
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
}

impl Driver {
    fn build_function(&mut self, name: Option<Sym>, ret_ty: Type, body: FunctionBody, params: Range<DeclId>) -> Function {
        let mut code = IdxVec::new();
        let void_instr = code.push(Instr::Void);
        for param in params.start.idx()..params.end.idx() {
            let param = DeclId::new(param);
            if let hir::Decl::Parameter { .. } = self.hir.decls[param] {
                code.push(Instr::Parameter(self.decl_type(param).clone()));
            } else {
                panic!("unexpected non-parameter as parameter decl");
            }
        }
        let mut basic_blocks = IdxVec::new();
        basic_blocks.push(InstrId::new(code.len()));
        let mut b = FunctionBuilder {
            name,
            ret_ty,
            void_instr,
            code,
            basic_blocks,
            stored_decl_locs: IdxVec::new(),
        };
        let ctx = Context::new(0, DataDest::Ret, ControlDest::Unreachable);
        match body {
            FunctionBody::Expr(expr) => self.build_expr(&mut b, expr, ctx),
            FunctionBody::Scope(scope) => self.build_scope(&mut b, scope, ctx),
        };
        Function {
            name: b.name,
            ret_ty: b.ret_ty,
            code: b.code,
            basic_blocks: b.basic_blocks,
        }
    }

    fn type_of(&self, expr: ExprId) -> Type {
        self.tc.types[expr].clone()
    }

    fn build_item(&mut self, b: &mut FunctionBuilder, item: Item) {
        match item {
            Item::Stmt(expr) => {
                self.build_expr(b, expr, Context::new(0, DataDest::Void, ControlDest::Continue));
            },
            Item::StoredDecl { id, root_expr, .. } => {
                let ty = self.type_of(root_expr);
                let location = b.code.push(Instr::Alloca(ty));
                assert_eq!(b.stored_decl_locs.len(), id.idx());
                b.stored_decl_locs.push(location);
                self.build_expr(b, root_expr, Context::new(0, DataDest::Store { location }, ControlDest::Continue));
            },
            // No need to give local computed decls special treatment at the MIR level
            Item::ComputedDecl(_) => {},
        }
    }

    fn build_scope(&mut self, b: &mut FunctionBuilder, scope: ScopeId, ctx: Context) -> InstrId {
        for i in 0..self.hir.scopes[scope].items.len() {
            let item = self.hir.scopes[scope].items[i];
            self.build_item(b, item);
        }
        self.build_expr(b, self.hir.scopes[scope].terminal_expr, ctx)
    }

    fn get(&mut self, b: &mut FunctionBuilder, arguments: SmallVec<[InstrId; 2]>, id: DeclRefId) -> InstrId {
        let id = self.tc.overloads[id].expect("No overload found!");
        match self.get_decl(id) {
            Decl::Computed { get } => b.code.push(Instr::Call { arguments, func: get }),
            Decl::Stored(id) => {
                assert!(arguments.is_empty());
                let location = b.stored_decl_locs[id];
                b.code.push(Instr::Load(location))
            },
            Decl::LocalConst { value } => value,
            Decl::Intrinsic(intr, ref ty) => {
                b.code.push(Instr::Intrinsic { arguments, ty: ty.clone(), intr })
            },
            Decl::Const(ref konst) => b.code.push(Instr::Const(konst.clone())),
            Decl::Static(statik) => {
                let location = b.code.push(Instr::AddressOfStatic(statik));
                b.code.push(Instr::Load(location))
            },
        }
    }

    fn set(&mut self, b: &mut FunctionBuilder, arguments: SmallVec<[InstrId; 2]>, id: DeclRefId, value: InstrId) -> InstrId {
        let id = self.tc.overloads[id].expect("No overload found!");
        match self.get_decl(id) {
            Decl::Computed { .. } => panic!("setters not yet implemented!"),
            Decl::Stored(id) => {
                assert!(arguments.is_empty());
                let location = b.stored_decl_locs[id];
                b.code.push(Instr::Store { location, value })
            },
            Decl::LocalConst { .. } | Decl::Const(_) => panic!("can't set a constant!"),
            Decl::Intrinsic(_, _) => panic!("can't set an intrinsic! (yet?)"),
            Decl::Static(statik) => {
                let location = b.code.push(Instr::AddressOfStatic(statik));
                b.code.push(Instr::Store { location, value })
            }
        }
    }

    fn modify(&mut self, b: &mut FunctionBuilder, arguments: SmallVec<[InstrId; 2]>, id: DeclRefId) -> InstrId {
        let id = self.tc.overloads[id].expect("No overload found!");
        match self.get_decl(id) {
            Decl::Computed { .. } => panic!("modify accessors not yet implemented!"),
            Decl::Stored(id) => {
                assert!(arguments.is_empty());
                b.stored_decl_locs[id]
            },
            Decl::LocalConst { .. } | Decl::Const(_) => panic!("can't modify a constant!"),
            Decl::Intrinsic(_, _) => panic!("can't modify an intrinsic! (yet?)"),
            Decl::Static(statik) => b.code.push(Instr::AddressOfStatic(statik)),
        }
    }

    fn build_expr(&mut self, b: &mut FunctionBuilder, expr: ExprId, mut ctx: Context) -> InstrId {
        // HACK!!!!
        let mut should_allow_set = false;

        let ty = self.type_of(expr);

        let instr = match self.hir.exprs[expr] {
            Expr::Void => b.void_instr,
            Expr::IntLit { .. } | Expr::DecLit { .. } | Expr::StrLit { .. } | Expr::CharLit { .. } => {
                let konst = expr_to_const(&self.hir.exprs[expr], ty.clone(), &mut self.mir.strings);
                b.code.push(Instr::Const(konst))
            },
            Expr::Set { lhs, rhs } => {
                self.build_expr(
                    b,
                    rhs,
                    Context::new(0, DataDest::Set { dest: lhs }, ctx.control.clone()),
                );
                // Because we override the data destination above, we need to handle it ourselves
                return match ctx.data {
                    DataDest::Ret => b.code.push(Instr::Ret(b.void_instr)),
                    _ => b.void_instr,
                };
            },
            // This isn't really a loop! It's just a control flow hack to get around the fact
            // that you can't chain `if let`s in Rust.
            Expr::DeclRef { ref arguments, id, .. } => loop {
                // Check if the declaration is an intrinsic
                let decl_id = self.tc.overloads[id].unwrap();
                if let hir::Decl::Intrinsic { intr, .. } = self.hir.decls[decl_id] {
                    // Check if we need to special case the intrinsic
                    match intr {
                        Intrinsic::LogicalAnd => break {
                            assert_eq!(ctx.indirection, 0);
                            assert_eq!(arguments.len(), 2);
                            let (lhs, rhs) = (arguments[0], arguments[1]);
                            let left_true_bb = b.new_bb();
                            let location = if let DataDest::Read = ctx.data {
                                Some(b.code.push(Instr::Alloca(ty.clone())))
                            } else {
                                None
                            };
                            if let DataDest::Branch(true_bb, false_bb) = ctx.data {
                                self.build_expr(
                                    b,
                                    lhs,
                                    Context::new(0, DataDest::Branch(left_true_bb, false_bb), ControlDest::Continue),
                                );

                                b.begin_bb(left_true_bb);
                                return self.build_expr(
                                    b,
                                    rhs,
                                    Context::new(0, DataDest::Branch(true_bb, false_bb), ControlDest::Continue),
                                );
                            } else {
                                let left_false_bb = b.new_bb();
                                let after_bb = b.new_bb();
                                self.build_expr(
                                    b,
                                    lhs,
                                    Context::new(0, DataDest::Branch(left_true_bb, left_false_bb), ControlDest::Continue),
                                );

                                b.begin_bb(left_true_bb);
                                // No further branching required, because (true && foo) <=> foo
                                let branch_ctx = ctx.redirect(location, Some(after_bb));
                                self.build_expr(b, rhs, branch_ctx.clone());

                                b.begin_bb(left_false_bb);
                                let false_val = b.code.push(Instr::Const(Const::Bool(false)));
                                self.handle_context(b, false_val, expr, Type::Bool, branch_ctx, false);

                                b.begin_bb(after_bb);
                                if let Some(location) = location {
                                    b.code.push(Instr::Load(location))
                                } else {
                                    return b.void_instr
                                }
                            }
                        },
                        Intrinsic::LogicalOr => break {
                            assert_eq!(ctx.indirection, 0);
                            assert_eq!(arguments.len(), 2);
                            let (lhs, rhs) = (arguments[0], arguments[1]);
                            let left_false_bb = b.new_bb();
                            if let DataDest::Branch(true_bb, false_bb) = ctx.data {
                                self.build_expr(
                                    b,
                                    lhs,
                                    Context::new(0, DataDest::Branch(true_bb, left_false_bb), ControlDest::Continue),
                                );

                                b.begin_bb(left_false_bb);
                                return self.build_expr(
                                    b,
                                    rhs,
                                    Context::new(0, DataDest::Branch(true_bb, false_bb), ControlDest::Continue),
                                );
                            } else {
                                let left_true_bb = b.new_bb();
                                let after_bb = b.new_bb();
                                let location = if let DataDest::Read = ctx.data {
                                    Some(b.code.push(Instr::Alloca(ty.clone())))
                                } else {
                                    None
                                };
                                self.build_expr(
                                    b,
                                    lhs,
                                    Context::new(0, DataDest::Branch(left_true_bb, left_false_bb), ControlDest::Continue),
                                );

                                b.begin_bb(left_true_bb);
                                let true_val = b.code.push(Instr::Const(Const::Bool(true)));
                                let branch_ctx = ctx.redirect(location, Some(after_bb));
                                self.handle_context(b, true_val, expr, Type::Bool, branch_ctx.clone(), false);

                                b.begin_bb(left_false_bb);
                                self.build_expr(b, rhs, branch_ctx);

                                b.begin_bb(after_bb);
                                if let Some(location) = location {
                                    b.code.push(Instr::Load(location))
                                } else {
                                    return b.void_instr
                                }
                            }
                        },
                        Intrinsic::LogicalNot => break {
                            assert_eq!(arguments.len(), 1);
                            let operand = arguments[0];
                            if let DataDest::Branch(true_bb, false_bb) = ctx.data {
                                return self.build_expr(b, operand, Context::new(0, DataDest::Branch(false_bb, true_bb), ctx.control))
                            } else {
                                let operand = self.build_expr(b, operand, Context::new(0, DataDest::Read, ControlDest::Continue));
                                b.code.push(Instr::LogicalNot(operand))
                            }
                        },
                        _ => {},
                    }
                }

                // Otherwise, just handle the general case
                should_allow_set = true;
                // TODO: Don't clone arguments
                let arguments = arguments.clone().iter().map(|&argument|
                    self.build_expr(b, argument, Context::new(0, DataDest::Read, ControlDest::Continue))
                ).collect();

                break if ctx.indirection < 0 {
                    ctx.indirection += 1;
                    self.modify(b, arguments, id)
                } else {
                    match ctx.data {
                        DataDest::Receive { value, .. } => if ctx.indirection > 0 {
                            let mut location = self.get(b, arguments, id);
                            location = self.handle_indirection(b, location, ty.clone(), ctx.indirection, 1);
                            ctx.indirection = 0;
                            b.code.push(Instr::Store { location, value })
                        } else {
                            self.set(b, arguments, id, value)
                        },
                        _ => self.get(b, arguments, id),
                    }
                }
            },
            Expr::Cast { expr, ty: dest_ty, cast_id } => {
                let dest_ty = self.tc.get_evaluated_type(dest_ty).clone();
                match &self.tc.cast_methods[cast_id] {
                    CastMethod::Noop => return self.build_expr(b, expr, ctx),
                    CastMethod::Reinterpret => {
                        let value = self.build_expr(b, expr, Context::new(0, DataDest::Read, ControlDest::Continue));
                        b.code.push(Instr::Reinterpret(value, dest_ty))
                    },
                    CastMethod::Int => {
                        let (src_width, _src_is_signed, dest_width, dest_is_signed) = match (&self.tc.types[expr], &dest_ty) {
                            (&Type::Int { width: ref src_width, is_signed: src_is_signed }, &Type::Int { width: ref dest_width, is_signed: dest_is_signed })
                                => (src_width.clone(), src_is_signed, dest_width.clone(), dest_is_signed),
                            _ => panic!("Internal compiler error: found invalid cast types while generating MIR")
                        };
                        let (src_bit_width, dest_bit_width) = (src_width.bit_width(self.mir.arch), dest_width.bit_width(self.mir.arch));
                        let value = self.build_expr(b, expr, Context::new(0, DataDest::Read, ControlDest::Continue));

                        if src_bit_width == dest_bit_width {
                            // TODO: Bounds checking
                            b.code.push(Instr::Reinterpret(value, dest_ty))
                        } else if src_bit_width < dest_bit_width {
                            if dest_is_signed {
                                // TODO: Bounds checking
                                b.code.push(Instr::SignExtend(value, dest_ty))
                            } else {
                                // TODO: Bounds checking
                                b.code.push(Instr::ZeroExtend(value, dest_ty))
                            }
                        } else if src_bit_width > dest_bit_width {
                            // TODO: Bounds checking
                            b.code.push(Instr::Truncate(value, dest_ty))
                        } else {
                            unreachable!()
                        }
                    },
                    CastMethod::Float => {
                        let value = self.build_expr(b, expr, Context::new(0, DataDest::Read, ControlDest::Continue));
                        b.code.push(Instr::FloatCast(value, dest_ty))
                    },
                    CastMethod::FloatToInt => {
                        let value = self.build_expr(b, expr, Context::new(0, DataDest::Read, ControlDest::Continue));
                        b.code.push(Instr::FloatToInt(value, dest_ty))
                    },
                    CastMethod::IntToFloat => {
                        let value = self.build_expr(b, expr, Context::new(0, DataDest::Read, ControlDest::Continue));
                        b.code.push(Instr::IntToFloat(value, dest_ty))
                    },
                }
            },
            Expr::AddrOf { expr: operand, .. } => return self.build_expr(b, operand, Context::new(ctx.indirection - 1, ctx.data, ctx.control)),
            Expr::Pointer { expr, is_mut } => {
                assert_eq!(ctx.indirection, 0);
                let op = self.build_expr(b, expr, Context::new(0, DataDest::Read, ControlDest::Continue));
                b.code.push(Instr::Pointer { op, is_mut })
            },
            Expr::Deref(operand) => return self.build_expr(b, operand, Context::new(ctx.indirection + 1, ctx.data, ctx.control)),
            Expr::Do { scope } => return self.build_scope(b, scope, ctx),
            Expr::If { condition, then_scope, else_scope } => {
                let true_bb = b.new_bb();
                let false_bb = b.new_bb();
                let post_bb = if else_scope.is_some() {
                    b.new_bb()
                } else {
                    false_bb
                };

                let result_location = match (&ctx.data, else_scope) {
                    (DataDest::Read, Some(_)) => Some(
                        // TODO: this will be the wrong type if indirection != 0
                        b.code.push(Instr::Alloca(ty.clone()))
                    ),
                    _ => None,
                };
                self.build_expr(
                    b,
                    condition,
                    Context::new(0, DataDest::Branch(true_bb, false_bb), ControlDest::Continue),
                );
                b.begin_bb(true_bb);
                let scope_ctx = ctx.redirect(result_location, Some(post_bb));
                self.build_scope(b, then_scope, scope_ctx.clone());
                if let Some(else_scope) = else_scope {
                    b.begin_bb(false_bb);
                    self.build_scope(b, else_scope, scope_ctx);
                }

                b.begin_bb(post_bb);
                if let Some(location) = result_location {
                    return b.code.push(Instr::Load(location))
                } else if else_scope.is_some() {
                    return self.handle_control(b, b.void_instr, ctx.control)
                } else {
                    b.void_instr
                }
            },
            Expr::While { condition, scope } => {
                assert_eq!(ctx.indirection, 0);
                let test_bb = b.new_bb();
                let loop_bb = b.new_bb();
                let post_bb = match ctx.control {
                    ControlDest::Continue | ControlDest::Unreachable => b.new_bb(),
                    ControlDest::Block(block) => block,
                };

                b.code.push(Instr::Br(test_bb));
                b.begin_bb(test_bb);
                self.build_expr(b, condition, Context::new(0, DataDest::Branch(loop_bb, post_bb), ControlDest::Continue));

                b.begin_bb(loop_bb);
                let val = self.build_scope(b, scope, Context::new(0, DataDest::Void, ControlDest::Block(test_bb)));

                match ctx.control {
                    ControlDest::Continue | ControlDest::Unreachable => {
                        b.begin_bb(post_bb);
                        val
                    },
                    ControlDest::Block(_) => return val,
                }
            },
            Expr::Ret { expr } => {
                return self.build_expr(
                    b,
                    expr,
                    Context::new(0, DataDest::Ret, ctx.control),
                );
            }
        };
        
        self.handle_context(b, instr, expr, ty, ctx, should_allow_set)
    }

    fn handle_indirection(&mut self, b: &mut FunctionBuilder, mut instr: InstrId, ty: Type, mut indirection: i8, target: i8) -> InstrId {
        if indirection > target {
            while indirection > target {
                instr = b.code.push(Instr::Load(instr));
                indirection -= 1;
            }
        } else if indirection < target {
            while indirection < target {
                // TODO: this will be the wrong type if indirection != 0
                let location = b.code.push(Instr::Alloca(ty.clone()));
                b.code.push(Instr::Store { location, value: instr });
                instr = location;
                indirection += 1;
            }
        }
        instr
    }

    fn handle_control(&mut self, b: &mut FunctionBuilder, instr: InstrId, control: ControlDest) -> InstrId {
        match control {
            ControlDest::Block(block) => b.code.push(Instr::Br(block)),
            ControlDest::Continue => instr,
            ControlDest::Unreachable => b.void_instr,
        }
    }

    fn handle_context(&mut self, b: &mut FunctionBuilder, instr: InstrId, expr: ExprId, ty: Type, ctx: Context, should_allow_set: bool) -> InstrId {
        let instr = self.handle_indirection(b, instr, ty, ctx.indirection, 0);
        match ctx.data {
            DataDest::Read => return instr,
            DataDest::Ret => return b.code.push(Instr::Ret(instr)),
            DataDest::Branch(true_bb, false_bb)
                => return b.code.push(Instr::CondBr { condition: instr, true_bb, false_bb }),
            DataDest::Receive { .. } => {
                assert!(should_allow_set, "can't set constant expression!");
            },
            DataDest::Store { location } => {
                b.code.push(Instr::Store { location, value: instr });
            },
            DataDest::Set { dest } => {
                return self.build_expr(
                    b,
                    dest,
                    Context::new(0, DataDest::Receive { value: instr, expr }, ctx.control.clone()),
                );
            }
            DataDest::Void => {},
        }

        self.handle_control(b, instr, ctx.control)
    }
}
