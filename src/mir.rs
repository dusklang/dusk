use std::collections::HashMap;
use std::ffi::CString;
use std::fmt;
use std::ops::Range;

use smallvec::SmallVec;
use string_interner::Sym;

use crate::arch::Arch;
use crate::driver::Driver;
use crate::ty::{Type, FloatWidth};
use crate::typechecker as tc;
use tc::type_provider::TypeProvider;
use tc::CastMethod;
use crate::index_vec::{Idx, IdxVec};
use crate::builder::{DeclId, ExprId, ModScopeId, DeclRefId, ImperScopeId, StructId, Intrinsic};
use crate::hir::{self, Expr, ScopeItem, StoredDeclId};

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
    Mod(ModScopeId),
}

impl Const {
    pub fn ty(&self) -> Type {
        match self {
            Const::Int { ty, .. } => ty.clone(),
            Const::Float { ty, .. } => ty.clone(),
            Const::Str { ty, .. } => ty.clone(),
            Const::Bool(_) => Type::Bool,
            Const::Ty(_) => Type::Ty,
            Const::Mod(_) => Type::Mod,
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
    Struct { fields: SmallVec<[InstrId; 2]>, id: StructId },
    DirectFieldAccess { val: InstrId, index: usize },
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
    Field { index: usize },
}

#[derive(Debug, Default)]
pub struct Function {
    pub name: Option<Sym>,
    pub ret_ty: Type,
    pub code: IdxVec<InstrId, Instr>,
    pub basic_blocks: IdxVec<BasicBlockId, InstrId>,
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
#[derive(Copy, Clone, Debug)]
enum DataDest {
    /// This value needs to be returned from the current function
    Ret,
    /// A particular value needs to be assigned to this value
    Receive { value: InstrId },
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

#[derive(Copy, Clone)]
struct Value {
    instr: InstrId,
    /// The number of pointer hops the value is away from instr.
    ///     Positive values => number of layers of indirection
    ///     Negative values => number of times the pointer has been dereferenced
    indirection: i8,
}

impl Value {
    fn get_address(self) -> Value {
        Value {
            instr: self.instr,
            // It might seem counterintuitive that we are subtracting here.
            // It's because when we get the address, the type also changes.
            // So we're at the same level of indirection from the original value,
            // but we're closer to the pointer value!
            indirection: self.indirection - 1
        }
    }

    fn adjusted(self, indirection: i8) -> Value {
        Value {
            instr: self.instr,
            indirection: self.indirection - indirection,
        }
    }
}

impl InstrId {
    fn direct(self) -> Value {
        Value {
            instr: self,
            indirection: 0,
        }
    }

    fn indirect(self) -> Value {
        Value {
            instr: self,
            indirection: 1,
        }
    }
}

/// Where to go after the current value is computed (whether implicitly or explicitly, such as via a `break` in a loop)
#[derive(Copy, Clone, Debug)]
enum ControlDest {
    Continue,
    Unreachable,
    Block(BasicBlockId),
}

#[derive(Copy, Clone, Debug)]
struct Context {
    /// Relative to a Value indirection of 0, a:
    ///   - positive indirection means I need to move the value further away from me
    ///   - negative indirection means I need to get closer to the value
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

impl Driver {
    fn expr_to_const(&mut self, expr: ExprId, ty: Type) -> Const {
        match self.hir.exprs[expr] {
            Expr::IntLit { lit } => {
                match ty {
                    Type::Int { .. } => Const::Int { lit, ty },
                    Type::Float(_)   => Const::Float { lit: lit as f64, ty },
                    _ => panic!("Unrecognized integer literal type {:?}", ty),
                }
            },
            Expr::DecLit { lit } => Const::Float { lit, ty },
            Expr::StrLit { ref lit } => {
                let id = self.mir.strings.push(lit.clone());
                Const::Str { id, ty }
            },
            Expr::CharLit { lit } => match ty {
                Type::Int { .. } => Const::Int { lit: lit as u64, ty },
                Type::Pointer(_) => {
                    let id = self.mir.strings.push(CString::new([lit as u8].as_ref()).unwrap());
                    Const::Str { id, ty }
                },
                _ => panic!("unexpected type for character")
            },
            Expr::ConstTy(ref ty) => Const::Ty(ty.clone()),
            Expr::Mod { id } => Const::Mod(id),
            Expr::Import { file } => Const::Mod(self.hir.global_scopes[file]),
            _ => panic!("Cannot convert expression to constant: {:#?}", expr),
        }
    }
}

pub enum FunctionRef {
    Id(FuncId),
    Ref(Function),
}

pub struct Struct {
    pub field_tys: SmallVec<[Type; 2]>,
    pub layout: StructLayout,
}

pub struct StructLayout {
    pub field_offsets: SmallVec<[usize; 2]>,
    pub alignment: usize,
    pub size: usize,
    pub stride: usize,
}

pub struct Builder {
    decls: HashMap<DeclId, Decl>,
    static_inits: IdxVec<StaticId, ExprId>,
    pub arch: Arch,
    pub strings: IdxVec<StrId, CString>,
    pub functions: IdxVec<FuncId, Function>,
    pub statics: IdxVec<StaticId, Const>,
    pub structs: HashMap<StructId, Struct>,
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
            structs: HashMap::new(),
        }
    }
}

fn type_of(b: &Builder, instr: InstrId, code: &IdxVec<InstrId, Instr>) -> Type {
    match &code[instr] {
        Instr::Void | Instr::Store { .. } => Type::Void,
        Instr::Pointer { .. } | Instr::Struct { .. } => Type::Ty,
        Instr::Const(konst) => konst.ty(),
        Instr::Alloca(ty) => ty.clone().mut_ptr(),
        Instr::LogicalNot(_) => Type::Bool,
        &Instr::Call { func, .. } => b.functions[func].ret_ty.clone(),
        Instr::Intrinsic { ty, .. } => ty.clone(),
        Instr::Reinterpret(_, ty) | Instr::Truncate(_, ty) | Instr::SignExtend(_, ty)
            | Instr::ZeroExtend(_, ty) | Instr::FloatCast(_, ty) | Instr::FloatToInt(_, ty)
            | Instr::IntToFloat(_, ty)
              => ty.clone(),
        &Instr::Load(instr) => match type_of(b, instr, code) {
            Type::Pointer(pointee) => pointee.ty,
            _ => Type::Error,
        },
        &Instr::AddressOfStatic(statik) => b.statics[statik].ty().mut_ptr(),
        Instr::Ret(_) | Instr::Br(_) | Instr::CondBr { .. } => Type::Never,
        Instr::Parameter(ty) => ty.clone(),
        &Instr::DirectFieldAccess { val, index } => {
            let base_ty = type_of(b, val, code);
            match base_ty {
                Type::Struct(strukt) => b.structs[&strukt].field_tys[index].clone(),
                _ => panic!("Cannot directly access field of non-struct type {:?}!", base_ty),
            }
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
        type_of(self, instr, &func.code)
    }

    /// Size of an instance of a type in bytes
    pub fn size_of(&self, ty: &Type) -> usize {
        let arch = self.arch;
        match ty {
            Type::Error | Type::Void | Type::Never | Type::Ty | Type::Mod => 0,
            Type::Int { width, .. } => {
                let bit_width = width.bit_width(arch);
                assert_eq!(bit_width % 8, 0, "Unexpected bit width: not a multiple of eight!");
                bit_width / 8
            },
            Type::Float(width) => match width {
                FloatWidth::W32 => 32 / 8,
                FloatWidth::W64 => 64 / 8,
            },
            Type::Pointer(_) => {
                let bit_width = arch.pointer_size();
                assert_eq!(bit_width % 8, 0, "Unexpected bit width: not a multiple of eight!");
                bit_width / 8
            },
            Type::Bool => 1,
            &Type::Struct(id) => self.structs[&id].layout.size,
        }
    }

    /// Stride of an instance of a type in bytes
    pub fn stride_of(&self, ty: &Type) -> usize {
        match *ty {
            Type::Struct(id) => self.structs[&id].layout.stride,
            // Otherwise, stride == size
            _ => self.size_of(ty),
        }
    }

    /// Minimum alignment of an instance of a type in bytes
    pub fn align_of(&self, ty: &Type) -> usize {
        match *ty {
            Type::Struct(id) => self.structs[&id].layout.alignment,
            // Otherwise, alignment == size
            _ => self.size_of(ty),
        }
    }

    /// Compute the layout (field offsets, alignment, size, and stride) for a struct
    pub fn layout_struct(&self, field_tys: &[Type]) -> StructLayout {
        // Get max alignment of all the fields.
        let alignment = field_tys.iter()
            .map(|ty| self.align_of(ty))
            .max()
            .unwrap_or(0);

        fn next_multiple_of(n: usize, fac: usize) -> usize {
            match fac {
                0 => n,
                _ => {
                    let fac_minus_1 = fac - 1;
                    (n + fac_minus_1) - ((n + fac_minus_1) % fac)
                }
            }
        }
        let mut field_offsets = SmallVec::new();
        let mut last_size = 0;
        if !field_tys.is_empty() {
            field_offsets.push(0);
            last_size = self.size_of(&field_tys[0]);
        }
        for i in 1..field_tys.len() {
            let prev_field_end = field_offsets[i - 1] + last_size;
            field_offsets.push(
                next_multiple_of(
                    prev_field_end,
                    self.align_of(&field_tys[i])
                )
            );
            last_size = self.size_of(&field_tys[i]);
        }
        let size = field_offsets.last().copied().unwrap_or(0) + last_size;
        let stride = next_multiple_of(size, alignment);

        StructLayout { field_offsets, alignment, size, stride }
    }
}

impl Driver {
    pub fn build_mir(&mut self, tp: &impl TypeProvider) {
        for i in 0..self.hir.decls.len() {
            self.get_decl(DeclId::new(i), tp);
        }

        for i in 0..self.mir.static_inits.len() {
            let id = StaticId::new(i);
            let init = self.mir.static_inits[id];
            let konst = self.eval_expr(init, tp);
            self.mir.statics.push(konst);
        }
    }

    pub fn build_standalone_expr(&mut self, expr: ExprId, tp: &impl TypeProvider) -> Function {
        self.build_function(None, tp.ty(expr).clone(), FunctionBody::Expr(expr), DeclId::new(0)..DeclId::new(0), tp)
    }

    fn get_decl(&mut self, id: DeclId, tp: &impl TypeProvider) -> Decl {
        if let Some(decl) = self.mir.decls.get(&id) { return decl.clone(); }
        match self.hir.decls[id] {
            hir::Decl::Computed { ref params, scope, .. } => {
                // Add placeholder function to reserve ID ahead of time
                let get = self.mir.functions.push(Function::default());
                let decl = Decl::Computed { get };
                self.mir.decls.insert(id, decl.clone());
                let params = params.clone();
                let func = self.build_function(
                    Some(self.hir.names[id]),
                    self.decl_type(id, tp).clone(),
                    FunctionBody::Scope(scope),
                    params.clone(),
                    tp
                );
                self.mir.functions[get] = func;
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
                let decl = Decl::Intrinsic(intr, self.decl_type(id, tp).clone());
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
                let konst = self.eval_expr(root_expr, tp);

                // TODO: Deal with cycles!
                let decl = Decl::Const(konst);
                self.mir.decls.insert(id, decl.clone());
                decl
            },
            hir::Decl::Field(field_id) => {
                let index = self.hir.field_decls[field_id].index;
                let decl = Decl::Field { index };
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
            Const::Mod(id) => writeln!(f, "%mod{}", id.idx()),
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
    pub fn display_mir<'a>(&'a self) -> impl fmt::Display + 'a {
        MirDisplay(self)
    }

    fn display_mir_impl(&self, f: &mut fmt::Formatter) -> fmt::Result {
        if !self.mir.statics.raw.is_empty() {
            for (i, statik) in self.mir.statics.iter().enumerate() {
                write!(f, "%static{} = ", i)?;
                self.fmt_const(f, statik)?;
            }
            writeln!(f)?;
        }

        for (i, func) in self.mir.functions.iter().enumerate() {
            write!(f, "fn {}(", self.fn_name(func.name))?;
            assert_eq!(&func.code.raw[0], &Instr::Void);
            let mut first = true;
            for i in 1..func.code.len() {
                if let Instr::Parameter(ty) = &func.code.raw[i] {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "%{}: {:?}", i, ty)?;
                } else {
                    break;
                }
            }
            writeln!(f, "): {:?} {{", func.ret_ty)?;
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
                        Instr::Struct { fields, .. } => {
                            write!(f, "%{} = struct {{ ", i)?;
                            for i in 0..fields.len() {
                                write!(f, "%{}", fields[i].idx())?;
                                if i < (fields.len() - 1) {
                                    write!(f, ",")?;
                                }
                                write!(f, " ")?;
                            }
                            writeln!(f, "}}")?;
                        },
                        &Instr::DirectFieldAccess { val, index } => writeln!(f, "%{} = %{}.field{}", i, val.idx(), index)?,
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

#[must_use]
struct MirDisplay<'a>(&'a Driver);

impl fmt::Display for MirDisplay<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        self.0.display_mir_impl(f)
    }
}

#[derive(Copy, Clone)]
enum FunctionBody {
    Scope(ImperScopeId),
    Expr(ExprId),
}

struct FunctionBuilder {
    name: Option<Sym>,
    ret_ty: Type,
    void_instr: InstrId,
    code: IdxVec<InstrId, Instr>,
    basic_blocks: IdxVec<BasicBlockId, InstrId>,
    stored_decl_locs: IdxVec<StoredDeclId, InstrId>,
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

    fn type_of(&self, b: &Builder, instr: InstrId) -> Type {
        type_of(b, instr, &self.code)
    }
}

impl Driver {
    fn build_function(&mut self, name: Option<Sym>, ret_ty: Type, body: FunctionBody, params: Range<DeclId>, tp: &impl TypeProvider) -> Function {
        let mut code = IdxVec::new();
        let void_instr = code.push(Instr::Void);
        for param in params.start.idx()..params.end.idx() {
            let param = DeclId::new(param);
            if let hir::Decl::Parameter { .. } = self.hir.decls[param] {
                code.push(Instr::Parameter(self.decl_type(param, tp).clone()));
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
            FunctionBody::Expr(expr) => self.build_expr(&mut b, expr, ctx, tp),
            FunctionBody::Scope(scope) => self.build_scope(&mut b, scope, ctx, tp),
        };
        Function {
            name: b.name,
            ret_ty: b.ret_ty,
            code: b.code,
            basic_blocks: b.basic_blocks,
        }
    }

    fn build_scope_item(&mut self, b: &mut FunctionBuilder, item: ScopeItem, tp: &impl TypeProvider) {
        match item {
            ScopeItem::Stmt(expr) => {
                self.build_expr(b, expr, Context::new(0, DataDest::Void, ControlDest::Continue), tp);
            },
            ScopeItem::StoredDecl { id, root_expr, .. } => {
                let ty = tp.ty(root_expr).clone();
                let location = b.code.push(Instr::Alloca(ty));
                assert_eq!(b.stored_decl_locs.len(), id.idx());
                b.stored_decl_locs.push(location);
                self.build_expr(b, root_expr, Context::new(0, DataDest::Store { location }, ControlDest::Continue), tp);
            },
            // No need to give local computed decls special treatment at the MIR level
            ScopeItem::ComputedDecl(_) => {},
        }
    }

    fn build_scope(&mut self, b: &mut FunctionBuilder, scope: ImperScopeId, ctx: Context, tp: &impl TypeProvider) -> Value {
        for i in 0..self.hir.imper_scopes[scope].items.len() {
            let item = self.hir.imper_scopes[scope].items[i];
            self.build_scope_item(b, item, tp);
        }
        self.build_expr(b, self.hir.imper_scopes[scope].terminal_expr, ctx, tp)
    }

    fn get_base(&self, id: DeclRefId) -> ExprId {
        match self.hir.decl_refs[id].namespace {
            hir::Namespace::MemberRef { base_expr } => base_expr,
            _ => panic!("Expected member ref expression"),
        }
    }

    fn get(&mut self, b: &mut FunctionBuilder, arguments: SmallVec<[InstrId; 2]>, decl_ref_id: DeclRefId, tp: &impl TypeProvider) -> Value {
        let id = tp.selected_overload(decl_ref_id).expect("No overload found!");
        match self.get_decl(id, tp) {
            Decl::Computed { get } => b.code.push(Instr::Call { arguments, func: get }).direct(),
            Decl::Stored(id) => {
                assert!(arguments.is_empty());
                b.stored_decl_locs[id].indirect()
            },
            Decl::LocalConst { value } => value.direct(),
            Decl::Intrinsic(intr, ref ty) => {
                b.code.push(Instr::Intrinsic { arguments, ty: ty.clone(), intr }).direct()
            },
            Decl::Const(ref konst) => b.code.push(Instr::Const(konst.clone())).direct(),
            Decl::Static(statik) => {
                b.code.push(Instr::AddressOfStatic(statik)).indirect()
            },
            Decl::Field { index } => {
                let base = self.get_base(decl_ref_id);
                let base = self.build_expr(b, base, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                if base.indirection > 0 {
                    // TODO: handle indirect struct field access
                    //let base_ptr = self.handle_indirection(b, base.get_address());
                    let base = self.handle_indirection(b, base);
                    b.code.push(Instr::DirectFieldAccess { val: base, index }).direct()
                } else {
                    debug_assert_eq!(base.indirection, 0, "tried to dereference a struct?!");
                    b.code.push(Instr::DirectFieldAccess { val: base.instr, index }).direct()   
                }
            },
        }
    }

    fn set(&mut self, b: &mut FunctionBuilder, arguments: SmallVec<[InstrId; 2]>, id: DeclRefId, value: InstrId, tp: &impl TypeProvider) -> InstrId {
        let id = tp.selected_overload(id).expect("No overload found!");
        match self.get_decl(id, tp) {
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
            },
            Decl::Field { .. } => panic!("Unhandled struct field!"),
        }
    }

    fn modify(&mut self, b: &mut FunctionBuilder, arguments: SmallVec<[InstrId; 2]>, id: DeclRefId, tp: &impl TypeProvider) -> Value {
        let id = tp.selected_overload(id).expect("No overload found!");
        match self.get_decl(id, tp) {
            Decl::Computed { .. } => panic!("modify accessors not yet implemented!"),
            Decl::Stored(id) => {
                assert!(arguments.is_empty());
                b.stored_decl_locs[id].indirect()
            },
            Decl::LocalConst { .. } | Decl::Const(_) => panic!("can't modify a constant!"),
            Decl::Intrinsic(_, _) => panic!("can't modify an intrinsic! (yet?)"),
            Decl::Static(statik) => b.code.push(Instr::AddressOfStatic(statik)).indirect(),
            Decl::Field { .. } => panic!("Unhandled struct field!"),
        }
    }

    fn build_expr(&mut self, b: &mut FunctionBuilder, expr: ExprId, ctx: Context, tp: &impl TypeProvider) -> Value {
        // HACK!!!!
        let mut should_allow_set = false;

        let ty = tp.ty(expr).clone();

        let val = match self.hir.exprs[expr] {
            Expr::Void => b.void_instr.direct(),
            Expr::IntLit { .. } | Expr::DecLit { .. } | Expr::StrLit { .. } | Expr::CharLit { .. } | Expr::ConstTy(_) | Expr::Mod { .. } | Expr::Import { .. } => {
                let konst = self.expr_to_const(expr, ty.clone());
                b.code.push(Instr::Const(konst)).direct()
            },
            Expr::Set { lhs, rhs } => {
                self.build_expr(
                    b,
                    rhs,
                    Context::new(ctx.indirection, DataDest::Set { dest: lhs }, ctx.control),
                    tp,
                )
            },
            // This isn't really a loop! It's just a control flow hack to get around the fact
            // that you can't chain `if let`s in Rust.
            Expr::DeclRef { ref arguments, id } => loop {
                let decl_id = tp.selected_overload(id).unwrap();

                // Check if the declaration is an intrinsic
                if let hir::Decl::Intrinsic { intr, .. } = self.hir.decls[decl_id] {
                    // Check if we need to special case the intrinsic
                    match intr {
                        Intrinsic::LogicalAnd => break {
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
                                    tp,
                                );

                                b.begin_bb(left_true_bb);
                                return self.build_expr(
                                    b,
                                    rhs,
                                    Context::new(0, DataDest::Branch(true_bb, false_bb), ControlDest::Continue),
                                    tp,
                                );
                            } else {
                                let left_false_bb = b.new_bb();
                                let after_bb = b.new_bb();
                                self.build_expr(
                                    b,
                                    lhs,
                                    Context::new(0, DataDest::Branch(left_true_bb, left_false_bb), ControlDest::Continue),
                                    tp,
                                );

                                b.begin_bb(left_true_bb);
                                // No further branching required, because (true && foo) <=> foo
                                let branch_ctx = ctx.redirect(location, Some(after_bb));
                                self.build_expr(b, rhs, branch_ctx.clone(), tp);

                                b.begin_bb(left_false_bb);
                                let false_val = b.code.push(Instr::Const(Const::Bool(false))).direct();
                                self.handle_context(b, false_val, branch_ctx, false, tp);

                                b.begin_bb(after_bb);
                                if let Some(location) = location {
                                    b.code.push(Instr::Load(location)).direct()
                                } else {
                                    return b.void_instr.direct()
                                }
                            }
                        },
                        Intrinsic::LogicalOr => break {
                            assert_eq!(arguments.len(), 2);
                            let (lhs, rhs) = (arguments[0], arguments[1]);
                            let left_false_bb = b.new_bb();
                            if let DataDest::Branch(true_bb, false_bb) = ctx.data {
                                self.build_expr(
                                    b,
                                    lhs,
                                    Context::new(0, DataDest::Branch(true_bb, left_false_bb), ControlDest::Continue),
                                    tp,
                                );

                                b.begin_bb(left_false_bb);
                                return self.build_expr(
                                    b,
                                    rhs,
                                    Context::new(0, DataDest::Branch(true_bb, false_bb), ControlDest::Continue),
                                    tp
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
                                    tp,
                                );

                                b.begin_bb(left_true_bb);
                                let true_val = b.code.push(Instr::Const(Const::Bool(true))).direct();
                                let branch_ctx = ctx.redirect(location, Some(after_bb));
                                self.handle_context(b, true_val, branch_ctx.clone(), false, tp);

                                b.begin_bb(left_false_bb);
                                self.build_expr(b, rhs, branch_ctx, tp);

                                b.begin_bb(after_bb);
                                if let Some(location) = location {
                                    b.code.push(Instr::Load(location)).direct()
                                } else {
                                    return b.void_instr.direct()
                                }
                            }
                        },
                        Intrinsic::LogicalNot => break {
                            assert_eq!(arguments.len(), 1);
                            let operand = arguments[0];
                            if let DataDest::Branch(true_bb, false_bb) = ctx.data {
                                return self.build_expr(b, operand, Context::new(0, DataDest::Branch(false_bb, true_bb), ctx.control), tp)
                            } else {
                                let operand = self.build_expr(b, operand, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                                b.code.push(Instr::LogicalNot(operand.instr)).direct()
                            }
                        },
                        _ => {},
                    }
                }

                // Otherwise, just handle the general case
                should_allow_set = true;
                // TODO: Don't clone arguments
                let arguments = arguments.clone().iter().map(|&argument| {
                    let val = self.build_expr(b, argument, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                    self.handle_indirection(b, val)
                }).collect();

                break self.get(b, arguments, id, tp);
            },
            Expr::Cast { expr, ty: dest_ty, cast_id } => {
                let dest_ty = tp.get_evaluated_type(dest_ty).clone();
                match tp.cast_method(cast_id) {
                    CastMethod::Noop => return self.build_expr(b, expr, ctx, tp),
                    CastMethod::Reinterpret => {
                        let value = self.build_expr(b, expr, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                        let value = self.handle_indirection(b, value);
                        b.code.push(Instr::Reinterpret(value, dest_ty)).direct()
                    },
                    CastMethod::Int => {
                        let (src_width, _src_is_signed, dest_width, dest_is_signed) = match (tp.ty(expr), &dest_ty) {
                            (&Type::Int { width: ref src_width, is_signed: src_is_signed }, &Type::Int { width: ref dest_width, is_signed: dest_is_signed })
                                => (src_width.clone(), src_is_signed, dest_width.clone(), dest_is_signed),
                            _ => panic!("Internal compiler error: found invalid cast types while generating MIR")
                        };
                        let (src_bit_width, dest_bit_width) = (src_width.bit_width(self.mir.arch), dest_width.bit_width(self.mir.arch));
                        let value = self.build_expr(b, expr, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                        let value = self.handle_indirection(b, value);

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
                        }.direct()
                    },
                    CastMethod::Float => {
                        let value = self.build_expr(b, expr, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                        let value = self.handle_indirection(b, value);
                        b.code.push(Instr::FloatCast(value, dest_ty)).direct()
                    },
                    CastMethod::FloatToInt => {
                        let value = self.build_expr(b, expr, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                        let value = self.handle_indirection(b, value);
                        b.code.push(Instr::FloatToInt(value, dest_ty)).direct()
                    },
                    CastMethod::IntToFloat => {
                        let value = self.build_expr(b, expr, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                        let value = self.handle_indirection(b, value);
                        b.code.push(Instr::IntToFloat(value, dest_ty)).direct()
                    },
                }
            },
            Expr::AddrOf { expr: operand, .. } => return self.build_expr(
                b,
                operand,
                Context::new(ctx.indirection + 1, ctx.data, ctx.control),
                tp,
            ).get_address(),
            Expr::Pointer { expr, is_mut } => {
                let op = self.build_expr(
                    b,
                    expr,
                    Context::new(0, DataDest::Read, ControlDest::Continue),
                    tp,
                );
                let op = self.handle_indirection(b, op);
                b.code.push(Instr::Pointer { op, is_mut }).direct()
            },
            Expr::Struct(id) => {
                let mut fields = SmallVec::new();
                for i in 0..self.hir.structs[id].fields.len() {
                    let field = self.hir.structs[id].fields[i];
                    let field_ty = self.hir.field_decls[field].ty;
                    let field = self.build_expr(
                        b,
                        field_ty,
                        Context::new(0, DataDest::Read, ControlDest::Continue),
                        tp,
                    );
                    let field = self.handle_indirection(b, field);
                    fields.push(field);
                }
                b.code.push(Instr::Struct { fields, id }).direct()
            },
            Expr::Deref(operand) => return self.build_expr(
                b,
                operand,
                Context::new(ctx.indirection - 1, ctx.data, ctx.control),
                tp,
            ),
            Expr::Do { scope } => return self.build_scope(b, scope, ctx, tp),
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
                    tp,
                );
                b.begin_bb(true_bb);
                let scope_ctx = ctx.redirect(result_location, Some(post_bb));
                self.build_scope(b, then_scope, scope_ctx.clone(), tp);
                if let Some(else_scope) = else_scope {
                    b.begin_bb(false_bb);
                    self.build_scope(b, else_scope, scope_ctx, tp);
                }

                b.begin_bb(post_bb);
                if let Some(location) = result_location {
                    return b.code.push(Instr::Load(location)).direct()
                } else if else_scope.is_some() {
                    return self.handle_control(b, b.void_instr.direct(), ctx.control)
                } else {
                    b.void_instr.direct()
                }
            },
            Expr::While { condition, scope } => {
                let test_bb = b.new_bb();
                let loop_bb = b.new_bb();
                let post_bb = match ctx.control {
                    ControlDest::Continue | ControlDest::Unreachable => b.new_bb(),
                    ControlDest::Block(block) => block,
                };

                b.code.push(Instr::Br(test_bb));
                b.begin_bb(test_bb);
                self.build_expr(b, condition, Context::new(0, DataDest::Branch(loop_bb, post_bb), ControlDest::Continue), tp);

                b.begin_bb(loop_bb);
                let val = self.build_scope(b, scope, Context::new(0, DataDest::Void, ControlDest::Block(test_bb)), tp);

                match ctx.control {
                    ControlDest::Continue | ControlDest::Unreachable => {
                        b.begin_bb(post_bb);
                        val
                    },
                    ControlDest::Block(_) => return val,
                }
            },
            Expr::Ret { expr, .. } => {
                return self.build_expr(
                    b,
                    expr,
                    Context::new(0, DataDest::Ret, ctx.control),
                    tp,
                );
            },
        };

        self.handle_context(b, val, ctx, should_allow_set, tp)
    }

    fn handle_indirection(&mut self, b: &mut FunctionBuilder, mut val: Value) -> InstrId {
        if val.indirection > 0 {
            while val.indirection > 0 {
                val.instr = b.code.push(Instr::Load(val.instr));
                val.indirection -= 1;
            }
        } else if val.indirection < 0 {
            let mut ty = b.type_of(&self.mir, val.instr);
            while val.indirection < 0 {
                let location = b.code.push(Instr::Alloca(ty.clone()));
                b.code.push(Instr::Store { location, value: val.instr });
                val.instr = location;
                val.indirection += 1;
                // Mutability doesn't matter for now
                ty = ty.mut_ptr();
            }
        }
        val.instr
    }

    fn handle_control(&mut self, b: &mut FunctionBuilder, val: Value, control: ControlDest) -> Value {
        match control {
            ControlDest::Block(block) => b.code.push(Instr::Br(block)).direct(),
            ControlDest::Continue => val,
            ControlDest::Unreachable => b.void_instr.direct(),
        }
    }

    fn handle_context(&mut self, b: &mut FunctionBuilder, mut val: Value, ctx: Context, should_allow_set: bool, tp: &impl TypeProvider) -> Value {
        val = val.adjusted(ctx.indirection);
        match ctx.data {
            DataDest::Read => return val,
            DataDest::Ret => {
                let instr = self.handle_indirection(b, val);
                return b.code.push(Instr::Ret(instr)).direct()
            },
            DataDest::Branch(true_bb, false_bb) => {
                let instr = self.handle_indirection(b, val);
                return b.code.push(Instr::CondBr { condition: instr, true_bb, false_bb }).direct()
            },
            DataDest::Receive { value } => {
                assert!(should_allow_set, "can't set constant expression!");
                let location = self.handle_indirection(b, val.get_address());
                b.code.push(Instr::Store { location, value });
            },
            DataDest::Store { location } => {
                let instr = self.handle_indirection(b, val);
                b.code.push(Instr::Store { location, value: instr });
            },
            DataDest::Set { dest } => {
                let instr = self.handle_indirection(b, val);
                return self.build_expr(
                    b,
                    dest,
                    Context::new(0, DataDest::Receive { value: instr }, ctx.control.clone()),
                    tp,
                )
            }
            DataDest::Void => {},
        }

        self.handle_control(b, val, ctx.control)
    }
}
