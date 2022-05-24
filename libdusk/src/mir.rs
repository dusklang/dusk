use std::collections::{HashSet, HashMap};
use std::ffi::CString;
use std::ops::Range;

use dire::index_counter::IndexCounter;
use num_bigint::BigInt;
use smallvec::{SmallVec, smallvec};
use string_interner::DefaultSymbol as Sym;
use display_adapter::display_adapter;

use dire::hir::{self, DeclId, ExprId, EnumId, DeclRefId, ImperScopeId, Intrinsic, Expr, StoredDeclId, GenericParamId, Item, PatternBindingDeclId, ExternModId, ExternFunctionRef, PatternBindingPathComponent, VOID_TYPE};
use dire::mir::{FuncId, StaticId, Const, Instr, InstrId, Function, MirCode, StructLayout, EnumLayout, ExternMod, ExternFunction, InstrNamespace, SwitchCase, VOID_INSTR};
use dire::{Block, BlockId, Op, OpId};
use dire::ty::{Type, InternalType, FunctionType, FloatWidth, StructType};
use dire::source_info::SourceRange;

use crate::driver::{Driver, DriverRef};
use crate::typechecker as tc;
use tc::type_provider::TypeProvider;
use tc::CastMethod;
use crate::index_vec::*;
use crate::source_info::ToSourceRange;
use crate::error::Error;

use dusk_proc_macros::*;

#[derive(Clone, Debug)]
enum Decl {
    Stored(StoredDeclId),
    Computed { get: FuncId },
    ExternFunction(ExternFunctionRef),
    Parameter { index: usize },
    PatternBinding { id: PatternBindingDeclId },
    Intrinsic(Intrinsic, Type),
    Static(StaticId),
    Const(Const),
    Field { index: usize },
    Variant { enuum: EnumId, index: usize, payload_ty: Option<Type> },
    GenericParam(GenericParamId),

    Invalid,
}

/// What to do with a value
#[derive(Copy, Clone, Debug)]
enum DataDest {
    /// This value needs to be returned from the current function
    Ret,
    /// A particular value needs to be assigned to this value
    Receive { value: OpId },
    /// This value needs to be assigned to a particular expression
    Set { dest: ExprId },
    /// This value needs to be written to a particular memory location
    Store { location: OpId },
    /// This value just needs to be read
    Read,
    /// This value will never be used
    Void,
    /// If this value is true, branch to the first basic block, otherwise branch to the second
    Branch(BlockId, BlockId),
}

#[derive(Debug, Copy, Clone)]
struct Value {
    instr: OpId,
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

trait Indirection {
    fn direct(self) -> Value;
    fn indirect(self) -> Value;
}

impl Indirection for OpId {
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
    Block(BlockId),
    RetVoid,
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

    fn redirect(&self, read: Option<OpId>, kontinue: Option<BlockId>) -> Context {
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

    fn new_data_dest(&self, data: DataDest) -> Context {
        if let DataDest::Ret = self.data {
            assert!(matches!(self.control, ControlDest::Unreachable));
            Context::new(self.indirection, data, ControlDest::RetVoid)
        } else {
            Context::new(self.indirection, data, self.control)
        }
    }
}

impl Driver {
    fn expr_to_const(&mut self, expr: ExprId, ty: Type) -> Const {
        match ef!(expr.hir) {
            Expr::IntLit { lit } => {
                match ty {
                    Type::Int { .. } => Const::Int { lit: BigInt::from(lit), ty },
                    Type::Float(_)   => Const::Float { lit: lit as f64, ty },
                    _ => panic!("Unrecognized integer literal type {:?}", ty),
                }
            },
            Expr::DecLit { lit } => Const::Float { lit, ty },
            Expr::StrLit { ref lit } => {
                if matches!(ty, Type::Internal(InternalType::StringLiteral)) {
                    Const::StrLit(lit.clone())
                } else {
                    let id = self.code.mir_code.strings.push(lit.clone());
                    Const::Str { id, ty }
                }
            },
            Expr::CharLit { lit } => match ty {
                Type::Int { .. } => Const::Int { lit: BigInt::from(lit), ty },
                Type::Pointer(_) => {
                    let id = self.code.mir_code.strings.push(CString::new([lit as u8].as_ref()).unwrap());
                    Const::Str { id, ty }
                },
                _ => panic!("unexpected type for character")
            },
            Expr::BoolLit { lit } => Const::Bool(lit),
            Expr::ConstTy(ref ty) => Const::Ty(ty.clone()),
            Expr::Mod { id } => Const::Mod(id),
            Expr::Import { file } => Const::Mod(self.code.hir_code.global_scopes[file]),
            _ => panic!("Cannot convert expression to constant: {:#?}", expr),
        }
    }
}

pub enum FunctionRef {
    Id(FuncId),
    Ref(Function),
}

#[derive(Clone)]
struct Static {
    name: String,
    assignment: ExprId,
}

#[derive(Default)]
pub struct Builder {
    decls: HashMap<DeclId, Decl>,
    statics: IndexVec<StaticId, Static>,
}

impl Builder {
    pub fn new() -> Self {
        Self {
            decls: HashMap::new(),
            statics: IndexVec::new(),
        }
    }
}

// TODO: remove this as soon as discriminants can be other types, and deal with the fallout from that
const TYPE_OF_DISCRIMINANTS: Type = Type::u32();

pub fn function_by_ref<'a>(code: &'a MirCode, func_ref: &'a FunctionRef) -> &'a Function {
    match func_ref {
        &FunctionRef::Id(id) => &code.functions[id],
        FunctionRef::Ref(func) => func,
    }
}

const MAX_LITERAL_BASED_INSTRUCTION_NAME_LENGTH: usize = 15;
/// Takes an arbitrary byte string and makes it suitable for inclusion in an instruction name
fn identifierify(mut string: Vec<u8>) -> String {
    for byte in string.iter_mut() {
        if !byte.is_ascii_alphanumeric() {
            *byte = b'_';
        }
    }
    string.truncate(MAX_LITERAL_BASED_INSTRUCTION_NAME_LENGTH);
    // Safety: all bytes that are not alphanumeric ASCII characters will be replaced with
    // underscores above.
    unsafe { String::from_utf8_unchecked(string) }
}

fn next_multiple_of(n: usize, fac: usize) -> usize {
    match fac {
        0 => n,
        _ => {
            let fac_minus_1 = fac - 1;
            (n + fac_minus_1) - ((n + fac_minus_1) % fac)
        }
    }
}

impl Driver {
    /// Size of an instance of a type in bytes
    pub fn size_of(&self, ty: &Type) -> usize {
        let arch = self.arch;
        match ty {
            Type::Error | Type::Void | Type::Never | Type::Ty | Type::Mod { .. } | Type::Internal(_) => 0,
            Type::Int { width, .. } => {
                let bit_width = width.bit_width(arch);
                assert_eq!(bit_width % 8, 0, "Unexpected bit width: not a multiple of eight!");
                bit_width / 8
            },
            Type::Float(width) => match width {
                FloatWidth::W32 => 32 / 8,
                FloatWidth::W64 => 64 / 8,
            },
            Type::Pointer(_) | Type::Function(_) => {
                let bit_width = arch.pointer_size();
                assert_eq!(bit_width % 8, 0, "Unexpected bit width: not a multiple of eight!");
                bit_width / 8
            },
            Type::Bool => 1,
            Type::Struct(strukt) => self.layout_struct(strukt).size,
            &Type::Enum(_id) => 4,
            Type::GenericParam(_) => panic!("can't get size of generic parameter without more context"),
            Type::Inout(_) => panic!("can't get size of inout parameter type"),
        }
    }

    /// Stride of an instance of a type in bytes
    pub fn stride_of(&self, ty: &Type) -> usize {
        match ty {
            Type::Struct(strukt) => self.layout_struct(strukt).stride,
            Type::Enum(_) => {
                let size = self.size_of(ty);
                match size {
                    0..=2 => size,
                    _ => next_multiple_of(size, 4),
                }
            },
            // Otherwise, stride == size
            _ => self.size_of(ty),
        }
    }

    /// Minimum alignment of an instance of a type in bytes
    pub fn align_of(&self, ty: &Type) -> usize {
        match ty {
            Type::Struct(strukt) => self.layout_struct(strukt).alignment,
            Type::Enum(_) => self.stride_of(ty),
            // Otherwise, alignment == size
            _ => self.size_of(ty),
        }
    }

    /// Compute the layout (field offsets, alignment, size, and stride) for a struct
    pub fn layout_struct(&self, strukt: &StructType) -> StructLayout {
        // Get max alignment of all the fields.
        let alignment = strukt.field_tys.iter()
            .map(|ty| self.align_of(ty))
            .max()
            .unwrap_or(0);

        let mut field_offsets = SmallVec::new();
        let mut last_size = 0;
        if !strukt.field_tys.is_empty() {
            field_offsets.push(0);
            last_size = self.size_of(&strukt.field_tys[0]);
        }
        for i in 1..strukt.field_tys.len() {
            let prev_field_end = field_offsets[i - 1] + last_size;
            field_offsets.push(
                next_multiple_of(
                    prev_field_end,
                    self.align_of(&strukt.field_tys[i])
                )
            );
            last_size = self.size_of(&strukt.field_tys[i]);
        }
        let size = field_offsets.last().copied().unwrap_or(0) + last_size;
        let stride = next_multiple_of(size, alignment);

        StructLayout { field_offsets, alignment, size, stride }
    }

    pub fn layout_enum(&self, variant_payload_tys: &[Type]) -> EnumLayout {
        use std::cmp::max;
        // Get max alignment of all the fields.
        let alignment = variant_payload_tys.iter()
            .map(|ty| self.align_of(ty))
            .max()
            .unwrap_or(0);
        let alignment = max(alignment, 4);

        let mut payload_offsets = SmallVec::new();
        let mut size = 4;
        for ty in variant_payload_tys {
            let offset = next_multiple_of(4, self.align_of(ty));
            payload_offsets.push(offset);
            size = max(size, offset + self.size_of(ty));
        }
        let stride = next_multiple_of(size, alignment);

        EnumLayout { payload_offsets, alignment, size, stride }
    }
}

impl DriverRef<'_> {
    pub fn build_mir(&mut self, tp: &impl TypeProvider) {
        // It is important to hold on to the write lock throughout this entire method, maybe
        self.write();
        // Start at 1 to avoid RETURN_VALUE_DECL, which we can't and shouldn't generate code for
        let len = self.read().code.hir_code.decls.len();
        for i in 1..len {
            self.get_decl(DeclId::new(i), tp);
        }

        let len = self.read().mir.statics.len();
        for i in 0..len {
            let id = StaticId::new(i);
            let statik = self.read().mir.statics[id].clone();
            let konst = self.eval_expr(statik.assignment, tp);
            self.write().code.mir_code.statics.push_at(
                id,
                dire::mir::Static {
                    name: statik.name,
                    val: konst.clone(),
                }
            );
        }
    }
}

impl DriverRef<'_> {
    pub fn build_standalone_expr(&mut self, expr: ExprId, tp: &impl TypeProvider) -> Function {
        let func_ty = FunctionType { param_tys: Vec::new(), return_ty: Box::new(tp.ty(expr).clone()) };
        self.build_function(None, func_ty, FunctionBody::Expr(expr), DeclId::new(0)..DeclId::new(0), Vec::new(), tp)
    }
}

impl Driver {
    fn resolve_extern_mod(&mut self, id: ExternModId, tp: &impl TypeProvider) {
        if self.code.mir_code.extern_mods.get(&id).is_some() { return; }

        let extern_mod = &self.code.hir_code.extern_mods[id];
        let library_path = extern_mod.library_path.clone();
        let mut imported_functions = Vec::with_capacity(extern_mod.imported_functions.len());
        for func in &extern_mod.imported_functions {
            let param_tys = func.param_tys.iter()
                .map(|&ty| tp.get_evaluated_type(ty))
                .cloned()
                .collect();
            let return_ty = tp.get_evaluated_type(func.return_ty).clone();
            imported_functions.push(
                ExternFunction {
                    name: func.name.clone(),
                    ty: FunctionType {
                        param_tys,
                        return_ty: Box::new(return_ty),
                    },
                }
            );
        }
        self.code.mir_code.extern_mods.insert(
            id,
            ExternMod {
                library_path,
                imported_functions,
            },
        );
    }
}

impl DriverRef<'_> {
    fn get_decl(&mut self, id: DeclId, tp: &impl TypeProvider) -> Decl {
        if let Some(decl) = self.read().mir.decls.get(&id) { return decl.clone(); }
        let d = self.read();
        match df!(d, id.hir) {
            hir::Decl::Computed { ref params, scope, generic_params: ref generic_params_range, .. } => {
                // Add placeholder function to reserve ID ahead of time
                let params = params.clone();
                let generic_params_range = generic_params_range.clone();
                drop(d);
                let get = self.write().code.mir_code.functions.push(Function::default());
                let decl = Decl::Computed { get };
                self.write().mir.decls.insert(id, decl.clone());

                // Convert DeclIds to GenericParamIds
                // TODO: don't require this?
                let mut generic_params = Vec::new();
                generic_params.reserve(generic_params_range.end.index() - generic_params_range.start.index());
                for i in generic_params_range.start.index()..generic_params_range.end.index() {
                    let id = DeclId::new(i);
                    let d = self.read();
                    let generic_param = match df!(d, id.hir) {
                        hir::Decl::GenericParam(param) => param,
                        _ => panic!("unexpected decl type, expected generic parameter"),
                    };
                    generic_params.push(generic_param);
                }

                let params = params.clone();
                let func_ty = self.read().decl_type(id, tp).as_function().unwrap().clone();
                let name = self.read().code.hir_code.names[id];
                let func = self.build_function(
                    Some(name),
                    func_ty,
                    FunctionBody::Scope { scope, decl: id },
                    params,
                    generic_params,
                    tp
                );
                self.write().code.mir_code.functions[get] = func;
                decl
            },
            hir::Decl::ComputedPrototype { extern_func, .. } => {
                let decl = if let Some(extern_func) = extern_func {
                    drop(d);
                    self.write().resolve_extern_mod(extern_func.extern_mod, tp);
                    Decl::ExternFunction(extern_func)
                } else {
                    let err = Error::new("cannot declare prototype outside of extern module")
                        .adding_primary_range(df!(d, id.range), "prototype here");
                    drop(d);
                    self.write().errors.push(err);
                    Decl::Invalid
                };
                self.write().mir.decls.insert(id, decl.clone());
                decl
            },
            hir::Decl::Stored { id: index, .. } => {
                drop(d);
                let decl = Decl::Stored(index);
                self.write().mir.decls.insert(id, decl.clone());
                decl
            },
            hir::Decl::Parameter { index } => {
                drop(d);
                let decl = Decl::Parameter { index };
                self.write().mir.decls.insert(id, decl.clone());
                decl
            },
            hir::Decl::PatternBinding { id: index, .. } => {
                drop(d);
                let decl = Decl::PatternBinding { id: index };
                self.write().mir.decls.insert(id, decl.clone());
                decl
            },
            hir::Decl::Intrinsic { intr, function_like, .. } => {
                drop(d);
                let mut ty = self.read().decl_type(id, tp);
                if function_like {
                    ty = ty.return_ty().unwrap().clone();
                }
                let decl = Decl::Intrinsic(intr, ty);
                self.write().mir.decls.insert(id, decl.clone());
                decl
            },
            hir::Decl::Static(expr) => {
                drop(d);
                let name = format!("{}", self.read().display_item(id));
                let decl = Decl::Static(StaticId::new(self.read().mir.statics.len()));
                self.write().mir.decls.insert(id, decl.clone());
                self.write().mir.statics.push(
                    Static {
                        name,
                        assignment: expr,
                    }
                );
                decl
            },
            hir::Decl::Const(root_expr) => {
                drop(d);
                let konst = self.eval_expr(root_expr, tp);

                // TODO: Deal with cycles!
                let decl = Decl::Const(konst);
                self.write().mir.decls.insert(id, decl.clone());
                decl
            },
            hir::Decl::Field { index, .. } => {
                drop(d);
                let decl = Decl::Field { index };
                self.write().mir.decls.insert(id, decl.clone());
                decl
            },
            hir::Decl::Variant { enuum, index, payload_ty } => {
                let payload_ty = payload_ty.map(|ty| tp.get_evaluated_type(ty).clone());
                Decl::Variant { enuum, index, payload_ty }
            },
            hir::Decl::GenericParam(param) => {
                drop(d);
                let decl = Decl::GenericParam(param);
                self.write().mir.decls.insert(id, decl.clone());
                decl
            },
            hir::Decl::ReturnValue => panic!("Can't get_decl() the return_value decl"),
        }
    }
}

impl Driver {
    #[allow(dead_code)]
    #[display_adapter]
    fn fmt_variant_name(&self, f: &mut Formatter, enuum: EnumId, index: usize) {
        let variant = &self.code.hir_code.enums[enuum].variants[index];
        let name = self.interner.resolve(variant.name).unwrap();
        write!(f, "{}", name)
    }

    #[allow(dead_code)]
    #[display_adapter]
    fn fmt_const(&self, f: &mut Formatter, konst: &Const) {
        match *konst {
            Const::Bool(val) => write!(f, "{}", val)?,
            Const::Float { lit, ref ty } => write!(f, "{} as {:?}", lit, ty)?,
            Const::Int { ref lit, ref ty } => write!(f, "{} as {:?}", lit, ty)?,
            Const::Str { id, ref ty } => write!(f, "%str{} ({:?}) as {:?}", id.index(), self.code.mir_code.strings[id], ty)?,
            Const::StrLit(ref lit) => write!(f, "str_lit \"{}\"", lit.clone().into_string().unwrap())?,
            Const::Ty(ref ty) => write!(f, "`{:?}`", ty)?,
            Const::Void => write!(f, "void")?,
            Const::Mod(id) => write!(f, "%mod{}", id.index())?,
            Const::BasicVariant { enuum, index } => write!(f, "%enum{}.{}", enuum.index(), self.fmt_variant_name(enuum, index))?,
            Const::StructLit { ref fields, id } => {
                write!(f, "const literal struct{} {{ ", id.index())?;
                for i in 0..fields.len() {
                    write!(f, "{}", self.fmt_const(&fields[i]))?;
                    if i < (fields.len() - 1) {
                        write!(f, ",")?;
                    }
                    write!(f, " ")?;
                }
                write!(f, "}}")?;
            },
        }

        Ok(())
    }

    #[display_adapter]
    fn fmt_const_for_instr_name(&self, f: &mut Formatter, konst: &Const) {
        match *konst {
            Const::Bool(val) => write!(f, "const_{}", val)?,
            Const::Float { lit, .. } => {
                let name = lit.to_string()
                    .replace("-", "negative_")
                    .replace(".", "_dot_");
                write!(f, "const_{}", name)?
            },
            Const::Int { ref lit, .. } => write!(f, "const_int_{}", lit)?,
            Const::Str { id, .. } => write!(f, "string_{}", identifierify(self.code.mir_code.strings[id].clone().into_bytes()))?,
            Const::StrLit(ref lit) => write!(f, "string_lit_{}", identifierify(lit.clone().into_bytes()))?,
            Const::Ty(ref ty) => write!(f, "type_{}", identifierify(format!("{:?}", ty).into_bytes()))?,
            Const::Void => write!(f, "const_void")?,

            // TODO: heuristics for associating declaration names with modules and types
            Const::Mod(id) => write!(f, "mod{}", id.index())?,
            Const::BasicVariant { enuum, index } => write!(f, "enum{}_variant_{}", enuum.index(), self.fmt_variant_name(enuum, index))?,

            Const::StructLit { id, .. } => {
                write!(f, "const_struct_literal_{}", id.index())?;
            }
        }

        Ok(())
    }

    pub fn fn_name(&self, name: Option<Sym>) -> &str {
        match name {
            Some(name) => self.interner.resolve(name).unwrap(),
            None => "{anonymous}",
        }
    }

    #[display_adapter]
    pub fn display_item(&'a self, item: impl Into<ToSourceRange> + Copy + 'a, f: &mut Formatter) {
        let range = self.get_range(item);
        write!(f, "{}", self.src_map.substring_from_range(range))
    }

    #[display_adapter]
    pub fn display_instr_name(&self, item: OpId, f: &mut Formatter) {
        write!(f, "{}", self.code.mir_code.instr_names.get(&item).cloned()
            .unwrap_or_else(|| format!("instr{}", item.index())))
    }

    #[display_adapter]
    pub fn display_mir_instr(&self, op_id: OpId, f: &mut Formatter) {
        let instr = self.code.ops[op_id].as_mir_instr().unwrap();
        write!(f, "    ")?;
        macro_rules! write_args {
            ($args:expr) => {{
                let mut first = true;
                for &arg in $args {
                    if first {
                        write!(f, "(")?;
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "%{}", self.display_instr_name(arg))?;
                }
                if !first {
                    write!(f, ")")?;
                }
            }}
        }
        match instr {
            Instr::Alloca(ty) => write!(f, "%{} = alloca {:?}", self.display_instr_name(op_id), ty)?,
            Instr::Br(block) => write!(f, "br %bb{}", block.index())?,
            &Instr::CondBr { condition, true_bb, false_bb }
                => write!(f, "condbr %{}, %bb{}, %bb{}", self.display_instr_name(condition), true_bb.index(), false_bb.index())?,
            &Instr::SwitchBr { scrutinee, ref cases, catch_all_bb } => {
                write!(f, "switchbr %{} : ", self.display_instr_name(scrutinee))?;
                for case in cases {
                    write!(f, "case {} => %bb{}, ", self.fmt_const(&case.value), case.bb.index())?;
                }
                write!(f, "else => %bb{}", catch_all_bb.index())?;
            }
            // TODO: print generic arguments
            &Instr::Call { ref arguments, func: callee, .. } => {
                write!(f, "%{} = call `{}`", self.display_instr_name(op_id), self.fn_name(self.code.mir_code.functions[callee].name))?;
                write_args!(arguments);
            },
            &Instr::FunctionRef { func: callee, .. } => {
                write!(f, "%{} = function_ref `{}`", self.display_instr_name(op_id), self.fn_name(self.code.mir_code.functions[callee].name))?
            },
            &Instr::ExternCall { ref arguments, func: callee, .. } => {
                let extern_mod = &self.code.mir_code.extern_mods[&callee.extern_mod];
                let callee_func = &extern_mod.imported_functions[callee.index];
                write!(f, "%{} = externcall `{}`", self.display_instr_name(op_id), callee_func.name)?;
                write_args!(arguments);
                write!(f, " from {:?}", extern_mod.library_path)?
            },
            Instr::Const(konst) => {
                write!(f, "%{} = {}", self.display_instr_name(op_id), self.fmt_const(konst))?;
            },
            Instr::Intrinsic { arguments, intr, .. } => {
                write!(f, "%{} = intrinsic `{}`", self.display_instr_name(op_id), intr.name())?;
                write_args!(arguments);
            },
            &Instr::Pointer { op, is_mut } => {
                write!(f, "%{} = %{} *", self.display_instr_name(op_id), self.display_instr_name(op))?;
                if is_mut {
                    write!(f, "mut")?
                }
            },
            &Instr::Load(location) => write!(f, "%{} = load %{}", self.display_instr_name(op_id), self.display_instr_name(location))?,
            &Instr::LogicalNot(op) => write!(f, "%{} = not %{}", self.display_instr_name(op_id), self.display_instr_name(op))?,
            &Instr::Ret(val) => write!(f,  "return %{}", self.display_instr_name(val))?,
            &Instr::Store { location, value } => write!(f, "store %{} in %{}", self.display_instr_name(value), self.display_instr_name(location))?,
            &Instr::AddressOfStatic(statik) => write!(f, "%{} = address of static %{}", self.display_instr_name(op_id), self.code.mir_code.statics[statik].name)?,
            &Instr::Reinterpret(val, ref ty) => write!(f, "%{} = reinterpret %{} as {:?}", self.display_instr_name(op_id), self.display_instr_name(val), ty)?,
            &Instr::SignExtend(val, ref ty) => write!(f, "%{} = sign-extend %{} as {:?}", self.display_instr_name(op_id), self.display_instr_name(val), ty)?,
            &Instr::ZeroExtend(val, ref ty) => write!(f, "%{} = zero-extend %{} as {:?}", self.display_instr_name(op_id), self.display_instr_name(val), ty)?,
            &Instr::Truncate(val, ref ty) => write!(f, "%{} = truncate %{} as {:?}", self.display_instr_name(op_id), self.display_instr_name(val), ty)?,
            &Instr::FloatCast(val, ref ty) => write!(f, "%{} = floatcast %{} as {:?}", self.display_instr_name(op_id), self.display_instr_name(val), ty)?,
            &Instr::IntToFloat(val, ref ty) => write!(f, "%{} = inttofloat %{} as {:?}", self.display_instr_name(op_id), self.display_instr_name(val), ty)?,
            &Instr::FloatToInt(val, ref ty) => write!(f, "%{} = floattoint %{} as {:?}", self.display_instr_name(op_id), self.display_instr_name(val), ty)?,
            &Instr::Struct { ref fields, id } => {
                write!(f, "%{} = define struct{} {{ ", self.display_instr_name(op_id), id.index())?;
                for i in 0..fields.len() {
                    write!(f, "%{}", self.display_instr_name(fields[i]))?;
                    if i < (fields.len() - 1) {
                        write!(f, ",")?;
                    }
                    write!(f, " ")?;
                }
                write!(f, "}}")?;
            },
            &Instr::StructLit { ref fields, id } => {
                write!(f, "%{} = literal struct{} {{ ", self.display_instr_name(op_id), id.index())?;
                for i in 0..fields.len() {
                    write!(f, "%{}", self.display_instr_name(fields[i]))?;
                    if i < (fields.len() - 1) {
                        write!(f, ",")?;
                    }
                    write!(f, " ")?;
                }
                write!(f, "}}")?;
            },
            &Instr::Enum { ref variants, id } => {
                write!(f, "%{} = define enum{} {{", self.display_instr_name(op_id), id.index())?;

                for (i, variant) in self.code.hir_code.enums[id].variants.iter().enumerate() {
                    write!(f, "{}", self.interner.resolve(variant.name).unwrap())?;
                    if variant.payload_ty.is_some() {
                        write!(f, "(%{})", self.display_instr_name(variants[i]))?;
                    }
                    if i < (variants.len() - 1) {
                        write!(f, ",")?;
                    }
                    write!(f, " ")?;
                }

                write!(f, "}}")?;
            },
            &Instr::FunctionTy { ref param_tys, ret_ty } => {
                write!(f, "%{} = fn type (", self.display_instr_name(op_id))?;
                for (i, &param) in param_tys.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "%{}", self.display_instr_name(param))?;
                }
                write!(f, " -> {}", self.display_instr_name(ret_ty))?;
            },
            &Instr::Variant { enuum, index, payload } => {
                let variant = &self.code.hir_code.enums[enuum].variants[index];
                let variant_name = variant.name;
                write!(f, "%{} = %enum{}.{}", self.display_instr_name(op_id), enuum.index(), self.interner.resolve(variant_name).unwrap())?;
                if variant.payload_ty.is_some() {
                    write!(f, "(%{})", self.display_instr_name(payload))?
                }
            },
            &Instr::DirectFieldAccess { val, index } => write!(f, "%{} = %{}.field{}", self.display_instr_name(op_id), self.display_instr_name(val), index)?,
            &Instr::IndirectFieldAccess { val, index } => write!(f, "%{} = &(*%{}).field{}", self.display_instr_name(op_id), self.display_instr_name(val), index)?,
            &Instr::DiscriminantAccess { val } => write!(f, "%{} = discriminant of %{}", self.display_instr_name(op_id), self.display_instr_name(val))?,
            &Instr::GenericParam(param) => {
                write!(f, "%{} = generic_param{}", self.display_instr_name(op_id), param.index())?
            },
            Instr::Parameter(_) => {},
            Instr::Invalid => write!(f, "%{} = invalid!", self.display_instr_name(op_id))?,
            Instr::Void => panic!("unexpected void!"),
        };
        Ok(())
    }

    #[display_adapter]
    pub fn display_mir_block(&self, id: BlockId, f: &mut Formatter) {
        writeln!(f, "%bb{}:", id.index())?;
        let block = &self.code.blocks[id];
        let mut start = 0;
        for (i, &op) in block.ops.iter().enumerate() {
            let instr = self.code.ops[op].as_mir_instr().unwrap();
            if !matches!(instr, Instr::Parameter(_)) {
                start = i;
                break;
            }
        }
        
        for &op_id in &block.ops[start..] {
            writeln!(f, "{}", self.display_mir_instr(op_id))?;
        }
        Ok(())
    }

    #[display_adapter]
    pub fn display_mir_function(&self, func: &FunctionRef, f: &mut Formatter) {
        let func = function_by_ref(&self.code.mir_code, func);
        write!(f, "fn {}(", self.fn_name(func.name))?;
        let entry_block = &self.code.blocks[func.blocks[0]];
        let mut first = true;
        for &op in &entry_block.ops {
            let instr = self.code.ops[op].as_mir_instr().unwrap();
            if let Instr::Parameter(ty) = instr {
                if first {
                    first = false;
                } else {
                    write!(f, ", ")?;
                }
                write!(f, "%{}: {:?}", self.display_instr_name(op), ty)?;
            } else {
                break;
            }
        }
        writeln!(f, "): {:?} {{", func.ty.return_ty.as_ref())?;
        for i in 0..func.blocks.len() {
            let block_id = func.blocks[i];
            write!(f, "{}", self.display_mir_block(block_id))?;
        }
        write!(f, "}}")
    }

    #[display_adapter]
    pub fn display_mir(&self, f: &mut Formatter) {
        if !self.code.mir_code.statics.raw.is_empty() {
            for statik in &self.code.mir_code.statics {
                writeln!(f, "%{} = {}", statik.name, self.fmt_const(&statik.val))?;
            }
            writeln!(f)?;
        }

        for i in self.code.mir_code.functions.indices() {
            write!(f, "{}", self.display_mir_function(&FunctionRef::Id(i)))?;
            if i + 1 < self.code.mir_code.functions.len() {
                writeln!(f, "\n")?;
            }
        }
        Ok(())
    }
}

#[derive(Copy, Clone)]
enum FunctionBody {
    Scope { scope: ImperScopeId, decl: DeclId },
    Expr(ExprId),
    ConstantInstruction(OpId),
}

struct FunctionBuilder {
    name: Option<Sym>,
    ty: FunctionType,
    instrs: IndexCounter<InstrId>,
    blocks: Vec<BlockId>,
    current_block: BlockId,
    stored_decl_locs: IndexVec<StoredDeclId, OpId>,
    pattern_binding_locs: HashMap<PatternBindingDeclId, Value>,
    instr_namespace: InstrNamespace,
}

#[derive(Debug)]
enum DeclRef {
    Intrinsic { intrinsic: Intrinsic, ty: Type },
    Function { func: FuncId, generic_args: Vec<Type> },
    ExternFunction { func: ExternFunctionRef },
    #[allow(unused)]
    EnumVariantWithPayload { enuum: EnumId, index: usize, payload_ty: Option<Type> },
    Value(Value),
}

impl Driver {
    fn create_bb(&mut self, b: &mut FunctionBuilder) -> BlockId {
        let block = self.code.blocks.push(Block::default());
        b.blocks.push(block);
        block
    }
    fn start_bb(&mut self, b: &mut FunctionBuilder, block: BlockId) {
        self.code.mir_code.start_block(block).unwrap();
        b.current_block = block;
    }
    fn end_current_bb(&mut self, b: &FunctionBuilder) {
        let bb = b.current_block;
        if self.code.mir_code.end_block(bb).is_err() {
            panic!("Failed to end block {} in function {}:\n{}", bb.index(), self.fn_name(b.name), self.code.display_block(bb));
        }
        let block = &self.code.blocks[bb];
        let last_instr = self.code.ops[block.ops.last().copied().unwrap()].as_mir_instr().unwrap();
        assert!(
            match last_instr {
                Instr::Br(_) | Instr::CondBr { .. } | Instr::SwitchBr { .. } | Instr::Ret { .. } | Instr::Intrinsic { intr: Intrinsic::Panic, .. } => true,
                _ => false,
            },
            "expected terminal instruction before moving on to next block, found {:?}",
            last_instr,
        );
    }

    pub fn type_of(&self, instr: OpId) -> &Type {
        self.code.ops[instr].get_mir_instr_type().unwrap()
    }
}

impl DriverRef<'_> {
    fn build_function(&mut self, name: Option<Sym>, func_ty: FunctionType, body: FunctionBody, params: Range<DeclId>, generic_params: Vec<GenericParamId>, tp: &impl TypeProvider) -> Function {
        debug_assert_ne!(func_ty.return_ty.as_ref(), &Type::Error, "can't build MIR function with Error return type");

        let mut entry = Block::default();
        let mut instr_namespace = InstrNamespace::default();
        let mut instrs = IndexCounter::new();
        instrs.next(); // void
        for param in params.start.index()..params.end.index() {
            let param = DeclId::new(param);
            let d = self.read();
            assert!(matches!(df!(d, param.hir), hir::Decl::Parameter { .. }));
            drop(d);
            let ty = self.read().decl_type(param, tp);
            let instr = Instr::Parameter(ty.clone());
            let instr_id = instrs.next();
            let op = self.write().code.ops.push(Op::MirInstr(instr, instr_id, ty));
            let d = self.read();
            let range = df!(d, param.range);
            drop(d);
            let name = instr_namespace.insert(format!("{}", self.read().display_item(range)));
            self.write().code.mir_code.source_ranges.insert(op, range);
            self.write().code.mir_code.instr_names.insert(op, name);
            entry.ops.push(op);
        }
        let entry = self.write().code.blocks.push(entry);
        let mut b = FunctionBuilder {
            name,
            ty: func_ty,
            instrs,
            blocks: vec![entry],
            current_block: entry,
            stored_decl_locs: IndexVec::new(),
            pattern_binding_locs: HashMap::new(),
            instr_namespace,
        };
        self.write().start_bb(&mut b, entry);
        let ctx = Context::new(0, DataDest::Ret, ControlDest::Unreachable);
        let decl = match body {
            FunctionBody::Expr(expr) => {
                self.build_expr(&mut b, expr, ctx, tp);
                None
            },
            FunctionBody::Scope { scope, decl } => {
                self.build_scope(&mut b, scope, ctx, tp);
                Some(decl)
            },
            FunctionBody::ConstantInstruction(op) => {
                let instruction = self.read().code.ops[op].as_mir_instr().unwrap().clone();
                match instruction {
                    Instr::Intrinsic { arguments, .. } => {
                        let mut copier = MirCopier::default();
                        for arg in arguments {
                            self.copy_instruction_if_needed(&mut b, &mut copier, arg);
                        }
                        let result = self.copy_instruction_if_needed(&mut b, &mut copier, op);
                        self.write().push_instr(&mut b, Instr::Ret(result), result);
                        self.write().end_current_bb(&mut b);
                    },
                    _ => unimplemented!(),
                }
                None
            },
        };
        let mut function = Function {
            name: b.name,
            ty: b.ty,
            num_instrs: b.instrs.len(),
            blocks: b.blocks,
            instr_namespace: b.instr_namespace,
            decl,
            generic_params,
        };

        let is_constant_instruction = matches!(body, FunctionBody::ConstantInstruction(_));
        self.optimize_function(&mut function, !is_constant_instruction, tp);
        self.validate_function(&function);
        self.read().code.mir_code.check_all_blocks_ended(&function);
        function 
    }
}

impl Driver {
    fn remove_redundant_loads(&mut self, func: &mut Function) {
        // Remove obviously-redundant loads (assumes no other threads are accessing a memory location simultaneously)
        let mut replace_list = Vec::new();
        for &block_id in &func.blocks {
            let block = &self.code.blocks[block_id];
            let mut delete_list = HashSet::new();
            for (i, &op_id) in block.ops.iter().enumerate() {
                let instr = self.code.ops[op_id].as_mir_instr().unwrap();
                if let &Instr::Store { location, value } = instr {
                    if i + 1 < block.ops.len() {
                        let next_op = block.ops[i+1];
                        let next_instr = self.code.ops[next_op].as_mir_instr().unwrap();
                        if let &Instr::Load(load_loc) = next_instr {
                            if load_loc == location {
                                replace_list.push((next_op, value));
                                delete_list.insert(next_op);
                            }
                        }
                    }
                }
            }
            
            self.code.blocks[block_id].ops.retain(|op| !delete_list.contains(op));
        }
        for &block_id in &func.blocks {
            let block = &self.code.blocks[block_id];
            for &op in &block.ops {
                for &(old, new) in &replace_list {
                    self.code.ops[op].as_mir_instr_mut().unwrap().replace_value(old, new);
                }
            }
        }
    }

    fn remove_unused_allocas(&mut self, func: &mut Function) {
        let mut delete_list = HashSet::new();
        for &block_id in &func.blocks {
            let block = &self.code.blocks[block_id];
            for &op_id in &block.ops {
                let instr = self.code.ops[op_id].as_mir_instr().unwrap();
                let mut potential_deletions = Vec::new();
                if let Instr::Alloca(_) = instr {
                    let mut is_used = false;
                    'check_uses: for &other_block_id in &func.blocks {
                        let other_block = &self.code.blocks[other_block_id];

                        for &other_op_id in &other_block.ops {
                            let other_instr = self.code.ops[other_op_id].as_mir_instr().unwrap();
                            if other_instr.references_value(op_id) {
                                if let &Instr::Store { value, .. } = other_instr {
                                    // If the address of the alloca is used as the *value* in the store, then we can't
                                    // delete either instruction. Otherwise, it must be the location, in which case we
                                    // can delete both (assuming the alloca isn't used elsewhere).
                                    if value == op_id {
                                        is_used = true;
                                        break 'check_uses;
                                    } else {
                                        potential_deletions.push(other_op_id);
                                    }
                                } else {
                                    is_used = true;
                                    break 'check_uses;
                                }
                            }
                        }
                    }
                    if !is_used {
                        delete_list.insert(op_id);
                        delete_list.extend(potential_deletions);
                    }
                }
            }
            
        }
        for &block_id in &func.blocks {
            self.code.blocks[block_id].ops.retain(|op| !delete_list.contains(op));
        }
    }

    fn remove_unused_values(&mut self, func: &mut Function) {
        for &block_id in &func.blocks {
            let block = &self.code.blocks[block_id];
            let mut delete_list = HashSet::new();

            for &op_id in &block.ops {
                let instr = self.code.ops[op_id].as_mir_instr().unwrap();
                if let Instr::Const(_) | Instr::Load(_) = instr {
                    let mut is_used = false;
                    'check_uses: for &other_block_id in &func.blocks {
                        let other_block = &self.code.blocks[other_block_id];

                        for &other_op_id in &other_block.ops {
                            let other_instr = self.code.ops[other_op_id].as_mir_instr().unwrap();
                            if other_instr.references_value(op_id) {
                                is_used = true;
                                break 'check_uses;
                            }
                        }
                    }
                    if !is_used {
                        delete_list.insert(op_id);
                    }
                }
            }

            self.code.blocks[block_id].ops.retain(|op| !delete_list.contains(op));
        }
    }

    fn remove_redundant_blocks(&mut self, func: &mut Function) {
        // Replace blocks that do nothing but branch to another block
        let mut replace_list = Vec::new();
        let mut delete_list = HashSet::new();
        let mut new_entry_block = None;
        for (i, &block_id) in func.blocks.iter().enumerate() {
            let block = &self.code.blocks[block_id];
            let mut num_parameters = 0;
            for &op in &block.ops {
                if !matches!(self.code.ops[op].as_mir_instr().unwrap(), Instr::Parameter(_)) {
                    break;
                }
                num_parameters += 1;
            }
            if block.ops.len() - num_parameters != 1 { continue; }

            let terminal = *block.ops.last().unwrap();
            let terminal = self.code.ops[terminal].as_mir_instr().unwrap();
            if let &Instr::Br(other) = terminal {
                // don't remove infinite loops
                if other != block_id {
                    replace_list.push((block_id, other));
                    delete_list.insert(block_id);
                    if i == 0 {
                        let parameters: Vec<_> = block.ops[..num_parameters].iter()
                            .copied()
                            .collect();
                        new_entry_block = Some((other, parameters));
                    }
                    if new_entry_block.as_ref().map(|(b, _)| b) == Some(&block_id) {
                        new_entry_block.as_mut().unwrap().0 = other;
                    }
                }
            }
        }
        func.blocks.retain(|block| !delete_list.contains(block));
        let mut new_entry_block_index = None;
        for (i, &block_id) in func.blocks.iter().enumerate() {
            let block = &self.code.blocks[block_id];
            let terminal = *block.ops.last().unwrap();
            let terminal = self.code.ops[terminal].as_mir_instr_mut().unwrap();
            for &(from, to) in &replace_list {
                terminal.replace_bb(from, to);
            }
            if new_entry_block.as_ref().map(|(b, _)| b) == Some(&block_id) {
                new_entry_block_index = Some(i);
            }
        }
        if let Some((new_entry_block, parameters)) = new_entry_block {
            func.blocks.swap(0, new_entry_block_index.unwrap());
            
            self.code.blocks[new_entry_block].ops.splice(0..0, parameters);
        }
    }

    fn traverse_descendants(&self, func: &mut Function, visited: &mut HashSet<BlockId>, block: BlockId) {
        if !visited.insert(block) { return; }

        let terminal = *self.code.blocks[block].ops.last().unwrap();
        let terminal = self.code.ops[terminal].as_mir_instr().unwrap();
        match *terminal {
            Instr::Br(block) => self.traverse_descendants(func, visited, block),
            Instr::CondBr { true_bb, false_bb, .. } => {
                self.traverse_descendants(func, visited, true_bb);
                self.traverse_descendants(func, visited, false_bb);
            },
            Instr::SwitchBr { ref cases, catch_all_bb, .. } => {
                for case in cases {
                    self.traverse_descendants(func, visited, case.bb);
                }
                self.traverse_descendants(func, visited, catch_all_bb);
            },
            _ => {},
        }
    }

    fn remove_unreachable_blocks(&mut self, func: &mut Function) {
        let mut visited = HashSet::new();
        self.traverse_descendants(func, &mut visited, func.blocks[0]);
        func.blocks.retain(|block| visited.contains(block));
    }

    fn remove_constant_branches(&mut self, func: &mut Function) {
        let mut replace_list = Vec::new();
        for &block_id in &func.blocks {
            let block = &self.code.blocks[block_id];
            if let Some(&terminal) = block.ops.last() {
                match self.code.ops[terminal].as_mir_instr().unwrap() {
                    &Instr::CondBr { condition, true_bb, false_bb } => {
                        let condition = self.code.ops[condition].as_mir_instr().unwrap();
                        if let &Instr::Const(Const::Bool(condition)) = condition {
                            let destination = [false_bb, true_bb][condition as usize];
                            replace_list.push((terminal, Instr::Br(destination)));
                        }
                    },
                    _ => {},
                }
            }
        }

        for (id, new_instr) in replace_list {
            *self.code.ops[id].as_mir_instr_mut().unwrap() = new_instr;
        }
    }
}

#[derive(Default)]
struct MirCopier {
    old_to_new: HashMap<OpId, OpId>,
}

impl DriverRef<'_> {
    fn instruction_is_const(&self, instr: OpId) -> bool {
        let d = self.read();
        let instr = d.code.ops[instr].as_mir_instr().unwrap();
        match *instr {
            Instr::Const(_) | Instr::Void => true,
            Instr::Intrinsic { intr, .. } => {
                match intr {
                    Intrinsic::Mult | Intrinsic::Div | Intrinsic::Mod | Intrinsic::Add | Intrinsic::Sub
                        | Intrinsic::Less | Intrinsic::LessOrEq | Intrinsic::Greater | Intrinsic::GreaterOrEq
                        | Intrinsic::Eq | Intrinsic::NotEq | Intrinsic::BitwiseAnd | Intrinsic::BitwiseOr
                        | Intrinsic::BitwiseNot | Intrinsic::BitwiseXor | Intrinsic::LeftShift | Intrinsic::RightShift
                        | Intrinsic::LogicalAnd | Intrinsic::LogicalOr | Intrinsic::LogicalNot | Intrinsic::Neg
                        | Intrinsic::Pos => {
                        instr.referenced_values().iter().all(|&val| self.instruction_is_const(val))
                    }
                    _ => false,
                }
            },
            Instr::LogicalNot(val) | Instr::Truncate(val, _) | Instr::SignExtend(val, _) | Instr::ZeroExtend(val, _)
                | Instr::FloatCast(val, _) | Instr::FloatToInt(val, _) | Instr::IntToFloat(val, _)
                => self.instruction_is_const(val),
            _ => false,
        }
    }

    fn instruction_is_nontrivial_const(&self, instr: OpId) -> bool {
        self.instruction_is_const(instr) && !matches!(self.read().code.ops[instr].as_mir_instr().unwrap(), Instr::Const(_))
    }

    fn copy_instruction_if_needed(&mut self, b: &mut FunctionBuilder, copier: &mut MirCopier, instr_id: OpId) -> OpId {
        if let Some(&new) = copier.old_to_new.get(&instr_id) {
            new
        } else {
            let mut instr = self.read().code.ops[instr_id].as_mir_instr().unwrap().clone();
            let replacements: Vec<_> = instr.referenced_values().into_iter().map(|arg| (arg, self.copy_instruction_if_needed(b, copier, arg))).collect();
            for (old, new) in replacements {
                instr.replace_value(old, new);
            }

            let copied_instr_id = self.write().push_instr(b, instr, instr_id);
            copier.old_to_new.insert(instr_id, copied_instr_id);
            copied_instr_id
        }
    }

    fn eval_constants(&mut self, func: &mut Function, tp: &impl TypeProvider) {
        self.write();
        for &block in &func.blocks {
            let ops = self.read().code.blocks[block].ops.clone();
            for op in ops {
                if self.instruction_is_nontrivial_const(op) {
                    let ty = self.read().type_of(op).clone();
                    let func_ty = FunctionType { param_tys: vec![], return_ty: Box::new(ty.clone()) };
                    let func = self.build_function(func.name, func_ty, FunctionBody::ConstantInstruction(op), DeclId::new(0)..DeclId::new(0), Vec::new(), tp);
                    let result = self.call(FunctionRef::Ref(func), Vec::new(), Vec::new());
                    let konst = self.write().value_to_const(result, ty, tp);
                    *self.write().code.ops[op].as_mir_instr_mut().unwrap() = Instr::Const(konst);
                }
            }
        }
    }

    fn optimize_function(&mut self, func: &mut Function, should_eval_constants: bool, tp: &impl TypeProvider) {
        // Get rid of empty blocks
        // TODO: get rid of unreachable blocks instead. Otherwise we might accidentally remove an
        // empty, reachable block and fail silently (at MIR generation time).
        func.blocks.retain(|&block| {
            let block = &self.read().code.blocks[block];
            !block.ops.is_empty()
        });

        for _ in 0..5 {
            self.write().remove_redundant_loads(func);
            self.write().remove_unused_allocas(func);
            self.write().remove_unused_values(func);
            self.write().remove_constant_branches(func);
            self.write().remove_redundant_blocks(func);
            self.write().remove_unreachable_blocks(func);
            if should_eval_constants {
                self.eval_constants(func, tp);
            }
        }
    }

    fn check_no_invalid_instructions(&self, func: &Function) {
        let d = self.read();
        for &block in &func.blocks {
            let block = &d.code.blocks[block];
            for &op in &block.ops {
                if matches!(d.code.ops[op].as_mir_instr().unwrap(), Instr::Invalid) {
                    panic!("Found invalid instruction in function");
                }
            }
        }
    }

    fn validate_function(&self, func: &Function) {
        self.read().code.mir_code.check_all_blocks_ended(func);
        self.check_no_invalid_instructions(func);
    }
}

impl Driver {
    fn generate_type_of(&self, instr: &Instr) -> Type {
        let b = &self.code.mir_code;
        match instr {
            Instr::Void | Instr::Store { .. } => Type::Void,
            Instr::Invalid => Type::Error,
            Instr::Pointer { .. } | Instr::Struct { .. } | Instr::GenericParam(_) | Instr::Enum { .. } | Instr::FunctionTy { .. } => Type::Ty,
            &Instr::StructLit { ref fields, id } => {
                let field_tys = fields.iter()
                    .map(|&op| {
                        let instr = self.code.ops[op].as_mir_instr().unwrap();
                        self.generate_type_of(instr)
                    })
                    .collect();
                Type::Struct(
                    StructType {
                        field_tys,
                        identity: id,
                    }
                )
            },
            Instr::Const(konst) => konst.ty(),
            Instr::Alloca(ty) => ty.clone().mut_ptr(),
            Instr::LogicalNot(_) => Type::Bool,
            &Instr::Call { func, .. } => b.functions[func].ty.return_ty.as_ref().clone(),
            &Instr::FunctionRef { func, .. } => Type::Function(b.functions[func].ty.clone()),
            Instr::ExternCall { func, .. } => b.extern_mods[&func.extern_mod].imported_functions[func.index].ty.return_ty.as_ref().clone(),
            Instr::Intrinsic { ty, .. } => ty.clone(),
            Instr::Reinterpret(_, ty) | Instr::Truncate(_, ty) | Instr::SignExtend(_, ty)
            | Instr::ZeroExtend(_, ty) | Instr::FloatCast(_, ty) | Instr::FloatToInt(_, ty)
            | Instr::IntToFloat(_, ty)
            => ty.clone(),
            &Instr::Load(instr) => match self.type_of(instr) {
                Type::Pointer(pointee) => pointee.ty.clone(),
                _ => Type::Error,
            },
            &Instr::AddressOfStatic(statik) => b.statics[statik].val.ty().mut_ptr(),
            Instr::Ret(_) | Instr::Br(_) | Instr::CondBr { .. } | Instr::SwitchBr { .. } => Type::Never,
            Instr::Parameter(ref ty) => ty.clone(),
            &Instr::DirectFieldAccess { val, index } => {
                let base_ty = self.type_of(val);
                match base_ty {
                    Type::Struct(strukt) => strukt.field_tys[index].clone(),
                    _ => panic!("Cannot directly access field of non-struct type {:?}!", base_ty),
                }
            },
            &Instr::IndirectFieldAccess { val, index } => {
                let base_ty = self.type_of(val).deref().unwrap();
                match base_ty.ty {
                    Type::Struct(ref strukt) => strukt.field_tys[index].clone().ptr_with_mut(base_ty.is_mut),
                    _ => panic!("Cannot directly access field of non-struct type {:?}!", base_ty),
                }
            },
            &Instr::Variant { enuum, .. } => Type::Enum(enuum),
            Instr::DiscriminantAccess { .. } => TYPE_OF_DISCRIMINANTS, // TODO: update this when discriminants can be other types
        }
    }

    fn push_instr(&mut self, b: &mut FunctionBuilder, instr: Instr, item: impl Into<ToSourceRange>) -> OpId {
        let instr_id = b.instrs.next();
        let ty = self.generate_type_of(&instr);
        let op = self.code.ops.push(Op::MirInstr(instr, instr_id, ty));
        let source_range = self.get_range(item);
        self.code.mir_code.source_ranges.insert(op, source_range);

        let block = &mut self.code.blocks[b.current_block];
        block.ops.push(op);

        op
    }

    fn push_instr_with_name(&mut self, b: &mut FunctionBuilder, instr: Instr, item: impl Into<ToSourceRange>, name: impl Into<String>) -> OpId {
        let instr_id = b.instrs.next();
        let ty = self.generate_type_of(&instr);
        let op = self.code.ops.push(Op::MirInstr(instr, instr_id, ty));
        let source_range = self.get_range(item);
        self.code.mir_code.source_ranges.insert(op, source_range);
        let name = b.instr_namespace.insert(name.into());
        self.code.mir_code.instr_names.insert(op, name);

        let block = &mut self.code.blocks[b.current_block];
        block.ops.push(op);

        op
    }
}

impl DriverRef<'_> {
    fn build_scope_item(&mut self, b: &mut FunctionBuilder, item: Item, tp: &impl TypeProvider) {
        let d = self.read();
        match item {
            Item::Expr(expr) => {
                drop(d);
                self.build_expr(b, expr, Context::new(0, DataDest::Void, ControlDest::Continue), tp);
            },
            Item::Decl(decl) => match df!(d, decl.hir) {
                hir::Decl::Stored { id, root_expr, .. } => {
                    drop(d);
                    let ty = tp.ty(root_expr).clone();
                    let name = format!("{}", self.read().display_item(decl));
                    let location = self.write().push_instr_with_name(b, Instr::Alloca(ty), decl, name);
                    b.stored_decl_locs.push_at(id, location);
                    self.build_expr(b, root_expr, Context::new(0, DataDest::Store { location }, ControlDest::Continue), tp);
                },
                hir::Decl::Computed { .. } => {},
                _ => panic!("Invalid scope item"),
            },
        }
    }

    fn build_scope(&mut self, b: &mut FunctionBuilder, scope: ImperScopeId, ctx: Context, tp: &impl TypeProvider) -> Value {
        self.write();
        let block = self.read().code.hir_code.imper_scopes[scope].block;
        let len = self.read().code.blocks[block].ops.len();
        for i in 0..len {
            let op = self.read().code.blocks[block].ops[i];
            let item = self.read().code.ops[op].as_hir_item().unwrap();
            self.build_scope_item(b, item, tp);
        }
        let terminal_expr = self.read().code.hir_code.imper_scopes[scope].terminal_expr;
        self.build_expr(b, terminal_expr, ctx, tp)
    }
}

impl Driver {
    fn get_base(&self, id: DeclRefId) -> ExprId {
        match self.code.hir_code.decl_refs[id].namespace {
            hir::Namespace::MemberRef { base_expr } => base_expr,
            _ => panic!("Expected member ref expression"),
        }
    }
}

impl DriverRef<'_> {
    fn get_callee_declref(&mut self, b: &mut FunctionBuilder, tp: &impl TypeProvider, callee_id: ExprId) -> DeclRef {
        let d = self.read();
        let callee = &ef!(d, callee_id.hir);
        if let &Expr::DeclRef { id, .. } = callee {
            drop(d);
            self.get(b, id, tp)
        } else {
            panic!("expected declref callee");
        }
    }

    fn get(&mut self, b: &mut FunctionBuilder, decl_ref_id: DeclRefId, tp: &impl TypeProvider) -> DeclRef {
        let id = tp.selected_overload(decl_ref_id).expect("No overload found!");
        let generic_arguments = tp.generic_arguments(decl_ref_id).as_ref().unwrap_or(&Vec::new()).clone();
        let expr = self.read().code.hir_code.decl_refs[decl_ref_id].expr;
        let name = format!("{}", self.read().display_item(id));
        match self.get_decl(id, tp) {
            Decl::Computed { get } => DeclRef::Function { func: get, generic_args: generic_arguments },
            Decl::ExternFunction(func) => {
                assert!(generic_arguments.is_empty());
                DeclRef::ExternFunction { func }
            },
            Decl::Stored(id) => {
                DeclRef::Value(b.stored_decl_locs[id].indirect())
            },
            Decl::PatternBinding { id: binding_id } => {
                DeclRef::Value(b.pattern_binding_locs[&binding_id])
            },
            Decl::Parameter { index } => {
                let entry_block = b.blocks[0];
                let value = self.read().code.blocks[entry_block].ops[index];
                DeclRef::Value(value.direct())
            },
            Decl::GenericParam(param) => {
                DeclRef::Value(self.write().push_instr(b, Instr::GenericParam(param), expr).direct())
            },
            Decl::Intrinsic(intr, ref ty) => {
                let ty = ty.clone();
                DeclRef::Intrinsic { intrinsic: intr, ty }
            },
            Decl::Const(ref konst) => {
                let konst = konst.clone();
                DeclRef::Value(self.write().push_instr_with_name(b, Instr::Const(konst.clone()), expr, name).direct())
            },
            Decl::Static(statik) => {
                DeclRef::Value(self.write().push_instr_with_name(b, Instr::AddressOfStatic(statik), expr, format!("static_{}", name)).indirect())
            },
            Decl::Field { index } => {
                let base = self.read().get_base(decl_ref_id);
                let base_ty = tp.ty(base);
                let mut base = self.build_expr(b, base, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                if matches!(base_ty, Type::Pointer(_)) {
                    base.indirection += 1;
                }
                if base.indirection > 0 {
                    let base_ptr = self.write().handle_indirection(b, base.get_address());
                    DeclRef::Value(self.write().push_instr(b, Instr::IndirectFieldAccess { val: base_ptr, index }, expr).indirect())
                } else {
                    debug_assert_eq!(base.indirection, 0, "tried to dereference a struct?!");
                    DeclRef::Value(self.write().push_instr(b, Instr::DirectFieldAccess { val: base.instr, index }, expr).direct())
                }
            },
            Decl::Variant { enuum, index, payload_ty } => {
                if payload_ty.is_some() {
                    DeclRef::EnumVariantWithPayload { enuum, index, payload_ty }
                } else {
                    DeclRef::Value(self.write().push_instr_with_name(b, Instr::Const(Const::BasicVariant { enuum, index }), expr, name).direct())
                }
            },
            Decl::Invalid => panic!("INVALID DECL"),
        }
    }

    fn build_if_expr_recurse(&mut self, b: &mut FunctionBuilder, condition: ExprId, then_scope: ImperScopeId, result_location: Option<OpId>, true_bb: BlockId, false_bb: BlockId, post_bb: BlockId, ctx: Context, tp: &impl TypeProvider) {
        self.build_expr(
            b,
            condition,
            Context::new(0, DataDest::Branch(true_bb, false_bb), ControlDest::Continue),
            tp,
        );
        self.write().start_bb(b, true_bb);
        let scope_ctx = ctx.redirect(result_location, Some(post_bb));
        self.build_scope(b, then_scope, scope_ctx.clone(), tp);
    }
}

impl DriverRef<'_> {
    fn build_if_expr(&mut self, b: &mut FunctionBuilder, expr: ExprId, ty: Type, condition: ExprId, then_scope: ImperScopeId, else_scope: Option<ImperScopeId>, ctx: Context, tp: &impl TypeProvider) -> Value {
        // Create a location on the stack to store the result of the if, if necessary
        let result_location = match (&ctx.data, else_scope) {
            (DataDest::Read, Some(_)) => Some(
                // TODO: this will be the wrong type if indirection != 0
                self.write().push_instr(b, Instr::Alloca(ty.clone()), expr)
            ),
            _ => None,
        };

        let true_bb = self.write().create_bb(b);
        let false_bb = self.write().create_bb(b);
        let post_bb = if else_scope.is_some() {
            self.write().create_bb(b)
        } else {
            false_bb
        };

        self.build_if_expr_recurse(b, condition, then_scope, result_location, true_bb, false_bb, post_bb, ctx, tp);
        let mut next_scope = else_scope;
        let mut next_bb = false_bb;

        // Iterate through a linked list of if-else-if branches. Terminate when you find an if with no else branch or
        // an else branch with something other than an if.
        // This is done to reduce the size of the stack when generating code for long chains of if statements.
        while let Some(cur) = next_scope {
            self.write().start_bb(b, next_bb);

            let scope_ctx = ctx.redirect(result_location, Some(post_bb));
            let terminal_expr = self.read().code.hir_code.imper_scopes[cur].terminal_expr;
            let block = self.read().code.hir_code.imper_scopes[cur].block;
            // If the current scope consists of a lone if expression
            if self.read().code.blocks[block].ops.is_empty() {
                let d = self.read();
                if let Expr::If { condition, then_scope, else_scope } = ef!(d, terminal_expr.hir) {
                    drop(d);
                    let true_bb = self.write().create_bb(b);
                    let false_bb = self.write().create_bb(b);
                    self.build_if_expr_recurse(b, condition, then_scope, result_location, true_bb, false_bb, post_bb, scope_ctx, tp);
                    next_scope = else_scope;
                    next_bb = false_bb;
                    continue;
                }
            }

            self.build_scope(b, cur, scope_ctx, tp);
            next_scope = None;
        }

        self.write().start_bb(b, post_bb);
        if let Some(location) = result_location {
            self.write().push_instr(b, Instr::Load(location), expr).direct()
        } else if else_scope.is_none() {
            self.handle_context(b, VOID_INSTR.direct(), ctx, tp)
        } else {
            VOID_INSTR.direct()
        }
    }

    fn build_expr(&mut self, b: &mut FunctionBuilder, expr: ExprId, ctx: Context, tp: &impl TypeProvider) -> Value {
        let ty = tp.ty(expr).clone();
        let d = self.read();
        // TODO: in every single case of this match, I have to call drop() on d, otherwise the Ref<Driver> will
        // conflict with mutable uses of self.
        // Fix this, somehow. I don't have the faintest idea how.
        let val = match ef!(d, expr.hir) {
            Expr::Void | Expr::Error => {
                drop(d);
                VOID_INSTR.direct()
            },
            Expr::IntLit { .. } | Expr::DecLit { .. } | Expr::CharLit { .. } | Expr::StrLit { .. } | Expr::BoolLit { .. } | Expr::ConstTy(_) | Expr::Mod { .. } | Expr::Import { .. } => {
                drop(d);
                let konst = self.write().expr_to_const(expr, ty.clone());
                let name = format!("{}", self.read().fmt_const_for_instr_name(&konst));
                self.write().push_instr_with_name(b, Instr::Const(konst), expr, name).direct()
            },
            Expr::Set { lhs, rhs } => {
                drop(d);
                return self.build_expr(
                    b,
                    rhs,
                    ctx.new_data_dest(DataDest::Set { dest: lhs }),
                    tp,
                )
            },
            Expr::DeclRef { id, .. } => {
                drop(d);
                let decl_ref = self.get(b, id, tp);

                match decl_ref {
                    DeclRef::Function { func, generic_args } => {
                        self.write().push_instr(b, Instr::FunctionRef { func, generic_arguments: generic_args }, expr).direct()
                    },
                    DeclRef::Intrinsic { intrinsic, ty } => {
                        assert!(tp.ty(expr).return_ty().is_none(), "referring to intrinsic functions is not yet supported");
                        self.write().push_instr(b, Instr::Intrinsic { arguments: SmallVec::new(), ty, intr: intrinsic }, expr).direct()
                    },
                    DeclRef::Value(value) => value,
                    other => todo!("referring to {:?} not yet supported", other),
                }
            },
            Expr::Call { callee, ref arguments, .. } => {
                let arguments = arguments.clone();
                drop(d);
                let decl_ref = self.get_callee_declref(b, tp, callee);

                fn get_args(d: &mut DriverRef, b: &mut FunctionBuilder, tp: &impl TypeProvider, arguments: &[ExprId]) -> SmallVec<[OpId; 2]> {
                    arguments.iter().map(|&argument| {
                        let val = d.build_expr(b, argument, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                        d.write().handle_indirection(b, val)
                    }).collect()
                }

                match decl_ref {
                    DeclRef::Function { func, generic_args } => {
                        let arguments = get_args(self, b, tp, &arguments);
                        self.write().push_instr(b, Instr::Call { func, arguments, generic_arguments: generic_args }, expr).direct()
                    },
                    DeclRef::ExternFunction { func } => {
                        let arguments = get_args(self, b, tp, &arguments);
                        self.write().push_instr(b, Instr::ExternCall { arguments, func }, expr).direct()
                    },
                    DeclRef::Intrinsic { intrinsic, ty } => match intrinsic {
                        Intrinsic::LogicalAnd => {
                            assert_eq!(arguments.len(), 2);
                            let (lhs, rhs) = (arguments[0], arguments[1]);
                            let left_true_bb = self.write().create_bb(b);
                            let location = if let DataDest::Read = ctx.data {
                                Some(self.write().push_instr(b, Instr::Alloca(ty.clone()), expr))
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

                                self.write().start_bb(b, left_true_bb);
                                return self.build_expr(
                                    b,
                                    rhs,
                                    Context::new(0, DataDest::Branch(true_bb, false_bb), ControlDest::Continue),
                                    tp,
                                );
                            } else {
                                let left_false_bb = self.write().create_bb(b);
                                let after_bb = self.write().create_bb(b);
                                self.build_expr(
                                    b,
                                    lhs,
                                    Context::new(0, DataDest::Branch(left_true_bb, left_false_bb), ControlDest::Continue),
                                    tp,
                                );

                                self.write().start_bb(b, left_true_bb);
                                // No further branching required, because (true && foo) <=> foo
                                let branch_ctx = ctx.redirect(location, Some(after_bb));
                                self.build_expr(b, rhs, branch_ctx.clone(), tp);

                                self.write().start_bb(b, left_false_bb);
                                let false_const = Const::Bool(false);
                                let name = format!("{}", self.read().fmt_const_for_instr_name(&false_const));
                                let false_val = self.write().push_instr_with_name(b, Instr::Const(false_const), expr, name).direct();
                                self.handle_context(b, false_val, branch_ctx, tp);

                                self.write().start_bb(b, after_bb);
                                if let Some(location) = location {
                                    self.write().push_instr(b, Instr::Load(location), expr).direct()
                                } else {
                                    return VOID_INSTR.direct()
                                }
                            }
                        },
                        Intrinsic::LogicalOr => {
                            assert_eq!(arguments.len(), 2);
                            let (lhs, rhs) = (arguments[0], arguments[1]);
                            let left_false_bb = self.write().create_bb(b);
                            if let DataDest::Branch(true_bb, false_bb) = ctx.data {
                                self.build_expr(
                                    b,
                                    lhs,
                                    Context::new(0, DataDest::Branch(true_bb, left_false_bb), ControlDest::Continue),
                                    tp,
                                );

                                self.write().start_bb(b, left_false_bb);
                                return self.build_expr(
                                    b,
                                    rhs,
                                    Context::new(0, DataDest::Branch(true_bb, false_bb), ControlDest::Continue),
                                    tp
                                );
                            } else {
                                let left_true_bb = self.write().create_bb(b);
                                let after_bb = self.write().create_bb(b);
                                let location = if let DataDest::Read = ctx.data {
                                    Some(self.write().push_instr(b, Instr::Alloca(ty.clone()), expr))
                                } else {
                                    None
                                };
                                self.build_expr(
                                    b,
                                    lhs,
                                    Context::new(0, DataDest::Branch(left_true_bb, left_false_bb), ControlDest::Continue),
                                    tp,
                                );

                                self.write().start_bb(b, left_true_bb);
                                let true_const = Const::Bool(true);
                                let name = format!("{}", self.read().fmt_const_for_instr_name(&true_const));
                                let true_val = self.write().push_instr_with_name(b, Instr::Const(true_const), expr, name).direct();
                                let branch_ctx = ctx.redirect(location, Some(after_bb));
                                self.handle_context(b, true_val, branch_ctx.clone(), tp);

                                self.write().start_bb(b, left_false_bb);
                                self.build_expr(b, rhs, branch_ctx, tp);

                                self.write().start_bb(b, after_bb);
                                if let Some(location) = location {
                                    self.write().push_instr(b, Instr::Load(location), expr).direct()
                                } else {
                                    return VOID_INSTR.direct()
                                }
                            }
                        },
                        Intrinsic::LogicalNot => {
                            assert_eq!(arguments.len(), 1);
                            let operand = arguments[0];
                            if let DataDest::Branch(true_bb, false_bb) = ctx.data {
                                return self.build_expr(b, operand, Context::new(0, DataDest::Branch(false_bb, true_bb), ctx.control), tp)
                            } else {
                                let operand = self.build_expr(b, operand, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                                self.write().push_instr(b, Instr::LogicalNot(operand.instr), expr).direct()
                            }
                        },
                        Intrinsic::MultAssign | Intrinsic::DivAssign | Intrinsic::ModAssign | Intrinsic::AddAssign 
                            | Intrinsic::SubAssign | Intrinsic::AndAssign | Intrinsic::OrAssign | Intrinsic::XorAssign
                            | Intrinsic::LeftShiftAssign | Intrinsic::RightShiftAssign => {
                            assert_eq!(arguments.len(), 2);
                            let lhs = arguments[0];
                            let rhs = arguments[1];

                            let ty = tp.ty(lhs).clone();

                            let address = self.build_expr(b, lhs, Context::new(1, DataDest::Read, ControlDest::Continue), tp);
                            let address = self.write().handle_indirection(b, address);
                            let loaded = self.write().push_instr(b, Instr::Load(address), lhs);
                            let modifier = self.build_expr(b, rhs, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                            let modifier = self.write().handle_indirection(b, modifier);
                            let intr = match intrinsic {
                                Intrinsic::MultAssign => Intrinsic::Mult,
                                Intrinsic::DivAssign => Intrinsic::Div,
                                Intrinsic::ModAssign => Intrinsic::Mod,
                                Intrinsic::AddAssign => Intrinsic::Add,
                                Intrinsic::SubAssign => Intrinsic::Sub,
                                Intrinsic::AndAssign => Intrinsic::BitwiseAnd,
                                Intrinsic::OrAssign => Intrinsic::BitwiseOr,
                                Intrinsic::XorAssign => Intrinsic::BitwiseXor,
                                Intrinsic::LeftShiftAssign => Intrinsic::LeftShift,
                                Intrinsic::RightShiftAssign => Intrinsic::RightShift,
                                intrinsic => todo!("intrinsic {:?}", intrinsic),
                            };
                            let value = self.write().push_instr(b, Instr::Intrinsic { arguments: smallvec![loaded, modifier], ty, intr }, expr);
                            self.write().push_instr(b, Instr::Store { location: address, value }, expr).direct()
                        },
                        intrinsic => {
                            let arguments = get_args(self, b, tp, &arguments);
                            self.write().push_instr(b, Instr::Intrinsic { arguments, ty, intr: intrinsic }, expr).direct()
                        }
                    },
                    DeclRef::EnumVariantWithPayload { enuum, index, .. } => {
                        let arguments = get_args(self, b, tp, &arguments);
                        self.write().push_instr(b, Instr::Variant { enuum, index, payload: arguments[0] }, expr).direct()
                    },
                    DeclRef::Value(_) => todo!("calling function pointers is not yet supported"),
                }
            },
            Expr::Cast { expr: operand, ty: dest_ty, cast_id } => {
                let dest_ty = tp.get_evaluated_type(dest_ty).clone();
                drop(d);
                match tp.cast_method(cast_id) {
                    CastMethod::Noop => return self.build_expr(b, operand, ctx, tp),
                    CastMethod::Reinterpret => {
                        let value = self.build_expr(b, operand, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                        let value = self.write().handle_indirection(b, value);
                        self.write().push_instr(b, Instr::Reinterpret(value, dest_ty), expr).direct()
                    },
                    CastMethod::Int => {
                        let (src_width, _src_is_signed, dest_width, dest_is_signed) = match (tp.ty(operand), &dest_ty) {
                            (&Type::Int { width: ref src_width, is_signed: src_is_signed }, &Type::Int { width: ref dest_width, is_signed: dest_is_signed })
                                => (src_width.clone(), src_is_signed, dest_width.clone(), dest_is_signed),
                            (a, b) => panic!("Internal compiler error: found invalid cast types while generating MIR ({:?}, {:?})", a, b)
                        };
                        let (src_bit_width, dest_bit_width) = (src_width.bit_width(self.read().arch), dest_width.bit_width(self.read().arch));
                        let value = self.build_expr(b, operand, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                        let value = self.write().handle_indirection(b, value);

                        if src_bit_width == dest_bit_width {
                            // TODO: Bounds checking
                            self.write().push_instr(b, Instr::Reinterpret(value, dest_ty), expr)
                        } else if src_bit_width < dest_bit_width {
                            if dest_is_signed {
                                // TODO: Bounds checking
                                self.write().push_instr(b, Instr::SignExtend(value, dest_ty), expr)
                            } else {
                                // TODO: Bounds checking
                                self.write().push_instr(b, Instr::ZeroExtend(value, dest_ty), expr)
                            }
                        } else if src_bit_width > dest_bit_width {
                            // TODO: Bounds checking
                            self.write().push_instr(b, Instr::Truncate(value, dest_ty), expr)
                        } else {
                            unreachable!()
                        }.direct()
                    },
                    CastMethod::Float => {
                        let value = self.build_expr(b, operand, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                        let value = self.write().handle_indirection(b, value);
                        self.write().push_instr(b, Instr::FloatCast(value, dest_ty), expr).direct()
                    },
                    CastMethod::FloatToInt => {
                        let value = self.build_expr(b, operand, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                        let value = self.write().handle_indirection(b, value);
                        self.write().push_instr(b, Instr::FloatToInt(value, dest_ty), expr).direct()
                    },
                    CastMethod::IntToFloat => {
                        let value = self.build_expr(b, operand, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                        let value = self.write().handle_indirection(b, value);
                        self.write().push_instr(b, Instr::IntToFloat(value, dest_ty), expr).direct()
                    },
                    CastMethod::Invalid => panic!("FOUND INVALID CAST"),
                }
            },
            Expr::AddrOf { expr: operand, .. } => {
                drop(d);
                return self.build_expr(
                    b,
                    operand,
                    Context::new(ctx.indirection + 1, ctx.data, ctx.control),
                    tp,
                )
            },
            Expr::Pointer { expr: operand, is_mut } => {
                drop(d);
                let op = self.build_expr(
                    b,
                    operand,
                    Context::new(0, DataDest::Read, ControlDest::Continue),
                    tp,
                );
                let op = self.write().handle_indirection(b, op);
                self.write().push_instr(b, Instr::Pointer { op, is_mut }, expr).direct()
            },
            Expr::FunctionTy { ref param_tys, ret_ty } => {
                let param_tys = param_tys.clone();
                drop(d);
                let param_tys: Vec<_> = param_tys.iter()
                    .map(|&ty| {
                        let param_ty = self.build_expr(
                            b,
                            ty,
                            Context::new(0, DataDest::Read, ControlDest::Continue),
                            tp,
                        );
                        self.write().handle_indirection(b, param_ty)
                    }).collect();
                let ret_ty = self.build_expr(
                    b,
                    ret_ty,
                    Context::new(0, DataDest::Read, ControlDest::Continue),
                    tp,
                );
                let ret_ty = self.write().handle_indirection(b, ret_ty);

                self.write().push_instr(b, Instr::FunctionTy { param_tys, ret_ty }, expr).direct()
            }
            Expr::Struct(id) => {
                drop(d);
                let mut fields = SmallVec::new();
                let len = self.read().code.hir_code.structs[id].fields.len();
                for i in 0..len {
                    let field_ty = self.read().code.hir_code.structs[id].fields[i].ty;
                    let field = self.build_expr(
                        b,
                        field_ty,
                        Context::new(0, DataDest::Read, ControlDest::Continue),
                        tp,
                    );
                    let field = self.write().handle_indirection(b, field);
                    fields.push(field);
                }
                self.write().push_instr(b, Instr::Struct { fields, id }, expr).direct()
            },
            Expr::Enum(id) => {
                drop(d);
                let mut variants = SmallVec::new();
                let len = self.read().code.hir_code.enums[id].variants.len();
                for i in 0..len {
                    let payload_ty = self.read().code.hir_code.enums[id].variants[i].payload_ty.unwrap_or(VOID_TYPE);
                    let variant = self.build_expr(
                        b,
                        payload_ty,
                        Context::new(0, DataDest::Read, ControlDest::Continue),
                        tp,
                    );
                    let variant = self.write().handle_indirection(b, variant);
                    variants.push(variant);
                }
                self.write().push_instr(b, Instr::Enum { variants, id }, expr).direct()
            },
            Expr::StructLit { id, .. } => {
                drop(d);
                let lit = tp.struct_lit(id).as_ref().unwrap();
                let mut fields = SmallVec::new();
                for &field in &lit.fields {
                    let field = self.build_expr(
                        b,
                        field,
                        Context::new(0, DataDest::Read, ControlDest::Continue),
                        tp,
                    );
                    let field = self.write().handle_indirection(b, field);
                    fields.push(field);
                }
                self.write().push_instr(b, Instr::StructLit { fields, id: lit.strukt }, expr).direct()
            },
            Expr::Deref(operand) => {
                drop(d);
                return self.build_expr(
                    b,
                    operand,
                    Context::new(ctx.indirection - 1, ctx.data, ctx.control),
                    tp,
                )
            },
            Expr::Do { scope } => {
                drop(d);
                return self.build_scope(b, scope, ctx, tp)
            },
            Expr::If { condition, then_scope, else_scope } => {
                drop(d);
                return self.build_if_expr(b, expr, ty, condition, then_scope, else_scope, ctx, tp)
            },
            Expr::Switch { scrutinee, ref cases } => {
                let cases = cases.clone();
                drop(d);
                let scrutinee_val = self.build_expr(b, scrutinee, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                let scrutinee_ty = tp.ty(scrutinee);
                let discriminant = match scrutinee_ty {
                    Type::Enum(_) => self.write().get_discriminant(b, scrutinee_val),
                    Type::Int { .. } => self.write().handle_indirection(b, scrutinee_val),
                    _ => todo!(),
                };
                let result_location = match &ctx.data {
                    DataDest::Read => Some(
                        // TODO: this will be the wrong type if indirection != 0
                        self.write().push_instr(b, Instr::Alloca(ty.clone()), expr)
                    ),
                    _ => None,
                };
                let begin_bb = b.current_block;
                let post_bb = self.write().create_bb(b);
                let scope_ctx = ctx.redirect(result_location, Some(post_bb));
                let mut mir_cases = Vec::new();
                let enum_info = match scrutinee_ty {
                    &Type::Enum(id) => {
                        Some((id, self.read().code.hir_code.enums[id].variants.clone()))
                    },
                    Type::Int { .. } => None,
                    _ => todo!(),
                };
                let mut catch_all_bb = None;
                for case in cases {
                    let case_bb = self.write().create_bb(b);
                    self.write().start_bb(b, case_bb);
                    // Can only bind at most one binding at a time. The exception is when they alias. E.g., '.a(int_val) & enum_val' <- int_val and enum_val are two bindings that alias.
                    // I will need some way of detecting aliases and allowing them, but at the same time splitting up the list of bindings when they don't alias, such as in disjuction patterns.
                    assert!(case.pattern.bindings.len() <= 1);
                    for &binding_id in &case.pattern.bindings {
                        let binding = self.read().code.hir_code.pattern_binding_decls[binding_id].clone();
                        // Can only ever bind one path to a particular binding at a time. Will need some way of splitting these up when I implement disjunction patterns
                        assert_eq!(binding.paths.len(), 1);
                        for path in &binding.paths {
                            let val = self.write().handle_indirection(b, scrutinee_val);
                            for step in &path.components {
                                match step {
                                    &PatternBindingPathComponent::VariantPayload(_index) => {
                                        todo!();
                                    }
                                }
                            }
                            b.pattern_binding_locs.insert(binding_id, val.direct());
                        }
                    }
                    self.build_scope(b, case.scope, scope_ctx, tp);

                    self.write().start_bb(b, begin_bb);
                    match case.pattern.kind {
                        hir::PatternKind::ContextualMember { name, .. } => {
                            let (enum_id, variants) = enum_info.as_ref().expect("found contextual member pattern on non-enum type. typechecker should've caught this.");
                            let mut index = None;
                            for (i, variant) in variants.iter().enumerate() {
                                if variant.name == name.symbol {
                                    index = Some(i);
                                    break;
                                }
                            }
                            let index = index.expect("Unrecognized variant in switch case. Typechecker should have caught this.");
                            let discriminant = self.read().get_const_discriminant(*enum_id, index);
                            mir_cases.push(
                                SwitchCase {
                                    value: discriminant,
                                    bb: case_bb,
                                }
                            );
                        },
                        hir::PatternKind::IntLit { value, .. } => {
                            mir_cases.push(
                                SwitchCase {
                                    value: Const::Int { lit: BigInt::from(value), ty: scrutinee_ty.clone() },
                                    bb: case_bb,
                                }
                            );
                        }
                        hir::PatternKind::NamedCatchAll(_) | hir::PatternKind::AnonymousCatchAll(_) => {
                            catch_all_bb = Some(case_bb);
                        },
                    }
                }

                let catch_all_bb = if let Some(catch_all_bb) = catch_all_bb {
                    catch_all_bb
                } else {
                    let catch_all_bb = self.write().create_bb(b);
                    self.write().start_bb(b, catch_all_bb);
                    // TODO: add unreachable instruction I guess?
                    self.write().push_instr(b, Instr::Intrinsic { arguments: SmallVec::new(), ty: Type::Never, intr: Intrinsic::Panic }, SourceRange::default());
                    self.write().end_current_bb(b);
                    catch_all_bb
                };
                
                self.write().start_bb(b, begin_bb);
                self.write().push_instr(b, Instr::SwitchBr { scrutinee: discriminant, cases: mir_cases, catch_all_bb }, expr);
                self.write().end_current_bb(b);

                self.write().start_bb(b, post_bb);
                if let Some(location) = result_location {
                    return self.write().push_instr(b, Instr::Load(location), expr).direct()
                } else {
                    return self.write().handle_control(b, VOID_INSTR.direct(), ctx.control)
                }
            },
            Expr::While { condition, scope } => {
                drop(d);
                let test_bb = self.write().create_bb(b);
                let loop_bb = self.write().create_bb(b);
                let post_bb = match ctx.control {
                    ControlDest::Continue | ControlDest::Unreachable | ControlDest::RetVoid => self.write().create_bb(b),
                    ControlDest::Block(block) => block,
                };

                self.write().push_instr(b, Instr::Br(test_bb), expr);
                self.write().end_current_bb(b);
                self.write().start_bb(b, test_bb);
                self.build_expr(b, condition, Context::new(0, DataDest::Branch(loop_bb, post_bb), ControlDest::Continue), tp);

                self.write().start_bb(b, loop_bb);
                self.build_scope(b, scope, Context::new(0, DataDest::Void, ControlDest::Block(test_bb)), tp);

                match ctx.control {
                    ControlDest::Continue | ControlDest::Unreachable | ControlDest::RetVoid => {
                        self.write().start_bb(b, post_bb);
                        VOID_INSTR.direct()
                    },
                    // Already handled this above
                    ControlDest::Block(_) => return VOID_INSTR.direct(),
                }
            },
            Expr::Ret { expr, .. } => {
                drop(d);
                return self.build_expr(
                    b,
                    expr,
                    Context::new(0, DataDest::Ret, ctx.control),
                    tp,
                );
            },
        };
        self.handle_context(b, val, ctx, tp)
    }
}
impl Driver {
    fn get_const_discriminant(&self, _enum_id: EnumId, variant_index: usize) -> Const {
        // TODO: other discriminant types and custom values
        // Delete TYPE_OF_DISCRIMINANTS to deal with other cases
        Const::Int { lit: BigInt::from(variant_index), ty: TYPE_OF_DISCRIMINANTS }
    }

    fn get_discriminant(&mut self, b: &mut FunctionBuilder, val: Value) -> OpId {
        // TODO: handle indirect enum discriminant accesses, without loading the entire value.
        let val = self.handle_indirection(b, val);
        self.push_instr_with_name(b, Instr::DiscriminantAccess { val }, val, format!("{}.disc", self.display_instr_name(val)))
    }

    fn handle_indirection(&mut self, b: &mut FunctionBuilder, mut val: Value) -> OpId {
        if val.indirection > 0 {
            while val.indirection > 0 {
                val.instr = self.push_instr(b, Instr::Load(val.instr), val.instr);
                val.indirection -= 1;
            }
        } else if val.indirection < 0 {
            let mut ty = self.type_of(val.instr).clone();
            while val.indirection < 0 {
                let location = self.push_instr(b, Instr::Alloca(ty.clone()), val.instr);
                self.push_instr(b, Instr::Store { location, value: val.instr }, val.instr);
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
            ControlDest::Block(block) => {
                let val = self.push_instr(b, Instr::Br(block), val.instr).direct();
                self.end_current_bb(b);
                val
            },
            ControlDest::Continue => val,
            ControlDest::RetVoid => {
                let val = self.push_instr(b, Instr::Ret(VOID_INSTR), val.instr).direct();
                self.end_current_bb(b);
                val
            },
            ControlDest::Unreachable => VOID_INSTR.direct(),
        }
    }
}

impl DriverRef<'_> {
    fn handle_context(&mut self, b: &mut FunctionBuilder, mut val: Value, ctx: Context, tp: &impl TypeProvider) -> Value {
        val = val.adjusted(ctx.indirection);
        match ctx.data {
            DataDest::Read => return val,
            DataDest::Ret => {
                let instr = self.write().handle_indirection(b, val);
                let val = self.write().push_instr(b, Instr::Ret(instr), instr).direct();
                self.write().end_current_bb(b);
                return val;
            },
            DataDest::Branch(true_bb, false_bb) => {
                let op = self.write().handle_indirection(b, val);
                let instr = if let &Instr::Const(Const::Bool(val)) = self.read().code.ops[op].as_mir_instr().unwrap() {
                    let bb = [false_bb, true_bb][val as usize];
                    Instr::Br(bb)
                } else {
                    Instr::CondBr { condition: op, true_bb, false_bb }
                };
                let val = self.write().push_instr(b, instr, op).direct();
                self.write().end_current_bb(b);
                return val;
            },
            DataDest::Receive { value } => {
                let location = self.write().handle_indirection(b, val.get_address());
                let range = self.read().get_range(location) + self.read().get_range(value);
                self.write().push_instr(b, Instr::Store { location, value }, range);
            },
            DataDest::Store { location } => {
                let instr = self.write().handle_indirection(b, val);
                let range = self.read().get_range(location) + self.read().get_range(instr);
                self.write().push_instr(b, Instr::Store { location, value: instr }, range);
            },
            DataDest::Set { dest } => {
                let instr = self.write().handle_indirection(b, val);
                return self.build_expr(
                    b,
                    dest,
                    Context::new(0, DataDest::Receive { value: instr }, ctx.control),
                    tp,
                );
            }
            DataDest::Void => {},
        }

        self.write().handle_control(b, val, ctx.control)
    }
}
