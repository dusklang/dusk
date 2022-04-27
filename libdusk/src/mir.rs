use std::collections::HashMap;
use std::ffi::CString;
use std::ops::Range;

use dire::index_counter::IndexCounter;
use smallvec::SmallVec;
use string_interner::DefaultSymbol as Sym;
use display_adapter::display_adapter;

use dire::hir::{self, DeclId, ExprId, EnumId, DeclRefId, ImperScopeId, Intrinsic, Expr, StoredDeclId, GenericParamId, Item, PatternBindingDeclId, ExternModId, ExternFunctionRef, PatternBindingPathComponent, VOID_TYPE};
use dire::mir::{FuncId, StaticId, Const, Instr, InstrId, Function, MirCode, StructLayout, EnumLayout, ExternMod, ExternFunction, InstrNamespace, SwitchCase, VOID_INSTR};
use dire::{Block, BlockId, Op, OpId};
use dire::ty::{Type, FloatWidth};
use dire::source_info::SourceRange;

use crate::driver::Driver;
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

#[derive(Copy, Clone)]
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
                    Type::Int { .. } => Const::Int { lit, ty },
                    Type::Float(_)   => Const::Float { lit: lit as f64, ty },
                    _ => panic!("Unrecognized integer literal type {:?}", ty),
                }
            },
            Expr::DecLit { lit } => Const::Float { lit, ty },
            Expr::StrLit { ref lit } => {
                let id = self.code.mir_code.strings.push(lit.clone());
                Const::Str { id, ty }
            },
            Expr::CharLit { lit } => match ty {
                Type::Int { .. } => Const::Int { lit: lit as u64, ty },
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
            &Type::Struct(id) => self.code.mir_code.structs[&id].layout.size,
            &Type::Enum(_id) => 4,
            Type::GenericParam(_) => panic!("can't get size of generic parameter without more context"),
        }
    }

    /// Stride of an instance of a type in bytes
    pub fn stride_of(&self, ty: &Type) -> usize {
        match *ty {
            Type::Struct(id) => self.code.mir_code.structs[&id].layout.stride,
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
        match *ty {
            Type::Struct(id) => self.code.mir_code.structs[&id].layout.alignment,
            Type::Enum(_) => self.stride_of(ty),
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

    pub fn build_mir(&mut self, tp: &impl TypeProvider) {
        // Start at 1 to avoid RETURN_VALUE_DECL, which we can't and shouldn't generate code for
        for i in 1..self.code.hir_code.decls.len() {
            self.get_decl(DeclId::new(i), tp);
        }

        for i in 0..self.mir.statics.len() {
            let id = StaticId::new(i);
            let statik = self.mir.statics[id].clone();
            let konst = self.eval_expr(statik.assignment, tp);
            self.code.mir_code.statics.push_at(
                id,
                dire::mir::Static {
                    name: statik.name,
                    val: konst.clone(),
                }
            );
        }
    }

    pub fn build_standalone_expr(&mut self, expr: ExprId, tp: &impl TypeProvider) -> Function {
        self.build_function(None, tp.ty(expr).clone(), FunctionBody::Expr(expr), DeclId::new(0)..DeclId::new(0), Vec::new(), tp)
    }

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
                    param_tys,
                    return_ty,
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

    fn get_decl(&mut self, id: DeclId, tp: &impl TypeProvider) -> Decl {
        if let Some(decl) = self.mir.decls.get(&id) { return decl.clone(); }
        match df!(id.hir) {
            hir::Decl::Computed { ref params, scope, generic_params: ref generic_params_range, .. } => {
                // Add placeholder function to reserve ID ahead of time
                let get = self.code.mir_code.functions.push(Function::default());
                let decl = Decl::Computed { get };
                self.mir.decls.insert(id, decl.clone());

                // Convert DeclIds to GenericParamIds
                // TODO: don't require this?
                let mut generic_params = Vec::new();
                generic_params.reserve(generic_params_range.end.index() - generic_params_range.start.index());
                for i in generic_params_range.start.index()..generic_params_range.end.index() {
                    let id = DeclId::new(i);
                    let generic_param = match df!(id.hir) {
                        hir::Decl::GenericParam(param) => param,
                        _ => panic!("unexpected decl type, expected generic parameter"),
                    };
                    generic_params.push(generic_param);
                }

                let params = params.clone();
                let func = self.build_function(
                    Some(self.code.hir_code.names[id]),
                    self.decl_type(id, tp).clone(),
                    FunctionBody::Scope { scope, decl: id },
                    params,
                    generic_params,
                    tp
                );
                self.code.mir_code.functions[get] = func;
                decl
            },
            hir::Decl::ComputedPrototype { extern_func, .. } => {
                let decl = if let Some(extern_func) = extern_func {
                    self.resolve_extern_mod(extern_func.extern_mod, tp);
                    Decl::ExternFunction(extern_func)
                } else {
                    let err = Error::new("cannot declare prototype outside of extern module")
                        .adding_primary_range(df!(id.range), "prototype here");
                    self.errors.push(err);
                    Decl::Invalid
                };
                self.mir.decls.insert(id, decl.clone());
                decl
            },
            hir::Decl::Stored { id: index, .. } => {
                let decl = Decl::Stored(index);
                self.mir.decls.insert(id, decl.clone());
                decl
            },
            hir::Decl::Parameter { index } => {
                let decl = Decl::Parameter { index };
                self.mir.decls.insert(id, decl.clone());
                decl
            },
            hir::Decl::PatternBinding { id: index, .. } => {
                let decl = Decl::PatternBinding { id: index };
                self.mir.decls.insert(id, decl.clone());
                decl
            },
            hir::Decl::Intrinsic { intr, .. } => {
                let decl = Decl::Intrinsic(intr, self.decl_type(id, tp).clone());
                self.mir.decls.insert(id, decl.clone());
                decl
            },
            hir::Decl::Static(expr) => {
                let name = format!("{}", self.display_item(id));
                let decl = Decl::Static(StaticId::new(self.mir.statics.len()));
                self.mir.decls.insert(id, decl.clone());
                self.mir.statics.push(
                    Static {
                        name,
                        assignment: expr,
                    }
                );
                decl
            },
            hir::Decl::Const(root_expr) => {
                let konst = self.eval_expr(root_expr, tp);

                // TODO: Deal with cycles!
                let decl = Decl::Const(konst);
                self.mir.decls.insert(id, decl.clone());
                decl
            },
            hir::Decl::Field { index, .. } => {
                let decl = Decl::Field { index };
                self.mir.decls.insert(id, decl.clone());
                decl
            },
            hir::Decl::Variant { enuum, index, payload_ty } => {
                let payload_ty = payload_ty.map(|ty| tp.get_evaluated_type(ty).clone());
                Decl::Variant { enuum, index, payload_ty }
            },
            hir::Decl::GenericParam(param) => {
                let decl = Decl::GenericParam(param);
                self.mir.decls.insert(id, decl.clone());
                decl
            },
            hir::Decl::ReturnValue => panic!("Can't get_decl() the return_value decl"),
        }
    }

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
            Const::Int { lit, ref ty } => write!(f, "{} as {:?}", lit, ty)?,
            Const::Str { id, ref ty } => write!(f, "%str{} ({:?}) as {:?}", id.index(), self.code.mir_code.strings[id], ty)?,
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
            Const::Int { lit, .. } => write!(f, "const_{}", lit)?,
            Const::Str { id, .. } => write!(f, "string_{}", identifierify(self.code.mir_code.strings[id].clone().into_bytes()))?,
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
    pub fn display_mir(&self, f: &mut Formatter) {
        if !self.code.mir_code.statics.raw.is_empty() {
            for statik in &self.code.mir_code.statics {
                writeln!(f, "%{} = {}", statik.name, self.fmt_const(&statik.val))?;
            }
            writeln!(f)?;
        }

        for (i, func) in self.code.mir_code.functions.iter().enumerate() {
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
            writeln!(f, "): {:?} {{", func.ret_ty)?;
            for i in 0..func.blocks.len() {
                let block_id = func.blocks[i];
                writeln!(f, "%bb{}:", block_id.index())?;
                let block = &self.code.blocks[func.blocks[i]];
                let mut start = 0;
                for (i, &op) in block.ops.iter().enumerate() {
                    let instr = self.code.ops[op].as_mir_instr().unwrap();
                    if !matches!(instr, Instr::Parameter(_)) {
                        start = i;
                        break;
                    }
                }
                
                for &op_id in &block.ops[start..] {
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
                        Instr::Alloca(ty) => writeln!(f, "%{} = alloca {:?}", self.display_instr_name(op_id), ty)?,
                        Instr::Br(block) => writeln!(f, "br %bb{}", block.index())?,
                        &Instr::CondBr { condition, true_bb, false_bb }
                            => writeln!(f, "condbr %{}, %bb{}, %bb{}", self.display_instr_name(condition), true_bb.index(), false_bb.index())?,
                        &Instr::SwitchBr { scrutinee, ref cases, catch_all_bb } => {
                            write!(f, "switchbr %{} : ", self.display_instr_name(scrutinee))?;
                            for case in cases {
                                write!(f, "case {} => %bb{}, ", self.fmt_const(&case.value), case.bb.index())?;
                            }
                            writeln!(f, "else => %bb{}", catch_all_bb.index())?;
                        }
                        // TODO: print generic arguments
                        &Instr::Call { ref arguments, func: callee, .. } => {
                            write!(f, "%{} = call `{}`", self.display_instr_name(op_id), self.fn_name(self.code.mir_code.functions[callee].name))?;
                            write_args!(arguments);
                            writeln!(f)?
                        },
                        &Instr::ExternCall { ref arguments, func: callee, .. } => {
                            let extern_mod = &self.code.mir_code.extern_mods[&callee.extern_mod];
                            let callee_func = &extern_mod.imported_functions[callee.index];
                            write!(f, "%{} = externcall `{}`", self.display_instr_name(op_id), callee_func.name)?;
                            write_args!(arguments);
                            writeln!(f, " from {:?}", extern_mod.library_path)?
                        },
                        Instr::Const(konst) => {
                            writeln!(f, "%{} = {}", self.display_instr_name(op_id), self.fmt_const(konst))?;
                        },
                        Instr::Intrinsic { arguments, intr, .. } => {
                            write!(f, "%{} = intrinsic `{}`", self.display_instr_name(op_id), intr.name())?;
                            write_args!(arguments);
                            writeln!(f)?
                        },
                        &Instr::Pointer { op, is_mut } => {
                            write!(f, "%{} = %{} *", self.display_instr_name(op_id), self.display_instr_name(op))?;
                            if is_mut {
                                writeln!(f, "mut")?
                            } else {
                                writeln!(f)?
                            }
                        }
                        &Instr::Load(location) => writeln!(f, "%{} = load %{}", self.display_instr_name(op_id), self.display_instr_name(location))?,
                        &Instr::LogicalNot(op) => writeln!(f, "%{} = not %{}", self.display_instr_name(op_id), self.display_instr_name(op))?,
                        &Instr::Ret(val) => writeln!(f,  "return %{}", self.display_instr_name(val))?,
                        &Instr::Store { location, value } => writeln!(f, "store %{} in %{}", self.display_instr_name(value), self.display_instr_name(location))?,
                        &Instr::AddressOfStatic(statik) => writeln!(f, "%{} = address of static %{}", self.display_instr_name(op_id), self.code.mir_code.statics[statik].name)?,
                        &Instr::Reinterpret(val, ref ty) => writeln!(f, "%{} = reinterpret %{} as {:?}", self.display_instr_name(op_id), self.display_instr_name(val), ty)?,
                        &Instr::SignExtend(val, ref ty) => writeln!(f, "%{} = sign-extend %{} as {:?}", self.display_instr_name(op_id), self.display_instr_name(val), ty)?,
                        &Instr::ZeroExtend(val, ref ty) => writeln!(f, "%{} = zero-extend %{} as {:?}", self.display_instr_name(op_id), self.display_instr_name(val), ty)?,
                        &Instr::Truncate(val, ref ty) => writeln!(f, "%{} = truncate %{} as {:?}", self.display_instr_name(op_id), self.display_instr_name(val), ty)?,
                        &Instr::FloatCast(val, ref ty) => writeln!(f, "%{} = floatcast %{} as {:?}", self.display_instr_name(op_id), self.display_instr_name(val), ty)?,
                        &Instr::IntToFloat(val, ref ty) => writeln!(f, "%{} = inttofloat %{} as {:?}", self.display_instr_name(op_id), self.display_instr_name(val), ty)?,
                        &Instr::FloatToInt(val, ref ty) => writeln!(f, "%{} = floattoint %{} as {:?}", self.display_instr_name(op_id), self.display_instr_name(val), ty)?,
                        &Instr::Struct { ref fields, id } => {
                            write!(f, "%{} = define struct{} {{ ", self.display_instr_name(op_id), id.index())?;
                            for i in 0..fields.len() {
                                write!(f, "%{}", self.display_instr_name(fields[i]))?;
                                if i < (fields.len() - 1) {
                                    write!(f, ",")?;
                                }
                                write!(f, " ")?;
                            }
                            writeln!(f, "}}")?;
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
                            writeln!(f, "}}")?;
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

                            writeln!(f, "}}")?;
                        }
                        &Instr::Variant { enuum, index, payload } => {
                            let variant = &self.code.hir_code.enums[enuum].variants[index];
                            let variant_name = variant.name;
                            write!(f, "%{} = %enum{}.{}", self.display_instr_name(op_id), enuum.index(), self.interner.resolve(variant_name).unwrap())?;
                            if variant.payload_ty.is_some() {
                                write!(f, "(%{})", self.display_instr_name(payload))?
                            }
                            writeln!(f)?
                        },
                        &Instr::DirectFieldAccess { val, index } => writeln!(f, "%{} = %{}.field{}", self.display_instr_name(op_id), self.display_instr_name(val), index)?,
                        &Instr::IndirectFieldAccess { val, index } => writeln!(f, "%{} = &(*%{}).field{}", self.display_instr_name(op_id), self.display_instr_name(val), index)?,
                        &Instr::DiscriminantAccess { val } => writeln!(f, "%{} = discriminant of %{}", self.display_instr_name(op_id), self.display_instr_name(val))?,
                        &Instr::GenericParam(param) => {
                            writeln!(f, "{} = generic_param{}", self.display_instr_name(op_id), param.index())?
                        },
                        Instr::Parameter(_) => {},
                        Instr::Void => panic!("unexpected void!"),
                    };
                }
            }
            write!(f, "}}")?;
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
}

struct FunctionBuilder {
    name: Option<Sym>,
    ret_ty: Type,
    instrs: IndexCounter<InstrId>,
    blocks: Vec<BlockId>,
    current_block: BlockId,
    stored_decl_locs: IndexVec<StoredDeclId, OpId>,
    pattern_binding_locs: HashMap<PatternBindingDeclId, Value>,
    instr_namespace: InstrNamespace,
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

    fn build_function(&mut self, name: Option<Sym>, ret_ty: Type, body: FunctionBody, params: Range<DeclId>, generic_params: Vec<GenericParamId>, tp: &impl TypeProvider) -> Function {
        debug_assert_ne!(ret_ty, Type::Error, "can't build MIR function with Error return type");

        let mut entry = Block::default();
        let mut instr_namespace = InstrNamespace::default();
        let mut instrs = IndexCounter::new();
        instrs.next(); // void
        for param in params.start.index()..params.end.index() {
            let param = DeclId::new(param);
            assert!(matches!(df!(param.hir), hir::Decl::Parameter { .. }));
            let ty = self.decl_type(param, tp);
            let instr = Instr::Parameter(ty.clone());
            let instr_id = instrs.next();
            let ty = ty.clone();
            let op = self.code.ops.push(Op::MirInstr(instr, instr_id, ty));
            let range = df!(param.range);
            let name = instr_namespace.insert(format!("{}", self.display_item(range)));
            self.code.mir_code.source_ranges.insert(op, range);
            self.code.mir_code.instr_names.insert(op, name);
            entry.ops.push(op);
        }
        let entry = self.code.blocks.push(entry);
        let mut b = FunctionBuilder {
            name,
            ret_ty,
            instrs,
            blocks: vec![entry],
            current_block: entry,
            stored_decl_locs: IndexVec::new(),
            pattern_binding_locs: HashMap::new(),
            instr_namespace: InstrNamespace::default(),
        };
        self.start_bb(&mut b, entry);
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
        };
        let mut function = Function {
            name: b.name,
            ret_ty: b.ret_ty,
            num_instrs: b.instrs.len(),
            blocks: b.blocks,
            instr_namespace: b.instr_namespace,
            decl,
            generic_params,
        };
        self.optimize_function(&mut function);
        self.code.mir_code.check_all_blocks_ended(&function);
        function 
    }

    fn optimize_function(&self, func: &mut Function) {
        // Get rid of empty blocks
        // TODO: get rid of unreachable blocks instead. Otherwise we might accidentally remove an
        // empty, reachable block and fail silently (at MIR generation time).
        func.blocks.retain(|&block| {
            let block = &self.code.blocks[block];
            !block.ops.is_empty()
        });
    }

    fn generate_type_of(&self, instr: &Instr) -> Type {
        let b = &self.code.mir_code;
        match instr {
            Instr::Void | Instr::Store { .. } => Type::Void,
            Instr::Pointer { .. } | Instr::Struct { .. } | Instr::GenericParam(_) | Instr::Enum { .. } => Type::Ty,
            &Instr::StructLit { id, .. } => Type::Struct(id),
            Instr::Const(konst) => konst.ty(),
            Instr::Alloca(ty) => ty.clone().mut_ptr(),
            Instr::LogicalNot(_) => Type::Bool,
            &Instr::Call { func, .. } => b.functions[func].ret_ty.clone(),
            Instr::ExternCall { func, .. } => b.extern_mods[&func.extern_mod].imported_functions[func.index].return_ty.clone(),
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
            Instr::Parameter(_) => panic!("should not call generate_type_of() on a parameter"),
            &Instr::DirectFieldAccess { val, index } => {
                let base_ty = self.type_of(val);
                match base_ty {
                    Type::Struct(strukt) => b.structs[&strukt].field_tys[index].clone(),
                    _ => panic!("Cannot directly access field of non-struct type {:?}!", base_ty),
                }
            },
            &Instr::IndirectFieldAccess { val, index } => {
                let base_ty = self.type_of(val).deref().unwrap();
                match base_ty.ty {
                    Type::Struct(strukt) => b.structs[&strukt].field_tys[index].clone().ptr_with_mut(base_ty.is_mut),
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

    fn build_scope_item(&mut self, b: &mut FunctionBuilder, item: Item, tp: &impl TypeProvider) {
        match item {
            Item::Expr(expr) => {
                self.build_expr(b, expr, Context::new(0, DataDest::Void, ControlDest::Continue), tp);
            },
            Item::Decl(decl) => match df!(decl.hir) {
                hir::Decl::Stored { id, root_expr, .. } => {
                    let ty = tp.ty(root_expr).clone();
                    let name = format!("{}", self.display_item(decl));
                    let location = self.push_instr_with_name(b, Instr::Alloca(ty), decl, name);
                    b.stored_decl_locs.push_at(id, location);
                    self.build_expr(b, root_expr, Context::new(0, DataDest::Store { location }, ControlDest::Continue), tp);
                },
                hir::Decl::Computed { .. } => {},
                _ => panic!("Invalid scope item"),
            },
        }
    }

    fn build_scope(&mut self, b: &mut FunctionBuilder, scope: ImperScopeId, ctx: Context, tp: &impl TypeProvider) -> Value {
        let block = self.code.hir_code.imper_scopes[scope].block;
        for i in 0..self.code.blocks[block].ops.len() {
            let op = self.code.blocks[block].ops[i];
            let item = self.code.ops[op].as_hir_item().unwrap();
            self.build_scope_item(b, item, tp);
        }
        self.build_expr(b, self.code.hir_code.imper_scopes[scope].terminal_expr, ctx, tp)
    }

    fn get_base(&self, id: DeclRefId) -> ExprId {
        match self.code.hir_code.decl_refs[id].namespace {
            hir::Namespace::MemberRef { base_expr } => base_expr,
            _ => panic!("Expected member ref expression"),
        }
    }

    fn get(&mut self, b: &mut FunctionBuilder, arguments: SmallVec<[OpId; 2]>, decl_ref_id: DeclRefId, tp: &impl TypeProvider) -> Value {
        let id = tp.selected_overload(decl_ref_id).expect("No overload found!");
        let generic_arguments = tp.generic_arguments(decl_ref_id).as_ref().unwrap_or(&Vec::new()).clone();
        let expr = self.code.hir_code.decl_refs[decl_ref_id].expr;
        let name = format!("{}", self.display_item(id));
        match self.get_decl(id, tp) {
            Decl::Computed { get } => self.push_instr(b, Instr::Call { arguments, generic_arguments, func: get }, expr).direct(),
            Decl::ExternFunction(func) => {
                assert!(generic_arguments.is_empty());
                self.push_instr(b, Instr::ExternCall { arguments, func }, expr).direct()
            },
            Decl::Stored(id) => {
                assert!(arguments.is_empty());
                b.stored_decl_locs[id].indirect()
            },
            Decl::PatternBinding { id: binding_id } => {
                assert!(arguments.is_empty());
                b.pattern_binding_locs[&binding_id]
            },
            Decl::Parameter { index } => {
                let entry_block = b.blocks[0];
                let value = self.code.blocks[entry_block].ops[index];
                value.direct()
            },
            Decl::GenericParam(param) => {
                self.push_instr(b, Instr::GenericParam(param), expr).direct()
            },
            Decl::Intrinsic(intr, ref ty) => {
                let ty = ty.clone();
                self.push_instr(b, Instr::Intrinsic { arguments, ty, intr }, expr).direct()
            },
            Decl::Const(ref konst) => {
                let konst = konst.clone();
                self.push_instr_with_name(b, Instr::Const(konst.clone()), expr, name).direct()
            },
            Decl::Static(statik) => {
                self.push_instr_with_name(b, Instr::AddressOfStatic(statik), expr, format!("static_{}", name)).indirect()
            },
            Decl::Field { index } => {
                let base = self.get_base(decl_ref_id);
                let base = self.build_expr(b, base, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                if base.indirection > 0 {
                    let base_ptr = self.handle_indirection(b, base.get_address());
                    self.push_instr(b, Instr::IndirectFieldAccess { val: base_ptr, index }, expr).indirect()
                } else {
                    debug_assert_eq!(base.indirection, 0, "tried to dereference a struct?!");
                    self.push_instr(b, Instr::DirectFieldAccess { val: base.instr, index }, expr).direct()   
                }
            },
            Decl::Variant { enuum, index, payload_ty } => {
                if payload_ty.is_some() {
                    debug_assert_eq!(arguments.len(), 1);
                    self.push_instr_with_name(b, Instr::Variant { enuum, index, payload: arguments[0] }, expr, name).direct()
                } else {
                    debug_assert!(arguments.is_empty());
                    self.push_instr_with_name(b, Instr::Const(Const::BasicVariant { enuum, index }), expr, name).direct()
                }
            },
            Decl::Invalid => panic!("INVALID DECL"),
        }
    }

    #[allow(unused)]
    fn set(&mut self, b: &mut FunctionBuilder, arguments: SmallVec<[OpId; 2]>, id: DeclRefId, value: OpId, tp: &impl TypeProvider) -> OpId {
        let expr = self.code.hir_code.decl_refs[id].expr;
        let id = tp.selected_overload(id).expect("No overload found!");
        let range = self.get_range(expr) + self.get_range(value);
        match self.get_decl(id, tp) {
            Decl::Computed { .. } => panic!("setters not yet implemented!"),
            Decl::Stored(id) => {
                assert!(arguments.is_empty());
                let location = b.stored_decl_locs[id];
                self.push_instr(b, Instr::Store { location, value }, range)
            },
            Decl::PatternBinding { id: binding_id } => {
                todo!();
                // assert!(arguments.is_empty());
                // let location = b.pattern_binding_locs[&binding_id];
                // self.push_instr(b, Instr::Store { location, value }, range)
            },
            Decl::Parameter { .. } | Decl::Const(_) | Decl::GenericParam(_) => panic!("can't set a constant!"),
            Decl::Intrinsic(_, _) => panic!("can't set an intrinsic! (yet?)"),
            Decl::Static(statik) => {
                let location = self.push_instr(b, Instr::AddressOfStatic(statik), expr);
                self.push_instr(b, Instr::Store { location, value }, range)
            },
            Decl::Field { .. } => panic!("Unhandled struct field!"),
            Decl::Variant { .. } => panic!("Can't modify an enum variant"),
            Decl::ExternFunction(_) => panic!("Can't set an external function"),
            Decl::Invalid => panic!("INVALID DECL"),
        }
    }

    #[allow(unused)]
    fn modify(&mut self, b: &mut FunctionBuilder, arguments: SmallVec<[OpId; 2]>, id: DeclRefId, tp: &impl TypeProvider) -> Value {
        let expr = self.code.hir_code.decl_refs[id].expr;
        let id = tp.selected_overload(id).expect("No overload found!");
        match self.get_decl(id, tp) {
            Decl::Computed { .. } => panic!("modify accessors not yet implemented!"),
            Decl::Stored(id) => {
                assert!(arguments.is_empty());
                b.stored_decl_locs[id].indirect()
            },
            Decl::PatternBinding { id: binding_id } => {
                todo!()
                // assert!(arguments.is_empty());
                // b.pattern_binding_locs[&binding_id]
            },
            Decl::GenericParam(_) => panic!("can't modify a generic parameter!"),
            Decl::Parameter { .. } | Decl::Const(_) => panic!("can't modify a constant!"),
            Decl::Intrinsic(_, _) => panic!("can't modify an intrinsic! (yet?)"),
            Decl::Static(statik) => self.push_instr(b, Instr::AddressOfStatic(statik), expr).indirect(),
            Decl::Field { .. } => panic!("Unhandled struct field!"),
            Decl::Variant { .. } => panic!("Can't modify an enum variant"),
            Decl::ExternFunction(_) => panic!("Can't modify an external function"),
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
        self.start_bb(b, true_bb);
        let scope_ctx = ctx.redirect(result_location, Some(post_bb));
        self.build_scope(b, then_scope, scope_ctx.clone(), tp);
    }

    fn build_if_expr(&mut self, b: &mut FunctionBuilder, expr: ExprId, ty: Type, condition: ExprId, then_scope: ImperScopeId, else_scope: Option<ImperScopeId>, ctx: Context, tp: &impl TypeProvider) -> Value {
        // Create a location on the stack to store the result of the if, if necessary
        let result_location = match (&ctx.data, else_scope) {
            (DataDest::Read, Some(_)) => Some(
                // TODO: this will be the wrong type if indirection != 0
                self.push_instr(b, Instr::Alloca(ty.clone()), expr)
            ),
            _ => None,
        };

        let true_bb = self.create_bb(b);
        let false_bb = self.create_bb(b);
        let post_bb = if else_scope.is_some() {
            self.create_bb(b)
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
            self.start_bb(b, next_bb);

            let scope_ctx = ctx.redirect(result_location, Some(post_bb));
            let scope = &self.code.hir_code.imper_scopes[cur];
            let block = self.code.hir_code.imper_scopes[cur].block;
            // If the current scope consists of a lone if expression
            if self.code.blocks[block].ops.is_empty() {
                if let Expr::If { condition, then_scope, else_scope } = ef!(scope.terminal_expr.hir) {
                    let true_bb = self.create_bb(b);
                    let false_bb = self.create_bb(b);
                    self.build_if_expr_recurse(b, condition, then_scope, result_location, true_bb, false_bb, post_bb, scope_ctx, tp);
                    next_scope = else_scope;
                    next_bb = false_bb;
                    continue;
                }
            }

            self.build_scope(b, cur, scope_ctx, tp);
            next_scope = None;
        }

        self.start_bb(b, post_bb);
        if let Some(location) = result_location {
            return self.push_instr(b, Instr::Load(location), expr).direct()
        } else if else_scope.is_some() {
            return self.handle_control(b, VOID_INSTR.direct(), ctx.control)
        } else {
            VOID_INSTR.direct()
        }
    }

    fn build_expr(&mut self, b: &mut FunctionBuilder, expr: ExprId, ctx: Context, tp: &impl TypeProvider) -> Value {
        let ty = tp.ty(expr).clone();

        let val = match ef!(expr.hir) {
            Expr::Void | Expr::Error => VOID_INSTR.direct(),
            Expr::IntLit { .. } | Expr::DecLit { .. } | Expr::StrLit { .. } | Expr::CharLit { .. } | Expr::BoolLit { .. } | Expr::ConstTy(_) | Expr::Mod { .. } | Expr::Import { .. } => {
                let konst = self.expr_to_const(expr, ty.clone());
                let name = format!("{}", self.fmt_const_for_instr_name(&konst));
                self.push_instr_with_name(b, Instr::Const(konst), expr, name).direct()
            },
            Expr::Set { lhs, rhs } => {
                return self.build_expr(
                b,
                rhs,
                ctx.new_data_dest(DataDest::Set { dest: lhs }),
                tp,
            )},
            // This isn't really a loop! It's just a control flow hack to get around the fact
            // that you can't chain `if let`s in Rust.
            Expr::DeclRef { ref arguments, id } => loop {
                let decl_id = tp.selected_overload(id).unwrap();

                // Check if the declaration is an intrinsic
                if let hir::Decl::Intrinsic { intr, .. } = df!(decl_id.hir) {
                    // Check if we need to special case the intrinsic
                    match intr {
                        Intrinsic::LogicalAnd => break {
                            assert_eq!(arguments.len(), 2);
                            let (lhs, rhs) = (arguments[0], arguments[1]);
                            let left_true_bb = self.create_bb(b);
                            let location = if let DataDest::Read = ctx.data {
                                Some(self.push_instr(b, Instr::Alloca(ty.clone()), expr))
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

                                self.start_bb(b, left_true_bb);
                                return self.build_expr(
                                    b,
                                    rhs,
                                    Context::new(0, DataDest::Branch(true_bb, false_bb), ControlDest::Continue),
                                    tp,
                                );
                            } else {
                                let left_false_bb = self.create_bb(b);
                                let after_bb = self.create_bb(b);
                                self.build_expr(
                                    b,
                                    lhs,
                                    Context::new(0, DataDest::Branch(left_true_bb, left_false_bb), ControlDest::Continue),
                                    tp,
                                );

                                self.start_bb(b, left_true_bb);
                                // No further branching required, because (true && foo) <=> foo
                                let branch_ctx = ctx.redirect(location, Some(after_bb));
                                self.build_expr(b, rhs, branch_ctx.clone(), tp);

                                self.start_bb(b, left_false_bb);
                                let false_const = Const::Bool(false);
                                let name = format!("{}", self.fmt_const_for_instr_name(&false_const));
                                let false_val = self.push_instr_with_name(b, Instr::Const(false_const), expr, name).direct();
                                self.handle_context(b, false_val, branch_ctx, tp);

                                self.start_bb(b, after_bb);
                                if let Some(location) = location {
                                    self.push_instr(b, Instr::Load(location), expr).direct()
                                } else {
                                    return VOID_INSTR.direct()
                                }
                            }
                        },
                        Intrinsic::LogicalOr => break {
                            assert_eq!(arguments.len(), 2);
                            let (lhs, rhs) = (arguments[0], arguments[1]);
                            let left_false_bb = self.create_bb(b);
                            if let DataDest::Branch(true_bb, false_bb) = ctx.data {
                                self.build_expr(
                                    b,
                                    lhs,
                                    Context::new(0, DataDest::Branch(true_bb, left_false_bb), ControlDest::Continue),
                                    tp,
                                );

                                self.start_bb(b, left_false_bb);
                                return self.build_expr(
                                    b,
                                    rhs,
                                    Context::new(0, DataDest::Branch(true_bb, false_bb), ControlDest::Continue),
                                    tp
                                );
                            } else {
                                let left_true_bb = self.create_bb(b);
                                let after_bb = self.create_bb(b);
                                let location = if let DataDest::Read = ctx.data {
                                    Some(self.push_instr(b, Instr::Alloca(ty.clone()), expr))
                                } else {
                                    None
                                };
                                self.build_expr(
                                    b,
                                    lhs,
                                    Context::new(0, DataDest::Branch(left_true_bb, left_false_bb), ControlDest::Continue),
                                    tp,
                                );

                                self.start_bb(b, left_true_bb);
                                let true_const = Const::Bool(true);
                                let name = format!("{}", self.fmt_const_for_instr_name(&true_const));
                                let true_val = self.push_instr_with_name(b, Instr::Const(true_const), expr, name).direct();
                                let branch_ctx = ctx.redirect(location, Some(after_bb));
                                self.handle_context(b, true_val, branch_ctx.clone(), tp);

                                self.start_bb(b, left_false_bb);
                                self.build_expr(b, rhs, branch_ctx, tp);

                                self.start_bb(b, after_bb);
                                if let Some(location) = location {
                                    self.push_instr(b, Instr::Load(location), expr).direct()
                                } else {
                                    return VOID_INSTR.direct()
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
                                self.push_instr(b, Instr::LogicalNot(operand.instr), expr).direct()
                            }
                        },
                        _ => {},
                    }
                }

                // Otherwise, just handle the general case
                // TODO: Don't clone arguments
                let arguments = arguments.clone().iter().map(|&argument| {
                    let val = self.build_expr(b, argument, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                    self.handle_indirection(b, val)
                }).collect();

                break self.get(b, arguments, id, tp);
            },
            Expr::Cast { expr: operand, ty: dest_ty, cast_id } => {
                let dest_ty = tp.get_evaluated_type(dest_ty).clone();
                match tp.cast_method(cast_id) {
                    CastMethod::Noop => return self.build_expr(b, operand, ctx, tp),
                    CastMethod::Reinterpret => {
                        let value = self.build_expr(b, operand, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                        let value = self.handle_indirection(b, value);
                        self.push_instr(b, Instr::Reinterpret(value, dest_ty), expr).direct()
                    },
                    CastMethod::Int => {
                        let (src_width, _src_is_signed, dest_width, dest_is_signed) = match (tp.ty(operand), &dest_ty) {
                            (&Type::Int { width: ref src_width, is_signed: src_is_signed }, &Type::Int { width: ref dest_width, is_signed: dest_is_signed })
                                => (src_width.clone(), src_is_signed, dest_width.clone(), dest_is_signed),
                            _ => panic!("Internal compiler error: found invalid cast types while generating MIR")
                        };
                        let (src_bit_width, dest_bit_width) = (src_width.bit_width(self.arch), dest_width.bit_width(self.arch));
                        let value = self.build_expr(b, operand, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                        let value = self.handle_indirection(b, value);

                        if src_bit_width == dest_bit_width {
                            // TODO: Bounds checking
                            self.push_instr(b, Instr::Reinterpret(value, dest_ty), expr)
                        } else if src_bit_width < dest_bit_width {
                            if dest_is_signed {
                                // TODO: Bounds checking
                                self.push_instr(b, Instr::SignExtend(value, dest_ty), expr)
                            } else {
                                // TODO: Bounds checking
                                self.push_instr(b, Instr::ZeroExtend(value, dest_ty), expr)
                            }
                        } else if src_bit_width > dest_bit_width {
                            // TODO: Bounds checking
                            self.push_instr(b, Instr::Truncate(value, dest_ty), expr)
                        } else {
                            unreachable!()
                        }.direct()
                    },
                    CastMethod::Float => {
                        let value = self.build_expr(b, operand, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                        let value = self.handle_indirection(b, value);
                        self.push_instr(b, Instr::FloatCast(value, dest_ty), expr).direct()
                    },
                    CastMethod::FloatToInt => {
                        let value = self.build_expr(b, operand, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                        let value = self.handle_indirection(b, value);
                        self.push_instr(b, Instr::FloatToInt(value, dest_ty), expr).direct()
                    },
                    CastMethod::IntToFloat => {
                        let value = self.build_expr(b, operand, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                        let value = self.handle_indirection(b, value);
                        self.push_instr(b, Instr::IntToFloat(value, dest_ty), expr).direct()
                    },
                    CastMethod::Invalid => panic!("FOUND INVALID CAST"),
                }
            },
            Expr::AddrOf { expr: operand, .. } => return self.build_expr(
                b,
                operand,
                Context::new(ctx.indirection + 1, ctx.data, ctx.control),
                tp,
            ),
            Expr::Pointer { expr: operand, is_mut } => {
                let op = self.build_expr(
                    b,
                    operand,
                    Context::new(0, DataDest::Read, ControlDest::Continue),
                    tp,
                );
                let op = self.handle_indirection(b, op);
                self.push_instr(b, Instr::Pointer { op, is_mut }, expr).direct()
            },
            Expr::Struct(id) => {
                let mut fields = SmallVec::new();
                for i in 0..self.code.hir_code.structs[id].fields.len() {
                    let field = &self.code.hir_code.structs[id].fields[i];
                    let field_ty = field.ty;
                    let field = self.build_expr(
                        b,
                        field_ty,
                        Context::new(0, DataDest::Read, ControlDest::Continue),
                        tp,
                    );
                    let field = self.handle_indirection(b, field);
                    fields.push(field);
                }
                self.push_instr(b, Instr::Struct { fields, id }, expr).direct()
            },
            Expr::Enum(id) => {
                let mut variants = SmallVec::new();
                for i in 0..self.code.hir_code.enums[id].variants.len() {
                    let variant = &self.code.hir_code.enums[id].variants[i];
                    let payload_ty = variant.payload_ty.unwrap_or(VOID_TYPE);
                    let variant = self.build_expr(
                        b,
                        payload_ty,
                        Context::new(0, DataDest::Read, ControlDest::Continue),
                        tp,
                    );
                    let variant = self.handle_indirection(b, variant);
                    variants.push(variant);
                }
                self.push_instr(b, Instr::Enum { variants, id }, expr).direct()
            }
            Expr::StructLit { id, .. } => {
                let lit = tp.struct_lit(id).as_ref().unwrap();
                let mut fields = SmallVec::new();
                for &field in &lit.fields {
                    let field = self.build_expr(
                        b,
                        field,
                        Context::new(0, DataDest::Read, ControlDest::Continue),
                        tp,
                    );
                    let field = self.handle_indirection(b, field);
                    fields.push(field);
                }
                self.push_instr(b, Instr::StructLit { fields, id: lit.strukt }, expr).direct()
            },
            Expr::Deref(operand) => return self.build_expr(
                b,
                operand,
                Context::new(ctx.indirection - 1, ctx.data, ctx.control),
                tp,
            ),
            Expr::Do { scope } => return self.build_scope(b, scope, ctx, tp),
            Expr::If { condition, then_scope, else_scope } => return self.build_if_expr(b, expr, ty, condition, then_scope, else_scope, ctx, tp),
            Expr::Switch { scrutinee, ref cases } => {
                let cases = cases.clone();
                let scrutinee_val = self.build_expr(b, scrutinee, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                let discriminant = self.get_discriminant(b, scrutinee_val);
                let result_location = match &ctx.data {
                    DataDest::Read => Some(
                        // TODO: this will be the wrong type if indirection != 0
                        self.push_instr(b, Instr::Alloca(ty.clone()), expr)
                    ),
                    _ => None,
                };
                let begin_bb = b.current_block;
                let post_bb = self.create_bb(b);
                let scope_ctx = ctx.redirect(result_location, Some(post_bb));
                let mut mir_cases = Vec::new();
                let (enum_id, variants) = match tp.ty(scrutinee) {
                    &Type::Enum(id) => {
                        (id, self.code.hir_code.enums[id].variants.clone())
                    },
                    _ => todo!(),
                };
                let mut catch_all_bb = None;
                for case in cases {
                    let case_bb = self.create_bb(b);
                    self.start_bb(b, case_bb);
                    // Can only bind at most one binding at a time. The exception is when they alias. E.g., '.a(int_val) & enum_val' <- int_val and enum_val are two bindings that alias.
                    // I will need some way of detecting aliases and allowing them, but at the same time splitting up the list of bindings when they don't alias, such as in disjuction patterns.
                    assert!(case.pattern.bindings.len() <= 1);
                    for &binding_id in &case.pattern.bindings {
                        let binding = self.code.hir_code.pattern_binding_decls[binding_id].clone();
                        // Can only ever bind one path to a particular binding at a time. Will need some way of splitting these up when I implement disjunction patterns
                        assert_eq!(binding.paths.len(), 1);
                        for path in &binding.paths {
                            let val = self.handle_indirection(b, scrutinee_val);
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

                    self.start_bb(b, begin_bb);
                    match case.pattern.kind {
                        hir::PatternKind::ContextualMember { name, .. } => {
                            let mut index = None;
                            for (i, variant) in variants.iter().enumerate() {
                                if variant.name == name.symbol {
                                    index = Some(i);
                                    break;
                                }
                            }
                            let index = index.expect("Unrecognized variant in switch case. Typechecker should have caught this.");
                            let discriminant = self.get_const_discriminant(enum_id, index);
                            mir_cases.push(
                                SwitchCase {
                                    value: discriminant,
                                    bb: case_bb,
                                }
                            );
                        },
                        hir::PatternKind::NamedCatchAll(_) | hir::PatternKind::AnonymousCatchAll(_) => {
                            catch_all_bb = Some(case_bb);
                        },
                    }
                }

                let catch_all_bb = if let Some(catch_all_bb) = catch_all_bb {
                    catch_all_bb
                } else {
                    let catch_all_bb = self.create_bb(b);
                    self.start_bb(b, catch_all_bb);
                    // TODO: add unreachable instruction I guess?
                    self.push_instr(b, Instr::Intrinsic { arguments: SmallVec::new(), ty: Type::Never, intr: Intrinsic::Panic }, SourceRange::default());
                    self.end_current_bb(b);
                    catch_all_bb
                };
                
                self.start_bb(b, begin_bb);
                self.push_instr(b, Instr::SwitchBr { scrutinee: discriminant, cases: mir_cases, catch_all_bb }, expr);
                self.end_current_bb(b);

                self.start_bb(b, post_bb);
                if let Some(location) = result_location {
                    return self.push_instr(b, Instr::Load(location), expr).direct()
                } else {
                    return self.handle_control(b, VOID_INSTR.direct(), ctx.control)
                }
            },
            Expr::While { condition, scope } => {
                let test_bb = self.create_bb(b);
                let loop_bb = self.create_bb(b);
                let post_bb = match ctx.control {
                    ControlDest::Continue | ControlDest::Unreachable | ControlDest::RetVoid => self.create_bb(b),
                    ControlDest::Block(block) => block,
                };

                self.push_instr(b, Instr::Br(test_bb), expr);
                self.end_current_bb(b);
                self.start_bb(b, test_bb);
                self.build_expr(b, condition, Context::new(0, DataDest::Branch(loop_bb, post_bb), ControlDest::Continue), tp);

                self.start_bb(b, loop_bb);
                self.build_scope(b, scope, Context::new(0, DataDest::Void, ControlDest::Block(test_bb)), tp);

                match ctx.control {
                    ControlDest::Continue | ControlDest::Unreachable | ControlDest::RetVoid => {
                        self.start_bb(b, post_bb);
                        VOID_INSTR.direct()
                    },
                    // Already handled this above
                    ControlDest::Block(_) => return VOID_INSTR.direct(),
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

        self.handle_context(b, val, ctx, tp)
    }

    fn get_const_discriminant(&self, _enum_id: EnumId, variant_index: usize) -> Const {
        // TODO: other discriminant types and custom values
        // Delete TYPE_OF_DISCRIMINANTS to deal with other cases
        Const::Int { lit: variant_index as u64, ty: TYPE_OF_DISCRIMINANTS }
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

    fn handle_context(&mut self, b: &mut FunctionBuilder, mut val: Value, ctx: Context, tp: &impl TypeProvider) -> Value {
        val = val.adjusted(ctx.indirection);
        match ctx.data {
            DataDest::Read => return val,
            DataDest::Ret => {
                let instr = self.handle_indirection(b, val);
                let val = self.push_instr(b, Instr::Ret(instr), instr).direct();
                self.end_current_bb(b);
                return val;
            },
            DataDest::Branch(true_bb, false_bb) => {
                let instr = self.handle_indirection(b, val);
                let val = self.push_instr(b, Instr::CondBr { condition: instr, true_bb, false_bb }, instr).direct();
                self.end_current_bb(b);
                return val;
            },
            DataDest::Receive { value } => {
                let location = self.handle_indirection(b, val.get_address());
                let range = self.get_range(location) + self.get_range(value);
                self.push_instr(b, Instr::Store { location, value }, range);
            },
            DataDest::Store { location } => {
                let instr = self.handle_indirection(b, val);
                let range = self.get_range(location) + self.get_range(instr);
                self.push_instr(b, Instr::Store { location, value: instr }, range);
            },
            DataDest::Set { dest } => {
                let instr = self.handle_indirection(b, val);
                return self.build_expr(
                    b,
                    dest,
                    Context::new(0, DataDest::Receive { value: instr }, ctx.control),
                    tp,
                );
            }
            DataDest::Void => {},
        }

        self.handle_control(b, val, ctx.control)
    }
}
