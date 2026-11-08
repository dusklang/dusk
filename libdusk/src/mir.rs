use std::assert_matches;
use std::collections::{HashSet, HashMap};
use std::ffi::CString;
use std::ops::Range;
use std::cmp::Ordering;
use std::cell::RefCell;
use std::sync::OnceLock;

use num_bigint::BigInt;
use smallvec::{SmallVec, smallvec};
use string_interner::DefaultSymbol as Sym;
use crate::index_vec::IndexVec;
use crate::display_adapter;

use crate::mir::cursor::{Cursor, CursorMut};
use crate::pattern_matching::{SwitchDecisionNode, SwitchDecisionValue, SwitchScrutineeValueId, TypedSwitchScrutineeValue, TypedSwitchScrutineeValueKind};
use crate::source_info::SourceRange;

use crate::internal_types::InternalField;
use crate::ast::{self, Attribute, CompilerTestCondition, DeclId, DeclRefId, EnumId, Expr, ExprId, ExternFunctionRef, ExternModId, GenericParamId, ImperScopeId, Item, IntrinsicId, LegacyIntrinsic, LoopId, NewNamespaceId, PatternMatchingContextId, ScopedItem, StoredDeclId, StructId, VOID_TYPE};
use crate::ty::{EnumType, FloatWidth, FunctionType, LegacyInternalType, StructType, Type};
use crate::driver::Driver;
use crate::typechecker as tc;
use crate::type_provider::TypeProvider;
use tc::CastMethod;
use crate::index_vec::*;
use crate::source_info::ToSourceRange;
use crate::error::Error;
use crate::interpreter::EvalError;

pub mod cursor;

use dusk_proc_macros::*;

// TODO: fix potential data race when adding values to hashmaps in MIR
// TODO: store source ranges for ValueIds (particularly useful for parameters, in the future maybe constants, etc.)

define_index_type!(pub struct FuncId = u32;);
define_index_type!(pub struct StaticId = u32;);
define_index_type!(pub struct StrId = u32;);
define_index_type!(pub struct InstrId = u32;);
define_index_type!(pub struct BlockId = u32;);
define_index_type!(pub struct ValueId = u32;);

pub const VOID_VALUE: ValueId = ValueId::new(0);
pub const NEVER_VALUE: ValueId = ValueId::new(1);

#[derive(Clone, Debug)]
pub struct Instr {
    pub kind: InstrKind,
    pub value: ValueId,
}

impl Instr {
    pub fn new(kind: InstrKind, value: ValueId) -> Self {
        Self {
            kind,
            value,
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct Block {
    pub params: Vec<ValueId>,
    pub first_instr: Option<InstrId>,
    pub last_instr: Option<InstrId>,
    pub terminator: Option<TerminatorKind>,
    pub prev: Option<BlockId>,
    pub next: Option<BlockId>,
}

#[derive(Debug, Clone)]
pub struct InstrNode {
    pub block: Option<BlockId>,
    pub prev: Option<InstrId>,
    pub next: Option<InstrId>,
}

#[derive(Clone, Debug, PartialEq, InstrOperands)]
pub struct JumpTarget {
    pub block: BlockId,
    pub arguments: SmallVec<[ValueId; 2]>,
}

impl From<BlockId> for JumpTarget {
    fn from(value: BlockId) -> Self {
        JumpTarget { block: value, arguments: Default::default() }
    }
}

#[derive(Clone, Debug, PartialEq, InstrOperands)]
pub struct SwitchCase {
    pub value: Const,
    pub target: JumpTarget,
}

#[derive(Debug, Clone, InstrOperands)]
pub enum TerminatorKind {
    Ret(ValueId),
    Jump(JumpTarget),
    CondBr { condition: ValueId, true_target: JumpTarget, false_target: JumpTarget },
    SwitchBr { scrutinee: ValueId, cases: Vec<SwitchCase>, catch_all_target: JumpTarget },
    Call(CallKind),
}

#[derive(Clone, Debug, PartialEq, InstrOperands)]
pub enum CallKind {
    LegacyIntrinsic { arguments: SmallVec<[ValueId; 2]>, ty: Type, intr: LegacyIntrinsic },
    Intrinsic { arguments: SmallVec<[ValueId; 2]>, intr: IntrinsicId },
    Call { arguments: SmallVec<[ValueId; 2]>, generic_arguments: Vec<Type>, func: FuncId },
    ExternCall { arguments: SmallVec<[ValueId; 2]>, func: ExternFunctionRef },
}

#[derive(Clone, Debug, PartialEq, InstrOperands)]
pub enum InstrKind {
    Invalid, // Used temporarily when copying functions
    Const(Const),
    Alloca(Type),
    LogicalNot(ValueId),
    ObjcClassRef { extern_mod: ExternModId, index: usize },
    FunctionRef { generic_arguments: Vec<Type>, func: FuncId, },
    Reinterpret(ValueId, Type),
    Truncate(ValueId, Type),
    SignExtend(ValueId, Type),
    ZeroExtend(ValueId, Type),
    FloatCast(ValueId, Type),
    FloatToInt(ValueId, Type),
    IntToFloat(ValueId, Type),
    Load(ValueId),
    Store { location: ValueId, value: ValueId },
    AddressOfStatic(StaticId),
    Pointer { value: ValueId, is_mut: bool },
    Struct { fields: SmallVec<[ValueId; 2]>, id: StructId },
    Enum { variants: SmallVec<[ValueId; 2]>, id: EnumId },
    FunctionTy { param_tys: Vec<ValueId>, has_c_variadic_param: bool, ret_ty: ValueId },
    StructLit { fields: SmallVec<[ValueId; 2]>, id: StructId },
    DirectFieldAccess { val: ValueId, index: usize },
    IndirectFieldAccess { val: ValueId, index: usize },
    InternalFieldAccess { val: ValueId, field: InternalField },
    Variant { enuum: EnumId, index: usize, payload: ValueId },
    DiscriminantAccess { val: ValueId },
    PayloadAccess { val: ValueId, variant_index: usize },
    Call(CallKind),
    /// This variant currently exists (rather than just stuffing a constant `Type` into `InstrKind::Const()`) so that we
    /// know to lookup the generic param instead of preserving it as-is.
    GenericParam(GenericParamId),
}

#[derive(Clone, Debug, PartialEq)]
pub enum Const {
    Int { lit: BigInt, ty: Type },
    Float { lit: f64, ty: Type },
    Str { id: StrId, ty: Type },
    /// A compile-time known string that comes from a string literal. This will be used in the future to convert
    /// to some user-defined type at compile-time.
    StrLit(CString),
    Bool(bool),
    Ty(Type),
    Mod(NewNamespaceId),
    // TODO: Support payloads in consts
    Variant { enuum: EnumId, index: usize, payload_tys: Vec<Type> },
    StructLit { fields: Vec<Const>, id: StructId },
    Void,
    Invalid,
}

impl Const {
    pub fn ty(&self) -> Type {
        match self {
            Const::Int { ty, .. } | Const::Float { ty, .. } | Const::Str { ty, .. } => ty.clone(),
            Const::StrLit(_) => Type::LegacyInternal(LegacyInternalType::StringLiteral),
            Const::Bool(_) => Type::Bool,
            Const::Ty(_) => Type::Ty,
            &Const::Variant { enuum, ref payload_tys, .. } => Type::Enum(EnumType { payload_tys: payload_tys.clone(), identity: enuum }),
            Const::Mod(_) => Type::Mod,
            &Const::StructLit { id, ref fields } => Type::Struct(
                StructType {
                    field_tys: fields.iter().map(|field| field.ty()).collect(),
                    identity: id,
                }
            ),
            Const::Void => Type::Void,
            Const::Invalid => Type::Error,
        }
    }
}

impl From<Result<Const, EvalError>> for Const {
    fn from(value: Result<Const, EvalError>) -> Self {
        value.unwrap_or(Const::Invalid)
    }
}

#[derive(Default, Debug, Clone)]
pub struct ValueNamespace {
    name_usages: HashMap<String, u16>,
}

impl ValueNamespace {
    pub fn insert(&mut self, name: impl Into<String>) -> String {
        let mut name = name.into();
        let entry = self.name_usages.entry(name.clone()).or_default();
        if *entry > 0 {
            name = format!("{}.{}", name, *entry);
        }
        *entry += 1;
        name
    }
}

#[derive(Debug, Clone)]
pub struct CompilerTestCase {
    pub func: FuncId,
    pub condition: CompilerTestCondition,
    pub arguments: Vec<Const>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub name: Option<Sym>,
    pub ty: FunctionType,
    pub blocks: IndexVec<BlockId, Block>,
    pub first_block: BlockId,
    pub last_block: BlockId,
    pub entry_block: BlockId,
    pub instrs: IndexVec<InstrId, Instr>,
    pub instr_nodes: IndexVec<InstrId, InstrNode>,
    pub values: IndexVec<ValueId, Value>,
    pub source_ranges: HashMap<InstrId, SourceRange>,
    pub value_names: HashMap<ValueId, String>,
    pub value_namespace: ValueNamespace,
    pub decl: Option<DeclId>,
    pub generic_params: Range<GenericParamId>,
    pub is_comptime: bool,

    // The set of instructions that failed to be const-eval'ed (e.g., due to a panic)
    pub poisoned_instrs: HashSet<InstrId>,
}

struct FunctionBuilder {
    func: Function,

    block_states: HashMap<BlockId, BlockState>,
    current_block: BlockId,
    stored_decl_locs: IndexVec<StoredDeclId, ValueId>,
    loops: IndexVec<LoopId, LoopState>,
    pattern_matching_scrutinees: HashMap<PatternMatchingContextId, HashMap<SwitchScrutineeValueId, IndirectValue>>,
}

impl Default for Function {
    fn default() -> Self {
        Self {
            name: Default::default(),
            ty: Default::default(),
            blocks: Default::default(),
            first_block: BlockId::new(0),
            last_block: BlockId::new(0),
            entry_block: BlockId::new(0),
            instrs: Default::default(),
            instr_nodes: Default::default(),
            values: Default::default(),
            source_ranges: Default::default(),
            value_names: Default::default(),
            decl: Default::default(),
            generic_params: empty_range(),
            value_namespace: Default::default(),
            is_comptime: Default::default(),
            poisoned_instrs: Default::default(),
        }
    }
}

impl Driver {
    #[display_adapter]
    pub fn display_block(&self, b: &FunctionBuilder, block: BlockId, w: &mut Formatter) {
        let mut cursor = b.func.make_cursor();
        cursor.goto_top(block);
        while let Some(instr) = cursor.next_instr() {
            writeln!(w, "    %instr{} = mir.{:?}", instr.index(), b.func.instrs[instr].kind)?;
        }
        Ok(())
    }
}

#[derive(Clone)]
pub struct StructLayout {
    pub field_offsets: SmallVec<[usize; 2]>,
    pub alignment: usize,
    pub size: usize,
    pub stride: usize,
}

#[derive(Clone)]
pub struct EnumLayout {
    pub payload_tys: SmallVec<[Type; 2]>,
    pub payload_offsets: SmallVec<[usize; 2]>,
    pub alignment: usize,
    pub size: usize,
    pub stride: usize,
}

#[derive(Debug)]
pub enum BlockState {
    Created,
    Started,
    Ended,
}

pub struct Static {
    pub name: String,
    pub val: Const,
}

pub struct ExternMod {
    pub library_path: CString,
    pub imported_functions: Vec<ExternFunction>,
}

#[derive(Debug)]
pub struct ExternFunction {
    pub name: String,
    pub ty: FunctionType,
}

pub struct FunctionSignature {
    pub name: Sym,
    pub ty: FunctionType,
    pub generic_params: Range<GenericParamId>,
    pub is_comptime: bool,
}

#[derive(Default)]
pub struct Mir {
    pub strings: ConcurrentIndexVec<StrId, CString>,
    pub functions: ConcurrentIndexVec<FuncId, OnceLock<Function>>,
    pub function_sigs: papaya::HashMap<FuncId, FunctionSignature>,
    pub compiler_test_cases: boxcar::Vec<CompilerTestCase>,
    pub statics: ConcurrentIndexVec<StaticId, Static>,
    pub extern_mods: papaya::HashMap<ExternModId, ExternMod>,
    pub enums: papaya::HashMap<EnumId, EnumLayout>,
    decls: papaya::HashMap<DeclId, Decl>,
}

#[derive(Debug)]
pub enum StartBlockError {
    BlockEnded,
}

#[derive(Debug)]
pub enum EndBlockError {
    BlockEnded,
    BlockNotStarted,
}

impl Mir {
    pub fn new() -> Self {
        Default::default()
    }
}

#[derive(Clone, Debug)]
enum Decl {
    Stored(StoredDeclId),
    Function { get: FuncId },
    ExternFunction(ExternFunctionRef),
    ObjcClassRef { extern_mod: ExternModId, index: usize },
    Parameter { index: usize },
    PatternBinding { context: PatternMatchingContextId, scrutinee: SwitchScrutineeValueId, root_scrutinee: ExprId },
    LegacyIntrinsic(LegacyIntrinsic, Type),
    Intrinsic(IntrinsicId),
    MethodIntrinsic(IntrinsicId),
    Static(StaticId),
    Const(Const),
    Field { index: usize },
    InternalField(InternalField),
    Variant { enuum: EnumId, index: usize, payload_ty: Option<Type> },
    GenericParam(GenericParamId),

    Invalid,
}

/// What to do with a value
#[derive(Clone, Copy, Debug)]
enum DataDest {
    /// This value needs to be returned from the current function
    Ret,
    /// A particular value needs to be assigned to this value
    Receive { value: ValueId },
    /// Jump to a particular basic block argument, with this value as an argument
    JumpWithArgument(BlockId),
    /// This value just needs to be read
    Read,
    /// This value will never be used
    Void,
    /// If this value is true, jump to the first target, otherwise jump to the second
    Branch(BlockId, BlockId),
}

#[derive(Clone, Debug)]
pub struct Value {
    pub ty: Type,
    pub kind: ValueKind,
}

impl Value {
    fn new(ty: Type, kind: ValueKind) -> Self {
        Value { ty, kind }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum ValueKind {
    InstrResult(InstrId),
    Parameter {
        block: BlockId,
        index: usize,
    },
    Void,
    Never,
}

#[derive(Debug, Copy, Clone)]
struct IndirectValue {
    value: ValueId,
    /// The number of pointer hops the value is away from instr.
    ///     Positive values => number of layers of indirection
    ///     Negative values => number of times the pointer has been dereferenced
    indirection: i8,
}

impl IndirectValue {
    fn get_address(self) -> IndirectValue {
        IndirectValue {
            value: self.value,
            // It might seem counterintuitive that we are subtracting here.
            // It's because when we get the address, the type also changes.
            // So we're at the same level of indirection from the original value,
            // but we're closer to the pointer value!
            indirection: self.indirection - 1
        }
    }

    fn adjusted(self, indirection: i8) -> IndirectValue {
        IndirectValue {
            value: self.value,
            indirection: self.indirection - indirection,
        }
    }
}

trait Indirection {
    fn direct(self) -> IndirectValue;
    fn indirect(self) -> IndirectValue;
}

impl Indirection for ValueId {
    fn direct(self) -> IndirectValue {
        IndirectValue {
            value: self,
            indirection: 0,
        }
    }

    fn indirect(self) -> IndirectValue {
        IndirectValue {
            value: self,
            indirection: 1,
        }
    }
}

/// Where to go after the current value is computed (whether implicitly or explicitly, such as via a `break` in a loop)
#[derive(Clone, Copy, Debug)]
enum ControlDest {
    Continue,
    Unreachable,
    Jump(BlockId),
    IncrementVariableAndThenJump {
        location: ValueId,
        target: BlockId,
    },
    RetVoid,
}

#[derive(Clone, Copy, Debug)]
struct Context {
    /// Relative to a Value indirection of 0, a:
    ///   - positive indirection means I need to move the value further away from me
    ///   - negative indirection means I need to get closer to the value
    indirection: i8,
    data: DataDest,
    control: ControlDest,
}

impl Default for Context {
    fn default() -> Self {
        Self::new(0, DataDest::Read, ControlDest::Continue)
    }
}

impl Context {
    fn new(indirection: i8, data: DataDest, control: ControlDest) -> Context {
        Context { indirection, data, control }
    }

    fn redirect(&self, kontinue: BlockId, pass_value_as_argument: bool) -> Context {
        let jump_with_argument = pass_value_as_argument && matches!(self.data, DataDest::Read);
        Context::new(
            self.indirection,
            if jump_with_argument {
                DataDest::JumpWithArgument(kontinue)
            } else {
                self.data
            },
            match &self.control {
                _ if jump_with_argument => ControlDest::Unreachable,
                ControlDest::Continue => ControlDest::Jump(kontinue),
                x => *x,
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
    fn expr_to_const(&self, expr: ExprId, ty: Type) -> Const {
        match ef!(expr.ast) {
            Expr::IntLit { lit } => {
                match ty {
                    Type::Int { .. } => Const::Int { lit: BigInt::from(lit), ty },
                    Type::Float(_)   => Const::Float { lit: lit as f64, ty },
                    _ => panic!("Unrecognized integer literal type {:?}", ty),
                }
            },
            Expr::DecLit { lit } => Const::Float { lit, ty },
            Expr::StrLit { ref lit } => {
                if matches!(ty, Type::LegacyInternal(LegacyInternalType::StringLiteral)) {
                    Const::StrLit(lit.clone())
                } else {
                    let id = self.mir.strings.push(lit.clone());
                    Const::Str { id, ty }
                }
            },
            Expr::CharLit { lit } => match ty {
                Type::Int { .. } => Const::Int { lit: BigInt::from(lit), ty },
                Type::Pointer(_) => {
                    let id = self.mir.strings.push(CString::new([lit as u8].as_ref()).unwrap());
                    Const::Str { id, ty }
                },
                _ => panic!("unexpected type for character")
            },
            Expr::BoolLit { lit } => Const::Bool(lit),
            Expr::Const(ref val) => val.clone(),
            Expr::Mod { id, .. } => Const::Mod(id),
            _ => panic!("Cannot convert expression to constant: {:#?}", expr),
        }
    }
}

#[derive(Debug)]
pub enum FunctionRef {
    Id(FuncId),
    Ref(Function),
}

// TODO: remove this as soon as discriminants can be other types, and deal with the fallout from that
const TYPE_OF_DISCRIMINANTS: Type = Type::u32();

pub fn function_by_ref<'a>(code: &'a Mir, func_ref: &'a FunctionRef) -> &'a Function {
    match func_ref {
        &FunctionRef::Id(id) => code.functions[id].get().unwrap(),
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
            Type::Error | Type::Void | Type::Never | Type::Ty | Type::Mod | Type::LegacyInternal(_) => 0,
            &Type::Internal(id) => self.mutable_state.try_read().unwrap().ast.internal_types[id].size,
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
            Type::Enum(enuum) => self.layout_enum(enuum).size,
            Type::GenericParam(_) => panic!("can't get size of generic type without more context"),
            Type::TypeVar(_) => panic!("can't get size of type variable without more context"),
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
        let cached_layout = LAYOUT_CACHE.with(|cache| {
            cache.borrow_mut().struct_layouts.get(strukt).cloned()
        });
        if let Some(layout) = cached_layout {
            return layout;
        }
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
        let layout = StructLayout { field_offsets, alignment, size, stride };
        LAYOUT_CACHE.with(|cache| {
            let mut cache = cache.borrow_mut();
            cache.struct_layouts.insert(strukt.clone(), layout.clone());
        });

        layout
    }

    pub fn layout_enum(&self, enuum: &EnumType) -> EnumLayout {
        use std::cmp::max;
        // Get max alignment of all the payload types.
        let alignment = enuum.payload_tys.iter()
            .map(|ty| self.align_of(ty))
            .max()
            .unwrap_or(0);
        let alignment = max(alignment, 4);

        let mut payload_offsets = SmallVec::new();
        let mut size = 4;
        for ty in &enuum.payload_tys {
            let offset = next_multiple_of(4, self.align_of(ty));
            payload_offsets.push(offset);
            size = max(size, offset + self.size_of(ty));
        }
        let stride = next_multiple_of(size, alignment);

        EnumLayout { payload_tys: enuum.payload_tys.clone().into(), payload_offsets, alignment, size, stride }
    }
}

impl Driver {
    pub fn build_mir(&self, tp: &dyn TypeProvider) {
        // Start at 1 to avoid RETURN_VALUE_DECL, which we can't and shouldn't generate code for
        let range = DeclId::new(1)..self.mutable_state.try_read().unwrap().ast.decls.next_idx();
        for id in range_iter(range) {
            self.get_decl(id, tp);
        }
    }
}

impl Driver {
    pub fn build_standalone_expr(&self, expr: ExprId, tp: &dyn TypeProvider) -> Function {
        let func_ty = FunctionType { param_tys: Vec::new(), return_ty: Box::new(tp.ty(expr).clone()), has_c_variadic_param: false };
        self.build_function(None, func_ty, FunctionBody::Expr(expr), empty_range(), empty_range(), false, tp)
    }
}

impl Driver {
    fn resolve_extern_mod(&self, id: ExternModId, tp: &dyn TypeProvider) {
        let extern_mods = self.mir.extern_mods.pin();
        if extern_mods.contains_key(&id) { return; }

        let extern_mod = &self.mutable_state.try_read().unwrap().ast.extern_mods[id];
        let library_path = extern_mod.library_path;
        let library_path = match *tp.eval_result(library_path) {
            Const::Str { id, .. } => self.mir.strings[id].clone(),
            Const::StrLit(ref string) => string.clone(),
            _ => panic!("unable to get path as string"),
        };

        let mut imported_functions = Vec::with_capacity(extern_mod.imported_functions.len());
        for func in &extern_mod.imported_functions {
            let param_tys = func.param_list.param_tys.iter()
                .map(|&ty| tp.get_evaluated_type(ty))
                .cloned()
                .collect();
            let return_ty = tp.get_evaluated_type(func.return_ty).clone();
            imported_functions.push(
                ExternFunction {
                    name: func.name.clone(),
                    ty: FunctionType {
                        param_tys,
                        has_c_variadic_param: func.param_list.has_c_variadic_param,
                        return_ty: Box::new(return_ty),
                    },
                }
            );
        }
        extern_mods.insert(
            id,
            ExternMod {
                library_path,
                imported_functions,
            },
        );
    }
}

impl Driver {
    fn get_decl(&self, id: DeclId, tp: &dyn TypeProvider) -> Decl {
        if let Some(decl) = self.mir.decls.pin().get(&id) { return decl.clone(); }
        match df!(id.ast) {
            ast::Decl::Function { ref params, scope, ref generic_params, .. } => {
                // Add placeholder function to reserve ID ahead of time
                let params = params.clone();
                let generic_params = generic_params.clone();
                let get = self.mir.functions.push(OnceLock::new());
                let decl = Decl::Function { get };
                self.mir.decls.pin().insert(id, decl.clone());

                let func_ty = self.decl_type(&*self.mutable_state.try_read().unwrap(), id, tp).as_function().unwrap().clone();
                let name = self.mutable_state.try_read().unwrap().ast.names[id];
                let mut is_comptime = false;
                let mut compiler_test_cases = Vec::new();
                if let Some(attrs) = self.mutable_state.try_read().unwrap().ast.decl_attributes.get(&id).cloned() {
                    for attr in attrs {
                        match self.mutable_state.try_read().unwrap().ast.attrs[attr] {
                            Attribute::Comptime => is_comptime = true,
                            Attribute::CompilerTest { condition, ref arguments, .. } => {
                                let arguments = arguments.clone();
                                compiler_test_cases.push(
                                    CompilerTestCase {
                                        func: get,
                                        condition,
                                        arguments: arguments.iter().map(|&arg| self.eval_expr(arg, tp).unwrap_or(Const::Invalid)).collect(),
                                    }
                                )
                            }
                            _ => {},
                        }
                    }
                }
                let signature = FunctionSignature {
                    name,
                    ty: func_ty.clone(),
                    generic_params: generic_params.clone(),
                    is_comptime,
                };
                self.mir.function_sigs.pin().insert(get, signature);
                let func = self.build_function(
                    Some(name),
                    func_ty,
                    FunctionBody::Scope { scope, decl: id },
                    params,
                    generic_params,
                    is_comptime,
                    tp,
                );
                for test_case in compiler_test_cases {
                    self.mir.compiler_test_cases.push(test_case);
                }
                self.mir.functions[get].set(func).expect("function value set multiple times");
                decl
            },
            ast::Decl::FunctionPrototype { extern_func, .. } => {
                let decl = if let Some(extern_func) = extern_func {
                    self.resolve_extern_mod(extern_func.extern_mod, tp);
                    Decl::ExternFunction(extern_func)
                } else {
                    let err = Error::new("cannot declare prototype outside of extern module")
                        .adding_primary_range(id, "prototype here");
                    self.diag.push(err);
                    Decl::Invalid
                };
                self.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::ObjcClassRef { extern_mod, index } => {
                self.resolve_extern_mod(extern_mod, tp);
                let decl = Decl::ObjcClassRef { extern_mod, index };
                self.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::Stored { id: index, .. } | ast::Decl::LoopBinding { id: index, .. } => {
                let decl = Decl::Stored(index);
                self.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::Parameter { index } => {
                let decl = Decl::Parameter { index };
                self.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::PatternBinding { context, scrutinee, root_scrutinee, .. } => {
                let decl = Decl::PatternBinding { context, scrutinee, root_scrutinee };
                self.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::LegacyIntrinsic { intr, function_like, .. } => {
                let mut ty = self.decl_type(&*self.mutable_state.try_read().unwrap(), id, tp);
                if function_like {
                    ty = ty.return_ty().unwrap().clone();
                }
                let decl = Decl::LegacyIntrinsic(intr, ty);
                self.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::Intrinsic(intr) => {
                let decl = Decl::Intrinsic(intr);
                self.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::MethodIntrinsic(intr) => {
                let decl = Decl::MethodIntrinsic(intr);
                self.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::Static(expr) => {
                let name = self.display_item(None, id).to_string();
                let konst = self.eval_expr(expr, tp);
                let statik = self.mir.statics.push(
                    Static {
                        name,
                        val: konst.into(),
                    }
                );
                let decl = Decl::Static(statik);
                self.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::Const { assigned_expr: root_expr, .. } => {
                let konst = self.eval_expr(root_expr, tp);

                // TODO: Deal with cycles!
                let decl = Decl::Const(konst.into());
                self.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::Field { index, .. } => {
                let decl = Decl::Field { index };
                self.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::InternalField(field) => {
                let decl = Decl::InternalField(field);
                self.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::Variant { enuum, index, payload_ty } => {
                let payload_ty = payload_ty.map(|ty| tp.get_evaluated_type(ty).clone());
                Decl::Variant { enuum, index, payload_ty }
            },
            ast::Decl::GenericParam(param) => {
                let decl = Decl::GenericParam(param);
                self.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::ReturnValue => panic!("Can't get_decl() the return_value decl"),
        }
    }
}

impl Driver {
    #[display_adapter]
    fn fmt_variant_name(&self, f: &mut Formatter, enuum: EnumId, index: usize) {
        let variant = &self.mutable_state.try_read().unwrap().ast.enums[enuum].variants[index];
        let interner = self.interner.read().unwrap();
        let name = interner.resolve(variant.name).unwrap();
        write!(f, "{}", name)
    }

    #[display_adapter]
    pub fn display_const(&self, f: &mut Formatter, konst: &Const) {
        match *konst {
            Const::Bool(val) => write!(f, "{}", val)?,
            Const::Float { lit, ref ty } => write!(f, "{} as {:?}", lit, ty)?,
            Const::Int { ref lit, ref ty } => write!(f, "{} as {:?}", lit, ty)?,
            Const::Str { id, ref ty } => write!(f, "%str{} ({:?}) as {:?}", id.index(), self.mir.strings[id], ty)?,
            Const::StrLit(ref lit) => write!(f, "str_lit \"{}\"", lit.clone().into_string().unwrap())?,
            Const::Ty(ref ty) => write!(f, "`{:?}`", ty)?,
            Const::Void => write!(f, "void")?,
            Const::Mod(id) => write!(f, "%mod{}", id.index())?,
            Const::Variant { enuum, index, .. } => write!(f, "%enum{}.{}", enuum.index(), self.fmt_variant_name(enuum, index))?,
            Const::StructLit { ref fields, id } => {
                write!(f, "const literal struct{} {{ ", id.index())?;
                for i in 0..fields.len() {
                    write!(f, "{}", self.display_const(&fields[i]))?;
                    if i < (fields.len() - 1) {
                        write!(f, ",")?;
                    }
                    write!(f, " ")?;
                }
                write!(f, "}}")?;
            },
            Const::Invalid => write!(f, "INVALID CONST")?,
        }

        Ok(())
    }

    #[display_adapter]
    fn fmt_const_for_instr_name(&self, f: &mut Formatter, konst: &Const) {
        match *konst {
            Const::Bool(val) => write!(f, "const_{}", val)?,
            Const::Float { lit, .. } => {
                let name = lit.to_string()
                    .replace('-', "negative_")
                    .replace('.', "_dot_");
                write!(f, "const_{}", name)?
            },
            Const::Int { ref lit, .. } => write!(f, "const_int_{}", lit)?,
            Const::Str { id, .. } => write!(f, "string_{}", identifierify(self.mir.strings[id].clone().into_bytes()))?,
            Const::StrLit(ref lit) => write!(f, "string_lit_{}", identifierify(lit.clone().into_bytes()))?,
            Const::Ty(ref ty) => write!(f, "type_{}", identifierify(format!("{:?}", ty).into_bytes()))?,
            Const::Void => write!(f, "const_void")?,

            // TODO: heuristics for associating declaration names with modules and types
            Const::Mod(id) => write!(f, "mod{}", id.index())?,
            Const::Variant { enuum, index, .. } => write!(f, "enum{}_variant_{}", enuum.index(), self.fmt_variant_name(enuum, index))?,

            Const::StructLit { id, .. } => write!(f, "const_struct_literal_{}", id.index())?,
            Const::Invalid => write!(f, "INVALID_CONST")?,
        }

        Ok(())
    }

    pub fn fn_name(&self, name: Option<Sym>) -> String {
        match name {
            Some(name) => self.interner.read().unwrap().resolve(name).unwrap().to_owned(),
            None => "{anonymous}".to_string(),
        }
    }

    // TODO: Move this out of MIR
    #[display_adapter('a)]
    pub fn display_item(&'a self, ctx: Option<&'a Function>, item: impl Into<ToSourceRange> + Copy + 'a, f: &mut Formatter) {
        let range = self.get_range_with_mir_ctx(item, ctx);
        let mut fallback = || {
            match item.into() {
                ToSourceRange::Item(item) => match item {
                    Item::Decl(decl) => write!(f, "{:?}", df!(decl.ast)),
                    Item::Expr(expr) => write!(f, "{:?}", ef!(expr.ast)),
                    Item::Attr(attr) => write!(f, "{:?}", af!(attr.ast)),
                },
                ToSourceRange::Instr(instr) => write!(f, "instr{}", instr.index()),
                ToSourceRange::SourceRange(_) => write!(f, "empty_range"),
                ToSourceRange::Value(value) => write!(f, "value{}", value.index()),
            }
        };
        if range.is_empty() {
            fallback()
        } else {
            if let Some(substr) = self.substring_from_range(range) {
                write!(f, "{}", substr)
            } else {
                fallback()
            }
        }
    }

    #[display_adapter]
    pub fn display_value_impl(&self, values: &IndexVec<ValueId, Value>, value_names: &HashMap<ValueId, String>, value: ValueId, f: &mut Formatter) {
        match values[value].kind {
            ValueKind::InstrResult(_) | ValueKind::Parameter { .. } => write!(f, "%{}", self.display_value_name_impl(value_names, value)),
            ValueKind::Void => write!(f, "void"),
            ValueKind::Never => write!(f, "never"),
        }
    }

    #[display_adapter]
    pub fn display_value_name_impl(&self, value_names: &HashMap<ValueId, String>, value: ValueId, f: &mut Formatter) {
        write!(f, "{}", value_names.get(&value).cloned()
            .unwrap_or_else(|| format!("value{}", value.index())))
    }

    #[display_adapter]
    pub fn display_value(&self, func: &Function, value: ValueId, f: &mut Formatter) {
        write!(f, "{}", self.display_value_impl(&func.values, &func.value_names, value))
    }

    #[display_adapter]
    pub fn display_value_from_builder(&self, b: &FunctionBuilder, value: ValueId, f: &mut Formatter) {
        write!(f, "{}", self.display_value_impl(&b.func.values, &b.func.value_names, value))
    }

    #[display_adapter]
    pub fn display_value_name(&self, func: &Function, value: ValueId, f: &mut Formatter) {
        write!(f, "{}", self.display_value_name_impl(&func.value_names, value))
    }

    #[display_adapter]
    pub fn display_value_name_from_builder(&self, b: &FunctionBuilder, value: ValueId, f: &mut Formatter) {
        write!(f, "{}", self.display_value_name_impl(&b.func.value_names, value))
    }

    #[display_adapter]
    pub fn display_branch_target(&self, func: &Function, target: &JumpTarget, f: &mut Formatter) {
        write!(f, "%block{}", target.block.index())?;
        if !target.arguments.is_empty() {
            write!(f, "(")?;
            let mut first = true;
            for &arg in &target.arguments {
                if first {
                    first = false;
                } else {
                    write!(f, ", ")?;
                }
                write!(f, "{}", self.display_value(func, arg))?;
            }
            write!(f, ")")?;
        }
        Ok(())
    }

    #[display_adapter]
    fn display_generic_args(&self, args: &[Type], f: &mut Formatter) {
        if !args.is_empty() {
            write!(f, "<|")?;
            let mut first = true;
            for arg in args {
                if first {
                    first = false;
                } else {
                    write!(f, ", ")?;
                }
                write!(f, "{:?}", arg)?;
            }
            write!(f, "|>")?;
        }
        Ok(())
    }

    #[display_adapter]
    fn display_call_args(&self, func: &Function, args: &[ValueId], f: &mut Formatter) {
        write!(f, "(")?;
        let mut first = true;
        for &arg in args {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            write!(f, "{}", self.display_value(func, arg))?;
        }
        write!(f, ")")?;
        Ok(())
    }

    #[display_adapter]
    pub fn display_mir_call(&self, func: &Function, call: &CallKind, f: &mut Formatter) {
        match *call {
            CallKind::Call { ref arguments, func: callee, ref generic_arguments } => {
                let name_sym = self.mir.function_sigs.pin().get(&callee).unwrap().name;
                write!(f, "call `{}`{}{}", self.fn_name(Some(name_sym)), self.display_generic_args(generic_arguments), self.display_call_args(func, arguments))?;
            },
            CallKind::ExternCall { ref arguments, func: callee, .. } => {
                let extern_mods = self.mir.extern_mods.pin();
                let extern_mod = extern_mods.get(&callee.extern_mod).unwrap();
                let callee_func = &extern_mod.imported_functions[callee.index];
                write!(f, "externcall `{}`{} from {:?}", callee_func.name, self.display_call_args(func, arguments), extern_mod.library_path)?;
            },
            CallKind::LegacyIntrinsic { ref arguments, intr, .. } => {
                write!(f, "legacy_intrinsic `{}`{}", intr.name(), self.display_call_args(func, arguments))?;
            },
            CallKind::Intrinsic { ref arguments, intr, .. } => {
                write!(f, "intrinsic `{}`{}", &self.mutable_state.try_read().unwrap().ast.intrinsics[intr].kind.name(), self.display_call_args(func, arguments))?;
            },
        }
        Ok(())
    }

    #[display_adapter]
    pub fn display_mir_instr(&self, func: &Function, instr_id: InstrId, f: &mut Formatter) {
        let Instr { kind: ref instr, value } = func.instrs[instr_id];
        if !matches!(func.values[value].ty, Type::Void | Type::Never) {
            write!(f, "%{} = ", self.display_value_name(func, value))?;
        }
        match instr {
            InstrKind::Alloca(ty) => write!(f, "alloca {:?}", ty)?,
            InstrKind::Call(call) => write!(f, "{}", self.display_mir_call(&func, call))?,
            &InstrKind::FunctionRef { func: callee, ref generic_arguments } => {
                let name_sym = self.mir.function_sigs.pin().get(&callee).unwrap().name;
                write!(f, "function_ref `{}`{}", self.fn_name(Some(name_sym)), self.display_generic_args(generic_arguments))?;
            },
            &InstrKind::ObjcClassRef { extern_mod, index } => write!(
                f,
                "objc_class_ref `{}` from {:?}",
                self.mutable_state.try_read().unwrap().ast.extern_mods[extern_mod].objc_class_references[index],
                self.mir.extern_mods.pin().get(&extern_mod).unwrap().library_path
            )?,
            InstrKind::Const(konst) => {
                write!(f, "{}", self.display_const(konst))?;
            },
            &InstrKind::Pointer { value, is_mut } => {
                write!(f, "{} *", self.display_value(func, value))?;
                if is_mut {
                    write!(f, "mut")?
                }
            },
            &InstrKind::Load(location) => write!(f, "load {}", self.display_value(func, location))?,
            &InstrKind::LogicalNot(instr) => write!(f, "not {}", self.display_value(func, instr))?,
            &InstrKind::Store { location, value } => write!(f, "store {} in {}", self.display_value(func, value), self.display_value(func, location))?,
            &InstrKind::AddressOfStatic(statik) => write!(f, "address of static {}", self.mir.statics[statik].name)?,
            &InstrKind::Reinterpret(val, ref ty) => write!(f, "reinterpret {} as {:?}", self.display_value(func, val), ty)?,
            &InstrKind::SignExtend(val, ref ty) => write!(f, "sign-extend {} as {:?}", self.display_value(func, val), ty)?,
            &InstrKind::ZeroExtend(val, ref ty) => write!(f, "zero-extend {} as {:?}", self.display_value(func, val), ty)?,
            &InstrKind::Truncate(val, ref ty) => write!(f, "truncate {} as {:?}", self.display_value(func, val), ty)?,
            &InstrKind::FloatCast(val, ref ty) => write!(f, "floatcast {} as {:?}", self.display_value(func, val), ty)?,
            &InstrKind::IntToFloat(val, ref ty) => write!(f, "inttofloat {} as {:?}", self.display_value(func, val), ty)?,
            &InstrKind::FloatToInt(val, ref ty) => write!(f, "floattoint {} as {:?}", self.display_value(func, val), ty)?,
            &InstrKind::Struct { ref fields, id } => {
                write!(f, "define struct{} {{ ", id.index())?;
                for i in 0..fields.len() {
                    write!(f, "{}", self.display_value(func, fields[i]))?;
                    if i < (fields.len() - 1) {
                        write!(f, ",")?;
                    }
                    write!(f, " ")?;
                }
                write!(f, "}}")?;
            },
            &InstrKind::StructLit { ref fields, id } => {
                write!(f, "literal struct{} {{ ", id.index())?;
                for i in 0..fields.len() {
                    write!(f, "{}", self.display_value(func, fields[i]))?;
                    if i < (fields.len() - 1) {
                        write!(f, ",")?;
                    }
                    write!(f, " ")?;
                }
                write!(f, "}}")?;
            },
            &InstrKind::Enum { ref variants, id } => {
                write!(f, "define enum{} {{", id.index())?;

                for (i, variant) in self.mutable_state.try_read().unwrap().ast.enums[id].variants.iter().enumerate() {
                    write!(f, "{}", self.interner.read().unwrap().resolve(variant.name).unwrap())?;
                    if variant.payload_ty.is_some() {
                        write!(f, "({})", self.display_value(func, variants[i]))?;
                    }
                    if i < (variants.len() - 1) {
                        write!(f, ",")?;
                    }
                    write!(f, " ")?;
                }

                write!(f, "}}")?;
            },
            &InstrKind::FunctionTy { ref param_tys, has_c_variadic_param, ret_ty } => {
                write!(f, "fn type (")?;
                for (i, &param) in param_tys.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", self.display_value(func, param))?;
                }
                if has_c_variadic_param {
                    if !param_tys.is_empty() {
                        write!(f, ", ")?;
                    }
                    write!(f, "...")?;
                }
                write!(f, " -> {}", self.display_value(func, ret_ty))?;
            },
            &InstrKind::Variant { enuum, index, payload } => {
                let variant = &self.mutable_state.try_read().unwrap().ast.enums[enuum].variants[index];
                let variant_name = variant.name;
                write!(f, "%enum{}.{}", enuum.index(), self.interner.read().unwrap().resolve(variant_name).unwrap())?;
                if variant.payload_ty.is_some() {
                    write!(f, "({})", self.display_value(func, payload))?
                }
            },
            &InstrKind::DirectFieldAccess { val, index } => write!(f, "{}.field{}", self.display_value(func, val), index)?,
            &InstrKind::IndirectFieldAccess { val, index } => write!(f, "&(*{}).field{}", self.display_value(func, val), index)?,
            &InstrKind::InternalFieldAccess { val, field } => write!(f, "{}.{}", self.display_value(func, val), field.name())?,
            &InstrKind::DiscriminantAccess { val } => write!(f, "discriminant of {}", self.display_value(func, val))?,
            &InstrKind::PayloadAccess { val, variant_index } => write!(f, "payload of {} using variant {}", self.display_value(func, val), variant_index)?,
            // TODO: instead of emitting these instructions as needed, add all generic params to the beginning of the MIR function as "hidden" values, just like normal parameters.
            &InstrKind::GenericParam(param) => {
                write!(f, "generic_param{}", param.index())?
            },
            InstrKind::Invalid => write!(f, "invalid!")?,
        };
        Ok(())
    }

    #[display_adapter]
    pub fn display_mir_terminator(&self, func: &Function, terminator: &TerminatorKind, f: &mut Formatter) {
        match *terminator {
            TerminatorKind::Jump(ref block) => write!(f, "jump {}", self.display_branch_target(func, block))?,
            TerminatorKind::CondBr { condition, ref true_target, ref false_target }
                => write!(f, "condbr {}, {}, {}", self.display_value(func, condition), self.display_branch_target(func, true_target), self.display_branch_target(func, false_target))?,
            TerminatorKind::SwitchBr { scrutinee, ref cases, ref catch_all_target } => {
                write!(f, "switchbr {} : ", self.display_value(func, scrutinee))?;
                for case in cases {
                    write!(f, "case {} => {}, ", self.display_const(&case.value), self.display_branch_target(func, &case.target))?;
                }
                write!(f, "else => {}", self.display_branch_target(func, catch_all_target))?;
            }
            TerminatorKind::Ret(val) => write!(f,  "return {}", self.display_value(func, val))?,
            TerminatorKind::Call(ref call) => write!(f, "{}", self.display_mir_call(func, call))?,
        }
        Ok(())
    }

    #[display_adapter]
    pub fn display_param_list(&self, func: &Function, block: BlockId, f: &mut Formatter) {
        let mut first = true;
        for &param in &func.blocks[block].params {
            if first {
                first = false;
            } else {
                write!(f, ", ")?;
            }
            let param_ty = &func.values[param].ty;
            write!(f, "%{}: {:?}", self.display_value_name(func, param), param_ty)?;
        }
        Ok(())
    }

    #[display_adapter]
    pub fn display_mir_block(&self, func: &Function, id: BlockId, f: &mut Formatter) {
        let block = &func.blocks[id];
        write!(f, "%block{}", id.index())?;
        if id != func.entry_block && !block.params.is_empty() {
            write!(f, "({})", self.display_param_list(func, id))?;
        }
        writeln!(f, ":")?;

        let mut cursor = func.make_cursor();
        cursor.goto_top(id);
        while let Some(instr_id) = cursor.next_instr() {
            writeln!(f, "    {}", self.display_mir_instr(func, instr_id))?;
        }
        if let Some(terminator) = cursor.terminator() {
            writeln!(f, "    {}", self.display_mir_terminator(func, terminator))?;
        }
        Ok(())
    }

    #[display_adapter]
    pub fn display_mir_function(&self, func: &FunctionRef, f: &mut Formatter) {
        let func = function_by_ref(&self.mir, func);
        if func.is_comptime {
            write!(f, "@comptime ")?;
        }
        write!(f, "fn {}", self.fn_name(func.name))?;
        if !func.generic_params.is_empty() {
            write!(f, "<|")?;
            let mut first = true;
            for generic_param in range_iter(func.generic_params.clone()) {
                if first {
                    first = false;
                } else {
                    write!(f, ", ")?;
                }
                write!(f, "generic_param{}", generic_param.index())?;
            }
            write!(f, "|>")?;
        }
        writeln!(f, "({}): {:?} {{", self.display_param_list(func, func.entry_block), func.ty.return_ty.as_ref())?;
        write!(f, "{}", self.display_mir_block(func, func.entry_block))?;
        for block_id in func.make_cursor().block_ids_iter() {
            if block_id != func.entry_block {
                write!(f, "{}", self.display_mir_block(func, block_id))?;
            }
        }
        write!(f, "}}")
    }

    #[display_adapter]
    pub fn display_mir(&self, f: &mut Formatter) {
        if !self.mir.statics.is_empty() {
            for statik in self.mir.statics.iter() {
                writeln!(f, "%{} = {}", statik.name, self.display_const(&statik.val))?;
            }
            writeln!(f)?;
        }

        let mut iter = self.mir.functions.indices().peekable();
        while let Some(i) = iter.next() {
            write!(f, "{}", self.display_mir_function(&FunctionRef::Id(i)))?;
            if iter.peek().is_some() {
                writeln!(f, "\n")?;
            }
        }
        Ok(())
    }
}

#[derive(Copy, Clone)]
enum FunctionBody<'func> {
    Scope { scope: ImperScopeId, decl: DeclId },
    Expr(ExprId),
    ConstantInstruction {
        parent_func: &'func Function,
        instr: InstrId,
    },
}

#[derive(Clone, Debug, PartialEq)]
struct LoopState {
    break_block: BlockId,
    continue_block: BlockId,
    continue_location_of_variable_to_increment: Option<ValueId>,
}

#[derive(Debug)]
enum DeclRef {
    LegacyIntrinsic { intrinsic: LegacyIntrinsic, ty: Type },
    Intrinsic(IntrinsicId),
    MethodIntrinsic(IntrinsicId),
    Function { func: FuncId, generic_args: Vec<Type> },
    ExternFunction { func: ExternFunctionRef },
    ObjcClassRef { extern_mod: ExternModId, index: usize },
    #[allow(unused)]
    EnumVariantWithPayload { enuum: EnumId, index: usize, payload_ty: Option<Type> },
    Value(IndirectValue),
}

impl Driver {
    fn end_current_block(&self, b: &mut FunctionBuilder, terminator: TerminatorKind) -> IndirectValue {
        let block = b.current_block;
        if b.end_block(block).is_err() {
            panic!("Failed to end block {} in function {}:\n{}", block.index(), self.fn_name(b.func.name), self.display_block(b, block));
        }

        let terminator_ref = &mut b.func.blocks[block].terminator;
        assert!(terminator_ref.is_none(), "attempted to end block that already has a terminator");
        if let TerminatorKind::Call(ref call) = terminator {
            let ty = self.generate_type_of_call(call);
            assert_eq!(ty, Type::Never, "internal compiler error: call in terminator position has non-never result type {:?}: {:?}", ty, terminator);
        }
        *terminator_ref = Some(terminator);
        NEVER_VALUE.direct()
    }
}

impl Driver {
    fn build_function(&self, name: Option<Sym>, func_ty: FunctionType, body: FunctionBody, params: Range<DeclId>, generic_params: Range<GenericParamId>, is_comptime: bool, tp: &dyn TypeProvider) -> Function {
        debug_assert_ne!(func_ty.return_ty.as_ref(), &Type::Error, "can't build MIR function with Error return type");

        let mut entry = Block::default();
        let mut value_namespace = ValueNamespace::default();
        let mut values = IndexVec::new();
        values.push_at(VOID_VALUE, Value::new(Type::Void, ValueKind::Void));
        values.push_at(NEVER_VALUE, Value::new(Type::Never, ValueKind::Never));
        let mut value_names = HashMap::new();
        let mut blocks = IndexVec::new();
        let entry_block = blocks.next_idx();
        for (param_index, param) in range_iter(params.clone()).enumerate() {
            assert!(matches!(df!(param.ast), ast::Decl::Parameter { .. }));
            let ty = self.decl_type(&*self.mutable_state.try_read().unwrap(), param, tp);
            let param_value = Value::new(ty, ValueKind::Parameter { block: entry_block, index: param_index });
            let param_value = values.push(param_value);
            let range = df!(param.range);
            let name = value_namespace.insert(self.display_item(None, range).to_string());
            value_names.insert(param_value, name);
            entry.params.push(param_value);
        }
        blocks.push_at(entry_block, entry);
        let core = Function {
            name,
            ty: func_ty,
            blocks,
            first_block: entry_block,
            last_block: entry_block,
            entry_block,
            instrs: Default::default(),
            instr_nodes: Default::default(),
            values,
            source_ranges: Default::default(),
            value_names,
            value_namespace,
            generic_params,
            decl: if let FunctionBody::Scope { decl, .. } = body {
                Some(decl)
            } else {
                None
            },
            is_comptime,
            poisoned_instrs: Default::default(),
        };
        let mut b = FunctionBuilder {
            func: core,
            block_states: HashMap::new(),
            current_block: entry_block,
            stored_decl_locs: IndexVec::new(),
            loops: Default::default(),
            pattern_matching_scrutinees: Default::default(),
        };
        b.start_block(entry_block);
        let ctx = Context::new(0, DataDest::Ret, ControlDest::Unreachable);
        match body {
            FunctionBody::Expr(expr) => {
                self.build_expr(&mut b, expr, ctx, tp);
            },
            FunctionBody::Scope { scope, .. } => {
                self.build_scope(&mut b, scope, ctx, tp);
            },
            FunctionBody::ConstantInstruction { parent_func, instr } => {
                let instruction = parent_func.instrs[instr].kind.clone();
                match instruction {
                    InstrKind::Call(_) => {
                        let mut copier = MirCopier::default();
                        let result = self.copy_instruction_if_needed(&mut b, &mut copier, parent_func, instr);
                        let value = b.func.instrs[result].value;
                        self.end_current_block(&mut b, TerminatorKind::Ret(value));
                    },
                    InstrKind::DiscriminantAccess { .. } | InstrKind::SignExtend(_, _) | InstrKind::ZeroExtend(_, _) => {
                        let mut copier = MirCopier::default();
                        let result = self.copy_instruction_if_needed(&mut b, &mut copier, parent_func, instr);
                        let value = b.func.instrs[result].value;
                        self.end_current_block(&mut b, TerminatorKind::Ret(value));
                    },
                    _ => unimplemented!("{:?}", instruction),
                }
            },
        };
        // Remove inactive blocks
        let mut cursor = b.func.make_cursor_mut();
        while cursor.next_block().is_some() {
            if cursor.next_instr().is_none() && cursor.terminator().is_none() {
                cursor.remove_block();
            }
        }
        b.check_all_blocks_ended();

        let is_constant_instruction = matches!(body, FunctionBody::ConstantInstruction { .. });
        self.optimize_function(&mut b.func, !is_constant_instruction, tp);
        self.validate_function(&b.func);
        // At this point, it is assumed that any comptime functions that can be evaluated at compile time already have
        // been. Therefore, if there are any calls to comptime functions left, it must be because they were passed
        // non-const arguments. If *this* function itself is comptime, then this is perfectly fine.
        if !b.func.is_comptime {
            self.check_no_comptime_calls(&b.func);
        }
        b.func
    }
}


impl Driver {
    fn remove_redundant_loads(&self, func: &mut Function) -> bool {
        // Remove obviously-redundant loads (assumes no other threads are accessing a memory location simultaneously)
        let mut did_something = false;
        let uses = func.get_uses();
        let mut cursor = func.make_cursor_mut();
        while cursor.next_block().is_some() {
            while let Some(instr_id) = cursor.next_instr() {
                let instr = &cursor.func.instrs[instr_id].kind;
                if let &InstrKind::Store { location, value } = instr
                    && let Some(next_instr) = cursor.peek_next_instr() {
                        let next_instr_value = cursor.func.instrs[next_instr].value;
                        if let InstrKind::Load(load_loc) = cursor.func.instrs[next_instr].kind
                            && load_loc == location {
                                cursor.func.remove_instr(next_instr);
                                cursor.func.replace_uses(&uses, next_instr_value, value);
                                did_something = true;
                            }
                    }
            }
        }

        did_something
    }

    fn remove_unused_allocas(&self, func: &mut Function) -> bool {
        let mut did_something = false;
        let uses = func.get_uses();
        let mut cursor = func.make_cursor_mut();
        while cursor.next_block().is_some() {
            while let Some(instr_id) = cursor.next_instr() {
                let instr_value = cursor.func.instrs[instr_id].value;
                let instr = &cursor.func.instrs[instr_id].kind;
                let mut potential_deletions = Vec::new();
                if let InstrKind::Alloca(_) = instr {
                    let uses = uses.get(instr_value);
                    let mut is_used = false;
                    'check_uses: {
                        for &other_instr_id in &uses.instrs {
                            // If the address of the alloca is used as the *location* in the store, then it doesn't count
                            // as a use, and we can potentially delete both it and the alloca later.
                            if let InstrKind::Store { location, .. } = cursor.func.instrs[other_instr_id].kind
                                && location != instr_value {
                                potential_deletions.push(other_instr_id);
                            } else {
                                is_used = true;
                                break 'check_uses;
                            }
                        }
                        if uses.terminators.len() > 0 {
                            is_used = true;
                            break 'check_uses;
                        }
                    }
                    if !is_used {
                        cursor.remove_instr();
                        for potential_deletion in potential_deletions {
                            cursor.func.remove_instr(potential_deletion);
                        }
                        did_something = true;
                    }
                }
            }

        }

        did_something
    }

    fn remove_unused_values(&self, func: &mut Function) -> bool {
        let mut did_something = false;
        let uses = func.get_uses();
        let mut cursor = func.make_cursor_mut();
        while cursor.next_block().is_some() {
            while let Some(instr_id) = cursor.next_instr() {
                let instr_value = cursor.func.instrs[instr_id].value;
                let instr = &cursor.func.instrs[instr_id].kind;
                if let InstrKind::Const(_) | InstrKind::Load(_) = instr {
                    if !uses.get(instr_value).is_used() {
                        cursor.remove_instr();
                        did_something = true;
                    }
                }
            }
        }

        did_something
    }

    fn register_edges(&self, func: &Function, mut register_edge: impl FnMut(BlockId, BlockId)) {
        let mut cursor = func.make_cursor();
        while let Some(id) = cursor.next_block() {
            let terminator = cursor.terminator().unwrap();
            terminator.register_edges(id, &mut register_edge);
        }
    }

    fn count_block_edges(&self, func: &Function, block: BlockId) -> usize {
        let mut block_edge_count = 0usize;
        self.register_edges(func, |_, other_block| if other_block == block {
            block_edge_count += 1;
        });
        block_edge_count
    }

    fn count_block_predecessors(&self, func: &Function, block: BlockId) -> usize {
        let mut predecessors = HashSet::new();
        self.register_edges(func, |predecessor, other_block| if other_block == block {
            predecessors.insert(predecessor);
        });
        predecessors.len()
    }

    #[allow(unused)]
    fn count_all_predecessors(&self, func: &Function) -> HashMap<BlockId, usize> {
        let mut predecessors: HashMap<BlockId, HashSet<BlockId>> = HashMap::new();
        self.register_edges(
            func,
            |predecessor, other_block| {
                predecessors.entry(other_block).or_default().insert(predecessor);
            }
        );
        let mut num_predecessors = HashMap::new();
        for (block, predecessors) in predecessors {
            num_predecessors.insert(block, predecessors.len());
        }
        num_predecessors
    }

    fn check_entry_block_has_no_predecessors(&self, func: &Function) {
        assert_eq!(self.count_block_edges(func, func.entry_block), 0, "internal compiler error: entry block should not have predecessors");
    }

    fn check_no_invalid_instructions(&self, func: &Function) {
        let mut cursor = func.make_cursor();
        while cursor.next_block().is_some() {
            while let Some(instr) = cursor.next_instr() {
                if matches!(&func.instrs[instr].kind, InstrKind::Invalid) {
                    panic!("Found invalid instruction in function");
                }
            }
        }
    }

    fn remove_redundant_blocks(&self, func: &mut Function) -> bool {
        // Replace blocks that do nothing but branch to another block
        let mut replace_list = Vec::new();
        let mut delete_list = HashSet::new();
        let mut new_entry_block = None;
        let mut did_something = false;
        let mut cursor = func.make_cursor();
        let mut process_block = |block_id: BlockId, block: &Block| {
            let only_terminator_in_block = block.first_instr.is_none() && block.last_instr.is_none() && block.terminator.is_some();
            if !only_terminator_in_block { return; }

            cursor.goto_top(block_id);
            let terminator = cursor.terminator().unwrap();
            // TODO: continue the transformation even if there are basic block arguments
            if let TerminatorKind::Jump(other) = terminator && other.arguments.is_empty() && other.block != block_id {
                // Don't replace entry block with `other` if `other` has more than one predecessor.
                // Otherwise, the entry block would end up with predecessors, which is invalid.
                if block_id == func.entry_block && self.count_block_predecessors(func, other.block) > 1 {
                    return;
                }

                // If we plan to replace the entry block in this pass, never *also* replace the block it's being replaced with in the same pass.
                if new_entry_block.as_ref().map(|(b, _)| b) == Some(&block_id) {
                    return;
                }

                replace_list.push((block_id, other.block));
                delete_list.insert(block_id);
                did_something = true;
                if block_id == func.entry_block {
                    let parameters = block.params.to_vec();
                    new_entry_block = Some((other.block, parameters));
                }
            }
        };

        // Process entry block first, because process_block depends on that in order to avoid replacing a chain of blocks starting with the entry block.
        process_block(func.entry_block, &func.blocks[func.entry_block]);
        for (block_id, block) in func.make_cursor().blocks_iter_enumerated() {
            if block_id != func.entry_block {
                process_block(block_id, block);
            }
        }
        if let Some((new_entry_block_id, parameters)) = new_entry_block {
            func.entry_block = new_entry_block_id;
            for param in parameters {
                let new_entry_block = &mut func.blocks[new_entry_block_id];
                let new_param_index = new_entry_block.params.len();
                new_entry_block.params.push(param);
                func.values[param].kind = ValueKind::Parameter { block: new_entry_block_id, index: new_param_index };
            }
        }
        let mut cursor = func.make_cursor_mut();
        for block in delete_list {
            cursor.goto_top(block);
            cursor.remove_block();
        }
        let mut cursor = func.make_cursor_mut();
        while cursor.next_block().is_some() {
            let terminator = cursor.terminator_mut().unwrap();
            for &(from, to) in &replace_list {
                terminator.replace_block(from, to);
            }
        }
        did_something
    }

    fn traverse_descendants(&self, func: &Function, visited: &mut HashSet<BlockId>, block: BlockId) {
        if !visited.insert(block) { return; }

        let mut cursor = func.make_cursor();
        cursor.goto_top(block);
        let terminator = cursor.terminator().unwrap();
        terminator.visit_blocks(&mut |block| self.traverse_descendants(func, visited, block));
    }

    fn remove_unreachable_blocks(&self, func: &mut Function) -> bool {
        let mut visited = HashSet::new();
        self.traverse_descendants(func, &mut visited, func.entry_block);
        let mut removed_blocks = false;
        let mut cursor = func.make_cursor_mut();
        while let Some(block_id) = cursor.next_block() {
            if !visited.contains(&block_id) {
                cursor.remove_block();
                removed_blocks = true;
            }
        }

        removed_blocks
    }

    fn remove_constant_branches(&self, func: &mut Function) -> bool {
        let mut did_something = false;
        let mut cursor = func.make_cursor_mut();
        while cursor.next_block().is_some() {
            match *cursor.terminator().unwrap() {
                TerminatorKind::CondBr { condition, ref true_target, ref false_target } => {
                    match cursor.func.values[condition].kind {
                        ValueKind::InstrResult(condition) => {
                            let condition = &cursor.func.instrs[condition].kind;
                            if let &InstrKind::Const(Const::Bool(condition)) = condition {
                                let destination = if condition {
                                    true_target
                                } else {
                                    false_target
                                };
                                cursor.replace_terminator(TerminatorKind::Jump(destination.clone()));
                                did_something = true;
                            }
                        },
                        ValueKind::Parameter { .. } | ValueKind::Void | ValueKind::Never => {},
                    }
                },
                TerminatorKind::SwitchBr { scrutinee, ref cases, ref catch_all_target } => {
                    match cursor.func.values[scrutinee].kind {
                        ValueKind::InstrResult(scrutinee) => {
                            let scrutinee = &cursor.func.instrs[scrutinee].kind;
                            if let InstrKind::Const(scrutinee @ Const::Int { .. }) = scrutinee {
                                let destination = cases.iter()
                                    .find(|case| *scrutinee == case.value)
                                    .map(|case| &case.target)
                                    .unwrap_or(catch_all_target);

                                cursor.replace_terminator(TerminatorKind::Jump(destination.clone()));
                                did_something = true;
                            }
                        },
                        ValueKind::Parameter { .. } | ValueKind::Void | ValueKind::Never => {},
                    }
                },
                _ => {},
            }
        }

        did_something
    }

    fn remove_return_non_shared_void(&self, func: &mut Function) -> bool {
        let mut did_something = false;
        let mut cursor = func.make_cursor_mut();
        while cursor.next_block().is_some() {
            let terminator = cursor.terminator().unwrap();
            if let &TerminatorKind::Ret(ret_val) = terminator
                && *cursor.func.ty.return_ty == Type::Void && ret_val != VOID_VALUE {
                    cursor.replace_terminator(TerminatorKind::Ret(VOID_VALUE));
                    did_something = true;
                }
        }

        did_something
    }
}

#[derive(Default)]
struct MirCopier {
    old_to_new_instrs: HashMap<InstrId, InstrId>,
}

impl Driver {
    fn call_is_const(&self, func: &Function, call: &CallKind) -> bool {
        match *call {
            CallKind::LegacyIntrinsic { intr, ref arguments, .. } => {
                match intr {
                    LegacyIntrinsic::Mult | LegacyIntrinsic::Div | LegacyIntrinsic::Mod | LegacyIntrinsic::Add | LegacyIntrinsic::Sub
                        | LegacyIntrinsic::Less | LegacyIntrinsic::LessOrEq | LegacyIntrinsic::Greater | LegacyIntrinsic::GreaterOrEq
                        | LegacyIntrinsic::Eq | LegacyIntrinsic::NotEq | LegacyIntrinsic::BitwiseAnd | LegacyIntrinsic::BitwiseOr
                        | LegacyIntrinsic::BitwiseNot | LegacyIntrinsic::BitwiseXor | LegacyIntrinsic::LeftShift | LegacyIntrinsic::RightShift
                        | LegacyIntrinsic::LogicalAnd | LegacyIntrinsic::LogicalOr | LegacyIntrinsic::LogicalNot | LegacyIntrinsic::Neg
                        | LegacyIntrinsic::Pos => {
                        arguments.iter().all(|&val| self.value_is_const(func, val))
                    }
                    _ => false,
                }
            },
            CallKind::Call { func: callee, ref arguments, .. } if self.mir.function_sigs.pin().get(&callee).unwrap().is_comptime => arguments.iter().all(|&val| self.value_is_const(func, val)),

            _ => false,
        }
    }
    fn instruction_is_const(&self, func: &Function, instr: InstrId) -> bool {
        let instr = &func.instrs[instr].kind;
        match *instr {
            InstrKind::Const(_) => true,
            InstrKind::LogicalNot(val) | InstrKind::Truncate(val, _) | InstrKind::SignExtend(val, _) | InstrKind::ZeroExtend(val, _)
                | InstrKind::FloatCast(val, _) | InstrKind::FloatToInt(val, _) | InstrKind::IntToFloat(val, _)
                | InstrKind::DiscriminantAccess { val }
                => self.value_is_const(func, val),
            InstrKind::Call(ref call) => self.call_is_const(func, call),

            _ => false,
        }
    }

    fn value_is_const(&self, func: &Function, value: ValueId) -> bool {
        match func.values[value].kind {
            ValueKind::Void | ValueKind::Never => true,
            ValueKind::Parameter { .. } => false,
            ValueKind::InstrResult(instr) => self.instruction_is_const(func, instr),
        }
    }

    fn instruction_is_nontrivial_const(&self, func: &Function, instr: InstrId) -> bool {
        self.instruction_is_const(func, instr) && !matches!(&func.instrs[instr].kind, InstrKind::Const(_))
    }

    fn copy_value_if_needed(&self, b: &mut FunctionBuilder, copier: &mut MirCopier, src_function: &Function, value_id: ValueId) -> ValueId {
        match src_function.values[value_id].kind {
            ValueKind::InstrResult(instr_id) => {
                let instr = self.copy_instruction_if_needed(b, copier, src_function, instr_id);
                b.func.instrs[instr].value
            },
            ValueKind::Void => VOID_VALUE,
            ValueKind::Never => NEVER_VALUE,
            ValueKind::Parameter { .. } => panic!("internal compiler error: attempt to copy non-constant parameter value to new function"),
        }
    }

    fn copy_instruction_if_needed(&self, b: &mut FunctionBuilder, copier: &mut MirCopier, src_function: &Function, instr_id: InstrId) -> InstrId {
        if let Some(&new) = copier.old_to_new_instrs.get(&instr_id) {
            new
        } else {
            let mut instr = src_function.instrs[instr_id].kind.clone();
            let replacements: Vec<_> = instr.referenced_values().into_iter().map(|arg_value_id| {
                (arg_value_id, self.copy_value_if_needed(b, copier, src_function, arg_value_id))
            }).collect();
            for (old, new) in replacements {
                match src_function.values[old].kind {
                    ValueKind::InstrResult(_) => {
                        instr.replace_value(old, new);
                    },
                    ValueKind::Void => {
                        debug_assert_eq!(old, VOID_VALUE);
                        debug_assert_eq!(new, VOID_VALUE);
                    },
                    ValueKind::Never => {
                        debug_assert_eq!(old, NEVER_VALUE);
                        debug_assert_eq!(new, NEVER_VALUE);
                    },
                    ValueKind::Parameter { .. } => {},
                }
            }

            let (copied_instr_id, _) = self.push_instr_and_get_id(b, instr, src_function.source_ranges[&instr_id]);
            copier.old_to_new_instrs.insert(instr_id, copied_instr_id);
            copied_instr_id
        }
    }

    fn eval_constants(&self, func: &mut Function, tp: &dyn TypeProvider) -> bool {
        let mut did_something = false;
        let mut cursor = func.make_cursor_mut();
        while cursor.next_block().is_some() {
            while let Some(instr) = cursor.next_instr() {
                // TODO: be greedy about the number of instructions you take to reduce the number of ad hoc MIR
                // functions built. For example, in the MIR equivalent of 2 + 3 + 4, the current implementation would
                // evaluate 2 + 3 as its own function, then 5 + 4 as another. We should put both operations in the same
                // function whenever possible.
                //
                // With that being said, as I was writing this TODO, I realized that there is one problem: some of
                // those intermediate instructions might be depended on by *other* instructions as well, making it not
                // possible to put them all together (or they each need to be returned from the function via tuples or
                // something). So it's not quite as simple to do this as I had initially thought. But still a good idea
                // probably.
                if self.instruction_is_nontrivial_const(&cursor.func, instr) && !cursor.func.poisoned_instrs.contains(&instr) {
                    let ty = cursor.func.type_of_instr(instr).clone();
                    let func_ty = FunctionType { param_tys: vec![], has_c_variadic_param: false, return_ty: Box::new(ty.clone()) };
                    let new_func = self.build_function(cursor.func.name, func_ty, FunctionBody::ConstantInstruction { parent_func: cursor.func, instr }, empty_range(), empty_range(), true, tp);
                    let Ok(result) = self.call(FunctionRef::Ref(new_func), Vec::new(), Vec::new()) else {
                        // Make sure we won't repeatedly try and fail to const-eval this instruction.
                        cursor.func.poisoned_instrs.insert(instr);
                        did_something = true;
                        continue;
                    };
                    let konst = self.value_to_const(result, ty);
                    cursor.replace_instr(InstrKind::Const(konst));
                    did_something = true;
                }
            }
        }

        did_something
    }

    fn optimize_function(&self, func: &mut Function, should_eval_constants: bool, tp: &dyn TypeProvider) {
        let mut did_something = true;
        while did_something {
            did_something = false;
            did_something |= self.remove_redundant_loads(func);
            did_something |= self.remove_unused_allocas(func);
            did_something |= self.remove_unused_values(func);
            did_something |= self.remove_constant_branches(func);
            did_something |= self.remove_redundant_blocks(func);
            did_something |= self.remove_unreachable_blocks(func);
            did_something |= self.remove_return_non_shared_void(func);
            if should_eval_constants {
                did_something |= self.eval_constants(func, tp);
            }
        }
    }

    fn check_jump_target(&self, func: &Function, target: &JumpTarget) {
        let params = &func.blocks[target.block].params;
        assert_eq!(target.arguments.len(), params.len(), "number of basic block arguments != params");

        for (&arg, &param) in target.arguments.iter().zip(params) {
            let arg_ty = &func.values[arg].ty;
            let param_ty = &func.values[param].ty;
            assert!(arg_ty.trivially_convertible_to(param_ty), "basic block argument type doesn't match param type ({:?}, {:?})", arg_ty, param_ty);
        }
    }

    fn check_basic_block_params(&self, func: &Function) {
        for (block_id, block) in func.make_cursor().blocks_iter_enumerated() {
            for (param_index, &param) in block.params.iter().enumerate() {
                let param_value = &func.values[param];
                assert_eq!(param_value.kind, ValueKind::Parameter { block: block_id, index: param_index }, "internal compiler error: parameter value is not a parameter");
            }
        }

        let mut cursor = func.make_cursor();
        while cursor.next_block().is_some() {
            let terminator = cursor.terminator().unwrap();
            match *terminator {
                TerminatorKind::Jump(ref target) => self.check_jump_target(func, target),
                TerminatorKind::CondBr { ref true_target, ref false_target, .. } => {
                    self.check_jump_target(func, true_target);
                    self.check_jump_target(func, false_target);
                },
                TerminatorKind::SwitchBr { ref cases, ref catch_all_target, .. } => {
                    for case in cases {
                        self.check_jump_target(func, &case.target);
                    }
                    self.check_jump_target(func, catch_all_target);
                },
                TerminatorKind::Ret(_) | TerminatorKind::Call(_) => {},
            }
        }
    }

    fn check_no_comptime_calls(&self, func: &Function) {
        let mut comptime_calls = Vec::new();
        let mut cursor = func.make_cursor();
        while cursor.next_block().is_some() {
            while let Some(instr) = cursor.next_instr() {
                if let &InstrKind::Call(CallKind::Call { func: called_func, .. }) = &func.instrs[instr].kind
                    && self.mir.function_sigs.pin().get(&called_func).unwrap().is_comptime {
                        comptime_calls.push((called_func, instr));
                    }
            }
        }

        for (func, _instr) in comptime_calls {
            let name_sym = self.mir.function_sigs.pin().get(&func).unwrap().name;
            let name = self.fn_name(Some(name_sym)).to_string();
            self.diag.push(
                Error::new(format!("unable to evaluate call to @comptime function '{}'", name))
            );
        }
    }

    fn validate_function(&self, func: &Function) {
        self.check_no_invalid_instructions(func);
        self.check_basic_block_params(func);
        self.check_entry_block_has_no_predecessors(func);
    }
}

impl Driver {
    fn get_result_value(&self, b: &mut FunctionBuilder, id: InstrId, instr: &InstrKind) -> ValueId {
        let ty = self.generate_type_of(b, instr);
        if matches!(ty, Type::Void) {
            VOID_VALUE
        } else {
            b.func.values.push(Value::new(ty, ValueKind::InstrResult(id)))
        }
    }

    fn generate_type_of_call(&self, call: &CallKind) -> Type {
        match *call {
            CallKind::Call { func, ref generic_arguments, .. } => {
                let function_sigs = self.mir.function_sigs.pin();
                let signature = function_sigs.get(&func).unwrap();
                let mut replacements = HashMap::new();
                for (param, arg) in range_iter(signature.generic_params.clone()).zip(generic_arguments) {
                    replacements.insert(param, arg.clone());
                }

                signature.ty.return_ty.as_ref().clone().replacing_generic_params(&replacements)
            },
            CallKind::ExternCall { func, .. } => self.mir.extern_mods.pin().get(&func.extern_mod).unwrap().imported_functions[func.index].ty.return_ty.as_ref().clone(),
            CallKind::LegacyIntrinsic { ref ty, .. } => ty.clone(),
            CallKind::Intrinsic { intr, .. } => self.mutable_state.try_read().unwrap().ast.intrinsics[intr].ret_ty.clone(),
        }
    }

    fn generate_type_of(&self, b: &FunctionBuilder, instr: &InstrKind) -> Type {
        match instr {
            InstrKind::Store { .. } => Type::Void,
            InstrKind::ObjcClassRef { .. } => Type::Void.ptr(),
            InstrKind::Invalid => Type::Error,
            InstrKind::Pointer { .. } | InstrKind::Struct { .. } | InstrKind::GenericParam(_) | InstrKind::Enum { .. } | InstrKind::FunctionTy { .. } => Type::Ty,
            &InstrKind::StructLit { ref fields, id } => {
                let field_tys = fields.iter()
                    .map(|&val| {
                        b.func.values[val].ty.clone()
                    })
                    .collect();
                Type::Struct(
                    StructType {
                        field_tys,
                        identity: id,
                    }
                )
            },
            InstrKind::Const(konst) => konst.ty(),
            InstrKind::Alloca(ty) => ty.clone().mut_ptr(),
            InstrKind::LogicalNot(_) => Type::Bool,
            InstrKind::Call(call) => self.generate_type_of_call(call),
            &InstrKind::FunctionRef { func, .. } => Type::Function(self.mir.function_sigs.pin().get(&func).unwrap().ty.clone()),
            InstrKind::Reinterpret(_, ty) | InstrKind::Truncate(_, ty) | InstrKind::SignExtend(_, ty)
            | InstrKind::ZeroExtend(_, ty) | InstrKind::FloatCast(_, ty) | InstrKind::FloatToInt(_, ty)
            | InstrKind::IntToFloat(_, ty)
            => ty.clone(),
            &InstrKind::Load(value) => match b.func.type_of_val(value) {
                Type::Pointer(pointee) => pointee.ty.clone(),
                _ => Type::Error,
            },
            &InstrKind::AddressOfStatic(statik) => self.mir.statics[statik].val.ty().mut_ptr(),
            &InstrKind::DirectFieldAccess { val, index } => {
                let base_ty = b.func.type_of_val(val);
                match base_ty {
                    Type::Struct(strukt) => strukt.field_tys[index].clone(),
                    _ => panic!("Cannot directly access field of non-struct type {:?}!", base_ty),
                }
            },
            &InstrKind::IndirectFieldAccess { val, index } => {
                let base_ty = b.func.type_of_val(val).deref().unwrap();
                match base_ty.ty {
                    Type::Struct(ref strukt) => strukt.field_tys[index].clone().ptr_with_mut(base_ty.is_mut),
                    _ => panic!("Cannot directly access field of non-struct type {:?}!", base_ty),
                }
            },
            InstrKind::InternalFieldAccess { field, .. } => field.ty(),
            &InstrKind::Variant { enuum, .. } => {
                let payload_tys = self.mir.enums.pin().get(&enuum).unwrap().payload_tys.to_vec();
                Type::Enum(
                    EnumType {
                        payload_tys,
                        identity: enuum,
                    }
                )
            },
            &InstrKind::PayloadAccess { val, variant_index } => {
                let base_ty = b.func.type_of_val(val);
                match base_ty {
                    &Type::Enum(EnumType { identity, .. }) => self.mir.enums.pin().get(&identity).unwrap().payload_tys[variant_index].clone(),
                    _ => panic!("Cannot directly access payload of non-enum type {:?}!", base_ty),
                }
            },
            InstrKind::DiscriminantAccess { .. } => TYPE_OF_DISCRIMINANTS, // TODO: update this when discriminants can be other types
        }
    }

    fn push_instr_and_get_id(&self, b: &mut FunctionBuilder, instr: InstrKind, item: impl Into<ToSourceRange>) -> (InstrId, ValueId) {
        let id = b.func.instrs.next_idx();
        let value = self.get_result_value(b, id, &instr);
        let source_range = self.get_range_with_mir_ctx(item, Some(&b.func));
        b.push_instr(id, instr, value, source_range);

        (id, value)
    }

    fn push_instr(&self, b: &mut FunctionBuilder, instr: InstrKind, item: impl Into<ToSourceRange>) -> ValueId {
        self.push_instr_and_get_id(b, instr, item).1
    }

    fn push_call(&self, b: &mut FunctionBuilder, call: CallKind, item: impl Into<ToSourceRange>) -> ValueId {
        self.push_instr(b, InstrKind::Call(call), item)
    }

    fn push_call_with_name(&self, b: &mut FunctionBuilder, call: CallKind, item: impl Into<ToSourceRange>, name: impl Into<String>) -> ValueId {
        self.push_instr_with_name(b, InstrKind::Call(call), item, name)
    }

    fn push_instr_with_name(&self, b: &mut FunctionBuilder, instr: InstrKind, item: impl Into<ToSourceRange>, name: impl Into<String>) -> ValueId {
        let value = self.push_instr(b, instr, item);
        b.func.register_value_name(value, name);

        value
    }
}

impl Driver {
    fn build_scope_item(&self, b: &mut FunctionBuilder, item: ScopedItem, tp: &dyn TypeProvider) {
        match item {
            ScopedItem::Expr { expr, .. } => {
                self.build_expr(b, expr, Context::new(0, DataDest::Void, ControlDest::Continue), tp);
            },
            ScopedItem::Decl(decl) => {
                let ms = self.mutable_state.try_read().unwrap();
                match df!(@ms, decl.ast) {
                    ast::Decl::Stored { id, root_expr, .. } => {
                        drop(ms);
                        let ty = tp.ty(root_expr).clone();
                        let name = self.display_item(None, decl).to_string();
                        let location = self.push_instr_with_name(b, InstrKind::Alloca(ty), decl, name);
                        b.stored_decl_locs.push_at(id, location);
                        let val = self.build_expr(b, root_expr, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                        let instr = self.handle_indirection(b, val);
                        let range = self.get_range_with_mir_ctx(location, Some(&b.func)) + self.get_range_with_mir_ctx(instr, Some(&b.func));
                        self.push_instr(b, InstrKind::Store { location, value: instr }, range);
                    },
                    ast::Decl::Function { .. } => {},
                    _ => panic!("Invalid scope item"),
                }
            },
        }
    }

    fn build_scope(&self, b: &mut FunctionBuilder, scope: ImperScopeId, ctx: Context, tp: &dyn TypeProvider) -> IndirectValue {
        let len = self.mutable_state.try_read().unwrap().ast.imper_scopes[scope].items.len();
        for i in 0..len {
            let item = self.mutable_state.try_read().unwrap().ast.imper_scopes[scope].items[i];
            self.build_scope_item(b, item, tp);
        }
        let terminal_expr = self.mutable_state.try_read().unwrap().ast.imper_scopes[scope].terminal_expr;
        self.build_expr(b, terminal_expr, ctx, tp)
    }
}

impl Driver {
    fn get_base(&self, id: DeclRefId) -> ExprId {
        match self.mutable_state.try_read().unwrap().ast.decl_refs[id].namespace {
            ast::Namespace::MemberRef { base_expr } => base_expr,
            _ => panic!("Expected member ref expression"),
        }
    }
}

impl Driver {
    fn get_callee_declref(&self, b: &mut FunctionBuilder, tp: &dyn TypeProvider, callee_id: ExprId) -> DeclRef {
        let ms = self.mutable_state.try_read().unwrap();
        let callee = ef!(@ms, &callee_id.ast);
        if let &Expr::DeclRef { id, .. } = callee {
            drop(ms);
            self.get(b, id, tp)
        } else {
            panic!("expected declref callee");
        }
    }

    fn get(&self, b: &mut FunctionBuilder, decl_ref_id: DeclRefId, tp: &dyn TypeProvider) -> DeclRef {
        let id = tp.selected_overload(decl_ref_id).expect("No overload found!");
        let generic_params = self.mutable_state.try_read().unwrap().tir_builder.decls[id].generic_params.clone();
        let generic_arguments = tp.generic_arguments(decl_ref_id).as_ref().unwrap_or(&Vec::new()).clone();
        assert_eq!(generic_params.end - generic_params.start, generic_arguments.len());
        let expr = self.mutable_state.try_read().unwrap().ast.decl_refs[decl_ref_id].expr;
        let name = self.display_item(None, id).to_string();
        match self.get_decl(id, tp) {
            Decl::Function { get } => DeclRef::Function { func: get, generic_args: generic_arguments },
            Decl::ExternFunction(func) => {
                assert!(generic_arguments.is_empty());
                DeclRef::ExternFunction { func }
            },
            Decl::ObjcClassRef { extern_mod, index } => {
                DeclRef::ObjcClassRef { extern_mod, index }
            },
            Decl::Stored(id) => {
                DeclRef::Value(b.stored_decl_locs[id].indirect())
            },
            Decl::PatternBinding { context, scrutinee, root_scrutinee  } => {
                let context_val = tp.pattern_matching_context(context).as_ref().expect("must set pattern matching context before MIR generation");
                let scrutinee_value = self.get_scrutinee_value(b, tp, root_scrutinee, scrutinee, context_val, context);
                DeclRef::Value(scrutinee_value)
            },
            Decl::Parameter { index } => {
                let entry_block = &b.func.blocks[b.func.entry_block];
                let param_value = entry_block.params[index];
                DeclRef::Value(param_value.direct())
            },
            Decl::GenericParam(param) => {
                DeclRef::Value(self.push_instr(b, InstrKind::GenericParam(param), expr).direct())
            },
            Decl::LegacyIntrinsic(intr, ref ty) => {
                let ty = ty.clone();
                DeclRef::LegacyIntrinsic { intrinsic: intr, ty }
            },
            Decl::Intrinsic(id) => DeclRef::Intrinsic(id),
            Decl::MethodIntrinsic(id) => DeclRef::MethodIntrinsic(id),
            Decl::Const(ref konst) => {
                let mut generic_replacements = HashMap::new();
                for (param, arg) in range_iter(generic_params).zip(generic_arguments) {
                    generic_replacements.insert(param, arg);
                }
                let konst = match konst.clone() {
                    Const::Ty(ty) => Const::Ty(ty.replacing_generic_params(&generic_replacements)),
                    other => other,
                };

                let name = self.fmt_const_for_instr_name(&konst).to_string();
                DeclRef::Value(self.push_instr_with_name(b, InstrKind::Const(konst), expr, name).direct())
            },
            Decl::Static(statik) => {
                DeclRef::Value(self.push_instr_with_name(b, InstrKind::AddressOfStatic(statik), expr, format!("static_{}", name)).indirect())
            },
            Decl::Field { index } => {
                let base = self.get_base(decl_ref_id);
                let base_ty = tp.ty(base);
                let mut base = self.build_expr(b, base, Context::default(), tp);
                if matches!(base_ty, Type::Pointer(_)) {
                    base.indirection += 1;
                }
                if base.indirection > 0 {
                    let base_ptr = self.handle_indirection(b, base.get_address());
                    DeclRef::Value(self.push_instr(b, InstrKind::IndirectFieldAccess { val: base_ptr, index }, expr).indirect())
                } else {
                    debug_assert_eq!(base.indirection, 0, "tried to dereference a struct?!");
                    DeclRef::Value(self.push_instr(b, InstrKind::DirectFieldAccess { val: base.value, index }, expr).direct())
                }
            },
            Decl::InternalField(field) => {
                let base = self.get_base(decl_ref_id);
                let base = self.build_expr(b, base, Context::default(), tp);
                let base = self.handle_indirection(b, base);
                DeclRef::Value(self.push_instr(b, InstrKind::InternalFieldAccess { val: base, field }, expr).direct())
            },
            Decl::Variant { enuum, index, payload_ty } => {
                if payload_ty.is_some() {
                    DeclRef::EnumVariantWithPayload { enuum, index, payload_ty }
                } else {
                    let payload_tys = self.mir.enums.pin().get(&enuum).unwrap().payload_tys.to_vec();
                    let konst = Const::Variant { enuum, index, payload_tys };
                    let name = self.fmt_const_for_instr_name(&konst).to_string();
                    DeclRef::Value(self.push_instr_with_name(b, InstrKind::Const(konst), expr, name).direct())
                }
            },
            Decl::Invalid => panic!("INVALID DECL"),
        }
    }

    fn build_if_expr_recurse(&self, b: &mut FunctionBuilder, condition: ExprId, then_scope: ImperScopeId, true_block: BlockId, false_block: BlockId, post_block: BlockId, pass_value_as_argument: bool, ctx: Context, tp: &dyn TypeProvider) {
        self.build_expr(
            b,
            condition,
            Context::new(0, DataDest::Branch(true_block, false_block), ControlDest::Continue),
            tp,
        );
        b.start_block(true_block);
        let scope_ctx = ctx.redirect(post_block, pass_value_as_argument);
        self.build_scope(b, then_scope, scope_ctx, tp);
    }
}

impl Function {
    fn create_block(&mut self) -> BlockId {
        let prev = self.last_block;

        let block = Block {
            prev: Some(prev),
            ..Default::default()
        };
        let block_id = self.blocks.push(block);
        assert_eq!(self.blocks[prev].next, None);
        self.blocks[prev].next = Some(block_id);
        self.last_block = block_id;
        block_id
    }

    pub fn parameter_tys(&self) -> impl Iterator<Item=&Type> {
        let block = &self.blocks[self.entry_block];
        block.params.iter().map(|&param| &self.values[param].ty)
    }

    pub fn num_parameters(&self) -> usize {
        self.parameter_tys().count()
    }

    pub fn type_of_val(&self, value: ValueId) -> &Type {
        &self.values[value].ty
    }

    pub fn type_of_instr(&self, instr: InstrId) -> &Type {
        let value = self.instrs[instr].value;
        &self.values[value].ty
    }

    fn push_param_on_block(&mut self, block_id: BlockId, ty: Type) -> ValueId {
        let block = &mut self.blocks[block_id];
        let next_index = block.params.len();
        let param = self.values.push(Value::new(ty, ValueKind::Parameter { block: block_id, index: next_index }));
        block.params.push(param);
        param
    }

    fn push_instr(&mut self, block_id: BlockId, id: InstrId, instr: InstrKind, value: ValueId, source_range: SourceRange) {
        let instr = Instr::new(instr, value);
        self.instrs.push_at(id, instr);
        let block = &mut self.blocks[block_id];
        let prev = block.last_instr;
        self.instr_nodes.push_at(id, InstrNode { block: Some(block_id), prev, next: None });
        block.last_instr = Some(id);
        if block.first_instr.is_none() {
            block.first_instr = Some(id);
        }
        if let Some(prev) = prev {
            self.instr_nodes[prev].next = Some(id);
        }
        self.source_ranges.insert(id, source_range);
    }

    fn register_value_name(&mut self, value: ValueId, name: impl Into<String>) {
        let name = self.value_namespace.insert(name.into());
        self.value_names.insert(value, name);
    }

    fn prev_block(&self, block_id: BlockId) -> Option<BlockId> {
        self.blocks[block_id].prev
    }

    #[allow(unused)]
    fn next_block(&self, block_id: BlockId) -> Option<BlockId> {
        self.blocks[block_id].next
    }

    fn prev_instr(&self, instr_id: InstrId) -> Option<InstrId> {
        self.instr_nodes[instr_id].prev
    }

    #[allow(unused)]
    fn next_instr(&self, instr_id: InstrId) -> Option<InstrId> {
        self.instr_nodes[instr_id].next
    }

    fn remove_block(&mut self, block_id: BlockId) {
        assert!(block_id != self.first_block || block_id != self.last_block, "cannot remove only block in function");
        assert!(block_id != self.entry_block, "cannot remove entry block");

        let block = &mut self.blocks[block_id];
        let prev = block.prev;
        let next = block.next;
        block.prev = None;
        block.next = None;
        if let Some(prev) = prev {
            if self.last_block == block_id {
                self.last_block = prev;
            }
            let prev = &mut self.blocks[prev];
            debug_assert_eq!(prev.next, Some(block_id));
            prev.next = next;
        }
        if let Some(next) = next {
            if self.first_block == block_id {
                self.first_block = next;
            }
            let next = &mut self.blocks[next];
            debug_assert_eq!(next.prev, Some(block_id));
            next.prev = prev;
        }
    }

    fn remove_instr(&mut self, instr_id: InstrId) {
        let instr = &mut self.instr_nodes[instr_id];
        let block_id = instr.block.expect("instruction not part of a block");
        let block = &mut self.blocks[block_id];

        let first_instr = block.first_instr.expect("instruction is part of block with no first instruction");
        let last_instr = block.last_instr.expect("instruction is part of block with no last instruction");

        let prev = instr.prev;
        let next = instr.next;

        if last_instr == instr_id {
            block.last_instr = prev;
        }
        if first_instr == instr_id {
            block.first_instr = next;
        }

        instr.prev = None;
        instr.next = None;
        instr.block = None;
        if let Some(prev) = prev {
            let prev_node = &mut self.instr_nodes[prev];
            debug_assert_eq!(prev_node.next, Some(instr_id));
            prev_node.next = next;
        }
        if let Some(next) = next {
            let next_node = &mut self.instr_nodes[next];
            debug_assert_eq!(next_node.prev, Some(instr_id));
            next_node.prev = prev;
        }
    }

    fn replace_uses(&mut self, uses: &ValueUses, old: ValueId, new: ValueId) {
        let uses = uses.get(old);
        for &instr_id in &uses.instrs {
            self.instrs[instr_id].kind.replace_value(old, new);
        }
        for &block_id in &uses.terminators {
            self.blocks[block_id].terminator.as_mut().unwrap().replace_value(old, new)
        }
    }

    fn visit_uses(&self, mut instr_visitor: impl FnMut(ValueId, InstrId), mut terminator_visitor: impl FnMut(ValueId, BlockId)) {
        let mut cursor = self.make_cursor();
        while let Some(block_id) = cursor.next_block() {
            while let Some(instr_id) = cursor.next_instr() {
                let instr = &cursor.func.instrs[instr_id].kind;
                instr.visit_values(&mut |value| {
                    instr_visitor(value, instr_id);
                });
            }
            if let Some(terminator) = cursor.terminator() {
                terminator.visit_values(&mut |value| {
                    terminator_visitor(value, block_id);
                });
            }
        }
    }

    fn get_uses(&self) -> ValueUses {
        let uses = RefCell::<ValueUses>::default();
        self.visit_uses(|value, using_instr| {
            uses.borrow_mut().add_using_instr(value, using_instr);
        },
    |value, using_terminator| {
            uses.borrow_mut().add_using_terminator(value, using_terminator);
        });
        uses.into_inner()
    }
}

#[derive(Default)]
struct SingleValueUses {
    instrs: Vec<InstrId>,
    terminators: Vec<BlockId>,
}

impl SingleValueUses {
    const fn new() -> Self {
        Self {
            instrs: Vec::new(),
            terminators: Vec::new(),
        }
    }

    const fn is_used(&self) -> bool {
        !self.instrs.is_empty() || !self.terminators.is_empty()
    }
}

const EMPTY_USES: &'static SingleValueUses = &SingleValueUses::new();

#[derive(Default)]
struct ValueUses {
    uses: HashMap<ValueId, SingleValueUses>,
}

impl ValueUses {
    fn add_using_instr(&mut self, value: ValueId, using_instr: InstrId) {
        self.uses.entry(value).or_default().instrs.push(using_instr);
    }

    fn add_using_terminator(&mut self, value: ValueId, using_terminator: BlockId) {
        self.uses.entry(value).or_default().terminators.push(using_terminator);
    }

    fn get(&self, value: ValueId) -> &SingleValueUses {
        self.uses.get(&value).unwrap_or(EMPTY_USES)
    }
}

impl FunctionBuilder {
    fn get_block_state(&mut self, block: BlockId) -> &mut BlockState {
        self.block_states.entry(block).or_insert(BlockState::Created)
    }

    fn start_block(&mut self, block: BlockId) {
        let state = self.get_block_state(block);
        match state {
            BlockState::Created => {
                *state = BlockState::Started;
            }
            BlockState::Started => {},
            BlockState::Ended => panic!("internal compiler error: tried to start basic block after it was already ended"),
        }
        self.current_block = block;
    }

    pub fn end_block(&mut self, block: BlockId) -> Result<(), EndBlockError> {
        let state = self.get_block_state(block);
        match state {
            BlockState::Created => Err(EndBlockError::BlockNotStarted),
            BlockState::Started => {
                *state = BlockState::Ended;
                Ok(())
            },
            BlockState::Ended => Err(EndBlockError::BlockEnded),
        }
    }

    pub fn first_unended_block(&self) -> Option<BlockId> {
        for block_id in self.func.make_cursor().block_ids_iter() {
            let state = &self.block_states[&block_id];
            if !matches!(state, BlockState::Ended) {
                return Some(block_id)
            }
        }
        None
    }

    pub fn check_all_blocks_ended(&self) {
        if let Some(block) = self.first_unended_block() {
            panic!("MIR: Block {} was not ended", block.index());
        }
    }

    fn push_instr(&mut self, id: InstrId, instr: InstrKind, value: ValueId, source_range: SourceRange) {
        self.func.push_instr(self.current_block, id, instr, value, source_range)
    }

    fn push_param(&mut self, ty: Type) -> ValueId {
        self.func.push_param_on_block(self.current_block, ty)
    }
}

impl Driver {
    fn build_if_expr(&self, b: &mut FunctionBuilder, _expr: ExprId, ty: Type, condition: ExprId, then_scope: ImperScopeId, else_scope: Option<ImperScopeId>, ctx: Context, tp: &dyn TypeProvider) -> IndirectValue {
        let true_block = b.func.create_block();
        let mut false_block: BlockId = b.func.create_block();
        if matches!(ctx.data, DataDest::Read) {
            assert_matches!(ctx.control, ControlDest::Continue);
        }
        let pass_value_as_argument = matches!((&ctx.data, else_scope), (DataDest::Read, Some(_)));
        let (post_block, we_own_post_block) = match ctx.control {
            ControlDest::Jump(target) => (target, false),
            _ => (b.func.create_block(), true),
        };
        if else_scope.is_none() && !pass_value_as_argument {
            false_block = post_block;
        }

        self.build_if_expr_recurse(b, condition, then_scope, true_block, false_block, post_block, pass_value_as_argument, ctx, tp);
        let mut next_scope = else_scope;
        let mut next_block = false_block;

        // Iterate through a linked list of if-else-if branches. Terminate when you find an if with no else branch or
        // an else branch with something other than an if.
        // This is done to reduce the size of the stack when generating code for long chains of if statements.
        while let Some(cur) = next_scope {
            b.start_block(next_block);

            let scope_ctx = ctx.redirect(post_block, pass_value_as_argument);
            let terminal_expr = self.mutable_state.try_read().unwrap().ast.imper_scopes[cur].terminal_expr;
            // If the current scope consists of a lone if expression
            if self.mutable_state.try_read().unwrap().ast.imper_scopes[cur].items.is_empty() {
                let ms = self.mutable_state.try_read().unwrap();
                if let &Expr::If { condition, then_scope, else_scope } = ef!(@ms, &terminal_expr.ast) {
                    drop(ms);
                    let true_block = b.func.create_block();
                    let false_block = if else_scope.is_some() {
                        b.func.create_block()
                    } else {
                        post_block
                    };
                    self.build_if_expr_recurse(b, condition, then_scope, true_block, false_block, post_block, pass_value_as_argument, scope_ctx, tp);
                    next_scope = else_scope;
                    next_block = false_block;
                    continue;
                }
            }

            self.build_scope(b, cur, scope_ctx, tp);
            next_scope = None;
        }

        if we_own_post_block {
            b.start_block(post_block);
            if pass_value_as_argument {
                // TODO: this might be the wrong type if indirection != 0
                b.push_param(ty).direct()
            } else if else_scope.is_none() {
                self.handle_context(b, VOID_VALUE.direct(), ctx)
            } else {
                VOID_VALUE.direct()
            }
        } else {
            VOID_VALUE.direct()
        }
    }

    fn build_expr(&self, b: &mut FunctionBuilder, expr: ExprId, ctx: Context, tp: &dyn TypeProvider) -> IndirectValue {
        let ty = self.get_canonical_type(tp, expr);

        let expr_val = {
            ef!(expr.ast).clone()
        };

        let val = match expr_val {
            Expr::Void | Expr::Error | Expr::ExtendBlock { .. } => {
                VOID_VALUE.direct()
            },
            Expr::IntLit { .. } | Expr::DecLit { .. } | Expr::CharLit { .. } | Expr::StrLit { .. } | Expr::BoolLit { .. } | Expr::Const(_) | Expr::Mod { .. } => {
                let konst = self.expr_to_const(expr, ty);
                let name = self.fmt_const_for_instr_name(&konst).to_string();
                self.push_instr_with_name(b, InstrKind::Const(konst), expr, name).direct()
            },
            Expr::Set { lhs, rhs } => {
                let ctx = ctx.new_data_dest(DataDest::Read);
                let val = self.build_expr(
                    b,
                    rhs,
                    ctx,
                    tp,
                );
                let instr = self.handle_indirection(b, val);
                return self.build_expr(
                    b,
                    lhs,
                    Context::new(0, DataDest::Receive { value: instr }, ctx.control),
                    tp,
                );
            },
            Expr::DeclRef { id, .. } => {
                let decl_ref = self.get(b, id, tp);

                match decl_ref {
                    DeclRef::Function { func, generic_args } => {
                        self.push_instr(b, InstrKind::FunctionRef { func, generic_arguments: generic_args }, expr).direct()
                    },
                    DeclRef::LegacyIntrinsic { intrinsic, ty } => {
                        assert!(tp.ty(expr).return_ty().is_none(), "referring to intrinsic functions is not yet supported");
                        self.push_call(b, CallKind::LegacyIntrinsic { arguments: SmallVec::new(), ty, intr: intrinsic }, expr).direct()
                    },
                    DeclRef::Value(value) => value,
                    DeclRef::ObjcClassRef { extern_mod, index } => self.push_instr(b, InstrKind::ObjcClassRef { extern_mod, index }, expr).direct(),
                    other => todo!("referring to {:?} not yet supported", other),
                }
            },
            Expr::Call { callee, ref arguments } => {
                let arguments = arguments.clone();
                let decl_ref = self.get_callee_declref(b, tp, callee);

                fn get_args(d: &Driver, b: &mut FunctionBuilder, tp: &dyn TypeProvider, arguments: &[ExprId]) -> SmallVec<[ValueId; 2]> {
                    arguments.iter().map(|&argument| {
                        let val = d.build_expr(b, argument, Context::default(), tp);
                        d.handle_indirection(b, val)
                    }).collect()
                }

                match decl_ref {
                    DeclRef::Function { func, generic_args } => {
                        let Expr::DeclRef { id: decl_ref_id, .. } = ef!(callee.ast) else {
                            panic!("expected declref callee");
                        };
                        let mut arguments = get_args(self, b, tp, &arguments);
                        // Handle method calls.
                        // TODO: comparing the number of arguments to the number of parameters to determine whether this is a method call is kind of a horrible hack
                        // TODO: unify code here with the near-identical `DeclRef::MethodIntrinsic` case
                        if arguments.len() != self.mir.function_sigs.pin().get(&func).unwrap().ty.param_tys.len() {
                            let base = self.get_base(decl_ref_id);
                            let base_ty = tp.ty(base);
                            let self_ty = self.mir.function_sigs.pin().get(&func).unwrap().ty.param_tys.first().unwrap().clone();
                            let indirection = !base_ty.trivially_convertible_to(&self_ty) as i8;
                            let base = self.build_expr(b, base, Context::new(indirection, DataDest::Read, ControlDest::Continue), tp);
                            let base = self.handle_indirection(b, base);

                            arguments.insert(0, base);
                        }
                        self.push_call(b, CallKind::Call { func, arguments, generic_arguments: generic_args }, expr).direct()
                    },
                    DeclRef::ExternFunction { func } => {
                        let arguments = get_args(self, b, tp, &arguments);
                        self.push_call(b, CallKind::ExternCall { arguments, func }, expr).direct()
                    },
                    DeclRef::LegacyIntrinsic { intrinsic, ty } => match intrinsic {
                        LegacyIntrinsic::LogicalAnd => {
                            assert_eq!(arguments.len(), 2);
                            let (lhs, rhs) = (arguments[0], arguments[1]);
                            let left_true_block = b.func.create_block();
                            let pass_value_as_argument = matches!(ctx.data, DataDest::Read);
                            if let DataDest::Branch(true_block, false_block) = ctx.data {
                                self.build_expr(
                                    b,
                                    lhs,
                                    Context::new(0, DataDest::Branch(left_true_block, false_block), ControlDest::Continue),
                                    tp,
                                );

                                b.start_block(left_true_block);
                                return self.build_expr(
                                    b,
                                    rhs,
                                    Context::new(0, DataDest::Branch(true_block, false_block), ControlDest::Continue),
                                    tp,
                                );
                            } else {
                                let left_false_block = b.func.create_block();
                                let after_block = b.func.create_block();
                                self.build_expr(
                                    b,
                                    lhs,
                                    Context::new(0, DataDest::Branch(left_true_block, left_false_block), ControlDest::Continue),
                                    tp,
                                );

                                b.start_block(left_true_block);
                                // No further branching required, because (true && foo) <=> foo
                                let branch_ctx = ctx.redirect(after_block, pass_value_as_argument);
                                self.build_expr(b, rhs, branch_ctx, tp);

                                b.start_block(left_false_block);
                                let false_const = Const::Bool(false);
                                let name = self.fmt_const_for_instr_name(&false_const).to_string();
                                let false_val = self.push_instr_with_name(b, InstrKind::Const(false_const), expr, name).direct();
                                self.handle_context(b, false_val, branch_ctx);

                                b.start_block(after_block);
                                if pass_value_as_argument {
                                    b.push_param(ty).direct()
                                } else {
                                    return VOID_VALUE.direct()
                                }
                            }
                        },
                        LegacyIntrinsic::LogicalOr => {
                            assert_eq!(arguments.len(), 2);
                            let (lhs, rhs) = (arguments[0], arguments[1]);
                            let left_false_block = b.func.create_block();
                            if let DataDest::Branch(true_block, false_block) = ctx.data {
                                self.build_expr(
                                    b,
                                    lhs,
                                    Context::new(0, DataDest::Branch(true_block, left_false_block), ControlDest::Continue),
                                    tp,
                                );

                                b.start_block(left_false_block);
                                return self.build_expr(
                                    b,
                                    rhs,
                                    Context::new(0, DataDest::Branch(true_block, false_block), ControlDest::Continue),
                                    tp
                                );
                            } else {
                                let left_true_block = b.func.create_block();
                                let after_block = b.func.create_block();
                                let pass_value_as_argument = matches!(ctx.data, DataDest::Read);
                                self.build_expr(
                                    b,
                                    lhs,
                                    Context::new(0, DataDest::Branch(left_true_block, left_false_block), ControlDest::Continue),
                                    tp,
                                );

                                b.start_block(left_true_block);
                                let true_const = Const::Bool(true);
                                let name = self.fmt_const_for_instr_name(&true_const).to_string();
                                let true_val = self.push_instr_with_name(b, InstrKind::Const(true_const), expr, name).direct();
                                let branch_ctx = ctx.redirect(after_block, pass_value_as_argument);
                                self.handle_context(b, true_val, branch_ctx);

                                b.start_block(left_false_block);
                                self.build_expr(b, rhs, branch_ctx, tp);

                                b.start_block(after_block);
                                if pass_value_as_argument {
                                    b.push_param(ty).direct()
                                } else {
                                    return VOID_VALUE.direct()
                                }
                            }
                        },
                        LegacyIntrinsic::LogicalNot => {
                            assert_eq!(arguments.len(), 1);
                            let operand = arguments[0];
                            if let DataDest::Branch(true_block, false_block) = ctx.data {
                                return self.build_expr(b, operand, Context::new(0, DataDest::Branch(false_block, true_block), ctx.control), tp)
                            } else {
                                let operand = self.build_expr(b, operand, Context::default(), tp);
                                self.push_instr(b, InstrKind::LogicalNot(operand.value), expr).direct()
                            }
                        },
                        LegacyIntrinsic::MultAssign | LegacyIntrinsic::DivAssign | LegacyIntrinsic::ModAssign | LegacyIntrinsic::AddAssign
                            | LegacyIntrinsic::SubAssign | LegacyIntrinsic::AndAssign | LegacyIntrinsic::OrAssign | LegacyIntrinsic::XorAssign
                            | LegacyIntrinsic::LeftShiftAssign | LegacyIntrinsic::RightShiftAssign => {
                            assert_eq!(arguments.len(), 2);
                            let lhs = arguments[0];
                            let rhs = arguments[1];

                            let ty = tp.ty(lhs).clone();

                            let address = self.build_expr(b, lhs, Context::new(1, DataDest::Read, ControlDest::Continue), tp);
                            let address = self.handle_indirection(b, address);
                            let loaded = self.push_instr(b, InstrKind::Load(address), lhs);
                            let modifier = self.build_expr(b, rhs, Context::default(), tp);
                            let modifier = self.handle_indirection(b, modifier);
                            let intr = match intrinsic {
                                LegacyIntrinsic::MultAssign => LegacyIntrinsic::Mult,
                                LegacyIntrinsic::DivAssign => LegacyIntrinsic::Div,
                                LegacyIntrinsic::ModAssign => LegacyIntrinsic::Mod,
                                LegacyIntrinsic::AddAssign => LegacyIntrinsic::Add,
                                LegacyIntrinsic::SubAssign => LegacyIntrinsic::Sub,
                                LegacyIntrinsic::AndAssign => LegacyIntrinsic::BitwiseAnd,
                                LegacyIntrinsic::OrAssign => LegacyIntrinsic::BitwiseOr,
                                LegacyIntrinsic::XorAssign => LegacyIntrinsic::BitwiseXor,
                                LegacyIntrinsic::LeftShiftAssign => LegacyIntrinsic::LeftShift,
                                LegacyIntrinsic::RightShiftAssign => LegacyIntrinsic::RightShift,
                                intrinsic => todo!("intrinsic {:?}", intrinsic),
                            };
                            let value = self.push_call(b, CallKind::LegacyIntrinsic { arguments: smallvec![loaded, modifier], ty, intr }, expr);
                            self.push_instr(b, InstrKind::Store { location: address, value }, expr).direct()
                        },
                        intrinsic => {
                            let arguments = get_args(self, b, tp, &arguments);
                            self.push_call(b, CallKind::LegacyIntrinsic { arguments, ty, intr: intrinsic }, expr).direct()
                        }
                    },
                    DeclRef::Intrinsic(intr) => {
                        let arguments = get_args(self, b, tp, &arguments);
                        self.push_call(b, CallKind::Intrinsic { arguments, intr }, expr).direct()
                    },
                    DeclRef::MethodIntrinsic(intr) => {
                        let Expr::DeclRef { id: decl_ref_id, .. } = ef!(callee.ast) else {
                            panic!("expected declref callee");
                        };
                        let base = self.get_base(decl_ref_id);
                        let base_ty = tp.ty(base);
                        let self_ty = self.mutable_state.try_read().unwrap().ast.intrinsics[intr].param_tys[0];
                        let self_ty = tp.get_evaluated_type(self_ty);
                        let indirection = !base_ty.trivially_convertible_to(self_ty) as i8;
                        let base = self.build_expr(b, base, Context::new(indirection, DataDest::Read, ControlDest::Continue), tp);
                        let base = self.handle_indirection(b, base);
                        let mut arguments = get_args(self, b, tp, &arguments);
                        arguments.insert(0, base);
                        self.push_call(b, CallKind::Intrinsic { arguments, intr }, expr).direct()
                    },
                    DeclRef::EnumVariantWithPayload { enuum, index, .. } => {
                        let arguments = get_args(self, b, tp, &arguments);
                        self.push_instr(b, InstrKind::Variant { enuum, index, payload: arguments[0] }, expr).direct()
                    },
                    DeclRef::Value(_) => todo!("calling function pointers is not yet supported"),
                    DeclRef::ObjcClassRef { .. } => unimplemented!("can't call obj-c class ref"),
                }
            },
            Expr::Cast { expr: operand, ty: dest_ty, cast_id } => {
                let dest_ty = tp.get_evaluated_type(dest_ty).clone();
                match tp.cast_method(cast_id) {
                    CastMethod::Noop => return self.build_expr(b, operand, ctx, tp),
                    CastMethod::Reinterpret => {
                        let value = self.build_expr(b, operand, Context::default(), tp);
                        let value = self.handle_indirection(b, value);
                        self.push_instr(b, InstrKind::Reinterpret(value, dest_ty), expr).direct()
                    },
                    CastMethod::Int => {
                        let (src_width, _src_is_signed, dest_width, dest_is_signed) = match (tp.ty(operand), &dest_ty) {
                            (&Type::Int { width: ref src_width, is_signed: src_is_signed }, &Type::Int { width: ref dest_width, is_signed: dest_is_signed })
                                => (src_width, src_is_signed, dest_width, dest_is_signed),
                            (a, b) => panic!("Internal compiler error: found invalid cast types while generating MIR ({:?}, {:?})", a, b)
                        };
                        let (src_bit_width, dest_bit_width) = (src_width.bit_width(self.arch), dest_width.bit_width(self.arch));
                        let value = self.build_expr(b, operand, Context::default(), tp);
                        let value = self.handle_indirection(b, value);

                        match src_bit_width.cmp(&dest_bit_width) {
                            Ordering::Less => if dest_is_signed {
                                // TODO: Bounds checking
                                self.push_instr(b, InstrKind::SignExtend(value, dest_ty), expr)
                            } else {
                                // TODO: Bounds checking
                                self.push_instr(b, InstrKind::ZeroExtend(value, dest_ty), expr)
                            },
                            Ordering::Equal => {
                                // TODO: Bounds checking
                                self.push_instr(b, InstrKind::Reinterpret(value, dest_ty), expr)
                            },
                            Ordering::Greater => {
                                // TODO: Bounds checking
                                self.push_instr(b, InstrKind::Truncate(value, dest_ty), expr)
                            },
                        }.direct()
                    },
                    CastMethod::Float => {
                        let value = self.build_expr(b, operand, Context::default(), tp);
                        let value = self.handle_indirection(b, value);
                        self.push_instr(b, InstrKind::FloatCast(value, dest_ty), expr).direct()
                    },
                    CastMethod::FloatToInt => {
                        let value = self.build_expr(b, operand, Context::default(), tp);
                        let value = self.handle_indirection(b, value);
                        self.push_instr(b, InstrKind::FloatToInt(value, dest_ty), expr).direct()
                    },
                    CastMethod::IntToFloat => {
                        let value = self.build_expr(b, operand, Context::default(), tp);
                        let value = self.handle_indirection(b, value);
                        self.push_instr(b, InstrKind::IntToFloat(value, dest_ty), expr).direct()
                    },
                    CastMethod::Invalid => panic!("FOUND INVALID CAST"),
                }
            },
            Expr::AddrOf { expr: operand, .. } => {
                return self.build_expr(
                    b,
                    operand,
                    Context::new(ctx.indirection + 1, ctx.data, ctx.control),
                    tp,
                )
            },
            Expr::Pointer { expr: operand, is_mut } => {
                let val = self.build_expr(
                    b,
                    operand,
                    Context::default(),
                    tp,
                );
                let value = self.handle_indirection(b, val);
                self.push_instr(b, InstrKind::Pointer { value, is_mut }, expr).direct()
            },
            Expr::FunctionTy { ref param_tys, has_c_variadic_param, ret_ty } => {
                let param_tys = param_tys.clone();
                let param_tys: Vec<_> = param_tys.iter()
                    .map(|&ty| {
                        let param_ty = self.build_expr(
                            b,
                            ty,
                            Context::default(),
                            tp,
                        );
                        self.handle_indirection(b, param_ty)
                    }).collect();
                let ret_ty = self.build_expr(
                    b,
                    ret_ty,
                    Context::default(),
                    tp,
                );
                let ret_ty = self.handle_indirection(b, ret_ty);

                self.push_instr(b, InstrKind::FunctionTy { param_tys, has_c_variadic_param, ret_ty }, expr).direct()
            }
            Expr::Struct(id) => {
                let mut fields = SmallVec::new();
                let len = self.mutable_state.try_read().unwrap().ast.structs[id].fields.len();
                for i in 0..len {
                    let field_ty = self.mutable_state.try_read().unwrap().ast.structs[id].fields[i].ty;
                    let field = self.build_expr(
                        b,
                        field_ty,
                        Context::default(),
                        tp,
                    );
                    let field = self.handle_indirection(b, field);
                    fields.push(field);
                }
                self.push_instr(b, InstrKind::Struct { fields, id }, expr).direct()
            },
            Expr::Enum(id) => {
                let mut variants = SmallVec::new();
                let len = self.mutable_state.try_read().unwrap().ast.enums[id].variants.len();
                for i in 0..len {
                    let payload_ty = self.mutable_state.try_read().unwrap().ast.enums[id].variants[i].payload_ty.unwrap_or(VOID_TYPE);
                    let variant = self.build_expr(
                        b,
                        payload_ty,
                        Context::default(),
                        tp,
                    );
                    let variant = self.handle_indirection(b, variant);
                    variants.push(variant);
                }
                self.push_instr(b, InstrKind::Enum { variants, id }, expr).direct()
            },
            Expr::StructLit { id, .. } => {
                let lit = tp.struct_lit(id).as_ref().unwrap();
                let mut fields = SmallVec::new();
                for field in &lit.fields {
                    let field = self.build_expr(
                        b,
                        field.expr,
                        Context::default(),
                        tp,
                    );
                    let field = self.handle_indirection(b, field);
                    fields.push(field);
                }
                self.push_instr(b, InstrKind::StructLit { fields, id: lit.strukt }, expr).direct()
            },
            Expr::Deref(operand) => {
                return self.build_expr(
                    b,
                    operand,
                    Context::new(ctx.indirection - 1, ctx.data, ctx.control),
                    tp,
                )
            },
            Expr::Do { scope } => {
                return self.build_scope(b, scope, ctx, tp)
            },
            Expr::If { condition, then_scope, else_scope } => {
                return self.build_if_expr(b, expr, ty, condition, then_scope, else_scope, ctx, tp)
            },
            Expr::Switch { scrutinee, context: pattern_matching_ctx_id, ref cases } => {
                let _cases = cases.clone();
                let pass_value_as_argument = matches!(ctx.data, DataDest::Read);
                let post_block = b.func.create_block();
                let scope_ctx = ctx.redirect(post_block, pass_value_as_argument);
                let decision_tree = tp.switch_expr_decision_tree(pattern_matching_ctx_id).as_ref().expect("should always set decision tree on switch expr");
                let pattern_matching_ctx = tp.pattern_matching_context(pattern_matching_ctx_id).as_ref().expect("should always set pattern matching context on switch expr");
                self.handle_pattern_matching(b, expr, scope_ctx, tp, scrutinee, decision_tree, pattern_matching_ctx_id, pattern_matching_ctx);

                b.start_block(post_block);
                if pass_value_as_argument {
                    b.push_param(ty).direct()
                } else {
                    self.handle_control(b, VOID_VALUE.direct(), ctx.control)
                }
            },
            Expr::While { loop_id, condition, scope } => {
                let test_block = b.func.create_block();
                let loop_block = b.func.create_block();
                let post_block = match ctx.control {
                    ControlDest::Continue | ControlDest::Unreachable | ControlDest::RetVoid | ControlDest::IncrementVariableAndThenJump { .. } => b.func.create_block(),
                    ControlDest::Jump(block) => block,
                };

                self.end_current_block(b, TerminatorKind::Jump(test_block.into()));
                b.start_block(test_block);
                self.build_expr(b, condition, Context::new(0, DataDest::Branch(loop_block, post_block), ControlDest::Continue), tp);

                b.start_block(loop_block);
                let loop_state = LoopState {
                    break_block: post_block,
                    continue_block: test_block,
                    continue_location_of_variable_to_increment: None,
                };
                b.loops.push_at(loop_id, loop_state);
                self.build_scope(b, scope, Context::new(0, DataDest::Void, ControlDest::Jump(test_block)), tp);

                match ctx.control {
                    ControlDest::Continue | ControlDest::Unreachable | ControlDest::RetVoid | ControlDest::IncrementVariableAndThenJump { .. } => {
                        b.start_block(post_block);
                        VOID_VALUE.direct()
                    },
                    // Already handled this above
                    ControlDest::Jump(_) => return VOID_VALUE.direct(),
                }
            },
            Expr::For { loop_id, binding, lower_bound, upper_bound, scope } => {
                let ast::Decl::LoopBinding { id: binding_stored_decl_id, .. } = df!(binding.ast) else {
                    panic!("incorrect type of decl found in decl binding id");
                };
                let binding_ty = tp.ty(lower_bound).clone();
                let binding_name = self.display_item(None, binding).to_string();
                let binding_location = self.push_instr_with_name(b, InstrKind::Alloca(binding_ty), binding, &binding_name);
                b.stored_decl_locs.push_at(binding_stored_decl_id, binding_location);
                let val = self.build_expr(b, lower_bound, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                let instr = self.handle_indirection(b, val);
                let range = self.get_range_with_mir_ctx(binding_location, Some(&b.func)) + self.get_range_with_mir_ctx(instr, Some(&b.func));
                self.push_instr(b, InstrKind::Store { location: binding_location, value: instr }, range);
                let upper_bound = self.build_expr(b, upper_bound, Context::default(), tp);
                let upper_bound = self.handle_indirection(b, upper_bound);

                let test_block = b.func.create_block();
                let loop_block = b.func.create_block();
                let post_block = match ctx.control {
                    ControlDest::Continue | ControlDest::Unreachable | ControlDest::RetVoid | ControlDest::IncrementVariableAndThenJump { .. } => b.func.create_block(),
                    ControlDest::Jump(target) => target,
                };

                self.end_current_block(b, TerminatorKind::Jump(test_block.into()));
                b.start_block(test_block);
                let cur_binding_value = self.push_instr_with_name(b, InstrKind::Load(binding_location), binding, &binding_name);
                let less_than = self.push_call_with_name(b, CallKind::LegacyIntrinsic { arguments: smallvec![cur_binding_value, upper_bound], ty: Type::Bool, intr: LegacyIntrinsic::Less }, binding, &binding_name);
                self.end_current_block(b, TerminatorKind::CondBr { condition: less_than, true_target: loop_block.into(), false_target: post_block.into() });

                b.start_block(loop_block);
                let loop_state = LoopState {
                    break_block: post_block,
                    continue_block: test_block,
                    continue_location_of_variable_to_increment: Some(binding_location),
                };
                b.loops.push_at(loop_id, loop_state);
                self.build_scope(b, scope, Context::new(0, DataDest::Void, ControlDest::IncrementVariableAndThenJump { location: binding_location, target: test_block }), tp);

                match &ctx.control {
                    ControlDest::Continue | ControlDest::Unreachable | ControlDest::RetVoid | ControlDest::IncrementVariableAndThenJump { .. } => {
                        b.start_block(post_block);
                        VOID_VALUE.direct()
                    },
                    // Already handled this above
                    ControlDest::Jump(_) => return VOID_VALUE.direct(),
                }
            },
            Expr::Break(loop_id) => {
                let loop_id = loop_id.expect("loop id should be filled in by MIR generation time");
                let loop_state = b.loops[loop_id].clone();
                self.end_current_block(b, TerminatorKind::Jump(loop_state.break_block.into()));
                // Must create unreachable basic block in case there are more statements in this loop
                let unreachable_block = b.func.create_block();
                b.start_block(unreachable_block);

                return NEVER_VALUE.direct();
            },
            Expr::Continue(loop_id) => {
                let loop_id = loop_id.expect("loop id should be filled in by MIR generation time");
                let loop_state = b.loops[loop_id].clone();
                if let Some(variable_to_increment) = loop_state.continue_location_of_variable_to_increment {
                    self.increment_variable(b, variable_to_increment);
                }
                self.end_current_block(b, TerminatorKind::Jump(loop_state.continue_block.into()));
                // Must create unreachable basic block in case there are more statements in this loop
                let unreachable_block = b.func.create_block();
                b.start_block(unreachable_block);

                return NEVER_VALUE.direct();
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
        self.handle_context(b, val, ctx)
    }

    fn get_scrutinee_value(&self, b: &mut FunctionBuilder, tp: &dyn TypeProvider, og_scrutinee: ExprId, scrutinee: SwitchScrutineeValueId, pattern_matching_ctx: &IndexVec<SwitchScrutineeValueId, TypedSwitchScrutineeValue>, pattern_matching_ctx_id: PatternMatchingContextId) -> IndirectValue {
        let scrutinee_values = b.pattern_matching_scrutinees.entry(pattern_matching_ctx_id).or_default();
        if let Some(scrutinee) = scrutinee_values.get(&scrutinee) {
            return *scrutinee;
        }

        let scrutinee_value = &pattern_matching_ctx[scrutinee];
        let value = match scrutinee_value.kind {
            TypedSwitchScrutineeValueKind::EnumPayload { enum_value, variant_index } => {
                let val = self.get_scrutinee_value(b, tp, og_scrutinee, enum_value, pattern_matching_ctx, pattern_matching_ctx_id);
                let val = self.handle_indirection(b, val);
                self.push_instr(b, InstrKind::PayloadAccess { val, variant_index }, og_scrutinee).direct()
            },
            TypedSwitchScrutineeValueKind::OriginalScrutinee => {
                self.build_expr(b, og_scrutinee, Context::new(0, DataDest::Read, ControlDest::Continue), tp)
            },
            TypedSwitchScrutineeValueKind::VoidValue => VOID_VALUE.direct(),
        };
        let scrutinee_values = b.pattern_matching_scrutinees.get_mut(&pattern_matching_ctx_id).expect("just inserted entry above");
        scrutinee_values.insert(scrutinee, value);

        value
    }

    fn handle_pattern_matching(&self, b: &mut FunctionBuilder, expr: ExprId, ctx: Context, tp: &dyn TypeProvider, og_scrutinee: ExprId, node: &SwitchDecisionNode, pattern_matching_ctx_id: PatternMatchingContextId, pattern_matching_ctx: &IndexVec<SwitchScrutineeValueId, TypedSwitchScrutineeValue>) {
        match *node {
            SwitchDecisionNode::Branch { scrutinee: scrutinee_id, ref paths, ref default_path } => {
                let begin_block = b.current_block;
                b.start_block(begin_block);

                let scrutinee_val = self.get_scrutinee_value(b, tp, og_scrutinee, scrutinee_id, pattern_matching_ctx, pattern_matching_ctx_id);
                // TODO: support pattern matching of pointers
                let scrutinee = self.handle_indirection(b, scrutinee_val);
                let scrutinee_ty = b.func.type_of_val(scrutinee).clone();

                let mut discriminant_enum_ty = None;
                let discriminant: ValueId = match scrutinee_ty {
                    Type::Enum(ref enum_ty) => {
                        discriminant_enum_ty = Some(enum_ty.clone());
                        self.get_discriminant(b, IndirectValue { value: scrutinee, indirection: 0 })
                    }
                    Type::Int { .. } => scrutinee,
                    ty => todo!("Pattern matching for {:?}", ty),
                };

                let mut mir_cases = Vec::new();
                let mut nodes = Vec::new();
                for (value, node) in paths {
                    let const_value = match *value {
                        SwitchDecisionValue::EnumVariant(variant_index) => self.get_const_discriminant(discriminant_enum_ty.clone().expect("Must be enum type").identity, variant_index),
                        SwitchDecisionValue::SignedInt(value) => Const::Int { lit: BigInt::from(value), ty: scrutinee_ty.clone() },
                        SwitchDecisionValue::UnsignedInt(value) => Const::Int { lit: BigInt::from(value), ty: scrutinee_ty.clone() },
                    };

                    let case_block = b.func.create_block();
                    mir_cases.push(SwitchCase { value: const_value, target: case_block.into() });
                    nodes.push(node);
                }

                let default_block = b.func.create_block();

                self.end_current_block(b, TerminatorKind::SwitchBr { scrutinee: discriminant, cases: mir_cases.clone(), catch_all_target: default_block.into() });

                for (mir_case, node) in mir_cases.iter().zip(nodes) {
                    b.start_block(mir_case.target.block);
                    self.handle_pattern_matching(b, expr, ctx, tp, og_scrutinee, node, pattern_matching_ctx_id, pattern_matching_ctx);
                }

                b.start_block(default_block);
                if let Some(default_path) = &default_path {
                    self.handle_pattern_matching(b, expr, ctx, tp, og_scrutinee, default_path, pattern_matching_ctx_id, pattern_matching_ctx);
                } else {
                    self.end_current_block(b, TerminatorKind::Call(CallKind::LegacyIntrinsic { arguments: SmallVec::new(), ty: Type::Never, intr: LegacyIntrinsic::Panic }));
                }
            },
            SwitchDecisionNode::Destination { destination, .. } => {
                self.build_scope(b, destination, ctx, tp);
            },
            SwitchDecisionNode::Failure => {
                self.diag.report_error_no_range_msg("pattern matching failure", expr);
                self.end_current_block(b, TerminatorKind::Call(CallKind::LegacyIntrinsic { arguments: SmallVec::new(), ty: Type::Never, intr: LegacyIntrinsic::Panic }));
            }
        }
    }
}
impl Driver {
    fn get_const_discriminant(&self, _enum_id: EnumId, variant_index: usize) -> Const {
        // TODO: other discriminant types and custom values
        // Delete TYPE_OF_DISCRIMINANTS to deal with other cases
        Const::Int { lit: BigInt::from(variant_index), ty: TYPE_OF_DISCRIMINANTS }
    }

    fn get_discriminant(&self, b: &mut FunctionBuilder, val: IndirectValue) -> ValueId {
        // TODO: handle indirect enum discriminant accesses, without loading the entire value.
        let val = self.handle_indirection(b, val);
        self.push_instr_with_name(b, InstrKind::DiscriminantAccess { val }, val, format!("{}.disc", self.display_value_name_from_builder(b, val)))
    }

    fn handle_indirection(&self, b: &mut FunctionBuilder, mut val: IndirectValue) -> ValueId {
        if val.indirection > 0 {
            while val.indirection > 0 {
                val.value = self.push_instr(b, InstrKind::Load(val.value), val.value);
                val.indirection -= 1;
            }
        } else if val.indirection < 0 {
            let mut ty = b.func.type_of_val(val.value).clone();
            while val.indirection < 0 {
                let location = self.push_instr(b, InstrKind::Alloca(ty.clone()), val.value);
                self.push_instr(b, InstrKind::Store { location, value: val.value }, val.value);
                val.value = location;
                val.indirection += 1;
                // Mutability doesn't matter for now
                ty = ty.mut_ptr();
            }
        }
        val.value
    }

    fn increment_variable(&self, b: &mut FunctionBuilder, location: ValueId) {
        let ty = b.func.type_of_val(location).deref().unwrap().clone().ty;
        let loaded = self.push_instr(b, InstrKind::Load(location), location);
        let one = self.push_instr(b, InstrKind::Const(Const::Int { lit: BigInt::from(1), ty: ty.clone() }), location);
        let value = self.push_call(b, CallKind::LegacyIntrinsic { arguments: smallvec![loaded, one], ty, intr: LegacyIntrinsic::Add }, location);
        self.push_instr(b, InstrKind::Store { location, value }, location);
    }

    fn handle_control(&self, b: &mut FunctionBuilder, val: IndirectValue, control: ControlDest) -> IndirectValue {
        match control {
            ControlDest::Jump(target) => self.end_current_block(b, TerminatorKind::Jump(target.into())),
            ControlDest::IncrementVariableAndThenJump { location, target } => {
                // *location += 1
                self.increment_variable(b, location);

                // jump block
                self.end_current_block(b, TerminatorKind::Jump(target.into()))
            },
            ControlDest::Continue => val,
            ControlDest::RetVoid => self.end_current_block(b, TerminatorKind::Ret(VOID_VALUE)),
            ControlDest::Unreachable => VOID_VALUE.direct(),
        }
    }
}

impl Driver {
    fn handle_context(&self, b: &mut FunctionBuilder, mut val: IndirectValue, ctx: Context) -> IndirectValue {
        val = val.adjusted(ctx.indirection);
        match ctx.data {
            DataDest::Read => return val,
            DataDest::Ret => {
                let value = self.handle_indirection(b, val);
                return self.end_current_block(b, TerminatorKind::Ret(value));
            },
            DataDest::JumpWithArgument(block) => {
                let instr = self.handle_indirection(b, val);
                return self.end_current_block(b, TerminatorKind::Jump(JumpTarget { block, arguments: smallvec![instr] }));
            },
            DataDest::Branch(true_block, false_block) => {
                let condition = self.handle_indirection(b, val);
                let terminator = match b.func.values[condition].kind {
                    ValueKind::InstrResult(condition_instr) => {
                        if let &InstrKind::Const(Const::Bool(val)) = &b.func.instrs[condition_instr].kind {
                            let block = if val {
                                true_block
                            } else {
                                false_block
                            };
                            TerminatorKind::Jump(block.into())
                        } else {
                            TerminatorKind::CondBr { condition, true_target: true_block.into(), false_target: false_block.into() }
                        }
                    },
                    ValueKind::Parameter { .. } => TerminatorKind::CondBr { condition, true_target: true_block.into(), false_target: false_block.into() },
                    ValueKind::Void => {
                        panic!("internal compiler error: branching on void")
                    },
                    ValueKind::Never => return NEVER_VALUE.direct(),
                };

                return self.end_current_block(b, terminator);
            },
            DataDest::Receive { value } => {
                let location = self.handle_indirection(b, val.get_address());
                let range = self.get_range_with_mir_ctx(location, Some(&b.func)) + self.get_range_with_mir_ctx(value, Some(&b.func));
                self.push_instr(b, InstrKind::Store { location, value }, range);
            },
            DataDest::Void => {},
        }

        self.handle_control(b, val, ctx.control)
    }
}

#[derive(Default)]
struct LayoutCache {
    struct_layouts: HashMap<StructType, StructLayout>,
    // TODO: we used to have a fast path for non-generic structs and should add it back eventually
}

thread_local! {
    static LAYOUT_CACHE: RefCell<LayoutCache> = RefCell::new(LayoutCache::default());
}
