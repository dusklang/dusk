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
use crate::index_vec::{IndexVec, define_index_type};
use crate::display_adapter;

use crate::mir::cursor::{Cursor, CursorMut};
use crate::pattern_matching::{SwitchDecisionNode, SwitchDecisionValue, SwitchScrutineeValueId, TypedSwitchScrutineeValue, TypedSwitchScrutineeValueKind};
use crate::source_info::SourceRange;

use crate::internal_types::InternalField;
use crate::ast::{self, DeclId, DeclRefId, EnumId, Expr, ExprId, ExternFunctionRef, ExternModId, GenericParamId, ImperScopeId, IntrinsicId, Item, LegacyIntrinsic, LoopId, NewNamespaceId, PatternMatchingContextId, ScopedItem, StoredDeclId, StructId, VOID_TYPE};
use crate::ty::{EnumType, FloatWidth, FunctionType, LegacyInternalType, StructType, Type};
use crate::driver::{Driver, DriverRwRef};
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
// TODO: make instructions a linked list just like blocks are, instead of each block individually owning a Vec of InstrIds. Then we can support inserting/deleting instructions by cursor as well.
// TODO: decouple values (the outputs of instructions) from the instructions themselves. That way each instruction can in principle output no value, or even multiple values.
// TODO: add back VOID_INSTR (or VOID_VALUE) at a known, static index, so we can return that again instead of creating one. That, or allow functions like build_expr to return an Option<Value> or
// even some "no value" state.

define_index_type!(pub struct FuncId = u32;);
define_index_type!(pub struct StaticId = u32;);
define_index_type!(pub struct StrId = u32;);
define_index_type!(pub struct InstrId = u32;);
define_index_type!(pub struct BlockId = u32;);

#[derive(Clone, Debug)]
pub struct Instr {
    pub kind: InstrKind,
    pub ty: Type,
}

impl Instr {
    pub fn new(kind: InstrKind, ty: Type) -> Self {
        Self {
            kind,
            ty,
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct Block {
    pub instrs: Vec<InstrId>,
    pub prev: Option<BlockId>,
    pub next: Option<BlockId>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct JumpTarget {
    pub bb: BlockId,
    pub arguments: SmallVec<[InstrId; 2]>,
}

impl From<BlockId> for JumpTarget {
    fn from(value: BlockId) -> Self {
        JumpTarget { bb: value, arguments: Default::default() }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct SwitchCase {
    pub value: Const,
    pub target: JumpTarget,
}

#[derive(Clone, Debug, PartialEq)]
pub enum InstrKind {
    Void,
    Invalid, // Used temporarily when copying functions
    Const(Const),
    Alloca(Type),
    LogicalNot(InstrId),
    Call { arguments: SmallVec<[InstrId; 2]>, generic_arguments: Vec<Type>, func: FuncId },
    ExternCall { arguments: SmallVec<[InstrId; 2]>, func: ExternFunctionRef },
    ObjcClassRef { extern_mod: ExternModId, index: usize },
    FunctionRef { generic_arguments: Vec<Type>, func: FuncId, },
    LegacyIntrinsic { arguments: SmallVec<[InstrId; 2]>, ty: Type, intr: LegacyIntrinsic },
    Intrinsic { arguments: SmallVec<[InstrId; 2]>, intr: IntrinsicId },
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
    Pointer { instr: InstrId, is_mut: bool },
    Struct { fields: SmallVec<[InstrId; 2]>, id: StructId },
    Enum { variants: SmallVec<[InstrId; 2]>, id: EnumId },
    FunctionTy { param_tys: Vec<InstrId>, has_c_variadic_param: bool, ret_ty: InstrId },
    StructLit { fields: SmallVec<[InstrId; 2]>, id: StructId },
    DirectFieldAccess { val: InstrId, index: usize },
    IndirectFieldAccess { val: InstrId, index: usize },
    InternalFieldAccess { val: InstrId, field: InternalField },
    Variant { enuum: EnumId, index: usize, payload: InstrId },
    DiscriminantAccess { val: InstrId },
    PayloadAccess { val: InstrId, variant_index: usize },
    Ret(InstrId),
    Jump(JumpTarget),
    CondBr { condition: InstrId, true_target: JumpTarget, false_target: JumpTarget },
    SwitchBr { scrutinee: InstrId, cases: Vec<SwitchCase>, catch_all_target: JumpTarget },
    /// This variant currently exists (rather than just stuffing a constant `Type` into `InstrKind::Const()`) so that we
    /// know to lookup the generic param instead of preserving it as-is.
    GenericParam(GenericParamId),
    /// Only valid at the beginning of a function, right after the void instruction
    // TODO: Get rid of the type here! It is no longer required because instruction types are now stored on each Instr
    Parameter(Type),
}

impl InstrKind {
    pub fn replace_bb(&mut self, old: BlockId, new: BlockId) {
        fn replace(target: &mut JumpTarget, old: BlockId, new: BlockId) {
            // TODO: handle basic block arguments (currently not needed by any callers of this method)
            assert_eq!(target.arguments.len(), 0);
            if target.bb == old {
                target.bb = new;
            }
        }
        match self {
            InstrKind::Jump(target) => replace(target, old, new),
            InstrKind::CondBr { true_target, false_target, .. } => {
                replace(true_target, old, new);
                replace(false_target, old, new);
            },
            InstrKind::SwitchBr { cases, catch_all_target, .. } => {
                for case in cases {
                    replace(&mut case.target, old, new);
                }
                replace(catch_all_target, old, new);
            },
            _ => {}
        }
    }

    // TODO: allocating a Vec here sucks!
    pub fn referenced_values(&self) -> Vec<InstrId> {
        match *self {
            InstrKind::Void | InstrKind::Const(_) | InstrKind::Alloca(_) | InstrKind::AddressOfStatic(_)
                | InstrKind::GenericParam(_) | InstrKind::Parameter(_) | InstrKind::FunctionRef { .. } | InstrKind::Invalid | InstrKind::ObjcClassRef { .. } => vec![],
            InstrKind::LogicalNot(instr) | InstrKind::Reinterpret(instr, _) | InstrKind::Truncate(instr, _) | InstrKind::SignExtend(instr, _)
                | InstrKind::ZeroExtend(instr, _) | InstrKind::FloatCast(instr, _) | InstrKind::FloatToInt(instr, _)
                | InstrKind::IntToFloat(instr, _) | InstrKind::Load(instr) | InstrKind::Pointer { instr, .. }
                | InstrKind::DirectFieldAccess { val: instr, .. } | InstrKind::IndirectFieldAccess { val: instr, .. }
                | InstrKind::DiscriminantAccess { val: instr } | InstrKind::Ret(instr) | InstrKind::Variant { payload: instr, .. } | InstrKind::PayloadAccess { val: instr, .. }
                | InstrKind::InternalFieldAccess { val: instr, .. } => vec![instr],
            InstrKind::Store { location, value } => vec![location, value],
            InstrKind::Call { arguments: ref instrs, .. } | InstrKind::ExternCall { arguments: ref instrs, .. }
                | InstrKind::LegacyIntrinsic { arguments: ref instrs, .. } | InstrKind::Struct { fields: ref instrs, .. }
                | InstrKind::Enum { variants: ref instrs, .. } | InstrKind::StructLit { fields: ref instrs, .. }
                | InstrKind::Intrinsic { arguments: ref instrs, .. } => instrs.iter().copied().collect(),
            InstrKind::FunctionTy { ref param_tys, ret_ty, .. } => param_tys.iter().copied().chain(std::iter::once(ret_ty)).collect(),
            InstrKind::Jump(ref target) => target.arguments.iter().copied().collect(),
            InstrKind::CondBr { condition: instr, ref true_target, ref false_target } => std::iter::once(instr)
                .chain(true_target.arguments.iter().copied())
                .chain(false_target.arguments.iter().copied())
                .collect(),
            InstrKind::SwitchBr { scrutinee: instr, ref cases, ref catch_all_target } => std::iter::once(instr)
                .chain(cases.iter().flat_map(|case| case.target.arguments.iter().copied()))
                .chain(catch_all_target.arguments.iter().copied())
                .collect()
        }
    }

    pub fn references_value(&self, val: InstrId) -> bool {
        self.referenced_values().contains(&val)
    }

    pub fn replace_value(&mut self, old: InstrId, new: InstrId) {
        fn replace(target: &mut InstrId, old: InstrId, new: InstrId) {
            if *target == old {
                *target = new;
            }
        }
        fn replace_args(target: &mut JumpTarget, old: InstrId, new: InstrId) {
            for arg in &mut target.arguments {
                replace(arg, old, new);
            }
        }
        match self {
            InstrKind::Void | InstrKind::Const(_) | InstrKind::Alloca(_) | InstrKind::AddressOfStatic(_)
                | InstrKind::GenericParam(_) | InstrKind::Parameter(_) | InstrKind::FunctionRef { .. } | InstrKind::Invalid | InstrKind::ObjcClassRef { .. } => {},
            InstrKind::LogicalNot(instr) | InstrKind::Reinterpret(instr, _) | InstrKind::Truncate(instr, _) | InstrKind::SignExtend(instr, _)
                | InstrKind::ZeroExtend(instr, _) | InstrKind::FloatCast(instr, _) | InstrKind::FloatToInt(instr, _)
                | InstrKind::IntToFloat(instr, _) | InstrKind::Load(instr) | InstrKind::Pointer { instr, .. }
                | InstrKind::DirectFieldAccess { val: instr, .. } | InstrKind::IndirectFieldAccess { val: instr, .. }
                | InstrKind::DiscriminantAccess { val: instr } | InstrKind::Ret(instr) | InstrKind::Variant { payload: instr, .. } | InstrKind::PayloadAccess { val: instr, .. }
                | InstrKind::InternalFieldAccess { val: instr, .. } => replace(instr, old, new),
            InstrKind::Store { location, value } => {
                replace(location, old, new);
                replace(value, old, new);
            },
            InstrKind::Call { arguments: instrs, .. } | InstrKind::ExternCall { arguments: instrs, .. }
                | InstrKind::LegacyIntrinsic { arguments: instrs, .. } | InstrKind::Struct { fields: instrs, .. }
                | InstrKind::Enum { variants: instrs, .. } | InstrKind::StructLit { fields: instrs, .. }
                | InstrKind::Intrinsic { arguments: instrs, .. } => {
                    for instr in instrs {
                        replace(instr, old, new);
                    }
                }
            InstrKind::FunctionTy { param_tys, ret_ty, .. } => {
                for instr in param_tys {
                    replace(instr, old, new);
                }
                replace(ret_ty, old, new);
            },
            InstrKind::Jump(target) => replace_args(target, old, new),
            InstrKind::CondBr { condition: instr, true_target, false_target } => {
                replace(instr, old, new);
                replace_args(true_target, old, new);
                replace_args(false_target, old, new);
            },
            InstrKind::SwitchBr { scrutinee: instr, cases, catch_all_target } => {
                replace(instr, old, new);
                for case in cases {
                    replace_args(&mut case.target, old, new);
                }
                replace_args(catch_all_target, old, new);
            },
        }
    }
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
pub struct InstrNamespace {
    name_usages: HashMap<String, u16>,
}

impl InstrNamespace {
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
pub struct Function {
    pub name: Option<Sym>,
    pub ty: FunctionType,
    pub blocks: IndexVec<BlockId, Block>,
    pub first_block: BlockId,
    pub last_block: BlockId,
    pub entry_block: BlockId,
    pub instrs: IndexVec<InstrId, Instr>,
    pub source_ranges: HashMap<InstrId, SourceRange>,
    pub instr_names: HashMap<InstrId, String>,
    // The set of instructions that failed to be const-eval'ed (e.g., due to a panic)
    pub poisoned_instrs: HashSet<InstrId>,
    pub decl: Option<DeclId>,
    pub generic_params: Range<GenericParamId>,
    pub instr_namespace: InstrNamespace,
    pub is_comptime: bool,
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
            source_ranges: Default::default(),
            instr_names: Default::default(),
            poisoned_instrs: Default::default(),
            decl: Default::default(),
            generic_params: empty_range(),
            instr_namespace: Default::default(),
            is_comptime: Default::default(),
        }
    }
}

impl Function {
    pub fn parameter_tys(&self) -> impl Iterator<Item=&Type> {
        self.ty.param_tys.iter()
    }

    pub fn num_parameters(&self) -> usize {
        self.parameter_tys().count()
    }

    pub fn type_of(&self, instr: InstrId) -> &Type {
        &self.instrs[instr].ty
    }
}

impl FunctionBuilder {
    #[allow(unused)]
    pub fn parameter_tys(&self) -> impl Iterator<Item=&Type> {
        let block = &self.blocks[self.entry_block];
        block.instrs.iter()
            .filter_map(|&instr| {
                match &self.instrs[instr].kind {
                    InstrKind::Parameter(ty) => Some(ty),
                    _ => None,
                }
            })
    }

    #[allow(unused)]
    pub fn num_parameters(&self) -> usize {
        self.parameter_tys().count()
    }

    pub fn type_of(&self, instr: InstrId) -> &Type {
        &self.instrs[instr].ty
    }
}

impl Driver {
    #[display_adapter]
    pub fn display_block(&self, b: &FunctionBuilder, block: BlockId, w: &mut Formatter) {
        let block = &b.blocks[block];
        for &id in &block.instrs {
            writeln!(w, "    %instr{} = mir.{:?}", id.index(), b.instrs[id].kind)?;
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

pub struct Mir {
    pub strings: ConcurrentIndexVec<StrId, CString>,
    pub functions: ConcurrentIndexVec<FuncId, OnceLock<Function>>,
    pub function_sigs: papaya::HashMap<FuncId, FunctionSignature>,
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

impl FunctionBuilder {
    fn get_block_state(&mut self, block: BlockId) -> &mut BlockState {
        self.block_states.entry(block).or_insert(BlockState::Created)
    }

    pub fn start_block(&mut self, block: BlockId) -> Result<(), StartBlockError> {
        let state = self.get_block_state(block);
        match state {
            BlockState::Created => {
                *state = BlockState::Started;
                Ok(())
            }
            BlockState::Started => Ok(()),
            BlockState::Ended => Err(StartBlockError::BlockEnded),
        }
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
        for block_id in self.make_cursor().block_ids_iter() {
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
}

impl Mir {
    pub fn new() -> Self {
        Mir {
            strings: Default::default(),
            functions: Default::default(),
            function_sigs: Default::default(),
            statics: Default::default(),
            extern_mods: Default::default(),
            enums: Default::default(),
            decls: Default::default(),
        }
    }
}

impl Default for Mir {
    fn default() -> Self { Self::new() }
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
    Receive { value: InstrId },
    /// Jump to a particular basic block argument, with this value as an argument
    JumpWithArgument(BlockId),
    /// This value just needs to be read
    Read,
    /// This value will never be used
    Void,
    /// If this value is true, jump to the first target, otherwise jump to the second
    Branch(BlockId, BlockId),
}

#[derive(Debug, Copy, Clone)]
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

trait Indirection {
    fn direct(self) -> Value;
    fn indirect(self) -> Value;
}

impl Indirection for InstrId {
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
#[derive(Clone, Copy, Debug)]
enum ControlDest {
    Continue,
    Unreachable,
    Jump(BlockId),
    IncrementVariableAndThenJump {
        location: InstrId,
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
            &Type::Internal(id) => self.ast.internal_types[id].size,
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

impl DriverRwRef<'_> {
    pub fn build_mir(&self, tp: &dyn TypeProvider) {
        // Start at 1 to avoid RETURN_VALUE_DECL, which we can't and shouldn't generate code for
        let range = DeclId::new(1)..self.read().ast.decls.next_idx();
        for id in range_iter(range) {
            self.get_decl(id, tp);
        }
    }
}

impl DriverRwRef<'_> {
    pub fn build_standalone_expr(&self, expr: ExprId, tp: &dyn TypeProvider) -> Function {
        let func_ty = FunctionType { param_tys: Vec::new(), return_ty: Box::new(tp.ty(expr).clone()), has_c_variadic_param: false };
        self.build_function(None, func_ty, FunctionBody::Expr(expr), empty_range(), empty_range(), false, tp)
    }
}

impl Driver {
    fn resolve_extern_mod(&self, id: ExternModId, tp: &dyn TypeProvider) {
        let extern_mods = self.mir.extern_mods.pin();
        if extern_mods.contains_key(&id) { return; }

        let extern_mod = &self.ast.extern_mods[id];
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

impl DriverRwRef<'_> {
    fn get_decl(&self, id: DeclId, tp: &dyn TypeProvider) -> Decl {
        let d = self.read();
        if let Some(decl) = d.mir.decls.pin().get(&id) { return decl.clone(); }
        match df!(d, id.ast) {
            ast::Decl::Function { ref params, scope, ref generic_params, .. } => {
                // Add placeholder function to reserve ID ahead of time
                let params = params.clone();
                let generic_params = generic_params.clone();
                let get = d.mir.functions.push(OnceLock::new());
                let decl = Decl::Function { get };
                d.mir.decls.pin().insert(id, decl.clone());

                let func_ty = d.decl_type(id, tp).as_function().unwrap().clone();
                let name = d.ast.names[id];
                let comptime_sym = d.ast.known_idents.comptime;
                let is_comptime = d.ast.decl_attributes.get(&id)
                .map(|attrs|
                    attrs.iter()
                    .any(|attr| attr.attr == comptime_sym)
                ).unwrap_or(false);
                let signature = FunctionSignature {
                    name,
                    ty: func_ty.clone(),
                    generic_params: generic_params.clone(),
                    is_comptime,
                };
                d.mir.function_sigs.pin().insert(get, signature);
                drop(d);
                let func = self.build_function(
                    Some(name),
                    func_ty,
                    FunctionBody::Scope { scope, decl: id },
                    params,
                    generic_params,
                    is_comptime,
                    tp,
                );
                self.read().mir.functions[get].set(func).expect("function value set multiple times");
                decl
            },
            ast::Decl::FunctionPrototype { extern_func, .. } => {
                let decl = if let Some(extern_func) = extern_func {
                    d.resolve_extern_mod(extern_func.extern_mod, tp);
                    Decl::ExternFunction(extern_func)
                } else {
                    let err = Error::new("cannot declare prototype outside of extern module")
                        .adding_primary_range(id, "prototype here");
                    d.diag.push(err);
                    Decl::Invalid
                };
                d.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::ObjcClassRef { extern_mod, index } => {
                d.resolve_extern_mod(extern_mod, tp);
                let decl = Decl::ObjcClassRef { extern_mod, index };
                d.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::Stored { id: index, .. } | ast::Decl::LoopBinding { id: index, .. } => {
                let decl = Decl::Stored(index);
                d.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::Parameter { index } => {
                let decl = Decl::Parameter { index };
                d.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::PatternBinding { context, scrutinee, root_scrutinee, .. } => {
                let decl = Decl::PatternBinding { context, scrutinee, root_scrutinee };
                d.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::LegacyIntrinsic { intr, function_like, .. } => {
                let mut ty = d.decl_type(id, tp);
                if function_like {
                    ty = ty.return_ty().unwrap().clone();
                }
                let decl = Decl::LegacyIntrinsic(intr, ty);
                d.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::Intrinsic(intr) => {
                let decl = Decl::Intrinsic(intr);
                d.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::MethodIntrinsic(intr) => {
                let decl = Decl::MethodIntrinsic(intr);
                d.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::Static(expr) => {
                let name = d.display_item(&Default::default(), id).to_string();
                drop(d);
                let konst = self.eval_expr(expr, tp);
                let d = self.read();
                let statik = d.mir.statics.push(
                    Static {
                        name,
                        val: konst.into(),
                    }
                );
                let decl = Decl::Static(statik);
                d.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::Const { assigned_expr: root_expr, .. } => {
                drop(d);
                let konst = self.eval_expr(root_expr, tp);

                let d = self.read();

                // TODO: Deal with cycles!
                let decl = Decl::Const(konst.into());
                d.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::Field { index, .. } => {
                let decl = Decl::Field { index };
                d.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::InternalField(field) => {
                let decl = Decl::InternalField(field);
                d.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::Variant { enuum, index, payload_ty } => {
                let payload_ty = payload_ty.map(|ty| tp.get_evaluated_type(ty).clone());
                Decl::Variant { enuum, index, payload_ty }
            },
            ast::Decl::GenericParam(param) => {
                let decl = Decl::GenericParam(param);
                d.mir.decls.pin().insert(id, decl.clone());
                decl
            },
            ast::Decl::ReturnValue => panic!("Can't get_decl() the return_value decl"),
        }
    }
}

impl Driver {
    #[allow(dead_code)]
    #[display_adapter]
    fn fmt_variant_name(&self, f: &mut Formatter, enuum: EnumId, index: usize) {
        let variant = &self.ast.enums[enuum].variants[index];
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
    pub fn display_item(&'a self, ctx: &HashMap<InstrId, SourceRange>, item: impl Into<ToSourceRange> + Copy + 'a, f: &mut Formatter) {
        let range = self.get_range_with_mir_ctx(item, ctx);
        if range.is_empty() {
            let item = item.into();
            match item {
                ToSourceRange::Item(item) => match item {
                    Item::Decl(decl) => write!(f, "{:?}", df!(decl.ast)),
                    Item::Expr(expr) => write!(f, "{:?}", ef!(expr.ast)),
                },
                _ => write!(f, "{}", self.substring_from_range(range))
            }
        } else {
            write!(f, "{}", self.substring_from_range(range))
        }
    }

    #[display_adapter]
    pub fn display_instr_name(&self, func: &Function, item: InstrId, f: &mut Formatter) {
        write!(f, "{}", func.instr_names.get(&item).cloned()
            .unwrap_or_else(|| format!("instr{}", item.index())))
    }

    #[display_adapter]
    pub fn display_instr_name_from_builder(&self, b: &FunctionBuilder, item: InstrId, f: &mut Formatter) {
        write!(f, "{}", b.instr_names.get(&item).cloned()
            .unwrap_or_else(|| format!("instr{}", item.index())))
    }

    #[display_adapter]
    pub fn display_branch_target(&self, func: &Function, target: &JumpTarget, f: &mut Formatter) {
        write!(f, "%bb{}", target.bb.index())?;
        if !target.arguments.is_empty() {
            write!(f, "(")?;
            let mut first = true;
            for &arg in &target.arguments {
                if first {
                    first = false;
                } else {
                    write!(f, ", ")?;
                }
                write!(f, "%{}", self.display_instr_name(func, arg))?;
            }
            write!(f, ")")?;
        }
        Ok(())
    }

    #[display_adapter]
    pub fn display_mir_instr(&self, func: &Function, instr_id: InstrId, f: &mut Formatter) {
        let instr = &func.instrs[instr_id].kind;
        macro_rules! write_args {
            ($args:expr) => {{
                write!(f, "(")?;
                let mut first = true;
                for &arg in $args {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "%{}", self.display_instr_name(func, arg))?;
                }
                write!(f, ")")?;
            }}
        }
        macro_rules! write_generic_args {
            ($args:expr) => {{
                if !$args.is_empty() {
                    write!(f, "<|")?;
                    let mut first = true;
                    for arg in $args {
                        if first {
                            first = false;
                        } else {
                            write!(f, ", ")?;
                        }
                        write!(f, "{:?}", arg)?;
                    }
                    write!(f, "|>")?;
                }
            }}
        }
        match instr {
            InstrKind::Alloca(ty) => write!(f, "%{} = alloca {:?}", self.display_instr_name(func, instr_id), ty)?,
            InstrKind::Jump(block) => write!(f, "jump {}", self.display_branch_target(func, block))?,
            &InstrKind::CondBr { condition, ref true_target, ref false_target }
                => write!(f, "condbr %{}, {}, {}", self.display_instr_name(func, condition), self.display_branch_target(func, true_target), self.display_branch_target(func, false_target))?,
            &InstrKind::SwitchBr { scrutinee, ref cases, ref catch_all_target } => {
                write!(f, "switchbr %{} : ", self.display_instr_name(func, scrutinee))?;
                for case in cases {
                    write!(f, "case {} => {}, ", self.display_const(&case.value), self.display_branch_target(func, &case.target))?;
                }
                write!(f, "else => {}", self.display_branch_target(func, catch_all_target))?;
            }
            &InstrKind::Call { ref arguments, func: callee, ref generic_arguments } => {
                let name_sym = self.mir.function_sigs.pin().get(&callee).unwrap().name;
                write!(f, "%{} = call `{}`", self.display_instr_name(func, instr_id), self.fn_name(Some(name_sym)))?;
                write_generic_args!(generic_arguments);
                write_args!(arguments);
            },
            &InstrKind::FunctionRef { func: callee, ref generic_arguments } => {
                let name_sym = self.mir.function_sigs.pin().get(&callee).unwrap().name;
                write!(f, "%{} = function_ref `{}`", self.display_instr_name(func, instr_id), self.fn_name(Some(name_sym)))?;
                write_generic_args!(generic_arguments);
            },
            &InstrKind::ExternCall { ref arguments, func: callee, .. } => {
                let extern_mods = self.mir.extern_mods.pin();
                let extern_mod = extern_mods.get(&callee.extern_mod).unwrap();
                let callee_func = &extern_mod.imported_functions[callee.index];
                write!(f, "%{} = externcall `{}`", self.display_instr_name(func, instr_id), callee_func.name)?;
                write_args!(arguments);
                write!(f, " from {:?}", extern_mod.library_path)?
            },
            &InstrKind::ObjcClassRef { extern_mod, index } => write!(
                f,
                "%{} = objc_class_ref `{}` from {:?}",
                self.display_instr_name(func, instr_id),
                self.ast.extern_mods[extern_mod].objc_class_references[index],
                self.mir.extern_mods.pin().get(&extern_mod).unwrap().library_path
            )?,
            InstrKind::Const(konst) => {
                write!(f, "%{} = {}", self.display_instr_name(func, instr_id), self.display_const(konst))?;
            },
            InstrKind::LegacyIntrinsic { arguments, intr, .. } => {
                write!(f, "%{} = intrinsic `{}`", self.display_instr_name(func, instr_id), intr.name())?;
                write_args!(arguments);
            },
            InstrKind::Intrinsic { arguments, intr, .. } => {
                write!(f, "%{} = new_style_intrinsic `{}`", self.display_instr_name(func, instr_id), self.ast.intrinsics[*intr].name)?;
                write_args!(arguments);
            },
            &InstrKind::Pointer { instr, is_mut } => {
                write!(f, "%{} = %{} *", self.display_instr_name(func, instr_id), self.display_instr_name(func, instr))?;
                if is_mut {
                    write!(f, "mut")?
                }
            },
            &InstrKind::Load(location) => write!(f, "%{} = load %{}", self.display_instr_name(func, instr_id), self.display_instr_name(func, location))?,
            &InstrKind::LogicalNot(instr) => write!(f, "%{} = not %{}", self.display_instr_name(func, instr_id), self.display_instr_name(func, instr))?,
            &InstrKind::Ret(val) => write!(f,  "return %{}", self.display_instr_name(func, val))?,
            &InstrKind::Store { location, value } => write!(f, "store %{} in %{}", self.display_instr_name(func, value), self.display_instr_name(func, location))?,
            &InstrKind::AddressOfStatic(statik) => write!(f, "%{} = address of static %{}", self.display_instr_name(func, instr_id), self.mir.statics[statik].name)?,
            &InstrKind::Reinterpret(val, ref ty) => write!(f, "%{} = reinterpret %{} as {:?}", self.display_instr_name(func, instr_id), self.display_instr_name(func, val), ty)?,
            &InstrKind::SignExtend(val, ref ty) => write!(f, "%{} = sign-extend %{} as {:?}", self.display_instr_name(func, instr_id), self.display_instr_name(func, val), ty)?,
            &InstrKind::ZeroExtend(val, ref ty) => write!(f, "%{} = zero-extend %{} as {:?}", self.display_instr_name(func, instr_id), self.display_instr_name(func, val), ty)?,
            &InstrKind::Truncate(val, ref ty) => write!(f, "%{} = truncate %{} as {:?}", self.display_instr_name(func, instr_id), self.display_instr_name(func, val), ty)?,
            &InstrKind::FloatCast(val, ref ty) => write!(f, "%{} = floatcast %{} as {:?}", self.display_instr_name(func, instr_id), self.display_instr_name(func, val), ty)?,
            &InstrKind::IntToFloat(val, ref ty) => write!(f, "%{} = inttofloat %{} as {:?}", self.display_instr_name(func, instr_id), self.display_instr_name(func, val), ty)?,
            &InstrKind::FloatToInt(val, ref ty) => write!(f, "%{} = floattoint %{} as {:?}", self.display_instr_name(func, instr_id), self.display_instr_name(func, val), ty)?,
            &InstrKind::Struct { ref fields, id } => {
                write!(f, "%{} = define struct{} {{ ", self.display_instr_name(func, instr_id), id.index())?;
                for i in 0..fields.len() {
                    write!(f, "%{}", self.display_instr_name(func, fields[i]))?;
                    if i < (fields.len() - 1) {
                        write!(f, ",")?;
                    }
                    write!(f, " ")?;
                }
                write!(f, "}}")?;
            },
            &InstrKind::StructLit { ref fields, id } => {
                write!(f, "%{} = literal struct{} {{ ", self.display_instr_name(func, instr_id), id.index())?;
                for i in 0..fields.len() {
                    write!(f, "%{}", self.display_instr_name(func, fields[i]))?;
                    if i < (fields.len() - 1) {
                        write!(f, ",")?;
                    }
                    write!(f, " ")?;
                }
                write!(f, "}}")?;
            },
            &InstrKind::Enum { ref variants, id } => {
                write!(f, "%{} = define enum{} {{", self.display_instr_name(func, instr_id), id.index())?;

                for (i, variant) in self.ast.enums[id].variants.iter().enumerate() {
                    write!(f, "{}", self.interner.read().unwrap().resolve(variant.name).unwrap())?;
                    if variant.payload_ty.is_some() {
                        write!(f, "(%{})", self.display_instr_name(func, variants[i]))?;
                    }
                    if i < (variants.len() - 1) {
                        write!(f, ",")?;
                    }
                    write!(f, " ")?;
                }

                write!(f, "}}")?;
            },
            &InstrKind::FunctionTy { ref param_tys, has_c_variadic_param, ret_ty } => {
                write!(f, "%{} = fn type (", self.display_instr_name(func, instr_id))?;
                for (i, &param) in param_tys.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "%{}", self.display_instr_name(func, param))?;
                }
                if has_c_variadic_param {
                    if !param_tys.is_empty() {
                        write!(f, ", ")?;
                    }
                    write!(f, "...")?;
                }
                write!(f, " -> {}", self.display_instr_name(func, ret_ty))?;
            },
            &InstrKind::Variant { enuum, index, payload } => {
                let variant = &self.ast.enums[enuum].variants[index];
                let variant_name = variant.name;
                write!(f, "%{} = %enum{}.{}", self.display_instr_name(func, instr_id), enuum.index(), self.interner.read().unwrap().resolve(variant_name).unwrap())?;
                if variant.payload_ty.is_some() {
                    write!(f, "(%{})", self.display_instr_name(func, payload))?
                }
            },
            &InstrKind::DirectFieldAccess { val, index } => write!(f, "%{} = %{}.field{}", self.display_instr_name(func, instr_id), self.display_instr_name(func, val), index)?,
            &InstrKind::IndirectFieldAccess { val, index } => write!(f, "%{} = &(*%{}).field{}", self.display_instr_name(func, instr_id), self.display_instr_name(func, val), index)?,
            &InstrKind::InternalFieldAccess { val, field } => write!(f, "%{} = %{}.{}", self.display_instr_name(func, instr_id), self.display_instr_name(func, val), field.name())?,
            &InstrKind::DiscriminantAccess { val } => write!(f, "%{} = discriminant of %{}", self.display_instr_name(func, instr_id), self.display_instr_name(func, val))?,
            &InstrKind::PayloadAccess { val, variant_index } => write!(f, "%{} = payload of %{} using variant {}", self.display_instr_name(func, instr_id), self.display_instr_name(func, val), variant_index)?,
            // TODO: instead of emitting these instructions as needed, add all generic params to the beginning of the MIR function as "hidden" values, just like normal parameters.
            &InstrKind::GenericParam(param) => {
                write!(f, "%{} = generic_param{}", self.display_instr_name(func, instr_id), param.index())?
            },
            InstrKind::Parameter(_) => {},
            InstrKind::Invalid => write!(f, "%{} = invalid!", self.display_instr_name(func, instr_id))?,
            InstrKind::Void => write!(f, "%{} = void", self.display_instr_name(func, instr_id))?,
        };
        Ok(())
    }

    #[display_adapter]
    pub fn display_mir_block(&self, func: &Function, id: BlockId, f: &mut Formatter) {
        let block = &func.blocks[id];
        write!(f, "%bb{}", id.index())?;
        if id != func.entry_block && matches!(block.instrs.first().map(|&instr| &func.instrs[instr].kind), Some(InstrKind::Parameter(_))) {
            write!(f, "(")?;
            let mut first = true;
            for &instr in &block.instrs {
                if let InstrKind::Parameter(ty) = &func.instrs[instr].kind {
                    if first {
                        first = false;
                    } else {
                        write!(f, ", ")?;
                    }
                    write!(f, "%{}: {:?}", self.display_instr_name(func, instr), ty)?;
                } else {
                    break;
                }
            }
            write!(f, ")")?;
        }
        writeln!(f, ":")?;
        let mut start = 0;
        for (i, &instr) in block.instrs.iter().enumerate() {
            let instr = &func.instrs[instr].kind;
            if !matches!(instr, InstrKind::Parameter(_)) {
                start = i;
                break;
            }
        }

        for &instr_id in &block.instrs[start..] {
            writeln!(f, "    {}", self.display_mir_instr(func, instr_id))?;
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
        write!(f, "(")?;
        let entry_block = &func.blocks[func.entry_block];
        let mut first = true;
        for &instr in &entry_block.instrs {
            if let InstrKind::Parameter(ty) = &func.instrs[instr].kind {
                if first {
                    first = false;
                } else {
                    write!(f, ", ")?;
                }
                write!(f, "%{}: {:?}", self.display_instr_name(func, instr), ty)?;
            } else {
                break;
            }
        }
        writeln!(f, "): {:?} {{", func.ty.return_ty.as_ref())?;
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
    continue_location_of_variable_to_increment: Option<InstrId>,
}

struct FunctionBuilder {
    name: Option<Sym>,
    ty: FunctionType,
    blocks: IndexVec<BlockId, Block>,
    first_block: BlockId,
    last_block: BlockId,
    entry_block: BlockId,
    instrs: IndexVec<InstrId, Instr>,
    source_ranges: HashMap<InstrId, SourceRange>,
    instr_names: HashMap<InstrId, String>,
    block_states: HashMap<BlockId, BlockState>,
    current_block: BlockId,
    stored_decl_locs: IndexVec<StoredDeclId, InstrId>,
    instr_namespace: InstrNamespace,
    loops: IndexVec<LoopId, LoopState>,
    pattern_matching_scrutinees: HashMap<PatternMatchingContextId, HashMap<SwitchScrutineeValueId, Value>>,
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
    Value(Value),
}

impl Driver {
    fn create_bb(&self, b: &mut FunctionBuilder) -> BlockId {
        let prev = b.last_block;

        let block = Block {
            prev: Some(prev),
            ..Default::default()
        };
        let bb = b.blocks.push(block);
        assert_eq!(b.blocks[prev].next, None);
        b.blocks[prev].next = Some(bb);
        b.last_block = bb;
        bb
    }
    fn start_bb(&self, b: &mut FunctionBuilder, block: BlockId) {
        b.start_block(block).unwrap();
        b.current_block = block;
    }
    fn end_current_bb(&self, b: &mut FunctionBuilder) {
        let bb = b.current_block;
        if b.end_block(bb).is_err() {
            panic!("Failed to end block {} in function {}:\n{}", bb.index(), self.fn_name(b.name), self.display_block(b, bb));
        }
        let block = &b.blocks[bb];
        let last_instr = &b.instrs[block.instrs.last().copied().unwrap()].kind;
        assert!(
            matches!(last_instr, InstrKind::Jump(_) | InstrKind::CondBr { .. } | InstrKind::SwitchBr { .. } | InstrKind::Ret { .. } | InstrKind::LegacyIntrinsic { intr: LegacyIntrinsic::Panic, .. }),
            "expected terminal instruction before moving on to next block, found {:?}",
            last_instr,
        );
    }
}

impl DriverRwRef<'_> {
    fn build_function(&self, name: Option<Sym>, func_ty: FunctionType, body: FunctionBody, params: Range<DeclId>, generic_params: Range<GenericParamId>, is_comptime: bool, tp: &dyn TypeProvider) -> Function {
        debug_assert_ne!(func_ty.return_ty.as_ref(), &Type::Error, "can't build MIR function with Error return type");

        let mut entry = Block::default();
        let mut instr_namespace = InstrNamespace::default();
        let mut instrs = IndexVec::new();
        let mut source_ranges = HashMap::new();
        let mut instr_names = HashMap::new();
        for param in range_iter(params.clone()) {
            let d = self.read();
            assert!(matches!(df!(d, param.ast), ast::Decl::Parameter { .. }));
            let ty = self.read().decl_type(param, tp);
            let instr = InstrKind::Parameter(ty.clone());
            let id = instrs.push(Instr::new(instr, ty));
            let range = df!(d, param.range);
            let name = instr_namespace.insert(self.read().display_item(&source_ranges, range).to_string());
            source_ranges.insert(id, range);
            instr_names.insert(id, name);
            entry.instrs.push(id);
        }
        let mut blocks = IndexVec::new();
        let entry_block = blocks.push(entry);
        let mut b = FunctionBuilder {
            name,
            ty: func_ty,
            blocks,
            first_block: entry_block,
            last_block: entry_block,
            entry_block,
            instrs,
            source_ranges,
            instr_names,
            block_states: HashMap::new(),
            current_block: entry_block,
            stored_decl_locs: IndexVec::new(),
            instr_namespace,
            loops: Default::default(),
            pattern_matching_scrutinees: Default::default(),
        };
        self.read().start_bb(&mut b, entry_block);
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
            FunctionBody::ConstantInstruction { parent_func, instr } => {
                let instruction = parent_func.instrs[instr].kind.clone();
                match instruction {
                    InstrKind::LegacyIntrinsic { arguments, .. } | InstrKind::Call { arguments, .. } => {
                        let mut copier = MirCopier::default();
                        for arg in arguments {
                            self.copy_instruction_if_needed(&mut b, &mut copier, parent_func, arg);
                        }
                        let result = self.copy_instruction_if_needed(&mut b, &mut copier, parent_func, instr);
                        self.read().push_instr(&mut b, InstrKind::Ret(result), result);
                        self.read().end_current_bb(&mut b);
                    },
                    InstrKind::DiscriminantAccess { val } | InstrKind::SignExtend(val, _) | InstrKind::ZeroExtend(val, _) => {
                        let mut copier = MirCopier::default();
                        self.copy_instruction_if_needed(&mut b, &mut copier, parent_func, val);
                        let result = self.copy_instruction_if_needed(&mut b, &mut copier, parent_func, instr);
                        self.read().push_instr(&mut b, InstrKind::Ret(result), result);
                        self.read().end_current_bb(&mut b);
                    },
                    _ => unimplemented!("{:?}", instruction),
                }
                None
            },
        };
        // Remove inactive blocks
        let mut cursor = b.make_cursor_mut();
        while let Some(block) = cursor.next_block() {
            if cursor.blocks[block].instrs.is_empty() {
                cursor.remove_block();
            }
        }
        b.check_all_blocks_ended();
        let mut function = Function {
            name: b.name,
            ty: b.ty,
            blocks: b.blocks,
            first_block: b.first_block,
            last_block: b.last_block,
            entry_block: b.entry_block,
            instrs: b.instrs,
            source_ranges: b.source_ranges,
            instr_names: b.instr_names,
            poisoned_instrs: Default::default(),
            instr_namespace: b.instr_namespace,
            decl,
            generic_params,
            is_comptime,
        };

        let is_constant_instruction = matches!(body, FunctionBody::ConstantInstruction { .. });
        self.optimize_function(&mut function, !is_constant_instruction, tp);
        self.validate_function(&function);
        // At this point, it is assumed that any comptime functions that can be evaluated at compile time already have
        // been. Therefore, if there are any calls to comptime functions left, it must be because they were passed
        // non-const arguments. If *this* function itself is comptime, then this is perfectly fine.
        if !function.is_comptime {
            self.check_no_comptime_calls(&function);
        }
        function
    }
}

#[derive(Default)]
struct MirTransformer {
    delete_list: HashSet<InstrId>,
    ref_replace_list: Vec<(InstrId, InstrId)>,
    replace_list: Vec<(InstrId, InstrKind)>,
}

impl MirTransformer {
    fn q_delete_and_replace_references(&mut self, to_delete: InstrId, to_replace_with: InstrId) {
        self.delete_list.insert(to_delete);
        self.ref_replace_list.push((to_delete, to_replace_with));
    }

    fn q_delete(&mut self, to_delete: InstrId) {
        self.delete_list.insert(to_delete);
    }

    fn q_delete_items(&mut self, to_delete: impl IntoIterator<Item=InstrId>) {
        self.delete_list.extend(to_delete);
    }

    fn q_replace_instr(&mut self, to_replace: InstrId, instr: InstrKind) {
        self.replace_list.push((to_replace, instr));
    }

    fn transform(self, func: &mut Function) -> bool {
        if self.delete_list.is_empty() && self.ref_replace_list.is_empty() && self.replace_list.is_empty() {
            return false;
        }

        let mut cursor = func.make_cursor_mut();
        while let Some(block_id) = cursor.next_block() {
            let block = &mut cursor.blocks[block_id];
            block.instrs.retain(|instr| !self.delete_list.contains(instr));
            for &instr in &block.instrs {
                for &(old, new) in &self.ref_replace_list {
                    cursor.instrs[instr].kind.replace_value(old, new);
                }
            }
        }
        for (id, new_instr) in self.replace_list {
            cursor.instrs[id].kind = new_instr;
        }

        true
    }
}

impl Driver {
    fn remove_redundant_loads(&self, func: &mut Function) -> bool {
        // Remove obviously-redundant loads (assumes no other threads are accessing a memory location simultaneously)
        let mut transformer = MirTransformer::default();
        for block in func.make_cursor().blocks_iter() {
            for (i, &instr_id) in block.instrs.iter().enumerate() {
                let instr = &func.instrs[instr_id].kind;
                if let &InstrKind::Store { location, value } = instr
                    && i + 1 < block.instrs.len() {
                        let next_instr = block.instrs[i+1];
                        if let InstrKind::Load(load_loc) = func.instrs[next_instr].kind
                            && load_loc == location {
                                transformer.q_delete_and_replace_references(next_instr, value);
                            }
                    }
            }
        }
        transformer.transform(func)
    }

    fn remove_unused_allocas(&self, func: &mut Function) -> bool {
        let mut transformer = MirTransformer::default();
        for block in func.make_cursor().blocks_iter() {
            for &instr_id in &block.instrs {
                let instr = &func.instrs[instr_id].kind;
                let mut potential_deletions = Vec::new();
                if let InstrKind::Alloca(_) = instr {
                    let mut is_used = false;
                    'check_uses: for other_block in func.make_cursor().blocks_iter() {
                        for &other_instr_id in &other_block.instrs {
                            let other_instr = &func.instrs[other_instr_id].kind;
                            if other_instr.references_value(instr_id) {
                                if let &InstrKind::Store { value, .. } = other_instr {
                                    // If the address of the alloca is used as the *value* in the store, then we can't
                                    // delete either instruction. Otherwise, it must be the location, in which case we
                                    // can delete both (assuming the alloca isn't used elsewhere).
                                    if value == instr_id {
                                        is_used = true;
                                        break 'check_uses;
                                    } else {
                                        potential_deletions.push(other_instr_id);
                                    }
                                } else {
                                    is_used = true;
                                    break 'check_uses;
                                }
                            }
                        }
                    }
                    if !is_used {
                        transformer.q_delete(instr_id);
                        transformer.q_delete_items(potential_deletions);
                    }
                }
            }

        }
        transformer.transform(func)
    }

    fn remove_unused_values(&self, func: &mut Function) -> bool {
        let mut transformer = MirTransformer::default();
        for block in func.make_cursor().blocks_iter() {
            for &instr_id in &block.instrs {
                let instr = &func.instrs[instr_id].kind;
                if let InstrKind::Const(_) | InstrKind::Load(_) = instr {
                    let mut is_used = false;
                    'check_uses: for other_block in func.make_cursor().blocks_iter() {
                        for &other_instr_id in &other_block.instrs {
                            let other_instr = &func.instrs[other_instr_id].kind;
                            if other_instr.references_value(instr_id) {
                                is_used = true;
                                break 'check_uses;
                            }
                        }
                    }
                    if !is_used {
                        transformer.q_delete(instr_id);
                    }
                }
            }
        }
        transformer.transform(func)
    }

    fn remove_redundant_blocks(&self, func: &mut Function) -> bool {
        // Replace blocks that do nothing but branch to another block
        let mut replace_list = Vec::new();
        let mut delete_list = HashSet::new();
        let mut new_entry_block = None;
        let mut did_something = false;
        for (block_id, block) in func.make_cursor().blocks_iter_enumerated() {
            let mut num_parameters = 0;
            for &instr in &block.instrs {
                if !matches!(&func.instrs[instr].kind, InstrKind::Parameter(_)) {
                    break;
                }
                num_parameters += 1;
            }
            if block.instrs.len() - num_parameters != 1 { continue; }

            let terminal = *block.instrs.last().unwrap();
            let terminal = &func.instrs[terminal].kind;
            // TODO: continue the transformation even if there are basic block arguments
            if let InstrKind::Jump(other) = terminal && other.arguments.is_empty() && other.bb != block_id {
                replace_list.push((block_id, other.bb));
                delete_list.insert(block_id);
                did_something = true;
                if block_id == func.entry_block {
                    let parameters = block.instrs[..num_parameters].to_vec();
                    new_entry_block = Some((other.bb, parameters));
                }
                if new_entry_block.as_ref().map(|(b, _)| b) == Some(&block_id) {
                    new_entry_block.as_mut().unwrap().0 = other.bb;
                }
            }
        }
        if let Some((new_entry_block, parameters)) = new_entry_block {
            func.entry_block = new_entry_block;
            func.blocks[new_entry_block].instrs.splice(0..0, parameters);
        }
        let mut cursor = func.make_cursor_mut();
        for block in delete_list {
            cursor.goto_top(block);
            cursor.remove_block();
        }
        let mut cursor = func.make_cursor_mut();
        while let Some(block_id) = cursor.next_block() {
            let block = &cursor.blocks[block_id];
            let terminal = *block.instrs.last().unwrap();
            let terminal = &mut cursor.instrs[terminal].kind;
            for &(from, to) in &replace_list {
                terminal.replace_bb(from, to);
            }
        }
        did_something
    }

    fn traverse_descendants(&self, func: &Function, visited: &mut HashSet<BlockId>, block: BlockId) {
        if !visited.insert(block) { return; }

        let terminal = *func.blocks[block].instrs.last().unwrap();
        let terminal = &func.instrs[terminal].kind;
        match terminal {
            InstrKind::Jump(target) => self.traverse_descendants(func, visited, target.bb),
            InstrKind::CondBr { true_target, false_target, .. } => {
                self.traverse_descendants(func, visited, true_target.bb);
                self.traverse_descendants(func, visited, false_target.bb);
            },
            InstrKind::SwitchBr { cases, catch_all_target, .. } => {
                for case in cases {
                    self.traverse_descendants(func, visited, case.target.bb);
                }
                self.traverse_descendants(func, visited, catch_all_target.bb);
            },
            _ => {},
        }
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
        let mut transformer = MirTransformer::default();
        for block in func.make_cursor().blocks_iter() {
            if let Some(&terminal) = block.instrs.last() {
                match &func.instrs[terminal].kind {
                    InstrKind::CondBr { condition, true_target, false_target } => {
                        let condition = &func.instrs[*condition].kind;
                        if let &InstrKind::Const(Const::Bool(condition)) = condition {
                            let destination = if condition {
                                true_target
                            } else {
                                false_target
                            };
                            transformer.q_replace_instr(terminal, InstrKind::Jump(destination.clone()));
                        }
                    },
                    InstrKind::SwitchBr { scrutinee, cases, catch_all_target } => {
                        let scrutinee = &func.instrs[*scrutinee].kind;
                        if let InstrKind::Const(scrutinee @ Const::Int { .. }) = scrutinee {
                            let destination = cases.iter()
                                .find(|case| *scrutinee == case.value)
                                .map(|case| &case.target)
                                .unwrap_or(catch_all_target);
                            transformer.q_replace_instr(terminal, InstrKind::Jump(destination.clone()));
                        }
                    },
                    _ => {},
                }
            }
        }

        transformer.transform(func)
    }

    // fn remove_return_non_shared_void(&self, func: &mut Function) -> bool {
    //     let mut transformer = MirTransformer::default();
    //     for block in func.make_cursor().blocks_iter() {
    //         for &instr_id in &block.instrs {
    //             let instr = &func.instrs[instr_id].kind;
    //             if let &InstrKind::Ret(ret_val) = instr
    //                 && *func.ty.return_ty == Type::Void && ret_val != VOID_INSTR {
    //                     transformer.q_replace_instr(instr_id, InstrKind::Ret(VOID_INSTR));
    //                 }
    //         }
    //     }
    //     transformer.transform(func)
    // }
}

#[derive(Default)]
struct MirCopier {
    old_to_new: HashMap<InstrId, InstrId>,
}

struct BlockMetadata {
    param_tys: SmallVec<[Type; 2]>,
}

impl DriverRwRef<'_> {
    fn instruction_is_const(&self, func: &Function, instr: InstrId) -> bool {
        let d = self.read();
        let instr = &func.instrs[instr].kind;
        match *instr {
            InstrKind::Const(_) | InstrKind::Void => true,
            InstrKind::LegacyIntrinsic { intr, .. } => {
                match intr {
                    LegacyIntrinsic::Mult | LegacyIntrinsic::Div | LegacyIntrinsic::Mod | LegacyIntrinsic::Add | LegacyIntrinsic::Sub
                        | LegacyIntrinsic::Less | LegacyIntrinsic::LessOrEq | LegacyIntrinsic::Greater | LegacyIntrinsic::GreaterOrEq
                        | LegacyIntrinsic::Eq | LegacyIntrinsic::NotEq | LegacyIntrinsic::BitwiseAnd | LegacyIntrinsic::BitwiseOr
                        | LegacyIntrinsic::BitwiseNot | LegacyIntrinsic::BitwiseXor | LegacyIntrinsic::LeftShift | LegacyIntrinsic::RightShift
                        | LegacyIntrinsic::LogicalAnd | LegacyIntrinsic::LogicalOr | LegacyIntrinsic::LogicalNot | LegacyIntrinsic::Neg
                        | LegacyIntrinsic::Pos => {
                        instr.referenced_values().iter().all(|&val| self.instruction_is_const(func, val))
                    }
                    _ => false,
                }
            },
            InstrKind::LogicalNot(val) | InstrKind::Truncate(val, _) | InstrKind::SignExtend(val, _) | InstrKind::ZeroExtend(val, _)
                | InstrKind::FloatCast(val, _) | InstrKind::FloatToInt(val, _) | InstrKind::IntToFloat(val, _)
                | InstrKind::DiscriminantAccess { val }
                => self.instruction_is_const(func, val),
            InstrKind::Call { func: callee, .. } if d.mir.function_sigs.pin().get(&callee).unwrap().is_comptime => instr.referenced_values().iter().all(|&val| self.instruction_is_const(func, val)),
            _ => false,
        }
    }

    fn instruction_is_nontrivial_const(&self, func: &Function, instr: InstrId) -> bool {
        self.instruction_is_const(func, instr) && !matches!(&func.instrs[instr].kind, InstrKind::Const(_) | InstrKind::Void)
    }

    fn copy_instruction_if_needed(&self, b: &mut FunctionBuilder, copier: &mut MirCopier, src_function: &Function, instr_id: InstrId) -> InstrId {
        if let Some(&new) = copier.old_to_new.get(&instr_id) {
            new
        } else {
            let mut instr = src_function.instrs[instr_id].kind.clone();
            let replacements: Vec<_> = instr.referenced_values().into_iter().map(|arg| (arg, self.copy_instruction_if_needed(b, copier, src_function, arg))).collect();
            for (old, new) in replacements {
                instr.replace_value(old, new);
            }

            let copied_instr_id = self.read().push_instr(b, instr, src_function.source_ranges[&instr_id]);
            copier.old_to_new.insert(instr_id, copied_instr_id);
            copied_instr_id
        }
    }

    fn eval_constants(&self, func: &mut Function, tp: &dyn TypeProvider) -> bool {
        let mut transformer = MirTransformer::default();
        let mut poison_list = Vec::new();
        for block in func.make_cursor().blocks_iter() {
            for &instr in &block.instrs {
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
                if self.instruction_is_nontrivial_const(func, instr) && !func.poisoned_instrs.contains(&instr) {
                    let ty = func.type_of(instr).clone();
                    let func_ty = FunctionType { param_tys: vec![], has_c_variadic_param: false, return_ty: Box::new(ty.clone()) };
                    let new_func = self.build_function(func.name, func_ty, FunctionBody::ConstantInstruction { parent_func: func, instr }, empty_range(), empty_range(), true, tp);
                    let Ok(result) = self.call(FunctionRef::Ref(new_func), Vec::new(), Vec::new()) else {
                        // Make sure we won't repeatedly try and fail to const-eval this instruction.
                        poison_list.push(instr);
                        continue;
                    };
                    let konst = self.read().value_to_const(result, ty, tp);
                    transformer.replace_list.push((instr, InstrKind::Const(konst)));
                }
            }
        }

        for instr in poison_list {
            func.poisoned_instrs.insert(instr);
        }

        transformer.transform(func)
    }

    fn optimize_function(&self, func: &mut Function, should_eval_constants: bool, tp: &dyn TypeProvider) {
        let mut did_something = true;
        while did_something {
            did_something = false;
            did_something |= self.read().remove_redundant_loads(func);
            did_something |= self.read().remove_unused_allocas(func);
            did_something |= self.read().remove_unused_values(func);
            did_something |= self.read().remove_constant_branches(func);
            did_something |= self.read().remove_redundant_blocks(func);
            did_something |= self.read().remove_unreachable_blocks(func);
            // did_something |= self.read().remove_return_non_shared_void(func);
            if should_eval_constants {
                did_something |= self.eval_constants(func, tp);
            }
        }
    }

    fn check_jump_target(&self, func: &Function, target: &JumpTarget, block_metadata: &HashMap<BlockId, BlockMetadata>) {
        // TODO: check that all jump targets include arguments for all parameters, and that their types match
        let metadata = &block_metadata[&target.bb];
        assert_eq!(target.arguments.len(), metadata.param_tys.len(), "number of basic block arguments != params");

        for (&arg, param_ty) in target.arguments.iter().zip(&metadata.param_tys) {
            let arg_ty = &func.instrs[arg].ty;
            assert!(arg_ty.trivially_convertible_to(param_ty), "basic block argument type doesn't match param type ({:?}, {:?})", arg_ty, param_ty);
        }
    }

    fn check_basic_block_params(&self, func: &Function) {
        let mut block_metadata = HashMap::<BlockId, BlockMetadata>::new();
        for (bb, block) in func.make_cursor().blocks_iter_enumerated() {
            let mut expecting_parameters = true;
            let mut param_tys = SmallVec::new();
            for &instr in &block.instrs {
                let instr = &func.instrs[instr].kind;
                if let InstrKind::Parameter(ty) = instr {
                    assert!(expecting_parameters, "Parameter instruction in the middle of a block");
                    param_tys.push(ty.clone());
                } else {
                    expecting_parameters = false;
                }
            }

            block_metadata.insert(bb, BlockMetadata { param_tys });
        }

        for (bb, block) in func.make_cursor().blocks_iter_enumerated() {
            let metadata = &block_metadata[&bb];
            for &instr in &block.instrs[metadata.param_tys.len()..] {
                let instr = &func.instrs[instr].kind;
                match instr {
                    InstrKind::Jump(target) => self.check_jump_target(func, target, &block_metadata),
                    InstrKind::CondBr { true_target, false_target, .. } => {
                        self.check_jump_target(func, true_target, &block_metadata);
                        self.check_jump_target(func, false_target, &block_metadata);
                    },
                    InstrKind::SwitchBr { cases, catch_all_target, .. } => {
                        for case in cases {
                            self.check_jump_target(func, &case.target, &block_metadata);
                        }
                        self.check_jump_target(func, catch_all_target, &block_metadata);
                    },
                    _ => {},
                }
            }
        }
    }

    fn check_no_invalid_instructions(&self, func: &Function) {
        for block in func.make_cursor().blocks_iter() {
            for &instr in &block.instrs {
                if matches!(&func.instrs[instr].kind, InstrKind::Invalid) {
                    panic!("Found invalid instruction in function");
                }
            }
        }
    }

    fn check_no_comptime_calls(&self, func: &Function) {
        let mut comptime_calls = Vec::new();
        for block in func.make_cursor().blocks_iter() {
            for &instr in &block.instrs {
                if let &InstrKind::Call { func: called_func, .. } = &func.instrs[instr].kind
                    && self.read().mir.function_sigs.pin().get(&called_func).unwrap().is_comptime {
                        comptime_calls.push((called_func, instr));
                    }
            }
        }

        for (func, _instr) in comptime_calls {
            let name_sym = self.read().mir.function_sigs.pin().get(&func).unwrap().name;
            let name = self.read().fn_name(Some(name_sym)).to_string();
            self.read().diag.push(
                Error::new(format!("unable to evaluate call to @comptime function '{}'", name))
            );
        }
    }

    fn validate_function(&self, func: &Function) {
        self.check_no_invalid_instructions(func);
        self.check_basic_block_params(func);
    }
}

impl Driver {
    fn generate_type_of(&self, b: &FunctionBuilder, instr: &InstrKind) -> Type {
        match instr {
            InstrKind::Void | InstrKind::Store { .. } => Type::Void,
            InstrKind::ObjcClassRef { .. } => Type::Void.ptr(),
            InstrKind::Invalid => Type::Error,
            InstrKind::Pointer { .. } | InstrKind::Struct { .. } | InstrKind::GenericParam(_) | InstrKind::Enum { .. } | InstrKind::FunctionTy { .. } => Type::Ty,
            &InstrKind::StructLit { ref fields, id } => {
                let field_tys = fields.iter()
                    .map(|&instr| {
                        let instr = &b.instrs[instr].kind;
                        self.generate_type_of(b, instr)
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
            &InstrKind::Call { func, ref generic_arguments, .. } => {
                let function_sigs = self.mir.function_sigs.pin();
                let signature = function_sigs.get(&func).unwrap();
                let mut replacements = HashMap::new();
                for (param, arg) in range_iter(signature.generic_params.clone()).zip(generic_arguments) {
                    replacements.insert(param, arg.clone());
                }

                signature.ty.return_ty.as_ref().clone().replacing_generic_params(&replacements)
            },
            &InstrKind::FunctionRef { func, .. } => Type::Function(self.mir.function_sigs.pin().get(&func).unwrap().ty.clone()),
            InstrKind::ExternCall { func, .. } => self.mir.extern_mods.pin().get(&func.extern_mod).unwrap().imported_functions[func.index].ty.return_ty.as_ref().clone(),
            InstrKind::LegacyIntrinsic { ty, .. } => ty.clone(),
            &InstrKind::Intrinsic { intr, .. } => self.ast.intrinsics[intr].ret_ty.clone(),
            InstrKind::Reinterpret(_, ty) | InstrKind::Truncate(_, ty) | InstrKind::SignExtend(_, ty)
            | InstrKind::ZeroExtend(_, ty) | InstrKind::FloatCast(_, ty) | InstrKind::FloatToInt(_, ty)
            | InstrKind::IntToFloat(_, ty)
            => ty.clone(),
            &InstrKind::Load(instr) => match b.type_of(instr) {
                Type::Pointer(pointee) => pointee.ty.clone(),
                _ => Type::Error,
            },
            &InstrKind::AddressOfStatic(statik) => self.mir.statics[statik].val.ty().mut_ptr(),
            InstrKind::Ret(_) | InstrKind::Jump(_) | InstrKind::CondBr { .. } | InstrKind::SwitchBr { .. } => Type::Never,
            InstrKind::Parameter(ty) => ty.clone(),
            &InstrKind::DirectFieldAccess { val, index } => {
                let base_ty = b.type_of(val);
                match base_ty {
                    Type::Struct(strukt) => strukt.field_tys[index].clone(),
                    _ => panic!("Cannot directly access field of non-struct type {:?}!", base_ty),
                }
            },
            &InstrKind::IndirectFieldAccess { val, index } => {
                let base_ty = b.type_of(val).deref().unwrap();
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
                let base_ty = b.type_of(val);
                match base_ty {
                    &Type::Enum(EnumType { identity, .. }) => self.mir.enums.pin().get(&identity).unwrap().payload_tys[variant_index].clone(),
                    _ => panic!("Cannot directly access payload of non-enum type {:?}!", base_ty),
                }
            },
            InstrKind::DiscriminantAccess { .. } => TYPE_OF_DISCRIMINANTS, // TODO: update this when discriminants can be other types
        }
    }

    fn push_instr(&self, b: &mut FunctionBuilder, instr: InstrKind, item: impl Into<ToSourceRange>) -> InstrId {
        let ty = self.generate_type_of(b, &instr);
        let id = b.instrs.push(Instr::new(instr, ty));
        let source_range = self.get_range_with_mir_ctx(item, &b.source_ranges);
        b.source_ranges.insert(id, source_range);
        b.blocks[b.current_block].instrs.push(id);

        id
    }

    fn push_instr_with_name(&self, b: &mut FunctionBuilder, instr: InstrKind, item: impl Into<ToSourceRange>, name: impl Into<String>) -> InstrId {
        let ty = self.generate_type_of(b, &instr);
        let id = b.instrs.push(Instr::new(instr, ty));
        let source_range = self.get_range_with_mir_ctx(item, &b.source_ranges);
        b.source_ranges.insert(id, source_range);
        let name = b.instr_namespace.insert(name.into());
        b.instr_names.insert(id, name);
        b.blocks[b.current_block].instrs.push(id);

        id
    }
}

impl DriverRwRef<'_> {
    fn build_scope_item(&self, b: &mut FunctionBuilder, item: ScopedItem, tp: &dyn TypeProvider) {
        match item {
            ScopedItem::Expr { expr, .. } => {
                self.build_expr(b, expr, Context::new(0, DataDest::Void, ControlDest::Continue), tp);
            },
            ScopedItem::Decl(decl) => {
                let d = self.read();
                match df!(d, decl.ast) {
                    ast::Decl::Stored { id, root_expr, .. } => {
                        drop(d);
                        let ty = tp.ty(root_expr).clone();
                        let name = self.read().display_item(&b.source_ranges, decl).to_string();
                        let location = self.read().push_instr_with_name(b, InstrKind::Alloca(ty), decl, name);
                        b.stored_decl_locs.push_at(id, location);
                        let val = self.build_expr(b, root_expr, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                        let instr = self.read().handle_indirection(b, val);
                        let range = self.read().get_range_with_mir_ctx(location, &b.source_ranges) + self.read().get_range_with_mir_ctx(instr, &b.source_ranges);
                        self.read().push_instr(b, InstrKind::Store { location, value: instr }, range);
                    },
                    ast::Decl::Function { .. } => {},
                    _ => panic!("Invalid scope item"),
                }
            },
        }
    }

    fn build_scope(&self, b: &mut FunctionBuilder, scope: ImperScopeId, ctx: Context, tp: &dyn TypeProvider) -> Value {
        let len = self.read().ast.imper_scopes[scope].items.len();
        for i in 0..len {
            let item = self.read().ast.imper_scopes[scope].items[i];
            self.build_scope_item(b, item, tp);
        }
        let terminal_expr = self.read().ast.imper_scopes[scope].terminal_expr;
        self.build_expr(b, terminal_expr, ctx, tp)
    }
}

impl Driver {
    fn get_base(&self, id: DeclRefId) -> ExprId {
        match self.ast.decl_refs[id].namespace {
            ast::Namespace::MemberRef { base_expr } => base_expr,
            _ => panic!("Expected member ref expression"),
        }
    }
}

impl DriverRwRef<'_> {
    fn get_callee_declref(&self, b: &mut FunctionBuilder, tp: &dyn TypeProvider, callee_id: ExprId) -> DeclRef {
        let d = self.read();
        let callee = &ef!(d, callee_id.ast);
        if let &Expr::DeclRef { id, .. } = callee {
            drop(d);
            self.get(b, id, tp)
        } else {
            panic!("expected declref callee");
        }
    }

    fn get(&self, b: &mut FunctionBuilder, decl_ref_id: DeclRefId, tp: &dyn TypeProvider) -> DeclRef {
        let id = tp.selected_overload(decl_ref_id).expect("No overload found!");
        let generic_params = self.read().tir_builder.decls[id].generic_params.clone();
        let generic_arguments = tp.generic_arguments(decl_ref_id).as_ref().unwrap_or(&Vec::new()).clone();
        assert_eq!(generic_params.end - generic_params.start, generic_arguments.len());
        let expr = self.read().ast.decl_refs[decl_ref_id].expr;
        let name = self.read().display_item(&b.source_ranges, id).to_string();
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
                let value = b.blocks[b.entry_block].instrs[index];
                DeclRef::Value(value.direct())
            },
            Decl::GenericParam(param) => {
                DeclRef::Value(self.read().push_instr(b, InstrKind::GenericParam(param), expr).direct())
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

                let name = self.read().fmt_const_for_instr_name(&konst).to_string();
                DeclRef::Value(self.read().push_instr_with_name(b, InstrKind::Const(konst), expr, name).direct())
            },
            Decl::Static(statik) => {
                DeclRef::Value(self.read().push_instr_with_name(b, InstrKind::AddressOfStatic(statik), expr, format!("static_{}", name)).indirect())
            },
            Decl::Field { index } => {
                let base = self.read().get_base(decl_ref_id);
                let base_ty = tp.ty(base);
                let mut base = self.build_expr(b, base, Context::default(), tp);
                if matches!(base_ty, Type::Pointer(_)) {
                    base.indirection += 1;
                }
                if base.indirection > 0 {
                    let base_ptr = self.read().handle_indirection(b, base.get_address());
                    DeclRef::Value(self.read().push_instr(b, InstrKind::IndirectFieldAccess { val: base_ptr, index }, expr).indirect())
                } else {
                    debug_assert_eq!(base.indirection, 0, "tried to dereference a struct?!");
                    DeclRef::Value(self.read().push_instr(b, InstrKind::DirectFieldAccess { val: base.instr, index }, expr).direct())
                }
            },
            Decl::InternalField(field) => {
                let base = self.read().get_base(decl_ref_id);
                let base = self.build_expr(b, base, Context::default(), tp);
                let base = self.read().handle_indirection(b, base);
                DeclRef::Value(self.read().push_instr(b, InstrKind::InternalFieldAccess { val: base, field }, expr).direct())
            },
            Decl::Variant { enuum, index, payload_ty } => {
                if payload_ty.is_some() {
                    DeclRef::EnumVariantWithPayload { enuum, index, payload_ty }
                } else {
                    let payload_tys = self.read().mir.enums.pin().get(&enuum).unwrap().payload_tys.to_vec();
                    let konst = Const::Variant { enuum, index, payload_tys };
                    let name = self.read().fmt_const_for_instr_name(&konst).to_string();
                    DeclRef::Value(self.read().push_instr_with_name(b, InstrKind::Const(konst), expr, name).direct())
                }
            },
            Decl::Invalid => panic!("INVALID DECL"),
        }
    }

    fn build_if_expr_recurse(&self, b: &mut FunctionBuilder, condition: ExprId, then_scope: ImperScopeId, true_bb: BlockId, false_bb: BlockId, post_bb: BlockId, pass_value_as_argument: bool, ctx: Context, tp: &dyn TypeProvider) {
        self.build_expr(
            b,
            condition,
            Context::new(0, DataDest::Branch(true_bb, false_bb), ControlDest::Continue),
            tp,
        );
        self.read().start_bb(b, true_bb);
        let scope_ctx = ctx.redirect(post_bb, pass_value_as_argument);
        self.build_scope(b, then_scope, scope_ctx, tp);
    }
}

impl DriverRwRef<'_> {
    fn build_if_expr(&self, b: &mut FunctionBuilder, expr: ExprId, ty: Type, condition: ExprId, then_scope: ImperScopeId, else_scope: Option<ImperScopeId>, ctx: Context, tp: &dyn TypeProvider) -> Value {
        let true_bb = self.read().create_bb(b);
        let mut false_bb: BlockId = self.read().create_bb(b);
        if matches!(ctx.data, DataDest::Read) {
            assert_matches!(ctx.control, ControlDest::Continue);
        }
        let pass_value_as_argument = matches!((&ctx.data, else_scope), (DataDest::Read, Some(_)));
        let (post_bb, we_own_post_bb) = match ctx.control {
            ControlDest::Jump(target) => (target, false),
            _ => (self.read().create_bb(b), true),
        };
        if else_scope.is_none() && !pass_value_as_argument {
            false_bb = post_bb;
        }

        self.build_if_expr_recurse(b, condition, then_scope, true_bb, false_bb, post_bb, pass_value_as_argument, ctx, tp);
        let mut next_scope = else_scope;
        let mut next_bb = false_bb;

        // Iterate through a linked list of if-else-if branches. Terminate when you find an if with no else branch or
        // an else branch with something other than an if.
        // This is done to reduce the size of the stack when generating code for long chains of if statements.
        while let Some(cur) = next_scope {
            self.read().start_bb(b, next_bb);

            let scope_ctx = ctx.redirect(post_bb, pass_value_as_argument);
            let terminal_expr = self.read().ast.imper_scopes[cur].terminal_expr;
            // If the current scope consists of a lone if expression
            if self.read().ast.imper_scopes[cur].items.is_empty() {
                let d = self.read();
                if let Expr::If { condition, then_scope, else_scope } = ef!(d, terminal_expr.ast) {
                    drop(d);
                    let true_bb = self.read().create_bb(b);
                    let false_bb = if else_scope.is_some() {
                        self.read().create_bb(b)
                    } else {
                        post_bb
                    };
                    self.build_if_expr_recurse(b, condition, then_scope, true_bb, false_bb, post_bb, pass_value_as_argument, scope_ctx, tp);
                    next_scope = else_scope;
                    next_bb = false_bb;
                    continue;
                }
            }

            self.build_scope(b, cur, scope_ctx, tp);
            next_scope = None;
        }

        if we_own_post_bb {
            self.read().start_bb(b, post_bb);
            if pass_value_as_argument {
                // TODO: this might be the wrong type if indirection != 0
                self.read().push_instr(b, InstrKind::Parameter(ty.clone()), expr).direct()
            } else if else_scope.is_none() {
                let void_instr = self.read().push_instr(b, InstrKind::Void, expr);
                self.handle_context(b, void_instr.direct(), ctx)
            } else {
                self.read().push_instr(b, InstrKind::Void, expr).direct()
            }
        } else {
            self.read().push_instr(b, InstrKind::Void, expr).direct()
        }
    }

    fn build_expr(&self, b: &mut FunctionBuilder, expr: ExprId, ctx: Context, tp: &dyn TypeProvider) -> Value {
        let ty = self.read().get_canonical_type(tp, expr);

        let expr_val = {
            let d = self.read();
            ef!(d, expr.ast).clone()
        };

        let val = match expr_val {
            Expr::Void | Expr::Error | Expr::ExtendBlock { .. } => {
                self.read().push_instr(b, InstrKind::Void, expr).direct()
            },
            Expr::IntLit { .. } | Expr::DecLit { .. } | Expr::CharLit { .. } | Expr::StrLit { .. } | Expr::BoolLit { .. } | Expr::Const(_) | Expr::Mod { .. } => {
                let konst = self.read().expr_to_const(expr, ty);
                let name = self.read().fmt_const_for_instr_name(&konst).to_string();
                self.read().push_instr_with_name(b, InstrKind::Const(konst), expr, name).direct()
            },
            Expr::Set { lhs, rhs } => {
                let ctx = ctx.new_data_dest(DataDest::Read);
                let val = self.build_expr(
                    b,
                    rhs,
                    ctx,
                    tp,
                );
                let instr = self.read().handle_indirection(b, val);
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
                        self.read().push_instr(b, InstrKind::FunctionRef { func, generic_arguments: generic_args }, expr).direct()
                    },
                    DeclRef::LegacyIntrinsic { intrinsic, ty } => {
                        assert!(tp.ty(expr).return_ty().is_none(), "referring to intrinsic functions is not yet supported");
                        self.read().push_instr(b, InstrKind::LegacyIntrinsic { arguments: SmallVec::new(), ty, intr: intrinsic }, expr).direct()
                    },
                    DeclRef::Value(value) => value,
                    DeclRef::ObjcClassRef { extern_mod, index } => self.read().push_instr(b, InstrKind::ObjcClassRef { extern_mod, index }, expr).direct(),
                    other => todo!("referring to {:?} not yet supported", other),
                }
            },
            Expr::Call { callee, ref arguments } => {
                let arguments = arguments.clone();
                let decl_ref = self.get_callee_declref(b, tp, callee);

                fn get_args(d: &DriverRwRef, b: &mut FunctionBuilder, tp: &dyn TypeProvider, arguments: &[ExprId]) -> SmallVec<[InstrId; 2]> {
                    arguments.iter().map(|&argument| {
                        let val = d.build_expr(b, argument, Context::default(), tp);
                        d.read().handle_indirection(b, val)
                    }).collect()
                }

                match decl_ref {
                    DeclRef::Function { func, generic_args } => {
                        let d = self.read();
                        let Expr::DeclRef { id: decl_ref_id, .. } = ef!(d, callee.ast) else {
                            panic!("expected declref callee");
                        };
                        drop(d);
                        let mut arguments = get_args(self, b, tp, &arguments);
                        let d = self.read();
                        // Handle method calls.
                        // TODO: comparing the number of arguments to the number of parameters to determine whether this is a method call is kind of a horrible hack
                        // TODO: unify code here with the near-identical `DeclRef::MethodIntrinsic` case
                        if arguments.len() != d.mir.function_sigs.pin().get(&func).unwrap().ty.param_tys.len() {
                            let base = d.get_base(decl_ref_id);
                            let base_ty = tp.ty(base);
                            let self_ty = self.read().mir.function_sigs.pin().get(&func).unwrap().ty.param_tys.first().unwrap().clone();
                            let indirection = !base_ty.trivially_convertible_to(&self_ty) as i8;
                            drop(d);
                            let base = self.build_expr(b, base, Context::new(indirection, DataDest::Read, ControlDest::Continue), tp);
                            let base = self.read().handle_indirection(b, base);

                            arguments.insert(0, base);
                        }
                        self.read().push_instr(b, InstrKind::Call { func, arguments, generic_arguments: generic_args }, expr).direct()
                    },
                    DeclRef::ExternFunction { func } => {
                        let arguments = get_args(self, b, tp, &arguments);
                        self.read().push_instr(b, InstrKind::ExternCall { arguments, func }, expr).direct()
                    },
                    DeclRef::LegacyIntrinsic { intrinsic, ty } => match intrinsic {
                        LegacyIntrinsic::LogicalAnd => {
                            assert_eq!(arguments.len(), 2);
                            let (lhs, rhs) = (arguments[0], arguments[1]);
                            let left_true_bb = self.read().create_bb(b);
                            let pass_value_as_argument = matches!(ctx.data, DataDest::Read);
                            if let DataDest::Branch(true_bb, false_bb) = ctx.data {
                                self.build_expr(
                                    b,
                                    lhs,
                                    Context::new(0, DataDest::Branch(left_true_bb, false_bb), ControlDest::Continue),
                                    tp,
                                );

                                self.read().start_bb(b, left_true_bb);
                                return self.build_expr(
                                    b,
                                    rhs,
                                    Context::new(0, DataDest::Branch(true_bb, false_bb), ControlDest::Continue),
                                    tp,
                                );
                            } else {
                                let left_false_bb = self.read().create_bb(b);
                                let after_bb = self.read().create_bb(b);
                                self.build_expr(
                                    b,
                                    lhs,
                                    Context::new(0, DataDest::Branch(left_true_bb, left_false_bb), ControlDest::Continue),
                                    tp,
                                );

                                self.read().start_bb(b, left_true_bb);
                                // No further branching required, because (true && foo) <=> foo
                                let branch_ctx = ctx.redirect(after_bb, pass_value_as_argument);
                                self.build_expr(b, rhs, branch_ctx, tp);

                                self.read().start_bb(b, left_false_bb);
                                let false_const = Const::Bool(false);
                                let name = self.read().fmt_const_for_instr_name(&false_const).to_string();
                                let false_val = self.read().push_instr_with_name(b, InstrKind::Const(false_const), expr, name).direct();
                                self.handle_context(b, false_val, branch_ctx);

                                self.read().start_bb(b, after_bb);
                                if pass_value_as_argument {
                                    self.read().push_instr(b, InstrKind::Parameter(ty), expr).direct()
                                } else {
                                    return self.read().push_instr(b, InstrKind::Void, expr).direct()
                                }
                            }
                        },
                        LegacyIntrinsic::LogicalOr => {
                            assert_eq!(arguments.len(), 2);
                            let (lhs, rhs) = (arguments[0], arguments[1]);
                            let left_false_bb = self.read().create_bb(b);
                            if let DataDest::Branch(true_bb, false_bb) = ctx.data {
                                self.build_expr(
                                    b,
                                    lhs,
                                    Context::new(0, DataDest::Branch(true_bb, left_false_bb), ControlDest::Continue),
                                    tp,
                                );

                                self.read().start_bb(b, left_false_bb);
                                return self.build_expr(
                                    b,
                                    rhs,
                                    Context::new(0, DataDest::Branch(true_bb, false_bb), ControlDest::Continue),
                                    tp
                                );
                            } else {
                                let left_true_bb = self.read().create_bb(b);
                                let after_bb = self.read().create_bb(b);
                                let pass_value_as_argument = matches!(ctx.data, DataDest::Read);
                                self.build_expr(
                                    b,
                                    lhs,
                                    Context::new(0, DataDest::Branch(left_true_bb, left_false_bb), ControlDest::Continue),
                                    tp,
                                );

                                self.read().start_bb(b, left_true_bb);
                                let true_const = Const::Bool(true);
                                let name = self.read().fmt_const_for_instr_name(&true_const).to_string();
                                let true_val = self.read().push_instr_with_name(b, InstrKind::Const(true_const), expr, name).direct();
                                let branch_ctx = ctx.redirect(after_bb, pass_value_as_argument);
                                self.handle_context(b, true_val, branch_ctx);

                                self.read().start_bb(b, left_false_bb);
                                self.build_expr(b, rhs, branch_ctx, tp);

                                self.read().start_bb(b, after_bb);
                                if pass_value_as_argument {
                                    self.read().push_instr(b, InstrKind::Parameter(ty), expr).direct()
                                } else {
                                    return self.read().push_instr(b, InstrKind::Void, expr).direct()
                                }
                            }
                        },
                        LegacyIntrinsic::LogicalNot => {
                            assert_eq!(arguments.len(), 1);
                            let operand = arguments[0];
                            if let DataDest::Branch(true_bb, false_bb) = ctx.data {
                                return self.build_expr(b, operand, Context::new(0, DataDest::Branch(false_bb, true_bb), ctx.control), tp)
                            } else {
                                let operand = self.build_expr(b, operand, Context::default(), tp);
                                self.read().push_instr(b, InstrKind::LogicalNot(operand.instr), expr).direct()
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
                            let address = self.read().handle_indirection(b, address);
                            let loaded = self.read().push_instr(b, InstrKind::Load(address), lhs);
                            let modifier = self.build_expr(b, rhs, Context::default(), tp);
                            let modifier = self.read().handle_indirection(b, modifier);
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
                            let value = self.read().push_instr(b, InstrKind::LegacyIntrinsic { arguments: smallvec![loaded, modifier], ty, intr }, expr);
                            self.read().push_instr(b, InstrKind::Store { location: address, value }, expr).direct()
                        },
                        intrinsic => {
                            let arguments = get_args(self, b, tp, &arguments);
                            self.read().push_instr(b, InstrKind::LegacyIntrinsic { arguments, ty, intr: intrinsic }, expr).direct()
                        }
                    },
                    DeclRef::Intrinsic(intr) => {
                        let arguments = get_args(self, b, tp, &arguments);
                        self.read().push_instr(b, InstrKind::Intrinsic { arguments, intr }, expr).direct()
                    },
                    DeclRef::MethodIntrinsic(intr) => {
                        let d = self.read();
                        let Expr::DeclRef { id: decl_ref_id, .. } = ef!(d, callee.ast) else {
                            panic!("expected declref callee");
                        };
                        let base = self.read().get_base(decl_ref_id);
                        let base_ty = tp.ty(base);
                        let self_ty = self.read().ast.intrinsics[intr].param_tys[0];
                        let self_ty = tp.get_evaluated_type(self_ty);
                        let indirection = !base_ty.trivially_convertible_to(self_ty) as i8;
                        drop(d);
                        let base = self.build_expr(b, base, Context::new(indirection, DataDest::Read, ControlDest::Continue), tp);
                        let base = self.read().handle_indirection(b, base);
                        let mut arguments = get_args(self, b, tp, &arguments);
                        arguments.insert(0, base);
                        self.read().push_instr(b, InstrKind::Intrinsic { arguments, intr }, expr).direct()
                    },
                    DeclRef::EnumVariantWithPayload { enuum, index, .. } => {
                        let arguments = get_args(self, b, tp, &arguments);
                        self.read().push_instr(b, InstrKind::Variant { enuum, index, payload: arguments[0] }, expr).direct()
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
                        let value = self.read().handle_indirection(b, value);
                        self.read().push_instr(b, InstrKind::Reinterpret(value, dest_ty), expr).direct()
                    },
                    CastMethod::Int => {
                        let (src_width, _src_is_signed, dest_width, dest_is_signed) = match (tp.ty(operand), &dest_ty) {
                            (&Type::Int { width: ref src_width, is_signed: src_is_signed }, &Type::Int { width: ref dest_width, is_signed: dest_is_signed })
                                => (src_width, src_is_signed, dest_width, dest_is_signed),
                            (a, b) => panic!("Internal compiler error: found invalid cast types while generating MIR ({:?}, {:?})", a, b)
                        };
                        let (src_bit_width, dest_bit_width) = (src_width.bit_width(self.read().arch), dest_width.bit_width(self.read().arch));
                        let value = self.build_expr(b, operand, Context::default(), tp);
                        let value = self.read().handle_indirection(b, value);

                        match src_bit_width.cmp(&dest_bit_width) {
                            Ordering::Less => if dest_is_signed {
                                // TODO: Bounds checking
                                self.read().push_instr(b, InstrKind::SignExtend(value, dest_ty), expr)
                            } else {
                                // TODO: Bounds checking
                                self.read().push_instr(b, InstrKind::ZeroExtend(value, dest_ty), expr)
                            },
                            Ordering::Equal => {
                                // TODO: Bounds checking
                                self.read().push_instr(b, InstrKind::Reinterpret(value, dest_ty), expr)
                            },
                            Ordering::Greater => {
                                // TODO: Bounds checking
                                self.read().push_instr(b, InstrKind::Truncate(value, dest_ty), expr)
                            },
                        }.direct()
                    },
                    CastMethod::Float => {
                        let value = self.build_expr(b, operand, Context::default(), tp);
                        let value = self.read().handle_indirection(b, value);
                        self.read().push_instr(b, InstrKind::FloatCast(value, dest_ty), expr).direct()
                    },
                    CastMethod::FloatToInt => {
                        let value = self.build_expr(b, operand, Context::default(), tp);
                        let value = self.read().handle_indirection(b, value);
                        self.read().push_instr(b, InstrKind::FloatToInt(value, dest_ty), expr).direct()
                    },
                    CastMethod::IntToFloat => {
                        let value = self.build_expr(b, operand, Context::default(), tp);
                        let value = self.read().handle_indirection(b, value);
                        self.read().push_instr(b, InstrKind::IntToFloat(value, dest_ty), expr).direct()
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
                let instr = self.read().handle_indirection(b, val);
                self.read().push_instr(b, InstrKind::Pointer { instr, is_mut }, expr).direct()
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
                        self.read().handle_indirection(b, param_ty)
                    }).collect();
                let ret_ty = self.build_expr(
                    b,
                    ret_ty,
                    Context::default(),
                    tp,
                );
                let ret_ty = self.read().handle_indirection(b, ret_ty);

                self.read().push_instr(b, InstrKind::FunctionTy { param_tys, has_c_variadic_param, ret_ty }, expr).direct()
            }
            Expr::Struct(id) => {
                let mut fields = SmallVec::new();
                let len = self.read().ast.structs[id].fields.len();
                for i in 0..len {
                    let field_ty = self.read().ast.structs[id].fields[i].ty;
                    let field = self.build_expr(
                        b,
                        field_ty,
                        Context::default(),
                        tp,
                    );
                    let field = self.read().handle_indirection(b, field);
                    fields.push(field);
                }
                self.read().push_instr(b, InstrKind::Struct { fields, id }, expr).direct()
            },
            Expr::Enum(id) => {
                let mut variants = SmallVec::new();
                let len = self.read().ast.enums[id].variants.len();
                for i in 0..len {
                    let payload_ty = self.read().ast.enums[id].variants[i].payload_ty.unwrap_or(VOID_TYPE);
                    let variant = self.build_expr(
                        b,
                        payload_ty,
                        Context::default(),
                        tp,
                    );
                    let variant = self.read().handle_indirection(b, variant);
                    variants.push(variant);
                }
                self.read().push_instr(b, InstrKind::Enum { variants, id }, expr).direct()
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
                    let field = self.read().handle_indirection(b, field);
                    fields.push(field);
                }
                self.read().push_instr(b, InstrKind::StructLit { fields, id: lit.strukt }, expr).direct()
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
                let post_bb = self.read().create_bb(b);
                let scope_ctx = ctx.redirect(post_bb, pass_value_as_argument);
                let decision_tree = tp.switch_expr_decision_tree(pattern_matching_ctx_id).as_ref().expect("should always set decision tree on switch expr");
                let pattern_matching_ctx = tp.pattern_matching_context(pattern_matching_ctx_id).as_ref().expect("should always set pattern matching context on switch expr");
                let mut scope_blocks = HashMap::<ImperScopeId, BlockId>::new();
                self.handle_pattern_matching(b, expr, scope_ctx, tp, scrutinee, decision_tree, pattern_matching_ctx_id, pattern_matching_ctx, &mut scope_blocks);

                self.read().start_bb(b, post_bb);
                if pass_value_as_argument {
                    // TODO: this will be the wrong type if indirection != 0
                    self.read().push_instr(b, InstrKind::Parameter(ty), expr).direct()
                } else {
                    let void_instr = self.read().push_instr(b, InstrKind::Void, expr);
                    self.read().handle_control(b, void_instr.direct(), ctx.control)
                }
            },
            Expr::While { loop_id, condition, scope } => {
                let test_bb = self.read().create_bb(b);
                let loop_bb = self.read().create_bb(b);
                let post_bb = match ctx.control {
                    ControlDest::Continue | ControlDest::Unreachable | ControlDest::RetVoid | ControlDest::IncrementVariableAndThenJump { .. } => self.read().create_bb(b),
                    ControlDest::Jump(block) => block,
                };

                self.read().push_instr(b, InstrKind::Jump(test_bb.into()), expr);
                self.read().end_current_bb(b);
                self.read().start_bb(b, test_bb);
                self.build_expr(b, condition, Context::new(0, DataDest::Branch(loop_bb, post_bb), ControlDest::Continue), tp);

                self.read().start_bb(b, loop_bb);
                let loop_state = LoopState {
                    break_block: post_bb,
                    continue_block: test_bb,
                    continue_location_of_variable_to_increment: None,
                };
                b.loops.push_at(loop_id, loop_state);
                self.build_scope(b, scope, Context::new(0, DataDest::Void, ControlDest::Jump(test_bb)), tp);

                match ctx.control {
                    ControlDest::Continue | ControlDest::Unreachable | ControlDest::RetVoid | ControlDest::IncrementVariableAndThenJump { .. } => {
                        self.read().start_bb(b, post_bb);
                        self.read().push_instr(b, InstrKind::Void, expr).direct()
                    },
                    // Already handled this above
                    ControlDest::Jump(_) => return self.read().push_instr(b, InstrKind::Void, expr).direct(),
                }
            },
            Expr::For { loop_id, binding, lower_bound, upper_bound, scope } => {
                let d = self.read();
                let ast::Decl::LoopBinding { id: binding_stored_decl_id, .. } = df!(d, binding.ast) else {
                    panic!("incorrect type of decl found in decl binding id");
                };
                drop(d);
                let binding_ty = tp.ty(lower_bound).clone();
                let binding_name = self.read().display_item(&b.source_ranges, binding).to_string();
                let binding_location = self.read().push_instr_with_name(b, InstrKind::Alloca(binding_ty), binding, &binding_name);
                b.stored_decl_locs.push_at(binding_stored_decl_id, binding_location);
                let val = self.build_expr(b, lower_bound, Context::new(0, DataDest::Read, ControlDest::Continue), tp);
                let instr = self.read().handle_indirection(b, val);
                let range = self.read().get_range_with_mir_ctx(binding_location, &b.source_ranges) + self.read().get_range_with_mir_ctx(instr, &b.source_ranges);
                self.read().push_instr(b, InstrKind::Store { location: binding_location, value: instr }, range);
                let upper_bound = self.build_expr(b, upper_bound, Context::default(), tp);
                let upper_bound = self.read().handle_indirection(b, upper_bound);

                let test_bb = self.read().create_bb(b);
                let loop_bb = self.read().create_bb(b);
                let post_bb = match ctx.control {
                    ControlDest::Continue | ControlDest::Unreachable | ControlDest::RetVoid | ControlDest::IncrementVariableAndThenJump { .. } => self.read().create_bb(b),
                    ControlDest::Jump(target) => target,
                };

                self.read().push_instr(b, InstrKind::Jump(test_bb.into()), expr);
                self.read().end_current_bb(b);
                self.read().start_bb(b, test_bb);
                let cur_binding_value = self.read().push_instr_with_name(b, InstrKind::Load(binding_location), binding, &binding_name);
                let less_than = self.read().push_instr_with_name(b, InstrKind::LegacyIntrinsic { arguments: smallvec![cur_binding_value, upper_bound], ty: Type::Bool, intr: LegacyIntrinsic::Less }, binding, &binding_name);
                self.read().push_instr_with_name(b, InstrKind::CondBr { condition: less_than, true_target: loop_bb.into(), false_target: post_bb.into() }, binding, &binding_name);
                self.read().end_current_bb(b);

                self.read().start_bb(b, loop_bb);
                let loop_state = LoopState {
                    break_block: post_bb,
                    continue_block: test_bb,
                    continue_location_of_variable_to_increment: Some(binding_location),
                };
                b.loops.push_at(loop_id, loop_state);
                self.build_scope(b, scope, Context::new(0, DataDest::Void, ControlDest::IncrementVariableAndThenJump { location: binding_location, target: test_bb }), tp);

                match &ctx.control {
                    ControlDest::Continue | ControlDest::Unreachable | ControlDest::RetVoid | ControlDest::IncrementVariableAndThenJump { .. } => {
                        self.read().start_bb(b, post_bb);
                        self.read().push_instr(b, InstrKind::Void, expr).direct()
                    },
                    // Already handled this above
                    ControlDest::Jump(_) => return self.read().push_instr(b, InstrKind::Void, expr).direct(),
                }
            },
            Expr::Break(loop_id) => {
                let loop_id = loop_id.expect("loop id should be filled in by MIR generation time");
                let loop_state = b.loops[loop_id].clone();
                let branch = self.read().push_instr(b, InstrKind::Jump(loop_state.break_block.into()), expr);
                self.read().end_current_bb(b);
                // Must create unreachable basic block in case there are more statements in this loop
                let unreachable_bb = self.read().create_bb(b);
                self.read().start_bb(b, unreachable_bb);

                return branch.direct();
            },
            Expr::Continue(loop_id) => {
                let loop_id = loop_id.expect("loop id should be filled in by MIR generation time");
                let loop_state = b.loops[loop_id].clone();
                if let Some(variable_to_increment) = loop_state.continue_location_of_variable_to_increment {
                    self.read().increment_variable(b, variable_to_increment);
                }
                let branch = self.read().push_instr(b, InstrKind::Jump(loop_state.continue_block.into()), expr);
                self.read().end_current_bb(b);
                // Must create unreachable basic block in case there are more statements in this loop
                let unreachable_bb = self.read().create_bb(b);
                self.read().start_bb(b, unreachable_bb);

                return branch.direct();
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

    fn get_scrutinee_value(&self, b: &mut FunctionBuilder, tp: &dyn TypeProvider, og_scrutinee: ExprId, scrutinee: SwitchScrutineeValueId, pattern_matching_ctx: &IndexVec<SwitchScrutineeValueId, TypedSwitchScrutineeValue>, pattern_matching_ctx_id: PatternMatchingContextId) -> Value {
        let scrutinee_values = b.pattern_matching_scrutinees.entry(pattern_matching_ctx_id).or_default();
        if let Some(scrutinee) = scrutinee_values.get(&scrutinee) {
            return *scrutinee;
        }

        let scrutinee_value = &pattern_matching_ctx[scrutinee];
        let value = match scrutinee_value.kind {
            TypedSwitchScrutineeValueKind::EnumPayload { enum_value, variant_index } => {
                let val = self.get_scrutinee_value(b, tp, og_scrutinee, enum_value, pattern_matching_ctx, pattern_matching_ctx_id);
                let val = self.read().handle_indirection(b, val);
                self.read().push_instr(b, InstrKind::PayloadAccess { val, variant_index }, og_scrutinee).direct()
            },
            TypedSwitchScrutineeValueKind::OriginalScrutinee => {
                self.build_expr(b, og_scrutinee, Context::new(0, DataDest::Read, ControlDest::Continue), tp)
            },
            TypedSwitchScrutineeValueKind::VoidValue => self.read().push_instr(b, InstrKind::Void, og_scrutinee).direct(),
        };
        let scrutinee_values = b.pattern_matching_scrutinees.get_mut(&pattern_matching_ctx_id).expect("just inserted entry above");
        scrutinee_values.insert(scrutinee, value);

        value
    }

    fn handle_pattern_matching(&self, b: &mut FunctionBuilder, expr: ExprId, ctx: Context, tp: &dyn TypeProvider, og_scrutinee: ExprId, node: &SwitchDecisionNode, pattern_matching_ctx_id: PatternMatchingContextId, pattern_matching_ctx: &IndexVec<SwitchScrutineeValueId, TypedSwitchScrutineeValue>, scope_blocks: &mut HashMap<ImperScopeId, BlockId>) {
        match *node {
            SwitchDecisionNode::Branch { scrutinee: scrutinee_id, ref paths, ref default_path } => {
                let begin_bb = b.current_block;
                self.read().start_bb(b, begin_bb);

                let scrutinee_val = self.get_scrutinee_value(b, tp, og_scrutinee, scrutinee_id, pattern_matching_ctx, pattern_matching_ctx_id);
                // TODO: support pattern matching of pointers
                let scrutinee = self.read().handle_indirection(b, scrutinee_val);
                let scrutinee_ty = b.type_of(scrutinee).clone();

                let mut discriminant_enum_ty = None;
                let discriminant: InstrId = match scrutinee_ty {
                    Type::Enum(ref enum_ty) => {
                        discriminant_enum_ty = Some(enum_ty.clone());
                        self.read().get_discriminant(b, Value { instr: scrutinee, indirection: 0 })
                    }
                    Type::Int { .. } => scrutinee,
                    ty => todo!("Pattern matching for {:?}", ty),
                };

                let mut mir_cases = Vec::new();
                let mut nodes = Vec::new();
                for (value, node) in paths {
                    let const_value = match *value {
                        SwitchDecisionValue::EnumVariant(variant_index) => self.read().get_const_discriminant(discriminant_enum_ty.clone().expect("Must be enum type").identity, variant_index),
                        SwitchDecisionValue::SignedInt(value) => Const::Int { lit: BigInt::from(value), ty: scrutinee_ty.clone() },
                        SwitchDecisionValue::UnsignedInt(value) => Const::Int { lit: BigInt::from(value), ty: scrutinee_ty.clone() },
                    };

                    let case_bb = self.read().create_bb(b);
                    mir_cases.push(SwitchCase { value: const_value, target: case_bb.into() });
                    nodes.push(node);
                }

                let default_bb = self.read().create_bb(b);

                self.read().push_instr(b, InstrKind::SwitchBr { scrutinee: discriminant, cases: mir_cases.clone(), catch_all_target: default_bb.into() }, expr);
                self.read().end_current_bb(b);

                for (mir_case, node) in mir_cases.iter().zip(nodes) {
                    self.read().start_bb(b, mir_case.target.bb);
                    self.handle_pattern_matching(b, expr, ctx, tp, og_scrutinee, node, pattern_matching_ctx_id, pattern_matching_ctx, scope_blocks);
                }

                self.read().start_bb(b, default_bb);
                if let Some(default_path) = &default_path {
                    self.handle_pattern_matching(b, expr, ctx, tp, og_scrutinee, default_path, pattern_matching_ctx_id, pattern_matching_ctx, scope_blocks);
                } else {
                    // TODO: add unreachable instruction I guess?
                    self.read().push_instr(b, InstrKind::LegacyIntrinsic { arguments: SmallVec::new(), ty: Type::Never, intr: LegacyIntrinsic::Panic }, SourceRange::default());
                    self.read().end_current_bb(b);
                }
            },
            SwitchDecisionNode::Destination { destination, .. } => {
                self.build_scope(b, destination, ctx, tp);
            },
            SwitchDecisionNode::Failure => {
                self.read().diag.report_error_no_range_msg("Pattern matching failure", expr);
                self.read().push_instr(b, InstrKind::LegacyIntrinsic { arguments: SmallVec::new(), ty: Type::Never, intr: LegacyIntrinsic::Panic }, expr);
                self.read().end_current_bb(b);
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

    fn get_discriminant(&self, b: &mut FunctionBuilder, val: Value) -> InstrId {
        // TODO: handle indirect enum discriminant accesses, without loading the entire value.
        let val = self.handle_indirection(b, val);
        self.push_instr_with_name(b, InstrKind::DiscriminantAccess { val }, val, format!("{}.disc", self.display_instr_name_from_builder(b, val)))
    }

    fn handle_indirection(&self, b: &mut FunctionBuilder, mut val: Value) -> InstrId {
        if val.indirection > 0 {
            while val.indirection > 0 {
                val.instr = self.push_instr(b, InstrKind::Load(val.instr), val.instr);
                val.indirection -= 1;
            }
        } else if val.indirection < 0 {
            let mut ty = b.type_of(val.instr).clone();
            while val.indirection < 0 {
                let location = self.push_instr(b, InstrKind::Alloca(ty.clone()), val.instr);
                self.push_instr(b, InstrKind::Store { location, value: val.instr }, val.instr);
                val.instr = location;
                val.indirection += 1;
                // Mutability doesn't matter for now
                ty = ty.mut_ptr();
            }
        }
        val.instr
    }

    fn increment_variable(&self, b: &mut FunctionBuilder, location: InstrId) {
        let ty = b.type_of(location).deref().unwrap().clone().ty;
        let loaded = self.push_instr(b, InstrKind::Load(location), location);
        let one = self.push_instr(b, InstrKind::Const(Const::Int { lit: BigInt::from(1), ty: ty.clone() }), location);
        let value = self.push_instr(b, InstrKind::LegacyIntrinsic { arguments: smallvec![loaded, one], ty, intr: LegacyIntrinsic::Add }, location);
        self.push_instr(b, InstrKind::Store { location, value }, location);
    }

    fn handle_control(&self, b: &mut FunctionBuilder, val: Value, control: ControlDest) -> Value {
        match control {
            ControlDest::Jump(target) => {
                let val: Value = self.push_instr(b, InstrKind::Jump(target.into()), val.instr).direct();
                self.end_current_bb(b);
                val
            },
            ControlDest::IncrementVariableAndThenJump { location, target } => {
                // *location += 1
                self.increment_variable(b, location);

                // br block
                let val = self.push_instr(b, InstrKind::Jump(target.into()), val.instr).direct();
                self.end_current_bb(b);
                val
            },
            ControlDest::Continue => val,
            ControlDest::RetVoid => {
                let void_instr = self.push_instr(b, InstrKind::Void, val.instr);
                let val = self.push_instr(b, InstrKind::Ret(void_instr), val.instr).direct();
                self.end_current_bb(b);
                val
            },
            ControlDest::Unreachable => self.push_instr(b, InstrKind::Void, val.instr).direct(),
        }
    }
}

impl DriverRwRef<'_> {
    fn handle_context(&self, b: &mut FunctionBuilder, mut val: Value, ctx: Context) -> Value {
        val = val.adjusted(ctx.indirection);
        match ctx.data {
            DataDest::Read => return val,
            DataDest::Ret => {
                let instr = self.read().handle_indirection(b, val);
                let val = self.read().push_instr(b, InstrKind::Ret(instr), instr).direct();
                self.read().end_current_bb(b);
                return val;
            },
            DataDest::JumpWithArgument(bb) => {
                let instr = self.read().handle_indirection(b, val);
                let val = self.read().push_instr(b, InstrKind::Jump(JumpTarget { bb, arguments: smallvec![instr] }), instr).direct();
                self.read().end_current_bb(b);
                return val;
            },
            DataDest::Branch(true_bb, false_bb) => {
                let instr_id = self.read().handle_indirection(b, val);
                let instr = if let &InstrKind::Const(Const::Bool(val)) = &b.instrs[instr_id].kind {
                    let bb = if val {
                        true_bb
                    } else {
                        false_bb
                    };
                    InstrKind::Jump(bb.into())
                } else {
                    InstrKind::CondBr { condition: instr_id, true_target: true_bb.into(), false_target: false_bb.into() }
                };
                let val = self.read().push_instr(b, instr, instr_id).direct();
                self.read().end_current_bb(b);
                return val;
            },
            DataDest::Receive { value } => {
                let location = self.read().handle_indirection(b, val.get_address());
                let range = self.read().get_range_with_mir_ctx(location, &b.source_ranges) + self.read().get_range_with_mir_ctx(value, &b.source_ranges);
                self.read().push_instr(b, InstrKind::Store { location, value }, range);
            },
            DataDest::Void => {},
        }

        self.read().handle_control(b, val, ctx.control)
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
