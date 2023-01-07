use std::collections::HashMap;
use std::ffi::CString;

use index_vec::{IndexVec, define_index_type};
use smallvec::SmallVec;
use string_interner::DefaultSymbol as Sym;
use display_adapter::display_adapter;
use num_bigint::BigInt;

use crate::dire::hir::{LegacyIntrinsic, DeclId, StructId, EnumId, ExternModId, ExternFunctionRef, GenericParamId};
use crate::dire::ty::{Type, LegacyInternalType, FunctionType, StructType};
use crate::dire::{Code, BlockId, OpId, InternalField};
use crate::dire::source_info::SourceRange;

use crate::dire::hir::IntrinsicId;

use crate::dire::hir::NewNamespaceId;

define_index_type!(pub struct FuncId = u32;);
define_index_type!(pub struct StaticId = u32;);
define_index_type!(pub struct StrId = u32;);
define_index_type!(pub struct InstrId = u32;);

pub const VOID_INSTR: OpId = OpId::from_usize_unchecked(0);

#[derive(Clone, Debug, PartialEq)]
pub struct SwitchCase {
    pub value: Const,
    pub bb: BlockId,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Instr {
    Void,
    Invalid, // Used temporarily when copying functions
    Const(Const),
    Alloca(Type),
    LogicalNot(OpId),
    Call { arguments: SmallVec<[OpId; 2]>, generic_arguments: Vec<Type>, func: FuncId },
    ExternCall { arguments: SmallVec<[OpId; 2]>, func: ExternFunctionRef },
    FunctionRef { generic_arguments: Vec<Type>, func: FuncId, },
    LegacyIntrinsic { arguments: SmallVec<[OpId; 2]>, ty: Type, intr: LegacyIntrinsic },
    Intrinsic { arguments: SmallVec<[OpId; 2]>, intr: IntrinsicId },
    Reinterpret(OpId, Type),
    Truncate(OpId, Type),
    SignExtend(OpId, Type),
    ZeroExtend(OpId, Type),
    FloatCast(OpId, Type),
    FloatToInt(OpId, Type),
    IntToFloat(OpId, Type),
    Load(OpId),
    Store { location: OpId, value: OpId },
    AddressOfStatic(StaticId),
    Pointer { op: OpId, is_mut: bool },
    Struct { fields: SmallVec<[OpId; 2]>, id: StructId },
    Enum { variants: SmallVec<[OpId; 2]>, id: EnumId },
    FunctionTy { param_tys: Vec<OpId>, ret_ty: OpId },
    StructLit { fields: SmallVec<[OpId; 2]>, id: StructId },
    DirectFieldAccess { val: OpId, index: usize },
    IndirectFieldAccess { val: OpId, index: usize },
    InternalFieldAccess { val: OpId, field: InternalField },
    Variant { enuum: EnumId, index: usize, payload: OpId },
    DiscriminantAccess { val: OpId },
    Ret(OpId),
    Br(BlockId),
    CondBr { condition: OpId, true_bb: BlockId, false_bb: BlockId },
    SwitchBr { scrutinee: OpId, cases: Vec<SwitchCase>, catch_all_bb: BlockId },
    GenericParam(GenericParamId),
    /// Only valid at the beginning of a function, right after the void instruction
    // TODO: Get rid of the type here! It is no longer required because instruction types are now stored on each Op
    Parameter(Type),
}

impl Instr {
    pub fn replace_bb(&mut self, old: BlockId, new: BlockId) {
        fn replace(target: &mut BlockId, old: BlockId, new: BlockId) {
            if *target == old {
                *target = new;
            }
        }
        match self {
            Instr::Br(bb) => replace(bb, old, new),
            Instr::CondBr { true_bb, false_bb, .. } => {
                replace(true_bb, old, new);
                replace(false_bb, old, new);
            },
            Instr::SwitchBr { cases, catch_all_bb, .. } => {
                for case in cases {
                    replace(&mut case.bb, old, new);
                }
                replace(catch_all_bb, old, new);
            },
            _ => {}
        }
    }

    // TODO: allocating a Vec here sucks!
    pub fn referenced_values(&self) -> Vec<OpId> {
        match *self {
            Instr::Void | Instr::Const(_) | Instr::Alloca(_) | Instr::AddressOfStatic(_) | Instr::Br(_)
                | Instr::GenericParam(_) | Instr::Parameter(_) | Instr::FunctionRef { .. } | Instr::Invalid => vec![],
            Instr::LogicalNot(op) | Instr::Reinterpret(op, _) | Instr::Truncate(op, _) | Instr::SignExtend(op, _)
                | Instr::ZeroExtend(op, _) | Instr::FloatCast(op, _) | Instr::FloatToInt(op, _)
                | Instr::IntToFloat(op, _) | Instr::Load(op) | Instr::Pointer { op, .. }
                | Instr::DirectFieldAccess { val: op, .. } | Instr::IndirectFieldAccess { val: op, .. }
                | Instr::DiscriminantAccess { val: op } | Instr::Ret(op) | Instr::CondBr { condition: op, .. }
                | Instr::SwitchBr { scrutinee: op, .. } | Instr::Variant { payload: op, .. }
                | Instr::InternalFieldAccess { val: op, .. } => vec![op],
            Instr::Store { location, value } => vec![location, value],
            Instr::Call { arguments: ref ops, .. } | Instr::ExternCall { arguments: ref ops, .. }
                | Instr::LegacyIntrinsic { arguments: ref ops, .. } | Instr::Struct { fields: ref ops, .. }
                | Instr::Enum { variants: ref ops, .. } | Instr::StructLit { fields: ref ops, .. }
                | Instr::Intrinsic { arguments: ref ops, .. } => ops.iter().copied().collect(),
            Instr::FunctionTy { ref param_tys, ret_ty } => param_tys.iter().copied().chain(std::iter::once(ret_ty)).collect(),
        }
    }

    pub fn references_value(&self, val: OpId) -> bool {
        self.referenced_values().iter().any(|&referenced| referenced == val)
    }

    pub fn replace_value(&mut self, old: OpId, new: OpId) {
        fn replace(target: &mut OpId, old: OpId, new: OpId) {
            if *target == old {
                *target = new;
            }
        }
        match self {
            Instr::Void | Instr::Const(_) | Instr::Alloca(_) | Instr::AddressOfStatic(_) | Instr::Br(_)
                | Instr::GenericParam(_) | Instr::Parameter(_) | Instr::FunctionRef { .. } | Instr::Invalid => {},
            Instr::LogicalNot(op) | Instr::Reinterpret(op, _) | Instr::Truncate(op, _) | Instr::SignExtend(op, _)
                | Instr::ZeroExtend(op, _) | Instr::FloatCast(op, _) | Instr::FloatToInt(op, _)
                | Instr::IntToFloat(op, _) | Instr::Load(op) | Instr::Pointer { op, .. }
                | Instr::DirectFieldAccess { val: op, .. } | Instr::IndirectFieldAccess { val: op, .. }
                | Instr::DiscriminantAccess { val: op } | Instr::Ret(op) | Instr::CondBr { condition: op, .. }
                | Instr::SwitchBr { scrutinee: op, .. } | Instr::Variant { payload: op, .. }
                | Instr::InternalFieldAccess { val: op, .. } => replace(op, old, new),
            Instr::Store { location, value } => {
                replace(location, old, new);
                replace(value, old, new);
            },
            Instr::Call { arguments: ref mut ops, .. } | Instr::ExternCall { arguments: ref mut ops, .. }
                | Instr::LegacyIntrinsic { arguments: ref mut ops, .. } | Instr::Struct { fields: ref mut ops, .. }
                | Instr::Enum { variants: ref mut ops, .. } | Instr::StructLit { fields: ref mut ops, .. }
                | Instr::Intrinsic { arguments: ref mut ops, .. } => {
                    for op in ops {
                        replace(op, old, new);
                    }
                }
            Instr::FunctionTy { ref mut param_tys, ref mut ret_ty } => {
                for op in param_tys {
                    replace(op, old, new);
                }
                replace(ret_ty, old, new);
            }
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
    BasicVariant { enuum: EnumId, index: usize },
    StructLit { fields: Vec<Const>, id: StructId },
    Void,
}

impl Const {
    pub fn ty(&self) -> Type {
        match self {
            Const::Int { ty, .. } | Const::Float { ty, .. } | Const::Str { ty, .. } => ty.clone(),
            Const::StrLit(_) => Type::LegacyInternal(LegacyInternalType::StringLiteral),
            Const::Bool(_) => Type::Bool,
            Const::Ty(_) => Type::Ty,
            &Const::BasicVariant { enuum, .. } => Type::Enum(enuum),
            Const::Mod(_) => Type::Mod,
            &Const::StructLit { id, ref fields } => Type::Struct(
                StructType {
                    field_tys: fields.iter().map(|field| field.ty()).collect(),
                    identity: id,
                }
            ),
            Const::Void => Type::Void,
        }
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

#[derive(Debug, Default, Clone)]
pub struct Function {
    pub name: Option<Sym>,
    pub ty: FunctionType,
    pub num_instrs: usize,
    /// Index 0 is defined to be the entry block
    pub blocks: Vec<BlockId>,
    pub decl: Option<DeclId>,
    // Note: Is a Vec, not a Range, because generic params might not always be contiguous in
    // GenericParamId space
    pub generic_params: Vec<GenericParamId>,
    pub instr_namespace: InstrNamespace,
    pub is_comptime: bool,
}

impl Code {
    pub fn num_parameters(&self, func: &Function) -> usize {
        let entry = func.blocks[0];
        let block = &self.blocks[entry];
        block.ops.iter()
            .filter(|&&op| matches!(self.ops[op].as_mir_instr().unwrap(), Instr::Parameter(_)))
            .count()
    }

    #[display_adapter]
    pub fn display_func(&self, func: &Function, name: &str, w: &mut Formatter) {
        writeln!(w, "fn {}() {{", name)?;
        for &block in &func.blocks {
            write!(w, "%bb{}:\n{}", block.index(), self.display_block(block))?;
        }
        writeln!(w, "}}")?;
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

pub struct MirCode {
    pub strings: IndexVec<StrId, CString>,
    pub functions: IndexVec<FuncId, Function>,
    pub statics: IndexVec<StaticId, Static>,
    pub extern_mods: HashMap<ExternModId, ExternMod>,
    pub enums: HashMap<EnumId, EnumLayout>,
    pub source_ranges: HashMap<OpId, SourceRange>,
    pub instr_names: HashMap<OpId, String>,
    block_states: HashMap<BlockId, BlockState>,
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

impl MirCode {
    pub fn new() -> Self {
        MirCode {
            strings: IndexVec::new(),
            functions: IndexVec::new(),
            statics: IndexVec::new(),
            extern_mods: HashMap::new(),
            enums: HashMap::new(),
            source_ranges: HashMap::new(),
            instr_names: HashMap::new(),
            block_states: HashMap::new(),
        }
    }

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

    pub fn first_unended_block(&self, func: &Function) -> Option<BlockId> {
        func.blocks.iter().find(|&block| {
            let state = &self.block_states[block];
            !matches!(state, BlockState::Ended)
        }).copied()
    }

    pub fn check_all_blocks_ended(&self, func: &Function) {
        if let Some(block) = self.first_unended_block(func) {
            panic!("MIR: Block {} was not ended", block.index());
        }
    }
}

impl Default for MirCode {
    fn default() -> Self { Self::new() }
}