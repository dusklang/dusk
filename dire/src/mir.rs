use std::collections::HashMap;
use std::ffi::CString;

use index_vec::{IndexVec, define_index_type};
use smallvec::SmallVec;
use string_interner::DefaultSymbol as Sym;
use display_adapter::display_adapter;
use num_bigint::BigInt;

use crate::hir::{Intrinsic, DeclId, StructId, EnumId, ModScopeId, ExternModId, ExternFunctionRef, GenericParamId};
use crate::ty::Type;
use crate::{Code, BlockId, OpId};
use crate::source_info::SourceRange;

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
    Const(Const),
    Alloca(Type),
    LogicalNot(OpId),
    Call { arguments: SmallVec<[OpId; 2]>, generic_arguments: Vec<Type>, func: FuncId },
    ExternCall { arguments: SmallVec<[OpId; 2]>, func: ExternFunctionRef },
    FunctionRef { generic_arguments: Vec<Type>, func: FuncId, },
    Intrinsic { arguments: SmallVec<[OpId; 2]>, ty: Type, intr: Intrinsic },
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

#[derive(Clone, Debug, PartialEq)]
pub enum Const {
    Int { lit: BigInt, ty: Type },
    Float { lit: f64, ty: Type },
    Str { id: StrId, ty: Type },
    Bool(bool),
    Ty(Type),
    Mod(ModScopeId),
    BasicVariant { enuum: EnumId, index: usize },
    StructLit { fields: Vec<Const>, id: StructId },
    Void,
}

impl Const {
    pub fn ty(&self) -> Type {
        match self {
            Const::Int { ty, .. } | Const::Float { ty, .. } | Const::Str { ty, .. } => ty.clone(),
            Const::Bool(_) => Type::Bool,
            Const::Ty(_) => Type::Ty,
            &Const::BasicVariant { enuum, .. } => Type::Enum(enuum),
            Const::Mod(_) => Type::Mod,
            &Const::StructLit { id, .. } => Type::Struct(id),
            Const::Void => Type::Void,
        }
    }
}

#[derive(Default, Debug)]
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

#[derive(Debug, Default)]
pub struct Function {
    pub name: Option<Sym>,
    pub ret_ty: Type,
    pub func_ty: Type,
    pub num_instrs: usize,
    /// Index 0 is defined to be the entry block
    pub blocks: Vec<BlockId>,
    pub decl: Option<DeclId>,
    // Note: Is a Vec, not a Range, because generic params might not always be contiguous in
    // GenericParamId space
    pub generic_params: Vec<GenericParamId>,
    pub instr_namespace: InstrNamespace,
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
pub struct Struct {
    pub field_tys: SmallVec<[Type; 2]>,
    pub layout: StructLayout,
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
    pub param_tys: Vec<Type>,
    pub return_ty: Type,
}

pub struct MirCode {
    pub strings: IndexVec<StrId, CString>,
    pub functions: IndexVec<FuncId, Function>,
    pub statics: IndexVec<StaticId, Static>,
    pub extern_mods: HashMap<ExternModId, ExternMod>,
    pub structs: HashMap<StructId, Struct>,
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
            structs: HashMap::new(),
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