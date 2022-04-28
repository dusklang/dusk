use std::ffi::CString;
use std::collections::HashMap;
use std::ops::Range;

use index_vec::{IndexVec, define_index_type};
use smallvec::SmallVec;
use string_interner::DefaultSymbol as Sym;

use crate::ty::Type;
use crate::index_counter::IndexCounter;
use crate::source_info::{SourceRange, SourceFileId};
use crate::BlockId;

define_index_type!(pub struct ExprId = u32;);
define_index_type!(pub struct DeclRefId = u32;);
define_index_type!(pub struct ImperScopeId = u32;);
define_index_type!(pub struct CastId = u32;);
define_index_type!(pub struct DeclId = u32;);
define_index_type!(pub struct ItemId = u32;);
define_index_type!(pub struct ModScopeId = u32;);
define_index_type!(pub struct StructId = u32;);
define_index_type!(pub struct StructLitId = u32;);
define_index_type!(pub struct EnumId = u32;);
define_index_type!(pub struct StoredDeclId = u32;);
define_index_type!(pub struct PatternBindingDeclId = u32;);
define_index_type!(pub struct ImperScopeNsId = u32;);
define_index_type!(pub struct ModScopeNsId = u32;);
define_index_type!(pub struct ConditionNsId = u32;);
define_index_type!(pub struct CompDeclParamsNsId = u32;);
define_index_type!(pub struct GenericParamId = u32;);
define_index_type!(pub struct ExternModId = u32;);

#[derive(Debug, Clone, Copy)]
pub struct FieldAssignment {
    pub name: Sym,
    pub expr: ExprId,
}

/// A declaration in local (imperative) scope
#[derive(Debug, Clone, Copy)]
pub struct ImperScopedDecl {
    pub name: Sym,
    pub num_params: usize,
    pub id: DeclId,
}

/// A declaration in module scope
#[derive(Debug)]
pub struct ModScopedDecl {
    pub num_params: usize,
    pub id: DeclId,
}

#[derive(Debug)]
pub struct ImperScopeNs {
    pub decls: Vec<ImperScopedDecl>,
    pub parent: Option<Namespace>,
}

#[derive(Debug)]
pub struct ModScopeNs {
    pub scope: ModScopeId,
    pub parent: Option<Namespace>,
}

#[derive(Debug)]
pub struct ConditionNs {
    /// The function that this condition namespace refers to
    /// NOTE: updated to the correct value after parsing the function
    pub func: DeclId,
    pub parent: Option<Namespace>,
}

#[derive(Debug)]
pub struct CompDeclParamsNs {
    /// The computed decl that this comp decl params namespace refers to
    /// NOTE: updated to the correct value after parsing the function
    pub func: DeclId,
    pub parent: Option<Namespace>,
}

#[derive(Debug)]
pub struct ImperScope {
    pub block: BlockId,
    pub terminal_expr: ExprId,
}

#[derive(Debug, Copy, Clone)]
pub enum Namespace {
    Imper { scope: ImperScopeNsId, end_offset: usize },
    Mod(ModScopeNsId),
    MemberRef { base_expr: ExprId, },
    CompDeclParams(CompDeclParamsNsId),

    /// Includes the parameters of the function
    Requirement(ConditionNsId),
    /// Includes the parameters of the function, and a magic "return_value" value
    Guarantee(ConditionNsId),
}

#[derive(Debug)]
pub struct DeclRef {
    pub name: Sym,
    pub namespace: Namespace,
    pub num_arguments: usize,
    pub has_parens: bool,
    pub expr: ExprId,
}

#[derive(Debug)]
pub struct ExternMod {
    pub library_path: CString,
    pub imported_functions: Vec<ExternFunction>,
}

impl ExternMod {
    pub fn new(library_path: CString) -> Self {
        Self {
            library_path,
            imported_functions: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct ExternFunction {
    pub name: String,
    pub param_tys: Vec<ExprId>,
    pub return_ty: ExprId,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct ExternFunctionRef {
    pub extern_mod: ExternModId,
    pub index: usize,
}

#[derive(Debug)]
pub enum Expr {
    Void,
    Error,
    IntLit { lit: u64 },
    DecLit { lit: f64 },
    StrLit { lit: CString },
    CharLit { lit: i8 },
    BoolLit { lit: bool },
    ConstTy(Type),
    DeclRef { arguments: SmallVec<[ExprId; 2]>, id: DeclRefId },
    AddrOf { expr: ExprId, is_mut: bool },
    /// Transforms type into pointer type
    Pointer { expr: ExprId, is_mut: bool },
    Deref(ExprId),
    Set { lhs: ExprId, rhs: ExprId },
    Do { scope: ImperScopeId },
    If { condition: ExprId, then_scope: ImperScopeId, else_scope: Option<ImperScopeId> },
    While { condition: ExprId, scope: ImperScopeId },
    Switch {
        scrutinee: ExprId,
        cases: Vec<SwitchCase>,
    },
    Cast { expr: ExprId, ty: ExprId, cast_id: CastId },
    Ret { expr: ExprId, decl: Option<DeclId> },
    Mod { id: ModScopeId },
    Import { file: SourceFileId },
    Struct(StructId),
    Enum(EnumId),
    StructLit {
        ty: ExprId,
        fields: Vec<FieldAssignment>,
        id: StructLitId,
    },
}

#[derive(Copy, Clone, Debug)]
pub enum Item {
    Expr(ExprId),
    Decl(DeclId),
}

impl From<ExprId> for Item {
    fn from(expr: ExprId) -> Self {
        Item::Expr(expr)
    }
}

impl From<DeclId> for Item {
    fn from(decl: DeclId) -> Self {
        Item::Decl(decl)
    }
}

#[derive(Debug, Default)]
pub struct ModScope {
    pub decl_groups: HashMap<Sym, Vec<ModScopedDecl>>,
}

#[derive(Debug, Clone)]
pub struct PatternBindingDecl {
    pub paths: Vec<PatternBindingPath>,
    pub scrutinee: ExprId,
}

#[derive(Debug, Clone)]
pub struct PatternBindingPath {
    pub components: Vec<PatternBindingPathComponent>,
}

impl PatternBindingPath {
    pub fn identity() -> Self {
        PatternBindingPath {
            components: Vec::new()
        }
    }
}

#[derive(Debug, Clone)]
pub enum PatternBindingPathComponent {
    VariantPayload(usize),
}

#[derive(Debug)]
pub enum Decl {
    Computed {
        param_tys: SmallVec<[ExprId; 2]>,
        params: Range<DeclId>,
        scope: ImperScopeId,
        generic_params: Range<DeclId>,
    },
    ComputedPrototype {
        param_tys: SmallVec<[ExprId; 2]>,
        extern_func: Option<ExternFunctionRef>,
    },
    Stored { id: StoredDeclId, is_mut: bool, root_expr: ExprId, },
    PatternBinding { id: PatternBindingDeclId, is_mut: bool, },
    Parameter {
        /// Parameter index within the function
        index: usize,
    },
    Intrinsic { intr: Intrinsic, param_tys: SmallVec<[ExprId; 2]>, function_like: bool },
    Static(ExprId),
    Const(ExprId),
    Field { strukt: StructId, index: usize },
    Variant { enuum: EnumId, index: usize, payload_ty: Option<ExprId>, },
    /// The magic `return_value` declaration, for use in `@guarantees` attributes
    ReturnValue,
    GenericParam(GenericParamId),
}

#[derive(Debug)]
pub struct FieldDecl {
    pub decl: DeclId,
    pub name: Sym,
    pub ty: ExprId,
}

#[derive(Debug, Clone, Copy)]
pub struct VariantDecl {
    pub decl: DeclId,
    pub name: Sym,
    pub enuum: ExprId,
    pub payload_ty: Option<ExprId>,
}

#[derive(Debug)]
pub struct Struct {
    // TODO: store FieldDecls inline instead
    pub fields: Vec<FieldDecl>,
}

#[derive(Debug)]
pub struct Enum {
    pub variants: Vec<VariantDecl>,
}

#[derive(Debug, Clone)]
pub struct SwitchCase {
    pub pattern: Pattern,
    pub scope: ImperScopeId,
    pub scope_range: SourceRange,
}

#[derive(Debug, Clone, Copy)]
pub struct Ident {
    pub symbol: Sym,
    pub range: SourceRange,
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub kind: PatternKind,
    pub bindings: Vec<PatternBindingDeclId>,
}

#[derive(Debug, Clone)]
pub enum PatternKind {
    ContextualMember {
        name: Ident,
        range: SourceRange,
    },
    NamedCatchAll(Ident),
    AnonymousCatchAll(SourceRange),
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Intrinsic {
    Mult,
    Div,
    Mod,
    Add,
    Sub,
    Less,
    LessOrEq,
    Greater,
    GreaterOrEq,
    Eq,
    NotEq,
    BitwiseAnd,
    BitwiseOr,
    BitwiseNot,
    BitwiseXor,
    LeftShift,
    RightShift,
    LogicalAnd,
    LogicalOr,
    LogicalNot,
    Neg,
    Pos,
    Panic,
    Print,
    Malloc,
    Free,
    SizeOf,
    StrideOf,
    AlignOf,
    OffsetOf,

    // Named types
    I8,
    I16,
    I32,
    I64,
    Isize,
    U8,
    U16,
    U32,
    U64,
    Usize,
    F32,
    F64,
    Never,
    Bool,
    Void,
    Ty,
    Module,
    PrintType,
}

impl Intrinsic {
    pub fn name(&self) -> &str {
        use Intrinsic::*;
        match self {
            Mult => "*",
            Div => "/",
            Mod => "%",
            Add => "+",
            Sub => "-",
            Less => "<",
            LessOrEq => "<=",
            Greater => ">",
            GreaterOrEq => ">=",
            Eq => "==",
            NotEq => "!=",
            BitwiseAnd => "&",
            BitwiseOr => "|",
            BitwiseXor => "^",
            BitwiseNot => "~",
            LeftShift => "<<",
            RightShift => ">>",
            LogicalAnd => "&&",
            LogicalOr => "||",
            LogicalNot => "!",
            Neg => "-",
            Pos => "+",
            Panic => "panic",
            Print => "print",
            Malloc => "malloc",
            Free => "free",
            I8 => "i8",
            I16 => "i16",
            I32 => "i32",
            I64 => "i64",
            Isize => "isize",
            U8 => "u8",
            U16 => "u16",
            U32 => "u32",
            U64 => "u64",
            Usize => "usize",
            F32 => "f32",
            F64 => "f64",
            Never => "never",
            Bool => "bool",
            Void => "void",
            Ty => "type",
            Module => "module",
            PrintType => "print_type",
            AlignOf => "align_of",
            SizeOf => "size_of",
            StrideOf => "stride_of",
            OffsetOf => "offset_of",
        }
    }
}

// These struct literals are necessary because the methods to make an Idx type are not const.
pub const VOID_EXPR: ExprId = ExprId { _raw: 0 };
pub const VOID_EXPR_ITEM: ItemId = ItemId { _raw: 0 };
pub const ERROR_EXPR: ExprId = ExprId { _raw: 1 };
pub const VOID_TYPE: ExprId = ExprId { _raw: 2 };
pub const TYPE_TYPE: ExprId = ExprId { _raw: 3 };
pub const ERROR_TYPE: ExprId = ExprId { _raw: 4 };
pub const RETURN_VALUE_DECL: DeclId = DeclId { _raw: 0 };

pub struct Attribute {
    pub attr: Sym,
    pub arg: Option<ExprId>,
    /// The range of the whole attribute, including @ sign and parentheses
    pub range: SourceRange,
}

#[derive(Default)]
pub struct HirCode {
    pub items: IndexVec<ItemId, Item>,
    pub exprs: IndexVec<ExprId, Expr>,
    pub source_ranges: IndexVec<ItemId, SourceRange>,
    pub decl_refs: IndexVec<DeclRefId, DeclRef>,
    pub decls: IndexVec<DeclId, Decl>,
    pub pattern_binding_decls: IndexVec<PatternBindingDeclId, PatternBindingDecl>,
    pub decl_attributes: HashMap<DeclId, Vec<Attribute>>,
    pub expr_to_items: IndexVec<ExprId, ItemId>,
    pub decl_to_items: IndexVec<DeclId, ItemId>,
    pub names: IndexVec<DeclId, Sym>,
    pub explicit_tys: IndexVec<DeclId, Option<ExprId>>,
    pub global_scopes: IndexVec<SourceFileId, ModScopeId>,
    pub imper_scopes: IndexVec<ImperScopeId, ImperScope>,
    pub mod_scopes: IndexVec<ModScopeId, ModScope>,
    pub imper_ns: IndexVec<ImperScopeNsId, ImperScopeNs>,
    pub mod_ns: IndexVec<ModScopeNsId, ModScopeNs>,
    pub condition_ns: IndexVec<ConditionNsId, ConditionNs>,
    pub comp_decl_params_ns: IndexVec<CompDeclParamsNsId, CompDeclParamsNs>,
    pub cast_counter: IndexCounter<CastId>,
    pub structs: IndexVec<StructId, Struct>,
    pub enums: IndexVec<EnumId, Enum>,
    pub extern_mods: IndexVec<ExternModId, ExternMod>,
    pub struct_lits: IndexCounter<StructLitId>,
}