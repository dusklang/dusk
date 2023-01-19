use std::any::TypeId;
use std::ffi::CString;
use std::collections::HashMap;
use std::ops::Range;

use index_vec::{IndexVec, define_index_type};
use smallvec::SmallVec;
use string_interner::DefaultSymbol as Sym;

use crate::dire::mir::Const;
use crate::dire::ty::{Type, InternalType, InternalTypeId};
use crate::dire::index_counter::IndexCounter;
use crate::dire::source_info::{SourceRange, SourceFileId};
use crate::dire::BlockId;
use crate::dire::InternalField;

use crate::ast::Intrinsic;

define_index_type!(pub struct ExprId = u32;);
define_index_type!(pub struct DeclRefId = u32;);
define_index_type!(pub struct ImperScopeId = u32;);
define_index_type!(pub struct CastId = u32;);
define_index_type!(pub struct DeclId = u32;);
define_index_type!(pub struct ItemId = u32;);
define_index_type!(pub struct NewNamespaceId = u32;);
define_index_type!(pub struct StructId = u32;);
define_index_type!(pub struct StructLitId = u32;);
define_index_type!(pub struct EnumId = u32;);
define_index_type!(pub struct StoredDeclId = u32;);
define_index_type!(pub struct PatternBindingDeclId = u32;);
define_index_type!(pub struct ImperScopeNsId = u32;);
define_index_type!(pub struct ModScopeNsId = u32;);
define_index_type!(pub struct ConditionNsId = u32;);
define_index_type!(pub struct GenericContextNsId = u32;);
define_index_type!(pub struct GenericParamId = u32;);
define_index_type!(pub struct ExternModId = u32;);
define_index_type!(pub struct GenericCtxId = u32;);
define_index_type!(pub struct LoopId = u32;);
define_index_type!(pub struct IntrinsicId = u32;);

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

#[derive(Debug)]
pub struct ImperScopeNs {
    pub decls: Vec<ImperScopedDecl>,
    pub parent: Option<Namespace>,
}

#[derive(Debug)]
pub struct ModScopeNs {
    pub scope: NewNamespaceId,
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
pub struct GenericContextNs {
    pub generic_params: Range<DeclId>,
    pub parent: Option<Namespace>,
}

#[derive(Debug)]
pub struct StaticDecl {
    pub decl: DeclId,
    pub num_params: usize,
    pub name: Sym,
}

#[derive(Debug, Default)]
pub struct NewNamespace {
    // TODO: store decls as groups of overloads, which is probably more efficient

    // For fields & instance methods
    pub instance_decls: Vec<DeclId>,
    // For enum variants, static methods and mod-scoped decls
    pub static_decls: Vec<StaticDecl>,

    // NOTE: below TODO comment copied from former `ModScope` type
    //      TODO: ideally, names declared below a `use modulename.*` should take precedence over conflicting names in
    //      `modulename`, while names declared inside `modulename` should take precedence over names declared before the
    //      `use`.

    // TODO: replace with a Vec of `NewNamespaceId` or something similar
    pub blanket_uses: Vec<Namespace>,
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
    GenericContext(GenericContextNsId),

    /// Includes the parameters of the function
    Requirement(ConditionNsId),
    /// Includes the parameters of the function, and a magic "return_value" value
    Guarantee(ConditionNsId),
    Invalid,
}

#[derive(Debug)]
pub struct DeclRef {
    pub name: Sym,
    pub namespace: Namespace,
    pub expr: ExprId,
}

#[derive(Debug)]
pub struct ExternMod {
    pub library_path: ExprId,
    pub imported_functions: Vec<ExternFunction>,
}

impl ExternMod {
    pub fn new(library_path: ExprId) -> Self {
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
pub enum GenericCtx {
    Blank,
    Decl {
        parameters: Vec<GenericParamId>,
        parent: GenericCtxId,
    },
    DeclRef {
        id: DeclRefId,
        parent: GenericCtxId,
    },
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
    Const(Const),
    DeclRef { id: DeclRefId },
    Call { callee: ExprId, arguments: SmallVec<[ExprId; 2]> },
    AddrOf { expr: ExprId, is_mut: bool },
    /// Transforms type into pointer type
    Pointer { expr: ExprId, is_mut: bool },
    FunctionTy { param_tys: Vec<ExprId>, ret_ty: ExprId },
    Deref(ExprId),
    Set { lhs: ExprId, rhs: ExprId },
    Do { scope: ImperScopeId },
    If { condition: ExprId, then_scope: ImperScopeId, else_scope: Option<ImperScopeId> },
    While { loop_id: LoopId, condition: ExprId, scope: ImperScopeId },
    For { loop_id: LoopId, binding: DeclId, lower_bound: ExprId, upper_bound: ExprId, scope: ImperScopeId },
    Break(Option<LoopId>),
    Continue(Option<LoopId>),
    Switch {
        scrutinee: ExprId,
        cases: Vec<SwitchCase>,
    },
    Cast { expr: ExprId, ty: ExprId, cast_id: CastId },
    Ret { expr: ExprId, decl: Option<DeclId> },
    Mod { id: NewNamespaceId, extern_library_path: Option<ExprId> },
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
    LoopBinding { id: StoredDeclId, is_mut: bool }, // The `i` in `for i in 0..100 {}`
    Parameter {
        /// Parameter index within the function
        index: usize,
    },
    LegacyIntrinsic { intr: LegacyIntrinsic, param_tys: SmallVec<[ExprId; 2]>, function_like: bool },
    Intrinsic(IntrinsicId),
    MethodIntrinsic(IntrinsicId),
    Static(ExprId),
    Const(ExprId),
    Field { strukt: StructId, index: usize },
    InternalField(InternalField),
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
    pub fields: Vec<FieldDecl>,
    pub namespace: NewNamespaceId,
}

#[derive(Debug)]
pub struct Enum {
    pub variants: Vec<VariantDecl>,
    pub namespace: NewNamespaceId,
}

#[derive(Debug, Clone,)]
pub struct SwitchCase {
    pub pattern: Pattern,
    pub scope: ImperScopeId,
    pub scope_range: SourceRange,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
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
    IntLit {
        value: u64,
        range: SourceRange,
    },
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum LegacyIntrinsic {
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

    // modify-assignment operators
    MultAssign,
    DivAssign,
    ModAssign,
    AddAssign,
    SubAssign,
    AndAssign,
    OrAssign,
    XorAssign,
    LeftShiftAssign,
    RightShiftAssign,

    Panic,
    Print,
    Malloc,
    Free,
    SizeOf,
    StrideOf,
    AlignOf,
    OffsetOf,
    PrintType,
    Import,

    GetNumArgs, // runtime.get_num_args()
    GetArg,     // runtime.get_arg(usize)
}

impl LegacyIntrinsic {
    pub fn name(&self) -> &str {
        use LegacyIntrinsic::*;
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

            MultAssign => "*=",
            DivAssign => "/=",
            ModAssign => "%=",
            AddAssign => "+=",
            SubAssign => "-=",
            AndAssign => "&=",
            OrAssign => "|=",
            XorAssign => "^=",
            LeftShiftAssign => "<<=",
            RightShiftAssign => ">>=",

            Panic => "panic",
            Print => "print",
            Malloc => "malloc",
            Free => "free",
            Import => "import",

            PrintType => "print_type",
            AlignOf => "align_of",
            SizeOf => "size_of",
            StrideOf => "stride_of",
            OffsetOf => "offset_of",

            GetNumArgs => "get_num_args",
            GetArg => "get_arg",
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
pub const BLANK_GENERIC_CTX: GenericCtxId = GenericCtxId::from_usize_unchecked(0);
impl Default for GenericCtxId {
    fn default() -> Self { BLANK_GENERIC_CTX }
}

pub struct Attribute {
    pub attr: Sym,
    pub arg: Option<ExprId>,
    /// The range of the whole attribute, including @ sign and parentheses
    pub range: SourceRange,
}

#[derive(Default)]
pub struct Ast {
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
    pub intrinsics: IndexVec<IntrinsicId, Intrinsic>,
    pub global_scopes: HashMap<SourceFileId, NewNamespaceId>,
    pub imper_scopes: IndexVec<ImperScopeId, ImperScope>,
    pub imper_ns: IndexVec<ImperScopeNsId, ImperScopeNs>,
    pub mod_ns: IndexVec<ModScopeNsId, ModScopeNs>,
    pub condition_ns: IndexVec<ConditionNsId, ConditionNs>,
    pub generic_context_ns: IndexVec<GenericContextNsId, GenericContextNs>,
    pub new_namespaces: IndexVec<NewNamespaceId, NewNamespace>,
    pub cast_counter: IndexCounter<CastId>,
    pub internal_types: IndexVec<InternalTypeId, InternalType>,
    pub bridged_types: HashMap<TypeId, Type>,
    pub structs: IndexVec<StructId, Struct>,
    pub enums: IndexVec<EnumId, Enum>,
    pub extern_mods: IndexVec<ExternModId, ExternMod>,
    pub struct_lits: IndexCounter<StructLitId>,
    pub generic_ctxs: IndexVec<GenericCtxId, GenericCtx>,
    pub item_generic_ctxs: IndexVec<ItemId, GenericCtxId>,
}
