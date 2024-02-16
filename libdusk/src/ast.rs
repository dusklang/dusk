use std::any::TypeId;
use std::ffi::CString;
use std::collections::HashMap;
use std::ops::Range;
use std::fmt::Debug;
use std::collections::HashSet;

use crate::display_adapter;
use index_vec::{IndexVec, define_index_type};
use smallvec::{SmallVec, smallvec};
use string_interner::{DefaultSymbol as Sym, Symbol, StringInterner};

use crate::mir::Const;
use crate::code::{Op, Block};
use crate::code::BlockId;
use crate::index_counter::IndexCounter;
use crate::source_info::{SourceFileId, SourceRange};
use crate::internal_types::InternalField;

use crate::ty::{Type, InternalType, InternalTypeId};
use crate::autopop::{AutoPopStack, AutoPopStackEntry};
use crate::driver::Driver;
use crate::error::Error;
use crate::index_vec::*;
use crate::builder::{BinOp, UnOp};
use crate::source_info::ToSourceRange;
use crate::interpreter::Value;
use crate::driver::DriverRef;

use dusk_proc_macros::*;

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
define_index_type!(pub struct ExtendBlockNsId = u32;);
define_index_type!(pub struct ConditionNsId = u32;);
define_index_type!(pub struct GenericContextNsId = u32;);
define_index_type!(pub struct GenericParamId = u32;);
define_index_type!(pub struct TypeVarId = u32;);
define_index_type!(pub struct ExternModId = u32;);
define_index_type!(pub struct ExtendBlockId = u32;);
define_index_type!(pub struct GenericCtxId = u32;);
define_index_type!(pub struct LoopId = u32;);
define_index_type!(pub struct IntrinsicId = u32;);

#[derive(Debug, Clone)]
pub struct FieldAssignment {
    pub name: Sym,
    pub expr: ExprId,
}

/// A declaration in local (imperative) scope
#[derive(Debug, Clone, Copy)]
pub struct ImperScopedDecl {
    pub name: Sym,
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
pub struct ExtendBlockNs {
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
pub struct FnDeclParamsNs {
    /// The function decl that this function decl params namespace refers to
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
pub struct FieldInfo {
    pub struct_id: StructId,
    pub field_index: usize,
}

#[derive(Debug)]
pub struct InstanceDecl {
    pub decl: DeclId,
    pub field_info: Option<FieldInfo>,
}

#[derive(Debug)]
pub struct StaticDecl {
    pub decl: DeclId,
    pub name: Sym,
}

#[derive(Debug, Default)]
pub struct NewNamespace {
    // TODO: store decls as groups of overloads, which is probably more efficient

    // For fields & instance methods
    pub instance_decls: Vec<InstanceDecl>,
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
    ExtendBlock(ExtendBlockNsId),
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

#[derive(Debug, Clone)]
pub struct ExtendBlock {
    pub extendee: ExprId,
    pub static_methods: Vec<DeclId>,
    pub instance_methods: Vec<DeclId>,
}

impl ExtendBlock {
    fn new(extendee: ExprId) -> Self {
        Self {
            extendee,
            static_methods: Default::default(),
            instance_methods: Default::default(),
        }
    }
}

#[derive(Debug)]
pub struct ExternMod {
    pub library_path: ExprId,
    pub imported_functions: Vec<ExternFunction>,
    pub objc_class_references: Vec<String>,
}

impl ExternMod {
    pub fn new(library_path: ExprId) -> Self {
        Self {
            library_path,
            imported_functions: Vec::new(),
            objc_class_references: Vec::new(),
        }
    }
}

#[derive(Debug)]
pub struct ExternFunction {
    pub name: String,
    pub param_list: ParamList,
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
        parameters: Range<GenericParamId>,
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
    DeclRef { id: DeclRefId, explicit_generic_args: Option<Vec<ExprId>> },
    Call { callee: ExprId, arguments: SmallVec<[ExprId; 2]> },
    AddrOf { expr: ExprId, is_mut: bool },
    /// Transforms type into pointer type
    Pointer { expr: ExprId, is_mut: bool },
    FunctionTy { param_tys: Vec<ExprId>, has_c_variadic_param: bool, ret_ty: ExprId },
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
    ExtendBlock { extendee: ExprId, id: ExtendBlockId },
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

#[derive(Default, Debug, Clone)]
pub struct ParamList {
    pub param_tys: SmallVec<[ExprId; 2]>,
    pub has_c_variadic_param: bool,
}

#[derive(Debug)]
pub enum Decl {
    Function {
        param_tys: SmallVec<[ExprId; 2]>,
        params: Range<DeclId>,
        scope: ImperScopeId,
        generic_params: Range<GenericParamId>,
    },
    FunctionPrototype {
        param_list: ParamList,
        extern_func: Option<ExternFunctionRef>,
    },
    ObjcClassRef { extern_mod: ExternModId, index: usize },
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
    Const {
        assigned_expr: ExprId,
        generic_params: Range<GenericParamId>,
    },
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

#[derive(Clone, Copy)]
pub enum SelfParameterKind {
    Owned,
    Ptr,
    MutPtr,
}

#[derive(Clone, Copy)]
pub struct SelfParameter {
    pub kind: SelfParameterKind,
    pub self_ty: ExprId,
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
    pub decl_self_parameters: IndexVec<DeclId, Option<SelfParameter>>,
    pub expr_to_items: IndexVec<ExprId, ItemId>,
    pub decl_to_items: IndexVec<DeclId, ItemId>,
    pub names: IndexVec<DeclId, Sym>,
    pub explicit_tys: IndexVec<DeclId, Option<ExprId>>,
    pub intrinsics: IndexVec<IntrinsicId, Intrinsic>,
    pub global_scopes: HashMap<SourceFileId, NewNamespaceId>,
    pub imper_scopes: IndexVec<ImperScopeId, ImperScope>,
    pub imper_ns: IndexVec<ImperScopeNsId, ImperScopeNs>,
    pub mod_ns: IndexVec<ModScopeNsId, ModScopeNs>,
    pub extend_block_ns: IndexVec<ExtendBlockNsId, ExtendBlockNs>,
    pub condition_ns: IndexVec<ConditionNsId, ConditionNs>,
    pub generic_context_ns: IndexVec<GenericContextNsId, GenericContextNs>,
    pub new_namespaces: IndexVec<NewNamespaceId, NewNamespace>,
    pub cast_counter: IndexCounter<CastId>,
    pub internal_types: IndexVec<InternalTypeId, InternalType>,
    pub bridged_types: HashMap<TypeId, Type>,
    pub structs: IndexVec<StructId, Struct>,
    pub enums: IndexVec<EnumId, Enum>,
    pub extern_mods: IndexVec<ExternModId, ExternMod>,
    pub extend_blocks: IndexVec<ExtendBlockId, ExtendBlock>,
    pub struct_lits: IndexCounter<StructLitId>,
    pub type_vars: IndexCounter<TypeVarId>,
    pub expr_to_type_vars: IndexVec<ExprId, TypeVarId>,
    pub generic_ctxs: IndexVec<GenericCtxId, GenericCtx>,
    pub item_generic_ctxs: IndexVec<ItemId, GenericCtxId>,
    pub generic_arg_type_variables: HashMap<(DeclRefId, GenericParamId), TypeVarId>,
}


define_index_type!(pub struct ImperRootId = u32;);

// TODO: switch to AOS here
#[derive(Debug, Clone)]
pub struct GenericParamList {
    pub names: SmallVec<[Sym; 1]>,
    pub ids: Range<GenericParamId>,
    pub ranges: SmallVec<[SourceRange; 1]>,
}

pub type IntrinsicImpl = fn(&mut DriverRef, Vec<&Value>) -> Value;

pub struct Intrinsic {
    pub param_tys: SmallVec<[ExprId; 2]>,
    pub ret_ty: Type,
    pub name: String,
    pub implementation: IntrinsicImpl,
}

impl Driver {
    pub fn create_decls_for_generic_param_list(&mut self, list: &GenericParamList) -> Range<DeclId> {
        assert_eq!(list.names.len(), list.ids.end - list.ids.start);
        assert_eq!(list.names.len(), list.ranges.len());
        self.code.ast.decls.reserve(list.names.len());
        let first_generic_param = self.code.ast.decls.next_idx();
        for ((param, &name), &range) in range_iter(list.ids.clone()).zip(&list.names).zip(&list.ranges) {
            self.add_decl(Decl::GenericParam(param), name, Some(VOID_TYPE), range);
        }
        let last_generic_param = self.code.ast.decls.next_idx();
        first_generic_param..last_generic_param
    }
}

impl Default for GenericParamList {
    fn default() -> Self {
        Self {
            names: SmallVec::new(),
            ids: empty_range(),
            ranges: SmallVec::new(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct BufferedStmt {
    expr: ExprId,
    has_semicolon: bool,
}

#[derive(Debug, Clone, Copy)]
pub enum ScopeState {
    Imper {
        id: ImperScopeId,
        // TODO: choose `namespace` or `ns` and use it consistently for all variants of this enum
        namespace: ImperScopeNsId,
        stmt_buffer: Option<BufferedStmt>,
        root: ImperRootId,
    },
    Mod {
        id: NewNamespaceId,
        namespace: ModScopeNsId,
        extern_mod: Option<ExternModId>,
    },
    ExtendBlock {
        namespace: ExtendBlockNsId,
        id: ExtendBlockId,
        extendee: ExprId,
    },
    GenericContext(GenericContextNsId),
    Condition {
        ns: ConditionNsId,
        condition_kind: ConditionKind,
    },
}

#[derive(Debug, Default)]
struct ImperRoot {
    stored_decl_counter: IndexCounter<StoredDeclId>,
    loop_stack: AutoPopStack<LoopState>,
    loop_counter: IndexCounter<LoopId>,
}

#[derive(Debug)]
struct FnDeclState {
    scope: Option<ImperScopeId>,
    params: Range<DeclId>,
    generic_params: Range<DeclId>,
    id: DeclId,
    imper_scope_stack: u32,
}

#[derive(Debug)]
pub struct Builder {
    fn_decl_stack: Vec<FnDeclState>,
    scope_stack: AutoPopStack<ScopeState>,
    generic_ctx_stack: AutoPopStack<GenericCtxId>,
    debug_marked_exprs: HashSet<ExprId>,
    imper_roots: IndexVec<ImperRootId, ImperRoot>,

    pub prelude_namespace: Option<ModScopeNsId>,
    pub generic_params: IndexCounter<GenericParamId>,

    pub known_idents: KnownIdents,
}

macro_rules! declare_known_idents {
    ($($name:ident $(= $assignment:expr)?),*) => {
        #[derive(Debug)]
        pub struct KnownIdents {
            $(pub $name: Sym),*
        }

        impl KnownIdents {
            fn uninit() -> Self {
                KnownIdents {
                    $($name: Sym::try_from_usize((u32::MAX - 1) as usize).unwrap()),*
                }
            }
        
            fn init(&mut self, interner: &mut StringInterner) {
                *self = KnownIdents {
                    $(
                        $name: declare_known_idents!(@init interner, $name $(= $assignment)?)
                    ),*
                };
            }
        }
    };
    (@init $interner:expr, $name:ident = $assignment:expr) => {
        $interner.get_or_intern($assignment)
    };
    (@init $interner:expr, $name:ident) => {
        $interner.get_or_intern(stringify!($name))
    };
}
declare_known_idents!(requires, guarantees, comptime, return_value, invalid_declref, salf = "self", capital_self = "Self", underscore = "_");

impl Default for Builder {
    fn default() -> Self {
        Builder {
            fn_decl_stack: Default::default(),
            scope_stack: Default::default(),
            generic_ctx_stack: Default::default(),
            debug_marked_exprs: Default::default(),
            imper_roots: Default::default(),

            generic_params: IndexCounter::new(),

            prelude_namespace: None,

            // Gets initialized in Driver::initialize_ast() below
            known_idents: KnownIdents::uninit(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LoopState {
    id: LoopId,
    name: Option<Ident>,
    used: bool, // whether the loop name has been used by a continue or break
}

impl PartialEq<ScopeState> for ConditionNsId {
    fn eq(&self, other: &ScopeState) -> bool {
        match other {
            ScopeState::Condition { ns, .. } => ns == self,
            _ => false,
        }
    }
}
impl PartialEq<ScopeState> for GenericContextNsId {
    fn eq(&self, other: &ScopeState) -> bool {
        match other {
            ScopeState::GenericContext(ns) => ns == self,
            _ => false,
        }
    }
}
impl PartialEq<ScopeState> for ModScopeNsId {
    fn eq(&self, other: &ScopeState) -> bool {
        match other {
            ScopeState::Mod { namespace: ns, .. } => ns == self,
            _ => false,
        }
    }
}
impl PartialEq<ScopeState> for ImperScopeId {
    fn eq(&self, other: &ScopeState) -> bool {
        match other {
            ScopeState::Imper { id, .. } => id == self,
            _ => false,
        }
    }
}
impl PartialEq<ScopeState> for ExtendBlockId {
    fn eq(&self, other: &ScopeState) -> bool {
        match other {
            ScopeState::ExtendBlock { id, .. } => id == self,
            _ => false,
        }
    }
}
impl PartialEq<LoopState> for LoopId {
    fn eq(&self, other: &LoopState) -> bool {
        other.id == *self
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ConditionKind {
    Requirement, Guarantee,
}

impl Driver {
    pub fn initialize_ast(&mut self) {
        self.ast.generic_ctx_stack.push(BLANK_GENERIC_CTX, BLANK_GENERIC_CTX).make_permanent();
        self.add_expr(Expr::Void, SourceRange::default());
        self.add_expr(Expr::Error, SourceRange::default());
        self.add_const_ty(Type::Void);
        self.add_const_ty(Type::Ty);
        self.add_const_ty(Type::Error);
        assert_eq!(self.code.ast.exprs.len(), 5);

        self.ast.known_idents.init(&mut self.interner);
        self.add_decl(Decl::ReturnValue, self.ast.known_idents.return_value, None, SourceRange::default());

        self.register_internal_fields();
        self.add_prelude();
    }

    #[allow(unused)]
    #[display_adapter]
    pub fn dump_scope_stack(&self, w: &mut Formatter) {
        for scope in self.ast.scope_stack.stack.lock().unwrap().borrow().iter().rev() {
            writeln!(w, "{:?}", scope)?;
        }
        Ok(())
    }

    pub fn debug_mark_expr(&mut self, expr: ExprId) {
        self.ast.debug_marked_exprs.insert(expr);
    }

    #[allow(unused)]
    pub fn expr_is_debug_marked(&self, expr: ExprId) -> bool {
        self.ast.debug_marked_exprs.contains(&expr)
    }

    fn add_expr(&mut self, expr: Expr, range: SourceRange) -> ExprId {
        // TODO: I used to require callers to explicitly pass in the generic ctx id, but it seems fine to just take it
        // from the top of the stack. Is there some major downside to this approach that I have since forgotten?
        // NOTE: also, see add_decl()
        let generic_ctx = self.ast.generic_ctx_stack.peek().unwrap();
        let expr_id = self.code.ast.exprs.push(expr);
        let type_var_id = self.code.ast.type_vars.next_idx();
        let item_id = self.code.ast.items.push(Item::Expr(expr_id));
        self.code.ast.item_generic_ctxs.push_at(item_id, generic_ctx);
        self.code.ast.expr_to_items.push_at(expr_id, item_id);
        self.code.ast.expr_to_type_vars.push_at(expr_id, type_var_id);
        self.code.ast.source_ranges.push_at(item_id, range);

        expr_id
    }

    pub fn add_decl_with_self_parameter(&mut self, decl: Decl, name: Sym, explicit_ty: Option<ExprId>, range: SourceRange, self_parameter: Option<SelfParameter>) -> DeclId {
        // TODO: I used to require callers to explicitly pass in the generic ctx id, but it seems fine to just take it
        // from the top of the stack. Is there some major downside to this approach that I have since forgotten?
        // NOTE: also, see add_expr()
        let generic_ctx = self.ast.generic_ctx_stack.peek().unwrap();
        let decl_id = self.code.ast.decls.push(decl);
        self.code.ast.explicit_tys.push_at(decl_id, explicit_ty);
        self.code.ast.names.push_at(decl_id, name);
        self.code.ast.decl_self_parameters.push_at(decl_id, self_parameter);

        let item_id = self.code.ast.items.push(Item::Decl(decl_id));
        self.code.ast.decl_to_items.push_at(decl_id, item_id);

        self.code.ast.source_ranges.push_at(item_id, range);
        self.code.ast.item_generic_ctxs.push_at(item_id, generic_ctx);

        decl_id
    }

    pub fn add_decl(&mut self, decl: Decl, name: Sym, explicit_ty: Option<ExprId>, range: SourceRange) -> DeclId {
        self.add_decl_with_self_parameter(decl, name, explicit_ty, range, None)
    }

    pub fn int_lit(&mut self, lit: u64, range: SourceRange) -> ExprId {
        self.add_expr(Expr::IntLit { lit }, range)
    }
    pub fn dec_lit(&mut self, lit: f64, range: SourceRange) -> ExprId { 
        self.add_expr(Expr::DecLit { lit }, range)
    }
    pub fn str_lit(&mut self, lit: CString, range: SourceRange) -> ExprId { 
        self.add_expr(Expr::StrLit { lit }, range)
    }
    pub fn char_lit(&mut self, lit: i8, range: SourceRange) -> ExprId { 
        self.add_expr(Expr::CharLit { lit }, range)
    }
    pub fn bool_lit(&mut self, lit: bool, range: SourceRange) -> ExprId { 
        self.add_expr(Expr::BoolLit { lit }, range)
    }
    pub fn cast(&mut self, expr: ExprId, ty: ExprId, range: SourceRange) -> ExprId {
        let cast_id = self.code.ast.cast_counter.next_idx();
        self.add_expr(Expr::Cast { expr, ty, cast_id }, range)
    }
    pub fn next_stored_decl(&mut self) -> StoredDeclId {
        let imper_root = self.get_imper_root().unwrap();
        self.ast.imper_roots[imper_root].stored_decl_counter.next_idx()
    }
    pub fn is_in_imper_scope(&self) -> bool {
        for scope in self.ast.scope_stack.stack.lock().unwrap().borrow().iter().rev() {
            if matches!(scope, ScopeState::Imper { .. }) {
                return true;
            } else if matches!(scope, ScopeState::Mod { .. } | ScopeState::ExtendBlock { .. }) {
                break;
            }
        }
        false
    }
    pub fn is_in_mod_scope(&self) -> bool {
        for scope in self.ast.scope_stack.stack.lock().unwrap().borrow().iter().rev() {
            if matches!(scope, ScopeState::Mod { .. }) {
                return true;
            } else if matches!(scope, ScopeState::Imper { .. } | ScopeState::ExtendBlock { .. }) {
                break;
            }
        }
        false
    }
    // Returns extendee, not the extend block expression itself
    pub fn is_in_extend_block_scope(&self) -> Option<ExprId> {
        for scope in self.ast.scope_stack.stack.lock().unwrap().borrow().iter().rev() {
            if let &ScopeState::ExtendBlock { extendee, .. } = scope {
                return Some(extendee);
            } else if matches!(scope, ScopeState::Imper { .. } | ScopeState::Mod { .. }) {
                break;
            }
        }
        None
    }
    pub fn stored_decl(&mut self, name: Sym, generic_params: GenericParamList, explicit_ty: Option<ExprId>, is_mut: bool, root_expr: ExprId, range: SourceRange) -> DeclId {
        self.flush_stmt_buffer();
        match self.ast.scope_stack.peek().unwrap() {
            ScopeState::Imper { .. } => {
                let id = self.next_stored_decl();

                let decl_id = self.add_decl(Decl::Stored { id, is_mut, root_expr }, name, explicit_ty, range);
                self.scope_item(Item::Decl(decl_id), false);
                self.imper_scoped_decl(
                    ImperScopedDecl {
                        name,
                        id: decl_id,
                    }
                );
                decl_id
            }
            ScopeState::Mod { .. } => {
                let decl_id = self.add_decl(
                    if is_mut {
                        Decl::Static(root_expr)
                    } else {
                        Decl::Const { assigned_expr: root_expr, generic_params: generic_params.ids.clone() }
                    },
                    name,
                    explicit_ty,
                    range,
                );
                self.mod_scoped_decl(
                    StaticDecl {
                        name,
                        decl: decl_id
                    }
                );
                decl_id
            },
            ScopeState::Condition { .. } | ScopeState::GenericContext(_) | ScopeState::ExtendBlock { .. } => panic!("Stored decl unsupported in this position"),
        }
    }
    pub fn ret(&mut self, expr: ExprId, range: SourceRange) -> ExprId {
        let decl = self.ast.fn_decl_stack.last().map(|decl| decl.id);
        if decl.is_none() {
            self.diag.report_error_no_range_msg("returning outside of a function is invalid", range);
        }
        self.add_expr(Expr::Ret { expr, decl }, range)
    }
    fn lookup_loop_by_label(&mut self, label: Option<Ident>) -> Option<LoopId> {
        let imper_root = self.get_imper_root().unwrap();
        let loop_stack = self.ast.imper_roots[imper_root].loop_stack.clone();
        let loop_stack = loop_stack.stack.lock().unwrap();
        let mut loop_stack = loop_stack.borrow_mut();
        if let Some(label) = label {
            for looop in loop_stack.iter_mut().rev() {
                if looop.name.map(|ident| ident.symbol) == Some(label.symbol) {
                    looop.used = true;
                    return Some(looop.id);
                }
            }
            let label_str = self.interner.resolve(label.symbol).unwrap().to_owned();
            self.diag.report_error_no_range_msg(
                format!("unable to find loop label `{}`", label_str),
                label.range,
            );
            None
        } else {
            loop_stack.last().map(|state| state.id)
        }
    }
    fn control_flow_outside_loop_error(&mut self, kind: &str, range: SourceRange) {
        self.diag.push(
            Error::new(format!("`{}` expression found outside of a loop", kind))
                .adding_primary_range(range, "only valid inside a `for` or `while` loop")
        )
    }
    pub fn break_expr(&mut self, range: SourceRange, label: Option<Ident>) -> ExprId {
        let id = self.lookup_loop_by_label(label);
        if label.is_none() && id.is_none() {
            self.control_flow_outside_loop_error("break", range);
        }
        self.add_expr(Expr::Break(id), range)
    }
    pub fn continue_expr(&mut self, range: SourceRange, label: Option<Ident>) -> ExprId {
        let id = self.lookup_loop_by_label(label);
        if label.is_none() && id.is_none() {
            self.control_flow_outside_loop_error("break", range);
        }
        self.add_expr(Expr::Continue(id), range)
    }
    pub fn if_expr(&mut self, condition: ExprId, then_scope: ImperScopeId, else_scope: Option<ImperScopeId>, range: SourceRange) -> ExprId {
        self.add_expr(
            Expr::If { condition, then_scope, else_scope },
            range,
        )
    }
    pub fn while_expr(&mut self, loop_id: LoopId, condition: ExprId, scope: ImperScopeId, range: SourceRange) -> ExprId {
        self.add_expr(Expr::While { loop_id, condition, scope }, range)
    }
    pub fn for_expr(&mut self, loop_id: LoopId, binding: DeclId, lower_bound: ExprId, upper_bound: ExprId, scope: ImperScopeId, range: SourceRange) -> ExprId {
        self.add_expr(Expr::For { loop_id, binding, lower_bound, upper_bound, scope }, range)
    }
    pub fn switch_expr(&mut self, scrutinee: ExprId, cases: Vec<SwitchCase>, range: SourceRange) -> ExprId {
        self.add_expr(Expr::Switch { scrutinee, cases }, range)
    }
    pub fn do_expr(&mut self, scope: ImperScopeId, range: SourceRange) -> ExprId {
        self.add_expr(Expr::Do { scope }, range)
    }
    pub fn field_decl(&mut self, name: Sym, strukt: StructId, ty: ExprId, index: usize, range: SourceRange) -> FieldDecl {
        let decl = self.add_decl(Decl::Field { strukt, index }, name, Some(ty), range);
        FieldDecl { decl, name, ty }
    }
    pub fn variant_decl(&mut self, name: Sym, enuum: ExprId, enum_id: EnumId, index: usize, payload_ty: Option<ExprId>, range: SourceRange) -> VariantDecl {
        let decl = self.add_decl(Decl::Variant { enuum: enum_id, index, payload_ty }, name, Some(enuum), range);
        VariantDecl { decl, name, enuum, payload_ty }
    }
    pub fn reserve_struct(&mut self) -> (ExprId, StructId) {
        let strukt = self.code.ast.structs.push(Struct { fields: Vec::new(), namespace: NewNamespaceId::from_raw(u32::MAX) });
        let expr = self.add_expr(Expr::Struct(strukt), Default::default());
        (expr, strukt)
    }
    pub fn finish_struct(&mut self, fields: Vec<FieldDecl>, range: SourceRange, expr: ExprId, strukt: StructId) {
        let namespace = NewNamespace {
            instance_decls: fields.iter().enumerate().map(|(i, field)| InstanceDecl { decl: field.decl, field_info: Some(FieldInfo { struct_id: strukt, field_index: i }) }).collect(),
            ..Default::default()
        };
        let namespace = self.code.ast.new_namespaces.push(namespace);
        self.code.ast.structs[strukt] = Struct { fields, namespace };
        ef!(expr.range) = range;
    }
    pub fn reserve_enum(&mut self) -> (ExprId, EnumId) {
        let enuum = self.code.ast.enums.push(Enum { variants: Vec::new(), namespace: NewNamespaceId::from_raw(u32::MAX) });
        let expr = self.add_expr(Expr::Enum(enuum), Default::default());
        (expr, enuum)
    }
    pub fn finish_enum(&mut self, variants: Vec<VariantDecl>, range: SourceRange, expr: ExprId, enuum: EnumId) {
        let namespace = NewNamespace {
            static_decls: variants.iter()
                .map(|variant|
                    StaticDecl {
                        decl: variant.decl,
                        name: variant.name
                    }
                ).collect(),
            ..Default::default()
        };
        let namespace = self.code.ast.new_namespaces.push(namespace);
        self.code.ast.enums[enuum] = Enum { variants, namespace };
        ef!(expr.range) = range;
    }
    pub fn struct_lit(&mut self, ty: ExprId, fields: Vec<FieldAssignment>, range: SourceRange) -> ExprId {
        let id = self.code.ast.struct_lits.next_idx();
        self.add_expr(Expr::StructLit { ty, fields, id }, range)
    }
    pub fn begin_extend_block(&mut self, extendee: ExprId, range: SourceRange) -> (AutoPopStackEntry<ScopeState, ExtendBlockId>, ExprId) {
        let id = self.code.ast.extend_blocks.push(ExtendBlock::new(extendee));
        let expr = self.add_expr(Expr::ExtendBlock { extendee, id }, range);
        let parent = self.cur_namespace();
        let namespace = self.code.ast.extend_block_ns.push(ExtendBlockNs { parent: Some(parent) });
        let entry = self.push_to_scope_stack(id, ScopeState::ExtendBlock { namespace, id, extendee });
        (entry, expr)
    }
    pub fn error_expr(&mut self, range: SourceRange) -> ExprId {
        self.add_expr(Expr::Error, range)
    }
    pub fn get_pattern_bindings(&mut self, pattern: &PatternKind, scrutinee: ExprId) -> (Vec<ImperScopedDecl>, Vec<PatternBindingDeclId>) {
        let mut decls = Vec::new();
        let mut bindings = Vec::new();
        match pattern {
            PatternKind::ContextualMember { .. } | PatternKind::AnonymousCatchAll(_) | PatternKind::IntLit { .. } => {},
            PatternKind::NamedCatchAll(name) => {
                let paths = vec![
                    PatternBindingPath::identity()
                ];
                let binding_decl = PatternBindingDecl { paths, scrutinee };
                let id = self.code.ast.pattern_binding_decls.push(binding_decl);
                let decl = self.add_decl(Decl::PatternBinding { id, is_mut: false }, name.symbol, None, name.range);
                let decl = ImperScopedDecl { name: name.symbol, id: decl };
                decls.push(decl);
                bindings.push(id);
            },
        }
        (decls, bindings)
    }
    pub fn push_to_scope_stack<Id: PartialEq<ScopeState> + Debug + Copy>(&mut self, id: Id, state: ScopeState) -> AutoPopStackEntry<ScopeState, Id> {
        self.ast.scope_stack.push(id, state)
    }
    /// unchecked invariant: must call end_loop after this
    pub fn begin_loop(&mut self, name: Option<Ident>) -> AutoPopStackEntry<LoopState, LoopId> {
        let imper_root = self.get_imper_root().unwrap();
        let imper_root = &mut self.ast.imper_roots[imper_root];
        let id = imper_root.loop_counter.next_idx();
        let mut loop_stack = imper_root.loop_stack.clone();
        {
            let loop_stack = loop_stack.stack.lock().unwrap();
            let loop_stack = loop_stack.borrow();
            if let Some(name) = name {
                for state in &*loop_stack {
                    if state.name.map(|ident| ident.symbol) == Some(name.symbol) {
                        // AFAICT this is the first time I have emitted an error from inside the AST generator instead
                        // of the parser. This makes me a little uncomfortable. On the other hand, diagnosing this
                        // inside the parser would require exposing more state to the parser, which I also don't love.
                        let name_str = self.interner.resolve(name.symbol).unwrap();
                        self.diag.push(
                            Error::new(format!("loop with label `{}` already exists", name_str))
                                .adding_primary_range(name.range, "")
                                .adding_secondary_range(state.name.unwrap().range, "")
                        );
                        break;
                    }
                }
                
            }
        }

        loop_stack.push(id, LoopState { id, name, used: false })
    }
    pub fn end_loop(&mut self, entry: AutoPopStackEntry<LoopState, LoopId>) {
        let state = entry.stack.peek().unwrap();
        if let Some(name) = state.name {
            if !state.used {
                let name_str = self.interner.resolve(name.symbol).unwrap().to_string();
                self.diag.report_warning_no_range_msg(
                    format!("loop label `{}` never used", name_str),
                    name.range
                );
            }
        }
    }
    pub fn create_condition_namespace(&mut self) -> ConditionNsId {
        let parent = self.cur_namespace();
        self.code.ast.condition_ns.push(ConditionNs { func: DeclId::from_raw(u32::MAX), parent: Some(parent) })
    }
    pub fn enter_condition_namespace(&mut self, ns: ConditionNsId, condition_kind: ConditionKind) -> AutoPopStackEntry<ScopeState, ConditionNsId> {
        // This condition kind is just a placeholder which will be reset by each attribute. It is done this way so I can share a single
        // condition namespace across all condition attributes on a single function.
        self.push_to_scope_stack(ns, ScopeState::Condition { ns, condition_kind })
    }
    pub fn begin_generic_context(&mut self, generic_params: Range<DeclId>) -> AutoPopStackEntry<ScopeState, GenericContextNsId> {
        let parent = self.cur_namespace();
        let ns = self.code.ast.generic_context_ns.push(GenericContextNs { generic_params, parent: Some(parent) });
        self.push_to_scope_stack(ns, ScopeState::GenericContext(ns))
    }
    pub fn begin_module(&mut self, extern_mod: Option<ExternModId>, range: SourceRange) -> (AutoPopStackEntry<ScopeState, ModScopeNsId>, ExprId) {
        let parent = self.cur_namespace();
        let id = self.code.ast.new_namespaces.push(NewNamespace::default());
        let namespace = self.code.ast.mod_ns.push(
            ModScopeNs {
                scope: id, parent: Some(parent)
            }
        );
        let entry = self.push_to_scope_stack(namespace, ScopeState::Mod { id, namespace, extern_mod });
        let extern_library_path = extern_mod.map(|mawd| self.code.ast.extern_mods[mawd].library_path);
        let expr = self.add_expr(Expr::Mod { id, extern_library_path }, range);
        (entry, expr)
    }
    fn push_generic_ctx(&mut self, ctx: impl FnOnce(GenericCtxId) -> GenericCtx) -> AutoPopStackEntry<GenericCtxId> {
        let parent = self.ast.generic_ctx_stack.peek().unwrap();
        let generic_ctx = self.code.ast.generic_ctxs.push(ctx(parent));
        self.ast.generic_ctx_stack.push(generic_ctx, generic_ctx)
    }
    pub fn begin_decl_generic_ctx(&mut self, generic_param_list: GenericParamList) -> AutoPopStackEntry<GenericCtxId> {
        self.push_generic_ctx(|parent| GenericCtx::Decl { parameters: generic_param_list.ids, parent })
    }
    pub fn begin_fn_decl(&mut self, name: Sym, param_names: SmallVec<[Sym; 2]>, param_tys: SmallVec<[ExprId; 2]>, param_ranges: SmallVec<[SourceRange; 2]>, self_parameter: Option<SelfParameter>, generic_params: Range<GenericParamId>, generic_param_decls: Range<DeclId>, return_ty: ExprId, proto_range: SourceRange) -> DeclId {
        // This is a placeholder value that gets replaced once the parameter declarations get allocated.
        let id = self.add_decl_with_self_parameter(Decl::Static(ExprId::new(u32::MAX as usize)), name, Some(return_ty), proto_range, self_parameter);

        assert_eq!(param_names.len(), param_tys.len());
        self.code.ast.decls.reserve(param_tys.len());
        let first_param = self.code.ast.decls.next_idx();
        param_tys.iter()
            .enumerate()
            .zip(&param_names)
            .zip(&param_ranges)
            .for_each(|(((index, ty), &name), &range)| {
                self.add_decl(Decl::Parameter { index }, name, Some(*ty), range);
            });
        let last_param = self.code.ast.decls.next_idx();
        let params = first_param..last_param;

        // `end_fn_decl` will attach the real scope to this decl; we don't have it yet
        df!(id.ast) = Decl::Function {
            param_tys,
            params: params.clone(),
            scope: ImperScopeId::new(u32::MAX as usize),
            generic_params: generic_params.clone(),
        };
        match self.ast.scope_stack.peek().unwrap() {
            ScopeState::Imper { .. } => {
                self.flush_stmt_buffer();
                self.scope_item(Item::Decl(id), false);
            },
            ScopeState::Mod { .. } => {
                self.mod_scoped_decl(StaticDecl { name, decl: id });
            },
            ScopeState::ExtendBlock { id: extend_block_id, .. } => {
                if self_parameter.is_some() {
                    self.code.ast.extend_blocks[extend_block_id].instance_methods.push(id);
                } else {
                    self.code.ast.extend_blocks[extend_block_id].static_methods.push(id);
                }
            },
            ScopeState::Condition { .. } | ScopeState::GenericContext(_) => panic!("Function decls are not supported in this position"),
        }
        self.ast.fn_decl_stack.push(
            FnDeclState {
                scope: None,
                params,
                generic_params: generic_param_decls,
                id,
                imper_scope_stack: 0,
            }
        );

        id
    }
    pub fn fn_prototype(&mut self, name: Sym, param_list: ParamList, _param_ranges: SmallVec<[SourceRange; 2]>, return_ty: ExprId, range: SourceRange) -> DeclId {
        let extern_func = match self.ast.scope_stack.peek().unwrap() {
            ScopeState::Mod { extern_mod: Some(extern_mod), .. } => {
                let funcs = &mut self.code.ast.extern_mods[extern_mod].imported_functions;
                let index = funcs.len();
                let name = self.interner.resolve(name).unwrap();
                funcs.push(ExternFunction { name: name.to_string(), param_list: param_list.clone(), return_ty });
                Some(
                    ExternFunctionRef {
                        extern_mod,
                        index
                    }
                )
            },
            _ => None,
        };
        let id = self.add_decl(Decl::FunctionPrototype { param_list, extern_func }, name, Some(return_ty), range);
        match self.ast.scope_stack.peek().unwrap() {
            ScopeState::Imper { .. } => {
                self.flush_stmt_buffer();
                self.scope_item(Item::Decl(id), false);
            },
            ScopeState::Mod { .. } => {
                self.mod_scoped_decl(StaticDecl { name, decl: id });
            },
            ScopeState::ExtendBlock { .. } => panic!("Function prototypes are not supported in this position"),
            ScopeState::Condition { .. } | ScopeState::GenericContext(_) => panic!("Function decls are not supported in this position"),
        }

        id
    }
    pub fn begin_decl_ref_generic_ctx(&mut self) -> AutoPopStackEntry<GenericCtxId> {
        let id = self.code.ast.decl_refs.push(
            DeclRef {
                name: self.ast.known_idents.invalid_declref,
                namespace: Namespace::Invalid,
                expr: ERROR_EXPR,
            }
        );
        self.push_generic_ctx(|parent| GenericCtx::DeclRef { id, parent })
    }
    pub fn decl_ref(&mut self, base_expr: Option<ExprId>, name: Sym, explicit_generic_args: Option<Vec<ExprId>>, arguments: SmallVec<[ExprId; 2]>, has_parens: bool, range: SourceRange, generic_ctx: AutoPopStackEntry<GenericCtxId>) -> ExprId {
        let namespace = match base_expr {
            Some(base_expr) => {
                Namespace::MemberRef { base_expr }
            },
            None => self.cur_namespace(),
        };
        let GenericCtx::DeclRef { id, .. } = self.code.ast.generic_ctxs[generic_ctx.id()] else {
            panic!("Invalid generic context passed in");
        };
        let expr = self.add_expr(Expr::DeclRef { id, explicit_generic_args }, range);
        self.code.ast.decl_refs[id] = DeclRef {
            name,
            namespace,
            expr,
        };
        if has_parens {
            self.add_expr(Expr::Call { callee: expr, arguments }, range)
        } else {
            expr
        }
    }
    // TODO: intern constant expressions
    pub fn add_const_expr(&mut self, value: Const) -> ExprId {
        self.add_expr(Expr::Const(value), SourceRange::default())
    }
    pub fn add_const_ty(&mut self, ty: Type) -> ExprId {
        self.add_const_expr(Const::Ty(ty))
    }
    pub fn add_intrinsic(&mut self, intrinsic: LegacyIntrinsic, param_tys: SmallVec<[ExprId; 2]>, ret_ty: ExprId, function_like: bool) {
        let name = self.interner.get_or_intern(intrinsic.name());
        let id = self.add_decl(Decl::LegacyIntrinsic { intr: intrinsic, param_tys, function_like }, name, Some(ret_ty), SourceRange::default());
        self.mod_scoped_decl(
            StaticDecl { name, decl: id }
        );
    }
    pub fn internal_field(&mut self, field: InternalField, name: &str, ty: Type) -> DeclId {
        let name = self.interner.get_or_intern(name);
        let ty = self.add_const_ty(ty);
        self.add_decl(
            Decl::InternalField(field), name, Some(ty), SourceRange::default()
        )
    }
    pub fn bin_op(&mut self, op: BinOp, lhs: ExprId, rhs: ExprId, range: SourceRange) -> ExprId {
        match op {
            BinOp::Assign => self.add_expr(Expr::Set { lhs, rhs }, range),
            _ => {
                let name = self.interner.get_or_intern(op.symbol());
                // TODO: create generic context before parsing operands
                let generic_ctx = self.begin_decl_ref_generic_ctx();
                self.decl_ref(None, name, None, smallvec![lhs, rhs], true, range, generic_ctx)
            }
        }
    }
    pub fn un_op(&mut self, op: UnOp, expr: ExprId, range: SourceRange) -> ExprId {
        match op {
            UnOp::Deref      => self.add_expr(Expr::Deref(expr), range),
            UnOp::AddrOf     => self.add_expr(Expr::AddrOf  { expr, is_mut: false }, range),
            UnOp::AddrOfMut  => self.add_expr(Expr::AddrOf  { expr, is_mut: true  }, range),
            UnOp::Pointer    => self.add_expr(Expr::Pointer { expr, is_mut: false }, range),
            UnOp::PointerMut => self.add_expr(Expr::Pointer { expr, is_mut: true  }, range),
            _ => {
                let name = self.interner.get_or_intern(op.symbol());
                // TODO: create generic context before parsing operand
                let generic_ctx = self.begin_decl_ref_generic_ctx();
                self.decl_ref(None, name, None, smallvec![expr], true, range, generic_ctx)
            },
        }
    }
    pub fn fn_type(&mut self, param_tys: Vec<ExprId>, has_c_variadic_param: bool, ret_ty: ExprId, range: SourceRange) -> ExprId {
        self.add_expr(Expr::FunctionTy { param_tys, has_c_variadic_param, ret_ty }, range)
    }
    pub fn begin_new_file(&mut self, file: SourceFileId) -> AutoPopStackEntry<ScopeState, ModScopeNsId> {
        let mut global_scope = NewNamespace::default();
        // Use all of prelude
        global_scope.blanket_uses.push(Namespace::Mod(self.ast.prelude_namespace.unwrap()));
        let global_scope = self.code.ast.new_namespaces.push(global_scope);
        let global_namespace = self.code.ast.mod_ns.push(
            ModScopeNs {
                scope: global_scope,
                parent: None
            }
        );
        self.code.ast.global_scopes.insert(file, global_scope);
        self.push_to_scope_stack(global_namespace, ScopeState::Mod { id: global_scope, namespace: global_namespace, extern_mod: None })
    }

    fn flush_stmt_buffer(&mut self) {
        self.ast.scope_stack.peek_mut(|state| {
            if let Some(ScopeState::Imper { id, stmt_buffer, .. }) = state {
                if let Some(stmt) = *stmt_buffer {
                    let block = self.code.ast.imper_scopes[*id].block;
                    let op = self.code.ops.push(Op::AstItem { item: Item::Expr(stmt.expr), has_semicolon: stmt.has_semicolon });
                    self.code.blocks[block].ops.push(op);
                    *stmt_buffer = None;
                }
            }
        });
    }

    fn scope_item(&mut self, item: Item, has_semicolon: bool) {
        if let ScopeState::Imper { id, .. } = self.ast.scope_stack.peek().unwrap() {
            let block = self.code.ast.imper_scopes[id].block;
            let op = self.code.ops.push(Op::AstItem { item, has_semicolon });
            self.code.blocks[block].ops.push(op);
        }
    }

    pub fn imper_scoped_decl(&mut self, decl: ImperScopedDecl) {
        if let Some(ScopeState::Imper { namespace, .. }) = self.ast.scope_stack.peek() {
            self.code.ast.imper_ns[namespace].decls.push(decl);
        } else {
            panic!("tried to add imperative-scoped declaration in a non-imperative scope");
        }
    }

    // This is a hack to allow intrinsics to be added for comparing enum types
    pub fn find_nearest_mod_scope(&self) -> Option<NewNamespaceId> {
        for &scope in self.ast.scope_stack.stack.lock().unwrap().borrow().iter().rev() {
            match scope {
                ScopeState::Mod { id, .. } => return Some(id),
                _ => continue,
            }
        }
        None
    }

    pub fn mod_scoped_decl(&mut self, decl: StaticDecl) {
        let id = self.find_nearest_mod_scope().expect("tried to add module-scoped declaration where there is no module scope");
        self.code.ast.new_namespaces[id].static_decls.push(decl);
    }

    pub fn stmt(&mut self, expr: ExprId, has_semicolon: bool) {
        self.flush_stmt_buffer();
        self.ast.scope_stack.peek_mut(|state| {
            if let Some(ScopeState::Imper { stmt_buffer, .. }) = state {
                *stmt_buffer = Some(
                    BufferedStmt {
                        expr,
                        has_semicolon,
                    }
                );
            }
        });
        if has_semicolon {
            self.flush_stmt_buffer();
        }
    }
    pub fn begin_imper_scope(&mut self) -> AutoPopStackEntry<ScopeState, ImperScopeId> {
        let parent = self.cur_namespace();

        let block = self.code.blocks.push(Block::default());
        let id = self.code.ast.imper_scopes.push(
            ImperScope {
                block,
                terminal_expr: VOID_EXPR,
            }
        );
        let namespace = self.code.ast.imper_ns.push(
            ImperScopeNs {
                decls: Vec::new(),
                parent: Some(parent),
            }
        );
        let root = self.get_imper_root().unwrap_or_else(|| {
            self.ast.imper_roots.push(Default::default())
        });
        let scope_entry = self.push_to_scope_stack(
            id,
            ScopeState::Imper {
                id,
                namespace,
                stmt_buffer: None,
                root,
            }
        );
        
        if let Some(func) = self.ast.fn_decl_stack.last_mut() {
            assert!(func.imper_scope_stack > 0 || func.scope.is_none(), "Can't add multiple top-level scopes to a function decl");
            let is_first_scope = func.imper_scope_stack == 0;
            if is_first_scope {
                func.scope = Some(id);
            }
            func.imper_scope_stack += 1;

            if is_first_scope {
                let name = self.code.ast.names[func.id];
                let id = func.id;
    
                let params = func.params.clone();
                let generic_params = func.generic_params.clone();
    
                // Add the current comp decl to the decl scope, to enable recursion
                self.imper_scoped_decl(
                    ImperScopedDecl {
                        name,
                        id
                    }
                );
    
                // Add parameters to decl scope
                for id in range_iter(params.clone()) {
                    self.imper_scoped_decl(
                        ImperScopedDecl {
                            name: self.code.ast.names[id],
                            id,
                        }
                    );
                }
    
                // Add generic parameters to decl scope
                for id in range_iter(generic_params.clone()) {
                    self.imper_scoped_decl(
                        ImperScopedDecl {
                            name: self.code.ast.names[id],
                            id,
                        }
                    );
                }
            }
        }

        scope_entry
    }
    pub fn end_imper_scope(&mut self, _entry: AutoPopStackEntry<ScopeState, ImperScopeId>, has_terminal_expr: bool) {
        if let Some(ScopeState::Imper { id, stmt_buffer, .. }) = self.ast.scope_stack.peek() {
            if has_terminal_expr {
                let terminal_expr = stmt_buffer.expect("must pass terminal expression via Builder::stmt()");
                self.code.ast.imper_scopes[id].terminal_expr = terminal_expr.expr;
            }
        } else {
            panic!("tried to end imperative scope, but the top scope in the stack is not an imperative scope");
        }
    }
    pub fn end_fn_decl(&mut self) {
        let decl_state = self.ast.fn_decl_stack.pop().unwrap();
        if let Decl::Function { ref mut scope, .. } = df!(decl_state.id.ast) {
            *scope = decl_state.scope.unwrap();
        } else {
            panic!("Unexpected decl kind when ending function decl!");
        }
    }
    fn cur_namespace(&self) -> Namespace {
        match self.ast.scope_stack.peek().unwrap() {
            ScopeState::Imper { namespace, .. } => {
                let end_offset = self.code.ast.imper_ns[namespace].decls.len();
                Namespace::Imper { scope: namespace, end_offset }
            },
            ScopeState::Mod { namespace, .. } => Namespace::Mod(namespace),
            ScopeState::ExtendBlock { namespace, .. } => Namespace::ExtendBlock(namespace),
            ScopeState::GenericContext(ns) => Namespace::GenericContext(ns),
            ScopeState::Condition { ns, condition_kind: ConditionKind::Requirement } => Namespace::Requirement(ns),
            ScopeState::Condition { ns, condition_kind: ConditionKind::Guarantee } => Namespace::Guarantee(ns),
        }
    }
    pub fn get_range(&self, item: impl Into<ToSourceRange>) -> SourceRange {
        match item.into() {
            ToSourceRange::Item(item) => match item {
                Item::Expr(expr) => ef!(expr.range),
                Item::Decl(decl) => df!(decl.range),
            }
            ToSourceRange::Op(op) => *self.code.mir.source_ranges.get(&op).unwrap(),
            ToSourceRange::SourceRange(range) => range,
        }
    }

    fn get_imper_root(&self) -> Option<ImperRootId> {
        let stack = self.ast.scope_stack.stack.lock().unwrap();
        let stack = stack.borrow();
        for scope in stack.iter().rev() {
            match *scope {
                ScopeState::Imper { root, .. } => return Some(root),
                ScopeState::Mod { .. } | ScopeState::ExtendBlock { .. } => break,
                ScopeState::Condition { .. } | ScopeState::GenericContext(_) => continue,
            }
        }
        None
    }
}
