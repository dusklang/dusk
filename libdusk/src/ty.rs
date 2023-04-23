use std::fmt;

use arrayvec::ArrayVec;
use bitflags::bitflags;

use index_vec::define_index_type;

use dusk_proc_macros::DuskBridge;

use crate::arch::Arch;
use crate::ast::{StructId, EnumId, GenericParamId, NewNamespaceId, DeclRefId, DeclId};

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum IntWidth {
    W8, W16, W32, W64, Pointer,
}

impl IntWidth {
    pub fn bit_width(&self, arch: Arch) -> usize {
        match self {
            IntWidth::W8 => 8,
            IntWidth::W16 => 16,
            IntWidth::W32 => 32,
            IntWidth::W64 => 64,
            IntWidth::Pointer => arch.pointer_size(),
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq, Debug, Hash)]
pub enum FloatWidth {
    W32, W64,
}

#[derive(Clone, PartialEq, Eq, Default, Hash)]
pub struct FunctionType {
    pub param_tys: Vec<Type>,
    pub return_ty: Box<Type>,
    pub has_c_variadic_param: bool,
}

#[derive(Clone, PartialEq, Eq, Debug, Hash)]
pub struct StructType {
    pub field_tys: Vec<Type>,
    pub identity: StructId,
}

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub enum LegacyInternalType {
    StringLiteral,
}

impl LegacyInternalType {
    pub fn name(self) -> &'static str {
        match self {
            LegacyInternalType::StringLiteral => "StringLiteral",
        }
    }
}

impl From<LegacyInternalType> for Type {
    fn from(internal: LegacyInternalType) -> Self {
        Type::LegacyInternal(internal)
    }
}


define_index_type!(pub struct InternalTypeId = u32;);

#[derive(Clone, PartialEq, Eq, Hash)]
pub struct InternalType {
    pub name: String,
    pub size: usize,
    pub namespace: NewNamespaceId,
}

impl From<InternalTypeId> for Type {
    fn from(internal: InternalTypeId) -> Self {
        Type::Internal(internal)
    }
}

#[derive(Clone, PartialEq, Eq, Hash)]
pub enum TypeVar {
    GenericParamDecl(GenericParamId),
    GenericArg {
        decl_ref: DeclRefId,
        overload: DeclId,
        generic_param_index: usize,
    },
}

#[derive(DuskBridge, Clone, PartialEq, Eq, Hash)]
#[module = ""]
#[name = "type"]
#[variant = "Ty"]
pub enum Type {
    Error,
    Int {
        width: IntWidth,
        is_signed: bool,
    },
    Float(FloatWidth),
    // TODO: Eliminate this separate heap allocation by interning all types into an IndexVec
    Pointer(Box<QualType>),
    Inout(Box<Type>),
    Function(FunctionType),
    Struct(StructType),
    Enum(EnumId),
    /// Used for internal compiler data structures exposed to compile-time code
    /// TODO: Mod and Ty, at minimum, could probably be moved here
    LegacyInternal(LegacyInternalType),
    Internal(InternalTypeId),
    Bool,
    Void,
    Mod,
    Ty,
    TypeVar(TypeVar),
    Never,
}


impl Type {
    pub fn ptr(self) -> Self {
        self.ptr_with_mut(false)
    }

    pub fn mut_ptr(self) -> Self {
        self.ptr_with_mut(true)
    }

    pub fn ptr_with_mut(self, is_mut: bool) -> Self {
        Type::Pointer(
            Box::new(QualType { ty: self, is_mut })
        )
    }

    pub fn inout(self) -> Self {
        Type::Inout(
            Box::new(self)
        )
    }

    pub fn as_function(&self) -> Option<&FunctionType> {
        match self {
            Type::Function(fun) => Some(fun),
            _ => None,
        }
    }

    pub fn return_ty(&self) -> Option<&Type> {
        self.as_function().map(|fun| fun.return_ty.as_ref())
    }

    pub fn deref(&self) -> Option<&QualType> {
        match self {
            Type::Pointer(pointee) => Some(pointee),
            _ => None,
        }
    }

    pub const fn u8() -> Self {
        Type::Int { width: IntWidth::W8, is_signed: false }
    }

    pub const fn u16() -> Self {
        Type::Int { width: IntWidth::W16, is_signed: false }
    }

    pub const fn u32() -> Self {
        Type::Int { width: IntWidth::W32, is_signed: false }
    }

    pub const fn u64() -> Self {
        Type::Int { width: IntWidth::W64, is_signed: false }
    }

    pub const fn usize() -> Self {
        Type::Int { width: IntWidth::Pointer, is_signed: false }
    }

    pub const fn i8() -> Self {
        Type::Int { width: IntWidth::W8, is_signed: true }
    }

    pub const fn i16() -> Self {
        Type::Int { width: IntWidth::W16, is_signed: true }
    }

    pub const fn i32() -> Self {
        Type::Int { width: IntWidth::W32, is_signed: true }
    }

    pub const fn i64() -> Self {
        Type::Int { width: IntWidth::W64, is_signed: true }
    }

    pub const fn isize() -> Self {
        Type::Int { width: IntWidth::Pointer, is_signed: true }
    }

    pub const fn f32() -> Self {
        Type::Float(FloatWidth::W32)
    }

    pub const fn f64() -> Self {
        Type::Float(FloatWidth::W64)
    }

    pub fn trivially_convertible_to(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Never, _other) => true,
            // TODO: this is a hack!
            (_, Type::TypeVar(TypeVar::GenericParamDecl(_))) => true,
            (Type::Pointer(a), Type::Pointer(b)) => a.trivially_convertible_to(b),
            (a, b) => a == b,
        }
    }

    pub fn is_error(&self) -> bool { matches!(self, Type::Error) }
    pub fn is_int(&self) -> bool { matches!(self, Type::Int { .. }) }
}

impl Default for Type {
    fn default() -> Self {
        Type::Error
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Error => write!(f, "error"),
            Type::Never => write!(f, "never"),
            Type::Bool => write!(f, "bool"),
            Type::Void => write!(f, "void"),
            Type::Mod => write!(f, "module"),
            Type::Ty => write!(f, "type"),
            Type::Int { width, is_signed } => write!(
                f,
                "{}{}", 
                if *is_signed { 'i' } else { 'u' },
                match width {
                    IntWidth::W8 => "8",
                    IntWidth::W16 => "16",
                    IntWidth::W32 => "32",
                    IntWidth::W64 => "64",
                    IntWidth::Pointer => "size",
                }
            ),
            Type::Float(width) => write!(
                f,
                "f{}",
                match width {
                    FloatWidth::W32 => 32,
                    FloatWidth::W64 => 64,
                }
            ),
            Type::Pointer(pointee) => {
                pointee.ty.fmt(f)?;
                if pointee.is_mut { 
                    write!(f, " *mut")
                } else {
                    write!(f, "*")
                }
            },
            Type::Inout(ty) => write!(f, "inout {:?}", ty),
            Type::Function(fun) => fun.fmt(f),
            // TODO: print out fields (issue #76)
            Type::Struct(ty) => {
                write!(f, "struct{} {{ ", ty.identity.index())?;
                for (i, ty) in ty.field_tys.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{:?}", ty)?;
                }
                write!(f, " }}")
            },
            &Type::Enum(id) => {
                write!(f, "enum{}", id.index())
            }
            Type::TypeVar(var) => match var {
                &TypeVar::GenericParamDecl(id) => {
                    write!(f, "generic_param{}", id.index())
                },
                TypeVar::GenericArg { .. } => panic!("should be impossible"),
            },
            Type::LegacyInternal(internal) => write!(f, "{}", internal.name()),
            &Type::Internal(id) => write!(f, "internal_type{}", id.index()),
        }
    }
}

impl fmt::Debug for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "fn(")?;
        for (i, param) in self.param_tys.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{:?}", param)?;
        }
        write!(f, "): {:?}", self.return_ty)
    }
}

#[derive(Clone, PartialEq, Eq, Debug, Default, Hash)]
pub struct QualType {
    pub ty: Type,
    pub is_mut: bool,
}

impl QualType {
    pub fn trivially_convertible_to(&self, other: &QualType) -> bool {
        if let Type::Inout(ty) = &other.ty {
            if !self.is_mut {
                return false;
            } else {
                return self.ty.trivially_convertible_to(ty);
            }
        }
        if !self.is_mut && other.is_mut {
            return false;
        }
        self.ty.trivially_convertible_to(&other.ty)
    }

    pub fn ptr(self) -> Type {
        Type::Pointer(Box::new(self))
    }
}

impl From<Type> for QualType {
    fn from(ty: Type) -> Self {
        Self {
            ty, 
            is_mut: false,
        }
    }
}

impl<'a> From<&Type> for QualType {
    fn from(ty: &Type) -> Self {
        QualType::from(ty.clone())
    }
}

bitflags! {
    #[derive(Default)]
    pub struct BuiltinTraits: u8 {
        const INT  = 0b0000_0001;
        // ExpressibleByDecimalLiteral inherits from ExpressibleByIntegerLiteral
        const DEC  = 0b0000_0011;
        const CHAR = 0b0000_0100;
        // ExpressibleByStringLiteral inherits from ExpressibleByCharacterLiteral
        const STR  = 0b0000_1100;
    }
}

impl BuiltinTraits {
    pub fn names(self) -> ArrayVec<&'static str, 4> {
        let mut names = ArrayVec::new();
        if self.contains(BuiltinTraits::INT) {
            names.push("ExpressibleByIntegerLiteral");
        }
        if self.contains(BuiltinTraits::DEC) {
            names.push("ExpressibleByDecimalLiteral");
        }
        if self.contains(BuiltinTraits::CHAR) {
            names.push("ExpressibleByCharacterLiteral");
        }
        if self.contains(BuiltinTraits::STR) {
            names.push("ExpressibleByStringLiteral");
        }
        names
    }
}
