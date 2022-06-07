use std::fmt;

use crate::arch::Arch;
use crate::hir::{StructId, EnumId, GenericParamId};
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
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

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum FloatWidth {
    W32, W64,
}

#[derive(Clone, PartialEq, Eq, Default)]
pub struct FunctionType {
    pub param_tys: Vec<Type>,
    pub return_ty: Box<Type>,
}

#[derive(Clone, PartialEq, Eq)]
pub struct StructType {
    pub field_tys: Vec<Type>,
    pub identity: StructId,
}

#[derive(Copy, Clone, PartialEq, Eq)]
pub enum InternalType {
    StringLiteral,
}

impl InternalType {
    pub fn name(self) -> &'static str {
        match self {
            InternalType::StringLiteral => "StringLiteral",
        }
    }
}

impl From<InternalType> for Type {
    fn from(internal: InternalType) -> Self {
        Type::Internal(internal)
    }
}

#[derive(Clone, PartialEq, Eq)]
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
    Internal(InternalType),
    Bool,
    Void,
    Mod,
    Ty,
    GenericParam(GenericParamId),
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
        if let Type::Function(fun) = self {
            Some(fun)
        } else {
            None
        }
    }

    pub fn return_ty(&self) -> Option<&Type> {
        self.as_function().map(|fun| fun.return_ty.as_ref())
    }

    pub fn deref(&self) -> Option<&QualType> {
        if let Type::Pointer(pointee) = self {
            Some(pointee)
        } else {
            None
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
            (Type::Pointer(a), Type::Pointer(b)) => a.trivially_convertible_to(b),
            (_self, Type::GenericParam(_)) => true,
            (a, b) => a == b,
        }
    }
}

impl Default for Type {
    fn default() -> Self {
        Type::Error
    }
}

impl fmt::Debug for Type {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Type::Error => write!(f, "<ERROR>"),
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
            &Type::GenericParam(id) => {
                write!(f, "generic_param{}", id.index())
            },
            Type::Internal(internal) => write!(f, "{}", internal.name()),
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

#[derive(Clone, PartialEq, Eq, Debug, Default)]
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
                return self.ty.trivially_convertible_to(&ty);
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