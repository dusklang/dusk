use std::fmt;

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum IntWidth {
    W8, W16, W32, W64,
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum FloatWidth {
    W32, W64,
}

#[derive(Clone, PartialEq, Eq)]
pub enum Type {
    Error,
    Int {
        width: IntWidth,
        is_signed: bool,
    },
    Float(FloatWidth),
    // TODO: Eliminate this separate heap allocation by interning all types into an IdxVec
    Pointer(Box<QualType>),
    Bool,
    Void,
    Never,
}

impl Type {
    pub fn expressible_by_int_lit(&self) -> bool {
        match self {
            Type::Int { .. } | Type::Float(_) => true,
            _ => false,
        }
    }

    pub fn expressible_by_dec_lit(&self) -> bool {
        if let Type::Float(_) = self { true } else { false }
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

    pub const fn f32() -> Self {
        Type::Float(FloatWidth::W32)
    }

    pub const fn f64() -> Self {
        Type::Float(FloatWidth::W64)
    }

    pub fn trivially_convertible_to(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Never, other) => true,
            (Type::Pointer(a), Type::Pointer(b)) => a.trivially_convertible_to(b),
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
        match &self {
            Type::Error => write!(f, "<ERROR>"),
            Type::Never => write!(f, "never"),
            Type::Bool => write!(f, "bool"),
            Type::Void => write!(f, "void"),
            Type::Int { width, is_signed } => write!(
                f,
                "{}{}", 
                if *is_signed { 'i' } else { 'u' },
                match width {
                    IntWidth::W8 => 8,
                    IntWidth::W16 => 16,
                    IntWidth::W32 => 32,
                    IntWidth::W64 => 64,
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
            }
        }
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub struct QualType {
    pub ty: Type,
    pub is_mut: bool,
}

impl QualType {
    pub fn trivially_convertible_to(&self, other: &QualType) -> bool {
        if !self.is_mut && other.is_mut {
            return false;
        }
        self.ty.trivially_convertible_to(&other.ty)
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

impl<'a> From<Type> for &QualType {
    fn from(ty: Type) -> Self {
        &QualType::from(ty)
    }
}

impl<'a> From<&Type> for &QualType {
    fn from(ty: &Type) -> Self {
        &QualType::from(ty.clone())
    }
}