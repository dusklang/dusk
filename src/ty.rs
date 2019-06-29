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
    Pointer(Box<Type>),
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
                pointee.fmt(f)?;
                write!(f, "*")
            }
        }
    }
}
