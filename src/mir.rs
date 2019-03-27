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
    Bool,
    Void,
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

    pub fn convertible_to(&self, other: &Type) -> bool {
        self == other
    }

    pub fn u8() -> Self {
        Type::Int { width: IntWidth::W8, is_signed: false }
    }

    pub fn u16() -> Self {
        Type::Int { width: IntWidth::W16, is_signed: false }
    }

    pub fn u32() -> Self {
        Type::Int { width: IntWidth::W32, is_signed: false }
    }

    pub fn u64() -> Self {
        Type::Int { width: IntWidth::W64, is_signed: false }
    }

    pub fn i8() -> Self {
        Type::Int { width: IntWidth::W8, is_signed: true }
    }

    pub fn i16() -> Self {
        Type::Int { width: IntWidth::W16, is_signed: true }
    }

    pub fn i32() -> Self {
        Type::Int { width: IntWidth::W32, is_signed: true }
    }

    pub fn i64() -> Self {
        Type::Int { width: IntWidth::W64, is_signed: true }
    }

    pub fn f32() -> Self {
        Type::Float(FloatWidth::W32)
    }

    pub fn f64() -> Self {
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
            Type::Bool => write!(f, "bool"),
            Type::Void => write!(f, "void"),
        }
    }
}
