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
                "{}",
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
