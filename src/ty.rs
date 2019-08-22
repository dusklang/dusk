use std::fmt;

use bitflags::bitflags;

use crate::arch::Arch;

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

bitflags! {
    pub struct BuiltinTraits: u8 {
        const INT  = 0b0000_0001;
        // ExpressibleByDecimalLiteral inherits from ExpressibleByIntLiteral
        const DEC  = 0b0000_0011;
        const CHAR = 0b0000_0100;
        // ExpressibleByStringLiteral inherits from ExpressibleByCharLiteral
        const STR  = 0b0000_1100;
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
    // TODO: Eliminate this separate heap allocation by interning all types into an IdxVec
    Pointer(Box<QualType>),
    Bool,
    Void,
    Never,
}

impl Type {
    // TODO: Get rid of expressible_by_XXX methods and put the logic in here?
    pub fn implements_traits(&self, traits: BuiltinTraits) -> Result<(), BuiltinTraits> {
        let mut not_implemented = BuiltinTraits::empty();
        if traits.contains(BuiltinTraits::INT) && !self.expressible_by_int_lit() {
            not_implemented |= BuiltinTraits::INT;
        }
        if traits.contains(BuiltinTraits::DEC) && !self.expressible_by_dec_lit() {
            not_implemented |= BuiltinTraits::DEC;
        }
        if traits.contains(BuiltinTraits::CHAR) && !self.expressible_by_char_lit() {
            not_implemented |= BuiltinTraits::CHAR;
        }
        if traits.contains(BuiltinTraits::STR) && !self.expressible_by_str_lit() {
            not_implemented |= BuiltinTraits::STR;
        }

        if not_implemented.is_empty() {
            Ok(())
        } else {
            Err(not_implemented)
        }
    }

    /// Size of an instance of the type in bytes
    pub fn size(&self, arch: Arch) -> usize {
        match self {
            Type::Error | Type::Void | Type::Never => 0,
            Type::Int { width, .. } => {
                let bit_width = width.bit_width(arch);
                assert_eq!(bit_width % 8, 0, "Unexpected bit width: not a multiple of eight!");
                bit_width / 8
            },
            Type::Float(width) => match width {
                FloatWidth::W32 => 32 / 8,
                FloatWidth::W64 => 64 / 8,
            },
            Type::Pointer(_) => {
                let bit_width = arch.pointer_size();
                assert_eq!(bit_width % 8, 0, "Unexpected bit width: not a multiple of eight!");
                bit_width / 8
            },
            Type::Bool => 1,
        }
    }

    pub fn expressible_by_int_lit(&self) -> bool {
        match self {
            Type::Int { .. } | Type::Float(_) => true,
            _ => false,
        }
    }

    pub fn expressible_by_dec_lit(&self) -> bool {
        if let Type::Float(_) = self { true } else { false }
    }

    pub fn expressible_by_str_lit(&self) -> bool {
        if let Type::Pointer(pointee) = self {
            if let Type::Int { width: IntWidth::W8, .. } = &pointee.ty {
                !pointee.is_mut
            } else {
                false
            }
        } else {
            false
        }
    }

    pub fn expressible_by_char_lit(&self) -> bool {
        if let Type::Int { width: IntWidth::W8, .. } = self {
            true
        } else {
            self.expressible_by_str_lit()
        }
    }

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