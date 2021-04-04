use arrayvec::ArrayVec;
use bitflags::bitflags;

use mire::ty::{Type, IntWidth};

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

pub trait ImplementsTraits {
    fn implements_traits(&self, traits: BuiltinTraits) -> Result<(), BuiltinTraits>;
}

impl ImplementsTraits for Type {
    fn implements_traits(&self, traits: BuiltinTraits) -> Result<(), BuiltinTraits> {
        let mut not_implemented = BuiltinTraits::empty();
        fn expressible_by_str_lit(ty: &Type) -> bool {
            if let Type::Pointer(pointee) = ty {
                matches!(pointee.ty, Type::Int { width: IntWidth::W8, .. }) && !pointee.is_mut
            } else {
                false
            }
        }
        let mut check_implements = |trayt: BuiltinTraits, check: fn(&Type) -> bool| {
            if traits.contains(trayt) && !check(self) {
                not_implemented |= trayt;
            }
        };
        check_implements(BuiltinTraits::INT, |ty| matches!(ty, Type::Int { .. } | Type::Float(_)));
        check_implements(BuiltinTraits::DEC, |ty| matches!(ty, Type::Float(_)));
        check_implements(BuiltinTraits::CHAR, |ty| {
            matches!(ty, Type::Int { width: IntWidth::W8, .. }) || expressible_by_str_lit(ty)
        });
        check_implements(BuiltinTraits::STR, expressible_by_str_lit);

        if not_implemented.is_empty() {
            Ok(())
        } else {
            Err(not_implemented)
        }
    }
}