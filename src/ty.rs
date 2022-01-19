use arrayvec::ArrayVec;
use bitflags::bitflags;

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
