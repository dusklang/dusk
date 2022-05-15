use std::fmt::Display;

pub trait IntoBytes {
    type Bytes: IntoIterator<Item=u8>;
    fn into_bytes(&self) -> Self::Bytes;
}
macro_rules! into_bytes_impl {
    ($($ty:ty),*) => {
        $(
            impl IntoBytes for $ty {
                type Bytes = [u8; std::mem::size_of::<Self>()];
                fn into_bytes(&self) -> Self::Bytes { self.to_le_bytes() }
            }
        )*
    }
}
into_bytes_impl!(u8, u16, u32, u64, i8, i16, i32, i64);

trait Register {
    fn main_bits(&self) -> u8;
    fn ext(&self) -> bool;
}

macro_rules! define_registers {
    ($($bit8:ident $bit16:ident $bit32:ident $bit64:ident $xmm:ident $ymm:ident $control32:ident $debug32:ident),*$(,)?) => {
        #[repr(u8)]
        #[allow(unused)]
        #[derive(Clone, Copy, Debug)]
        pub enum Reg8 {
            $($bit8),*,

            // When bit 4 is set (as it is with these registers), there should be a REX prefix (with the relevant
            // extension bit set to 0).
            Spl = 0x14,
            Bpl,
            Sil,
            Dil,
        }

        impl Register for Reg8 {
            fn main_bits(&self) -> u8 {
                *self as u8 & 7
            }

            fn ext(&self) -> bool {
                (*self as u8 & 8) != 0
            }
        }
        impl Reg8 {
            #[allow(unused)]
            fn requires_rex(&self) -> bool {
                self.ext() || (*self as u8 & 16) != 0
            }
        }
        impl Display for Reg8 {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", format!("{:?}", self).to_lowercase())
            }
        }

        define_registers!(@define_enum Reg16 $($bit16),*);
        define_registers!(@define_enum Reg32 $($bit32),*);
        define_registers!(@define_enum Reg64 $($bit64),*);
        define_registers!(@define_enum RegXmm $($xmm),*);
        define_registers!(@define_enum RegYmm $($ymm),*);
        define_registers!(@define_enum RegControl $($control32),*);
        define_registers!(@define_enum RegDebug $($debug32),*);
    };
    (@define_enum $name:ident $($variant:ident),*) => {
        #[repr(u8)]
        #[allow(unused)]
        #[derive(Clone, Copy, Debug)]
        pub enum $name { $($variant),* }

        impl Register for $name {
            #[allow(unused)]
            fn main_bits(&self) -> u8 {
                *self as u8 & 7
            }

            #[allow(unused)]
            fn ext(&self) -> bool {
                (*self as u8 & 8) != 0
            }
        }

        impl Display for $name {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                write!(f, "{}", format!("{:?}", self).to_lowercase())
            }
        }
    };
}

// TODO: define MMX, XMM, YMM, CR and DR registers separately since they have very regular patterns and it sucks to
// have them laboriously defined here. One potential problem is concatenating the names with the numbers, but I think
// crates exist that can handle that.
define_registers!(
    Al   Ax   Eax  Rax Xmm0  Ymm0  Cr0  Dr0,
    Cl   Cx   Ecx  Rcx Xmm1  Ymm1  Cr1  Dr1,
    Dl   Dx   Edx  Rdx Xmm2  Ymm2  Cr2  Dr2,
    Bl   Bx   Ebx  Rbx Xmm3  Ymm3  Cr3  Dr3,
    Ah   Sp   Esp  Rsp Xmm4  Ymm4  Cr4  Dr4,
    Ch   Bp   Ebp  Rbp Xmm5  Ymm5  Cr5  Dr5,
    Dh   Si   Esi  Rsi Xmm6  Ymm6  Cr6  Dr6,
    Bh   Di   Edi  Rdi Xmm7  Ymm7  Cr7  Dr7,
    R8l  R8w  R8d  R8  Xmm8  Ymm8  Cr8  Dr8,
    R9l  R9w  R9d  R9  Xmm9  Ymm9  Cr9  Dr9,
    R10l R10w R10d R10 Xmm10 Ymm10 Cr10 Dr10,
    R11l R11w R11d R11 Xmm11 Ymm11 Cr11 Dr11,
    R12l R12w R12d R12 Xmm12 Ymm12 Cr12 Dr12,
    R13l R13w R13d R13 Xmm13 Ymm13 Cr13 Dr13,
    R14l R14w R14d R14 Xmm14 Ymm14 Cr14 Dr14,
    R15l R15w R15d R15 Xmm15 Ymm15 Cr15 Dr15,
);
define_registers!(@define_enum RegMmx Mmx0, Mmx1, Mmx2, Mmx3, Mmx4, Mmx5, Mmx6, Mmx7);
define_registers!(@define_enum RegX87 St0, St1, St2, St3, St4, St5, St6, St7);
define_registers!(@define_enum RegSegment Es, Cs, Ss, Ds, Fs, Gs);

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct RexBuilder(u8);
impl RexBuilder {
    pub fn new32() -> Self { Self(0x40) }
    pub fn new() -> Self { Self::new32().w_bit(true) }

    /// 1 for 64-bit operands. 0 for default, which is usually (but not always) 32-bit
    pub fn w_bit(self, bit: bool) -> Self {
        self.add_bit(3, bit)
    }

    /// Extends the MODRM.reg field
    pub fn r_bit(self, bit: bool) -> Self {
        self.add_bit(2, bit)
    }

    /// Extends the SIB.index field
    #[allow(unused)]
    pub fn x_bit(self, bit: bool) -> Self {
        self.add_bit(1, bit)
    }

    /// Extends the MODRM.rm field or the SIB.base field
    pub fn b_bit(self, bit: bool) -> Self {
        self.add_bit(0, bit)
    }

    fn add_bit(self, placement: u8, bit: bool) -> Self {
        Self(((self.0) & !(1 << (placement))) | ((bit as u8) << placement))
    }
}

fn build_modrm(mawd: u8, reg: u8, rm: u8) -> u8 {
    debug_assert!(mawd < 4);
    debug_assert!(reg < 8);
    debug_assert!(rm < 8);

    (mawd << 6) | (reg << 3) | (rm << 0)
}

fn build_sib(scale: u8, index: u8, base: u8) -> u8 {
    debug_assert!(scale < 4);
    debug_assert!(index < 8);
    debug_assert!(base < 8);

    (scale << 6) | (index << 3) | (base << 0)
}

impl IntoBytes for RexBuilder {
    type Bytes = [u8; 1];
    fn into_bytes(&self) -> Self::Bytes { self.0.into_bytes() }
}

#[derive(Default)]
pub struct X64Encoder {
    data: Vec<u8>,

    // TODO: use cfg attributes to disable this field and all associated code in a release build
    debug: bool,
}

#[derive(Debug)]
pub struct MemoryLoc64 {
    base: Reg64,
    offset: i32,
}

impl Display for MemoryLoc64 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}", self.base)?;
        if self.offset > 0 {
            write!(f, "+{}", self.offset)?;
        } else if self.offset < 0 {
            write!(f, "{}", self.offset)?;
        }
        write!(f, "]")
    }
}

impl X64Encoder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn enable_debug(&mut self, debug: bool) {
        self.debug = debug;
    }

    fn begin_instr_no_operands(&self, mnemonic: &str) {
        if self.debug {
            println!("{}", mnemonic);
        }
    }
    fn begin_instr(&self, mnemonic: &str, operand_a: &impl Display, operand_b: &impl Display) {
        if self.debug {
            println!("{} {}, {}", mnemonic, operand_a, operand_b);
        }
    }

    pub fn push_any<Val: IntoBytes>(&mut self, val: Val) {
        self.data.extend(val.into_bytes());
    }
    pub fn push(&mut self, byte: u8) {
        self.push_any(byte);
    }

    pub fn sub64_imm(&mut self, reg: Reg64, imm: i32) {
        self.begin_instr("sub", &reg, &imm);
        self.push_any(RexBuilder::new().b_bit(reg.ext()));
        self.push(0x83);
        self.push(build_modrm(0b11, 0b101, reg.main_bits()));
        self.push_any::<i8>(imm.try_into().unwrap());
    }

    pub fn add64_imm(&mut self, reg: Reg64, imm: i32) {
        self.begin_instr("add", &reg, &imm);
        self.push_any(RexBuilder::new().b_bit(reg.ext()));
        self.push(0x83);
        self.push(build_modrm(0b11, 0b000, reg.main_bits()));
        self.push_any::<i8>(imm.try_into().unwrap());
    }

    fn mov32_64_impl(&mut self, bit64: bool, opcode: u8, reg: impl Register, loc: MemoryLoc64) {
        if bit64 || reg.ext() || loc.base.ext() {
            self.push_any(RexBuilder::new32().w_bit(bit64).r_bit(reg.ext()).b_bit(loc.base.ext()));
        }
        self.push(opcode);
        if loc.offset == 0 {
            match loc.base.main_bits() {
                // BP and R13 both have the main bits 101, which is interpreted as a special value for IP-relative
                // addressing. The upshot is to actually encode these registers in the normal way, you need a different
                // mod field, plus a zero immediate offset.
                0b101 => {
                    self.push(build_modrm(0b01, reg.main_bits(), loc.base.main_bits()));
                    self.push(0);
                },
                // SP and R12 both have the main bits 100, which is interpreted as a special value meaning the SIB byte
                // is present. This enables a wide variety of more advanced addressing modes. The upshot is to actually
                // encode these registers in the normal way, you need an SIB byte.
                0b100 => {
                    self.push(build_modrm(0b00, reg.main_bits(), loc.base.main_bits()));
                    self.push(build_sib(0, 0b100, loc.base.main_bits()));
                },
                _ => self.push(build_modrm(0b00, reg.main_bits(), loc.base.main_bits())),
            }
        } else if let Ok(offset) = TryInto::<i8>::try_into(loc.offset) {
            match loc.base.main_bits() {
                // SP and R12 both have the main bits 100, which is interpreted as a special value meaning the SIB byte
                // is present. This enables a wide variety of more advanced addressing modes. The upshot is to actually
                // encode these registers in the normal way, you need an SIB byte.
                0b100 => {
                    self.push(build_modrm(0b01, reg.main_bits(), loc.base.main_bits()));
                    self.push(build_sib(0, 0b100, loc.base.main_bits()));
                },
                _ => {
                    self.push(build_modrm(0b01, reg.main_bits(), loc.base.main_bits()));
                }
            }
            self.push_any(offset);
        } else {
            match loc.base.main_bits() {
                // SP and R12 both have the main bits 100, which is interpreted as a special value meaning the SIB byte
                // is present. This enables a wide variety of more advanced addressing modes. The upshot is to actually
                // encode these registers in the normal way, you need an SIB byte.
                0b100 => {
                    self.push(build_modrm(0b10, reg.main_bits(), loc.base.main_bits()));
                    self.push(build_sib(0b00, 0b100, loc.base.main_bits()));
                },
                _ => {
                    self.push(build_modrm(0b10, reg.main_bits(), loc.base.main_bits()));
                }
            }
            self.push_any(loc.offset);
        }
    }

    fn mov64_impl(&mut self, opcode: u8, reg: Reg64, loc: MemoryLoc64) {
        self.mov32_64_impl(true, opcode, reg, loc);
    }

    pub fn load64(&mut self, dest: Reg64, src: impl Into<MemoryLoc64>) {
        let src = src.into();
        self.begin_instr("mov", &dest, &src);
        self.mov64_impl(0x8b, dest, src);
    }

    pub fn store64(&mut self, dest: impl Into<MemoryLoc64>, src: Reg64) {
        let dest = dest.into();
        self.begin_instr("mov", &dest, &src);
        self.mov64_impl(0x89, src, dest);
    }

    fn mov32_impl(&mut self, opcode: u8, reg: Reg32, loc: MemoryLoc64) {
        self.mov32_64_impl(false, opcode, reg, loc);
    }

    pub fn load32(&mut self, dest: Reg32, src: impl Into<MemoryLoc64>) {
        let src = src.into();
        self.begin_instr("mov", &dest, &src);
        self.mov32_impl(0x8b, dest, src);
    }

    pub fn store32(&mut self, dest: impl Into<MemoryLoc64>, src: Reg32) {
        let dest = dest.into();
        self.begin_instr("mov", &dest, &src);
        self.mov32_impl(0x89, src, dest);
    }

    pub fn ret(&mut self) {
        self.begin_instr_no_operands("ret");
        self.push(0xc3);
    }

    pub fn allocate(self) -> region::Allocation {
        let mut thunk = region::alloc(self.data.len(), region::Protection::READ_WRITE_EXECUTE).unwrap();
        unsafe {
            let thunk_ptr = thunk.as_mut_ptr::<u8>();
            thunk_ptr.copy_from(self.data.as_ptr(), self.data.len());
        }
        thunk
    }
}

impl std::ops::Add<i32> for Reg64 {
    type Output = MemoryLoc64;

    fn add(self, rhs: i32) -> Self::Output {
        MemoryLoc64 { base: self, offset: rhs }
    }
}
impl std::ops::Add<i32> for MemoryLoc64 {
    type Output = MemoryLoc64;

    fn add(mut self, rhs: i32) -> Self::Output {
        self.offset += rhs;
        self
    }
}
impl From<Reg64> for MemoryLoc64 {
    fn from(base: Reg64) -> Self { Self { base, offset: 0 } }
}