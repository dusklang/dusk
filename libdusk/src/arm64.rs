use libc::{pthread_jit_write_protect_np, c_void, size_t};

#[link(name="c")]
extern {
    fn sys_icache_invalidate(start: *mut c_void, len: size_t);
}

#[derive(Default)]
pub struct Arm64Encoder {
    data: Vec<u8>,
}

#[repr(u32)]
#[allow(unused)]
pub enum Reg {
    R0, R1, R2, R3, R4, R5, R6, R7, R8, R9, R10, R11, R12, R13, R14, R15,
    R16, R17, R18, R19, R20, R21, R22, R23, R24, R25, R26, R27, R28, R29, R30,
}

#[allow(unused)]
impl Reg {
    pub const ZERO: RegOrZero = RegOrZero::Zero;
    pub const SP: RegOrSp = RegOrSp::Sp;
    pub const FP: Reg = Reg::R29;
    pub const LR: Reg = Reg::R30;
}

pub enum RegOrZero {
    Reg(Reg),
    Zero,
}

pub enum RegOrSp {
    Reg(Reg),
    Sp,
}


impl From<Reg> for RegOrZero {
    fn from(value: Reg) -> Self {
        Self::Reg(value)
    }
}

impl From<Reg> for RegOrSp {
    fn from(value: Reg) -> Self {
        Self::Reg(value)
    }
}

trait Register {
    fn encoding(self) -> u32;
}

impl Register for Reg {
    fn encoding(self) -> u32 {
        self as u32
    }
}

impl Register for RegOrZero {
    fn encoding(self) -> u32 {
        match self {
            RegOrZero::Reg(reg) => reg.encoding(),
            RegOrZero::Zero => 31,
        }
    }
}

impl Register for RegOrSp {
    fn encoding(self) -> u32 {
        match self {
            RegOrSp::Reg(reg) => reg.encoding(),
            RegOrSp::Sp => 31,
        }
    }
}

pub struct InstrEncoder {
    val: u32,
    valid_bits: u32,
}

impl InstrEncoder {
    fn new() -> Self {
        Self {
            val: 0,
            valid_bits: 0,
        }
    }

    fn push_value_of_size(&mut self, value: u32, bits: u32) {
        assert!(self.valid_bits + bits <= 32);
        assert!(value <= ((1 << bits as u64) - 1) as u32);
        self.val |= value << (32 - self.valid_bits - bits);
        self.valid_bits += bits;
    }

    fn push_reg(&mut self, reg: impl Register) {
        self.push_value_of_size(reg.encoding(), 5);
    }

    fn get_instr(self) -> u32 {
        assert_eq!(self.valid_bits, 32);
        let val = self.val;
        std::mem::forget(self);
        val
    }
}

impl Drop for InstrEncoder {
    fn drop(&mut self) {
        panic!("must call Instr::get_instr()")
    }
}

impl Arm64Encoder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn push(&mut self, encoder: InstrEncoder) {
        self.data.extend(encoder.get_instr().to_le_bytes());
    }

    pub fn sub64_imm(&mut self, l_shift_by_12: bool, dest: impl Into<RegOrSp>, minuend: impl Into<RegOrSp>, subtrahend: u16) {
        assert!(subtrahend < (1 << 12));
        
        let mut instr = InstrEncoder::new();

        // 64-bit
        instr.push_value_of_size(1, 1);

        instr.push_value_of_size(0xA2, 8);
        instr.push_value_of_size(l_shift_by_12 as u32, 1);
        instr.push_value_of_size(subtrahend as u32, 12);
        instr.push_reg(minuend.into());
        instr.push_reg(dest.into());

        self.push(instr);
    }

    pub fn add64_imm(&mut self, l_shift_by_12: bool, dest: impl Into<RegOrSp>, addend1: impl Into<RegOrSp>, addend2: u16) {
        assert!(addend2 < (1 << 12));
        
        let mut instr = InstrEncoder::new();

        // 64-bit
        instr.push_value_of_size(1, 1);

        instr.push_value_of_size(0x22, 8);
        instr.push_value_of_size(l_shift_by_12 as u32, 1);
        instr.push_value_of_size(addend2 as u32, 12);
        instr.push_reg(addend1.into());
        instr.push_reg(dest.into());

        self.push(instr);
    }

    fn access_pair_impl64(&mut self, is_load: bool, val1: Reg, val2: Reg, addr: impl Into<RegOrSp>, imm: i16) {
        assert!(imm >= -512 && imm <= 504 && imm % 8 == 0);
        let imm7 = imm / 8;

        let mut instr = InstrEncoder::new();

        // 64-bit
        instr.push_value_of_size(1, 1);

        instr.push_value_of_size(0x52, 8);

        instr.push_value_of_size(is_load as u32, 1);

        instr.push_value_of_size((imm7 as u16 & 0x7f) as u32, 7);
        instr.push_reg(val2);
        instr.push_reg(addr.into());
        instr.push_reg(val1);

        self.push(instr);
    }

    pub fn stp64(&mut self, src1: Reg, src2: Reg, dest_addr: impl Into<RegOrSp>, imm: i16) {
        self.access_pair_impl64(false, src1, src2, dest_addr, imm);
    }

    pub fn ldp64(&mut self, dest1: Reg, dest2: Reg, src_addr: impl Into<RegOrSp>, imm: i16) {
        self.access_pair_impl64(true, dest1, dest2, src_addr, imm);
    }

    fn str_impl(&mut self, bits64: bool, src: Reg, dest_addr: impl Into<RegOrSp>, imm12: u16) {
        let mut instr = InstrEncoder::new();

        // size
        instr.push_value_of_size(2 | (bits64 as u32), 2);

        instr.push_value_of_size(0xE4, 8);

        instr.push_value_of_size(imm12 as u32, 12);
        instr.push_reg(dest_addr.into());
        instr.push_reg(src);

        self.push(instr);
    }

    pub fn str64(&mut self, src: Reg, dest_addr: impl Into<RegOrSp>, imm: u16) {
        assert!(imm <= 32760 && imm % 8 == 0);
        let imm12 = imm / 8;
        self.str_impl(true, src, dest_addr, imm12);
    }

    pub fn str32(&mut self, src: Reg, dest_addr: impl Into<RegOrSp>, imm: u16) {
        assert!(imm <= 16380 && imm % 4 == 0);
        let imm12 = imm / 8;
        self.str_impl(false, src, dest_addr, imm12);
    }

    fn ldr_impl(&mut self, bits64: bool, dest: Reg, src_addr: impl Into<RegOrSp>, imm12: u16) {
        let mut instr = InstrEncoder::new();

        // size
        instr.push_value_of_size(2 | bits64 as u32, 2);

        instr.push_value_of_size(0xE5, 8);

        instr.push_value_of_size(imm12 as u32, 12);
        instr.push_reg(src_addr.into());
        instr.push_reg(dest);

        self.push(instr);
    }

    pub fn ldr64(&mut self, dest: Reg, src_addr: impl Into<RegOrSp>, imm: u16) {
        assert!(imm <= 32760 && imm % 8 == 0);
        let imm12 = imm / 8;
        self.ldr_impl(true, dest, src_addr, imm12);
    }

    pub fn ldr32(&mut self, dest: Reg, src_addr: impl Into<RegOrSp>, imm: u16) {
        assert!(imm <= 16380 && imm % 4 == 0);
        let imm12 = imm / 4;
        self.ldr_impl(false, dest, src_addr, imm12);
    }

    pub fn bl(&mut self, offset: i32) {
        assert!(offset >= -134217728 && offset <= 134217724 && offset % 4 == 0);
        let imm26 = ((offset / 4) as u32) & 0x03FF_FFFF;

        let mut instr = InstrEncoder::new();
        instr.push_value_of_size(0x25, 6);

        instr.push_value_of_size(imm26, 26);

        self.push(instr);
    }

    pub fn ret(&mut self, return_addr: Reg) {
        let mut instr = InstrEncoder::new();
        instr.push_value_of_size(0x35_97c0, 22);
        instr.push_reg(return_addr);
        instr.push_value_of_size(0, 5);

        self.push(instr);
    }

    pub fn allocate(self) -> region::Allocation {
        let mut thunk = region::alloc(self.data.len(), region::Protection::WRITE_EXECUTE).unwrap();
        unsafe {
            pthread_jit_write_protect_np(0);
            let thunk_ptr = thunk.as_mut_ptr::<u8>();
            thunk_ptr.copy_from(self.data.as_ptr(), self.data.len());
            pthread_jit_write_protect_np(1);
            sys_icache_invalidate(thunk_ptr as *mut _, self.data.len());
        }
        thunk
    }
}
