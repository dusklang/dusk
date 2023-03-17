#[cfg(target_arch="aarch64")]
use libc::{pthread_jit_write_protect_np, c_void, size_t};

#[cfg(target_arch="aarch64")]
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
#[derive(Copy, Clone)]
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

#[repr(u32)]
#[derive(Copy, Clone)]
enum DataSize {
    Bits8,
    Bits16,
    Bits32,
    Bits64,
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

#[allow(unused)]
impl Arm64Encoder {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_bytes(self) -> Vec<u8> {
        self.data
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
        assert!(addend2 < (1 << 12), "{}", addend2);
        
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

    fn adr_impl(&mut self, l_shift_by_12: bool, dest: Reg, imm: i32) {
        assert!(imm >= -1048576 && imm <= 1048575);

        let mut instr = InstrEncoder::new();

        instr.push_value_of_size(l_shift_by_12 as u32, 1);
        instr.push_value_of_size((imm & 0b11) as u32, 2);
        instr.push_value_of_size(0x10, 5);
        instr.push_value_of_size(((imm >> 2) & 0x0007_FFFF) as u32, 19);
        instr.push_reg(dest);

        self.push(instr);
    }

    pub fn adrp(&mut self, dest: Reg, imm: i32) {
        self.adr_impl(true, dest, imm)
    }

    pub fn adr(&mut self, dest: Reg, imm: i32) {
        self.adr_impl(false, dest, imm)
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

    // used for str & ldr with unsigned offsets
    fn mem_access_reg_impl(&mut self, size: DataSize, is_load: bool, reg: RegOrZero, addr: impl Into<RegOrSp>, imm: u16) {
        let fac = 1u32 << (size as u32);
        assert!(imm as u32 % fac == 0);
        let imm12 = imm as u32 / fac;
        assert!(imm12 <= 4095);
        
        let mut instr = InstrEncoder::new();

        instr.push_value_of_size(size as u32, 2);
        instr.push_value_of_size(0x39, 6);
        instr.push_value_of_size(is_load as u32, 2);
        instr.push_value_of_size(imm12 as u32, 12);
        instr.push_reg(addr.into());
        instr.push_reg(reg);

        self.push(instr);
    }

    pub fn str64(&mut self, src: impl Into<RegOrZero>, dest_addr: impl Into<RegOrSp>, imm: u16) {
        self.mem_access_reg_impl(DataSize::Bits64, false, src.into(), dest_addr, imm);
    }

    pub fn str32(&mut self, src: impl Into<RegOrZero>, dest_addr: impl Into<RegOrSp>, imm: u16) {
        self.mem_access_reg_impl(DataSize::Bits32, false, src.into(), dest_addr, imm);
    }

    pub fn str16(&mut self, src: impl Into<RegOrZero>, dest_addr: impl Into<RegOrSp>, imm: u16) {
        self.mem_access_reg_impl(DataSize::Bits16, false, src.into(), dest_addr, imm);
    }

    pub fn str8(&mut self, src: impl Into<RegOrZero>, dest_addr: impl Into<RegOrSp>, imm: u16) {
        self.mem_access_reg_impl(DataSize::Bits8, false, src.into(), dest_addr, imm);
    }

    pub fn ldr64(&mut self, dest: impl Into<RegOrZero>, src_addr: impl Into<RegOrSp>, imm: u16) {
        self.mem_access_reg_impl(DataSize::Bits64, true, dest.into(), src_addr, imm);
    }

    pub fn ldr32(&mut self, dest: impl Into<RegOrZero>, src_addr: impl Into<RegOrSp>, imm: u16) {
        self.mem_access_reg_impl(DataSize::Bits32, true, dest.into(), src_addr, imm);
    }

    pub fn ldr16(&mut self, dest: impl Into<RegOrZero>, src_addr: impl Into<RegOrSp>, imm: u16) {
        self.mem_access_reg_impl(DataSize::Bits16, true, dest.into(), src_addr, imm);
    }

    pub fn ldr8(&mut self, dest: impl Into<RegOrZero>, src_addr: impl Into<RegOrSp>, imm: u16) {
        self.mem_access_reg_impl(DataSize::Bits8, true, dest.into(), src_addr, imm);
    }

    fn mov_wide64_impl(&mut self, should_keep_other_bits: bool, dest: Reg, imm: u16, shift_amount: u8) {
        assert!(shift_amount <= 48 && shift_amount % 16 == 0);

        let mut instr = InstrEncoder::new();

        // 64-bit
        instr.push_value_of_size(1, 1);

        instr.push_value_of_size(2 | (should_keep_other_bits as u32), 2);
        instr.push_value_of_size(0x25, 6);
        instr.push_value_of_size(shift_amount as u32 / 16, 2);
        instr.push_value_of_size(imm as u32, 16);
        instr.push_reg(dest);

        self.push(instr);
    }

    pub fn movz64(&mut self, dest: Reg, imm: u16, shift_amount: u8) {
        self.mov_wide64_impl(false, dest, imm, shift_amount);
    }

    pub fn movk64(&mut self, dest: Reg, imm: u16, shift_amount: u8) {
        self.mov_wide64_impl(true, dest, imm, shift_amount);
    }

    pub fn macro_mov64_abs(&mut self, dest: Reg, value: u64) {
        self.movz64(dest, (value >> 0 & 0xFFFF) as u16, 0);
        self.movk64(dest, (value >> 16 & 0xFFFF) as u16, 16);
        self.movk64(dest, (value >> 32 & 0xFFFF) as u16, 32);
        self.movk64(dest, (value >> 48 & 0xFFFF) as u16, 48);
    }

    pub fn bl(&mut self, offset: i32) {
        assert!(offset >= -134217728 && offset <= 134217724 && offset % 4 == 0);
        let imm26 = ((offset / 4) as u32) & 0x03FF_FFFF;

        let mut instr = InstrEncoder::new();
        instr.push_value_of_size(0x25, 6);

        instr.push_value_of_size(imm26, 26);

        self.push(instr);
    }

    pub fn blr(&mut self, addr: Reg) {
        let mut instr = InstrEncoder::new();

        instr.push_value_of_size(0x358FC0, 22);
        instr.push_reg(addr);
        instr.push_value_of_size(0, 5);

        self.push(instr);
    }

    pub fn ret(&mut self, return_addr: Reg) {
        let mut instr = InstrEncoder::new();
        instr.push_value_of_size(0x35_97c0, 22);
        instr.push_reg(return_addr);
        instr.push_value_of_size(0, 5);

        self.push(instr);
    }

    /// Allocate space for `n` instructions. Returns the offset, for convenience. Useful when you need to generate
    /// instructions with unknown values that need to be filled in later.
    pub fn allocate_instructions(&mut self, n: usize) -> usize {
        let offset = self.data.len();
        self.data.extend(std::iter::repeat(0).take(n * 4));
        offset
    }

    #[cfg(target_arch="aarch64")]
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
