use std::io::{self, Write};

use bitflags::bitflags;

use crate::linker::Linker;
use crate::mir::FuncId;

use crate::driver::Driver;
use dusk_proc_macros::ByteSwap;

#[derive(Copy, Clone)]
#[allow(unused)]
#[repr(u16)]
pub enum Machine {
    Unknown = 0x0,
    Am33 = 0x1d3,
    Amd64 = 0x8664,
    Arm = 0x1c0,
    Arm64 = 0xaa64,
    ArmNt = 0x1c4,
    Ebc = 0xebc,
    I386 = 0x14c,
    Ia64 = 0x200,
    M32r = 0x9041,
    Mips16 = 0x266,
    MipsFpu = 0x366,
    MipsFpu16 = 0x466,
    PowerPc = 0x1f0,
    PowerPcFp = 0x1f1,
    R4000 = 0x166,
    RiscV32 = 0x5032,
    RiscV64 = 0x5064,
    RiscV128 = 0x5128,
    Sh3 = 0x1a2,
    Sh3Dsp = 0x1a3,
    Sh4 = 0x1a6,
    Sh5 = 0x1a8,
    Thumb = 0x1c2,
    WceMipsV2 = 0x169,
}

bitflags! {
    // Deprecated fields are included but commented out
    #[derive(ByteSwap)]
    pub struct Characteristics: u16 {
        const RELOCS_STRIPPED = 0x0001;
        const EXECUTABLE_IMAGE = 0x0002;
        // const LINE_NUMS_STRIPPED = 0x0004;
        // const LOCAL_SYMS_STRIPPED = 0x0008;
        // const AGGRESSIVE_WS_TRIM = 0x0010;
        const LARGE_ADDRESS_AWARE = 0x0020;
        // const REVERSED_LO = 0x0080;
        const MACHINE_32BIT = 0x0100;
        const DEBUG_STRIPPED = 0x0200;
        const REMOVABLE_RUN_FROM_SWAP = 0x0400;
        const NET_RUN_FROM_SWAP = 0x0800;
        const SYSTEM = 0x1000;
        const DLL = 0x2000;
        const UP_SYSTEM_ONLY = 0x4000;
        // const BYTES_RESERVED_HI = 0x8000;
    }
}

pub struct PE32Linker {

}

impl PE32Linker {
    pub fn new() -> PE32Linker {
        PE32Linker {
        }
    }
}

impl Linker for PE32Linker {
    fn write(&mut self, _d: &Driver, _main_function_index: FuncId, _dest: &mut dyn Write) -> io::Result<()> {
        todo!()
    }
}
