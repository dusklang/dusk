#![allow(unused)]

use std::io::{self, Write};

use bitflags::bitflags;

use crate::linker::Linker;
use crate::mir::FuncId;

use crate::driver::Driver;
use dusk_proc_macros::ByteSwap;

use crate::linker::byte_swap::*;

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

#[repr(C)]
#[derive(ByteSwap)]
pub struct CoffHeader {
    pub machine: u16,
    pub number_of_sections: u16,
    pub time_date_stamp: u32,
    pub pointer_to_symbol_table: u32,
    pub number_of_symbols: u32,
    pub size_of_optional_header: u16,
    pub characteristics: Characteristics,
}

#[repr(u16)]
#[allow(unused)]
#[derive(Clone, Copy)]
pub enum Subsystem {
    Unknown = 0,
    Native = 1,
    WindowsGui = 2,
    WindowsCui = 3,
    Xbox = 16,
}

bitflags! {
    // Deprecated fields are included but commented out
    #[derive(ByteSwap)]
    pub struct DllCharacteristics: u16 {
        const DYNAMIC_BASE = 0x0040;
        const NX_COMPAT = 0x0100;
        const APPCONTAINER = 0x1000;
        const GUARD_CF = 0x4000;
        const TERMINAL_SERVER_AWARE = 0x8000;
    }
}


#[repr(C)]
#[derive(Clone, Copy, ByteSwap)]
pub struct ImageDataDirectory {
    pub virtual_address: u32,
    pub size: u32,
}

#[repr(C)]
#[derive(ByteSwap)]
pub struct Pe32PlusOptionalHeader {
    // standard fields

    /// Identifies the state of the image file
    pub magic: u16,
    pub major_linker_version: u8,
    pub minor_linker_version: u8,
    /// The size of the code (text) section, or the sum of all code sections if there are multiple sections
    pub size_of_code: u32,
    /// The size of the initialized data section, or the sum of all such sections if there are multiple data sections
    pub size_of_initialized_data: u32,
    /// The size of the uninitialized data section (BSS), or the sum of all such sections if there are multiple BSS sections
    pub size_of_uninitialized_data: u32,
    /// The address of the entry point relative to the image base when the executable file is loaded into memory. For program images, this is the starting address.
    /// For device drivers, this is the address of the initialization function. An entry point is optional for DLLs. When no entry point is present, this field must be zero.
    pub address_of_entry_point: u32,
    /// The address that is relative to the image base of the beginning-of-code section when it is loaded into memory.
    pub base_of_code: u32,

    // windows-specific fields

    pub image_base: u64,
    pub section_alignment: u32,
    pub file_alignment: u32,
    pub major_operating_system_version: u16,
    pub minor_operating_system_version: u16,
    pub major_image_version: u16,
    pub minor_image_version: u16,
    pub major_subsystem_version: u16,
    pub minor_subsystem_version: u16,
    pub win32_version_value: u32,
    pub size_of_image: u32,
    pub size_of_headers: u32,
    pub check_sum: u32,
    pub subsystem: u16,
    pub dll_characteristics: u16,
    pub size_of_stack_reserve: u64,
    pub size_of_stack_commit: u64,
    pub size_of_heap_reserve: u64,
    pub size_of_heap_commit: u64,
    pub loader_flags: u32,
    pub number_of_rva_and_sizes: u32,

    // Data directories

    pub export_table: ImageDataDirectory,
    pub import_table: ImageDataDirectory,
    pub resource_table: ImageDataDirectory,
    pub exception_table: ImageDataDirectory,
    pub certificate_table: ImageDataDirectory,
    pub base_relocation_table: ImageDataDirectory,
    pub debug_directory: ImageDataDirectory,
    pub architecture_specific_data: ImageDataDirectory,
    pub global_ptr: ImageDataDirectory,
    pub tls_table: ImageDataDirectory,
    pub load_config_table: ImageDataDirectory,
    pub bound_import_table: ImageDataDirectory,
    pub import_address_table: ImageDataDirectory,
    pub delay_import_descriptor: ImageDataDirectory,
    pub clr_runtime_header: ImageDataDirectory,
    pub reserved: ImageDataDirectory,
}

#[repr(C)]
#[derive(ByteSwap)]
pub struct SectionHeader {
    pub name: [u8; 8],
    pub virtual_size: u32,
    pub virtual_address: u32,
    pub size_of_raw_data: u32,
    pub pointer_to_raw_data: u32,
    pub pointer_to_relocations: u32,
    pub pointer_to_line_numbers: u32,
    pub number_of_relocations: u16,
    pub number_of_line_numbers: u16,
    pub characteristics: SectionCharacteristics,
}

bitflags! {
    // Deprecated fields are included but commented out
    #[derive(ByteSwap)]
    pub struct SectionCharacteristics: u32 {
        const CNT_CODE = 0x00000020;
        const CNT_INITIALIZED_DATA = 0x00000040;
        const CNT_UNINITIALIZED_DATA = 0x00000080;
        const LNK_INFO = 0x00000200; // valid for object files only
        const LNK_REMOVE = 0x00000800; // valid for object files only
        const LNK_COMDAT = 0x00001000; // valid for object files only
        const GPREL = 0x00008000;
        const ALIGN_1BYTES = 0x00100000; // valid for object files only
        const ALIGN_2BYTES = 0x00200000; // valid for object files only
        const ALIGN_4BYTES = 0x00300000; // valid for object files only
        const ALIGN_8BYTES = 0x00400000; // valid for object files only
        const ALIGN_16BYTES = 0x00500000; // valid for object files only
        const ALIGN_32BYTES = 0x00600000; // valid for object files only
        const ALIGN_64BYTES = 0x00700000; // valid for object files only
        const ALIGN_128BYTES = 0x00800000; // valid for object files only
        const ALIGN_256BYTES = 0x00900000; // valid for object files only
        const ALIGN_512BYTES = 0x00A00000; // valid for object files only
        const ALIGN_1024BYTES = 0x00B00000; // valid for object files only
        const ALIGN_2048BYTES = 0x00C00000; // valid for object files only
        const ALIGN_4096BYTES = 0x00D00000; // valid for object files only
        const ALIGN_8192BYTES = 0x00E00000; // valid for object files only
        const LNK_NRELOC_OVFL = 0x01000000;
        const MEM_DISCARDABLE = 0x02000000;
        const MEM_NOT_CACHED = 0x04000000;
        const MEM_NOT_PAGED = 0x08000000;
        const MEM_SHARED = 0x10000000;
        const MEM_EXECUTE = 0x20000000;
        const MEM_READ = 0x40000000;
        const MEM_WRITE = 0x80000000;
    }
}

#[repr(C)]
#[derive(ByteSwap, Clone, Copy)]
pub struct ImportDirectoryTableEntry {
    pub lookup_table_rva: u32,
    pub time_date_stamp: u32,
    pub forwarder_chain: u32,
    pub name_rva: u32,
    pub import_address_table_rva: u32,
}

impl ImportDirectoryTableEntry {
    pub fn null() -> Self {
        Self {
            lookup_table_rva: 0,
            time_date_stamp: 0,
            forwarder_chain: 0,
            name_rva: 0,
            import_address_table_rva: 0,
        }
    }
}

#[repr(transparent)]
#[derive(ByteSwap, Copy, Clone)]
pub struct Pe32PlusImportLookupTableEntry(pub u64);

#[allow(unused)]
impl Pe32PlusImportLookupTableEntry {
    pub fn ordinal(ordinal: u16) -> Self {
        Self((1 << 31) | (ordinal as u64))
    }

    pub fn hint_or_name_table_rva(hont: u32) -> Self {
        // hont is a 31-bit value, so bit 31 must be 0
        assert!(hont & 0x80000000 == 0);
        Self(hont as u64)
    }

    pub fn null() -> Self {
        Self(0)
    }
}

#[derive(Default)]
pub struct PE32Linker {
    buf: Buffer,

}

impl PE32Linker {
    pub fn new() -> PE32Linker {
        PE32Linker::default()
    }
}

const FILE_ALIGNMENT: usize = 0x200;
const SECTION_ALIGNMENT: usize = 0x1000;

impl Linker for PE32Linker {
    fn write(&mut self, _d: &Driver, _main_function_index: FuncId, dest: &mut dyn Write) -> io::Result<()> {
        self.buf.push(*b"MZ");
        self.buf.pad_with_zeroes(0x3a);
        let signature_offset = self.buf.pos() as u32 + 4;
        self.buf.push(signature_offset);
        self.buf.push(*b"PE\0\0");

        let coff_header = self.buf.alloc::<CoffHeader>();
        let optional_header = self.buf.alloc::<Pe32PlusOptionalHeader>();
        let text_header = self.buf.alloc::<SectionHeader>();
        let rdata_header = self.buf.alloc::<SectionHeader>();
        let idata_header = self.buf.alloc::<SectionHeader>();

        self.buf.pad_to_next_boundary::<FILE_ALIGNMENT>();

        let size_of_headers = self.buf.pos();

        let base_of_code: u32 = 0x1000;

        let text_section = self.begin_section(b".text\0\0\0", SectionCharacteristics::CNT_CODE | SectionCharacteristics::MEM_EXECUTE | SectionCharacteristics::MEM_READ);

        dest.write_all(&self.buf.data)?;
        Ok(())
    }
}

impl PE32Linker {
    fn begin_section(&mut self, _name: &[u8; 8], _characteristics: SectionCharacteristics) {
        
    }
}