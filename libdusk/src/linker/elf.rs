use std::collections::HashMap;
use std::io::{Write, Result as IoResult};
use std::mem;
use std::ffi::{CString, CStr};

use bitflags::bitflags;
use dusk_proc_macros::{ByteSwap, ByteSwapBitflags};
use index_vec::{define_index_type, IndexVec};

use crate::driver::Driver;
use crate::mir::FuncId;
use crate::linker::Linker;
use crate::backend::{Backend, CodeBlob};
use crate::linker::exe::{Exe, DynLibId, ImportedSymbolId, FixupLocationId};
use crate::linker::byte_swap::Buffer;
use crate::target::Arch;

define_index_type!(struct ProgramHeaderEntryId = u32;);

#[repr(u8)]
enum OsAbi {
    SystemV = 0x00,
    HpUx = 0x01,
    NetBsd = 0x02,
    Linux = 0x03,
    GnuHurd = 0x04,
    Solaris = 0x06,
    Aix = 0x07,
    Irix = 0x08,
    FreeBsd = 0x09,
    Tru64 = 0x0A,
    NovellModesto = 0x0B,
    OpenBsd = 0x0C,
    OpenVms = 0x0D,
    NonStopKernel = 0x0E,
    Aros = 0x0F,
    FenixOs = 0x10,
    NuxiCloudAbi = 0x11,
    StratusTechnologiesOpenVOS = 0x12,
}

#[repr(u16)]
enum ObjectFileType {
    None = 0x00,
    Relocatable = 0x01,
    Executable = 0x02,
    Dynamic = 0x03,
    Core = 0x04,
}

#[repr(u16)]
enum Machine {
    None = 0x00,
    X86 = 0x03,
    Arm32 = 0x28,
    X86_64 = 0x3E,
    Arm64 = 0xB7,
    RiscV = 0xF3,
}

#[repr(C)]
#[derive(ByteSwap)]
struct ElfHeader64 {
    magic: [u8; 4],
    class: u8,
    endianness: u8,
    version_1: u8,
    os_abi: u8,
    abi_version: u8,
    padding: [u8; 7],
    ty: u16,
    machine: u16,
    version_2: u32,
    entry_point_addr: u64,
    program_header_table_offset: u64,
    section_header_table_offset: u64,
    flags: u32,
    elf_header_size: u16,
    program_header_table_entry_size: u16,
    num_program_header_table_entries: u16,
    section_header_table_entry_size: u16,
    num_section_header_table_entries: u16,
    name_section_header_table_entry_index: u16,
}

#[repr(u32)]
enum SegmentType {
    Null,
    Loadable,
    Dynamic,
    Interpreter,
    Note,
    SharedLib,
    ProgramHeaderTable,
    Tls,
}

bitflags! {
    #[derive(ByteSwapBitflags, Copy, Clone)]
    struct SegmentFlags: u32 {
        const EXECUTABLE = 1;
        const WRITEABLE  = 2;
        const READABLE   = 4;
    }
}

#[repr(C)]
#[derive(ByteSwap)]
struct ProgramHeaderTableEntry64 {
    segment_type: u32,
    segment_flags: SegmentFlags,
    segment_offset: u64,
    segment_vaddr: u64,
    segment_paddr: u64,
    segment_file_size: u64,
    segment_memory_size: u64,
    segment_alignment: u64,
}

#[repr(u32)]
enum SectionType {
    Null = 0x00,
    ProgramData = 0x01,
    SymbolTable = 0x02,
    StringTable = 0x03,
    RelocationsWithAddends = 0x04,
    SymbolHashTable = 0x05,
    Dynamic = 0x06,
    Note = 0x07,
    NoData = 0x08,
    Relocations = 0x09,
    SharedLib = 0x0A,
    DynamicSymbolTable = 0x0B,
    Constructors = 0x0E,
    Destructors = 0x0F,
    Preconstructors = 0x10,
    SectionGroup = 0x11,
    ExtendedSectionIndices = 0x12,
    NumberOfDefinedTypes = 0x13,
}

bitflags! {
    #[derive(ByteSwapBitflags, Copy, Clone)]
    struct SectionFlags64: u64 {
        const WRITEABLE = 0x01;
        const ALLOC = 0x02;
        const EXECUTABLE = 0x04;
        const MERGEABLE = 0x10;
        const STRINGS = 0x20;
        const INFO_LINK = 0x40;
        const PRESERVE_ORDER = 0x80;
        const OS_NONCONFORMING = 0x100;
        const GROUP_MEMBER = 0x200;
        const TLS = 0x400;
    }
}

#[repr(C)]
#[derive(ByteSwap)]
struct SectionHeaderTableEntry64 {
    section_name_offset: u32,
    section_type: u32,
    section_flags: SectionFlags64,
    section_vaddr: u64,
    section_file_offset: u64,
    section_file_size: u64,
    linked_section_index: u32,
    section_info: u32,
    section_alignment: u64,
    section_entry_size: u64,
}

#[derive(Default)]
pub struct ElfLinker {
    buf: Buffer,
}

impl ElfLinker {
    pub fn new() -> Self {
        Default::default()
    }
}

enum ElfFixupLocation {
    CStringSectionOffset(usize),
}

#[derive(Default)]
struct ElfExe {
    cstrings: Vec<u8>,
    cstring_map: HashMap<CString, usize>,

    fixup_locations: IndexVec<FixupLocationId, ElfFixupLocation>,

    code_blob: Option<Box<dyn CodeBlob>>,
}

impl ElfExe {
    #[doc(hidden)]
    fn intern_cstring(&mut self, string: &CStr) -> usize {
        *self.cstring_map.entry(string.to_owned()).or_insert_with(|| {
            let offset = self.cstrings.len();
            self.cstrings.extend(string.to_bytes_with_nul());
            offset
        })
    }
}

impl Exe for ElfExe {
    fn import_dynamic_library(&mut self, name: &str) -> DynLibId {
        todo!("dynamic library import")
        // self.import_dynamic_library_impl(format!("usr/lib/{}", name))
    }

    fn import_symbol(&mut self, dylib: DynLibId, name: String) -> ImportedSymbolId {
        todo!("symbol import")
        // let symbol = MachOImportedSymbol { dylib, name };
        // *self.imported_symbol_map.entry(symbol.clone()).or_insert_with(|| {
        //     self.imported_symbols.push(symbol.clone())
        // })
    }

    fn use_imported_symbol(&mut self, symbol: ImportedSymbolId) -> FixupLocationId {
        todo!("symbol import usage")
        // let got_entry = self.add_got_entry(symbol);
        // self.fixup_locations.push(MachOFixupLocation::GotEntry(got_entry))
    }

    fn use_cstring(&mut self, string: &CStr) -> FixupLocationId {
        let offset = self.intern_cstring(string);
        self.fixup_locations.push(ElfFixupLocation::CStringSectionOffset(offset))
    }

    fn add_code_blob(&mut self, blob: Box<dyn CodeBlob>) {
        assert!(self.code_blob.is_none());
        self.code_blob = Some(blob);
    }
}

impl Linker for ElfLinker {
    fn write(&mut self, d: &Driver, main_function_index: FuncId, backend: &mut dyn Backend, dest: &mut dyn Write) -> IoResult<()> {
        let elf_header = self.buf.push(
            ElfHeader64 {
                magic: [0x7F, b'E', b'L', b'F'],
                class: 2, // 64 bits
                endianness: 1, // little endian
                version_1: 1,
                os_abi: OsAbi::SystemV as u8,
                // TODO: pick the right value here. Dynamic libraries apparently use this as the ABI version of the dynamic linker.
                abi_version: 0,
                padding: [0; 7],
                ty: ObjectFileType::Dynamic as u16,
                machine: match backend.arch() {
                    Arch::Arm64 => Machine::Arm64,
                    Arch::X86_64 => Machine::X86_64,
                    Arch::Dex => panic!("unable to generate ELF file with Dalvik bytecode"),
                } as u16,
                version_2: 1,

                // Most of these will be filled in later
                entry_point_addr: 0,
                program_header_table_offset: 0,
                section_header_table_offset: 0,
                flags: 0,
                elf_header_size: mem::size_of::<ElfHeader64>() as u16,
                program_header_table_entry_size: mem::size_of::<ProgramHeaderTableEntry64>() as u16,
                num_program_header_table_entries: 0,
                section_header_table_entry_size: mem::size_of::<SectionHeaderTableEntry64>() as u16,
                num_section_header_table_entries: 0,
                name_section_header_table_entry_index: 0,
            }
        );

        self.buf.push(
            ProgramHeaderTableEntry64 {
                segment_type: SegmentType::Loadable as u32,
                segment_flags: SegmentFlags::READABLE | SegmentFlags::EXECUTABLE,
                segment_offset: 0,
                segment_vaddr: 0,
                segment_paddr: 0,
                segment_file_size: 0, // To be filled in later
                segment_memory_size: 0, // To be filled in later
                segment_alignment: 65536,
            }
        );

        let mut exe = ElfExe::default();

        backend.generate_func(d, main_function_index, true, &mut exe);
        
        dest.write_all(&self.buf.data)?;
        Ok(())
    }
}
