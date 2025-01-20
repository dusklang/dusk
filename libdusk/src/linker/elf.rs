use std::collections::HashMap;
use std::io::{Write, Result as IoResult};
use std::mem;
use std::ffi::{CString, CStr};

use bitflags::bitflags;
use dusk_proc_macros::{ByteSwap, ByteSwapBitflags};
use index_vec::{define_index_type, IndexVec};
use crate::index_vec::IndexVecExt;

use crate::driver::Driver;
use crate::mir::FuncId;
use crate::linker::Linker;
use crate::backend::{Backend, CodeBlob, CodeBlobExt, Indirection};
use crate::linker::exe::{Exe, DynLibId, ImportedSymbolId, FixupLocationId};
use crate::linker::byte_swap::{Buffer, Ref};
use crate::target::Arch;

define_index_type!(struct ProgramHeaderEntryId = u32;);
define_index_type!(struct SectionIndex = u32;);
define_index_type!(struct SymbolTableEntryIndex = u32;);

#[repr(u8)]
#[allow(unused)]
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
#[allow(unused)]
enum ObjectFileType {
    None = 0x00,
    Relocatable = 0x01,
    Executable = 0x02,
    Dynamic = 0x03,
    Core = 0x04,
}

#[repr(u16)]
#[allow(unused)]
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
#[allow(unused)]
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

#[repr(C)]
#[derive(ByteSwap)]
struct DynamicSectionEntry64 {
    tag: i64,
    value: u64,
}

#[repr(i64)]
#[allow(unused)]
enum DynamicSectionEntryTag {
    Null,
    Needed,
    PltRelocSize,
    PltGot,
    Hash,
    StrTab,
    SymTab,
    Rela,
    RelaSize,
    RelaEntrySize,
    StrSize,
    SymEntrySize,
    Init,
    Fini,
    SoName,
    Rpath, // deprecated
    Symbolic,
    Rel,
    RelSize,
    RelEntrySize,
    PltRelType,
    Debug,
    TextRel,
    JmpRel,
    BindNow,
    InitArray,
    FiniArray,
    InitArraySize,
    FiniArraySize,
    RunPath,
    Flags,
    PreInitArray = 0x20,
    PreInitArraySize,
    SymTabShndx,
    Num,

    Flags1 = 0x6ffffffb,
}

bitflags! {
    #[derive(ByteSwapBitflags, Copy, Clone)]
    struct DynamicSectionEntryFlags: u64 {
        const ORIGIN   = 0x0000_0001;
        const SYMBOLIC = 0x0000_0002;
        const TEXT_RELOCATIONS = 0x0000_0004;
        const BIND_NOW = 0x0000_0008;
        // TODO: add more
    }
}

bitflags! {
    #[derive(ByteSwapBitflags, Copy, Clone)]
    struct DynamicSectionEntryFlags1: u64 {
        const NOW       = 0x0000_0001;
        const GLOBAL    = 0x0000_0002;
        const GROUP     = 0x0000_0004;
        const NO_DELETE = 0x0000_0008;
        const PIE       = 0x0800_0000;
        // TODO: add more
    }
}

#[repr(u32)]
#[allow(unused)]
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
    section_size: u64,
    linked_section_index: u32,
    section_info: u32,
    section_alignment: u64,
    section_entry_size: u64,
}

#[repr(C)]
#[derive(ByteSwap)]
struct SymbolTableEntry64 {
    name: u32,
    info: u8,
    other: u8,
    section_header_table_index: u16,
    value: u64,
    size: u64,
}

impl SymbolTableEntry64 {
    #[allow(unused)]
    fn undefined() -> Self {
        Self {
            name: 0,
            info: 0,
            other: 0,
            section_header_table_index: SHN_UNDEF,
            value: 0,
            size: 0,
        }
    }
}

#[repr(u8)]
#[derive(Copy, Clone)]
#[allow(unused)]
enum SymbolType {
    NoType,
    Object,
    Func,
    Section,
    File,
    Common,
}

#[derive(PartialEq, Copy, Clone)]
#[repr(u8)]
#[allow(unused)]
enum SymbolBinding {
    Local,
    Global,
    Weak,
}

fn symbol_info(ty: SymbolType, binding: SymbolBinding) -> u8 {
    ((binding as u8) << 4) + ((ty as u8) & 0xf)
}

#[repr(u8)]
#[allow(unused)]
enum SymbolVisibility {
    Default,
    Internal,
    Hidden,
    Protected
}

#[allow(unused)] const SHN_UNDEF:  u16 = 0;
#[allow(unused)] const SHN_BEFORE: u16 = 0xff00;
#[allow(unused)] const SHN_AFTER:  u16 = 0xff01;
#[allow(unused)] const SNH_ABS:    u16 = 0xfff1;
#[allow(unused)] const SNH_COMMON: u16 = 0xfff2;

struct SymbolTableBuilder {
    entries: IndexVec<SymbolTableEntryIndex, SymbolTableEntry64>,
    entry_refs: IndexVec<SymbolTableEntryIndex, Ref<SymbolTableEntry64>>,
    string_table: Buffer,
    first_non_local_entry: Option<u32>,
}

impl SymbolTableBuilder {
    fn new() -> Self {
        let mut salf = Self {
            entries: IndexVec::new(),
            entry_refs: IndexVec::new(),
            string_table: Buffer::new(),
            first_non_local_entry: None,
        };

        salf.add_entry("", SymbolType::NoType, SymbolBinding::Local, SymbolVisibility::Default, SHN_UNDEF, 0, 0);
        salf
    }

    fn add_entry(&mut self, name: &str, ty: SymbolType, binding: SymbolBinding, visibility: SymbolVisibility, section_header_table_index: u16, value: u64, size: u64) -> SymbolTableEntryIndex {
        assert!(self.entry_refs.is_empty());
        if binding == SymbolBinding::Local {
            assert!(self.first_non_local_entry.is_none());
        } else {
            if self.first_non_local_entry.is_none() {
                self.first_non_local_entry = Some(self.entries.len() as u32);
            }
        }
        let entry = self.entries.push(
            SymbolTableEntry64 {
                name: self.string_table.pos() as u32,
                info: symbol_info(ty, binding),
                other: visibility as u8,
                section_header_table_index,
                value,
                size,
            }
        );
        self.string_table.push_null_terminated_string(name);
        entry
    }

    fn alloc_in(&mut self, buf: &mut Buffer) {
        for entry in self.entries.indices() {
            self.entry_refs.push_at(entry, buf.alloc());
        }
    }

    fn write_to(&mut self, buf: &mut Buffer) {
        for (entry, entry_ref) in mem::take(&mut self.entries).into_iter().zip(mem::take(&mut self.entry_refs)) {
            buf.get_mut(entry_ref).set(entry);
        }
    }

    fn get_string_table(&self) -> &[u8] {
        &self.string_table.data
    }

    fn first_non_local_entry(&self) -> u32 {
        self.first_non_local_entry.unwrap_or(self.entries.len() as u32)
    }
}

#[derive(Default)]
pub struct ElfLinker {
    buf: Buffer,
    program_headers: Vec<Ref<ProgramHeaderTableEntry64>>,
    section_headers: IndexVec<SectionIndex, (String, SectionHeaderTableEntry64)>,
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
    fn import_dynamic_library(&mut self, _name: &str) -> DynLibId {
        todo!("dynamic library import")
        // self.import_dynamic_library_impl(format!("usr/lib/{}", name))
    }

    fn import_symbol(&mut self, _dylib: DynLibId, _name: String) -> ImportedSymbolId {
        todo!("symbol import")
        // let symbol = MachOImportedSymbol { dylib, name };
        // *self.imported_symbol_map.entry(symbol.clone()).or_insert_with(|| {
        //     self.imported_symbols.push(symbol.clone())
        // })
    }

    fn use_imported_symbol(&mut self, _symbol: ImportedSymbolId) -> FixupLocationId {
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

impl ElfLinker {
    fn add_program_header(&mut self, header: ProgramHeaderTableEntry64) -> Ref<ProgramHeaderTableEntry64> {
        let header = self.buf.push(header);
        self.program_headers.push(header);
        header
    }

    fn add_section_header(&mut self, name: impl Into<String>, header: SectionHeaderTableEntry64) -> SectionIndex {
        self.section_headers.push((name.into(), header))
    }
}

impl Linker for ElfLinker {
    fn write(&mut self, d: &Driver, main_function_index: FuncId, backend: &mut dyn Backend, dest: &mut dyn Write) -> IoResult<()> {
        let bss_size = 8 as u64; // TODO: don't hardcode this.

        let mut exe = ElfExe::default();
        backend.generate_func(d, main_function_index, true, &mut exe);
        let mut code = mem::take(&mut exe.code_blob).expect("generate_func must generate a code blob");

        let mut dynamic_symbol_table = SymbolTableBuilder::new();
        let mut normal_symbol_table = SymbolTableBuilder::new();

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

        // ============== Program headers (segments, added to the buffer immediately) ==============

        let program_header_table_offset = self.buf.pos() as u64;
        let program_header_table_header = self.add_program_header(
            ProgramHeaderTableEntry64 {
                segment_type: SegmentType::ProgramHeaderTable as u32,
                segment_flags: SegmentFlags::READABLE,
                segment_offset: program_header_table_offset,
                segment_vaddr: program_header_table_offset,
                segment_paddr: program_header_table_offset,
                segment_file_size: 0, // To be filled in later
                segment_memory_size: 0, // To be filled in later
                segment_alignment: 8,
            }
        );
        let interp_segment_header = self.add_program_header(
            ProgramHeaderTableEntry64 {
                segment_type: SegmentType::Interpreter as u32,
                segment_flags: SegmentFlags::READABLE,
                segment_offset: 0, // To be filled in later,
                segment_vaddr: 0, // To be filled in later
                segment_paddr: 0, // To be filled in later
                segment_file_size: 0, // To be filled in later
                segment_memory_size: 0, // To be filled in later
                segment_alignment: 1,
            }
        );
        let read_exec_segment_header = self.add_program_header(
            ProgramHeaderTableEntry64 {
                segment_type: SegmentType::Loadable as u32,
                segment_flags: SegmentFlags::READABLE | SegmentFlags::EXECUTABLE,
                segment_offset: 0,
                segment_vaddr: 0,
                segment_paddr: 0,
                segment_file_size: 0, // To be filled in later
                segment_memory_size: 0, // To be filled in later
                segment_alignment: 0x10000,
            }
        );
        let read_write_segment_header = self.add_program_header(
            ProgramHeaderTableEntry64 {
                segment_type: SegmentType::Loadable as u32,
                segment_flags: SegmentFlags::READABLE | SegmentFlags::WRITEABLE,
                segment_offset: 0,      // To be filled in later
                segment_vaddr: 0,       // To be filled in later
                segment_paddr: 0,       // To be filled in later
                segment_file_size: 0,   // To be filled in later
                segment_memory_size: 0, // To be filled in later
                segment_alignment: 0x10000,
            }
        );
        let dynamic_segment_header = self.add_program_header(
            ProgramHeaderTableEntry64 {
                segment_type: SegmentType::Dynamic as u32,
                segment_flags: SegmentFlags::READABLE | SegmentFlags::WRITEABLE,
                segment_offset: 0,      // To be filled in later
                segment_vaddr: 0,       // To be filled in later
                segment_paddr: 0,       // To be filled in later
                segment_file_size: 0,   // To be filled in later
                segment_memory_size: 0, // To be filled in later
                segment_alignment: 8,
            }
        );
        // TODO: NOTE segment
        // TODO: maybe GNU_EH_FRAME, GNU_STACK, GNU_RELRO

        // ============== Section headers (not added to the buffer until later) ==============

        self.add_section_header(
            "",
            SectionHeaderTableEntry64 {
                section_name_offset: 0,
                section_type: SectionType::Null as u32,
                section_flags: SectionFlags64::empty(),
                section_vaddr: 0,
                section_file_offset: 0,
                section_size: 0,
                linked_section_index: 0,
                section_info: 0,
                section_alignment: 0,
                section_entry_size: 0,
            }
        );
        let interp_section = self.add_section_header(
            ".interp",
            SectionHeaderTableEntry64 {
                section_name_offset: 0,
                section_type: SectionType::ProgramData as u32,
                section_flags: SectionFlags64::ALLOC,
                section_vaddr: 0, // To be filled in later
                section_file_offset: 0, // To be filled in later
                section_size: 0, // To be filled in later
                linked_section_index: 0,
                section_info: 0,
                section_alignment: 1,
                section_entry_size: 0,
            }
        );
        let dynamic_symbol_section = self.add_section_header(
            ".dynsym",
            SectionHeaderTableEntry64 {
                section_name_offset: 0,
                section_type: SectionType::DynamicSymbolTable as u32,
                section_flags: SectionFlags64::ALLOC,
                section_vaddr: 0, // To be filled in later
                section_file_offset: 0, // To be filled in later
                section_size: 0, // To be filled in later
                linked_section_index: 0, // To be filled in almost immediately
                section_info: dynamic_symbol_table.first_non_local_entry(),
                section_alignment: 8,
                section_entry_size: 24,
            }
        );
        let dynamic_str_section = self.add_section_header(
            ".dynstr",
            SectionHeaderTableEntry64 {
                section_name_offset: 0,
                section_type: SectionType::StringTable as u32,
                section_flags: SectionFlags64::ALLOC,
                section_vaddr: 0, // To be filled in later
                section_file_offset: 0, // To be filled in later
                section_size: 0, // To be filled in later
                linked_section_index: 0,
                section_info: 0,
                section_alignment: 1,
                section_entry_size: 0,
            }
        );
        self.section_headers[dynamic_symbol_section].1.linked_section_index = dynamic_str_section.raw();
        let text_section = self.add_section_header(
            ".text",
            SectionHeaderTableEntry64 {
                section_name_offset: 0,
                section_type: SectionType::ProgramData as u32,
                section_flags: SectionFlags64::ALLOC | SectionFlags64::EXECUTABLE,
                section_vaddr: 0, // To be filled in later
                section_file_offset: 0, // To be filled in later
                section_size: 0, // To be filled in later
                linked_section_index: 0,
                section_info: 0,
                section_alignment: 4,
                section_entry_size: 0,
            }
        );
        let rodata_section = (!exe.cstrings.is_empty()).then(|| {
            self.add_section_header(
                ".rodata",
                SectionHeaderTableEntry64 {
                    section_name_offset: 0,
                    section_type: SectionType::ProgramData as u32,
                    section_flags: SectionFlags64::ALLOC,
                    section_vaddr: 0, // To be filled in later
                    section_file_offset: 0, // To be filled in later
                    section_size: 0, // To be filled in later
                    linked_section_index: 0,
                    section_info: 0,
                    section_alignment: 8,
                    section_entry_size: 0,
                }
            )
        });
        let dynamic_section = self.add_section_header(
            ".dynamic",
            SectionHeaderTableEntry64 {
                section_name_offset: 0,
                section_type: SectionType::Dynamic as u32,
                section_flags: SectionFlags64::WRITEABLE | SectionFlags64::ALLOC,
                section_vaddr: 0, // To be filled in later
                section_file_offset: 0, // To be filled in later
                section_size: 0, // To be filled in later
                linked_section_index: dynamic_str_section.raw(),
                section_info: 0,
                section_alignment: 8,
                section_entry_size: 16,
            }
        );

        let mut section_symbol_table_indices = HashMap::<SectionIndex, SymbolTableEntryIndex>::new();
        for (section_index, (section_name, _)) in self.section_headers.iter_enumerated().skip(1) {
            let symbol = normal_symbol_table.add_entry(section_name, SymbolType::Section, SymbolBinding::Local, SymbolVisibility::Default, section_index.raw() as u16, 0, 0);
            section_symbol_table_indices.insert(section_index, symbol);
        }
        let func_symbol = normal_symbol_table.add_entry("start", SymbolType::Func, SymbolBinding::Global, SymbolVisibility::Default, text_section.raw() as u16, 0, code.len() as u64);

        let bss_section = (bss_size > 0).then(|| self.add_section_header(
            ".bss",
            SectionHeaderTableEntry64 {
                section_name_offset: 0,
                section_type: SectionType::NoData as u32,
                section_flags: SectionFlags64::WRITEABLE | SectionFlags64::ALLOC,
                section_vaddr: 0, // To be filled in later
                section_file_offset: 0, // To be filled in later
                section_size: 0, // To be filled in later
                linked_section_index: 0,
                section_info: 0,
                section_alignment: 1,
                section_entry_size: 0,
            }
        ));
        let symbol_table_section = self.add_section_header(
            ".symtab",
            SectionHeaderTableEntry64 {
                section_name_offset: 0,
                section_type: SectionType::SymbolTable as u32,
                section_flags: SectionFlags64::empty(),
                section_vaddr: 0,
                section_file_offset: 0, // To be filled in later
                section_size: 0, // To be filled in later
                linked_section_index: 0, // To be filled in almost immediately
                section_info: normal_symbol_table.first_non_local_entry(),
                section_alignment: 8,
                section_entry_size: 24,
            }
        );
        let string_table_section = self.add_section_header(
            ".strtab",
            SectionHeaderTableEntry64 {
                section_name_offset: 0,
                section_type: SectionType::StringTable as u32,
                section_flags: SectionFlags64::empty(),
                section_vaddr: 0,
                section_file_offset: 0, // To be filled in later
                section_size: 0, // To be filled in later
                linked_section_index: 0,
                section_info: 0,
                section_alignment: 1,
                section_entry_size: 0,
            }
        );
        self.section_headers[symbol_table_section].1.linked_section_index = string_table_section.raw();
        let section_names_section_header = self.add_section_header(
            ".shstrtab",
            SectionHeaderTableEntry64 {
                section_name_offset: 0,
                section_type: SectionType::StringTable as u32,
                section_flags: SectionFlags64::empty(),
                section_vaddr: 0,
                section_file_offset: 0,  // to be filled in later
                section_size: 0,    // to be filled in later
                linked_section_index: 0,
                section_info: 0,
                section_alignment: 1,
                section_entry_size: 0,
            }
        );

        let program_header_table_size = self.buf.pos() as u64 - program_header_table_offset;
        self.buf.get_mut(program_header_table_header).modify(|header| {
            header.segment_file_size = program_header_table_size;
            header.segment_memory_size = program_header_table_size;
        });
        self.buf.get_mut(elf_header).modify(|header| {
            header.program_header_table_offset = program_header_table_offset;
            header.num_program_header_table_entries = self.program_headers.len() as u16;
        });

        let interp_offset = self.buf.pos() as u64;
        self.buf.push_null_terminated_string("/lib/ld-linux-aarch64.so.1");
        let interp_size = self.buf.pos() as u64 - interp_offset;
        self.buf.get_mut(interp_segment_header).modify(|header| {
            header.segment_offset = interp_offset;
            header.segment_vaddr = interp_offset;
            header.segment_paddr = interp_offset;
            header.segment_file_size = interp_size;
            header.segment_memory_size = interp_size;
        });
        self.section_headers[interp_section].1.section_vaddr = interp_offset;
        self.section_headers[interp_section].1.section_file_offset = interp_offset;
        self.section_headers[interp_section].1.section_size = interp_size;

        self.buf.pad_both_to_next_boundary(8);
        let dynamic_symbol_section_pos = self.buf.pos() as u64;
        dynamic_symbol_table.alloc_in(&mut self.buf);
        let dynamic_symbol_section_size = self.buf.pos() as u64 - dynamic_symbol_section_pos;
        self.section_headers[dynamic_symbol_section].1.section_vaddr = dynamic_symbol_section_pos;
        self.section_headers[dynamic_symbol_section].1.section_file_offset = dynamic_symbol_section_pos;
        self.section_headers[dynamic_symbol_section].1.section_size = dynamic_symbol_section_size;

        let dynamic_str_section_pos = self.buf.pos() as u64;
        self.buf.extend(dynamic_symbol_table.get_string_table());
        // TODO: add dynamic string table entries
        let dynamic_str_section_size = self.buf.pos() as u64 - dynamic_str_section_pos;

        self.section_headers[dynamic_str_section].1.section_vaddr = dynamic_str_section_pos;
        self.section_headers[dynamic_str_section].1.section_file_offset = dynamic_str_section_pos;
        self.section_headers[dynamic_str_section].1.section_size = dynamic_str_section_size;

        self.buf.pad_both_to_next_boundary(4);
        let text_section_pos = self.buf.pos() as u64;
        self.buf.pad_with_zeroes(code.len());

        let text_section_size = self.buf.pos() as u64 - text_section_pos;

        self.section_headers[text_section].1.section_vaddr = text_section_pos;
        self.section_headers[text_section].1.section_file_offset = text_section_pos;
        self.section_headers[text_section].1.section_size = text_section_size;

        normal_symbol_table.entries[func_symbol].value = text_section_pos;

        let rodata_section_pos = rodata_section.and_then(|rodata_section| {
            self.buf.pad_both_to_next_boundary(8);
            let rodata_section_pos = self.buf.pos() as u64;
            self.buf.extend(&exe.cstrings);
            self.section_headers[rodata_section].1.section_vaddr = rodata_section_pos;
            self.section_headers[rodata_section].1.section_file_offset = rodata_section_pos;
            self.section_headers[rodata_section].1.section_size = exe.cstrings.len() as u64;
            Some(rodata_section_pos)
        });

        let read_exec_segment_size = self.buf.pos() as u64;
        self.buf.get_mut(read_exec_segment_header).modify(|header| {
            header.segment_file_size = read_exec_segment_size;
            header.segment_memory_size = read_exec_segment_size;
        });

        self.buf.jump_to_rva(self.buf.rva() + 0x10000);
        self.buf.pad_both_to_next_boundary(8);
        let read_write_segment_rva = self.buf.rva() as u64;
        let read_write_segment_pos = self.buf.pos() as u64;

        let (dynamic_section_pos, dynamic_section_rva) = (self.buf.pos() as u64, self.buf.rva() as u64);
        self.buf.push(
            DynamicSectionEntry64 {
                tag: DynamicSectionEntryTag::StrTab as i64,
                value: dynamic_str_section_pos,
            }
        );
        self.buf.push(
            DynamicSectionEntry64 {
                tag: DynamicSectionEntryTag::SymTab as i64,
                value: dynamic_symbol_section_pos,
            }
        );
        self.buf.push(
            DynamicSectionEntry64 {
                tag: DynamicSectionEntryTag::StrSize as i64,
                value: dynamic_str_section_size,
            }
        );
        self.buf.push(
            DynamicSectionEntry64 {
                tag: DynamicSectionEntryTag::SymEntrySize as i64,
                value: 24,
            }
        );
        self.buf.push(
            DynamicSectionEntry64 {
                tag: DynamicSectionEntryTag::Debug as i64,
                value: 0,
            }
        );
        self.buf.push(
            DynamicSectionEntry64 {
                tag: DynamicSectionEntryTag::Flags as i64,
                value: DynamicSectionEntryFlags::BIND_NOW.bits(),
            }
        );
        self.buf.push(
            DynamicSectionEntry64 {
                tag: DynamicSectionEntryTag::Flags1 as i64,
                value: (DynamicSectionEntryFlags1::NOW | DynamicSectionEntryFlags1::PIE).bits(),
            }
        );
        self.buf.push(
            DynamicSectionEntry64 {
                tag: DynamicSectionEntryTag::Null as i64,
                value: 0,
            }
        );
        let dynamic_section_size = self.buf.pos() as u64 - dynamic_section_pos;
        self.buf.get_mut(dynamic_segment_header).modify(|header| {
            header.segment_offset = dynamic_section_pos;
            header.segment_vaddr = dynamic_section_rva;
            header.segment_paddr = dynamic_section_rva;
            header.segment_file_size = dynamic_section_size;
            header.segment_memory_size = dynamic_section_size;
        });

        self.section_headers[dynamic_section].1.section_vaddr = dynamic_section_rva;
        self.section_headers[dynamic_section].1.section_file_offset = dynamic_section_pos;
        self.section_headers[dynamic_section].1.section_size = dynamic_section_size;

        if let Some(bss_section) = bss_section {
            let bss_pos = self.buf.pos() as u64;
            let bss_rva = self.buf.rva() as u64;
            self.buf.jump_to_rva(self.buf.rva() + bss_size as usize);

            self.section_headers[bss_section].1.section_vaddr = bss_rva;
            self.section_headers[bss_section].1.section_file_offset = bss_pos;
            self.section_headers[bss_section].1.section_size = bss_size;
        }

        let read_write_segment_file_size = self.buf.pos() as u64 - read_write_segment_pos;
        let read_write_segment_memory_size = read_write_segment_file_size + bss_size;
        self.buf.get_mut(read_write_segment_header).modify(|header| {
            header.segment_offset = read_write_segment_pos;
            header.segment_vaddr = read_write_segment_rva;
            header.segment_paddr = read_write_segment_rva;
            header.segment_file_size = read_write_segment_file_size;
            header.segment_memory_size = read_write_segment_memory_size;
        });

        self.buf.pad_both_to_next_boundary(8);
        let symbol_table_pos = self.buf.pos() as u64;
        normal_symbol_table.alloc_in(&mut self.buf);
        let symbol_table_size = self.buf.pos() as u64 - symbol_table_pos;
        self.section_headers[symbol_table_section].1.section_file_offset = symbol_table_pos;
        self.section_headers[symbol_table_section].1.section_size = symbol_table_size;

        let string_table_pos = self.buf.pos() as u64;
        self.buf.extend(normal_symbol_table.get_string_table());
        let string_table_size = self.buf.pos() as u64 - string_table_pos;
        self.section_headers[string_table_section].1.section_file_offset = string_table_pos;
        self.section_headers[string_table_section].1.section_size = string_table_size;

        let section_name_string_table_offset = self.buf.pos() as u64;
        let mut section_name_string_table_offsets = IndexVec::<SectionIndex, u32>::new();
        for (section, (name, _)) in self.section_headers.iter_enumerated() {
            let offset = self.buf.pos() as u64 - section_name_string_table_offset;
            section_name_string_table_offsets.push_at(section, offset as u32);
            self.buf.push_null_terminated_string(name);
        }

        self.section_headers[section_names_section_header].1.section_file_offset = section_name_string_table_offset;
        self.section_headers[section_names_section_header].1.section_size = self.buf.pos() as u64 - section_name_string_table_offset;

        for (section_index, symbol_index) in section_symbol_table_indices {
            normal_symbol_table.entries[symbol_index].value = self.section_headers[section_index].1.section_vaddr;
        }

        self.buf.pad_both_to_next_boundary(8);
        let section_header_table_offset = self.buf.pos() as u64;
        let num_section_headers = self.section_headers.len();
        for (section, (_, mut section_header)) in std::mem::take(&mut self.section_headers).into_iter_enumerated() {
            section_header.section_name_offset = section_name_string_table_offsets[section];
            self.buf.push(section_header);
        }

        self.buf.get_mut(elf_header).modify(|header| {
            header.section_header_table_offset = section_header_table_offset;
            header.num_section_header_table_entries = num_section_headers as u16;
            header.name_section_header_table_entry_index = section_names_section_header.index() as u16;
            header.entry_point_addr = text_section_pos;
        });

        let code = code.perform_fixups(text_section_pos as usize, |fixup| {
            match exe.fixup_locations[fixup] {
                ElfFixupLocation::CStringSectionOffset(offset) => (rodata_section_pos.unwrap() as usize + offset, Indirection::Indirect),
            }
        });

        self.buf.data[text_section_pos as usize..][..code.len()].copy_from_slice(code);

        dynamic_symbol_table.write_to(&mut self.buf);
        normal_symbol_table.write_to(&mut self.buf);

        dest.write_all(&self.buf.data)?;
        Ok(())
    }
}
