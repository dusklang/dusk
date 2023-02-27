#![allow(unused)]

use std::io::{self, Write};
use std::marker::PhantomData;
use std::mem;

use crate::arm64::{Arm64Encoder, Reg};

#[derive(Default)]
pub struct MachOEncoder {
    data: Vec<u8>, // TODO: support writing directly to a file instead of copying from a byte buffer?
    num_load_commands: u32,
    num_segments: u32,
    num_symbol_table_entries: u32,
}

const MH_MAGIC_64: u32           = 0xFEED_FACF;
const CPU_TYPE_ARM64: u32        = 0x0100_000C;
const CPU_SUBTYPE_ARM64_ALL: u32 = 0x0000_0000;
const MH_EXECUTE: u32            = 0x0000_0002;

const MH_NOUNDEFS: u32 = 0x0000_0001;
const MH_DYLDLINK: u32 = 0x0000_0004;
const MH_TWOLEVEL: u32 = 0x0000_0080;
const MH_PIE:      u32 = 0x0020_0000;

const LC_REQ_DYLD: u32 = 0x8000_0000;

const LC_SYMTAB:        u32 = 0x0000_0002;
const LC_DYSYMTAB:      u32 = 0x0000_000B;
const LC_LOAD_DYLINKER: u32 = 0x0000_000E;

const LC_SEGMENT_64: u32 = 0x0000_0019;
const LC_FUNCTION_STARTS: u32 = 0x0000_0026;
const LC_MAIN: u32 = 0x28 | LC_REQ_DYLD;
const LC_DYLD_EXPORTS_TRIE: u32 = 0x33 | LC_REQ_DYLD;
const LC_DYLD_CHAINED_FIXUPS: u32 = 0x34 | LC_REQ_DYLD;
const LC_LOAD_DYLIB: u32 = 0x0000_000C;

const DYLD_CHAINED_IMPORT: u32 = 0x0000_0001;

const VM_PROT_NONE:    u32 = 0x0000_0000;
const VM_PROT_READ:    u32 = 0x0000_0001;
const VM_PROT_WRITE:   u32 = 0x0000_0002;
const VM_PROT_EXECUTE: u32 = 0x0000_0004;

const S_ATTR_PURE_INSTRUCTIONS:   u32 = 0x8000_0000;
const S_ATTR_NO_TOC:              u32 = 0x4000_0000;
const S_ATTR_STRIP_STATIC_SYMS:   u32 = 0x2000_0000;
const S_ATTR_NO_DEAD_STRIP:       u32 = 0x1000_0000;
const S_ATTR_LIVE_SUPPORT:        u32 = 0x0800_0000;
const S_ATTR_SELF_MODIFYING_CODE: u32 = 0x0400_0000;
const S_ATTR_DEBUG:               u32 = 0x0200_0000;
const S_ATTR_SOME_INSTRUCTIONS:   u32 = 0x0000_0400;
const S_ATTR_EXT_RELOC:           u32 = 0x0000_0200;
const S_ATTR_LOC_RELOC:           u32 = 0x0000_0100;

struct Ref<T> {
    addr: usize,
    _phantom: PhantomData<T>,
}

impl<T> Clone for Ref<T> {
    fn clone(&self) -> Self {
        Self {
            addr: self.addr,
            _phantom: PhantomData,
        }
    }
}
impl<T> Copy for Ref<T> {}

impl<T> Ref<T> {
    fn new(addr: usize) -> Self {
        Self {
            addr,
            _phantom: PhantomData,
        }
    }

    fn size(self) -> usize { mem::size_of::<T>() }
    fn start(self) -> usize { self.addr }
    fn end(self) -> usize { self.addr + self.size() }
}

#[repr(C, packed)]
struct MachHeader {
    magic: u32,
    cpu_type: u32,
    cpu_subtype: u32,
    file_type: u32,
    num_commands: u32,
    size_of_commands: u32,
    flags: u32,
    reserved: u32,
}

#[repr(C, packed)]
struct LcSegment64 {
    command: u32,
    command_size: u32,
    name: [u8; 16],
    vm_addr: u64,
    vm_size: u64,
    file_offset: u64,
    file_size: u64,
    max_vm_protection: u32,
    initial_vm_protection: u32,
    num_sections: u32,
    flags: u32,
}

#[repr(C, packed)]
struct Section64 {
    name: [u8; 16],
    segment_name: [u8; 16],
    vm_addr: u64,
    vm_size: u64,
    file_offset: u32,
    alignment: u32, // stored as log base 2
    relocations_file_offset: u32,
    num_relocations: u32,
    flags: SectionFlags,
    reserved: [u32; 3],
}

#[repr(C, packed)]
struct Dylib {
    name_offset: u32,
    timestamp: u32,
    current_version: u32,
    compatibility_version: u32,
}

#[repr(C, packed)]
struct DylibCommand {
    command: u32,
    command_size: u32,
    dylib: Dylib,
}

#[repr(C, packed)]
struct DylinkerCommand {
    command: u32,
    command_size: u32,
    name_offset: u32,
}

#[repr(C, packed)]
struct SymbolTableCommand {
    command: u32,
    command_size: u32,
    symbol_table_offset: u32,
    num_symbols: u32,
    string_table_offset: u32,
    string_table_size: u32,
}

#[repr(C, packed)]
struct SymbolTableEntry {
    string_table_offset: u32,
    ty: u8,
    section_number: u8,
    desc: u16,
    value: u64,
}

#[repr(C, packed)]
struct DynamicSymbolTableCommand {
    command: u32,
    command_size: u32,

    local_symbols_index: u32,
    num_local_symbols: u32,

    extern_symbols_index: u32,
    num_extern_symbols: u32,

    undef_symbols_index: u32,
    num_undef_symbols: u32,

    toc_offset: u32,
    num_toc_entries: u32,

    module_table_offset: u32,
    num_module_table_entries: u32,

    referenced_symbol_table_offset: u32,
    num_referenced_symbol_table_entries: u32,

    indirect_symbol_table_offset: u32,
    num_indirect_symbol_table_entries: u32,

    extern_relocation_entries_offset: u32,
    num_extern_relocation_entries: u32,

    local_relocation_entries_offset: u32,
    num_local_relocation_entries: u32,
}

#[repr(C, packed)]
struct EntryPointCommand {
    command: u32,
    command_size: u32,
    entry_point_file_offset: u64,
    stack_size: u64,
}

#[derive(Copy, Clone)]
#[repr(u32)]
enum SectionType {
    Regular,
    ZeroFill,
    CStringLiterals,
    Literals4Byte,
    Literals8Byte,
    LiteralPointers,
    NonLazySymbolPointers,
    LazySymbolPointers,
    SymbolStubs,
    ModInitFuncPointers,
    ModTermFuncPointer,
    Coalesced,
    GbZeroFill,
    Interposing,
    Literals16Byte,
    DtraceDof,
    LazyDylibSymbolPointers,
    ThreadLocalRegular,
    ThreadLocalZeroFill,
    ThreadLocalVariables,
    ThreadLocalVariablePointers,
    ThreadLocalInitFunctionPointers,
    InitFuncOffsets,
}

#[repr(transparent)]
struct SectionFlags(u32);

impl SectionFlags {
    fn new(ty: SectionType, attributes: u32) -> SectionFlags {
        debug_assert_eq!(attributes & 0xFF, 0);
        debug_assert_eq!(ty as u32 & 0xFF, ty as u32);

        SectionFlags(attributes | ty as u32)
    }
}

fn encode_string_16(name: &str) -> [u8; 16] {
    let mut out = [0; 16];
    debug_assert!(name.len() <= 16);
    debug_assert!(name.is_ascii());
    out[..name.len()].copy_from_slice(name.as_bytes());
    out
}

struct SegmentBuilder {
    header: Ref<LcSegment64>,
    sections: Vec<Ref<Section64>>,
}

impl SegmentBuilder {
    fn size(&self) -> u32 {
        (self.sections.last()
            .map(|sect| sect.end())
            .unwrap_or(self.header.end()) - self.header.start()) as u32
    }
}

struct DylibCommandBuilder {
    header: Ref<DylibCommand>,
    additional_size: usize,
}

impl DylibCommandBuilder {
    fn size(&self) -> u32 {
        (self.header.size() + self.additional_size) as u32
    }
}

#[repr(C, packed)]
struct LinkEditDataCommand {
    command: u32,
    command_size: u32,
    data_offset: u32,
    data_size: u32,
}

#[repr(C, packed)]
struct DyldChainedFixupsHeader {
    fixups_version: u32, // 0
    starts_offset: u32,  // offset of DyldChainedStartsInImage in bytes, relative to start of this structure
    imports_offset: u32, // offset of imports table
    symbols_offset: u32, // offset of symbol strings
    imports_count: u32,  // number of imported symbol names
    imports_format: u32, // DYLD_CHAINED_IMPORT*
    symbols_format: u32, // 0 => uncompressed, 1 => zlib compressed
}

const fn is_power_of_2(num: u64) -> bool {
    if num == 0 { return false; }
    let mut i = 0u64;
    while i < 64 {
        if (1 << i) & num == num {
            return true;
        }
        i += 1;
    }
    false
}

// Thank you, Hagen von Eitzen: https://math.stackexchange.com/a/291494
macro_rules! nearest_multiple_of {
    (@unsafe $val:expr, $factor:expr) => {
        ((($val) - 1) | ($factor - 1)) + 1
    };

    ($val:expr, $factor:expr) => {{
        const _: () = assert!(is_power_of_2($factor));
        nearest_multiple_of!(@unsafe $val, $factor)
    }};
}

macro_rules! nearest_multiple_of_rt {
    ($val:expr, $factor:expr) => {{
        assert!(is_power_of_2($factor));
        nearest_multiple_of!(@unsafe $val, $factor)
    }};
}

impl MachOEncoder {
    pub fn new() -> Self { Self::default() }

    fn alloc<T>(&mut self) -> Ref<T> {
        let reff = Ref::new(self.data.len());
        self.pad_with_zeroes(mem::size_of::<T>());
        reff
    }

    fn alloc_cmd<T>(&mut self) -> Ref<T> {
        self.num_load_commands += 1;
        self.alloc()
    }

    fn alloc_segment(&mut self) -> SegmentBuilder {
        self.num_segments += 1;
        let header = self.alloc_cmd::<LcSegment64>();
        SegmentBuilder {
            header,
            sections: Default::default(),
        }
    }

    fn alloc_section(&mut self, segment: &mut SegmentBuilder) -> Ref<Section64> {
        let section = self.alloc::<Section64>();
        segment.sections.push(section);
        section
    }

    fn alloc_dylib_command(&mut self, name: &str) -> DylibCommandBuilder {
        let header = self.alloc_cmd::<DylibCommand>();
        let name_begin = self.pos();
        self.push_null_terminated_string(name);

        // According to https://opensource.apple.com/source/xnu/xnu-7195.81.3/EXTERNAL_HEADERS/mach-o/loader.h.auto.html
        // we're supposed to pad to the next 4 byte boundary, but the sample files I've examined seem to pad to 8 bytes
        // (which honestly makes more sense to me anyway, given the presence of 64 bit values in some of the load
        // commands)
        self.pad_to_next_boundary::<8>();
        let end = self.pos();

        DylibCommandBuilder {
            header,
            additional_size: end - name_begin,
        }
    }

    fn alloc_symbol_table_entry(&mut self) -> Ref<SymbolTableEntry> {
        self.num_symbol_table_entries += 1;
        self.alloc()
    }

    fn push_null_terminated_string(&mut self, val: &str) -> usize {
        let pos = self.pos();
        self.data.extend(val.as_bytes());
        self.data.push(0);
        pos
    }

    #[allow(unused)]
    fn get<T>(&self, addr: Ref<T>) -> &T {
        debug_assert!(addr.addr + mem::size_of::<T>() <= self.data.len());
        let (head, body, _tail) = unsafe { self.data[addr.addr..].align_to::<T>() };
        assert!(head.is_empty(), "unaligned data");
        &body[0]
    }

    fn get_mut<T>(&mut self, addr: Ref<T>) -> &mut T {
        debug_assert!(addr.addr + mem::size_of::<T>() <= self.data.len());
        let (head, body, _tail) = unsafe { self.data[addr.addr..].align_to_mut::<T>() };
        assert!(head.is_empty(), "unaligned data");
        &mut body[0]
    }

    fn push<T>(&mut self, value: T) {
        let addr = self.alloc();
        *self.get_mut(addr) = value;
    }

    fn pos(&self) -> usize { self.data.len() }

    fn pad_with_zeroes(&mut self, size: usize) {
        self.data.extend(std::iter::repeat(0).take(size as usize));
    }

    fn pad_to_next_boundary<const B: u64>(&mut self) {
        let padded_pos = nearest_multiple_of_rt!(self.data.len() as u64, B);
        self.pad_with_zeroes(padded_pos as usize - self.pos());
    }

    fn push_uleb128(&mut self, mut value: u32) {
        loop {
            let mut next_byte = (value & 0x7F) as u8;
            value >>= 7;
            if value != 0 {
                next_byte |= 0x80;
            }
            self.push(next_byte);

            if value == 0 { break; }
        }
    }

    pub fn write(&mut self, dest: &mut impl Write) -> io::Result<()> {
        let mach_header = self.alloc::<MachHeader>();

        let lc_begin = self.pos();

        let page_zero = self.alloc_segment();

        let mut text_segment = self.alloc_segment();
        let text_section = self.alloc_section(&mut text_segment);

        let link_edit_segment = self.alloc_segment();

        let chained_fixups = self.alloc_cmd::<LinkEditDataCommand>();
        let exports_trie = self.alloc_cmd::<LinkEditDataCommand>();

        let symbol_table = self.alloc_cmd::<SymbolTableCommand>();
        let dynamic_symbol_table = self.alloc_cmd::<DynamicSymbolTableCommand>();
        
        let load_dylinker_begin = self.pos();
        let load_dylinker = self.alloc_cmd::<DylinkerCommand>();
        self.push_null_terminated_string("/usr/lib/dyld");

        self.pad_to_next_boundary::<8>();
        let load_dylinker_size = self.pos() - load_dylinker_begin;

        let entry_point = self.alloc_cmd::<EntryPointCommand>();

        let load_lib_system = self.alloc_dylib_command("/usr/lib/libSystem.B.dylib");

        let function_starts = self.alloc_cmd::<LinkEditDataCommand>();

        let lc_end = self.pos();

        let mut code = Arm64Encoder::new();
        // TODO: this should actually be a 32-bit move, if we supported that. Not that it matters in this case.
        code.movz64(Reg::R0, 1, 0);
        code.ret(Reg::R0);
        let code = code.get_bytes();

        let text_sections_size: u64 = code.len() as u64;

        const PAGE_SIZE: u64 = 0x4000;

        let text_addr: u64 = 0x0000_0001_0000_0000;
        let text_end_addr = nearest_multiple_of!(text_addr + lc_end as u64 + text_sections_size, PAGE_SIZE);
        let text_sections_addr = text_end_addr - text_sections_size;
        let text_segment_size = text_end_addr - text_addr;

        let padding_size = text_sections_addr - text_addr - lc_end as u64;
        self.pad_with_zeroes(padding_size as usize);
        self.data.extend(&code);

        let link_edit_begin = self.pos();

        let chained_fixups_header = self.alloc::<DyldChainedFixupsHeader>();
        self.pad_to_next_boundary::<8>();
        // Push dyld_chained_starts_in_image (a dynamically-sized structure)
        let chained_starts_offset = self.pos() - chained_fixups_header.start();
        self.push(self.num_segments as u32);
        for i in 0..self.num_segments {
            // AFAICT, the offset is relative to the start of dyld_chained_starts_in_image, which makes any offset less
            // than 4 + 4 * num_segments invalid, thus 0 should indicate "no starts for this page"
            self.push(0 as u32); 
        }

        let imports_count = 0;

        let imports_offset = self.pos() - chained_fixups_header.start();
        // TODO: create a bitfield data structure for the `dyld_chained_import` struct
        self.push(0 as u32);
        self.pad_to_next_boundary::<8>();
        let import_symbols_offset = if imports_count == 0 {
            imports_offset
        } else {
            self.pos() - chained_fixups_header.start()
        };
        // TODO: add symbols

        self.pad_to_next_boundary::<8>();
        
        *self.get_mut(chained_fixups_header) = DyldChainedFixupsHeader {
            fixups_version: 0,
            starts_offset: chained_starts_offset as u32,
            imports_offset: imports_offset as u32,
            symbols_offset: import_symbols_offset as u32,
            imports_count,
            imports_format: DYLD_CHAINED_IMPORT,
            symbols_format: 0, // uncompressed
        };

        let chained_fixups_data_size = self.pos() - chained_fixups_header.start();

        let exports_trie_start = self.pos();
        self.pad_with_zeroes(8);
        let exports_trie_len = self.pos() - exports_trie_start;

        let function_starts_start = self.pos();
        // offset to first function, relative to the beginning of the __TEXT segment.
        // subsequent functions would be specified relative to the previous one in the list.
        self.push_uleb128((text_sections_addr - text_addr) as u32);
        self.pad_to_next_boundary::<8>();
        let function_starts_len = self.pos() - function_starts_start;

        let symbol_table_begin = self.pos();
        let mh_execute_header_entry = self.alloc_symbol_table_entry();
        let main_entry = self.alloc_symbol_table_entry();

        let string_table_begin = self.pos();
        self.push_null_terminated_string(" ");
        let mh_execute_header_str_offset = self.push_null_terminated_string("__mh_execute_header");
        let main_str_offset = self.push_null_terminated_string("_main");
        self.pad_to_next_boundary::<8>();
        let string_table_len = self.pos() - string_table_begin;

        *self.get_mut(mh_execute_header_entry) = SymbolTableEntry {
            string_table_offset: (mh_execute_header_str_offset - string_table_begin) as u32,
            ty: 0x0F,
            section_number: 1,
            desc: 0x0010,
            value: text_addr,
        };
        *self.get_mut(main_entry) = SymbolTableEntry {
            string_table_offset: (main_str_offset - string_table_begin) as u32,
            ty: 0x0F,
            section_number: 1,
            desc: 0x0000,
            value: text_sections_addr,
        };

        let link_edit_end = self.pos();

        *self.get_mut(mach_header) = MachHeader {
            magic: MH_MAGIC_64,
            cpu_type: CPU_TYPE_ARM64,
            cpu_subtype: CPU_SUBTYPE_ARM64_ALL,
            file_type: MH_EXECUTE,

            num_commands: self.num_load_commands,
            size_of_commands: (lc_end - lc_begin) as u32,

            flags: MH_NOUNDEFS | MH_DYLDLINK | MH_TWOLEVEL | MH_PIE,
            reserved: 0,
        };
        *self.get_mut(page_zero.header) = LcSegment64 {
            command: LC_SEGMENT_64,
            command_size: page_zero.size(),
            name: encode_string_16("__PAGEZERO"),
            vm_addr: 0,
            vm_size: text_addr,
            file_offset: 0,
            file_size: 0,
            max_vm_protection: VM_PROT_NONE,
            initial_vm_protection: VM_PROT_NONE,
            num_sections: page_zero.sections.len() as u32,
            flags: 0,
        };
        *self.get_mut(text_segment.header) = LcSegment64 {
            command: LC_SEGMENT_64,
            command_size: text_segment.size(),
            name: encode_string_16("__TEXT"),
            vm_addr: text_addr,
            vm_size: text_segment_size,
            file_offset: 0,
            file_size: text_segment_size,
            max_vm_protection: VM_PROT_READ | VM_PROT_EXECUTE,
            initial_vm_protection: VM_PROT_READ | VM_PROT_EXECUTE,
            num_sections: text_segment.sections.len() as u32,
            flags: 0,
        };
        *self.get_mut(text_section) = Section64 {
            name: encode_string_16("__text"),
            segment_name: encode_string_16("__TEXT"),
            vm_addr: text_sections_addr,
            vm_size: text_sections_size,
            file_offset: (text_sections_addr - text_addr) as u32,
            alignment: 2, // stored as log base 2, so this is actually 4
            relocations_file_offset: 0,
            num_relocations: 0,
            flags: SectionFlags::new(SectionType::Regular, S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS),
            reserved: [0; 3],
        };
        *self.get_mut(link_edit_segment.header) = LcSegment64 {
            command: LC_SEGMENT_64,
            command_size: link_edit_segment.size(),
            name: encode_string_16("__LINKEDIT"),
            vm_addr: text_end_addr,
            vm_size: PAGE_SIZE, // TODO: increase this size if needed
            file_offset: link_edit_begin as u64,
            file_size: (link_edit_end - link_edit_begin) as u64,
            max_vm_protection: VM_PROT_READ,
            initial_vm_protection: VM_PROT_READ,
            num_sections: 0,
            flags: 0,
        };
        *self.get_mut(chained_fixups) = LinkEditDataCommand {
            command: LC_DYLD_CHAINED_FIXUPS,
            command_size: chained_fixups.size() as u32,
            data_offset: chained_fixups_header.start() as u32,
            data_size: chained_fixups_data_size as u32,
        };
        *self.get_mut(exports_trie) = LinkEditDataCommand {
            command: LC_DYLD_EXPORTS_TRIE,
            command_size: exports_trie.size() as u32,
            data_offset: exports_trie_start as u32,
            data_size: exports_trie_len as u32,
        };
        *self.get_mut(function_starts) = LinkEditDataCommand {
            command: LC_FUNCTION_STARTS,
            command_size: function_starts.size() as u32,
            data_offset: function_starts_start as u32,
            data_size: function_starts_len as u32,
        };
        *self.get_mut(load_lib_system.header) = DylibCommand {
            command: LC_LOAD_DYLIB,
            command_size: load_lib_system.size(),
            dylib: Dylib {
                name_offset: 0x18,
                timestamp: 2,
                current_version: 0x05_27_0000,
                compatibility_version: 0x00_01_0000,
            },
        };
        *self.get_mut(symbol_table) = SymbolTableCommand {
            command: LC_SYMTAB,
            command_size: symbol_table.size() as u32,
            symbol_table_offset: symbol_table_begin as u32,
            num_symbols: self.num_symbol_table_entries,
            string_table_offset: string_table_begin as u32,
            string_table_size: string_table_len as u32,
        };
        *self.get_mut(dynamic_symbol_table) = DynamicSymbolTableCommand {
            command: LC_DYSYMTAB,
            command_size: dynamic_symbol_table.size() as u32,

            local_symbols_index: 0,
            num_local_symbols: 0,

            extern_symbols_index: 0,
            num_extern_symbols: 2,

            undef_symbols_index: 2,
            num_undef_symbols: 0,

            toc_offset: 0,
            num_toc_entries: 0,

            module_table_offset: 0,
            num_module_table_entries: 0,

            referenced_symbol_table_offset: 0,
            num_referenced_symbol_table_entries: 0,

            indirect_symbol_table_offset: 0,
            num_indirect_symbol_table_entries: 0,

            extern_relocation_entries_offset: 0,
            num_extern_relocation_entries: 0,

            local_relocation_entries_offset: 0,
            num_local_relocation_entries: 0,
        };
        *self.get_mut(load_dylinker) = DylinkerCommand {
            command: LC_LOAD_DYLINKER,
            command_size: load_dylinker_size as u32,
            name_offset: load_dylinker.size() as u32,
        };
        *self.get_mut(entry_point) = EntryPointCommand {
            command: LC_MAIN,
            command_size: entry_point.size() as u32,
            entry_point_file_offset: text_sections_addr - text_addr,
            stack_size: 0,
        };

        dest.write_all(&self.data)?;

        Ok(())
    }
}
