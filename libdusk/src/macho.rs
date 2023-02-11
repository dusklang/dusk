#![allow(unused)]

use std::io::{self, Write};
use std::marker::PhantomData;
use std::mem;

use crate::arm64::{Arm64Encoder, Reg};

#[derive(Default)]
pub struct MachOEncoder {
    data: Vec<u8>, // TODO: support writing directly to a file instead of copying from a byte buffer?
    num_load_commands: u32,
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

const LC_SYMTAB:        u32 = 0x0000_0002;
const LC_DYSYMTAB:      u32 = 0x0000_000B;
const LC_LOAD_DYLINKER: u32 = 0x0000_000E;

const LC_SEGMENT_64: u32 = 0x0000_0019;
const LC_LOAD_DYLIB: u32 = 0x0000_000C;

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

    local_relocation_entries_offset: u32,
    num_local_relocation_entries: u32,
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
    ($val:expr, $factor:expr) => {{
        const _: () = assert!(is_power_of_2($factor));
        ((($val) - 1) | ($factor - 1)) + 1
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
        self.data.extend(name.as_bytes());
        let size = header.size() + name.len();

        // According to https://opensource.apple.com/source/xnu/xnu-7195.81.3/EXTERNAL_HEADERS/mach-o/loader.h.auto.html
        // we're supposed to pad to the next 4 byte boundary, but the sample files I've examined seem to pad to 8 bytes
        // (which honestly makes more sense to me anyway, given the presence of 64 bit values in some of the load
        // commands)
        let padded_size = nearest_multiple_of!(size + 1, 8);
        self.pad_with_zeroes(padded_size - size);
        DylibCommandBuilder {
            header,
            additional_size: padded_size - header.size(),
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

    fn pos(&self) -> usize { self.data.len() }

    fn pad_with_zeroes(&mut self, size: usize) {
        self.data.extend(std::iter::repeat(0).take(size as usize));
    }

    pub fn write(&mut self, dest: &mut impl Write) -> io::Result<()> {
        let mach_header = self.alloc::<MachHeader>();

        let lc_begin = self.pos();

        let page_zero = self.alloc_segment();

        let mut text_segment = self.alloc_segment();
        let text_section = self.alloc_section(&mut text_segment);

        let link_edit_segment = self.alloc_segment();

        let load_lib_system = self.alloc_dylib_command("/usr/lib/libSystem.B.dylib");

        let symbol_table = self.alloc_cmd::<SymbolTableCommand>();
        let dynamic_symbol_table = self.alloc_cmd::<DynamicSymbolTableCommand>();
        
        let load_dylinker_begin = self.pos();
        let load_dylinker = self.alloc_cmd::<DylinkerCommand>();
        self.push_null_terminated_string("/usr/lib/dyld");
        let load_dylinker_size_without_padding = self.pos() - load_dylinker_begin;
        let load_dylinker_size = nearest_multiple_of!(load_dylinker_size_without_padding, 8);

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

        let symbol_table_begin = self.pos();
        let mh_execute_header_entry = self.alloc_symbol_table_entry();
        let main_entry = self.alloc_symbol_table_entry();

        let string_table_begin = self.pos();
        self.push_null_terminated_string(" ");
        let mh_execute_header_str_offset = self.push_null_terminated_string("__mh_execute_header");
        let main_str_offset = self.push_null_terminated_string("_main");
        let string_table_len_without_padding = self.pos() - string_table_begin;
        let string_table_len = nearest_multiple_of!(string_table_len_without_padding, 8);
        self.pad_with_zeroes(string_table_len - string_table_len_without_padding);

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
            vm_size: PAGE_SIZE,
            file_offset: link_edit_begin as u64,
            file_size: (link_edit_end - link_edit_begin) as u64,
            max_vm_protection: VM_PROT_READ,
            initial_vm_protection: VM_PROT_READ,
            num_sections: 0,
            flags: 0,
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

            local_relocation_entries_offset: 0,
            num_local_relocation_entries: 0,
        };
        *self.get_mut(load_dylinker) = DylinkerCommand {
            command: LC_LOAD_DYLINKER,
            command_size: dbg!(load_dylinker_size as u32),
            name_offset: dbg!(load_dylinker.size() as u32),
        };

        dest.write_all(&self.data)?;

        Ok(())
    }
}
