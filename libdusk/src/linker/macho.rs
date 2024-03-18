#![allow(unused)]

use std::collections::HashMap;
use std::ffi::{CString, CStr};
use std::hash::Hash;
use std::io::{self, Write};
use std::marker::PhantomData;
use std::path::PathBuf;
use std::mem;

use rsa::sha2::{Sha256, Digest};
use index_vec::define_index_type;

use dusk_proc_macros::ByteSwap;

use crate::backend::{Backend, CodeBlob, CodeBlobExt, Indirection};
use crate::linker::exe::*;
use crate::linker::byte_swap::*;
use crate::linker::Linker;
use crate::index_vec::*;
use crate::mir::FuncId;
use crate::tbd_parser::parse_tbd;
use crate::driver::Driver;

#[derive(Default)]
pub struct MachOLinker {
    buf: Buffer,
    num_load_commands: u32,
    num_symbol_table_entries: u32,
    segments: IndexVec<SegmentId, SegmentBuilder>,
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
const LC_UUID: u32 = 0x0000_001B;
const LC_CODE_SIGNATURE: u32 = 0x0000_001D;
const LC_FUNCTION_STARTS: u32 = 0x0000_0026;
const LC_DATA_IN_CODE: u32 = 0x0000_0029;
const LC_SOURCE_VERSION: u32 = 0x0000_002A;
const LC_MAIN: u32 = 0x28 | LC_REQ_DYLD;
const LC_BUILD_VERSION: u32 = 0x0000_0032;
const LC_DYLD_EXPORTS_TRIE: u32 = 0x33 | LC_REQ_DYLD;
const LC_DYLD_CHAINED_FIXUPS: u32 = 0x34 | LC_REQ_DYLD;
const LC_LOAD_DYLIB: u32 = 0x0000_000C;

const DYLD_CHAINED_PTR_64_OFFSET: u16 = 6;

const DYLD_CHAINED_IMPORT: u32 = 0x0000_0001;

const VM_PROT_NONE:    u32 = 0x0000_0000;
const VM_PROT_READ:    u32 = 0x0000_0001;
const VM_PROT_WRITE:   u32 = 0x0000_0002;
const VM_PROT_EXECUTE: u32 = 0x0000_0004;

const SG_HIGHVM: u32 = 0x1;
const SG_FVMLIB: u32 = 0x2;
const SG_NORELOC: u32 = 0x4;
const SG_PROTECTED_VERSION_1: u32 = 0x8;
const SG_READ_ONLY: u32 = 0x10;

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

const CSMAGIC_REQUIREMENT: u32 = 0xFADE_0C00;
const CSMAGIC_REQUIREMENTS: u32 = 0xFADE_0C01;
const CSMAGIC_CODEDIRECTORY: u32 = 0xFADE_0C02;
const CSMAGIC_EMBEDDED_SIGNATURE: u32 = 0xFADE_0CC0;
const CSMAGIC_DETACHED_SIGNATURE: u32 = 0xFADE_0CC1;
const CSSLOT_CODEDIRECTORY: u32 = 0;

const CD_HASH_TYPE_SHA1: u8 = 1;
const CD_HASH_TYPE_SHA256: u8 = 2;

#[repr(C)]
#[derive(ByteSwap)]
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

#[repr(C)]
#[derive(ByteSwap)]
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

#[repr(C)]
#[derive(ByteSwap)]
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

#[repr(C)]
#[derive(ByteSwap)]
struct Dylib {
    name_offset: u32,
    timestamp: u32,
    current_version: u32,
    compatibility_version: u32,
}

#[repr(C)]
#[derive(ByteSwap)]
struct DylibCommand {
    command: u32,
    command_size: u32,
    dylib: Dylib,
}

#[repr(C)]
#[derive(ByteSwap)]
struct DylinkerCommand {
    command: u32,
    command_size: u32,
    name_offset: u32,
}

#[repr(C)]
#[derive(ByteSwap)]
struct UuidCommand {
    command: u32,
    command_size: u32,
    uuid: [u8; 16],
}

#[derive(Copy, Clone)]
#[repr(u32)]
enum PlatformEnum {
    MacOs = 1,
    Ios,
    TvOs,
    WatchOs,
    BridgeOs,
    MacCatalyst,
    IosSimulator,
    TvOsSimulator,
    WatchOsSimulator,
    DriverKit,
}

type Platform = u32;

#[derive(Copy, Clone)]
#[repr(u32)]
enum ToolEnum {
    Clang = 1,
    Swift,
    Ld,
}

type Tool = u32;

#[repr(C)]
#[derive(ByteSwap)]
struct BuildVersionCommand {
    command: u32,
    command_size: u32,
    platform: Platform,
    min_os: u32,
    sdk: u32,
    num_tools: u32,
}

#[repr(C)]
#[derive(ByteSwap)]
struct BuildToolVersion {
    tool: Tool,
    version: u32,
}

#[repr(C)]
#[derive(ByteSwap)]
struct SourceVersionCommand {
    command: u32,
    command_size: u32,
    version: u64,
}

#[repr(C)]
#[derive(ByteSwap)]
struct SymbolTableCommand {
    command: u32,
    command_size: u32,
    symbol_table_offset: u32,
    num_symbols: u32,
    string_table_offset: u32,
    string_table_size: u32,
}

#[repr(C)]
#[derive(ByteSwap)]
struct SymbolTableEntry {
    string_table_offset: u32,
    ty: SymbolFlags,
    section_number: u8,
    description: SymbolDescription,
    value: u64,
}

#[repr(C)]
#[derive(ByteSwap, Clone, Copy)]
struct SymbolDescription(u16);

impl SymbolDescription {
    fn new(reference_type: u8, referenced_dynamically: bool, no_dead_strip: bool, weak_ref: bool, weak_def: bool, library_ordinal: u8) -> Self {
        assert!(reference_type <= 7);
        Self(reference_type as u16 | (referenced_dynamically as u16) << 4 | (no_dead_strip as u16) << 5 | (weak_ref as u16) << 6 | (weak_def as u16) << 7 | (library_ordinal as u16) << 7)
    }
}

#[repr(C)]
#[derive(ByteSwap)]
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

#[repr(C)]
#[derive(ByteSwap)]
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

#[derive(Clone, Copy)]
#[repr(transparent)]
struct SectionFlags(u32);

impl ByteSwap for SectionFlags {
    fn byte_swap(&mut self) {
        self.0.byte_swap();
    }
}

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

struct TextSegmentBuilder {
    segment: SegmentId,
    sections: Vec<TextSection>,
}

struct TextSection {
    size: usize,
    id: SectionId,
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

#[repr(C)]
#[derive(ByteSwap)]
struct LinkEditDataCommand {
    command: u32,
    command_size: u32,
    data_offset: u32,
    data_size: u32,
}

#[repr(C)]
#[derive(ByteSwap)]
struct DyldChainedFixupsHeader {
    fixups_version: u32, // 0
    starts_offset: u32,  // offset of DyldChainedStartsInImage in bytes, relative to start of this structure
    imports_offset: u32, // offset of imports table
    symbols_offset: u32, // offset of symbol strings
    imports_count: u32,  // number of imported symbol names
    imports_format: u32, // DYLD_CHAINED_IMPORT*
    symbols_format: u32, // 0 => uncompressed, 1 => zlib compressed
}

#[repr(C)]
#[derive(ByteSwap)]
struct DyldChainedStartsInSegment {
    size: u32, // size of this
    page_size: u16, // 0x1000 or 0x4000
    pointer_format: u16, // DYLD_CHAINED_PTR_*
    segment_offset: u64, // offset in memory to start of segment

    // TODO: The ByteSwap macro doesn't support packed structs, so I need to cut off the end of this struct, for now.
}

#[repr(C)]
#[derive(ByteSwap)]
struct DyldChainedPtr64(u64);

impl ResolvedRefMut<'_, DyldChainedPtr64> {
    fn set_next(&mut self, next: u16) {
        assert!(next <= 0xFFF); // 12 bits
        let mask = 0xFFF << 51;
        let value = unsafe { self.get_value() };
        value.0 = value.0 & !mask;
        value.0 |= (next as u64) << 51;
    }
}

#[repr(C)]
#[derive(ByteSwap)]
struct DyldChainedPtr64Bind(u64);

impl DyldChainedPtr64Bind {
    fn new(ordinal: u32, addend: u8, next: u16) -> Self {
        assert!(ordinal <= 0xFFFFFF); // 24 bits
        assert!(next <= 0xFFF); // 12 bits
        Self(ordinal as u64 | (addend as u64) << 24 | (next as u64) << 51 | 1 << 63)
    }
}

impl From<Ref<DyldChainedPtr64Bind>> for Ref<DyldChainedPtr64> {
    fn from(value: Ref<DyldChainedPtr64Bind>) -> Self {
        Ref {
            addr: value.addr,
            rva: value.rva,
            _phantom: PhantomData,
        }
    }
}

#[repr(C)]
#[derive(ByteSwap)]
struct DyldChainedPtr64Rebase(u64);

impl DyldChainedPtr64Rebase {
    fn new(target: u64, high8: u8, next: u16) -> Self {
        assert!(target <= 0xF_FFFF_FFFF); // 36 bits
        assert!(next <= 0xFFF); // 12 bits
        Self(target as u64 | (high8 as u64) << 36 | (next as u64) << 51)
    }
}

impl From<Ref<DyldChainedPtr64Rebase>> for Ref<DyldChainedPtr64> {
    fn from(value: Ref<DyldChainedPtr64Rebase>) -> Self {
        Ref::new(value.addr, value.rva)
    }
}

#[repr(C)]
#[derive(ByteSwap)]
struct DyldChainedImport(u32);

impl DyldChainedImport {
    fn new(lib_ordinal: u8, weak_import: bool, name_offset: u32) -> Self {
        assert!(name_offset <= 0x7F_FFFF);
        Self(lib_ordinal as u32 | (weak_import as u32) << 8 | name_offset << 9)
    }
}

#[derive(Copy, Clone)]
#[repr(u8)]
enum SymbolType {
    Undefined = 0,
    Absolute = 1,
    Indirect = 5,
    PreboundUndefined = 6,
    DefinedInSectionNumber = 7,
}

#[repr(C)]
#[derive(ByteSwap)]
struct SymbolFlags(u8);

impl SymbolFlags {
    fn new(external: bool, ty: SymbolType, private_external: bool, stab: u8) -> Self {
        assert!(stab <= 7);
        Self(external as u8 | (ty as u8) << 1 | (private_external as u8) << 4 | stab << 5)
    }
}

#[repr(C)]
#[derive(ByteSwap)]
struct SuperBlobHeader {
    magic: u32,
    length: u32,
    count: u32,
}

#[repr(C)]
#[derive(ByteSwap)]
struct BlobIndex {
    ty: u32,
    offset: u32,
}

#[repr(C)]
#[derive(ByteSwap)]
struct CodeDirectory {
    magic: u32,
    length: u32,
    version: u32,
    flags: u32,
    hash_offset: u32,
    ident_offset: u32,
    num_special_slots: u32,
    num_code_slots: u32,
    code_limit: u32,
    hash_size: u8,
    hash_type: u8,
    platform: u8,
    page_size: u8,
    spare_2: u32,
    scatter_offset: u32,
    team_offset: u32,
    spare_3: u32,
    code_limit_64: u64,
    exec_seg_base: u64,
    exec_seg_limit: u64,
    exec_seg_flags: u64,
}

fn log_base_2(mut num: usize) -> usize {
    assert!(is_power_of_2(num));
    let mut ret_val = 0;
    while num > 1 {
        num >>= 1;
        ret_val += 1;
    }
    ret_val
}

const PAGE_SIZE: usize = 0x4000;
const TEXT_ADDR: u64 = 0x0000_0001_0000_0000;

// TODO: try to unify the multiple different segment/section representations somewhat
define_index_type!(struct SegmentId = u32;);
type SectionId = (SegmentId, usize);
struct SegmentBuilder {
    sections: Vec<SectionBuilder>,
    header: Ref<LcSegment64>,

    name: &'static str,
    vm_protection: u32,
    flags: u32,
    info: Option<SegmentInfo>,
}

struct SectionBuilder {
    header: Ref<Section64>,

    name: &'static str,
    alignment: usize,
    flags: SectionFlags,
    info: Option<SectionInfo>,
}

struct SegmentInfo {
    offset: SegmentOffset,
    size: SegmentSize,
}

enum SegmentOffset {
    PageZero,
    FileOffset(usize),
}

impl From<usize> for SegmentOffset {
    fn from(value: usize) -> Self {
        Self::FileOffset(value)
    }
}

enum SegmentSize {
    Same(usize),
    Different {
        vm_size: usize,
        file_size: usize,
    }
}

impl From<usize> for SegmentSize {
    fn from(value: usize) -> Self {
        Self::Same(value)
    }
}

#[derive(Clone, Copy)]
struct SectionInfo {
    offset: usize,
    size: usize,
}

impl MachOLinker {
    pub fn new() -> Self { Self::default() }
}

impl Linker for MachOLinker {
    fn write(&mut self, d: &Driver, main_function_index: FuncId, backend: &mut dyn Backend, dest: &mut dyn Write) -> io::Result<()> {
        let mut exe = MachOExe::new();
        let _lib_system = exe.import_dynamic_library("libSystem");

        backend.generate_func(d, main_function_index, true, &mut exe);
        let mut code = mem::take(&mut exe.code_blob).expect("generate_func must generate a code blob");

        let mach_header = self.buf.alloc::<MachHeader>();

        let lc_begin = self.buf.pos();

        let page_zero = self.alloc_segment("__PAGEZERO", VM_PROT_NONE, 0);
        self.add_info_to_segment(page_zero, SegmentOffset::PageZero, SegmentSize::Different { vm_size: TEXT_ADDR as usize, file_size: 0 });
        
        let mut text_segment = self.alloc_text_segment();
        let text_section = self.reserve_text_section(&mut text_segment, "__text", code.len(), 4, SectionFlags::new(SectionType::Regular, S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS));
        let cstring_section = (!exe.cstrings.is_empty()).then(|| self.reserve_text_section(&mut text_segment, "__cstring", exe.cstrings.len(), 1, SectionFlags::new(SectionType::CStringLiterals, 0)));
        let objc_methname_section = (!exe.objc_method_names.is_empty()).then(|| self.reserve_text_section(&mut text_segment, "__objc_methname", exe.objc_method_names.len(), 2, SectionFlags::new(SectionType::CStringLiterals, 0)));

        let (data_const, got_section, cfstring_section, objc_imageinfo_section) = if !exe.got_entries.is_empty() || !exe.constant_nsstrings.is_empty() {
            let segment = self.alloc_segment("__DATA_CONST", VM_PROT_READ | VM_PROT_WRITE, SG_READ_ONLY);

            let got_section = (!exe.got_entries.is_empty()).then(|| {
                self.alloc_section(segment, "__got", 8, SectionFlags::new(SectionType::NonLazySymbolPointers, 0))
            });

            let cfstring_section = (!exe.constant_nsstrings.is_empty()).then(|| {
                self.alloc_section(segment, "__cfstring", 8, SectionFlags::new(SectionType::Regular, 0))
            });

            let objc_imageinfo_section = self.alloc_section(segment, "__objc_imageinfo", 4, SectionFlags::new(SectionType::Regular, 0));

            (Some(segment), got_section, cfstring_section, Some(objc_imageinfo_section))
        } else {
            (None, None, None, None)
        };

        let (data_segment, objc_selrefs_section) = if !exe.objc_selectors.is_empty() {
            let segment = self.alloc_segment("__DATA", VM_PROT_READ | VM_PROT_WRITE, 0);

            let objc_selrefs_section = (!exe.objc_selectors.is_empty()).then(|| {
                self.alloc_section(segment, "__objc_selrefs", 8, SectionFlags::new(SectionType::LiteralPointers, S_ATTR_NO_DEAD_STRIP))
            });

            (Some(segment), objc_selrefs_section)
        } else {
            (None, None)
        };

        let link_edit_segment = self.alloc_segment("__LINKEDIT", VM_PROT_READ, 0);

        let chained_fixups = self.alloc_cmd::<LinkEditDataCommand>();
        let exports_trie = self.alloc_cmd::<LinkEditDataCommand>();
        
        let symbol_table = self.alloc_cmd::<SymbolTableCommand>();
        let dynamic_symbol_table = self.alloc_cmd::<DynamicSymbolTableCommand>();
        
        let load_dylinker = self.alloc_cmd::<DylinkerCommand>();
        self.buf.push_null_terminated_string("/usr/lib/dyld");
        self.buf.pad_to_next_boundary(8);
        let load_dylinker_size = self.buf.pos() - load_dylinker.addr;
        
        // let uuid = self.alloc_cmd::<UuidCommand>();
        
        let build_version = self.alloc_cmd::<BuildVersionCommand>();
        let ld_tool = self.buf.alloc::<BuildToolVersion>();
        let build_version_len = self.buf.pos() - build_version.addr;
        
        let src_version = self.alloc_cmd::<SourceVersionCommand>();
        
        let entry_point = self.alloc_cmd::<EntryPointCommand>();

        let dylib_load_commands: Vec<_> = exe.dylibs.iter()
            .map(|dylib| self.alloc_dylib_command(&dylib.name))
            .collect();
        
        let function_starts = self.alloc_cmd::<LinkEditDataCommand>();
        
        let data_in_code = self.alloc_cmd::<LinkEditDataCommand>();
        
        let code_signature = self.alloc_cmd::<LinkEditDataCommand>();
        
        let lc_end = self.buf.pos();

        self.layout_text_sections(&text_segment, lc_end);
        if let Some(cstring_section) = cstring_section {
            self.fill_text_section(&text_segment, cstring_section, &exe.cstrings);
        }
        if let Some(objc_methname_section) = objc_methname_section {
            self.fill_text_section(&text_segment, objc_methname_section, &exe.objc_method_names);
        }

        struct Symbol {
            symbol: String,
            internal_address: Option<u64>,
            description: SymbolDescription,
        }

        let mut local_symbols = Vec::new();
        let mut imported_symbols = Vec::new();
        local_symbols.push(
            Symbol {
                symbol: "__mh_execute_header".to_string(),
                internal_address: Some(TEXT_ADDR),
                description: SymbolDescription::new(0, true, false, false, false, 0),
            }
        );
        let text_section_offset = self.get_section_offset(text_section);
        local_symbols.push(
            Symbol {
                symbol: "_main".to_string(),
                internal_address: Some(TEXT_ADDR + text_section_offset as u64),
                description: SymbolDescription::new(0, false, false, false, false, 0),
            }
        );
        for import in &exe.imported_symbols {
            imported_symbols.push(
                Symbol {
                    symbol: import.name.clone(),
                    internal_address: None,
                    description: SymbolDescription::new(0, false, false, false, false, import.dylib.index() as u8 + 1),
                }
            )
        }

        let data_const_begin = self.buf.pos();
        let mut got_begin = self.buf.pos();
        let mut cfstrings_begin = self.buf.pos();

        let mut data_const_size = 0;

        if let Some(segment) = data_const {
            let mut prev_ptr: Option<Ref<DyldChainedPtr64>> = None;

            if let Some(section) = got_section {
                got_begin = self.buf.pos();
                for &import in &exe.got_entries {
                    self.push_chained_fixup(&mut prev_ptr, DyldChainedPtr64Bind::new(import.index() as u32, 0, 0));
                }
                let got_size = self.buf.pos() - got_begin;
                self.add_info_to_section(section, got_begin, got_size);
            }

            if let Some(section) = cfstring_section {
                let cf_constant_string_class_reference_import = exe.cf_constant_string_class_reference_import.unwrap();
                cfstrings_begin = self.buf.pos();
                for str in &exe.constant_nsstrings {
                    // TODO: handle crossing page boundaries (e.g., I'm assuming we don't want to put half of the
                    // CFString's fields on a new page)
                    let cstring_section_offset = cstring_section.map(|section| self.get_section_offset(section));
                    let (offset, flags) = match str.location {
                        ConstantNSStringLocation::CStringSectionOffset(offset) => (cstring_section_offset.unwrap() + offset, 0x7C8 as u64),
                        // TODO: UTF-16 strings
                        // ConstantNSStringLocation::UStringSectionOffset(offset) => (text_segment.sections[ustring_section.as_ref().unwrap().id].offset + offset, 0x7D0 as u64),
                    };
                    self.push_chained_fixup(&mut prev_ptr, DyldChainedPtr64Bind::new(cf_constant_string_class_reference_import.index() as u32, 0, 0));
                    self.buf.push(flags);
                    self.push_chained_fixup(&mut prev_ptr, DyldChainedPtr64Rebase::new(offset as u64, 0, 0));
                    self.buf.push(str.size);
                }
                let cfstrings_size = self.buf.pos() - cfstrings_begin;
                self.add_info_to_section(section, cfstrings_begin, cfstrings_size);
            }

            let objc_imageinfo_begin = self.buf.pos();
            self.buf.push([0u8, 0, 0, 0, 4, 0, 0, 0]);
            let objc_imageinfo_size = self.buf.pos() - objc_imageinfo_begin;
            self.add_info_to_section(objc_imageinfo_section.unwrap(), objc_imageinfo_begin, objc_imageinfo_size);

            self.buf.pad_to_next_boundary(PAGE_SIZE);
            data_const_size = self.buf.pos() - data_const_begin;
            self.add_info_to_segment(segment, data_const_begin, data_const_size);
        }

        let data_segment_begin = self.buf.pos();
        let mut data_segment_size = self.buf.pos();
        let mut objc_selrefs_begin = self.buf.pos();
        if let Some(segment) = data_segment {
            let mut prev_ptr: Option<Ref<DyldChainedPtr64>> = None;

            if let Some(section) = objc_selrefs_section {
                let objc_methname_section_offset = objc_methname_section.map(|section| self.get_section_offset(section));
                objc_selrefs_begin = self.buf.pos();
                for &selector in &exe.objc_selectors {
                    let offset = objc_methname_section_offset.unwrap() + selector.offset;
                    self.push_chained_fixup(&mut prev_ptr, DyldChainedPtr64Rebase::new(offset as u64, 0, 0));
                }
                let objc_selrefs_size = self.buf.pos() - objc_selrefs_begin;
                self.add_info_to_section(section, objc_selrefs_begin, objc_selrefs_size);
            }

            self.buf.pad_to_next_boundary(PAGE_SIZE);
            data_segment_size = self.buf.pos() - data_segment_begin;
            self.add_info_to_segment(segment, data_segment_begin, data_segment_size);
        }
        
        let link_edit_begin = self.buf.pos();
        
        let chained_fixups_header = self.buf.alloc::<DyldChainedFixupsHeader>();
        self.buf.pad_to_next_boundary(8);
        // Push dyld_chained_starts_in_image (a dynamically-sized structure)
        let chained_starts_offset = self.buf.pos();
        let num_segments = self.segments.len() as u32;
        self.buf.push(num_segments as u32);
        let mut data_const_starts_offset = None;
        let mut data_segment_starts_offset = None;
        for i in self.segments.indices() {
            if data_const == Some(i) {
                data_const_starts_offset = Some(self.buf.alloc::<u32>());
            } else if data_segment == Some(i) {
                data_segment_starts_offset = Some(self.buf.alloc::<u32>());
            } else {
                // AFAICT, the offset is relative to the start of dyld_chained_starts_in_image, which makes any offset less
                // than 4 + 4 * num_segments invalid, thus 0 should indicate "no starts for this page"
                self.buf.push(0 as u32);
            }
        }

        // TODO: reduce code duplication between __DATA and __DATA_CONST.
        if let Some(data_const_starts_offset) = data_const_starts_offset {
            self.buf.pad_to_next_boundary(8);
            let chained_starts_in_segment_pos = self.buf.pos();
            self.buf.get_mut(data_const_starts_offset).set((chained_starts_in_segment_pos -  chained_starts_offset) as u32);
            
            let chained_starts_in_segment = self.buf.alloc::<DyldChainedStartsInSegment>();
            // TODO: these should be fields of DyldChainedStartsInSegment, but need to be here instead because packed
            // structs are not yet supported by our ByteSwap macro.
            self.buf.push(0 as u32); // max_valid_pointer
            let page_count = (data_const_size - 1) / PAGE_SIZE + 1;
            self.buf.push(u16::try_from(page_count).unwrap());

            // The first fix-up in each page is at offset 0, because the whole point of the __got section is to provide
            // fixed up addresses, and __cfstring values also begin with a fix-up.
            for _ in 0..page_count {
                self.buf.push(0 as u16);
            }

            let chained_starts_in_segment_size = self.buf.pos() - chained_starts_in_segment_pos;
            self.buf.get_mut(chained_starts_in_segment).set(
                DyldChainedStartsInSegment {
                    size: chained_starts_in_segment_size as u32,
                    page_size: 0x4000,
                    pointer_format: DYLD_CHAINED_PTR_64_OFFSET,
                    segment_offset: data_const_begin as u64,
                }
            );
        }

        if let Some(data_segment_starts_offset) = data_segment_starts_offset {
            self.buf.pad_to_next_boundary(8);
            let chained_starts_in_segment_pos = self.buf.pos();
            self.buf.get_mut(data_segment_starts_offset).set((chained_starts_in_segment_pos - chained_starts_offset) as u32);
            
            let chained_starts_in_segment = self.buf.alloc::<DyldChainedStartsInSegment>();
            // TODO: these should be fields of DyldChainedStartsInSegment, but need to be here instead because packed
            // structs are not yet supported by our ByteSwap macro.
            self.buf.push(0 as u32); // max_valid_pointer
            let page_count = (data_segment_size - 1) / PAGE_SIZE + 1;
            self.buf.push(u16::try_from(page_count).unwrap());

            // The first fix-up in each page is at offset 0, because the whole point of the __objc_selrefs and
            // __objc_classrefs sections is to provide fixed up addresses.
            for _ in 0..page_count {
                self.buf.push(0 as u16);
            }

            let chained_starts_in_segment_size = self.buf.pos() - chained_starts_in_segment_pos;
            self.buf.get_mut(chained_starts_in_segment).set(
                DyldChainedStartsInSegment {
                    size: chained_starts_in_segment_size as u32,
                    page_size: 0x4000,
                    pointer_format: DYLD_CHAINED_PTR_64_OFFSET,
                    segment_offset: data_segment_begin as u64,
                }
            );
        }
        
        let imports_offset = self.buf.pos();
        let mut chained_imports = Vec::new();
        for _ in &exe.imported_symbols {
            chained_imports.push(self.buf.alloc::<DyldChainedImport>());
        }
        let imported_symbols_offset = self.buf.pos();
        self.buf.push(0 as u8);
        for (&import_header, import) in chained_imports.iter().zip(&exe.imported_symbols) {
            let offset = self.buf.pos() - imported_symbols_offset;
            self.buf.push_null_terminated_string(&import.name);
            self.buf.get_mut(import_header).set(
                DyldChainedImport::new((import.dylib.index() + 1).try_into().unwrap(), false, offset as u32)
            );
        }
        self.buf.pad_to_next_boundary(8);
        let imported_symbols_offset = if exe.got_entries.is_empty() {
            imports_offset
        } else {
            imported_symbols_offset
        };

        self.buf.pad_to_next_boundary(8);

        self.buf.get_mut(chained_fixups_header).set(
            DyldChainedFixupsHeader {
                fixups_version: 0,
                starts_offset: (chained_starts_offset - chained_fixups_header.start()) as u32,
                imports_offset: (imports_offset - chained_fixups_header.start()) as u32,
                symbols_offset: (imported_symbols_offset - chained_fixups_header.start()) as u32,
                imports_count: exe.imported_symbols.len() as u32,
                imports_format: DYLD_CHAINED_IMPORT,
                symbols_format: 0, // uncompressed
            }
        );

        let chained_fixups_data_size = self.buf.pos() - chained_fixups_header.start();
        
        let exports_trie_start = self.buf.pos();
        self.buf.pad_with_zeroes(8);
        let exports_trie_len = self.buf.pos() - exports_trie_start;
        
        let function_starts_start = self.buf.pos();
        // offset to first function, relative to the beginning of the __TEXT segment (aka, beginning of file).
        // subsequent functions would be specified relative to the previous one in the list.
        let text_section_offset = self.get_section_offset(text_section);
        self.buf.push_uleb128(text_section_offset as u32);
        self.buf.pad_to_next_boundary(8);
        let function_starts_len = self.buf.pos() - function_starts_start;
        
        let data_in_code_start = self.buf.pos();
        let data_in_code_len = self.buf.pos() - data_in_code_start;

        let symbol_table_begin = self.buf.pos();
        let mut symbol_headers = Vec::new();
        for _ in local_symbols.iter().chain(&imported_symbols) {
            symbol_headers.push(self.alloc_symbol_table_entry());
        }

        let indirect_symbol_table_offset = if exe.imported_symbols.is_empty() {
            0
        } else {
            self.buf.pos() as u32
        };
        for i in 0..imported_symbols.len() {
            self.buf.push(i as u32 + local_symbols.len() as u32);
        }
        
        let string_table_begin = self.buf.pos();
        self.buf.push_null_terminated_string(" ");
        let mut symbol_string_offsets = Vec::new();
        for symbol in local_symbols.iter().chain(&imported_symbols) {
            symbol_string_offsets.push(self.buf.push_null_terminated_string(&symbol.symbol));
        }
        self.buf.pad_to_next_boundary(8);
        let string_table_len = self.buf.pos() - string_table_begin;
        
        self.buf.pad_to_next_boundary(16);
        
        let code_signature_start = self.buf.pos();
        let super_blob = self.buf.alloc_be::<SuperBlobHeader>();
        
        let mut blob_indices = Vec::new();
        
        let code_directory_index = self.buf.alloc_be::<BlobIndex>();
        
        let code_directory_offset = self.buf.pos() - code_signature_start;
        self.buf.get_mut(code_directory_index).set(
            BlobIndex {
                ty: 0,
                offset: code_directory_offset as u32,
            }
        );
        blob_indices.push(code_directory_index);
        
        let code_directory = self.buf.alloc_be::<CodeDirectory>();
        
        let ident_offset = self.buf.pos() - code_directory.start();
        self.buf.push_null_terminated_string("a.out");
        
        // TODO: alignment?
        let hash_offset = self.buf.pos() - code_directory.start();
        
        let num_code_slots = nearest_multiple_of!(code_signature_start, 4096) / 4096;
        let code_slots_len = num_code_slots * 32;
        let code_signature_len = self.buf.pos() + code_slots_len - code_signature_start;
        
        for ((symbol, symbol_header), string_offset) in local_symbols.iter().chain(&imported_symbols).zip(symbol_headers).zip(symbol_string_offsets) {
            let ty = if symbol.internal_address.is_some() {
                SymbolFlags::new(false, SymbolType::DefinedInSectionNumber, true, 0)
            } else {
                SymbolFlags::new(true, SymbolType::Undefined, false, 0)
            };
            let section_number = if symbol.internal_address.is_some() { 
                1 // ordinal of __text section
            } else {
                0
            };
            self.buf.get_mut(symbol_header).set(
                SymbolTableEntry {
                    string_table_offset: (string_offset - string_table_begin) as u32,
                    ty,
                    section_number,
                    description: symbol.description,
                    value: symbol.internal_address.unwrap_or(0),
                }
            )
        }

        let cstring_section_offset = cstring_section.map(|section| self.get_section_offset(section));
        let final_code = code.perform_fixups(self.get_section_offset(text_section), |fixup| {
            match exe.fixup_locations[fixup] {
                MachOFixupLocation::GotEntry(got_entry_id) => (got_begin + got_entry_id.index() * 8, Indirection::Direct),
                MachOFixupLocation::ObjcSelectorId(id) => (objc_selrefs_begin + id.index() * 8, Indirection::Direct),
                MachOFixupLocation::ConstantNSStringId(id) => (cfstrings_begin + id.index() * 32, Indirection::Indirect),
                MachOFixupLocation::CStringSectionOffset(offset) => (cstring_section_offset.unwrap() + offset, Indirection::Indirect),
            }
        });

        self.fill_text_section(&text_segment, text_section, final_code);


        let link_edit_end = self.buf.pos() + code_slots_len;

        let link_edit_size = link_edit_end - link_edit_begin;
        self.add_info_to_segment(link_edit_segment, link_edit_begin, SegmentSize::Different { vm_size: nearest_multiple_of!(link_edit_size, PAGE_SIZE), file_size: link_edit_size });

        let num_commands = self.num_load_commands;
        self.buf.get_mut(mach_header).set(
            MachHeader {
                magic: MH_MAGIC_64,
                cpu_type: CPU_TYPE_ARM64,
                cpu_subtype: CPU_SUBTYPE_ARM64_ALL,
                file_type: MH_EXECUTE,

                num_commands,
                size_of_commands: (lc_end - lc_begin) as u32,

                flags: MH_NOUNDEFS | MH_DYLDLINK | MH_TWOLEVEL | MH_PIE,
                reserved: 0,
            }
        );

        self.fill_segment_headers();

        self.buf.get_mut(chained_fixups).set(
            LinkEditDataCommand {
                command: LC_DYLD_CHAINED_FIXUPS,
                command_size: chained_fixups.size() as u32,
                data_offset: chained_fixups_header.start() as u32,
                data_size: chained_fixups_data_size as u32,
            }
        );
        self.buf.get_mut(exports_trie).set(
            LinkEditDataCommand {
                command: LC_DYLD_EXPORTS_TRIE,
                command_size: exports_trie.size() as u32,
                data_offset: exports_trie_start as u32,
                data_size: exports_trie_len as u32,
            }
        );
        self.buf.get_mut(function_starts).set(
            LinkEditDataCommand {
                command: LC_FUNCTION_STARTS,
                command_size: function_starts.size() as u32,
                data_offset: function_starts_start as u32,
                data_size: function_starts_len as u32,
            }
        );
        self.buf.get_mut(data_in_code).set(
            LinkEditDataCommand {
                command: LC_DATA_IN_CODE,
                command_size: data_in_code.size() as u32,
                data_offset: data_in_code_start as u32,
                data_size: data_in_code_len as u32,
            }
        );
        self.buf.get_mut(code_signature).set(
            LinkEditDataCommand {
                command: LC_CODE_SIGNATURE,
                command_size: code_signature.size() as u32,
                data_offset: code_signature_start as u32,
                data_size: code_signature_len as u32,
            }
        );
        self.buf.get_mut(build_version).set(
            BuildVersionCommand {
                command: LC_BUILD_VERSION,
                command_size: build_version_len as u32,
                platform: PlatformEnum::MacOs as u32,
                min_os: 13 << 16, // 13.0
                sdk: (13 << 16) | (1 << 8), // 13.1
                num_tools: 1,
            }
        );
        self.buf.get_mut(ld_tool).set(
            BuildToolVersion {
                tool: ToolEnum::Ld as u32,
                version: (820 << 16) | (1 << 8),
            }
        );
        self.buf.get_mut(src_version).set(
            SourceVersionCommand {
                command: LC_SOURCE_VERSION,
                command_size: src_version.size() as u32,
                version: 0,
            }
        );
        for (dylib, command) in exe.dylibs.iter().zip(&dylib_load_commands) {
            self.buf.get_mut(command.header).set(
                DylibCommand {
                    command: LC_LOAD_DYLIB,
                    command_size: command.size(),
                    dylib: Dylib {
                        name_offset: 0x18, // size of this DylibCommand structure
                        timestamp: dylib.timestamp,
                        current_version: dylib.current_version,
                        compatibility_version: dylib.compatibility_version,
                    },
                }
            );
        }
        
        let num_symbols = self.num_symbol_table_entries;
        self.buf.get_mut(symbol_table).set(
            SymbolTableCommand {
                command: LC_SYMTAB,
                command_size: symbol_table.size() as u32,
                symbol_table_offset: symbol_table_begin as u32,
                num_symbols,
                string_table_offset: string_table_begin as u32,
                string_table_size: string_table_len as u32,
            }
        );
        self.buf.get_mut(dynamic_symbol_table).set(
            DynamicSymbolTableCommand {
                command: LC_DYSYMTAB,
                command_size: dynamic_symbol_table.size() as u32,

                local_symbols_index: 0,
                num_local_symbols: local_symbols.len() as u32,

                extern_symbols_index: local_symbols.len() as u32,
                num_extern_symbols: imported_symbols.len() as u32,

                undef_symbols_index: local_symbols.len() as u32,
                num_undef_symbols: imported_symbols.len() as u32,

                toc_offset: 0,
                num_toc_entries: 0,

                module_table_offset: 0,
                num_module_table_entries: 0,

                referenced_symbol_table_offset: 0,
                num_referenced_symbol_table_entries: 0,

                indirect_symbol_table_offset,
                num_indirect_symbol_table_entries: imported_symbols.len() as u32,

                extern_relocation_entries_offset: 0,
                num_extern_relocation_entries: 0,

                local_relocation_entries_offset: 0,
                num_local_relocation_entries: 0,
            }
        );
        self.buf.get_mut(load_dylinker).set(
            DylinkerCommand {
                command: LC_LOAD_DYLINKER,
                command_size: load_dylinker_size as u32,
                name_offset: load_dylinker.size() as u32,
            }
        );
        let entry_point_file_offset = self.get_section_offset(text_section);
        self.buf.get_mut(entry_point).set(
            EntryPointCommand {
                command: LC_MAIN,
                command_size: entry_point.size() as u32,
                entry_point_file_offset: entry_point_file_offset as u64,
                stack_size: 0,
            }
        );
        // self.buf.get_mut(uuid).set(
        //     UuidCommand {
        //         command: LC_UUID,
        //         command_size: uuid.size() as u32,
        //         uuid: [0; 16], // to be filled in later.
        //     }
        // );

        // let mut hasher = Md5::new();
        // hasher.update(&self.data[..code_signature_start]);
        // self.buf.get_mut(uuid).map(|uuid| &mut uuid.uuid).set(hasher.finalize().into());

        let mut i = 0;
        let mut hash_buf = [0; 32];
        let mut num_code_slots = 0;
        while i < code_signature_start {
            let mut sha256 = Sha256::new();
            if code_signature_start - i < 4096 {
                sha256.update(&self.buf.data[i..code_signature_start]);
            } else {
                sha256.update(&self.buf.data[i..(i + 4096)]);
            }
            hash_buf = sha256.finalize().into();
            self.buf.push(hash_buf);
            num_code_slots += 1;
            i += 4096;
        }

        let code_directory_len = self.buf.pos() - code_directory.start();
        let real_code_signature_len = self.buf.pos() - code_signature_start;
        assert_eq!(real_code_signature_len, code_signature_len);

        self.buf.get_mut(code_directory).set(
            CodeDirectory {
                magic: CSMAGIC_CODEDIRECTORY,
                length: code_directory_len as u32,
                version: (2 << 16) | (4 << 8),
                flags: 0x0002_0002,
                hash_offset: hash_offset as u32,
                ident_offset: ident_offset as u32,
                num_special_slots: 0,
                num_code_slots,
                code_limit: code_signature_start as u32,
                hash_size: 32, // copied from a sample file compiled with clang
                hash_type: CD_HASH_TYPE_SHA256,
                platform: 0,
                page_size: 12, // 2^12 = 4096
                spare_2: 0,
                scatter_offset: 0,
                team_offset: 0,
                spare_3: 0,
                code_limit_64: 0,
                exec_seg_base: 0,
                exec_seg_limit: 0x4000,
                exec_seg_flags: 1,
            }
        );

        self.buf.get_mut(super_blob).set(
            SuperBlobHeader {
                magic: CSMAGIC_EMBEDDED_SIGNATURE,
                length: code_signature_len as u32,
                count: blob_indices.len() as u32,
            }
        );

        dest.write_all(&self.buf.data)?;

        Ok(())
    }
}

impl MachOLinker {
    fn alloc_segment(&mut self, name: &'static str, vm_protection: u32, flags: u32) -> SegmentId {
        let header = self.alloc_cmd::<LcSegment64>();
        self.segments.push(
            SegmentBuilder {
                sections: Default::default(),
                header,

                name,
                vm_protection,
                flags,
                info: None,
            }
        )
    }

    fn add_info_to_segment(&mut self, segment: SegmentId, offset: impl Into<SegmentOffset>, size: impl Into<SegmentSize>) {
        self.segments[segment].info = Some(
            SegmentInfo {
                offset: offset.into(),
                size: size.into()
            }
        )
    }

    fn alloc_section(&mut self, segment_id: SegmentId, name: &'static str, alignment: usize, flags: SectionFlags) -> SectionId {
        let header = self.buf.alloc::<Section64>();
        let segment = &mut self.segments[segment_id];
        let index = segment.sections.len();
        segment.sections.push(
            SectionBuilder {
                header,
                name,
                alignment,
                flags,
                info: None,
            }
        );
        (segment_id, index)
    }

    fn add_info_to_section(&mut self, (segment, section): SectionId, offset: usize, size: usize) {
        self.segments[segment].sections[section].info = Some(
            SectionInfo {
                offset,
                size,
            }
        )
    }

    fn fill_segment_headers(&mut self) {
        let segments = std::mem::take(&mut self.segments);
        for segment in &segments {
            let info = segment.info.as_ref().expect(&format!("no info found for segment '{}'", segment.name));
            let vm_addr = match info.offset {
                SegmentOffset::PageZero => 0,
                SegmentOffset::FileOffset(offset) => TEXT_ADDR + offset as u64,
            };
            let file_offset = match info.offset {
                SegmentOffset::PageZero => 0,
                SegmentOffset::FileOffset(offset) => offset as u64,
            };
            let (vm_size, file_size) = match info.size {
                SegmentSize::Same(size) => (size as u64, size as u64),
                SegmentSize::Different { vm_size, file_size } => (vm_size as u64, file_size as u64),
            };
            let command_size = (segment.sections.last()
                .map(|sect| sect.header.end())
                .unwrap_or(segment.header.end()) - segment.header.start()) as u32;
            self.buf.get_mut(segment.header).set(
                LcSegment64 {
                    command: LC_SEGMENT_64,
                    command_size,
                    name: encode_string_16(segment.name),
                    vm_addr,
                    vm_size,
                    file_offset,
                    file_size,
                    max_vm_protection: segment.vm_protection,
                    initial_vm_protection: segment.vm_protection,
                    num_sections: segment.sections.len() as u32,
                    flags: segment.flags,
                }
            );

            for section in &segment.sections {
                let info = section.info.as_ref().unwrap();
                self.buf.get_mut(section.header).set(
                    Section64 {
                        name: encode_string_16(section.name),
                        segment_name: encode_string_16(segment.name),
                        vm_addr: TEXT_ADDR + info.offset as u64,
                        vm_size: info.size as u64,
                        file_offset: info.offset as u32,
                        alignment: log_base_2(section.alignment) as u32,
                        relocations_file_offset: 0,
                        num_relocations: 0,
                        flags: section.flags,
                        reserved: [0; 3],
                    }
                );
            }
        }
        self.segments = segments;
    }

    fn alloc_text_segment(&mut self) -> TextSegmentBuilder {
        let segment = self.alloc_segment("__TEXT", VM_PROT_READ | VM_PROT_EXECUTE, 0);
        TextSegmentBuilder { segment, sections: Vec::new() }
    }

    fn reserve_text_section(&mut self, segment: &mut TextSegmentBuilder, name: &'static str, size: usize, alignment: usize, flags: SectionFlags) -> SectionId {
        let id = self.alloc_section(segment.segment, name, alignment, flags);
        segment.sections.push(
            TextSection { size, id }
        );
        id
    }

    fn layout_text_sections(&mut self, segment: &TextSegmentBuilder, lc_end: usize) {
        let mut len = 0usize;
        let mut offsets_from_end = Vec::with_capacity(segment.sections.len());
        let sections = &self.segments[segment.segment].sections;
        debug_assert_eq!(segment.sections.len(), sections.len());
        for (text_section, section) in segment.sections.iter().zip(sections).rev() {
            let padding = nearest_multiple_of_rt!(len, section.alignment) as usize - len;
            len += text_section.size + padding;
            offsets_from_end.push(len);
        }

        let text_end_addr = nearest_multiple_of!(TEXT_ADDR + (lc_end + len) as u64, PAGE_SIZE);
        let text_segment_size = text_end_addr - TEXT_ADDR as usize;

        for (i, (section, &offset_from_end)) in segment.sections.iter().zip(offsets_from_end.iter().rev()).enumerate() {
            self.add_info_to_section((segment.segment, i), text_segment_size as usize - offset_from_end, section.size);
        }

        self.add_info_to_segment(segment.segment, 0, text_segment_size);

        self.buf.pad_with_zeroes(text_segment_size - self.buf.pos());
    }

    fn fill_text_section(&mut self, segment: &TextSegmentBuilder, section: SectionId, data: &[u8]) {
        assert_eq!(segment.sections[section.1].id, section);
        assert_eq!(segment.segment, section.0);
        let size = segment.sections[section.1].size;
        let offset = self.segments[segment.segment].sections[section.1].info.unwrap().offset;
        self.buf.data[offset..(offset + size)].copy_from_slice(data);
    }

    fn get_section_offset(&self, section: SectionId) -> usize {
        let segment = &self.segments[section.0];
        segment.sections[section.1].info.unwrap().offset
    }

    fn alloc_dylib_command(&mut self, name: &str) -> DylibCommandBuilder {
        let header = self.alloc_cmd::<DylibCommand>();
        let name_begin = self.buf.pos();
        self.buf.push_null_terminated_string(name);

        // According to https://opensource.apple.com/source/xnu/xnu-7195.81.3/EXTERNAL_HEADERS/mach-o/loader.h.auto.html
        // we're supposed to pad to the next 4 byte boundary, but the sample files I've examined seem to pad to 8 bytes
        // (which honestly makes more sense to me anyway, given the presence of 64 bit values in some of the load
        // commands)
        self.buf.pad_to_next_boundary(8);
        let end = self.buf.pos();

        DylibCommandBuilder {
            header,
            additional_size: end - name_begin,
        }
    }

    fn alloc_symbol_table_entry(&mut self) -> Ref<SymbolTableEntry> {
        self.num_symbol_table_entries += 1;
        self.buf.alloc()
    }

    fn alloc_cmd<T: ByteSwap>(&mut self) -> Ref<T> {
        self.num_load_commands += 1;
        self.buf.alloc()
    }

    fn push_chained_fixup<T>(&mut self, prev_ptr: &mut Option<Ref<DyldChainedPtr64>>, fixup: T)
    where
        T: ByteSwap,
        Ref<T>: Into<Ref<DyldChainedPtr64>>
    {
        // TODO: handle page boundaries, also perhaps automatically generate first fixup per-page here
        if let Some(prev_ptr) = prev_ptr {
            let diff = self.buf.pos() - prev_ptr.addr;
            assert_eq!(diff % 4, 0);
            self.buf.get_mut(*prev_ptr).set_next((diff / 4) as u16);
        }
        *prev_ptr = Some(self.buf.push(fixup).into());
    }
}

define_index_type!(struct GotEntryId = u32;);
define_index_type!(struct ConstantNSStringId = u32;);
define_index_type!(struct ObjcSelectorId = u32;);

struct MachODylib {
    name: String,
    timestamp: u32,
    current_version: u32,
    compatibility_version: u32,
}

#[derive(Clone, Hash, PartialEq, Eq)]
struct MachOImportedSymbol {
    dylib: DynLibId,
    name: String,
}

struct ConstantNSString {
    location: ConstantNSStringLocation,
    size: usize,
}

#[derive(Hash, PartialEq, Eq)]
enum ConstantNSStringLocation {
    CStringSectionOffset(usize),
    // TODO: UTF-16 strings
    //UStringSectionOffset(usize),
}

enum MachOFixupLocation {
    GotEntry(GotEntryId),
    ObjcSelectorId(ObjcSelectorId),
    CStringSectionOffset(usize),
    ConstantNSStringId(ConstantNSStringId),
}

#[derive(Clone, Copy)]
struct ObjcSelector {
    // byte offset into __objc_methname string table
    offset: usize,
}

#[derive(Default)]
struct MachOExe {
    dylibs: IndexVec<DynLibId, MachODylib>,
    dylib_map: HashMap<PathBuf, DynLibId>,

    imported_symbols: IndexVec<ImportedSymbolId, MachOImportedSymbol>,
    imported_symbol_map: HashMap<MachOImportedSymbol, ImportedSymbolId>,

    fixup_locations: IndexVec<FixupLocationId, MachOFixupLocation>,

    cstrings: Vec<u8>,
    cstring_map: HashMap<CString, usize>,

    objc_method_names: Vec<u8>,
    objc_method_names_map: HashMap<CString, usize>,

    objc_selectors: IndexVec<ObjcSelectorId, ObjcSelector>,
    objc_selector_map: HashMap<usize, ObjcSelectorId>,

    constant_nsstrings: IndexVec<ConstantNSStringId, ConstantNSString>,
    constant_nsstring_map: HashMap<ConstantNSStringLocation, ConstantNSStringId>,
    cf_constant_string_class_reference_import: Option<ImportedSymbolId>,
    
    got_entries: IndexVec<GotEntryId, ImportedSymbolId>,
    got_map: HashMap<ImportedSymbolId, GotEntryId>,

    code_blob: Option<Box<dyn CodeBlob>>,
}

// TODO: don't hardcode this
const SDK_ROOT: &'static str = "/Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/SDKs/MacOSX.sdk";

impl MachOExe {
    fn new() -> Self {
        Default::default()
    }

    #[doc(hidden)]
    fn intern_cstring(&mut self, string: &CStr) -> usize {
        *self.cstring_map.entry(string.to_owned()).or_insert_with(|| {
            let offset = self.cstrings.len();
            self.cstrings.extend(string.to_bytes_with_nul());
            offset
        })
    }

    #[doc(hidden)]
    fn intern_objc_method_name(&mut self, name: &CStr) -> usize {
        *self.objc_method_names_map.entry(name.to_owned()).or_insert_with(|| {
            if self.objc_method_names.len() % 2 != 0 {
                self.objc_method_names.push(0); // Align to an even boundary. Not sure if necessary, but this what Clang does.
            }
            let offset = self.objc_method_names.len();
            self.objc_method_names.extend(name.to_bytes_with_nul());
            offset
        })
    }

    fn add_got_entry(&mut self, symbol: ImportedSymbolId) -> GotEntryId {
        *self.got_map.entry(symbol).or_insert_with(|| {
            self.got_entries.push(symbol)
        })
    }

    fn import_dynamic_library_impl(&mut self, offset: String) -> DynLibId {
        let mut tbd_path = PathBuf::from(SDK_ROOT);
        tbd_path.push(offset);
        tbd_path.set_extension("tbd");

        *self.dylib_map.entry(tbd_path.clone()).or_insert_with(|| {
            // TODO: error handling
            let tbd = parse_tbd(&tbd_path).unwrap();
            self.dylibs.push(
                MachODylib {
                    name: tbd.name,
                    timestamp: 2,
                    current_version: tbd.current_version,
                    compatibility_version: tbd.compatibility_version,
                }
            )
        })
    }
}

impl Exe for MachOExe {
    fn import_dynamic_library(&mut self, name: &str) -> DynLibId {
        self.import_dynamic_library_impl(format!("usr/lib/{}", name))
    }

    fn import_symbol(&mut self, dylib: DynLibId, name: String) -> ImportedSymbolId {
        let symbol = MachOImportedSymbol { dylib, name };
        *self.imported_symbol_map.entry(symbol.clone()).or_insert_with(|| {
            self.imported_symbols.push(symbol.clone())
        })
    }

    fn use_imported_symbol(&mut self, symbol: ImportedSymbolId) -> FixupLocationId {
        let got_entry = self.add_got_entry(symbol);
        self.fixup_locations.push(MachOFixupLocation::GotEntry(got_entry))
    }

    fn use_cstring(&mut self, string: &CStr) -> FixupLocationId {
        let offset = self.intern_cstring(string);
        self.fixup_locations.push(MachOFixupLocation::CStringSectionOffset(offset))
    }

    fn add_code_blob(&mut self, blob: Box<dyn CodeBlob>) {
        assert!(self.code_blob.is_none());
        self.code_blob = Some(blob);
    }

    fn as_objc_exe(&mut self) -> Option<&mut dyn ObjCExe> {
        Some(self)
    }
}

impl ObjCExe for MachOExe {
    fn import_framework(&mut self, name: &str) -> DynLibId {
        self.import_dynamic_library_impl(format!("System/Library/Frameworks/{}.framework/{}", name, name))
    }

    fn use_constant_nsstring(&mut self, string: &CStr) -> FixupLocationId {
        if self.cf_constant_string_class_reference_import.is_none() {
            let foundation = self.import_framework("Foundation");
            let sym = self.import_symbol(foundation, "___CFConstantStringClassReference".to_string());
            self.cf_constant_string_class_reference_import = Some(sym);
        }
        let offset = self.intern_cstring(string);
        let location = ConstantNSStringLocation::CStringSectionOffset(offset);
        let id = *self.constant_nsstring_map.entry(location).or_insert_with(|| {
            let str = ConstantNSString {
                location: ConstantNSStringLocation::CStringSectionOffset(offset),
                size: string.to_bytes().len(),
            };
            self.constant_nsstrings.push(str)
        });
        self.fixup_locations.push(MachOFixupLocation::ConstantNSStringId(id))
    }

    fn use_objc_selector(&mut self, name: &CStr) -> FixupLocationId {
        let offset = self.intern_objc_method_name(name);
        let id = *self.objc_selector_map.entry(offset).or_insert_with(|| {
            let selector = ObjcSelector {
                offset,
            };
            self.objc_selectors.push(selector)
        });
        self.fixup_locations.push(MachOFixupLocation::ObjcSelectorId(id))
    }
}
