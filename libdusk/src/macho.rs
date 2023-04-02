#![allow(unused)]

use std::collections::HashMap;
use std::hash::Hash;
use std::io::{self, Write};
use std::marker::PhantomData;
use std::mem;

use index_vec::define_index_type;
use crypto::digest::Digest;
use crypto::sha2::Sha256;
use crate::index_vec::*;

use crate::driver::Driver;
use dusk_proc_macros::ByteSwap;

trait ByteSwap {
    fn byte_swap(&mut self);
}

macro_rules! byte_swap_impl {
    (@noop: $ty:ty) => {
        impl ByteSwap for $ty {
            fn byte_swap(&mut self) {}
        }
    };
    (@num: $ty:ty) => {
        impl ByteSwap for $ty {
            fn byte_swap(&mut self) {
                *self = <$ty>::from_be_bytes(self.to_le_bytes());
            }
        }
    };
    (noops: $($noop_ty:ty),*;
     nums: $($num_ty:ty),* $(;)?) => {
        $(byte_swap_impl!(@noop: $noop_ty);)*
        $(byte_swap_impl!(@num: $num_ty);)*
    };
}

impl<T: ByteSwap, const N: usize> ByteSwap for [T; N] {
    fn byte_swap(&mut self) {
        for value in self {
            value.byte_swap();
        }
    }
}

byte_swap_impl!(noops: u8, i8; nums: u16, u32, u64, usize, i16, i32, i64, isize);

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

struct Ref<T: ByteSwap, const BIG_ENDIAN: bool = false> {
    addr: usize,
    _phantom: PhantomData<T>,
}

struct ResolvedRefMut<'a, T: ByteSwap, const BIG_ENDIAN: bool = false> {
    value: &'a mut T,
}

impl<'a, T: ByteSwap, const BIG_ENDIAN: bool> ResolvedRefMut<'a, T, BIG_ENDIAN> {
    fn set(&mut self, new_value: T) {
        *self.value = new_value;
        if BIG_ENDIAN != cfg!(target_endian = "big") {
            self.value.byte_swap();
        }
    }

    fn map<U: ByteSwap, M: FnOnce(&'a mut T) -> &mut U>(&'a mut self, mapper: M) -> ResolvedRefMut<'a, U, BIG_ENDIAN> {
        ResolvedRefMut { value: mapper(self.value) }
    }
}

impl<T: ByteSwap, const BIG_ENDIAN: bool> Clone for Ref<T, BIG_ENDIAN> {
    fn clone(&self) -> Self {
        Self {
            addr: self.addr,
            _phantom: PhantomData,
        }
    }
}
impl<T: ByteSwap, const BIG_ENDIAN: bool> Copy for Ref<T, BIG_ENDIAN> {}

impl<T: ByteSwap, const BIG_ENDIAN: bool> Ref<T, BIG_ENDIAN> {
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

define_index_type!(struct TextSectionId = u32;);

struct SegmentBuilder {
    header: Ref<LcSegment64>,
    sections: IndexVec<TextSectionId, Ref<Section64>>,
}

impl SegmentBuilder {
    fn size(&self) -> u32 {
        (self.sections.last()
            .map(|sect| sect.end())
            .unwrap_or(self.header.end()) - self.header.start()) as u32
    }
}

struct TextSectionAllocationInfo {
    packed_offset: usize,
    size: usize,
    alignment: usize,
}

struct TextSegmentBuilder {
    segment_builder: SegmentBuilder,
    section_data: Vec<u8>,
    sections: IndexVec<TextSectionId, TextSectionAllocationInfo>,
}

impl TextSegmentBuilder {
    fn header_size(&self) -> u32 {
        self.segment_builder.size()
    }
}

struct TextSection {
    header: Ref<Section64>,
    id: TextSectionId,
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
        self.value.0 = self.value.0 & !mask;
        self.value.0 |= (next as u64) << 51;
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
        Ref {
            addr: value.addr,
            _phantom: PhantomData,
        }
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

const fn is_power_of_2(num: usize) -> bool {
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
        (((($val) as usize).wrapping_sub(1)) | ((($factor) as usize).wrapping_sub(1))).wrapping_add(1)
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

const PAGE_SIZE: usize = 0x4000;
const TEXT_ADDR: u64 = 0x0000_0001_0000_0000;

impl MachOEncoder {
    pub fn new() -> Self { Self::default() }

    fn alloc<T: ByteSwap>(&mut self) -> Ref<T> {
        let reff = Ref::new(self.data.len());
        self.pad_with_zeroes(mem::size_of::<T>());
        reff
    }

    fn alloc_be<T: ByteSwap>(&mut self) -> Ref<T, true> {
        let reff = Ref::new(self.data.len());
        self.pad_with_zeroes(mem::size_of::<T>());
        reff
    }

    fn alloc_cmd<T: ByteSwap>(&mut self) -> Ref<T> {
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

    fn alloc_text_segment(&mut self) -> TextSegmentBuilder {
        let segment_builder = self.alloc_segment();
        TextSegmentBuilder { segment_builder, section_data: Vec::new(), sections: IndexVec::new() }
    }

    fn alloc_text_section(&mut self, segment: &mut TextSegmentBuilder, data: &[u8], alignment: usize) -> TextSection {
        let header = self.alloc_section(&mut segment.segment_builder);
        let offset = segment.section_data.len();
        let alloc_info = TextSectionAllocationInfo {
            packed_offset: offset,
            size: data.len(),
            alignment,
        };
        let id = segment.sections.push(alloc_info);
        segment.section_data.extend(data);
        TextSection { header, id }
    }

    fn push_text_sections(&mut self, segment: &TextSegmentBuilder, lc_end: usize) -> (IndexVec<TextSectionId, usize>, usize) {
        let mut len = 0usize;
        let mut offsets_from_end = Vec::with_capacity(segment.sections.len());
        for alloc_info in segment.sections.iter().rev() {
            let padding = nearest_multiple_of_rt!(len, alloc_info.alignment) as usize - len;
            len += alloc_info.size + padding;
            offsets_from_end.push((len, padding));
        }

        let text_end_addr = nearest_multiple_of!(TEXT_ADDR + (lc_end + len) as u64, PAGE_SIZE);
        let text_sections_addr = text_end_addr - len;

        debug_assert_eq!(segment.sections.len(), segment.segment_builder.sections.len());
        
        let text_segment_size = text_end_addr - TEXT_ADDR as usize;
        
        let padding_size = text_sections_addr - TEXT_ADDR as usize - lc_end;
        self.pad_with_zeroes(padding_size);

        let mut offsets = IndexVec::with_capacity(segment.sections.len());
        for ((id, alloc_info), &(offset_from_end, padding)) in segment.sections.iter_enumerated().zip(offsets_from_end.iter().rev()) {
            let offset = text_segment_size as usize - offset_from_end;
            offsets.push_at(id, text_segment_size as usize - offset_from_end);

            self.data.extend(&segment.section_data[alloc_info.packed_offset..(alloc_info.packed_offset+alloc_info.size)]);
            self.pad_with_zeroes(padding);
        }
        debug_assert_eq!(text_segment_size, self.pos());

        (offsets, text_segment_size)
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

    fn get_mut<'a, T: ByteSwap, const BIG_ENDIAN: bool>(&'a mut self, addr: Ref<T, BIG_ENDIAN>) -> ResolvedRefMut<'a, T, BIG_ENDIAN> {
        debug_assert!(addr.addr + mem::size_of::<T>() <= self.data.len());

        // TODO: don't produce a &mut T at all. I didn't think we would need to support unaligned access, but we do.
        let region = &mut self.data[addr.addr..];
        let ptr = region.as_mut_ptr() as *mut T;

        ResolvedRefMut { value: unsafe { &mut *ptr } }
    }

    fn push<T: ByteSwap>(&mut self, value: T) -> Ref<T> {
        let addr = self.alloc();
        self.get_mut(addr).set(value);
        addr
    }

    fn push_be<T: ByteSwap>(&mut self, value: T) -> Ref<T, true> {
        let addr = self.alloc_be();
        self.get_mut(addr).set(value);
        addr
    }

    fn pos(&self) -> usize { self.data.len() }

    fn pad_with_zeroes(&mut self, size: usize) {
        self.data.extend(std::iter::repeat(0).take(size as usize));
    }

    fn pad_to_next_boundary<const B: usize>(&mut self) {
        let padded_pos = nearest_multiple_of_rt!(self.data.len() as u64, B);
        self.pad_with_zeroes(padded_pos as usize - self.pos());
    }

    fn pad_to_next_boundary_rt(&mut self, alignment: usize) {
        let padded_pos = nearest_multiple_of_rt!(self.data.len(), alignment);
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
    
    pub fn write(&mut self, d: &Driver, main_function_index: usize, dest: &mut impl Write) -> io::Result<()> {
        let mut exe = MachOExe::new();
        let lib_system = exe.load_dylib("/usr/lib/libSystem.B.dylib", 2, 0x05_27_0000, 0x00_01_0000);


        let (code, string_literal_fixups, imports, import_fixups) = d.generate_arm64_func(main_function_index, true);
        let mut cstrings: Vec<u8> = Vec::new();
        let mut string_literal_offsets = Vec::new();
        for lit in &string_literal_fixups {
            string_literal_offsets.push(cstrings.len());
            cstrings.extend(d.code.mir.strings[lit.id].as_bytes_with_nul());
        }


        let mach_header = self.alloc::<MachHeader>();
        
        let lc_begin = self.pos();
        
        let page_zero = self.alloc_segment();
        
        let mut text_segment = self.alloc_text_segment();

        let text_section = self.alloc_text_section(&mut text_segment, &code, 4);
        let cstring_section = (!string_literal_fixups.is_empty()).then(|| self.alloc_text_section(&mut text_segment, &cstrings, 1));
        // let unwind_info_section = self.alloc_section(&mut text_segment);


        let data_const = (!imports.is_empty()).then(|| {
            let segment_number = self.num_segments;
            let mut segment = self.alloc_segment();
            let got_section = self.alloc_section(&mut segment);
            (segment_number, segment, got_section)
        });
        
        let link_edit_segment = self.alloc_segment();

        let chained_fixups = self.alloc_cmd::<LinkEditDataCommand>();
        let exports_trie = self.alloc_cmd::<LinkEditDataCommand>();
        
        let symbol_table = self.alloc_cmd::<SymbolTableCommand>();
        let dynamic_symbol_table = self.alloc_cmd::<DynamicSymbolTableCommand>();
        
        let load_dylinker = self.alloc_cmd::<DylinkerCommand>();
        self.push_null_terminated_string("/usr/lib/dyld");
        self.pad_to_next_boundary::<8>();
        let load_dylinker_size = self.pos() - load_dylinker.addr;
        
        // let uuid = self.alloc_cmd::<UuidCommand>();
        
        let build_version = self.alloc_cmd::<BuildVersionCommand>();
        let ld_tool = self.alloc::<BuildToolVersion>();
        let build_version_len = self.pos() - build_version.addr;
        
        let src_version = self.alloc_cmd::<SourceVersionCommand>();
        
        let entry_point = self.alloc_cmd::<EntryPointCommand>();

        let dylib_load_commands: Vec<_> = exe.dylibs.iter()
            .map(|dylib| self.alloc_dylib_command(&dylib.name))
            .collect();
        
        let function_starts = self.alloc_cmd::<LinkEditDataCommand>();
        
        let data_in_code = self.alloc_cmd::<LinkEditDataCommand>();
        
        let code_signature = self.alloc_cmd::<LinkEditDataCommand>();
        
        let lc_end = self.pos();

        let (text_section_offsets, text_segment_size) = self.push_text_sections(&text_segment, lc_end);

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
        local_symbols.push(
            Symbol {
                symbol: "_main".to_string(),
                internal_address: Some(TEXT_ADDR + text_section_offsets[text_section.id] as u64),
                description: SymbolDescription::new(0, false, false, false, false, 0),
            }
        );
        for import in &imports {
            imported_symbols.push(
                Symbol {
                    symbol: import.name.clone(),
                    internal_address: None,
                    // we only support importing from libSystem for now, which is currently imported at ordinal 1
                    description: SymbolDescription::new(0, false, false, false, false, 1),
                }
            )
        }

        // TODO: move this to arm64 backend
        if let Some(cstring_section) = &cstring_section {
            for (fixup, &string_offset) in string_literal_fixups.iter().zip(&string_literal_offsets) {
                let code_offset = text_section_offsets[text_section.id] + fixup.offset;
                let string_offset = text_section_offsets[cstring_section.id] + string_offset;
    
                let page_mask = !0x0FFF;
    
                // if the code and string are in the same page, we don't need to provide a non-zero offset to `adrp`.
                // TODO: support case where that is not true.
                assert_eq!(code_offset & page_mask, string_offset & page_mask);
    
                let mut fixed_up_code = Arm64Encoder::new();
                fixed_up_code.adrp(Reg::R0, 0);
                fixed_up_code.add64_imm(false, Reg::R0, Reg::R0, (string_offset & !page_mask).try_into().unwrap());
                self.data[code_offset..(code_offset + 8)].copy_from_slice(&fixed_up_code.get_bytes());
            }
        }

        let data_const = data_const.map(|(segment_number, segment, got_section)| {
            let data_const_begin = self.pos();

            let mut prev_ptr: Option<Ref<DyldChainedPtr64>> = None;
            for (i, import) in imports.iter().enumerate() {
                if let Some(prev_ptr) = prev_ptr {
                    let diff = self.pos() - prev_ptr.addr;
                    assert!(diff % 4 == 0);
                    self.get_mut(prev_ptr).set_next((diff / 4) as u16);
                }
                prev_ptr = Some(self.push(DyldChainedPtr64Bind::new(i as u32, 0, 0)).into());
            }

            let got_size = self.pos() - data_const_begin;
            self.pad_to_next_boundary::<PAGE_SIZE>();
            let data_const_size = self.pos() - data_const_begin;

            (segment_number, segment, got_section, data_const_begin, data_const_size, got_size)
        });
        
        let link_edit_begin = self.pos();
        
        let chained_fixups_header = self.alloc::<DyldChainedFixupsHeader>();
        self.pad_to_next_boundary::<8>();
        // Push dyld_chained_starts_in_image (a dynamically-sized structure)
        let chained_starts_offset = self.pos();
        self.push(self.num_segments as u32);
        let mut data_const_starts_offset = None;
        for i in 0..self.num_segments {
            if data_const.as_ref().map(|(num, ..)| *num) == Some(i) {
                data_const_starts_offset = Some(self.alloc::<u32>());
            } else {
                // AFAICT, the offset is relative to the start of dyld_chained_starts_in_image, which makes any offset less
                // than 4 + 4 * num_segments invalid, thus 0 should indicate "no starts for this page"
                self.push(0 as u32);
            }
        }
        if let Some(data_const_starts_offset) = data_const_starts_offset {
            self.pad_to_next_boundary::<8>();
            let chained_starts_in_segment_pos = self.pos();
            self.get_mut(data_const_starts_offset).set((chained_starts_in_segment_pos -  chained_starts_offset) as u32);
            
            let chained_starts_in_segment = self.alloc::<DyldChainedStartsInSegment>();
            // TODO: these should be fields of DyldChainedStartsInSegment, but need to be here instead because packed
            // structs are not yet supported by our ByteSwap macro.
            self.push(0 as u32); // max_valid_pointer
            let page_count = (imports.len() * 8 - 1) / PAGE_SIZE + 1;
            self.push(u16::try_from(page_count).unwrap());

            // The first fix-up in each page is at offset 0, because the whole point of the __got section is to provide
            // fixed up addresses.
            for _ in 0..page_count {
                self.push(0 as u16);
            }

            let &(_, _, _, data_const_begin, ..) = data_const.as_ref().unwrap();

            let chained_starts_in_segment_size = self.pos() - chained_starts_in_segment_pos;
            self.get_mut(chained_starts_in_segment).set(
                DyldChainedStartsInSegment {
                    size: chained_starts_in_segment_size as u32,
                    page_size: 0x4000,
                    pointer_format: DYLD_CHAINED_PTR_64_OFFSET,
                    segment_offset: data_const_begin as u64,
                }
            );
        }
        
        let imports_offset = self.pos();
        let mut chained_imports = Vec::new();
        for import in &imports {
            chained_imports.push(self.alloc::<DyldChainedImport>());
        }
        let imported_symbols_offset = self.pos();
        self.push(0 as u8);
        for (&import_header, import) in chained_imports.iter().zip(&imports) {
            let offset = self.pos() - imported_symbols_offset;
            self.push_null_terminated_string(&import.name);
            self.get_mut(import_header).set(
                DyldChainedImport::new(1, false, offset as u32)
            );
        }
        self.pad_to_next_boundary::<8>();
        let imported_symbols_offset = if imports.is_empty() {
            imports_offset
        } else {
            imported_symbols_offset
        };

        self.pad_to_next_boundary::<8>();

        self.get_mut(chained_fixups_header).set(
            DyldChainedFixupsHeader {
                fixups_version: 0,
                starts_offset: (chained_starts_offset - chained_fixups_header.start()) as u32,
                imports_offset: (imports_offset - chained_fixups_header.start()) as u32,
                symbols_offset: (imported_symbols_offset - chained_fixups_header.start()) as u32,
                imports_count: imports.len() as u32,
                imports_format: DYLD_CHAINED_IMPORT,
                symbols_format: 0, // uncompressed
            }
        );

        let chained_fixups_data_size = self.pos() - chained_fixups_header.start();
        
        let exports_trie_start = self.pos();
        self.pad_with_zeroes(8);
        let exports_trie_len = self.pos() - exports_trie_start;
        
        let function_starts_start = self.pos();
        // offset to first function, relative to the beginning of the __TEXT segment (aka, beginning of file).
        // subsequent functions would be specified relative to the previous one in the list.
        self.push_uleb128(text_section_offsets[text_section.id] as u32);
        self.pad_to_next_boundary::<8>();
        let function_starts_len = self.pos() - function_starts_start;
        
        let data_in_code_start = self.pos();
        let data_in_code_len = self.pos() - data_in_code_start;

        let symbol_table_begin = self.pos();
        let mut symbol_headers = Vec::new();
        for symbol in local_symbols.iter().chain(&imported_symbols) {
            symbol_headers.push(self.alloc_symbol_table_entry());
        }

        let indirect_symbol_table_offset = if imports.is_empty() {
            0
        } else {
            self.pos() as u32
        };
        for i in 0..imported_symbols.len() {
            self.push(i as u32 + local_symbols.len() as u32);
        }
        
        let string_table_begin = self.pos();
        self.push_null_terminated_string(" ");
        let mut symbol_string_offsets = Vec::new();
        for symbol in local_symbols.iter().chain(&imported_symbols) {
            symbol_string_offsets.push(self.push_null_terminated_string(&symbol.symbol));
        }
        self.pad_to_next_boundary::<8>();
        let string_table_len = self.pos() - string_table_begin;
        
        self.pad_to_next_boundary::<16>();
        
        let code_signature_start = self.pos();
        let super_blob = self.alloc_be::<SuperBlobHeader>();
        
        let mut blob_indices = Vec::new();
        
        let code_directory_index = self.alloc_be::<BlobIndex>();
        
        let code_directory_offset = self.pos() - code_signature_start;
        self.get_mut(code_directory_index).set(
            BlobIndex {
                ty: 0,
                offset: code_directory_offset as u32,
            }
        );
        blob_indices.push(code_directory_index);
        
        let code_directory = self.alloc_be::<CodeDirectory>();
        
        let ident_offset = self.pos() - code_directory.start();
        self.push_null_terminated_string("a.out");
        
        // TODO: alignment?
        let hash_offset = self.pos() - code_directory.start();
        
        let num_code_slots = nearest_multiple_of!(code_signature_start, 4096) / 4096;
        let code_slots_len = num_code_slots * 32;
        let code_signature_len = self.pos() + code_slots_len - code_signature_start;
        
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
            self.get_mut(symbol_header).set(
                SymbolTableEntry {
                    string_table_offset: (string_offset - string_table_begin) as u32,
                    ty,
                    section_number,
                    description: symbol.description,
                    value: symbol.internal_address.unwrap_or(0),
                }
            )
        }


        if let Some((segment_number, segment, got_section, data_const_begin, data_const_size, got_size)) = &data_const {
            for (i, fixup) in import_fixups.iter().enumerate() {
                let symbol_offset = data_const_begin + fixup.id.raw() as usize * 8;
                let code_offset = text_section_offsets[text_section.id] + fixup.offset;
    
                let page_mask = 0x0FFF;
                let page_offset = (symbol_offset >> 12) as i32 - (code_offset >> 12) as i32;
    
                let mut fixed_up_code = Arm64Encoder::new();
                fixed_up_code.adrp(Reg::R16, page_offset);
                fixed_up_code.ldr64(Reg::R16, Reg::R16, (symbol_offset & page_mask).try_into().unwrap());
                self.data[code_offset..(code_offset + 8)].copy_from_slice(&fixed_up_code.get_bytes());
            }
        }


        let link_edit_end = self.pos() + code_slots_len;
        let num_commands = self.num_load_commands;
        self.get_mut(mach_header).set(
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
        self.get_mut(page_zero.header).set(
            LcSegment64 {
                command: LC_SEGMENT_64,
                command_size: page_zero.size(),
                name: encode_string_16("__PAGEZERO"),
                vm_addr: 0,
                vm_size: TEXT_ADDR,
                file_offset: 0,
                file_size: 0,
                max_vm_protection: VM_PROT_NONE,
                initial_vm_protection: VM_PROT_NONE,
                num_sections: page_zero.sections.len() as u32,
                flags: 0,
            }
        );
        self.get_mut(text_segment.segment_builder.header).set(
            LcSegment64 {
                command: LC_SEGMENT_64,
                command_size: text_segment.header_size(),
                name: encode_string_16("__TEXT"),
                vm_addr: TEXT_ADDR,
                vm_size: text_segment_size as u64,
                file_offset: 0,
                file_size: text_segment_size as u64,
                max_vm_protection: VM_PROT_READ | VM_PROT_EXECUTE,
                initial_vm_protection: VM_PROT_READ | VM_PROT_EXECUTE,
                num_sections: text_segment.segment_builder.sections.len() as u32,
                flags: 0,
            }
        );
        self.get_mut(text_section.header).set(
            Section64 {
                name: encode_string_16("__text"),
                segment_name: encode_string_16("__TEXT"),
                vm_addr: TEXT_ADDR + text_section_offsets[text_section.id] as u64,
                vm_size: code.len() as u64,
                file_offset: text_section_offsets[text_section.id] as u32,
                alignment: 2, // stored as log base 2, so this is actually 4
                relocations_file_offset: 0,
                num_relocations: 0,
                flags: SectionFlags::new(SectionType::Regular, S_ATTR_PURE_INSTRUCTIONS | S_ATTR_SOME_INSTRUCTIONS),
                reserved: [0; 3],
            }
        );
        if let Some(cstring_section) = cstring_section {
            self.get_mut(cstring_section.header).set(
                Section64 {
                    name: encode_string_16("__cstring"),
                    segment_name: encode_string_16("__TEXT"),
                    vm_addr: TEXT_ADDR + text_section_offsets[cstring_section.id] as u64,
                    vm_size: cstrings.len() as u64,
                    file_offset: text_section_offsets[cstring_section.id] as u32,
                    alignment: 0, // stored as log base 2, so this is actually 1
                    relocations_file_offset: 0,
                    num_relocations: 0,
                    flags: SectionFlags::new(SectionType::CStringLiterals, 0),
                    reserved: [0; 3],
                }
            );
        }
        // self.get_mut(unwind_info_section).set(
        //     Section64 {
        //         name: encode_string_16("__unwind_info"),
        //         segment_name: encode_string_16("__TEXT"),
        //         vm_addr: TEXT_ADDR + text_section_offsets[unwind_info_section.id] as u64,
        //         vm_size: unwind_info.len() as u64,
        //         file_offset: text_section_offsets[unwind_info_section.id] as u32,
        //         alignment: 2, // stored as log base 2, so this is actually 4
        //         relocations_file_offset: 0,
        //         num_relocations: 0,
        //         flags: SectionFlags::new(SectionType::Regular, 0),
        //         reserved: [0; 3],
        //     }
        // );

        if let Some((segment_number, segment, got_section, data_const_begin, data_const_size, got_size)) = data_const {
            self.get_mut(segment.header).set(
                LcSegment64 {
                    command: LC_SEGMENT_64,
                    command_size: segment.size(),
                    name: encode_string_16("__DATA_CONST"),
                    vm_addr: TEXT_ADDR + data_const_begin as u64,
                    vm_size: data_const_size as u64,
                    file_offset: data_const_begin as u64,
                    file_size: data_const_size as u64,
                    max_vm_protection: VM_PROT_READ | VM_PROT_WRITE,
                    initial_vm_protection: VM_PROT_READ | VM_PROT_WRITE,
                    num_sections: 1,
                    flags: SG_READ_ONLY,
                }
            );
            self.get_mut(got_section).set(
                Section64 {
                    name: encode_string_16("__got"),
                    segment_name: encode_string_16("__DATA_CONST"),
                    vm_addr: TEXT_ADDR + data_const_begin as u64,
                    vm_size: got_size as u64,
                    file_offset: data_const_begin as u32,
                    alignment: 3, // stored as log base 2, so this is actually 8
                    relocations_file_offset: 0,
                    num_relocations: 0,
                    flags: SectionFlags::new(SectionType::NonLazySymbolPointers, 0),
                    reserved: [0; 3], // reserved[0] is the first offset into the indirect symbol table
                }
            );
        };
        self.get_mut(link_edit_segment.header).set(
            LcSegment64 {
                command: LC_SEGMENT_64,
                command_size: link_edit_segment.size(),
                name: encode_string_16("__LINKEDIT"),
                vm_addr: TEXT_ADDR + link_edit_begin as u64,
                vm_size: PAGE_SIZE as u64,
                file_offset: link_edit_begin as u64,
                file_size: (link_edit_end - link_edit_begin) as u64,
                max_vm_protection: VM_PROT_READ,
                initial_vm_protection: VM_PROT_READ,
                num_sections: 0,
                flags: 0,
            }
        );
        self.get_mut(chained_fixups).set(
            LinkEditDataCommand {
                command: LC_DYLD_CHAINED_FIXUPS,
                command_size: chained_fixups.size() as u32,
                data_offset: chained_fixups_header.start() as u32,
                data_size: chained_fixups_data_size as u32,
            }
        );
        self.get_mut(exports_trie).set(
            LinkEditDataCommand {
                command: LC_DYLD_EXPORTS_TRIE,
                command_size: exports_trie.size() as u32,
                data_offset: exports_trie_start as u32,
                data_size: exports_trie_len as u32,
            }
        );
        self.get_mut(function_starts).set(
            LinkEditDataCommand {
                command: LC_FUNCTION_STARTS,
                command_size: function_starts.size() as u32,
                data_offset: function_starts_start as u32,
                data_size: function_starts_len as u32,
            }
        );
        self.get_mut(data_in_code).set(
            LinkEditDataCommand {
                command: LC_DATA_IN_CODE,
                command_size: data_in_code.size() as u32,
                data_offset: data_in_code_start as u32,
                data_size: data_in_code_len as u32,
            }
        );
        self.get_mut(code_signature).set(
            LinkEditDataCommand {
                command: LC_CODE_SIGNATURE,
                command_size: code_signature.size() as u32,
                data_offset: code_signature_start as u32,
                data_size: code_signature_len as u32,
            }
        );
        self.get_mut(build_version).set(
            BuildVersionCommand {
                command: LC_BUILD_VERSION,
                command_size: build_version_len as u32,
                platform: PlatformEnum::MacOs as u32,
                min_os: 13 << 16, // 13.0
                sdk: (13 << 16) | (1 << 8), // 13.1
                num_tools: 1,
            }
        );
        self.get_mut(ld_tool).set(
            BuildToolVersion {
                tool: ToolEnum::Ld as u32,
                version: (820 << 16) | (1 << 8),
            }
        );
        self.get_mut(src_version).set(
            SourceVersionCommand {
                command: LC_SOURCE_VERSION,
                command_size: src_version.size() as u32,
                version: 0,
            }
        );
        for (dylib, command) in exe.dylibs.iter().zip(&dylib_load_commands) {
            self.get_mut(command.header).set(
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
        self.get_mut(symbol_table).set(
            SymbolTableCommand {
                command: LC_SYMTAB,
                command_size: symbol_table.size() as u32,
                symbol_table_offset: symbol_table_begin as u32,
                num_symbols,
                string_table_offset: string_table_begin as u32,
                string_table_size: string_table_len as u32,
            }
        );
        self.get_mut(dynamic_symbol_table).set(
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
        self.get_mut(load_dylinker).set(
            DylinkerCommand {
                command: LC_LOAD_DYLINKER,
                command_size: load_dylinker_size as u32,
                name_offset: load_dylinker.size() as u32,
            }
        );
        self.get_mut(entry_point).set(
            EntryPointCommand {
                command: LC_MAIN,
                command_size: entry_point.size() as u32,
                entry_point_file_offset: text_section_offsets[text_section.id] as u64,
                stack_size: 0,
            }
        );
        // self.get_mut(uuid).set(
        //     UuidCommand {
        //         command: LC_UUID,
        //         command_size: uuid.size() as u32,
        //         uuid: [0; 16], // to be filled in later.
        //     }
        // );

        // let mut hasher = Md5::new();
        // hasher.update(&self.data[..code_signature_start]);
        // self.get_mut(uuid).map(|uuid| &mut uuid.uuid).set(hasher.finalize().into());

        let mut sha256 = Sha256::new();
        let mut i = 0;
        let mut hash_buf = [0; 32];
        let mut num_code_slots = 0;
        while i < code_signature_start {
            sha256.reset();
            if code_signature_start - i < 4096 {
                sha256.input(&self.data[i..code_signature_start]);
            } else {
                sha256.input(&self.data[i..(i + 4096)]);
            }
            sha256.result(&mut hash_buf);
            self.push(hash_buf);
            num_code_slots += 1;
            i += 4096;
        }

        let code_directory_len = self.pos() - code_directory.start();
        let real_code_signature_len = self.pos() - code_signature_start;
        assert_eq!(real_code_signature_len, code_signature_len);

        self.get_mut(code_directory).set(
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

        self.get_mut(super_blob).set(
            SuperBlobHeader {
                magic: CSMAGIC_EMBEDDED_SIGNATURE,
                length: code_signature_len as u32,
                count: blob_indices.len() as u32,
            }
        );

        dest.write_all(&self.data)?;

        Ok(())
    }
}

define_index_type!(struct DylibId = u32;);
define_index_type!(struct ImportedSymbolId = u32;);

struct MachODylib {
    name: String,
    timestamp: u32,
    current_version: u32,
    compatibility_version: u32,
}

struct MachOImportedSymbol {
    dylib: DylibId,
    name: String,
}

struct MachOExe {
    dylibs: IndexVec<DylibId, MachODylib>,
    dylib_map: HashMap<String, DylibId>,

    imported_symbols: IndexVec<ImportedSymbolId, MachOImportedSymbol>,
    imported_symbol_map: HashMap<String, ImportedSymbolId>,
}

impl MachOExe {
    fn new() -> Self {
        Self {
            dylibs: Default::default(),
            dylib_map: Default::default(),

            imported_symbols: Default::default(),
            imported_symbol_map: Default::default(),
        }
    }

    fn load_dylib(&mut self, name: impl Into<String> + AsRef<str>, timestamp: u32, current_version: u32, compatibility_version: u32) -> DylibId {
        if let Some(&id) = self.dylib_map.get(name.as_ref()) {
            return id;
        } else {
            let name = name.into();
            let id = self.dylibs.push(
                MachODylib {
                    name: name.clone(),
                    timestamp,
                    current_version,
                    compatibility_version
                }
            );
            self.dylib_map.insert(name, id);
            id
        }
    }

    fn import_symbol(&mut self, dylib: DylibId, name: impl Into<String> + AsRef<str>) -> ImportedSymbolId {
        if let Some(&id) = self.imported_symbol_map.get(name.as_ref()) {
            return id;
        } else {
            let name = name.into();
            let id = self.imported_symbols.push(
                MachOImportedSymbol {
                    dylib,
                    name: name.clone(),
                }
            );
            self.imported_symbol_map.insert(name, id);
            id
        }
    }
}