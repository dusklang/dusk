#![allow(unused)]

use std::collections::HashMap;
use std::io::{self, Write};
use std::ffi::{CStr, CString};
use std::mem;

use bitflags::bitflags;
use index_vec::IndexVec;

use crate::backend::{Backend, CodeBlobExt, Indirection};
use crate::linker::Linker;
use crate::linker::exe::*;
use crate::mir::FuncId;
use crate::driver::Driver;
use crate::target::Arch;

use dusk_proc_macros::{ByteSwap, ByteSwapBitflags};

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
    #[derive(ByteSwapBitflags)]
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
    #[derive(ByteSwapBitflags)]
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
    #[derive(ByteSwapBitflags, Clone, Copy)]
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

#[derive(Copy, Clone)]
struct SectionRef {
    rva: usize,
    address: usize,
    name: [u8; 8],
    characteristics: SectionCharacteristics,
}

#[derive(Default)]
pub struct PELinker {
    buf: Buffer,
    num_sections: usize,
}

impl PELinker {
    pub fn new() -> PELinker {
        PELinker::default()
    }
}

const FILE_ALIGNMENT: usize = 0x200;
const SECTION_ALIGNMENT: usize = 0x1000;

impl Linker for PELinker {
    fn write(&mut self, d: &Driver, main_function_index: FuncId, backend: &mut dyn Backend, dest: &mut dyn Write) -> io::Result<()> {
        let mut exe = PEExe::new();

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

        self.buf.pad_to_next_boundary(FILE_ALIGNMENT);

        let size_of_headers = self.buf.pos();

        let base_of_code = 0x1000;
        self.buf.jump_to_rva(base_of_code);

        let text_section = self.begin_section(b".text\0\0\0", SectionCharacteristics::CNT_CODE | SectionCharacteristics::MEM_EXECUTE | SectionCharacteristics::MEM_READ);

        let address_of_entry_point = self.buf.rva();

        let mut code = backend.generate_func(d, main_function_index, true, &mut exe);
        self.buf.pad_with_zeroes(code.len());

        let size_of_code = self.end_section(text_section, text_header);

        let rdata_section = self.begin_section(b".rdata\0\0", SectionCharacteristics::MEM_READ);
        self.buf.extend(&exe.cstrings);
        self.end_section(rdata_section, rdata_header);

        let (import_table, dll_import_info) = self.write_idata(&exe, idata_header);

        // This is done so that size_of_image below will be aligned to the section_alignment
        self.buf.pad_rva_to_next_boundary(SECTION_ALIGNMENT);

        let machine = match backend.arch() {
            Arch::Arm64 => Machine::Arm64,
            Arch::X86_64 => Machine::Amd64,
            Arch::Dex => unimplemented!(),
        };
        self.buf.get_mut(coff_header).set(
            CoffHeader {
                machine: machine as u16,
                number_of_sections: self.num_sections as u16,
                time_date_stamp: 0, // should be set to a meaningful timestamp, I guess
                pointer_to_symbol_table: 0xfaceface,
                number_of_symbols: 0xfaceface,
                size_of_optional_header: mem::size_of::<Pe32PlusOptionalHeader>() as u16,
                characteristics: Characteristics::EXECUTABLE_IMAGE | Characteristics::LARGE_ADDRESS_AWARE,
            }
        );

        let size_of_image = self.buf.rva() as u32;
        self.buf.get_mut(optional_header).set(
            Pe32PlusOptionalHeader {
                magic: 0x20B,
                major_linker_version: 0xfa,
                minor_linker_version: 0xfa,
                size_of_code: size_of_code as u32,
                size_of_initialized_data: 0xfaceface,
                size_of_uninitialized_data: 0xfaceface,
                address_of_entry_point: address_of_entry_point as u32,
                base_of_code: base_of_code as u32,
    
                image_base: 0x0000000140000000,
                section_alignment: SECTION_ALIGNMENT as u32,
                file_alignment: FILE_ALIGNMENT as u32,
                major_operating_system_version: 0xface,
                minor_operating_system_version: 0xface,
                major_image_version: 0xface,
                minor_image_version: 0xface,
                major_subsystem_version: 5,
                minor_subsystem_version: 0,
                win32_version_value: 0xfaceface,
                size_of_image,
                size_of_headers: size_of_headers as u32,
                check_sum: 0xfaceface,
                subsystem: Subsystem::WindowsCui as u16,
                dll_characteristics: DllCharacteristics::NX_COMPAT.bits() | DllCharacteristics::DYNAMIC_BASE.bits(),
                size_of_stack_reserve: 0x00ceface,
                size_of_stack_commit: 0x00ceface,
                size_of_heap_reserve: 0x00ceface,
                size_of_heap_commit: 0x00ceface,
                loader_flags: 0xfaceface,
                number_of_rva_and_sizes: 16,
    
                export_table: ImageDataDirectory { virtual_address: 0, size: 0 },
                import_table,
                resource_table: ImageDataDirectory { virtual_address: 0, size: 0 },
                exception_table: ImageDataDirectory { virtual_address: 0, size: 0 },
                certificate_table: ImageDataDirectory { virtual_address: 0, size: 0 },
                base_relocation_table: ImageDataDirectory { virtual_address: 0, size: 0 },
                debug_directory: ImageDataDirectory { virtual_address: 0, size: 0 },
                architecture_specific_data: ImageDataDirectory { virtual_address: 0, size: 0 },
                global_ptr: ImageDataDirectory { virtual_address: 0, size: 0 },
                tls_table: ImageDataDirectory { virtual_address: 0, size: 0 },
                load_config_table: ImageDataDirectory { virtual_address: 0, size: 0 },
                bound_import_table: ImageDataDirectory { virtual_address: 0, size: 0 },
                import_address_table: ImageDataDirectory { virtual_address: 0, size: 0 },
                delay_import_descriptor: ImageDataDirectory { virtual_address: 0, size: 0 },
                clr_runtime_header: ImageDataDirectory { virtual_address: 0, size: 0 },
                reserved: ImageDataDirectory { virtual_address: 0, size: 0 },
            }
        );

        let code = code.perform_fixups(text_section.rva, |fixup| {
            match exe.fixup_locations[fixup] {
                PEFixupLocation::ImportedProc(proc) => {
                    let proc = &exe.procs[proc];
                    let dll = proc.dll;
                    let index = proc.proc_index;
                    (dll_import_info[dll].address_entries[index].rva, Indirection::Direct)
                },
                PEFixupLocation::RDataSectionOffset(offset) => {
                    (rdata_section.rva + offset, Indirection::Indirect)
                },
            }
        });

        self.buf.data[text_section.address..(text_section.address + code.len())].copy_from_slice(code);

        dest.write_all(&self.buf.data)?;
        Ok(())
    }
}

struct DllImportInfo {
    directory_entry: Ref<ImportDirectoryTableEntry>,
    lookup_table_rva: u32,
    lookup_entries: Vec<Ref<Pe32PlusImportLookupTableEntry>>,
    address_table_rva: u32,
    address_entries: Vec<Ref<Pe32PlusImportLookupTableEntry>>,
    hont_rvas: Vec<u32>,
}

impl PELinker {
    fn write_idata(&mut self, exe: &PEExe, idata_header: Ref<SectionHeader>) -> (ImageDataDirectory, IndexVec<DynLibId, DllImportInfo>) {
        let idata_section = self.begin_section(b".idata\0\0", SectionCharacteristics::MEM_READ);

        let import_table_rva = self.buf.rva();

        // Import directory table
        let mut dll_import_info: IndexVec<DynLibId, DllImportInfo> = exe.dll_imports.iter()
            .map(|_| DllImportInfo {
                directory_entry: self.buf.alloc(),
                lookup_table_rva: u32::MAX,
                lookup_entries: Vec::new(),
                address_table_rva: u32::MAX,
                address_entries: Vec::new(),
                hont_rvas: Vec::new(),
            })
            .collect();
        self.buf.push(ImportDirectoryTableEntry::null());

        for (dll, info) in exe.dll_imports.iter().zip(&mut dll_import_info) {
            // Lookup table
            info.lookup_table_rva = self.buf.rva() as u32;
            for _ in &dll.procs {
                info.lookup_entries.push(self.buf.alloc());
            }
            self.buf.push(Pe32PlusImportLookupTableEntry::null());
        }

        for (dll, info) in exe.dll_imports.iter().zip(&mut dll_import_info) {
            // Address table
            info.address_table_rva = self.buf.rva() as u32;
            for _ in &dll.procs {
                info.address_entries.push(self.buf.alloc());
            }
            self.buf.push(Pe32PlusImportLookupTableEntry::null());
        }

        for (dll, info) in exe.dll_imports.iter().zip(&mut dll_import_info) {
            for proc in &dll.procs {
                let rva = self.buf.rva() as u32;
                // Hint/Name Entry:
                self.buf.push(0u16);             // hint
                self.push_pe_ascii_string(proc); // name
                info.hont_rvas.push(rva);
            }
        }

        let import_table_size = self.buf.rva() - import_table_rva;

        for (dll, info) in exe.dll_imports.iter().zip(&dll_import_info) {
            let name_rva = self.buf.rva() as u32;
            self.push_pe_ascii_string(&dll.name);

            self.buf.get_mut(info.directory_entry).set(
                ImportDirectoryTableEntry {
                    lookup_table_rva: info.lookup_table_rva,
                    time_date_stamp: 0,
                    forwarder_chain: 0,
                    name_rva,
                    import_address_table_rva: info.address_table_rva,
                }
            );

            for (&lookup_entry, &hont_rva) in info.lookup_entries.iter().zip(&info.hont_rvas) {
                self.buf.get_mut(lookup_entry).set(Pe32PlusImportLookupTableEntry::hint_or_name_table_rva(hont_rva));
            }

            for (&address_entry, &hont_rva) in info.address_entries.iter().zip(&info.hont_rvas) {
                self.buf.get_mut(address_entry).set(Pe32PlusImportLookupTableEntry::hint_or_name_table_rva(hont_rva));
            }
        }

        self.end_section(idata_section, idata_header);

        let dir = ImageDataDirectory {
            virtual_address: import_table_rva as u32,
            size: import_table_size as u32,
        };

        (dir, dll_import_info)
    }

    fn push_pe_ascii_string(&mut self, val: &str) {
        self.buf.push_null_terminated_string(val);
        if val.len() % 2 == 0 {
            self.buf.push(0u8);
        }
    }

    fn begin_section(&mut self, name: &[u8; 8], characteristics: SectionCharacteristics) -> SectionRef {
        self.buf.pad_to_next_boundary(FILE_ALIGNMENT);
        self.buf.pad_rva_to_next_boundary(SECTION_ALIGNMENT);

        SectionRef {
            rva: self.buf.rva(),
            address: self.buf.pos(),
            name: *name,
            characteristics,
        }
    }

    // Returns the size
    fn end_section(&mut self, section_ref: SectionRef, header_ref: Ref<SectionHeader>) -> usize {
        let virtual_size = self.buf.rva() - section_ref.rva;
        self.buf.pad_to_next_boundary(FILE_ALIGNMENT);
        let size_of_raw_data = self.buf.pos() - section_ref.address;

        self.num_sections += 1;

        self.buf.get_mut(header_ref).set(
            SectionHeader {
                name: section_ref.name,
                virtual_size: virtual_size as u32,
                virtual_address: section_ref.rva as u32,
                size_of_raw_data: size_of_raw_data as u32,
                pointer_to_raw_data: section_ref.address as u32,
                pointer_to_relocations: 0,
                pointer_to_line_numbers: 0,
                number_of_relocations: 0,
                number_of_line_numbers: 0,
                characteristics: section_ref.characteristics,
            }
        );

        size_of_raw_data
    }
}

struct DllImport {
    name: String,
    procs: Vec<String>,
}

struct DllProcHandle {
    dll: DynLibId,
    proc_index: usize,
    name: String,
}

enum PEFixupLocation {
    ImportedProc(ImportedSymbolId),
    RDataSectionOffset(usize),
}

#[derive(Default)]
struct PEExe {
    dll_imports: IndexVec<DynLibId, DllImport>,
    procs: IndexVec<ImportedSymbolId, DllProcHandle>,
    fixup_locations: IndexVec<FixupLocationId, PEFixupLocation>,

    cstrings: Vec<u8>,
    cstring_map: HashMap<CString, usize>,
}

impl PEExe {
    fn new() -> Self {
        Default::default()
    }

    #[doc(hidden)]
    fn intern_cstring(&mut self, string: &CStr) -> usize {
        *self.cstring_map.entry(string.to_owned()).or_insert_with(|| {
            let offset = self.cstrings.len();
            self.cstrings.extend(string.to_bytes_with_nul());
            if self.cstrings.len() % 2 != 0 {
                self.cstrings.push(0);
            }
            offset
        })
    }
}

impl Exe for PEExe {
    fn import_dynamic_library(&mut self, name: &str) -> DynLibId {
        self.dll_imports.push(
            DllImport {
                name: String::from(name),
                procs: Vec::new(),
            }
        )
    }

    fn import_symbol(&mut self, dll: DynLibId, name: String) -> ImportedSymbolId {
        let procs = &mut self.dll_imports[dll].procs;
        let id = self.procs.push(DllProcHandle { dll, proc_index: procs.len(), name: name.clone() });
        procs.push(name);
        id
    }

    fn use_imported_symbol(&mut self, symbol: ImportedSymbolId) -> FixupLocationId {
        self.fixup_locations.push(PEFixupLocation::ImportedProc(symbol))
    }

    fn use_cstring(&mut self, string: &CStr) -> FixupLocationId {
        let offset = self.intern_cstring(string);
        self.fixup_locations.push(PEFixupLocation::RDataSectionOffset(offset))
    }
}
