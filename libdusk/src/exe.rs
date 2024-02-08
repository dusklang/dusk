use std::ffi::CStr;

use index_vec::*;

define_index_type!(pub struct DynLibId = u32;);
define_index_type!(pub struct ImportedSymbolId = u32;);
define_index_type!(pub struct FixupLocationId = u32;);

pub struct Fixup {
    pub offset: usize,
    pub id: FixupLocationId,
}

pub enum DynamicLibrarySource<'a> {
    Name(&'a str),
    FrameworkName(&'a str),
}

pub trait Exe {
    fn import_dynamic_library(&mut self, source: DynamicLibrarySource) -> DynLibId;

    fn import_symbol(&mut self, dyn_lib: DynLibId, name: String) -> ImportedSymbolId;

    fn use_imported_symbol(&mut self, symbol: ImportedSymbolId) -> FixupLocationId;

    // TODO: perhaps we should have separate methods to intern and use C strings?
    fn use_cstring(&mut self, string: &CStr) -> FixupLocationId;

    // TODO: perhaps these should be Mach-O specific somehow? Or maybe they just panic when targeting non-Apple
    // platforms.
    fn use_constant_nsstring(&mut self, string: &CStr) -> FixupLocationId;
    fn use_objc_selector(&mut self, name: &CStr) -> FixupLocationId;
}
