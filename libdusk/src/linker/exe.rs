use std::ffi::CStr;

use index_vec::*;

define_index_type!(pub struct DynLibId = u32;);
define_index_type!(pub struct ImportedSymbolId = u32;);
define_index_type!(pub struct FixupLocationId = u32;);

pub struct Fixup {
    pub offset: usize,
    pub id: FixupLocationId,
}

pub trait Exe {
    fn import_dynamic_library(&mut self, name: &str) -> DynLibId;

    fn import_symbol(&mut self, dyn_lib: DynLibId, name: String) -> ImportedSymbolId;

    fn use_imported_symbol(&mut self, symbol: ImportedSymbolId) -> FixupLocationId;

    // TODO: perhaps we should have separate methods to intern and use C strings?
    fn use_cstring(&mut self, string: &CStr) -> FixupLocationId;

    fn as_objc_exe(&mut self) -> Option<&mut dyn ObjCExe>;
}

/// Implemented by an executable format that supports Objective-C interop. In theory, we could support Objective-C interop on any platform or executable format.
/// In practice, this is synonymous with Mach-O.
pub trait ObjCExe: Exe {
    fn import_framework(&mut self, name: &str) -> DynLibId;
    fn use_constant_nsstring(&mut self, string: &CStr) -> FixupLocationId;
    fn use_objc_selector(&mut self, name: &CStr) -> FixupLocationId;
}
