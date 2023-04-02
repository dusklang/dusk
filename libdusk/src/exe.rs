use std::ffi::CStr;

use index_vec::*;

define_index_type!(pub struct DylibId = u32;);
define_index_type!(pub struct ImportedSymbolId = u32;);
define_index_type!(pub struct FixupLocationId = u32;);

pub struct Fixup {
    pub offset: usize,
    pub id: FixupLocationId,
}

pub trait Exe {
    #[doc(hidden)]
    fn import_symbol_impl(&mut self, dylib: DylibId, name: String) -> ImportedSymbolId;

    fn use_imported_symbol(&mut self, symbol: ImportedSymbolId) -> FixupLocationId;

    // TODO: perhaps we should have separate methods to intern and use C strings?
    fn use_cstring(&mut self, string: &CStr) -> FixupLocationId;
}

pub trait ExeExt: Exe {
    fn import_symbol(&mut self, dylib: DylibId, name: impl Into<String>) -> ImportedSymbolId {
        self.import_symbol_impl(dylib, name.into())
    }
}

impl<T: Exe> ExeExt for T {}

impl<'a> ExeExt for dyn Exe + 'a {}