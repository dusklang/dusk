use std::ffi::CStr;

use index_vec::*;

use crate::{backend::CodeBlob, linker::dex::DexExe};

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

    // TODO: as a performance optimization, perhaps we should have separate methods to intern and use C strings?
    fn use_cstring(&mut self, string: &CStr) -> FixupLocationId;

    // TODO: CodeBlob is not usable for Dex files (and honestly probably insufficient for all executable types if I were to implement features like debug info, for example)
    // I should instead think of a better interface that makes Exe more knowledgeable about functions/methods. E.g., `add_func(FuncSignatureId) -> ExeFuncId` with platform-specific
    // methods for things like creating a function signature, or adding code to a function.
    fn add_code_blob(&mut self, blob: Box<dyn CodeBlob>);

    // Platform or runtime-specific extensions (a bit hacky, but also kind of unavoidable I think)
    fn as_objc_exe(&mut self) -> Option<&mut dyn ObjCExe> { None }
    fn as_dex_exe(&mut self) -> Option<&mut DexExe> { None }
}

/// Implemented by an executable format that supports Objective-C interop. In theory, we could support Objective-C interop on any platform or executable format.
/// In practice, this is synonymous with Mach-O.
pub trait ObjCExe: Exe {
    fn import_framework(&mut self, name: &str) -> DynLibId;
    fn use_constant_nsstring(&mut self, string: &CStr) -> FixupLocationId;
    fn use_objc_selector(&mut self, name: &CStr) -> FixupLocationId;
}
