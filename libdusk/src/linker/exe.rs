use std::{ffi::CStr, ops::{Index, IndexMut}};

use crate::{backend::{CodeBlob, CodeBlobExt, Indirection}, index_vec::IndexVec, linker::dex::DexExe};

define_index_type!(pub struct DynLibId = u32;);
define_index_type!(pub struct ImportedSymbolId = u32;);
define_index_type!(pub struct FixupLocationId = u32;);
define_index_type!(pub struct ExeFuncId = u32;);

pub struct Fixup {
    pub offset: usize,
    pub id: FixupLocationId,
}

#[derive(Default)]
pub struct ExeCode {
    func_blobs: IndexVec<ExeFuncId, Box<dyn CodeBlob>>,
    func_offsets: IndexVec<ExeFuncId, usize>,
    len: usize,
}

impl ExeCode {
    pub fn push(&mut self, blob: Box<dyn CodeBlob>) -> ExeFuncId {
        let offset = self.len;
        self.len += blob.len();
        let id = self.func_blobs.push(blob);
        self.func_offsets.push_at(id, offset);
        id
    }

    pub fn len(&self) -> usize { self.len }

    pub fn func_offset(&self, index: ExeFuncId) -> usize {
        self.func_offsets[index]
    }

    pub fn perform_fixups(&mut self, code_addr: usize, mut get_fixup_addr: impl FnMut(FixupLocationId) -> (usize, Indirection)) -> Vec<u8> {
        let mut code = Vec::new();
        for (blob, &offset) in self.func_blobs.iter_mut().zip(&self.func_offsets) {
            assert_eq!(offset, code.len());
            let code_addr = code_addr + code.len();
            let blob_code = blob.perform_fixups(code_addr, &mut get_fixup_addr);
            code.extend(blob_code);
        }

        code
    }
}

impl Index<ExeFuncId> for ExeCode {
    type Output = dyn CodeBlob;

    fn index(&self, index: ExeFuncId) -> &Self::Output {
        &*self.func_blobs[index]
    }
}

impl IndexMut<ExeFuncId> for ExeCode {
    fn index_mut(&mut self, index: ExeFuncId) -> &mut Self::Output {
        &mut *self.func_blobs[index]
    }
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
    fn add_func(&mut self, blob: Box<dyn CodeBlob>) -> ExeFuncId;

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
