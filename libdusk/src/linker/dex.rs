use std::io::{self, Write};
use std::ffi::CStr;

use crate::linker::Linker;
use crate::linker::exe::Exe;
use crate::driver::Driver;
use crate::mir::FuncId;
use crate::backend::{Backend, CodeBlobExt, Indirection};
use crate::linker::exe::*;

pub struct DexExe;

impl Exe for DexExe {
    fn import_dynamic_library(&mut self, name: &str) -> DynLibId {
        todo!()
    }

    fn import_symbol(&mut self, dyn_lib: DynLibId, name: String) -> ImportedSymbolId {
        todo!()
    }

    fn use_imported_symbol(&mut self, symbol: ImportedSymbolId) -> FixupLocationId {
        todo!()
    }

    fn use_cstring(&mut self, string: &CStr) -> FixupLocationId {
        todo!()
    }

    fn as_objc_exe(&mut self) -> Option<&mut dyn ObjCExe> {
        None
    }
}

pub struct DexLinker;

impl Linker for DexLinker {
    fn write(&mut self, d: &Driver, main_function_index: FuncId, backend: &mut dyn Backend, dest: &mut dyn Write) -> io::Result<()> {
        let mut exe = DexExe;
        let mut code = backend.generate_func(d, main_function_index, true, &mut exe);
        let code = code.perform_fixups(0, |fixup| (0, Indirection::dont_care()));
        dest.write_all(code)?;
        Ok(())
    }
}

impl DexLinker {
    pub fn new() -> Self {
        Self
    }
}
