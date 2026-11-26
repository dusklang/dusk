use crate::linker::exe::{Exe, FixupLocationId};
use crate::mir;
use crate::lir;
use crate::driver::Driver;
use crate::target::Arch;

use crate::backend::arm64::Arm64Backend;
use crate::backend::x64::X64Backend;
use crate::backend::dex::DexBackend;

pub mod x64;
pub mod arm64;
pub mod dex;

pub trait AnyBackend {
    fn arch(&self) -> Arch;
    fn compile_func(&self, d: &Driver, func_index: mir::FuncId, is_main: bool, exe: &mut dyn Exe);
}

impl<B> AnyBackend for B where B: Backend {
    fn arch(&self) -> Arch {
        Backend::arch(self)
    }
    fn compile_func(&self, d: &Driver, func_index: mir::FuncId, is_main: bool, exe: &mut dyn Exe) {
        let func = self.lower_func(d, func_index, is_main, exe);
        self.generate_func(d, &func, is_main, exe);
    }
}

pub trait Backend: AnyBackend {
    type Instr;

    fn arch(&self) -> Arch;
    fn lower_func(&self, d: &Driver, func_index: mir::FuncId, is_main: bool, exe: &mut dyn Exe) -> lir::Function<Self::Instr>;
    fn generate_func(&self, d: &Driver, func: &lir::Function<Self::Instr>, is_main: bool, exe: &mut dyn Exe);
}

impl Driver {
    pub fn create_backend(&self) -> Box<dyn AnyBackend> {
        match self.arch {
            Arch::X86_64 => Box::new(X64Backend::new()),
            Arch::Arm64 => Box::new(Arm64Backend::new()),
            Arch::Dex => Box::new(DexBackend),
        }
    }
}


pub enum Indirection {
    Direct,
    Indirect,
}

impl Indirection {
    pub fn dont_care() -> Self {
        Indirection::Direct
    }
}

pub trait CodeBlob {
    fn len(&self) -> usize;

    fn perform_fixups_impl<'a>(&'a mut self, code_addr: usize, get_fixup_addr: Box<dyn FnMut(FixupLocationId) -> (usize, Indirection) + 'a>) -> &'a [u8];
}

pub trait CodeBlobExt: CodeBlob {
    fn perform_fixups<'a>(&'a mut self, code_addr: usize, get_fixup_addr: impl FnMut(FixupLocationId) -> (usize, Indirection) + 'a) -> &'a [u8] {
        self.perform_fixups_impl(code_addr, Box::new(get_fixup_addr))
    }
}

impl<T: ?Sized + CodeBlob> CodeBlobExt for T {
}
