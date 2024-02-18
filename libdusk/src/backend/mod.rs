use crate::linker::exe::{Exe, FixupLocationId};
use crate::mir::FuncId;
use crate::driver::Driver;
use crate::target::Arch;

use crate::backend::arm64::Arm64Backend;
use crate::backend::x64::X64Backend;

pub mod x64;
pub mod arm64;

pub trait Backend {
    fn arch(&self) -> Arch;
    fn generate_func(&self, d: &Driver, func_index: FuncId, is_main: bool, exe: &mut dyn Exe) -> Box<dyn CodeBlob>;
}

impl Driver {
    pub fn create_backend(&self) -> Box<dyn Backend> {
        match self.arch {
            Arch::X86_64 => Box::new(X64Backend::new()),
            Arch::Arm64 => Box::new(Arm64Backend::new()),
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
