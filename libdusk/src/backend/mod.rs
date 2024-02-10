use crate::exe::FixupLocationId;

#[cfg(target_arch="x86_64")]
pub mod x64;
pub mod arm64;

pub enum Indirection {
    Direct,
    Indirect,
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
