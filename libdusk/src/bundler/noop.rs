use std::io::{self, Write};

use crate::driver::Driver;
use crate::mir::FuncId;
use crate::linker::Linker;
use crate::backend::AnyBackend;
use crate::bundler::Bundler;

pub struct NoOpBundler;

impl NoOpBundler {
    pub fn new() -> Self { Self }
}

impl Bundler for NoOpBundler {
    fn write(&mut self, d: &Driver, main_function_index: FuncId, linker: &mut dyn Linker, backend: &mut dyn AnyBackend, dest: &mut dyn Write) -> io::Result<()> {
        linker.write(d, main_function_index, backend, dest)
    }
}
