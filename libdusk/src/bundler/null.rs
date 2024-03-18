use std::io::{self, Write};

use crate::driver::Driver;
use crate::mir::FuncId;
use crate::linker::Linker;
use crate::backend::Backend;
use crate::bundler::Bundler;

pub struct NullBundler;

impl NullBundler {
    pub fn new() -> Self { Self }
}

impl Bundler for NullBundler {
    fn write(&mut self, d: &Driver, main_function_index: FuncId, linker: &mut dyn Linker, backend: &mut dyn Backend, dest: &mut dyn Write) -> io::Result<()> {
        linker.write(d, main_function_index, backend, dest)
    }
}
