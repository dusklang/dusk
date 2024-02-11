use std::io::{self, Write};
use crate::linker::Linker;
use crate::mir::FuncId;

pub struct PE32Linker {

}

impl PE32Linker {
    pub fn new() -> PE32Linker {
        PE32Linker {
        }
    }
}

impl Linker for PE32Linker {
    fn write(&mut self, _d: &crate::driver::Driver, _main_function_index: FuncId, _dest: &mut dyn Write) -> io::Result<()> {
        todo!()
    }
}