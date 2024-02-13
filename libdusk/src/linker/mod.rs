use std::io::{self, Write};
use crate::driver::Driver;
use crate::mir::FuncId;
use crate::target::OperatingSystem;

use crate::linker::macho::MachOLinker;
use crate::linker::pe::PELinker;

#[macro_use]
mod byte_swap;
mod pe;
mod macho;

pub mod exe;

pub trait Linker {
    fn write(&mut self, d: &Driver, main_function_index: FuncId, dest: &mut dyn Write) -> io::Result<()>;
}

impl Driver {
    pub fn create_linker(&self) -> Box<dyn Linker> {
        match self.os {
            OperatingSystem::MacOS => Box::new(MachOLinker::new()),
            OperatingSystem::Windows => Box::new(PELinker::new()),
        }
    }
}
