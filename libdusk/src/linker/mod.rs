use std::io::{self, Write};
use crate::driver::Driver;
use crate::mir::FuncId;
use crate::target::OperatingSystem;
use crate::backend::Backend;
use crate::linker::macho::MachOLinker;
use crate::linker::pe::PELinker;
use crate::linker::dex::DexLinker;

#[macro_use]
pub mod byte_swap;
mod pe;
mod macho;
pub mod dex;

pub mod exe;

pub trait Linker {
    fn write(&mut self, d: &Driver, main_function_index: FuncId, backend: &mut dyn Backend, dest: &mut dyn Write) -> io::Result<()>;
}

impl Driver {
    pub fn create_linker(&self) -> Box<dyn Linker> {
        match self.os {
            OperatingSystem::MacOS => Box::new(MachOLinker::new()),
            OperatingSystem::Windows => Box::new(PELinker::new()),
            OperatingSystem::Android => Box::new(DexLinker::new()),
        }
    }
}
