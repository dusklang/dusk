use std::io::{self, Write};
use crate::driver::Driver;
use crate::mir::FuncId;
use crate::target::OperatingSystem;
use crate::backend::Backend;
use crate::linker::Linker;

mod noop;
mod apk;

use noop::NoOpBundler;
use apk::ApkBundler;

pub trait Bundler {
    fn write(&mut self, d: &Driver, main_function_index: FuncId, linker: &mut dyn Linker, backend: &mut dyn Backend, dest: &mut dyn Write) -> io::Result<()>;
}

impl Driver {
    pub fn create_bundler(&self) -> Box<dyn Bundler> {
        match self.os {
            // TODO: support macOS app bundles, and more. The former of which will require changing the Bundler trait's interface since it only supports outputting a single file.
            OperatingSystem::MacOS | OperatingSystem::Windows | OperatingSystem::Linux => Box::new(NoOpBundler::new()),
            OperatingSystem::Android => Box::new(ApkBundler::new()),
        }
    }
}
