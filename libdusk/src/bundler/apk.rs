use std::io::{self, Write};

use crate::driver::Driver;
use crate::mir::FuncId;
use crate::linker::Linker;
use crate::backend::Backend;
use crate::bundler::Bundler;
use crate::zip::ZipBuilder;

pub struct ApkBundler;

impl ApkBundler {
    pub fn new() -> Self { Self }
}

impl Bundler for ApkBundler {
    fn write(&mut self, d: &Driver, main_function_index: FuncId, linker: &mut dyn Linker, backend: &mut dyn Backend, dest: &mut dyn Write) -> io::Result<()> {
        let mut classes_dex = Vec::new();
        linker.write(d, main_function_index, backend, &mut classes_dex)?;

        let mut archive = ZipBuilder::new();
        archive.add("classes.dex", classes_dex);
        // TODO: generate a binary-encoded AndroidManifest.xml instead of hardcoding this one.
        archive.add("AndroidManifest.xml", include_bytes!("../../files/AndroidManifest.xml"));
        archive.write(dest)?;

        Ok(())
    }
}
