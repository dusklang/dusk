use std::fs::File;
use std::io::prelude::*;
use std::io::Result;
use std::path::Path;

fn main() -> Result<()> {
    if !Path::new("editor-name.txt").exists() {
        let mut file = File::create("editor-name.txt")?;
        file.write_all(b"code")?;
    }
    Ok(())
}
