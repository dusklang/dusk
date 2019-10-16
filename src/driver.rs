use crate::source_info::SourceFile;
use crate::arch::Arch;
use crate::hir;
use crate::tir;
use crate::type_checker::TypeChecker;
use crate::error::Error;
use crate::mir;
use crate::interpreter::Interpreter;

pub struct Driver {
    pub file: SourceFile,
    pub hir: hir::Program,
    pub tir: tir::Builder,
    pub tc: TypeChecker,
    pub errors: Vec<Error>,
    pub mir: mir::Builder,
    pub interp: Interpreter,
}

impl Driver {
    pub fn new(file: SourceFile, hir: hir::Program, debug_tc: bool, arch: Arch) -> Self {
        Self {
            file,
            hir,
            tir: tir::Builder::new(),
            tc: TypeChecker::new(debug_tc),
            errors: Vec::new(),
            mir: mir::Builder::new(arch),
            interp: Interpreter::new(),
        }
    }
}