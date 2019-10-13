use crate::source_info::SourceFile;
use crate::arch::Arch;
use crate::hir;
use crate::type_checker;
use crate::error::Error;
use crate::mir;
use crate::interpreter::Interpreter;

pub struct Driver<'src> {
    pub file: SourceFile,
    pub hir: &'src hir::Program,
    pub tc: &'src type_checker::Program,
    pub errors: Vec<Error>,
    pub mir: mir::Builder,
    pub interp: Interpreter,
}

impl<'src> Driver<'src> {
    pub fn new(file: SourceFile, hir: &'src hir::Program, tc: &'src type_checker::Program, errors: Vec<Error>, arch: Arch) -> Self {
        Self {
            file,
            hir,
            tc,
            errors,
            mir: mir::Builder::new(arch),
            interp: Interpreter::new(),
        }
    }
}