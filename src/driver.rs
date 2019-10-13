use crate::arch::Arch;
use crate::hir;
use crate::type_checker;
use crate::mir;
use crate::interpreter::Interpreter;

pub struct Driver<'src> {
    pub hir: &'src hir::Program,
    pub tc: &'src type_checker::Program,
    pub mir: mir::Builder,
    pub interp: Interpreter,
}

impl<'src> Driver<'src> {
    pub fn new(hir: &'src hir::Program, tc: &'src type_checker::Program, arch: Arch) -> Self {
        Self {
            hir,
            tc,
            mir: mir::Builder::new(arch),
            interp: Interpreter::new(),
        }
    }
}