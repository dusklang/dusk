use string_interner::DefaultStringInterner;

use crate::source_info::SourceFile;
use crate::token::TokenVec;
use crate::arch::Arch;
use crate::hir;
use crate::tir;
use crate::type_checker::TypeChecker;
use crate::error::Error;
use crate::mir;
use crate::interpreter::Interpreter;

pub struct Driver {
    pub file: SourceFile,
    pub toks: TokenVec,
    pub interner: DefaultStringInterner,
    pub hir: hir::Builder,
    pub tir: tir::Builder,
    pub tc: TypeChecker,
    pub errors: Vec<Error>,
    pub mir: mir::Builder,
    pub interp: Interpreter,
}

impl Driver {
    pub fn new(file: SourceFile, interner: DefaultStringInterner, debug_tc: bool, arch: Arch) -> Self {
        Self {
            file,
            toks: TokenVec::new(),
            interner,
            hir: hir::Builder::new(),
            tir: tir::Builder::new(),
            tc: TypeChecker::new(debug_tc),
            errors: Vec::new(),
            mir: mir::Builder::new(arch),
            interp: Interpreter::new(),
        }
    }
}