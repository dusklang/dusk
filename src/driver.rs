use string_interner::DefaultStringInterner;

use crate::source_info::SourceFile;
use crate::builder::ExprId;
use crate::token::TokenVec;
use crate::arch::Arch;
use crate::hir;
use crate::tir;
use crate::error::Error;
use crate::mir::{self, FunctionRef, Const};
use crate::interpreter::Interpreter;
use crate::type_checker::type_provider::TypeProvider;

pub struct Driver {
    pub file: SourceFile,
    pub toks: TokenVec,
    pub interner: DefaultStringInterner,
    pub hir: hir::Builder,
    pub tir: tir::Builder,
    pub errors: Vec<Error>,
    pub mir: mir::Builder,
    pub interp: Interpreter,
    /// Total number of errors that have been flushed
    pub flushed_errors: u32,
}

impl Driver {
    pub fn new(file: SourceFile, arch: Arch) -> Self {
        Self {
            file,
            toks: TokenVec::new(),
            interner: DefaultStringInterner::new(),
            hir: hir::Builder::new(),
            tir: tir::Builder::new(),
            errors: Vec::new(),
            mir: mir::Builder::new(arch),
            interp: Interpreter::new(),
            flushed_errors: 0,
        }
    }

    pub fn eval_expr(&mut self, expr: ExprId, tp: &impl TypeProvider) -> Const {
        let func = self.build_standalone_expr(expr, tp);
        self.call(FunctionRef::Ref(func), Vec::new())
            .to_const(self.mir.arch, tp.ty(expr).clone(), &mut self.mir.strings)
    }
}