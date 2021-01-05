use string_interner::StringInterner;

use mire::hir::ExprId;
use mire::mir::Const;
use mire::arch::Arch;
use mire::source_info::SourceFileId;
use mire::Code;

use crate::source_info::SourceMap;
use crate::token::TokenVec;
use crate::hir;
use crate::tir;
use crate::error::Error;
use crate::mir::{self, FunctionRef};
use crate::interpreter::Interpreter;
use crate::typechecker::type_provider::TypeProvider;
use crate::refine::Refine;
use crate::index_vec::*;

pub struct Driver {
    pub arch: Arch,
    pub src_map: SourceMap,
    pub toks: IndexVec<SourceFileId, TokenVec>,
    pub interner: StringInterner,
    pub hir: hir::Builder,
    pub tir: tir::Builder,
    pub errors: Vec<Error>,
    pub mir: mir::Builder,
    pub refine: Refine,
    pub code: Code,
    pub interp: Interpreter,
    /// Total number of errors that have been flushed
    pub flushed_errors: u32,
}

impl Driver {
    pub fn new(src_map: SourceMap, arch: Arch) -> Self {
        let mut d = Self {
            arch,
            src_map,
            toks: IndexVec::new(),
            interner: StringInterner::new(),
            hir: hir::Builder::default(),
            tir: tir::Builder::default(),
            errors: Vec::new(),
            mir: mir::Builder::new(),
            refine: Refine::default(),
            code: Code::default(),
            interp: Interpreter::new(),
            flushed_errors: 0,
        };
        d.initialize_hir();
        d
    }

    pub fn eval_expr(&mut self, expr: ExprId, tp: &impl TypeProvider) -> Const {
        let func = self.build_standalone_expr(expr, tp);
        let function_ref = FunctionRef::Ref(func);
        self.refine_func(&function_ref);
        let val = self.call(function_ref, Vec::new());
        self.value_to_const(val, tp.ty(expr).clone(), tp)
    }
}
