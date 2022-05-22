use std::sync::RwLock;

use string_interner::StringInterner;
use lazy_static::lazy_static;

use dire::hir::ExprId;
use dire::mir::Const;
use dire::arch::Arch;
use dire::source_info::SourceFileId;
use dire::Code;

use crate::source_info::SourceMap;
use crate::token::TokenVec;
use crate::hir;
use crate::tir;
use crate::error::Error;
use crate::mir::{self, FunctionRef};
use crate::typechecker::type_provider::TypeProvider;
use crate::refine::Refine;
use crate::index_vec::*;
use crate::rw_ref::RwRef;

#[derive(Default)]
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
    /// Total number of errors that have been flushed
    pub flushed_errors: u32,
    pub run_refiner: bool,
}
pub type DriverRef<'l> = RwRef<'l, Driver>;

impl Driver {
    pub fn new(src_map: SourceMap, arch: Arch, run_refiner: bool) -> Self {
        Self {
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
            flushed_errors: 0,
            run_refiner,
        }
    }
}
impl DriverRef<'_> {
    pub fn eval_expr(&mut self, expr: ExprId, tp: &impl TypeProvider) -> Const {
        let func = self.build_standalone_expr(expr, tp);
        let function_ref = FunctionRef::Ref(func);
        if self.read().run_refiner {
            self.write().refine_func(&function_ref, tp);
        }
        let val = self.call(function_ref, Vec::new(), Vec::new());
        self.write().value_to_const(val, tp.ty(expr).clone(), tp)
    }
}

lazy_static! {
    pub static ref DRIVER: RwLock<Driver> = RwLock::new(Driver::default());
}