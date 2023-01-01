use std::sync::RwLock;

use string_interner::StringInterner;
use lazy_static::lazy_static;

use crate::dire::hir::ExprId;
use crate::dire::mir::Const;
use crate::dire::arch::Arch;
use crate::dire::source_info::SourceFileId;
use crate::dire::Code;
use crate::dire::InternalFieldDecls;

use crate::source_info::SourceMap;
use crate::token::TokenVec;
use crate::hir;
use crate::tir;
use crate::error::DiagnosticReporter;
use crate::mir::{self, FunctionRef};
use crate::type_provider::TypeProvider;
use crate::index_vec::*;
use crate::rw_ref::RwRef;

// This derive is here so that I can initialize the global Driver instance with something. It is *not* recommended that
// anyone actually uses `Driver` in its default state.
#[derive(Default)]
pub struct Driver {
    pub arch: Arch,
    pub src_map: SourceMap,
    pub toks: IndexVec<SourceFileId, TokenVec>,
    pub interner: StringInterner,
    pub hir: hir::Builder,
    pub tir: tir::Builder,
    pub diag: DiagnosticReporter,
    pub mir: mir::Builder,
    pub code: Code,
    pub internal_field_decls: InternalFieldDecls,
    pub no_core: bool,

    pub boxed_ints: Vec<usize>,
}
pub type DriverRef<'l> = RwRef<'l, Driver>;

impl Driver {
    pub fn new(src_map: SourceMap, arch: Arch, no_core: bool) -> Self {
        Self {
            arch,
            src_map,
            toks: IndexVec::new(),
            interner: StringInterner::new(),
            hir: hir::Builder::default(),
            tir: tir::Builder::default(),
            diag: Default::default(),
            mir: mir::Builder::new(),
            code: Code::default(),
            internal_field_decls: InternalFieldDecls::default(),
            no_core,

            boxed_ints: Vec::default(),
        }
    }
}
impl DriverRef<'_> {
    pub fn eval_expr(&mut self, expr: ExprId, tp: &impl TypeProvider) -> Const {
        let func = self.build_standalone_expr(expr, tp);
        let function_ref = FunctionRef::Ref(func);
        let val = self.call(function_ref, Vec::new(), Vec::new());
        self.write().value_to_const(val, tp.ty(expr).clone(), tp)
    }
}

lazy_static! {
    pub static ref DRIVER: RwLock<Driver> = RwLock::new(Driver::default());
}