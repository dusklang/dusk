use std::sync::{Arc, LazyLock, OnceLock, RwLock};
use crossbeam_skiplist::SkipMap;
use string_interner::DefaultStringInterner as StringInterner;

use crate::ast::ExprId;
use crate::mir::Const;
use crate::target::{Arch, OperatingSystem};
use crate::source_info::SourceFileId;
use crate::code::Code;
use crate::internal_types::InternalFieldDecls;
use crate::source_info::SourceMap;
use crate::token::TokenVec;
use crate::type_interner::TypeInterner;
use crate::ast;
use crate::tir;
use crate::error::DiagnosticReporter;
use crate::mir::{self, FunctionRef};
use crate::type_provider::TypeProvider;
use crate::rw_ref::RwRef;
use crate::interpreter::EvalError;

// This derive is here so that I can initialize the global Driver instance with something. It is *not* recommended that
// anyone actually uses `Driver` in its default state.
#[derive(Default)]
pub struct Driver {
    // Constant compiler options
    pub arch: Arch,
    pub os: OperatingSystem,
    pub no_core: bool,

    // Concurrently-accessible global state (these don't really need to be Arcs, they just are for the time being).
    pub diag: Arc<DiagnosticReporter>,
    pub types: Arc<TypeInterner>,
    pub interner: Arc<RwLock<StringInterner>>,
    pub src_map: Arc<SourceMap>,
    pub toks: SkipMap<SourceFileId, TokenVec>,
    pub internal_field_decls: OnceLock<InternalFieldDecls>,

    // Mutable state
    pub ast: ast::Builder,
    pub tir: tir::Builder,
    pub mir: mir::Builder,
    pub code: Code,
}
pub type DriverRwRef<'l> = RwRef<'l, Driver>;

impl Driver {
    pub fn new(src_map: SourceMap, arch: Arch, os: OperatingSystem, no_core: bool) -> Self {
        Self {
            arch,
            os,
            src_map: Arc::new(src_map),
            toks: Default::default(),
            interner: Default::default(),
            types: Default::default(),
            ast: ast::Builder::default(),
            tir: tir::Builder::default(),
            diag: Default::default(),
            mir: mir::Builder::new(),
            code: Code::default(),
            internal_field_decls: Default::default(),
            no_core,
        }
    }
}
impl DriverRwRef<'_> {
    pub fn eval_expr(&mut self, expr: ExprId, tp: &dyn TypeProvider) -> Result<Const, EvalError> {
        let func = self.build_standalone_expr(expr, tp);
        let function_ref = FunctionRef::Ref(func);
        let val = self.call(function_ref, Vec::new(), Vec::new())?;
        Ok(self.write().value_to_const(val, tp.ty(expr).clone(), tp))
    }
}

pub static DRIVER: LazyLock<RwLock<Driver>> = LazyLock::new(|| RwLock::new(Driver::default()));
