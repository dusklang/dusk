use std::sync::{Arc, LazyLock, OnceLock, RwLock};
use string_interner::DefaultStringInterner as StringInterner;

use crate::ast::{Ast, ExprId, GenericCtx, KnownIdents};
use crate::mir::{Const, Mir};
use crate::target::{Arch, OperatingSystem};
use crate::internal_types::InternalFieldDecls;
use crate::source_info::SourceMap;
use crate::type_interner::TypeInterner;
use crate::tir;
use crate::error::DiagnosticReporter;
use crate::mir::FunctionRef;
use crate::type_provider::TypeProvider;
use crate::rw_ref::RwRef;
use crate::interpreter::EvalError;

// TODO: finish making all fields on Driver either immutable, or mutable from multiple threads through a shared reference
// The intent is for high-volume mutation during the main passes of the compiler (e.g., creating AST, TIR and MIR) to take place
// in a more efficient data structure outside of Driver, then merged into the shared data structure when ready.

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
    pub mir: Arc<Mir>,
    pub internal_field_decls: OnceLock<InternalFieldDecls>,

    // Mutable state
    pub tir_builder: tir::Builder,
    pub ast: Ast,
}
pub type DriverRwRef<'l> = RwRef<'l, Driver>;

impl Driver {
    pub fn new(src_map: SourceMap, arch: Arch, os: OperatingSystem, no_core: bool) -> Self {
        let interner: Arc<RwLock<StringInterner>> = Default::default();
        let ast = Ast {
            known_idents: KnownIdents::new(&interner),
            ..Default::default()
        };
        let mut val = Self {
            arch,
            os,
            src_map: Arc::new(src_map),
            interner,
            types: Default::default(),
            tir_builder: tir::Builder::default(),
            diag: Default::default(),
            internal_field_decls: Default::default(),
            no_core,
            ast,
            mir: Default::default(),
        };
        val.ast.generic_ctxs.push(GenericCtx::Blank);
        val
    }
}
impl DriverRwRef<'_> {
    pub fn eval_expr(&self, expr: ExprId, tp: &dyn TypeProvider) -> Result<Const, EvalError> {
        let func = self.build_standalone_expr(expr, tp);
        let function_ref = FunctionRef::Ref(func);
        let val = self.call(function_ref, Vec::new(), Vec::new())?;
        Ok(self.read().value_to_const(val, tp.ty(expr).clone(), tp))
    }
}

pub static DRIVER: LazyLock<RwLock<Driver>> = LazyLock::new(|| RwLock::new(Driver::default()));
