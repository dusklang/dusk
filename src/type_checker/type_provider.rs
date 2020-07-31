use crate::builder::{ExprId, DeclId, DeclRefId, CastId};
use crate::ty::{Type, QualType};
use super::{CastMethod, constraints::ConstraintList};
use crate::{hir, tir};
use crate::mir::Const;
use crate::index_vec::Idx;
use crate::source_info::{SourceFile, CommentatedSourceRange};

pub trait TypeProvider {
    fn init(&mut self, debug: bool, hir: &hir::Builder, tir: &tir::Builder);

    fn debug_output(&mut self, hir: &hir::Builder, file: &SourceFile, level: usize);

    fn ty(&self, expr: ExprId) -> &Type;
    fn ty_mut(&mut self, expr: ExprId) -> &mut Type;

    fn overload(&self, decl_ref: DeclRefId) -> Option<DeclId>;
    fn overload_mut(&mut self, decl_ref: DeclRefId) -> &mut Option<DeclId>;

    fn cast_method(&self, cast: CastId) -> CastMethod;
    fn cast_method_mut(&mut self, cast: CastId) -> &mut CastMethod;

    fn constraints(&self, expr: ExprId) -> &ConstraintList;
    fn constraints_mut(&mut self, expr: ExprId) -> &mut ConstraintList;

    fn preferred_overload(&self, decl_ref: DeclRefId) -> Option<DeclId>;
    fn preferred_overload_mut(&mut self, decl_ref: DeclRefId) -> &mut Option<DeclId>;

    fn insert_eval_result(&mut self, expr: ExprId, result: Const);
    /// Doesn't get the type *of* `id`, gets the type that `id` as an expression *is*
    fn get_evaluated_type(&self, id: ExprId) -> &Type;

    fn fetch_decl_type(&mut self, hir: &hir::Builder, id: DeclId) -> &QualType;
    fn decl_type_mut(&mut self, decl: DeclId) -> &mut QualType;

}

impl TypeProvider for super::TypeChecker {
    fn init(&mut self, debug: bool, hir: &hir::Builder, tir: &tir::Builder) {
        self.debug = debug;
        self.types.resize_with(hir.exprs.len(), Default::default);
        self.constraints.resize_with(hir.exprs.len(), Default::default);
        if self.debug {
            self.constraints_copy.resize_with(hir.exprs.len(), Default::default);
        }
        self.overloads.resize_with(tir.overloads.len(), || None);
        self.preferred_overloads.resize_with(tir.overloads.len(), || None);
        self.cast_methods.resize_with(hir.cast_counter.len(), || CastMethod::Noop);

        for i in 0..tir.decls.len() {
            let id = DeclId::new(i);
            let is_mut = tir.decls[id].is_mut;
            self.decl_types.push(QualType { ty: Type::Error, is_mut });
        }
    }

    fn debug_output(&mut self, hir: &hir::Builder, file: &SourceFile, level: usize) {
        if !self.debug { return; }
        println!("LEVEL {}", level);
        assert_eq!(self.constraints.len(), self.constraints_copy.len());
        for i in 0..self.constraints.len() {
            let i = ExprId::new(i);
            let new_constraints = &self.constraints[i];
            let old_constraints = &mut self.constraints_copy[i];
            if new_constraints != old_constraints {
                file.print_commentated_source_ranges(&mut [
                    CommentatedSourceRange::new(hir.get_range(i), "", '-')
                ]);
                old_constraints.print_diff(new_constraints);
                *old_constraints = new_constraints.clone();
                println!("============================================================================================\n")
            }
        }
    }

    fn insert_eval_result(&mut self, expr: ExprId, result: Const) {
        self.eval_results.insert(expr, result).unwrap();
    }
    fn get_evaluated_type(&self, id: ExprId) -> &Type {
        match &self.eval_results[&id] {
            Const::Ty(ty) => ty,
            x => panic!("Expected type! Found {:?}", x),
        }
    }

    fn fetch_decl_type(&mut self, hir: &hir::Builder, id: DeclId) -> &QualType {
        if let Type::Error = self.decl_types[id].ty {
            if let Some(expr) = hir.explicit_tys[id] {
                let ty = self.get_evaluated_type(expr).clone();
                self.decl_types[id].ty = ty;
            }
        }

        &self.decl_types[id]
    }

    fn decl_type_mut(&mut self, decl: DeclId) -> &mut QualType {
        &mut self.decl_types[decl]
    }

    fn ty(&self, expr: ExprId) -> &Type {
        &self.types[expr]
    }

    fn ty_mut(&mut self, expr: ExprId) -> &mut Type {
        &mut self.types[expr]
    }

    fn overload(&self, decl_ref: DeclRefId) -> Option<DeclId> {
        self.overloads[decl_ref]
    }
    fn overload_mut(&mut self, decl_ref: DeclRefId) -> &mut Option<DeclId> {
        &mut self.overloads[decl_ref]
    }

    fn cast_method(&self, cast: CastId) -> CastMethod {
        self.cast_methods[cast]
    }
    fn cast_method_mut(&mut self, cast: CastId) -> &mut CastMethod {
        &mut self.cast_methods[cast]
    }

    fn constraints(&self, expr: ExprId) -> &ConstraintList {
        &self.constraints[expr]
    }
    fn constraints_mut(&mut self, expr: ExprId) -> &mut ConstraintList {
        &mut self.constraints[expr]
    }

    fn preferred_overload(&self, decl_ref: DeclRefId) -> Option<DeclId> {
        self.preferred_overloads[decl_ref]
    }
    fn preferred_overload_mut(&mut self, decl_ref: DeclRefId) -> &mut Option<DeclId> {
        &mut self.preferred_overloads[decl_ref]
    }
}