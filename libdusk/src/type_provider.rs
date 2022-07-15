//! The type provider is a (arguably poorly named) data structure that stores not only the selected type for each
//! expression in the program, but also a bunch of other stuff (see declare_tp_fields). Why does it exist at all,
//! instead of just storing this info on Driver? Because there are some occasions where the typechecker wants to make
//! queries about the program, such as which possible types an expression could have. Making these queries requires
//! making changes to the values in the type provider, which we don't want to do. So we have two kinds of type
//! providers, abstracted by the [TypeProvider] trait: [real](RealTypeProvider) and [mock](MockTypeProvider). There is
//! only one real type provider, which stores the real answers. A mock type provider wraps the real type provider and
//! provides a safe sandbox to temporarily make modifications to, then throw away.

use std::collections::HashMap;

use paste::paste;

use index_vec::IdxRangeBounds;

use dire::hir::{ExprId, DeclId, DeclRefId, StructLitId, CastId, Namespace, Decl};
use dire::mir::Const;
use dire::ty::{Type, FunctionType, QualType};
use dusk_proc_macros::df;

use crate::typechecker::{CastMethod, StructLit, constraints::ConstraintList, Overloads, GenericConstraints};
use crate::index_vec::*;
use crate::source_info::CommentatedSourceRange;
use crate::driver::Driver;
use crate::new_code::NewCode;

/// The body of this macro defines the fields of the type provider. Each one maps to all of the following:
/// - A field of type [`IndexVec<IdType, ValueType>`] in [RealTypeProvider]
/// - A field of type [`HashMap<IdType, ValueType>`] in [MockTypeProvider]
/// - Methods in the type provider trait for immutable and mutable access to a specified index of that field
/// - Code in the [RealTypeProvider] [initializer](`RealTypeProvider::new`) to resize each field's [IndexVec]
macro_rules! declare_tp_fields {
    ($receiver:ident) => {
        $receiver! {
            /// The list of overloads for each decl ref
            overloads overloads: DeclRefId -> Overloads,
            /// Whether each declref's overload list was affected by an error (e.g., accessing a field on a nonexistent variable)
            decl_ref_has_error decl_ref_has_error: DeclRefId -> bool,
            /// The selected overload for each decl ref
            selected_overloads selected_overload: DeclRefId -> Option<DeclId>,
            // The list of expressions that the given declref must iterate through to substitute its generic arguments into their types
            generic_substitution_list generic_substitution_list: DeclRefId -> Vec<ExprId>,
            /// The generic arguments for each decl ref
            generic_arguments generic_arguments: DeclRefId -> Option<Vec<Type>>,
            /// The constraints on all generic arguments that might get passed by each declref
            generic_constraints generic_constraints: DeclRefId -> GenericConstraints,
            /// Each struct literal matched to a structure
            struct_lits struct_lit: StructLitId -> Option<StructLit>,
            /// The cast method for each cast expression
            cast_methods cast_method: CastId -> CastMethod,
            /// The type of each expression
            types ty: ExprId -> Type,
            /// The constraints on each expression's type
            constraints constraints: ExprId -> ConstraintList,
            decl_types decl_type: DeclId -> QualType,
        }
    }
}
macro_rules! forward_mock {
    ($($doc:literal)* $field_name:ident, $fw_name:ident, $id_ty:ty, $val_ty:ty) => {
        $(#[doc=$doc])*
        fn $fw_name(&self, id: $id_ty) -> &$val_ty {
            let base = self.base;
            let val = self.$field_name.get(&id).map(|val| val).unwrap_or_else(|| base.$fw_name(id));
            val
        }
        $(#[doc=$doc])*
        paste! {
            fn [<$fw_name _mut>](&mut self, id: $id_ty) -> &mut $val_ty {
                let base = self.base;
                self.$field_name.entry(id).or_insert_with(|| base.$fw_name(id).clone())
            }
        }
    };
}
macro_rules! forward_real {
    ($($doc:literal)* $field_name:ident, $fw_name:ident, $id_ty:ty, $val_ty:ty) => {
        $(#[doc=$doc])*
        fn $fw_name(&self, id: $id_ty) -> &$val_ty {
            &self.$field_name[id]
        }
        paste! {
            $(#[doc=$doc])*
            fn [<$fw_name _mut>](&mut self, id: $id_ty) -> &mut $val_ty {
                &mut self.$field_name[id]
            }
        }
    };
}
macro_rules! forward_trait {
    ($($doc:literal)* $field_name:ident, $fw_name:ident, $id_ty:ty, $val_ty:ty) => {
        $(#[doc=$doc])*
        fn $fw_name(&self, id: $id_ty) -> &$val_ty;
        paste! {
            $(#[doc=$doc])*
            fn [<$fw_name _mut>](&mut self, id: $id_ty) -> &mut $val_ty;
        }
    };
}
mod private {
    pub trait Sealed {}
}
macro_rules! declare_tp {
    (
        $(
            $(#[doc=$doc:literal])*
            $field_name:ident $fn_name:ident: $id_ty:ident -> $payload_ty:ty
        ),*$(,)*
    ) => {
        pub trait TypeProvider: private::Sealed {
            fn debug(&self) -> bool;
            fn debug_output(&mut self, d: &Driver, level: usize);
        
            fn insert_eval_result(&mut self, expr: ExprId, result: Const);

            $(
                forward_trait!($($doc)* $field_name, $fn_name, $id_ty, $payload_ty);
            )*
            forward_trait!(eval_results, eval_result, ExprId, Const);
        
            /// Doesn't get the type *of* `id`, gets the type that `id` as an expression *is*
            fn get_evaluated_type(&self, id: ExprId) -> &Type {
                match self.eval_result(id) {
                    Const::Ty(ty) => ty,
                    x => panic!("Expected type! Found {:?}", x),
                }
            }
            fn multi_constraints_mut<'a>(&'a mut self, a: ExprId, b: ExprId) -> (&'a mut ConstraintList, &'a mut ConstraintList) {
                assert_ne!(a, b, "`a` ({:?}) must not equal `b` ({:?})", a, b);
                // Ensure both exist, in the case of MockTypeProvider
                self.constraints_mut(a);
                self.constraints_mut(b);
                unsafe {
                    let a = self.constraints_mut(a) as *mut _;
                    let b = self.constraints_mut(b) as *mut _;
                    (&mut *a, &mut *b)
                }
            }
            fn fetch_decl_type(&mut self, d: &Driver, id: DeclId, decl_ref: Option<DeclRefId>) -> QualType {
                if let Type::Error = self.decl_type(id).ty {
                    if let Some(decl_ref) = decl_ref {
                        let decl_ref = &d.code.hir.decl_refs[decl_ref];
                        match decl_ref.namespace {
                            Namespace::Guarantee(ns) if decl_ref.name == d.hir.known_idents.return_value => {
                                let func = d.code.hir.condition_ns[ns].func;
                                // This gets the return value because this declref refers to the return_value decl
                                return self.fetch_decl_type(d, func, None).ty.return_ty().unwrap().into();
                            }
                            _ => self.set_decl_type_to_explicit_type_if_exists(d, id),
                        }
                    } else {
                        self.set_decl_type_to_explicit_type_if_exists(d, id);
                    }
                }
        
                self.decl_type(id).clone()
            }
            #[doc(hidden)]
            fn set_decl_type_to_explicit_type_if_exists(&mut self, d: &Driver, id: DeclId) {
                let explicit_ty = d.code.hir.explicit_tys[id]
                    .map(|expr| self.get_evaluated_type(expr).clone());
                match &df!(d, id.hir) {
                    Decl::Computed { param_tys, .. } | Decl::ComputedPrototype { param_tys, .. } | Decl::Intrinsic { function_like: true, param_tys, .. } => {
                        let param_tys: Vec<_> = param_tys.iter().copied()
                            .map(|ty| self.get_evaluated_type(ty).clone())
                            .collect();
                        let return_ty = Box::new(explicit_ty.unwrap());
                        self.decl_type_mut(id).ty = Type::Function(FunctionType { param_tys, return_ty });
                    }
                    _ => if let Some(explicit_ty) = explicit_ty {
                        self.decl_type_mut(id).ty = explicit_ty;
                    }
                }
            }
        }
        
        pub struct RealTypeProvider {
            $(
                $(#[doc = $doc])*
                $field_name: IndexVec<$id_ty, $payload_ty>,
            )*
            /// A copy of the constraints, used for debugging the typechecker
            constraints_copy: IndexVec<ExprId, ConstraintList>,
        
            eval_results: HashMap<ExprId, Const>,
        
            debug: bool,
        }
        
        impl RealTypeProvider {
            pub fn new(debug: bool, d: &Driver) -> Self {
                let mut tp = RealTypeProvider {
                    $($field_name: IndexVec::new(),)*
                    constraints_copy: IndexVec::new(),
                    eval_results: HashMap::new(),
                    
                    debug,
                };

                tp.resize_impl(d, DeclId::new(0));
                
                tp
            }

            fn resize_impl(&mut self, d: &Driver, decl_start: DeclId) {
                macro_rules! resize_idx_vec {
                    ($fname:ident, DeclRefId) => {
                        self.$fname.resize_with(d.code.hir.decl_refs.len(), Default::default);
                    };
                    ($fname:ident, ExprId) => {
                        self.$fname.resize_with(d.code.hir.exprs.len(), Default::default);
                    };
                    ($fname:ident, DeclId) => {
                        self.$fname.resize_with(d.code.hir.decls.len(), Default::default);
                    };
                    ($fname:ident, StructLitId) => {
                        self.$fname.resize_with(d.code.hir.struct_lits.len(), Default::default);
                    };
                    ($fname:ident, CastId) => {
                        self.$fname.resize_with(d.code.hir.cast_counter.len(), Default::default);
                    };
                }
                $(resize_idx_vec!($field_name, $id_ty);)*
                if self.debug {
                    self.constraints_copy.resize_with(d.code.hir.exprs.len(), Default::default);
                }
                for i in (decl_start..self.decl_types.next_idx()).into_range() {
                    let decl = DeclId::new(i);
                    self.decl_types[decl].is_mut = d.tir.decls[decl].is_mut;
                }
                for (decl, ty) in self.decl_types.iter_mut_enumerated() {
                    ty.is_mut = d.tir.decls[decl].is_mut;
                }
            }

            pub fn resize(&mut self, d: &Driver, new_code: NewCode) {
                self.resize_impl(d, new_code.decls.start);
            }
        }
        
        impl Driver {
            fn print_debug_diff_and_set_old_constraints(&self, id: ExprId, old_constraints: &mut ConstraintList, new_constraints: &ConstraintList) {
                if new_constraints != old_constraints {
                    self.src_map.print_commentated_source_ranges(&mut [
                        CommentatedSourceRange::new(self.get_range(id), "", '-')
                    ]);
                    old_constraints.print_diff(new_constraints);
                    *old_constraints = new_constraints.clone();
                    println!("============================================================================================\n")
                }
            }
        }
        
        impl private::Sealed for RealTypeProvider {}
        
        impl TypeProvider for RealTypeProvider {
            fn debug(&self) -> bool { self.debug }
        
            fn debug_output(&mut self, d: &Driver, level: usize) {
                if !self.debug { return; }
                println!("LEVEL {}", level);
                assert_eq!(self.constraints.len(), self.constraints_copy.len());
                for i in 0..self.constraints.len() {
                    let id = ExprId::new(i);
                    d.print_debug_diff_and_set_old_constraints(id, &mut self.constraints_copy[id], &self.constraints[id]);
                }
            }
        
            fn insert_eval_result(&mut self, expr: ExprId, result: Const) {
                self.eval_results.insert(expr, result);
            }

            $(
                forward_real!($field_name, $fn_name, $id_ty, $payload_ty);
            )*
        
            fn eval_result(&self, id: ExprId) -> &Const {
                &self.eval_results[&id]
            }
            fn eval_result_mut(&mut self, id: ExprId) -> &mut Const {
                self.eval_results.get_mut(&id).unwrap()
            }
        }
        
        pub struct MockTypeProvider<'base> {
            base: &'base dyn TypeProvider,

            $(
                $(#[doc = $doc])*
                $field_name: HashMap<$id_ty, $payload_ty>,
            )*

            /// A copy of the constraints, used for debugging the typechecker
            constraints_copy: HashMap<ExprId, ConstraintList>,
        
            eval_results: HashMap<ExprId, Const>,
        }

        impl<'base> MockTypeProvider<'base> {
            pub fn new(base: &'base dyn TypeProvider) -> Self {
                MockTypeProvider {
                    base,
                    $($field_name: HashMap::new(),)*
                    constraints_copy: HashMap::new(),
                    eval_results: HashMap::new(),
                }
            }
        }

        impl private::Sealed for MockTypeProvider<'_> {}

        impl<'base> TypeProvider for MockTypeProvider<'base> {
            fn debug(&self) -> bool { self.base.debug() }

            fn debug_output(&mut self, d: &Driver, level: usize) {
                if !self.debug() { return; }
                println!("LEVEL {}", level);
                let base = self.base;
                for (&id, new_constraints) in &self.constraints {
                    let old_constraints = self.constraints_copy.entry(id)
                        .or_insert_with(|| base.constraints(id).clone());
                    d.print_debug_diff_and_set_old_constraints(id, old_constraints, new_constraints);
                }
            }

            fn insert_eval_result(&mut self, expr: ExprId, result: Const) {
                self.eval_results.insert(expr, result);
            }

            $(
                forward_mock!($field_name, $fn_name, $id_ty, $payload_ty);
            )*
            forward_mock!(eval_results, eval_result, ExprId, Const);
        }

    }
}
declare_tp_fields!(declare_tp);