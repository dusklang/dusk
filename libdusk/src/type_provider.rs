//! The type provider is a (arguably poorly named) data structure that stores not only the selected type for each
//! expression in the program, but also a bunch of other stuff (see declare_tp_fields). Why does it exist at all,
//! instead of just storing this info on Driver? Because there are some occasions where the typechecker wants to make
//! queries about the program, such as which possible types an expression could have. Making these queries requires
//! making changes to the values in the type provider, which we don't want to do. So we have two kinds of type
//! providers, abstracted by the [TypeProvider] trait: [real](RealTypeProvider) and [mock](MockTypeProvider). There is
//! only one real type provider, which stores the real answers. A mock type provider wraps the real type provider and
//! provides a safe sandbox to temporarily make modifications that will soon be either thrown away, or in some cases
//! saved to the underlying type provider.

use std::mem;
use std::collections::HashMap;

use paste::paste;

use crate::ast::{ExprId, DeclId, DeclRefId, StructLitId, GenericParamId, TypeVarId, CastId, Namespace, PatternMatchingContextId};
use crate::mir::Const;
use crate::ty::{Type, QualType};
use crate::pattern_matching::{SwitchScrutineeValueId, TypedSwitchScrutineeValue, SwitchDecisionNode};
use crate::typechecker::{CastMethod, StructLit, constraints::ConstraintList, Overloads};
use crate::index_vec::*;
use crate::driver::Driver;
use crate::new_code::NewCode;
pub use crate::tir::MockStateCommand;

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
            /// The generic arguments for each decl ref
            generic_arguments generic_arguments: DeclRefId -> Option<Vec<Type>>,

            /// The decision tree generated for each pattern matching context
            switch_expr_decision_trees switch_expr_decision_tree: PatternMatchingContextId -> Option<SwitchDecisionNode>,

            /// The typed pattern matching context for each pattern matching context
            pattern_matching_contexts pattern_matching_context: PatternMatchingContextId -> Option<IndexVec<SwitchScrutineeValueId, TypedSwitchScrutineeValue>>,

            /// For each expression, a list of substitutions from generic params to type variable type.
            /// Currently only set on decl refs.
            generic_param_substitutions generic_param_substitutions: ExprId -> Option<HashMap<GenericParamId, Type>>,

            /// Each struct literal matched to a structure
            struct_lits struct_lit: StructLitId -> Option<StructLit>,
            /// The cast method for each cast expression
            cast_methods cast_method: CastId -> CastMethod,
            /// The type of each expression
            types ty: ExprId -> Type,
            /// The constraints on each type variable
            constraints constraints: TypeVarId -> ConstraintList,
            decl_types decl_type: DeclId -> QualType,
        }
    }
}
macro_rules! forward_mock {
    ($($doc:literal)* $field_name:ident, $fw_name:ident, $id_ty:ty, $val_ty:ty) => {
        $(#[doc=$doc])*
        fn $fw_name(&self, id: $id_ty) -> &$val_ty {
            let base = &*self.base;
            let val = self.$field_name.get(&id).map(|val| val).unwrap_or_else(|| base.$fw_name(id));
            val
        }
        $(#[doc=$doc])*
        paste! {
            fn [<$fw_name _mut>](&mut self, id: $id_ty) -> &mut $val_ty {
                let base = &*self.base;
                self.$field_name.entry(id).or_insert_with(|| base.$fw_name(id).clone())
            }
        }
        $(#[doc=$doc])*
        paste! {
            fn [<get_multiple_ $fw_name _mut>](&mut self, a_id: $id_ty, b_id: $id_ty) -> (&mut $val_ty, &mut $val_ty) {
                self.[<$fw_name _mut>](a_id);
                self.[<$fw_name _mut>](b_id);
                let [a, b] = self.$field_name.get_disjoint_mut([&a_id, &b_id]);
                return (a.unwrap(), b.unwrap());
            }
        }
    };
}
macro_rules! forward_save {
    ($salf:expr, $($doc:literal)* $field_name:ident, $fw_name:ident, $id_ty:ty, $val_ty:ty) => {{
        paste! {
            for (key, value) in mem::take(&mut $salf.$field_name) {
                *$salf.base.[<$fw_name _mut>](key) = value;
            }
        }
    }};
}
macro_rules! forward_real {
    ($($doc:literal)* $field_name:ident, $fw_name:ident, $id_ty:ty, $val_ty:ty) => {
        $(#[doc=$doc])*
        fn $fw_name(&self, id: $id_ty) -> &$val_ty {
            &self.$field_name[id]
        }
        $(#[doc=$doc])*
        paste! {
            fn [<$fw_name _mut>](&mut self, id: $id_ty) -> &mut $val_ty {
                &mut self.$field_name[id]
            }
        }
        $(#[doc=$doc])*
        paste! {
            fn [<get_multiple_ $fw_name _mut>](&mut self, a_id: $id_ty, b_id: $id_ty) -> (&mut $val_ty, &mut $val_ty) {
                self.$field_name.index_mut(a_id, b_id)
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
        paste! {
            $(#[doc=$doc])*
            fn [<get_multiple_ $fw_name _mut>](&mut self, a_id: $id_ty, b_id: $id_ty) -> (&mut $val_ty, &mut $val_ty);
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
            fn insert_eval_result(&mut self, expr: ExprId, result: Const);

            fn resize(&mut self, d: &Driver, new_code: NewCode);

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
            fn fetch_decl_type(&mut self, d: &Driver, id: DeclId, decl_ref: Option<DeclRefId>) -> QualType {
                if let Type::Error = self.decl_type(id).ty {
                    if let Some(decl_ref) = decl_ref {
                        let decl_ref = &d.code.ast.decl_refs[decl_ref];
                        match decl_ref.namespace {
                            Namespace::Guarantee(ns) if decl_ref.name == d.ast.known_idents.return_value => {
                                let func = d.code.ast.condition_ns[ns].func;
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
                let ty = d.decl_type(id, self);
                self.decl_type_mut(id).ty = ty;
            }

            fn is_mock(&self) -> bool;

            fn save(&mut self);
        }

        pub struct RealTypeProvider {
            $(
                $(#[doc = $doc])*
                $field_name: IndexVec<$id_ty, $payload_ty>,
            )*
            eval_results: HashMap<ExprId, Const>,
        }

        impl RealTypeProvider {
            pub fn new(d: &Driver) -> Self {
                let mut tp = RealTypeProvider {
                    $($field_name: IndexVec::new(),)*
                    eval_results: HashMap::new(),
                };

                tp.resize_impl(d, DeclId::new(0));

                tp
            }

            fn resize_impl(&mut self, d: &Driver, decl_start: DeclId) {
                macro_rules! resize_idx_vec {
                    ($fname:ident, DeclRefId) => {
                        self.$fname.resize_with(d.code.ast.decl_refs.len(), Default::default);
                    };
                    ($fname:ident, ExprId) => {
                        self.$fname.resize_with(d.code.ast.exprs.len(), Default::default);
                    };
                    ($fname:ident, TypeVarId) => {
                        self.$fname.resize_with(d.code.ast.type_vars.len(), Default::default);
                    };
                    ($fname:ident, DeclId) => {
                        self.$fname.resize_with(d.code.ast.decls.len(), Default::default);
                    };
                    ($fname:ident, StructLitId) => {
                        self.$fname.resize_with(d.code.ast.struct_lits.len(), Default::default);
                    };
                    ($fname:ident, CastId) => {
                        self.$fname.resize_with(d.code.ast.cast_counter.len(), Default::default);
                    };
                    ($fname:ident, StructId) => {
                        self.$fname.resize_with(d.code.ast.structs.len(), Default::default);
                    };
                    ($fname:ident, PatternMatchingContextId) => {
                        self.$fname.resize_with(d.code.ast.pattern_matching_contexts.len(), Default::default);
                    };
                }
                $(resize_idx_vec!($field_name, $id_ty);)*
                let end = self.decl_types.next_idx();
                for decl in range_iter(decl_start..end) {
                    self.decl_types[decl].is_mut = d.tir.decls[decl].is_mut;
                }
                for (decl, ty) in self.decl_types.iter_mut_enumerated() {
                    ty.is_mut = d.tir.decls[decl].is_mut;
                }
            }
        }

        impl private::Sealed for RealTypeProvider {}

        impl TypeProvider for RealTypeProvider {
            fn insert_eval_result(&mut self, expr: ExprId, result: Const) {
                self.eval_results.insert(expr, result);
            }

            fn resize(&mut self, d: &Driver, new_code: NewCode) {
                self.resize_impl(d, new_code.decls.start);
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
            fn get_multiple_eval_result_mut(&mut self, a_id: ExprId, b_id: ExprId) -> (&mut Const, &mut Const) {
                let [a, b] = self.eval_results.get_disjoint_mut([&a_id, &b_id]);
                (a.unwrap(), b.unwrap())
            }

            fn is_mock(&self) -> bool { false }

            fn save(&mut self) {}
        }

        pub struct MockTypeProvider<'base> {
            base: &'base mut dyn TypeProvider,

            $(
                $(#[doc = $doc])*
                $field_name: HashMap<$id_ty, $payload_ty>,
            )*

            eval_results: HashMap<ExprId, Const>,
        }

        impl<'base> MockTypeProvider<'base> {
            // base must ONLY be mutated in a resize or save operation
            pub fn new(base: &'base mut dyn TypeProvider) -> Self {
                MockTypeProvider {
                    base,
                    $($field_name: HashMap::new(),)*
                    eval_results: HashMap::new(),
                }
            }
        }

        impl private::Sealed for MockTypeProvider<'_> {}

        impl<'base> TypeProvider for MockTypeProvider<'base> {
            fn insert_eval_result(&mut self, expr: ExprId, result: Const) {
                self.eval_results.insert(expr, result);
            }

            fn resize(&mut self, d: &Driver, new_code: NewCode) {
                self.base.resize(d, new_code);
            }

            fn is_mock(&self) -> bool { true }

            $(
                forward_mock!($field_name, $fn_name, $id_ty, $payload_ty);
            )*
            forward_mock!(eval_results, eval_result, ExprId, Const);

            fn save(&mut self) {
                $(
                    forward_save!(self, $field_name, $fn_name, $id_ty, $payload_ty);
                )*
                for (key, value) in mem::take(&mut self.eval_results) {
                    self.base.insert_eval_result(key, value);
                }
            }
        }

    }
}
declare_tp_fields!(declare_tp);
