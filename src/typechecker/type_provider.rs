use std::collections::HashMap;

use crate::builder::{ExprId, DeclId, DeclRefId, CastId};
use crate::ty::{Type, QualType};
use super::{CastMethod, constraints::ConstraintList};
use crate::{hir, tir};
use crate::mir::Const;
use crate::index_vec::{IdxVec, Idx};
use crate::source_info::{SourceMap, CommentatedSourceRange};

mod private {
    pub trait Sealed {}
}

pub trait TypeProvider: private::Sealed {
    fn debug(&self) -> bool;
    fn debug_output(&mut self, hir: &hir::Builder, map: &SourceMap, level: usize);

    fn ty(&self, expr: ExprId) -> &Type;
    fn ty_mut(&mut self, expr: ExprId) -> &mut Type;

    fn overloads(&self, decl_ref: DeclRefId) -> &Vec<DeclId>;
    fn overloads_mut(&mut self, decl_ref: DeclRefId) -> &mut Vec<DeclId>;

    fn selected_overload(&self, decl_ref: DeclRefId) -> Option<DeclId>;
    fn selected_overload_mut(&mut self, decl_ref: DeclRefId) -> &mut Option<DeclId>;

    fn cast_method(&self, cast: CastId) -> CastMethod;
    fn cast_method_mut(&mut self, cast: CastId) -> &mut CastMethod;

    fn constraints(&self, expr: ExprId) -> &ConstraintList;
    fn constraints_mut(&mut self, expr: ExprId) -> &mut ConstraintList;
    fn multi_constraints_mut(&mut self, a: ExprId, b: ExprId) -> (&mut ConstraintList, &mut ConstraintList);

    fn preferred_overload(&self, decl_ref: DeclRefId) -> Option<DeclId>;
    fn preferred_overload_mut(&mut self, decl_ref: DeclRefId) -> &mut Option<DeclId>;

    fn insert_eval_result(&mut self, expr: ExprId, result: Const);
    /// Doesn't get the type *of* `id`, gets the type that `id` as an expression *is*
    fn get_evaluated_type(&self, id: ExprId) -> &Type;

    fn fetch_decl_type(&mut self, hir: &hir::Builder, id: DeclId) -> &QualType;
    fn decl_type_mut(&mut self, decl: DeclId) -> &mut QualType;

    #[doc(hidden)]
    fn fw_decl_types(&self, id: DeclId) -> &QualType;
    #[doc(hidden)]
    fn fw_decl_types_mut(&mut self, id: DeclId) -> &mut QualType;

    #[doc(hidden)]
    fn fw_eval_results(&self, id: ExprId) -> &Const;
    #[doc(hidden)]
    fn fw_eval_results_mut(&mut self, id: ExprId) -> &mut Const;
}

pub struct RealTypeProvider {
    /// The type of each expression
    types: IdxVec<ExprId, Type>,
    /// The list of overloads for each decl ref
    overloads: IdxVec<DeclRefId, Vec<DeclId>>,
    /// The selected overload for each decl ref
    selected_overloads: IdxVec<DeclRefId, Option<DeclId>>,
    /// The cast method for each cast expression
    cast_methods: IdxVec<CastId, CastMethod>,
    /// The constraints on each expression's type
    constraints: IdxVec<ExprId, ConstraintList>,
    /// A copy of the constraints, used for debugging the typechecker
    constraints_copy: IdxVec<ExprId, ConstraintList>,
    /// The preferred overload for each decl ref (currently only ever originates from literals)
    preferred_overloads: IdxVec<DeclRefId, Option<DeclId>>,

    decl_types: IdxVec<DeclId, QualType>,

    eval_results: HashMap<ExprId, Const>,

    debug: bool,
}

impl RealTypeProvider {
    pub fn new(debug: bool, hir: &hir::Builder, tir: &tir::Builder) -> Self {
        let mut tp = RealTypeProvider {
            types: IdxVec::new(),
            overloads: IdxVec::new(),
            selected_overloads: IdxVec::new(),
            cast_methods: IdxVec::new(),
            constraints: IdxVec::new(),
            constraints_copy: IdxVec::new(),
            preferred_overloads: IdxVec::new(),
            
            decl_types: IdxVec::new(),
            
            eval_results: HashMap::new(),
            
            debug,
        };
        tp.overloads.resize_with(hir.decl_refs.len(), Default::default);
        tp.types.resize_with(hir.exprs.len(), Default::default);
        tp.constraints.resize_with(hir.exprs.len(), Default::default);
        if debug {
            tp.constraints_copy.resize_with(hir.exprs.len(), Default::default);
        }
        tp.selected_overloads.resize_with(hir.decl_refs.len(), || None);
        tp.preferred_overloads.resize_with(hir.decl_refs.len(), || None);
        tp.cast_methods.resize_with(hir.cast_counter.len(), || CastMethod::Noop);
        
        for i in 0..tir.decls.len() {
            let id = DeclId::new(i);
            let is_mut = tir.decls[id].is_mut;
            tp.decl_types.push(QualType { ty: Type::Error, is_mut });
        }
        
        tp
    }
}

fn print_debug_diff_and_set_old_constraints(id: ExprId, old_constraints: &mut ConstraintList, new_constraints: &ConstraintList, hir: &hir::Builder, map: &SourceMap) {
    if new_constraints != old_constraints {
        map.print_commentated_source_ranges(&mut [
            CommentatedSourceRange::new(hir.get_range(id), "", '-')
        ]);
        old_constraints.print_diff(new_constraints);
        *old_constraints = new_constraints.clone();
        println!("============================================================================================\n")
    }
}

impl private::Sealed for RealTypeProvider {}

impl TypeProvider for RealTypeProvider {
    fn debug(&self) -> bool { self.debug }

    fn debug_output(&mut self, hir: &hir::Builder, map: &SourceMap, level: usize) {
        if !self.debug { return; }
        println!("LEVEL {}", level);
        assert_eq!(self.constraints.len(), self.constraints_copy.len());
        for i in 0..self.constraints.len() {
            let id = ExprId::new(i);
            print_debug_diff_and_set_old_constraints(id, &mut self.constraints_copy[id], &self.constraints[id], hir, map);
        }
    }

    fn insert_eval_result(&mut self, expr: ExprId, result: Const) {
        self.eval_results.insert(expr, result);
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

    fn overloads(&self, decl_ref: DeclRefId) -> &Vec<DeclId> {
        &self.overloads[decl_ref]
    }
    fn overloads_mut(&mut self, decl_ref: DeclRefId) -> &mut Vec<DeclId> {
        &mut self.overloads[decl_ref]
    }

    fn selected_overload(&self, decl_ref: DeclRefId) -> Option<DeclId> {
        self.selected_overloads[decl_ref]
    }
    fn selected_overload_mut(&mut self, decl_ref: DeclRefId) -> &mut Option<DeclId> {
        &mut self.selected_overloads[decl_ref]
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
    fn multi_constraints_mut(&mut self, a: ExprId, b: ExprId) -> (&mut ConstraintList, &mut ConstraintList) {
        self.constraints.index_mut(a, b)
    }

    fn preferred_overload(&self, decl_ref: DeclRefId) -> Option<DeclId> {
        self.preferred_overloads[decl_ref]
    }
    fn preferred_overload_mut(&mut self, decl_ref: DeclRefId) -> &mut Option<DeclId> {
        &mut self.preferred_overloads[decl_ref]
    }

    fn fw_decl_types(&self, id: DeclId) -> &QualType {
        &self.decl_types[id]
    }

    fn fw_decl_types_mut(&mut self, id: DeclId) -> &mut QualType {
        &mut self.decl_types[id]
    }


    fn fw_eval_results(&self, id: ExprId) -> &Const {
        &self.eval_results[&id]
    }

    fn fw_eval_results_mut(&mut self, id: ExprId) -> &mut Const {
        self.eval_results.entry(id).or_insert(Const::Ty(Type::Error))
    }
}

pub struct MockTypeProvider<'base> {
    base: &'base dyn TypeProvider,

    /// The type of each expression
    types: HashMap<ExprId, Type>,
    /// The list of overloads for each decl ref
    overloads: HashMap<DeclRefId, Vec<DeclId>>,
    /// The selected overload for each decl ref
    selected_overloads: HashMap<DeclRefId, Option<DeclId>>,
    /// The cast method for each cast expression
    cast_methods: HashMap<CastId, CastMethod>,
    /// The constraints on each expression's type
    constraints: HashMap<ExprId, ConstraintList>,
    /// A copy of the constraints, used for debugging the typechecker
    constraints_copy: HashMap<ExprId, ConstraintList>,
    /// The preferred overload for each decl ref (currently only ever originates from literals)
    preferred_overloads: HashMap<DeclRefId, Option<DeclId>>,

    decl_types: HashMap<DeclId, QualType>,

    eval_results: HashMap<ExprId, Const>,
}

macro_rules! deref {
    (val: $expr:expr) => {{*$expr}};
    (type: $ty:ty) => {$ty};
}

macro_rules! no_deref {
    (val: $expr:expr) => {{$expr}};
    (type: $ty:ty) => {&$ty};
}

macro_rules! no_addr {
    ($id:expr) => {{$id}}
}

macro_rules! forward_mock {
    ($field_name:ident, $fw_name:ident, $fw_name_mut:ident, $id_ty:ty, $val_ty:ty, $addr:ident, $deref:ident) => {
        fn $fw_name(&self, id: $id_ty) -> $deref!(type: $val_ty) {
            let base = self.base;
            let val = self.$field_name.get(&id).map(|val| $deref!(val: val)).unwrap_or_else(|| base.$fw_name($addr!(id)));
            val
        }

        fn $fw_name_mut(&mut self, id: $id_ty) -> &mut $val_ty {
            let base = self.base;
            self.$field_name.entry(id).or_insert_with(|| base.$fw_name($addr!(id)).clone())
        }
    };
    ($field_name:ident, $fw_name:ident, $fw_name_mut:ident, $id_ty:ty, $val_ty:ty) => {
        forward_mock!($field_name, $fw_name, $fw_name_mut, $id_ty, $val_ty, no_addr, no_deref);
    };
    ($field_name:ident, $fw_name:ident, $fw_name_mut:ident, $id_ty:ty, $val_ty:ty, addr: $addr:ident) => {
        forward_mock!($field_name, $fw_name, $fw_name_mut, $id_ty, $val_ty, $addr, no_deref);
    };
    ($field_name:ident, $fw_name:ident, $fw_name_mut:ident, $id_ty:ty, $val_ty:ty, deref: $deref:ident) => {
        forward_mock!($field_name, $fw_name, $fw_name_mut, $id_ty, $val_ty, no_addr, $deref);
    };
}

impl<'base> MockTypeProvider<'base> {
    pub fn new(base: &'base dyn TypeProvider) -> Self {
        MockTypeProvider {
            base,
            types: HashMap::new(),
            overloads: HashMap::new(),
            selected_overloads: HashMap::new(),
            cast_methods: HashMap::new(),
            constraints: HashMap::new(),
            constraints_copy: HashMap::new(),
            preferred_overloads: HashMap::new(),
            decl_types: HashMap::new(),
            eval_results: HashMap::new(),
        }
    }
}

impl private::Sealed for MockTypeProvider<'_> {}

impl<'base> TypeProvider for MockTypeProvider<'base> {
    fn debug(&self) -> bool { self.base.debug() }

    fn debug_output(&mut self, hir: &hir::Builder, map: &SourceMap, level: usize) {
        if !self.debug() { return; }
        println!("LEVEL {}", level);
        let base = self.base;
        for (&id, new_constraints) in &self.constraints {
            let old_constraints = self.constraints_copy.entry(id)
                .or_insert_with(|| base.constraints(id).clone());
            print_debug_diff_and_set_old_constraints(id, old_constraints, new_constraints, hir, map);
        }
    }

    fn insert_eval_result(&mut self, expr: ExprId, result: Const) {
        self.eval_results.insert(expr, result);
    }
    fn get_evaluated_type(&self, id: ExprId) -> &Type {
        match self.fw_eval_results(id) {
            Const::Ty(ty) => ty,
            x => panic!("Expected type! Found {:?}", x),
        }
    }

    fn fetch_decl_type(&mut self, hir: &hir::Builder, id: DeclId) -> &QualType {
        if let Type::Error = self.fw_decl_types(id).ty {
            if let Some(expr) = hir.explicit_tys[id] {
                let ty = self.get_evaluated_type(expr).clone();
                self.fw_decl_types_mut(id).ty = ty;
            }
        }

        self.fw_decl_types(id)
    }

    fn decl_type_mut(&mut self, decl: DeclId) -> &mut QualType {
        self.fw_decl_types_mut(decl)
    }

    forward_mock!(overloads, overloads, overloads_mut, DeclRefId, Vec<DeclId>);
    forward_mock!(selected_overloads, selected_overload, selected_overload_mut, DeclRefId, Option<DeclId>, deref: deref);
    forward_mock!(cast_methods, cast_method, cast_method_mut, CastId, CastMethod, deref: deref);
    forward_mock!(types, ty, ty_mut, ExprId, Type);
    forward_mock!(constraints, constraints, constraints_mut, ExprId, ConstraintList);
    forward_mock!(preferred_overloads, preferred_overload, preferred_overload_mut, DeclRefId, Option<DeclId>, deref: deref);
    forward_mock!(decl_types, fw_decl_types, fw_decl_types_mut, DeclId, QualType);
    forward_mock!(eval_results, fw_eval_results, fw_eval_results_mut, ExprId, Const);

    fn multi_constraints_mut<'a>(&'a mut self, a: ExprId, b: ExprId) -> (&'a mut ConstraintList, &'a mut ConstraintList) {
        unsafe {
            assert_ne!(a, b, "`a` ({:?}) must not equal `b` ({:?})", a, b);
            let base = self.base;

            // Insert b, then insert a and hold on to the reference, then get a reference to what we inserted to b.
            // I think (but don't know) that this is necessary, due to potential iterator invalidation.
            self.constraints.entry(b).or_insert_with(|| base.constraints(b).clone());
            let a = self.constraints.entry(a).or_insert_with(|| base.constraints(a).clone()) as *mut _;
            let b = self.constraints.get_mut(&b).unwrap() as *mut _;
            (&mut *a, &mut *b)
        }
    }
}
