use std::collections::HashMap;

use smallvec::{SmallVec, smallvec};

mod constraints;
pub mod type_provider;

use constraints::{ConstraintList, UnificationError};
use type_provider::TypeProvider;

use crate::source_info::SourceFile;
use crate::driver::Driver;
use crate::error::Error;
use crate::builder::{ExprId, DeclId, DeclRefId, CastId};
use crate::index_vec::Idx;
use crate::ty::{BuiltinTraits, Type, QualType, IntWidth};
use crate::index_vec::IdxVec;
use crate::dep_vec;
use crate::source_info::CommentatedSourceRange;
use crate::mir::Const;
use crate::hir;
use crate::tir::{UnitId, Unit};

#[derive(Copy, Clone, Debug)]
pub enum CastMethod {
    Noop,
    Reinterpret,
    Int,
    Float,
    FloatToInt,
    IntToFloat,
}

pub struct TypeChecker {
    /// The type of each expression
    pub types: IdxVec<Type, ExprId>,
    /// The selected overload for each decl ref
    pub overloads: IdxVec<Option<DeclId>, DeclRefId>,
    /// The cast method for each cast expression
    pub cast_methods: IdxVec<CastMethod, CastId>,
    /// The constraints on each expression's type
    constraints: IdxVec<ConstraintList, ExprId>,
    /// A copy of the constraints, used for debugging the typechecker
    constraints_copy: IdxVec<ConstraintList, ExprId>,
    /// The preferred overload for each decl ref (currently only ever originates from literals)
    preferred_overloads: IdxVec<Option<DeclId>, DeclRefId>,

    decl_types: IdxVec<QualType, DeclId>,

    eval_results: HashMap<ExprId, Const>,

    debug: bool,
}


impl TypeChecker {
    pub fn new(debug: bool) -> Self {
        Self {
            types: IdxVec::new(),
            overloads: IdxVec::new(),
            cast_methods: IdxVec::new(),
            constraints: IdxVec::new(),
            constraints_copy: IdxVec::new(),
            preferred_overloads: IdxVec::new(),

            decl_types: IdxVec::new(),

            eval_results: HashMap::new(),

            debug,
        }
    }
}

enum UnitRef<'a> {
    Id(UnitId),
    Ref(&'a mut Unit),
}

impl Driver {
    pub fn decl_type(&self, id: DeclId) -> &Type {
        self.hir.explicit_tys[id].map(|ty| self.tc.get_evaluated_type(ty)).unwrap_or(&Type::Error)
    }

    fn run_pass_1(&mut self, unit: UnitRef) -> u32 {
        let unit = match unit {
            UnitRef::Id(uid) => &mut self.tir.units[uid],
            UnitRef::Ref(unit) => unit,
        };

        let levels = dep_vec::unify_sizes(&mut [
            &mut unit.assigned_decls, &mut unit.assignments, &mut unit.decl_refs, 
            &mut unit.addr_ofs, &mut unit.derefs, &mut unit.pointers, &mut unit.ifs,
            &mut unit.dos, &mut unit.ret_groups, &mut unit.casts, &mut unit.whiles,
            &mut unit.explicit_rets, &mut unit.modules,
        ]);

        // Pass 1: propagate info down from leaves to roots
        if self.tc.debug { println!("===============TYPECHECKING: PASS 1==============="); }
        fn lit_pass_1(tp: &mut impl TypeProvider, lits: &[ExprId], trait_impls: BuiltinTraits, pref: Type) {
            for &item in lits {
                *tp.constraints_mut(item) = ConstraintList::new(
                    trait_impls, 
                    None,
                    Some(pref.clone().into())
                );
            }
        }
        lit_pass_1(&mut self.tc, &unit.int_lits, BuiltinTraits::INT, Type::i32());
        lit_pass_1(&mut self.tc, &unit.dec_lits, BuiltinTraits::DEC, Type::i32());
        lit_pass_1(&mut self.tc, &unit.str_lits, BuiltinTraits::STR, Type::u8().ptr());
        lit_pass_1(&mut self.tc, &unit.char_lits, BuiltinTraits::CHAR, Type::u8().ptr());
        for &item in &unit.const_tys {
            *self.tc.constraints_mut(item) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Ty.into()]), None);
            *self.tc.ty_mut(item) = Type::Ty;
        }
        for level in 0..levels {
            for item in unit.assigned_decls.get_level(level) {

                let constraints = self.tc.constraints(item.root_expr);
                let ty = if let &Some(explicit_ty) = &item.explicit_ty {
                    let explicit_ty = self.tc.get_evaluated_type(explicit_ty).clone();
                    if let Some(err) = constraints.can_unify_to(&explicit_ty.clone().into()).err() {
                        let range = self.hir.get_range(item.root_expr);
                        let mut error = Error::new(format!("Couldn't unify expression to assigned decl type `{:?}`", explicit_ty))
                            .adding_primary_range(range.clone(), "expression here");
                        match err {
                            UnificationError::InvalidChoice(choices)
                                => error.add_secondary_range(range, format!("note: expression could've unified to any of {:?}", choices)),
                            UnificationError::Trait(not_implemented)
                                => error.add_secondary_range(
                                    range,
                                    format!(
                                        "note: couldn't unify because expression requires implementations of {:?}",
                                        not_implemented.names(),
                                    ),
                                ),
                        }
                        self.errors.push(error);
                    }
                    explicit_ty
                } else {
                    constraints.solve().expect("Ambiguous type for assigned declaration").ty
                };
                self.tc.decl_type_mut(item.decl_id).ty = ty;
            }
            for item in unit.assignments.get_level(level) {
                self.tc.constraints_mut(item.id).set_to(Type::Void);
                *self.tc.ty_mut(item.id) = Type::Void;
            }
            for item in unit.casts.get_level(level) {
                let ty = self.tc.get_evaluated_type(item.ty).clone();
                *self.tc.constraints_mut(item.id) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![ty.clone().into()]), None);
                *self.tc.ty_mut(item.id) = ty;
            }
            for item in unit.whiles.get_level(level) {
                *self.tc.constraints_mut(item.id) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Void.into()]), None);
                *self.tc.ty_mut(item.id) = Type::Void;
            }
            for &item in unit.explicit_rets.get_level(level) {
                *self.tc.constraints_mut(item) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Never.into()]), None);
                *self.tc.ty_mut(item) = Type::Never;
            }
            for &item in unit.modules.get_level(level) {
                *self.tc.constraints_mut(item) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Mod.into()]), None);
                *self.tc.ty_mut(item) = Type::Mod;
            }
            for i in 0..unit.decl_refs.level_len(level) {
                let item = unit.decl_refs.at(level, i);
                let id = item.id;
                let args = item.args.clone();
                let decl_ref_id = item.decl_ref_id;

                // Filter overloads that don't match the constraints of the parameters.
                // These borrows are only here because the borrow checker is dumb
                let decls = &self.tir.decls;
                let tc = &self.tc;
                // Rule out overloads that don't match the arguments
                self.tir.overloads[decl_ref_id].retain(|&overload| {
                    assert_eq!(decls[overload].param_tys.len(), args.len());
                    let arg_constraints = args.iter().map(|&arg| tc.constraints(arg));
                    let param_tys = decls[overload].param_tys.iter().map(|&expr| tc.get_evaluated_type(expr));
                    for (constraints, ty) in arg_constraints.zip(param_tys) {
                        if constraints.can_unify_to(&ty.into()).is_err() { return false; }
                    }
                    true
                });

                let mut one_of = SmallVec::new();
                one_of.reserve(self.tir.overloads[decl_ref_id].len());
                for i in 0..self.tir.overloads[decl_ref_id].len() {
                    let overload = self.tir.overloads[decl_ref_id][i];
                    let ty = self.tc.fetch_decl_type(&self.hir, overload).ty.clone();
                    let is_mut = self.tir.decls[overload].is_mut;
                    one_of.push(QualType { ty, is_mut });
                }
                let mut pref = None;
                'find_preference: for (i, &arg) in args.iter().enumerate() {
                    if let Some(ty) = self.tc.constraints(arg).preferred_type() {
                        for &overload in &self.tir.overloads[decl_ref_id] {
                            let decl = &self.tir.decls[overload];
                            if ty.ty.trivially_convertible_to(self.tc.get_evaluated_type(decl.param_tys[i])) {
                                let ty = self.tc.fetch_decl_type(&self.hir, overload).clone();
                                pref = Some(ty);
                                *self.tc.preferred_overload_mut(decl_ref_id) = Some(overload);
                                break 'find_preference;
                            }
                        }
                    }
                }
                *self.tc.constraints_mut(id) = ConstraintList::new(BuiltinTraits::empty(), Some(one_of), pref);
            }
            for item in unit.addr_ofs.get_level(level) {
                let constraints = self.tc.constraints(item.expr).filter_map(|ty| {
                    if item.is_mut && !ty.is_mut { return None; }
                    Some(
                        QualType::from(
                            ty.ty.clone().ptr_with_mut(item.is_mut)
                        )
                    )
                });
                *self.tc.constraints_mut(item.id) = constraints;
            }
            for item in unit.derefs.get_level(level) {
                let constraints = self.tc.constraints(item.expr).filter_map(|ty| {
                    if let Type::Pointer(pointee) = &ty.ty {
                        Some(pointee.as_ref().clone())
                    } else {
                        None
                    }
                });
                *self.tc.constraints_mut(item.id) = constraints;
            }
            for item in unit.pointers.get_level(level) {
                if let Some(err) = self.tc.constraints(item.expr).can_unify_to(&Type::Ty.into()).err() {
                    let mut error = Error::new("Expected type operand to pointer operator");
                    let range = self.hir.get_range(item.expr);
                    match err {
                        UnificationError::InvalidChoice(choices)
                            => error.add_secondary_range(range, format!("note: expression could've unified to any of {:?}", choices)),
                        UnificationError::Trait(not_implemented)
                            => error.add_secondary_range(
                                range,
                                format!(
                                    "note: couldn't unify because expression requires implementations of {:?}",
                                    not_implemented.names(),
                                ),
                            ),
                    }
                    self.errors.push(error);
                }
                self.tc.constraints_mut(item.id).set_to(Type::Ty);
            }
            for item in unit.ifs.get_level(level) {
                if let Some(err) = self.tc.constraints(item.condition).can_unify_to(&Type::Bool.into()).err() {
                    let mut error = Error::new("Expected boolean condition in if expression");
                    let range = self.hir.get_range(item.condition);
                    match err {
                        UnificationError::InvalidChoice(choices)
                            => error.add_secondary_range(range, format!("note: expression could've unified to any of {:?}", choices)),
                        UnificationError::Trait(not_implemented)
                            => error.add_secondary_range(
                                range,
                                format!(
                                    "note: couldn't unify because expression requires implementations of {:?}",
                                    not_implemented.names(),
                                ),
                            ),
                    }
                    self.errors.push(error);
                }
                let constraints = self.tc.constraints(item.then_expr).intersect_with(self.tc.constraints(item.else_expr));

                if constraints.solve().is_err() {
                    // TODO: handle void expressions, which don't have appropriate source location info.
                    self.errors.push(
                        Error::new("Failed to unify branches of if expression")
                            .adding_primary_range(self.hir.get_range(item.then_expr), "first terminal expression here")
                            .adding_primary_range(self.hir.get_range(item.else_expr), "second terminal expression here")
                    );
                }
                *self.tc.constraints_mut(item.id) = constraints;
            }
            for item in unit.dos.get_level(level) {
                *self.tc.constraints_mut(item.id) = self.tc.constraints(item.terminal_expr).clone();
            }
            self.tc.debug_output(&self.hir, &self.file, level as usize);
        }

        levels
    }

    fn run_pass_2(&mut self, unit: UnitRef, levels: u32) {
        let unit = match unit {
            UnitRef::Id(uid) => &mut self.tir.units[uid],
            UnitRef::Ref(unit) => unit,
        };

        if self.tc.debug { println!("===============TYPECHECKING: PASS 2==============="); }
        for level in (0..levels).rev() {
            for i in 0..unit.assigned_decls.level_len(level) {
                let item = unit.assigned_decls.at(level, i);
                let decl_id = item.decl_id;
                let root_expr = item.root_expr;
                let ty = self.tc.fetch_decl_type(&self.hir, decl_id).ty.clone();
                self.tc.constraints_mut(root_expr).set_to(ty);
            }
            for item in unit.ret_groups.get_level(level) {
                let ty = self.tc.get_evaluated_type(item.ty).clone();
                for &expr in &item.exprs {
                    if let Some(err) = self.tc.constraints(expr).can_unify_to(&QualType::from(&ty)).err() {
                        let range = self.hir.get_range(expr);
                        let mut error = Error::new(format!("can't unify expression to return type {:?}", ty))
                            .adding_primary_range(range.clone(), "expression here");
                        match err {
                            UnificationError::InvalidChoice(choices)
                                => error.add_secondary_range(range, format!("note: expression could've unified to any of {:?}", choices)),
                            UnificationError::Trait(not_implemented)
                                => error.add_secondary_range(
                                    range,
                                    format!(
                                        "note: couldn't unify because expression requires implementations of {:?}",
                                        not_implemented.names(),
                                    ),
                                ),
                        }
                        self.errors.push(error);
                    }

                    // Assume we panic above unless the returned expr can unify to the return type
                    self.tc.constraints_mut(expr).set_to(ty.clone());
                }
            }
            for item in unit.whiles.get_level(level) {
                if self.tc.constraints(item.condition).can_unify_to(&Type::Bool.into()).is_ok() {
                    self.tc.constraints_mut(item.condition).set_to(Type::Bool);
                } else {
                    panic!("Expected boolean condition in while expression");
                }
            }
            for item in unit.casts.get_level(level) {
                let ty = self.tc.get_evaluated_type(item.ty).clone();
                let constraints = self.tc.constraints_mut(item.expr);
                let ty_and_method: Result<(Type, CastMethod), Vec<&QualType>> = if constraints.can_unify_to(&QualType::from(&ty)).is_ok() {
                    Ok((ty, CastMethod::Noop))
                } else if let Type::Pointer(dest_pointee_ty) = ty {
                    let dest_pointee_ty = dest_pointee_ty.as_ref();
                    let src_ty = constraints.max_ranked_type(|ty|
                        match ty.ty {
                            Type::Pointer(ref pointee) if pointee.is_mut || dest_pointee_ty.is_mut => 2,
                            Type::Int { width, .. } if width == IntWidth::Pointer => 1,
                            _ => 0,
                        }
                    ).expect("Invalid cast!").clone();
                    Ok((src_ty.ty.clone(), CastMethod::Reinterpret))
                } else if let Type::Int { width, .. } = ty {
                    constraints.max_ranked_type_with_assoc_data(|ty|
                        match ty.ty {
                            Type::Int { .. } => (3, CastMethod::Int),
                            Type::Float { .. } => (2, CastMethod::FloatToInt),
                            Type::Pointer(_) if width == IntWidth::Pointer => (1, CastMethod::Reinterpret),
                            _ => (0, CastMethod::Noop),
                        }
                    ).map(|(ty, method)| (ty.ty.clone(), method))
                    .map_err(|options| options.iter().map(|(ty, _)| ty.clone()).collect())
                } else if let Type::Float { .. } = ty {
                    constraints.max_ranked_type_with_assoc_data(|ty|
                        match ty.ty {
                            Type::Float { .. } => (2, CastMethod::Float),
                            Type::Int { .. } => (1, CastMethod::IntToFloat),
                            _ => (0, CastMethod::Noop),
                        }
                    ).map(|(ty, method)| (ty.ty.clone(), method))
                    .map_err(|options| options.iter().map(|(ty, _)| ty.clone()).collect())
                } else {
                    panic!("Invalid cast!")
                };
                match ty_and_method {
                    Ok((ty, method)) => {
                        constraints.set_to(ty);
                        *self.tc.cast_method_mut(item.cast_id) = method;
                    },
                    Err(_) => {
                        self.errors.push(Error::new("Invalid cast!").adding_primary_range(self.hir.get_range(item.id), "cast here"));
                        constraints.set_to(Type::Error);
                        *self.tc.cast_method_mut(item.cast_id) = CastMethod::Noop;
                    }
                }
            }
            for item in unit.assignments.get_level(level) {
                let (lhs, rhs) = self.tc.constraints.index_mut(item.lhs, item.rhs);
                lhs.lopsided_intersect_with(rhs);
            }
            for item in unit.decl_refs.get_level(level) {
                let ty = self.tc.constraints(item.id).solve().unwrap_or(Type::Error.into());
                *self.tc.ty_mut(item.id) = ty.ty.clone();

                // P.S. These borrows are only here because the borrow checker is dumb
                let decls = &self.tir.decls;
                let overloads = &mut self.tir.overloads[item.decl_ref_id];
                let tc = &mut self.tc;
                let hir = &self.hir;
                overloads.retain(|&overload| {
                    tc.fetch_decl_type(hir, overload).trivially_convertible_to(&ty)
                });
                let pref = self.tc.preferred_overload(item.decl_ref_id);

                let overload = if !overloads.is_empty() {
                    let overload = pref
                        .filter(|overload| overloads.contains(overload))
                        .unwrap_or_else(|| overloads[0]);
                    let overload_is_function = match self.hir.decls[overload] {
                        hir::Decl::Computed { .. } => true,
                        hir::Decl::Intrinsic { function_like, .. } => function_like,
                        _ => false,
                    };
                    let has_parens = self.hir.decl_refs[item.decl_ref_id].has_parens;
                    if has_parens && !overload_is_function {
                        self.errors.push(
                            Error::new("reference to non-function must not have parentheses")
                                .adding_primary_range(self.hir.get_range(item.id), "")
                        );
                    } else if !has_parens && overload_is_function {
                        self.errors.push(
                            Error::new("function call must have parentheses")
                                .adding_primary_range(self.hir.get_range(item.id), "")
                        );
                    }
                    let decl = &decls[overload];
                    for (i, &arg) in item.args.iter().enumerate() {
                        let ty = self.tc.get_evaluated_type(decl.param_tys[i]).clone();
                        self.tc.constraints_mut(arg).set_to(ty);
                    }
                    Some(overload)
                } else {
                    self.errors.push(
                        Error::new("ambiguous overload for declaration")
                            .adding_primary_range(self.hir.get_range(item.id), "expression here")
                    );
                    for &arg in &item.args {
                        self.tc.constraints_mut(arg).set_to(Type::Error);
                    }
                    None
                };
                *self.tc.overload_mut(item.decl_ref_id) = overload;
            }
            for item in unit.addr_ofs.get_level(level) {
                let pointer_ty = self.tc.constraints(item.id).solve()
                    .map(|ty| ty.ty)
                    .unwrap_or(Type::Error);
                let pointee_ty = match pointer_ty {
                    Type::Pointer(ref pointee) => pointee.as_ref().clone(),
                    Type::Error => Type::Error.into(),
                    _ => panic!("unexpected non-pointer, non-error type for addr of expression"),
                };
                self.tc.constraints_mut(item.expr).set_to(pointee_ty);
                *self.tc.ty_mut(item.id) = pointer_ty;
            }
            for item in unit.derefs.get_level(level) {
                let mut ty = self.tc.constraints(item.id).solve().unwrap_or(Type::Error.into());
                *self.tc.ty_mut(item.id) = ty.ty.clone();

                if ty.ty != Type::Error {
                    ty = ty.ptr().into();
                }
                self.tc.constraints_mut(item.expr).set_to(ty);
            }
            for item in unit.pointers.get_level(level) {
                let expr_ty = self.tc.constraints(item.expr).solve().map(|ty| ty.ty).unwrap_or(Type::Error);
                // Don't bother checking if it's a type, because we already did that in pass 1
                self.tc.constraints_mut(item.expr).set_to(expr_ty);
                let ty = self.tc.constraints(item.id).solve().expect("Ambiguous type for pointer expression");
                debug_assert_eq!(ty.ty, Type::Ty);
                *self.tc.ty_mut(item.id) = Type::Ty;
            }
            for item in unit.ifs.get_level(level) {
                let condition_ty = self.tc.constraints(item.condition).solve().map(|ty| ty.ty).unwrap_or(Type::Error);
                // Don't bother checking if bool, because we already did that in pass 1
                self.tc.constraints_mut(item.condition).set_to(condition_ty);
                let ty = self.tc.constraints(item.id).solve().expect("ambiguous type for if expression");
                *self.tc.ty_mut(item.id) = ty.ty.clone();
                self.tc.constraints_mut(item.then_expr).set_to(ty.clone());
                self.tc.constraints_mut(item.else_expr).set_to(ty);
            }
            for item in unit.dos.get_level(level) {
                let ty = self.tc.constraints(item.id).solve().expect("Ambiguous type for do expression");
                *self.tc.ty_mut(item.id) = ty.ty.clone();
                self.tc.constraints_mut(item.terminal_expr).set_to(ty);
            }
            if level > 0 {
                self.tc.debug_output(&self.hir, &self.file, level as usize);
            }
        }
        fn lit_pass_2(
            tp: &mut impl TypeProvider,
            lits: &[ExprId],
            lit_ty: &str
        ) {
            for &item in lits {
                *tp.ty_mut(item) = tp.constraints(item).solve().expect(format!("Ambiguous type for {} literal", lit_ty).as_ref()).ty;
            }
        }
        lit_pass_2(&mut self.tc, &unit.int_lits, "integer");
        lit_pass_2(&mut self.tc, &unit.dec_lits, "decimal");
        lit_pass_2(&mut self.tc, &unit.str_lits, "string");
        lit_pass_2(&mut self.tc, &unit.char_lits, "character");
        self.tc.debug_output(&self.hir, &self.file, 0);
    }

    pub fn type_check(&mut self) {
        // TODO: This stuff should all be moved into RealTypeProvider
        self.tc.init(self.tc.debug, &self.hir, &self.tir);

        // Assign the type of the void expression to be void.
        *self.tc.constraints_mut(self.hir.void_expr) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Void.into()]), None);
        *self.tc.ty_mut(self.hir.void_expr) = Type::Void;

        for unit_i in 0..self.tir.units.len() {
            let uid = UnitId::new(unit_i);

            // Pass 1: propagate info down from leaves to roots
            let levels = self.run_pass_1(UnitRef::Id(uid));
            
            // NOTE: statements are handled specially, and don't need to be broken off in the event of a dynamically-discovered type 4 dependency
            for item in &self.tir.units[uid].stmts {
                let constraints = self.tc.constraints_mut(item.root_expr);
                if let Some(err) = constraints.can_unify_to(&Type::Void.into()).err() {
                    let mut error = Error::new("statements must return void");
                    let range = self.hir.get_range(item.root_expr);
                    match err {
                        UnificationError::InvalidChoice(choices)
                        => error.add_secondary_range(range, format!("note: expression could've unified to any of {:?}", choices)),
                        UnificationError::Trait(not_implemented)
                        => error.add_secondary_range(
                            range,
                            format!(
                                "note: couldn't unify because expression requires implementations of {:?}",
                                not_implemented.names(),
                            ),
                        ),
                    }
                    self.errors.push(error);
                }
                constraints.set_to(Type::Void);
            }
            
            // Pass 2: propagate info up from roots to leaves
            self.run_pass_2(UnitRef::Id(uid), levels);

            for i in 0..self.tir.units[uid].eval_dependees.len() {
                let expr = self.tir.units[uid].eval_dependees[i];
                let val = self.eval_expr(expr);
                self.tc.insert_eval_result(expr, val);
            }
        }
    }
}