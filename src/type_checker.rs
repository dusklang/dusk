use smallvec::smallvec;

mod constraints;
use constraints::{ConstraintList, UnificationError};

use crate::driver::Driver;
use crate::error::Error;
use crate::builder::{ExprId, DeclId, DeclRefId, CastId};
use crate::index_vec::Idx;
use crate::ty::{BuiltinTraits, Type, QualType, IntWidth};
use crate::index_vec::IdxVec;
use crate::dep_vec;
use crate::source_info::CommentatedSourceRange;

#[derive(Clone, Debug)]
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
            debug,
        }
    }
}

impl Driver {
    fn debug_output(&mut self, level: usize) {
        if !self.tc.debug { return; }
        println!("LEVEL {}", level);
        assert_eq!(self.tc.constraints.len(), self.tc.constraints_copy.len());
        for i in 0..self.tc.constraints.len() {
            let i = ExprId::new(i);
            let new_constraints = &self.tc.constraints[i];
            let old_constraints = &mut self.tc.constraints_copy[i];
            if new_constraints != old_constraints {
                self.file.print_commentated_source_ranges(&mut [
                    CommentatedSourceRange::new(self.hir.source_ranges[i].clone(), "", '-')
                ]);
                old_constraints.print_diff(new_constraints);
                *old_constraints = new_constraints.clone();
                println!("============================================================================================\n")
            }
        }
    }

    pub fn type_check(&mut self) {
        self.tc.types.resize_with(self.tir.num_exprs, Default::default);
        self.tc.constraints.resize_with(self.tir.num_exprs, Default::default);
        if self.tc.debug {
            self.tc.constraints_copy.resize_with(self.tir.num_exprs, Default::default);
        }
        self.tc.overloads.resize_with(self.tir.overloads.len(), || None);
        self.tc.preferred_overloads.resize_with(self.tir.overloads.len(), || None);
        self.tc.cast_methods.resize_with(self.tir.casts.len(), || CastMethod::Noop);

        // Extend arrays as needed so they all have the same number of levels.
        let levels = dep_vec::unify_sizes(&mut [
            &mut self.tir.assigned_decls, &mut self.tir.assignments, &mut self.tir.decl_refs, 
            &mut self.tir.addr_ofs, &mut self.tir.derefs, &mut self.tir.pointers, &mut self.tir.ifs,
            &mut self.tir.dos,
        ]);

        // Assign the type of the void expression to be void.
        self.tc.constraints[self.tir.void_expr] = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Void.into()]), None);
        self.tc.types[self.tir.void_expr] = Type::Void;

        // Pass 1: propagate info down from leaves to roots
        if self.tc.debug { println!("===============TYPECHECKING: PASS 1==============="); }
        fn independent_pass_1<T>(constraints: &mut IdxVec<ConstraintList, ExprId>, tys: &mut IdxVec<Type, ExprId>, exprs: &[T], data: impl Fn(&T) -> (ExprId, Type)) {
            for item in exprs {
                let (id, ty) = data(item);
                constraints[id] = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![ty.clone().into()]), None);
                tys[id] = ty;
            }
        }
        independent_pass_1(&mut self.tc.constraints, &mut self.tc.types, &self.tir.explicit_rets, |&id| (id, Type::Never));
        independent_pass_1(&mut self.tc.constraints, &mut self.tc.types, &self.tir.whiles, |item| (item.id, Type::Void));
        independent_pass_1(&mut self.tc.constraints, &mut self.tc.types, &self.tir.casts, |item| (item.id, item.ty.clone()));

        fn lit_pass_1(constraints: &mut IdxVec<ConstraintList, ExprId>, lits: &[ExprId], trait_impls: BuiltinTraits, pref: Type) {
            for &item in lits {
                constraints[item] = ConstraintList::new(
                    trait_impls, 
                    None,
                    Some(pref.clone().into())
                );
            }
        }
        lit_pass_1(&mut self.tc.constraints, &self.tir.int_lits, BuiltinTraits::INT, Type::i32());
        lit_pass_1(&mut self.tc.constraints, &self.tir.dec_lits, BuiltinTraits::DEC, Type::i32());
        lit_pass_1(&mut self.tc.constraints, &self.tir.str_lits, BuiltinTraits::STR, Type::u8().ptr());
        lit_pass_1(&mut self.tc.constraints, &self.tir.char_lits, BuiltinTraits::CHAR, Type::u8().ptr());
        for level in 0..levels {
            for item in self.tir.assigned_decls.get_level(level) {
                let constraints = &self.tc.constraints[item.root_expr];
                let ty = if let Some(explicit_ty) = &item.explicit_ty {
                    if let Some(err) = constraints.can_unify_to(&explicit_ty.into()).err() {
                        let range = self.hir.source_ranges[item.root_expr].clone();
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
                    explicit_ty.clone()
                } else {
                    constraints.solve().expect("Ambiguous type for assigned declaration").ty
                };
                self.tir.decls[item.decl_id].ret_ty.ty = ty;
            }
            for item in self.tir.assignments.get_level(level) {
                self.tc.constraints[item.id].set_to(Type::Void);
                self.tc.types[item.id] = Type::Void;
            }
            for item in self.tir.decl_refs.get_level(level) {
                // Filter overloads that don't match the constraints of the parameters.
                // P.S. These borrows are only here because the borrow checker is dumb
                let decls = &self.tir.decls;
                let constraints = &self.tc.constraints;
                // Rule out overloads that don't match the arguments
                self.tir.overloads[item.decl_ref_id].retain(|&overload| {
                    assert_eq!(decls[overload].param_tys.len(), item.args.len());
                    for (constraints, ty) in item.args.iter().map(|&arg| &constraints[arg]).zip(&decls[overload].param_tys) {
                        if constraints.can_unify_to(&ty.into()).is_err() { return false; }
                    }
                    true
                });

                let one_of = self.tir.overloads[item.decl_ref_id].iter()
                    .map(|&overload| decls[overload].ret_ty.clone())
                    .collect();
                let mut pref = None;
                'find_preference: for (i, &arg) in item.args.iter().enumerate() {
                    if let Some(ty) = self.tc.constraints[arg].preferred_type() {
                        for &overload in &self.tir.overloads[item.decl_ref_id] {
                            let decl = &decls[overload];
                            if ty.ty.trivially_convertible_to(&decl.param_tys[i]) {
                                pref = Some(decl.ret_ty.clone());
                                self.tc.preferred_overloads[item.decl_ref_id] = Some(overload);
                                break 'find_preference;
                            }
                        }
                    }
                }
                self.tc.constraints[item.id] = ConstraintList::new(BuiltinTraits::empty(), Some(one_of), pref);
            }
            for item in self.tir.addr_ofs.get_level(level) {
                let constraints = self.tc.constraints[item.expr].filter_map(|ty| {
                    if item.is_mut && !ty.is_mut { return None; }
                    Some(
                        QualType::from(
                            ty.ty.clone().ptr_with_mut(item.is_mut)
                        )
                    )
                });
                self.tc.constraints[item.id] = constraints;
            }
            for item in self.tir.derefs.get_level(level) {
                let constraints = self.tc.constraints[item.expr].filter_map(|ty| {
                    if let Type::Pointer(pointee) = &ty.ty {
                        Some(pointee.as_ref().clone())
                    } else {
                        None
                    }
                });
                self.tc.constraints[item.id] = constraints;
            }
            for item in self.tir.pointers.get_level(level) {
                if let Some(err) = self.tc.constraints[item.expr].can_unify_to(&Type::Ty.into()).err() {
                    let mut error = Error::new("Expected type operand to pointer operator");
                    let range = self.hir.source_ranges[item.expr].clone();
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
                self.tc.constraints[item.id].set_to(Type::Ty);
            }
            for item in self.tir.ifs.get_level(level) {
                if let Some(err) = self.tc.constraints[item.condition].can_unify_to(&Type::Bool.into()).err() {
                    let mut error = Error::new("Expected boolean condition in if expression");
                    let range = self.hir.source_ranges[item.condition].clone();
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
                let constraints = self.tc.constraints[item.then_expr].intersect_with(&self.tc.constraints[item.else_expr]);
                
                if constraints.solve().is_err() {
                    // TODO: handle void expressions, which don't have appropriate source location info.
                    self.errors.push(
                        Error::new("Failed to unify branches of if expression")
                            .adding_primary_range(self.hir.source_ranges[item.then_expr].clone(), "first terminal expression here")
                            .adding_primary_range(self.hir.source_ranges[item.else_expr].clone(), "second terminal expression here")
                    );
                }
                self.tc.constraints[item.id] = constraints;
            }
            for item in self.tir.dos.get_level(level) {
                self.tc.constraints[item.id] = self.tc.constraints[item.terminal_expr].clone();
            }
            self.debug_output(level as usize);
        }

        // Pass 2: propagate info up from roots to leaves
        if self.tc.debug { println!("===============TYPECHECKING: PASS 2==============="); }
        for item in &self.tir.stmts {
            let constraints = &mut self.tc.constraints[item.root_expr];
            if let Some(err) = constraints.can_unify_to(&Type::Void.into()).err() {
                let mut error = Error::new("statements must return void");
                let range = self.hir.source_ranges[item.root_expr].clone();
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

        for group in &self.tir.ret_groups {
            for &expr in &group.exprs {
                if let Some(err) = self.tc.constraints[expr].can_unify_to(&QualType::from(&group.ty)).err() {
                    let range = self.hir.source_ranges[expr].clone();
                    let mut error = Error::new(format!("can't unify expression to return type {:?}", group.ty))
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
                self.tc.constraints[expr].set_to(group.ty.clone());
            }
        }
        for item in &self.tir.whiles {
            if self.tc.constraints[item.condition].can_unify_to(&Type::Bool.into()).is_ok() {
                self.tc.constraints[item.condition].set_to(Type::Bool);
            } else {
                panic!("Expected boolean condition in while expression");
            }
        }
        for item in &self.tir.casts {
            let constraints = &mut self.tc.constraints[item.expr];
            let ty_and_method: Result<(Type, CastMethod), Vec<&QualType>> = if constraints.can_unify_to(&item.ty.clone().into()).is_ok() {
                Ok((item.ty.clone(), CastMethod::Noop))
            } else if let Type::Pointer(dest_pointee_ty) = &item.ty {
                let dest_pointee_ty = dest_pointee_ty.as_ref();
                let src_ty = constraints.max_ranked_type(|ty|
                    match ty.ty {
                        Type::Pointer(ref pointee) if pointee.is_mut || dest_pointee_ty.is_mut => 2,
                        Type::Int { width, .. } if width == IntWidth::Pointer => 1,
                        _ => 0,
                    }
                ).expect("Invalid cast 1!").clone();
                Ok((src_ty.ty.clone(), CastMethod::Reinterpret))
            } else if let Type::Int { width, .. } = item.ty {
                constraints.max_ranked_type_with_assoc_data(|ty|
                    match ty.ty {
                        Type::Int { .. } => (3, CastMethod::Int),
                        Type::Float { .. } => (2, CastMethod::FloatToInt),
                        Type::Pointer(_) if width == IntWidth::Pointer => (1, CastMethod::Reinterpret),
                        _ => (0, CastMethod::Noop),
                    }
                ).map(|(ty, method)| (ty.ty.clone(), method))
                .map_err(|options| options.iter().map(|(ty, _)| ty.clone()).collect())
            } else if let Type::Float { .. } = item.ty {
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
                    self.tc.cast_methods[item.cast_id] = method;
                },
                Err(_) => {
                    self.errors.push(Error::new("Invalid cast!").adding_primary_range(self.hir.source_ranges[item.id].clone(), "cast here"));
                    constraints.set_to(Type::Error);
                    self.tc.cast_methods[item.cast_id] = CastMethod::Noop;
                }
            }
        }
        for level in (0..levels).rev() {
            for item in self.tir.assigned_decls.get_level(level) {
                self.tc.constraints[item.root_expr].set_to(self.tir.decls[item.decl_id].ret_ty.ty.clone());
            }
            for item in self.tir.assignments.get_level(level) {
                let (lhs, rhs) = self.tc.constraints.index_mut(item.lhs, item.rhs);
                lhs.lopsided_intersect_with(rhs);
            }
            for item in self.tir.decl_refs.get_level(level) {
                let ty = self.tc.constraints[item.id].solve().unwrap_or(Type::Error.into());
                self.tc.types[item.id] = ty.ty.clone();

                // P.S. These borrows are only here because the borrow checker is dumb
                let decls = &self.tir.decls;
                let overloads = &mut self.tir.overloads[item.decl_ref_id];
                overloads.retain(|&overload| {
                    decls[overload].ret_ty
                        .trivially_convertible_to(&ty)
                });
                let pref = self.tc.preferred_overloads[item.decl_ref_id];

                let overload = if !overloads.is_empty() {
                    let overload = pref
                        .filter(|overload| overloads.contains(overload))
                        .unwrap_or_else(|| overloads[0]);
                    let decl = &decls[overload];
                    for (i, &arg) in item.args.iter().enumerate() {
                        self.tc.constraints[arg].set_to(decl.param_tys[i].clone());
                    }
                    Some(overload)
                } else {
                    self.errors.push(
                        Error::new("ambiguous overload for declaration")
                            .adding_primary_range(self.hir.source_ranges[item.id].clone(), "expression here")
                    );
                    for &arg in &item.args {
                        self.tc.constraints[arg].set_to(Type::Error);
                    }
                    None
                };
                self.tc.overloads[item.decl_ref_id] = overload;
            }
            for item in self.tir.addr_ofs.get_level(level) {
                let pointer_ty = self.tc.constraints[item.id].solve()
                    .map(|ty| ty.ty)
                    .unwrap_or(Type::Error);
                let pointee_ty = match pointer_ty {
                    Type::Pointer(ref pointee) => pointee.as_ref().clone(),
                    Type::Error => Type::Error.into(),
                    _ => panic!("unexpected non-pointer, non-error type for addr of expression"),
                };
                self.tc.constraints[item.expr].set_to(pointee_ty);
                self.tc.types[item.id] = pointer_ty;
            }
            for item in self.tir.derefs.get_level(level) {
                let mut ty = self.tc.constraints[item.id].solve().unwrap_or(Type::Error.into());
                self.tc.types[item.id] = ty.ty.clone();

                if ty.ty != Type::Error {
                    ty = ty.ptr().into();
                }
                self.tc.constraints[item.expr].set_to(ty);
            }
            for item in self.tir.pointers.get_level(level) {
                let expr = &mut self.tc.constraints[item.expr];
                let expr_ty = expr.solve().map(|ty| ty.ty).unwrap_or(Type::Error);
                // Don't bother checking if it's a type, because we already did that in pass 1
                self.tc.constraints[item.expr].set_to(expr_ty);
                let ty = self.tc.constraints[item.id].solve().expect("Ambiguous type for pointer expression");
                debug_assert_eq!(ty.ty, Type::Ty);
                self.tc.types[item.id] = Type::Ty;
            }
            for item in self.tir.ifs.get_level(level) {
                let condition = &mut self.tc.constraints[item.condition];
                let condition_ty = condition.solve().map(|ty| ty.ty).unwrap_or(Type::Error);
                // Don't bother checking if bool, because we already did that in pass 1
                self.tc.constraints[item.condition].set_to(condition_ty);
                let ty = self.tc.constraints[item.id].solve().expect("ambiguous type for if expression");
                self.tc.types[item.id] = ty.ty.clone();
                self.tc.constraints[item.then_expr].set_to(ty.clone());
                self.tc.constraints[item.else_expr].set_to(ty);
            }
            for item in self.tir.dos.get_level(level) {
                let ty = self.tc.constraints[item.id].solve().expect("Ambiguous type for do expression");
                self.tc.types[item.id] = ty.ty.clone();
                self.tc.constraints[item.terminal_expr].set_to(ty);
            }
            if level > 0 {
                self.debug_output(level as usize);
            }
        }
        fn lit_pass_2(
            constraints: &IdxVec<ConstraintList, ExprId>,
            types: &mut IdxVec<Type, ExprId>,
            lits: &[ExprId],
            lit_ty: &str
        ) {
            for &item in lits {
                types[item] = constraints[item].solve().expect(format!("Ambiguous type for {} literal", lit_ty).as_ref()).ty;
            }
        }
        lit_pass_2(&self.tc.constraints, &mut self.tc.types, &self.tir.int_lits, "integer");
        lit_pass_2(&self.tc.constraints, &mut self.tc.types, &self.tir.dec_lits, "decimal");
        lit_pass_2(&self.tc.constraints, &mut self.tc.types, &self.tir.str_lits, "string");
        lit_pass_2(&self.tc.constraints, &mut self.tc.types, &self.tir.char_lits, "character");
        self.debug_output(0);
    }
}