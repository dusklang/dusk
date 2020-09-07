use smallvec::{SmallVec, smallvec};

mod constraints;
pub mod type_provider;

use constraints::{ConstraintList, UnificationError};
use type_provider::{TypeProvider, RealTypeProvider, MockTypeProvider};

use crate::driver::Driver;
use crate::error::Error;
use crate::builder::{ExprId, DeclId};
use crate::ty::{BuiltinTraits, Type, QualType, IntWidth};
use crate::mir::Const;
use crate::hir;
use crate::tir::{Unit, UnitItems, LevelMetaDependees, ExprNamespace};

#[derive(Copy, Clone, Debug)]
pub enum CastMethod {
    Noop,
    Reinterpret,
    Int,
    Float,
    FloatToInt,
    IntToFloat,
}

fn unit_string(unit_num: Option<usize>) -> String {
    match unit_num {
        Some(num) => format!("UNIT {}", num),
        None => "MOCK UNIT".to_string()
    }
}

impl Driver {
    pub fn decl_type<'a>(&'a self, id: DeclId, tp: &'a impl TypeProvider) -> &Type {
        self.hir.explicit_tys[id].map(|ty| tp.get_evaluated_type(ty)).unwrap_or(&Type::Error)
    }

    fn run_pass_1(&mut self, unit: &UnitItems, unit_num: Option<usize>, start_level: u32, meta_dependees: &[LevelMetaDependees], tp: &mut impl TypeProvider) {
        // Pass 1: propagate info down from leaves to roots
        if tp.debug() {
            println!(
                "===============TYPECHECKING {}: PASS 1===============",
                unit_string(unit_num),
            );
        }
        fn lit_pass_1(tp: &mut impl TypeProvider, lits: &[ExprId], trait_impls: BuiltinTraits, pref: Type) {
            for &item in lits {
                *tp.constraints_mut(item) = ConstraintList::new(
                    trait_impls, 
                    None,
                    Some(pref.clone().into())
                );
            }
        }
        lit_pass_1(tp, &unit.int_lits, BuiltinTraits::INT, Type::i32());
        lit_pass_1(tp, &unit.dec_lits, BuiltinTraits::DEC, Type::i32());
        lit_pass_1(tp, &unit.str_lits, BuiltinTraits::STR, Type::u8().ptr());
        lit_pass_1(tp, &unit.char_lits, BuiltinTraits::CHAR, Type::u8().ptr());
        for &item in &unit.const_tys {
            *tp.constraints_mut(item) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Ty.into()]), None);
            *tp.ty_mut(item) = Type::Ty;
        }
        let mut meta_dependee_i = 0;
        for level in start_level..unit.num_levels() {
            for item in unit.assigned_decls.get_level(level) {
                let constraints = tp.constraints(item.root_expr);
                let ty = if let &Some(explicit_ty) = &item.explicit_ty {
                    let explicit_ty = tp.get_evaluated_type(explicit_ty).clone();
                    if let Some(err) = constraints.can_unify_to(&explicit_ty.clone().into()).err() {
                        let range = self.hir.get_range(item.root_expr);
                        let mut error = Error::new(format!("Couldn't unify expression to assigned decl type `{:?}`", explicit_ty))
                            .adding_primary_range(range, "expression here");
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
                tp.decl_type_mut(item.decl_id).ty = ty;
            }
            for item in unit.assignments.get_level(level) {
                tp.constraints_mut(item.id).set_to(Type::Void);
                *tp.ty_mut(item.id) = Type::Void;
            }
            for item in unit.casts.get_level(level) {
                let ty = tp.get_evaluated_type(item.ty).clone();
                *tp.constraints_mut(item.id) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![ty.clone().into()]), None);
                *tp.ty_mut(item.id) = ty;
            }
            for item in unit.whiles.get_level(level) {
                *tp.constraints_mut(item.id) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Void.into()]), None);
                *tp.ty_mut(item.id) = Type::Void;
            }
            for &item in unit.explicit_rets.get_level(level) {
                *tp.constraints_mut(item) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Never.into()]), None);
                *tp.ty_mut(item) = Type::Never;
            }
            for &item in unit.modules.get_level(level) {
                *tp.constraints_mut(item) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Mod.into()]), None);
                *tp.ty_mut(item) = Type::Mod;
            }
            for &item in unit.imports.get_level(level) {
                *tp.constraints_mut(item) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Mod.into()]), None);
                *tp.ty_mut(item) = Type::Mod;
            }
            for i in 0..unit.decl_refs.level_len(level) {
                let item = unit.decl_refs.at(level, i);
                let id = item.id;
                let args = item.args.clone();
                let decl_ref_id = item.decl_ref_id;

                // Filter overloads that don't match the constraints of the parameters.
                // These borrows are only here because the borrow checker is dumb
                let decls = &self.tir.decls;
                let tc = &tp;
                let mut overloads = self.find_overloads(&self.hir.decl_refs[decl_ref_id]);
                // Rule out overloads that don't match the arguments
                overloads.retain(|&overload| {
                    assert_eq!(decls[overload].param_tys.len(), args.len());
                    let arg_constraints = args.iter().map(|&arg| tc.constraints(arg));
                    let param_tys = decls[overload].param_tys.iter().map(|&expr| tc.get_evaluated_type(expr));
                    for (constraints, ty) in arg_constraints.zip(param_tys) {
                        if constraints.can_unify_to(&ty.into()).is_err() { return false; }
                    }
                    true
                });

                let mut one_of = SmallVec::new();
                one_of.reserve(overloads.len());
                for i in 0..overloads.len() {
                    let overload = overloads[i];
                    let ty = tp.fetch_decl_type(&self.hir, overload).ty.clone();
                    let is_mut = self.tir.decls[overload].is_mut;
                    one_of.push(QualType { ty, is_mut });
                }
                let mut pref = None;
                'find_preference: for (i, &arg) in args.iter().enumerate() {
                    if let Some(ty) = tp.constraints(arg).preferred_type() {
                        for &overload in &overloads {
                            let decl = &self.tir.decls[overload];
                            if ty.ty.trivially_convertible_to(tp.get_evaluated_type(decl.param_tys[i])) {
                                let ty = tp.fetch_decl_type(&self.hir, overload).clone();
                                pref = Some(ty);
                                *tp.preferred_overload_mut(decl_ref_id) = Some(overload);
                                break 'find_preference;
                            }
                        }
                    }
                }
                *tp.constraints_mut(id) = ConstraintList::new(BuiltinTraits::empty(), Some(one_of), pref);
                *tp.overloads_mut(decl_ref_id) = overloads;
            }
            for item in unit.addr_ofs.get_level(level) {
                let constraints = tp.constraints(item.expr).filter_map(|ty| {
                    if item.is_mut && !ty.is_mut { return None; }
                    Some(
                        QualType::from(
                            ty.ty.clone().ptr_with_mut(item.is_mut)
                        )
                    )
                });
                *tp.constraints_mut(item.id) = constraints;
            }
            for item in unit.derefs.get_level(level) {
                let constraints = tp.constraints(item.expr).filter_map(|ty| {
                    if let Type::Pointer(pointee) = &ty.ty {
                        Some(pointee.as_ref().clone())
                    } else {
                        None
                    }
                });
                *tp.constraints_mut(item.id) = constraints;
            }
            for item in unit.pointers.get_level(level) {
                if let Some(err) = tp.constraints(item.expr).can_unify_to(&Type::Ty.into()).err() {
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
                tp.constraints_mut(item.id).set_to(Type::Ty);
            }
            for item in unit.ifs.get_level(level) {
                if let Some(err) = tp.constraints(item.condition).can_unify_to(&Type::Bool.into()).err() {
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
                let constraints = tp.constraints(item.then_expr).intersect_with(tp.constraints(item.else_expr));

                if constraints.solve().is_err() {
                    // TODO: handle void expressions, which don't have appropriate source location info.
                    self.errors.push(
                        Error::new("Failed to unify branches of if expression")
                            .adding_primary_range(self.hir.get_range(item.then_expr), "first terminal expression here")
                            .adding_primary_range(self.hir.get_range(item.else_expr), "second terminal expression here")
                    );
                }
                *tp.constraints_mut(item.id) = constraints;
            }
            for item in unit.dos.get_level(level) {
                *tp.constraints_mut(item.id) = tp.constraints(item.terminal_expr).clone();
            }
            for item in &unit.stmts {
                let constraints = tp.constraints_mut(item.root_expr);
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
            tp.debug_output(&self.hir, &self.src_map, level as usize);

            // Evaluate meta-dependees to build namespaces for expressions
            if meta_dependee_i < meta_dependees.len() && meta_dependees[meta_dependee_i].level == level {
                for dep in &meta_dependees[meta_dependee_i].meta_dependees {
                    let mut mock = MockTypeProvider::new(tp);
                    if mock.constraints(dep.dependee).can_unify_to(&Type::Mod.into()).is_ok() {
                        self.run_pass_1(&dep.items, None, level+1, &[], &mut mock);
                        mock.constraints_mut(dep.dependee).set_to(&Type::Mod);
                        self.run_pass_2(&dep.items, None, &mut mock);
                        let module = self.eval_expr(dep.dependee, &mock);
                        match module {
                            Const::Mod(scope) => self.tir.expr_namespaces.entry(dep.dependee).or_default()
                                .push(ExprNamespace::Mod(scope)),
                            _ => panic!("Unexpected const kind, expected module!"),
                        }
                    }
                }
                meta_dependee_i += 1;
            }
        }
    }

    fn run_pass_2(&mut self, unit: &UnitItems, unit_num: Option<usize>, tp: &mut impl TypeProvider) {
        if tp.debug() {
            println!(
                "===============TYPECHECKING {}: PASS 2===============",
                unit_string(unit_num)
            );
        }
        for level in (0..unit.num_levels()).rev() {
            for i in 0..unit.assigned_decls.level_len(level) {
                let item = unit.assigned_decls.at(level, i);
                let decl_id = item.decl_id;
                let root_expr = item.root_expr;
                let ty = tp.fetch_decl_type(&self.hir, decl_id).ty.clone();
                tp.constraints_mut(root_expr).set_to(ty);
            }
            for item in unit.ret_groups.get_level(level) {
                let ty = tp.get_evaluated_type(item.ty).clone();
                for &expr in &item.exprs {
                    if let Some(err) = tp.constraints(expr).can_unify_to(&QualType::from(&ty)).err() {
                        let range = self.hir.get_range(expr);
                        let mut error = Error::new(format!("can't unify expression to return type {:?}", ty))
                            .adding_primary_range(range, "expression here");
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
                    tp.constraints_mut(expr).set_to(ty.clone());
                }
            }
            for item in unit.whiles.get_level(level) {
                if tp.constraints(item.condition).can_unify_to(&Type::Bool.into()).is_ok() {
                    tp.constraints_mut(item.condition).set_to(Type::Bool);
                } else {
                    panic!("Expected boolean condition in while expression");
                }
            }
            for item in unit.casts.get_level(level) {
                let ty = tp.get_evaluated_type(item.ty).clone();
                let constraints = tp.constraints_mut(item.expr);
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
                        *tp.cast_method_mut(item.cast_id) = method;
                    },
                    Err(_) => {
                        self.errors.push(Error::new("Invalid cast!").adding_primary_range(self.hir.get_range(item.id), "cast here"));
                        constraints.set_to(Type::Error);
                        *tp.cast_method_mut(item.cast_id) = CastMethod::Noop;
                    }
                }
            }
            for item in unit.assignments.get_level(level) {
                let (lhs, rhs) = tp.multi_constraints_mut(item.lhs, item.rhs);
                lhs.lopsided_intersect_with(rhs);
            }
            for item in unit.decl_refs.get_level(level) {
                let ty = tp.constraints(item.id).solve().unwrap_or(Type::Error.into());
                *tp.ty_mut(item.id) = ty.ty.clone();

                // P.S. These borrows are only here because the borrow checker is dumb
                let decls = &self.tir.decls;
                let mut overloads = tp.overloads(item.decl_ref_id).clone();
                let hir = &self.hir;
                overloads.retain(|&overload| {
                    tp.fetch_decl_type(hir, overload).trivially_convertible_to(&ty)
                });
                let pref = tp.preferred_overload(item.decl_ref_id);

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
                        let ty = tp.get_evaluated_type(decl.param_tys[i]).clone();
                        tp.constraints_mut(arg).set_to(ty);
                    }
                    Some(overload)
                } else {
                    self.errors.push(
                        Error::new("ambiguous overload for declaration")
                            .adding_primary_range(self.hir.get_range(item.id), "expression here")
                    );
                    for &arg in &item.args {
                        tp.constraints_mut(arg).set_to(Type::Error);
                    }
                    None
                };
                *tp.overloads_mut(item.decl_ref_id) = overloads;
                *tp.selected_overload_mut(item.decl_ref_id) = overload;
            }
            for item in unit.addr_ofs.get_level(level) {
                let pointer_ty = tp.constraints(item.id).solve()
                    .map(|ty| ty.ty)
                    .unwrap_or(Type::Error);
                let pointee_ty = match pointer_ty {
                    Type::Pointer(ref pointee) => pointee.as_ref().clone(),
                    Type::Error => Type::Error.into(),
                    _ => panic!("unexpected non-pointer, non-error type for addr of expression"),
                };
                tp.constraints_mut(item.expr).set_to(pointee_ty);
                *tp.ty_mut(item.id) = pointer_ty;
            }
            for item in unit.derefs.get_level(level) {
                let mut ty = tp.constraints(item.id).solve().unwrap_or(Type::Error.into());
                *tp.ty_mut(item.id) = ty.ty.clone();

                if ty.ty != Type::Error {
                    ty = ty.ptr().into();
                }
                tp.constraints_mut(item.expr).set_to(ty);
            }
            for item in unit.pointers.get_level(level) {
                let expr_ty = tp.constraints(item.expr).solve().map(|ty| ty.ty).unwrap_or(Type::Error);
                // Don't bother checking if it's a type, because we already did that in pass 1
                tp.constraints_mut(item.expr).set_to(expr_ty);
                let ty = tp.constraints(item.id).solve().expect("Ambiguous type for pointer expression");
                debug_assert_eq!(ty.ty, Type::Ty);
                *tp.ty_mut(item.id) = Type::Ty;
            }
            for item in unit.ifs.get_level(level) {
                let condition_ty = tp.constraints(item.condition).solve().map(|ty| ty.ty).unwrap_or(Type::Error);
                // Don't bother checking if bool, because we already did that in pass 1
                tp.constraints_mut(item.condition).set_to(condition_ty);
                let ty = tp.constraints(item.id).solve().expect("ambiguous type for if expression");
                *tp.ty_mut(item.id) = ty.ty.clone();
                tp.constraints_mut(item.then_expr).set_to(ty.clone());
                tp.constraints_mut(item.else_expr).set_to(ty);
            }
            for item in unit.dos.get_level(level) {
                let ty = tp.constraints(item.id).solve().expect("Ambiguous type for do expression");
                *tp.ty_mut(item.id) = ty.ty.clone();
                tp.constraints_mut(item.terminal_expr).set_to(ty);
            }
            if level > 0 {
                tp.debug_output(&self.hir, &self.src_map, level as usize);
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
        lit_pass_2(tp, &unit.int_lits, "integer");
        lit_pass_2(tp, &unit.dec_lits, "decimal");
        lit_pass_2(tp, &unit.str_lits, "string");
        lit_pass_2(tp, &unit.char_lits, "character");
        tp.debug_output(&self.hir, &self.src_map, 0);
    }

    pub fn type_check(&mut self, units: &[Unit], dbg: bool) -> RealTypeProvider {
        // Assign the type of the void expression to be void.
        let mut tp = RealTypeProvider::new(dbg, &self.hir, &self.tir);
        *tp.constraints_mut(self.hir.void_expr) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Void.into()]), None);
        *tp.ty_mut(self.hir.void_expr) = Type::Void;

        for (num, unit) in units.iter().enumerate() {
            // Pass 1: propagate info down from leaves to roots
            self.run_pass_1(&unit.items, Some(num), 0, &unit.meta_dependees, &mut tp);
            
            // Pass 2: propagate info up from roots to leaves
            self.run_pass_2(&unit.items, Some(num), &mut tp);

            for i in 0..unit.eval_dependees.len() {
                let expr = unit.eval_dependees[i];
                let val = self.eval_expr(expr, &tp);
                tp.insert_eval_result(expr, val);
            }
        }

        tp
    }
}