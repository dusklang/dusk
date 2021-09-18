use smallvec::{SmallVec, smallvec};

mod constraints;
pub mod type_provider;

use constraints::*;
use type_provider::{TypeProvider, RealTypeProvider, MockTypeProvider};

use mire::hir::{self, ExprId, DeclId, StructId};
use mire::mir::Const;
use mire::ty::{Type, QualType, IntWidth};

use crate::driver::Driver;
use crate::error::Error;
use crate::ty::BuiltinTraits;
use crate::tir::{Units, UnitItems, ExprNamespace, self};

#[derive(Copy, Clone, Debug)]
pub enum CastMethod {
    Noop,
    Reinterpret,
    Int,
    Float,
    FloatToInt,
    IntToFloat,
}

#[derive(Clone, Debug)]
pub struct StructLit {
    pub strukt: StructId,
    pub fields: Vec<ExprId>,
}

fn unit_string(unit_kind: UnitKind) -> String {
    match unit_kind {
        UnitKind::Normal(num) => format!("UNIT {}", num),
        UnitKind::Mock(num) => format!("MOCK UNIT {}", num),
    }
}

enum UnitKind {
    Normal(usize),
    Mock(usize),
}

impl tir::AssignedDecl {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        let constraints = tp.constraints(self.root_expr);
        let ty = if let &Some(explicit_ty) = &self.explicit_ty {
            let explicit_ty = tp.get_evaluated_type(explicit_ty).clone();
            if let Some(err) = can_unify_to(&constraints, &explicit_ty.clone().into()).err() {
                let range = driver.get_range(self.root_expr);
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
                driver.errors.push(error);
            }
            explicit_ty
        } else {
            constraints.solve().expect("Ambiguous type for assigned declaration").ty
        };
        tp.decl_type_mut(self.decl_id).ty = ty;
    }
}

impl tir::Expr<tir::Assignment> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        tp.constraints_mut(self.id).set_to(Type::Void);
        *tp.ty_mut(self.id) = Type::Void;
    }
}

impl tir::Expr<tir::Cast> {
    fn run_pass_1(&self, _driver: &mut Driver, tp: &mut impl TypeProvider) {
        let ty = tp.get_evaluated_type(self.ty).clone();
        *tp.constraints_mut(self.id) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![ty.clone().into()]), None);
        *tp.ty_mut(self.id) = ty;
    }
}

impl tir::Expr<tir::While> {
    fn run_pass_1(&self, _driver: &mut Driver, tp: &mut impl TypeProvider) {
        *tp.constraints_mut(self.id) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Void.into()]), None);
        *tp.ty_mut(self.id) = Type::Void;
    }
}

impl tir::Expr<tir::ExplicitRet> {
    fn run_pass_1(&self, _driver: &mut Driver, tp: &mut impl TypeProvider) {
        *tp.constraints_mut(self.id) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Never.into()]), None);
        *tp.ty_mut(self.id) = Type::Never;
    }
}

impl tir::Expr<tir::Module> {
    fn run_pass_1(&self, _driver: &mut Driver, tp: &mut impl TypeProvider) {
        *tp.constraints_mut(self.id) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Mod.into()]), None);
        *tp.ty_mut(self.id) = Type::Mod;
    }
}

impl tir::Expr<tir::Import> {
    fn run_pass_1(&self, _driver: &mut Driver, tp: &mut impl TypeProvider) {
        *tp.constraints_mut(self.id) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Mod.into()]), None);
        *tp.ty_mut(self.id) = Type::Mod;
    }
}

impl tir::Expr<tir::DeclRef> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        let id = self.id;
        let args = self.args.clone();
        let decl_ref_id = self.decl_ref_id;

        // Filter overloads that don't match the constraints of the parameters.
        // These borrows are only here because the borrow checker is dumb
        let decls = &driver.tir.decls;
        let tp_immutable = &*tp;
        let mut overloads = driver.find_overloads(&driver.code.hir_code.decl_refs[decl_ref_id]);
        let mut generic_args = Vec::new();
        // Rule out overloads that don't match the arguments
        overloads.retain(|&overload| {
            assert_eq!(decls[overload].param_tys.len(), args.len());
            let arg_constraints = args.iter().map(|&arg| tp_immutable.constraints(arg));
            let param_tys = decls[overload].param_tys.iter().map(|&expr| tp_immutable.get_evaluated_type(expr));
            let mut generic_arg_constraints: Vec<_> = decls[overload].generic_params.iter()
                .map(|_| ConstraintList::new(BuiltinTraits::empty(), None, None))
                .collect();
            for (constraints, ty) in arg_constraints.zip(param_tys) {
                match can_unify_to_in_generic_context(&constraints, &ty.into(), &decls[overload].generic_params) {
                    Ok(constraints) => {
                        debug_assert_eq!(generic_arg_constraints.len(), constraints.len());
                        for (og, new) in generic_arg_constraints.iter_mut().zip(constraints) {
                            *og = og.intersect_with(&new);
                        }
                    },
                    Err(_) => return false,
                }
            }

            generic_args.push(generic_arg_constraints);

            true
        });

        dbg!(generic_args);

        let mut one_of = SmallVec::new();
        one_of.reserve(overloads.len());
        for i in 0..overloads.len() {
            let overload = overloads[i];
            let ty = tp.fetch_decl_type(driver, overload, Some(decl_ref_id)).ty.clone();
            let mut is_mut = driver.tir.decls[overload].is_mut;
            if let hir::Namespace::MemberRef { base_expr } = driver.code.hir_code.decl_refs[decl_ref_id].namespace {
                let constraints = tp.constraints(base_expr);
                // TODO: Robustness! Base_expr could be an overload set with these types, but also include struct types
                if can_unify_to(&constraints, &Type::Ty.into()).is_err() && can_unify_to(&constraints, &Type::Mod.into()).is_err() {
                    is_mut = is_mut && constraints.solve().unwrap().is_mut;
                }
            }
            one_of.push(QualType { ty, is_mut });
        }
        let mut pref = None;
        'find_preference: for (i, &arg) in args.iter().enumerate() {
            if let Some(ty) = tp.constraints(arg).preferred_type() {
                for &overload in &overloads {
                    let decl = &driver.tir.decls[overload];
                    if ty.ty.trivially_convertible_to(tp.get_evaluated_type(decl.param_tys[i])) {
                        let ty = tp.fetch_decl_type(driver, overload, None).clone();
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
}

impl tir::Expr<tir::AddrOf> {
    fn run_pass_1(&self, _driver: &mut Driver, tp: &mut impl TypeProvider) {
        let constraints = tp.constraints(self.expr).filter_map(|ty| {
            if self.is_mut && !ty.is_mut { return None; }
            Some(
                QualType::from(
                    ty.ty.clone().ptr_with_mut(self.is_mut)
                )
            )
        });
        *tp.constraints_mut(self.id) = constraints;
    }
}

impl tir::Expr<tir::Dereference> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        let constraints = tp.constraints(self.expr).filter_map(|ty| {
            if let Type::Pointer(pointee) = &ty.ty {
                Some(pointee.as_ref().clone())
            } else {
                None
            }
        });
        *tp.constraints_mut(self.id) = constraints;
    }
}

impl tir::Expr<tir::Pointer> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        if let Some(err) = can_unify_to(tp.constraints(self.expr), &Type::Ty.into()).err() {
            let mut error = Error::new("Expected type operand to pointer operator");
            let range = driver.get_range(self.expr);
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
            driver.errors.push(error);
        }
        tp.constraints_mut(self.id).set_to(Type::Ty);
    }
}

impl tir::Expr<tir::Struct> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        for &field_ty in &self.field_tys {
            if let Some(err) = can_unify_to(tp.constraints(field_ty), &Type::Ty.into()).err() {
                let mut error = Error::new("Expected field type");
                let range = driver.get_range(field_ty);
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
                driver.errors.push(error);
            }
        }
        tp.constraints_mut(self.id).set_to(Type::Ty);
    }
}

impl tir::Expr<tir::StructLit> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        let ty = if let Some(err) = can_unify_to(tp.constraints(self.ty), &Type::Ty.into()).err() {
            let mut error = Error::new("Expected struct type");
            let range = driver.get_range(self.ty);
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
            driver.errors.push(error);
            Type::Error
        } else {
            let ty = tp.get_evaluated_type(self.ty).clone();
            match ty {
                Type::Struct(id) => {
                    let struct_fields = &driver.code.hir_code.structs[id].fields;
                    let mut matches = Vec::new();
                    matches.resize(struct_fields.len(), ExprId::new(u32::MAX as usize));

                    let mut successful = true;

                    // Find matches for each field in the literal
                    'lit_fields: for lit_field in &self.fields {
                        for (i, &struct_field) in struct_fields.iter().enumerate() {
                            let struct_field = &driver.code.hir_code.field_decls[struct_field];
                            if struct_field.name == lit_field.name {
                                matches[i] = lit_field.expr;
                                continue 'lit_fields;
                            }
                        }

                        // We can assume there is no match for this field at this point; if we had found one, we
                        // would've already continued to the next field.
                        successful = false;
                        
                        // TODO: Use range of the field identifier, which we don't have fine-grained access to yet
                        let range = driver.get_range(self.id);
                        driver.errors.push(
                            Error::new(format!("Unknown field {} in struct literal", driver.interner.resolve(lit_field.name).unwrap()))
                                .adding_primary_range(range, "")
                        );

                    }

                    let lit_range = driver.get_range(self.id);
                    // Make sure each field in the struct has a match in the literal
                    for (i, &maatch) in matches.iter().enumerate() {
                        let field = struct_fields[i];
                        let field = &driver.code.hir_code.field_decls[field];
                        let field_ty = tp.get_evaluated_type(field.ty).clone();
                        if maatch == ExprId::new(u32::MAX as usize) {
                            successful = false;

                            let field_item = driver.code.hir_code.decl_to_items[field.decl];
                            let field_range = driver.code.hir_code.source_ranges[field_item];

                            driver.errors.push(
                                Error::new(format!("Field {} not included in struct literal", driver.interner.resolve(field.name).unwrap()))
                                    .adding_primary_range(lit_range, "")
                                    .adding_secondary_range(field_range, "field declared here")
                            );
                        } else if let Some(err) = can_unify_to(tp.constraints(maatch), &field_ty.into()).err() {
                            successful = false;
                            let range = driver.get_range(maatch);
                            let mut error = Error::new("Invalid struct field type")
                                .adding_primary_range(range, "");
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
                            driver.errors.push(error);
                        }
                    }

                    if successful {
                        *tp.struct_lit_mut(self.struct_lit_id) = Some(
                            StructLit { strukt: id, fields: matches }
                        );
                    }
                },
                ref other => {
                    let range = driver.get_range(self.ty);
                    driver.errors.push(
                        Error::new(format!("Expected struct type in literal, found {:?}", *other))
                            .adding_primary_range(range, "")
                    );
                },
            }
            
            ty
        };

        tp.constraints_mut(self.id).set_to(ty);
    }
}

impl tir::Expr<tir::If> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        if let Some(err) = can_unify_to(tp.constraints(self.condition), &Type::Bool.into()).err() {
            let mut error = Error::new("Expected boolean condition in if expression");
            let range = driver.get_range(self.condition);
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
            driver.errors.push(error);
        }
        let constraints = tp.constraints(self.then_expr).intersect_with(tp.constraints(self.else_expr));

        if constraints.solve().is_err() {
            // TODO: handle void expressions, which don't have appropriate source location info.
            driver.errors.push(
                Error::new("Failed to unify branches of if expression")
                    .adding_primary_range(driver.get_range(self.then_expr), "first terminal expression here")
                    .adding_primary_range(driver.get_range(self.else_expr), "second terminal expression here")
            );
        }
        *tp.constraints_mut(self.id) = constraints;
    }
}

impl tir::Expr<tir::Do> {
    fn run_pass_1(&self, _driver: &mut Driver, tp: &mut impl TypeProvider) {
        *tp.constraints_mut(self.id) = tp.constraints(self.terminal_expr).clone();
    }
}

impl tir::Stmt {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        let constraints = tp.constraints_mut(self.root_expr);
        if let Some(err) = can_unify_to(&constraints, &Type::Void.into()).err() {
            let mut error = Error::new("statements must return void");
            let range = driver.get_range(self.root_expr);
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
            driver.errors.push(error);
        }
        constraints.set_to(Type::Void);
    }
}


impl Driver {
    pub fn decl_type<'a>(&'a self, id: DeclId, tp: &'a impl TypeProvider) -> &Type {
        self.code.hir_code.explicit_tys[id].map(|ty| tp.get_evaluated_type(ty)).unwrap_or(&tp.fw_decl_types(id).ty)
    }

    fn run_pass_1(&mut self, unit: &UnitItems, unit_kind: UnitKind, start_level: u32, tp: &mut impl TypeProvider) {
        // Pass 1: propagate info down from leaves to roots
        if tp.debug() {
            println!(
                "===============TYPECHECKING {}: PASS 1===============",
                unit_string(unit_kind),
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
        lit_pass_1(tp, &unit.dec_lits, BuiltinTraits::DEC, Type::f64());
        lit_pass_1(tp, &unit.str_lits, BuiltinTraits::STR, Type::u8().ptr());
        lit_pass_1(tp, &unit.char_lits, BuiltinTraits::CHAR, Type::u8().ptr());
        for &item in &unit.const_tys {
            *tp.constraints_mut(item) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Ty.into()]), None);
            *tp.ty_mut(item) = Type::Ty;
        }
        for &param in &unit.generic_params {
            *tp.decl_type_mut(param) = Type::Ty.into();
        }
        for level in start_level..unit.num_levels() {
            for item in unit.assigned_decls.get_level(level) {
                item.run_pass_1(self, tp);
            }
            for item in unit.assignments.get_level(level) {
                item.run_pass_1(self, tp);
            }
            for item in unit.casts.get_level(level) {
                item.run_pass_1(self, tp);
            }
            for item in unit.whiles.get_level(level) {
                item.run_pass_1(self, tp);
            }
            for item in unit.explicit_rets.get_level(level) {
                item.run_pass_1(self, tp);
            }
            for item in unit.modules.get_level(level) {
                item.run_pass_1(self, tp);
            }
            for item in unit.imports.get_level(level) {
                item.run_pass_1(self, tp);
            }
            for item in unit.decl_refs.get_level(level) {
                item.run_pass_1(self, tp);
            }
            for item in unit.addr_ofs.get_level(level) {
                item.run_pass_1(self, tp);
            }
            for item in unit.derefs.get_level(level) {
                item.run_pass_1(self, tp);
            }
            for item in unit.pointers.get_level(level) {
                item.run_pass_1(self, tp);
            }
            for item in unit.structs.get_level(level) {
                item.run_pass_1(self, tp);
            }
            for item in unit.struct_lits.get_level(level) {
                item.run_pass_1(self, tp);
            }
            for item in unit.ifs.get_level(level) {
                item.run_pass_1(self, tp);
            }
            for item in unit.dos.get_level(level) {
                item.run_pass_1(self, tp);
            }
            tp.debug_output(self, level as usize);
        }

        // Must be here, because checks that statements all can unify to void
        for item in &unit.stmts {
            item.run_pass_1(self, tp);
        }
    }

    fn run_pass_2(&mut self, unit: &UnitItems, unit_kind: UnitKind, tp: &mut impl TypeProvider) {
        if tp.debug() {
            println!(
                "===============TYPECHECKING {}: PASS 2===============",
                unit_string(unit_kind)
            );
        }
        for level in (0..unit.num_levels()).rev() {
            for i in 0..unit.assigned_decls.level_len(level) {
                let item = unit.assigned_decls.at(level, i);
                let decl_id = item.decl_id;
                let root_expr = item.root_expr;
                let ty = tp.fetch_decl_type(self, decl_id, None).ty.clone();
                tp.constraints_mut(root_expr).set_to(ty);
            }
            for item in unit.ret_groups.get_level(level) {
                let ty = tp.get_evaluated_type(item.ty).clone();
                for &expr in &item.exprs {
                    if let Some(err) = can_unify_to(tp.constraints(expr), &QualType::from(&ty)).err() {
                        let range = self.get_range(expr);
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
                if can_unify_to(tp.constraints(item.condition), &Type::Bool.into()).is_ok() {
                    tp.constraints_mut(item.condition).set_to(Type::Bool);
                } else {
                    panic!("Expected boolean condition in while expression");
                }
            }
            for item in unit.casts.get_level(level) {
                let ty = tp.get_evaluated_type(item.ty).clone();
                let constraints = tp.constraints_mut(item.expr);
                let ty_and_method: Result<(Type, CastMethod), Vec<&QualType>> = if can_unify_to(&constraints, &QualType::from(&ty)).is_ok() {
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
                        self.errors.push(Error::new("Invalid cast!").adding_primary_range(self.get_range(item.id), "cast here"));
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
                overloads.retain(|&overload| {
                    tp.fetch_decl_type(self, overload, Some(item.decl_ref_id)).trivially_convertible_to(&ty)
                });
                let pref = tp.preferred_overload(item.decl_ref_id);

                let overload = if !overloads.is_empty() {
                    let overload = pref
                        .filter(|overload| overloads.contains(overload))
                        .unwrap_or_else(|| overloads[0]);
                    let overload_is_function = match self.code.hir_code.decls[overload] {
                        hir::Decl::Computed { .. } => true,
                        hir::Decl::Intrinsic { function_like, .. } => function_like,
                        _ => false,
                    };
                    let has_parens = self.code.hir_code.decl_refs[item.decl_ref_id].has_parens;
                    if has_parens && !overload_is_function {
                        self.errors.push(
                            Error::new("reference to non-function must not have parentheses")
                                .adding_primary_range(self.get_range(item.id), "")
                        );
                    } else if !has_parens && overload_is_function {
                        self.errors.push(
                            Error::new("function call must have parentheses")
                                .adding_primary_range(self.get_range(item.id), "")
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
                        Error::new("no matching overload for declaration")
                            .adding_primary_range(self.get_range(item.id), "expression here")
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
            for item in unit.structs.get_level(level) {
                for &field_ty in &item.field_tys {
                    let field_type = tp.constraints(field_ty).solve().map(|ty| ty.ty).unwrap_or(Type::Error);
                    // Don't bother checking if it's a type, because we already did that in pass 1
                    tp.constraints_mut(field_ty).set_to(field_type);
                }
                let ty = tp.constraints(item.id).solve().expect("Ambiguous type for struct expression");
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
            for item in unit.struct_lits.get_level(level) {
                let ty = tp.constraints(item.id).solve().unwrap();
                *tp.ty_mut(item.id) = ty.ty.clone();

                // Yay borrow checker:
                if let Some(lit) = tp.struct_lit(item.struct_lit_id).clone() {
                    let fields = &self.code.hir_code.structs[lit.strukt].fields;
                    debug_assert_eq!(lit.fields.len(), fields.len());

                    for i in 0..fields.len() {
                        let field = fields[i];
                        let field = self.code.hir_code.field_decls[field].decl;
                        let field_ty = tp.fetch_decl_type(self, field, None).ty.clone();

                        tp.constraints_mut(lit.fields[i]).set_to(field_ty);
                    }
                }
            }
            if level > 0 {
                tp.debug_output(self, level as usize);
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
        tp.debug_output(self, 0);
    }

    pub fn get_real_type_provider(&self, dbg: bool) -> RealTypeProvider {
        // Assign the type of the void expression to be void.
        let mut tp = RealTypeProvider::new(dbg, self);
        *tp.constraints_mut(hir::VOID_EXPR) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Void.into()]), None);
        *tp.ty_mut(hir::VOID_EXPR) = Type::Void;
        tp
    }

    pub fn type_check(&mut self, units: &Units, tp: &mut RealTypeProvider) {
        for (num, unit) in units.units.iter().enumerate() {
            // Pass 1: propagate info down from leaves to roots
            self.run_pass_1(&unit.items, UnitKind::Normal(num), 0, tp);
            
            // Pass 2: propagate info up from roots to leaves
            self.run_pass_2(&unit.items, UnitKind::Normal(num), tp);

            self.flush_errors();

            for i in 0..unit.eval_dependees.len() {
                let expr = unit.eval_dependees[i];
                let val = self.eval_expr(expr, tp);
                tp.insert_eval_result(expr, val);
            }
        }

        for (num, unit) in units.mock_units.iter().enumerate() {
            let mut mock_tp = MockTypeProvider::new(tp);
            self.run_pass_1(&unit.items, UnitKind::Mock(num), 0, &mut mock_tp);
            let one_of = mock_tp.constraints(unit.main_expr)
                .one_of().iter()
                .map(|ty| ty.ty.clone())
                .collect::<Vec<_>>();
            for ty in one_of {
                let ns = match ty {
                    Type::Mod => {
                        self.run_pass_2(&unit.items, UnitKind::Mock(num), &mut mock_tp);
                        let module = self.eval_expr(unit.main_expr, &mut mock_tp);
                        match module {
                            Const::Mod(scope) => ExprNamespace::Mod(scope),
                            _ => panic!("Unexpected const kind, expected module!"),
                        }
                    },
                    Type::Struct(strukt) => ExprNamespace::Struct(strukt),
                    _ => continue,
                };
                self.tir.expr_namespaces.entry(unit.main_expr).or_default().push(ns);
            }
        }
    }
}