use smallvec::smallvec;

mod constraints;
use constraints::{ConstraintList, UnificationError};

use crate::error::Error;
use crate::tir;
use crate::builder::{ExprId, DeclId, DeclRefId, CastId};
use crate::index_vec::Idx;
use crate::ty::{BuiltinTraits, Type, QualType, IntWidth};
use crate::index_vec::IdxVec;
use crate::dep_vec;
use crate::source_info::{SourceFile, CommentatedSourceRange};

#[derive(Clone, Debug)]
pub enum CastMethod {
    Noop,
    Reinterpret,
    Int,
    Float,
    FloatToInt,
    IntToFloat,
}

struct TypeChecker<'src> {
    /// The input TIR program
    prog: tir::Program<'src>,
    /// The type of each expression
    types: IdxVec<Type, ExprId>,
    /// The constraints on each expression's type
    constraints: IdxVec<ConstraintList, ExprId>,
    /// A copy of the constraints, used for debugging the typechecker
    constraints_copy: IdxVec<ConstraintList, ExprId>,
    /// The preferred overload for each decl ref (currently only ever originates from literals)
    preferred_overloads: IdxVec<Option<DeclId>, DeclRefId>,
    /// The selected overload for each decl ref
    selected_overloads: IdxVec<Option<DeclId>, DeclRefId>,
    /// The cast method for each cast expression
    cast_methods: IdxVec<CastMethod, CastId>,

    source_file: &'src SourceFile,
    debug: bool,
}

impl<'src> TypeChecker<'src> {
    fn debug_output(&mut self, level: usize) {
        if !self.debug { return; }
        println!("LEVEL {}", level);
        assert_eq!(self.constraints.len(), self.constraints_copy.len());
        for i in 0..self.constraints.len() {
            let i = ExprId::new(i);
            let new_constraints = &self.constraints[i];
            let old_constraints = &mut self.constraints_copy[i];
            if new_constraints != old_constraints {
                self.source_file.print_commentated_source_ranges(&mut [
                    CommentatedSourceRange::new(self.prog.source_ranges[i].clone(), "", '-')
                ]);
                old_constraints.print_diff(new_constraints);
                *old_constraints = new_constraints.clone();
                println!("============================================================================================\n")
            }
        }
    }
}

pub struct Program {
    pub types: IdxVec<Type, ExprId>,
    pub decl_types: IdxVec<Type, DeclId>,
    pub overloads: IdxVec<Option<DeclId>, DeclRefId>,
    pub cast_methods: IdxVec<CastMethod, CastId>,
}

#[inline(never)]
pub fn type_check(prog: tir::Program, source_file: &SourceFile, debug: bool) -> (Program, Vec<Error>) {
    let mut tc = TypeChecker {
        prog,
        types: IdxVec::new(),
        constraints: IdxVec::new(),
        constraints_copy: IdxVec::new(),
        preferred_overloads: IdxVec::new(),
        selected_overloads: IdxVec::new(),
        cast_methods: IdxVec::new(),
        source_file,
        debug,
    };
    let mut errs = Vec::new();
    tc.types.resize_with(tc.prog.num_exprs, Default::default);
    tc.constraints.resize_with(tc.prog.num_exprs, Default::default);
    if debug {
        tc.constraints_copy.resize_with(tc.prog.num_exprs, Default::default);
    }
    tc.selected_overloads.resize_with(tc.prog.overloads.len(), || None);
    tc.preferred_overloads.resize_with(tc.prog.overloads.len(), || None);
    tc.cast_methods.resize_with(tc.prog.casts.len(), || CastMethod::Noop);

    // Extend arrays as needed so they all have the same number of levels.
    let levels = dep_vec::unify_sizes(&mut [
        &mut tc.prog.assigned_decls, &mut tc.prog.assignments, &mut tc.prog.decl_refs, 
        &mut tc.prog.addr_ofs, &mut tc.prog.derefs, &mut tc.prog.ifs,
        &mut tc.prog.dos,
    ]);

    // Assign the type of the void expression to be void.
    tc.constraints[tc.prog.void_expr] = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Void.into()]), None);
    tc.types[tc.prog.void_expr] = Type::Void;

    // Pass 1: propagate info down from leaves to roots
    if debug { println!("===============TYPECHECKING: PASS 1==============="); }
    fn independent_pass_1<T>(constraints: &mut IdxVec<ConstraintList, ExprId>, tys: &mut IdxVec<Type, ExprId>, exprs: &[T], data: impl Fn(&T) -> (ExprId, Type)) {
        for item in exprs {
            let (id, ty) = data(item);
            constraints[id] = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![ty.clone().into()]), None);
            tys[id] = ty;
        }
    }
    independent_pass_1(&mut tc.constraints, &mut tc.types, &tc.prog.explicit_rets, |&id| (id, Type::Never));
    independent_pass_1(&mut tc.constraints, &mut tc.types, &tc.prog.whiles, |item| (item.id, Type::Void));
    independent_pass_1(&mut tc.constraints, &mut tc.types, &tc.prog.casts, |item| (item.id, item.ty.clone()));

    fn lit_pass_1(constraints: &mut IdxVec<ConstraintList, ExprId>, lits: &[ExprId], trait_impls: BuiltinTraits, pref: Type) {
        for &item in lits {
            constraints[item] = ConstraintList::new(
                trait_impls, 
                None,
                Some(pref.clone().into())
            );
        }
    }
    lit_pass_1(&mut tc.constraints, &tc.prog.int_lits, BuiltinTraits::INT, Type::i32());
    lit_pass_1(&mut tc.constraints, &tc.prog.dec_lits, BuiltinTraits::DEC, Type::i32());
    lit_pass_1(&mut tc.constraints, &tc.prog.str_lits, BuiltinTraits::STR, Type::u8().ptr());
    lit_pass_1(&mut tc.constraints, &tc.prog.char_lits, BuiltinTraits::CHAR, Type::u8().ptr());
    for level in 0..levels {
        for item in tc.prog.assigned_decls.get_level(level) {
            let constraints = &tc.constraints[item.root_expr];
            let ty = if let Some(explicit_ty) = &item.explicit_ty {
                if let Some(err) = constraints.can_unify_to(&explicit_ty.into()).err() {
                    let range = tc.prog.source_ranges[item.root_expr].clone();
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
                    errs.push(error);
                }
                explicit_ty.clone()
            } else {
                constraints.solve().expect("Ambiguous type for assigned declaration").ty
            };
            tc.prog.decls[item.decl_id].ret_ty.ty = ty;
        }
        for item in tc.prog.assignments.get_level(level) {
            tc.constraints[item.id].set_to(Type::Void);
            tc.types[item.id] = Type::Void;
        }
        for item in tc.prog.decl_refs.get_level(level) {
            // Filter overloads that don't match the constraints of the parameters.
            // P.S. These borrows are only here because the borrow checker is dumb
            let decls = &tc.prog.decls;
            let constraints = &tc.constraints;
            // Rule out overloads that don't match the arguments
            tc.prog.overloads[item.decl_ref_id].retain(|&overload| {
                assert_eq!(decls[overload].param_tys.len(), item.args.len());
                for (constraints, ty) in item.args.iter().map(|&arg| &constraints[arg]).zip(&decls[overload].param_tys) {
                    if constraints.can_unify_to(&ty.into()).is_err() { return false; }
                }
                true
            });

            let one_of = tc.prog.overloads[item.decl_ref_id].iter()
                .map(|&overload| decls[overload].ret_ty.clone())
                .collect();
            let mut pref = None;
            'find_preference: for (i, &arg) in item.args.iter().enumerate() {
                if let Some(ty) = tc.constraints[arg].preferred_type() {
                    for &overload in &tc.prog.overloads[item.decl_ref_id] {
                        let decl = &decls[overload];
                        if ty.ty.trivially_convertible_to(&decl.param_tys[i]) {
                            pref = Some(decl.ret_ty.clone());
                            tc.preferred_overloads[item.decl_ref_id] = Some(overload);
                            break 'find_preference;
                        }
                    }
                }
            }
            // if one_of.is_empty() {
            //     println!("")
            // }
            tc.constraints[item.id] = ConstraintList::new(BuiltinTraits::empty(), Some(one_of), pref);
        }
        for item in tc.prog.addr_ofs.get_level(level) {
            let constraints = tc.constraints[item.expr].filter_map(|ty| {
                if item.is_mut && !ty.is_mut { return None; }
                Some(
                    QualType::from(
                        ty.ty.clone().ptr_with_mut(item.is_mut)
                    )
                )
            });
            tc.constraints[item.id] = constraints;
        }
        for item in tc.prog.derefs.get_level(level) {
            let constraints = tc.constraints[item.expr].filter_map(|ty| {
                if let Type::Pointer(pointee) = &ty.ty {
                    Some(pointee.as_ref().clone())
                } else {
                    None
                }
            });
            tc.constraints[item.id] = constraints;
        }
        for item in tc.prog.ifs.get_level(level) {
            if let Some(err) = tc.constraints[item.condition].can_unify_to(&Type::Bool.into()).err() {
                let mut error = Error::new("Expected boolean condition in if expression");
                let range = tc.prog.source_ranges[item.condition].clone();
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
                errs.push(error);
            }
            let constraints = tc.constraints[item.then_expr].intersect_with(&tc.constraints[item.else_expr]);
            
            if constraints.solve().is_err() {
                // TODO: handle void expressions, which don't have appropriate source location info.
                errs.push(
                    Error::new("Failed to unify branches of if expression")
                        .adding_primary_range(tc.prog.source_ranges[item.then_expr].clone(), "first terminal expression here")
                        .adding_primary_range(tc.prog.source_ranges[item.else_expr].clone(), "second terminal expression here")
                );
            }
            tc.constraints[item.id] = constraints;
        }
        for item in tc.prog.dos.get_level(level) {
            tc.constraints[item.id] = tc.constraints[item.terminal_expr].clone();
        }
        tc.debug_output(level as usize);
    }

    // Pass 2: propagate info up from roots to leaves
    if debug { println!("===============TYPECHECKING: PASS 2==============="); }
    for item in &tc.prog.stmts {
        let constraints = &mut tc.constraints[item.root_expr];
        if let Some(err) = constraints.can_unify_to(&Type::Void.into()).err() {
            let mut error = Error::new("statements must return void");
            let range = tc.prog.source_ranges[item.root_expr].clone();
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
            errs.push(error);
        }
        constraints.set_to(Type::Void);
    }

    for group in &tc.prog.ret_groups {
        for &expr in &group.exprs {
            if let Some(err) = tc.constraints[expr].can_unify_to(&QualType::from(&group.ty)).err() {
                let range = tc.prog.source_ranges[expr].clone();
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
                errs.push(error);
            }

            // Assume we panic above unless the returned expr can unify to the return type
            tc.constraints[expr].set_to(group.ty.clone());
        }
    }
    for item in &tc.prog.whiles {
        if tc.constraints[item.condition].can_unify_to(&Type::Bool.into()).is_ok() {
            tc.constraints[item.condition].set_to(Type::Bool);
        } else {
            panic!("Expected boolean condition in while expression");
        }
    }
    for item in &tc.prog.casts {
        let constraints = &mut tc.constraints[item.expr];
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
                tc.cast_methods[item.cast_id] = method;
            },
            Err(_) => {
                errs.push(Error::new("Invalid cast!").adding_primary_range(tc.prog.source_ranges[item.id].clone(), "cast here"));
                constraints.set_to(Type::Error);
                tc.cast_methods[item.cast_id] = CastMethod::Noop;
            }
        }
    }
    for level in (0..levels).rev() {
        for item in tc.prog.assigned_decls.get_level(level) {
            tc.constraints[item.root_expr].set_to(tc.prog.decls[item.decl_id].ret_ty.ty.clone());
        }
        for item in tc.prog.assignments.get_level(level) {
            let (lhs, rhs) = tc.constraints.index_mut(item.lhs, item.rhs);
            lhs.lopsided_intersect_with(rhs);
        }
        for item in tc.prog.decl_refs.get_level(level) {
            let ty = tc.constraints[item.id].solve().unwrap_or(Type::Error.into());
            tc.types[item.id] = ty.ty.clone();

            // P.S. These borrows are only here because the borrow checker is dumb
            let decls = &tc.prog.decls;
            let overloads = &mut tc.prog.overloads[item.decl_ref_id];
            overloads.retain(|&overload| {
                decls[overload].ret_ty
                    .trivially_convertible_to(&ty)
            });
            let pref = tc.preferred_overloads[item.decl_ref_id];

            let overload = if !overloads.is_empty() {
                let overload = pref
                    .filter(|overload| overloads.contains(overload))
                    .unwrap_or_else(|| overloads[0]);
                let decl = &decls[overload];
                for (i, &arg) in item.args.iter().enumerate() {
                    tc.constraints[arg].set_to(decl.param_tys[i].clone());
                }
                Some(overload)
            } else {
                errs.push(
                    Error::new("ambiguous overload for declaration")
                        .adding_primary_range(tc.prog.source_ranges[item.id].clone(), "expression here")
                );
                for &arg in &item.args {
                    tc.constraints[arg].set_to(Type::Error);
                }
                None
            };
            tc.selected_overloads[item.decl_ref_id] = overload;
        }
        for item in tc.prog.addr_ofs.get_level(level) {
            let pointer_ty = tc.constraints[item.id].solve()
                .map(|ty| ty.ty)
                .unwrap_or(Type::Error);
            let pointee_ty = match pointer_ty {
                Type::Pointer(ref pointee) => pointee.as_ref().clone(),
                Type::Error => Type::Error.into(),
                _ => panic!("unexpected non-pointer, non-error type for addr of expression"),
            };
            tc.constraints[item.expr].set_to(pointee_ty);
            tc.types[item.id] = pointer_ty;
        }
        for item in tc.prog.derefs.get_level(level) {
            let mut ty = tc.constraints[item.id].solve().unwrap_or(Type::Error.into());
            tc.types[item.id] = ty.ty.clone();

            if ty.ty != Type::Error {
                ty = ty.ptr().into();
            }
            tc.constraints[item.expr].set_to(ty);
        }
        for item in tc.prog.ifs.get_level(level) {
            let condition = &mut tc.constraints[item.condition];
            let condition_ty = condition.solve().map(|ty| ty.ty).unwrap_or(Type::Error);
            // Don't bother checking if bool, because we already did that in pass 1
            tc.constraints[item.condition].set_to(condition_ty);
            let ty = tc.constraints[item.id].solve().expect("ambiguous type for if expression");
            tc.types[item.id] = ty.ty.clone();
            tc.constraints[item.then_expr].set_to(ty.clone());
            tc.constraints[item.else_expr].set_to(ty);
        }
        for item in tc.prog.dos.get_level(level) {
            let ty = tc.constraints[item.id].solve().expect("Ambiguous type for do expression");
            tc.types[item.id] = ty.ty.clone();
            tc.constraints[item.terminal_expr].set_to(ty);
        }
        if level > 0 {
            tc.debug_output(level as usize);
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
    lit_pass_2(&tc.constraints, &mut tc.types, &tc.prog.int_lits, "integer");
    lit_pass_2(&tc.constraints, &mut tc.types, &tc.prog.dec_lits, "decimal");
    lit_pass_2(&tc.constraints, &mut tc.types, &tc.prog.str_lits, "string");
    lit_pass_2(&tc.constraints, &mut tc.types, &tc.prog.char_lits, "character");
    tc.debug_output(0);

    //println!("Types: {:#?}", tc.types);
    //println!("Program: {:#?}", tc.prog);
    //println!("Decl types: {:#?}", tc.prog.local_decls);
    //println!("Constraints: {:#?}", tc.constraints);

    let mut decl_types = IdxVec::new();
    decl_types.reserve(tc.prog.decls.len());
    for decl in &tc.prog.decls {
        decl_types.push(decl.ret_ty.ty.clone());
    }

    let prog = Program {
        types: tc.types,
        decl_types,
        overloads: tc.selected_overloads,
        cast_methods: tc.cast_methods,
    };
    (prog, errs)
}