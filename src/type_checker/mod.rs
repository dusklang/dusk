use smallvec::smallvec;

mod constraints;
use constraints::{ConstraintList, LiteralType, UnificationError};

use crate::error::Error;
use crate::tir;
use crate::builder::{ExprId, DeclId, DeclRefId, CastId};
use crate::ty::{Type, QualType, IntWidth};
use crate::index_vec::IdxVec;
use crate::dep_vec;

#[derive(Clone, Debug)]
pub enum CastMethod {
    Noop,
    Reinterpret,
    Int,
    Float,
    FloatToInt,
    IntToFloat,
}

struct TypeChecker {
    /// The input TIR program
    prog: tir::Program,
    /// The type of each expression
    types: IdxVec<Type, ExprId>,
    /// The constraints on each expression's type
    constraints: IdxVec<ConstraintList, ExprId>,
    /// The preferred overload for each decl ref (currently only ever originates from literals)
    preferred_overloads: IdxVec<Option<DeclId>, DeclRefId>,
    /// The selected overload for each decl ref
    selected_overloads: IdxVec<Option<DeclId>, DeclRefId>,
    /// The cast method for each cast expression
    cast_methods: IdxVec<CastMethod, CastId>,
}

pub struct Program {
    pub types: IdxVec<Type, ExprId>,
    pub decl_types: IdxVec<Type, DeclId>,
    pub overloads: IdxVec<Option<DeclId>, DeclRefId>,
    pub cast_methods: IdxVec<CastMethod, CastId>,
}

#[inline(never)]
pub fn type_check(prog: tir::Program) -> (Program, Vec<Error>) {
    let mut tc = TypeChecker {
        prog,
        types: IdxVec::new(),
        constraints: IdxVec::new(),
        preferred_overloads: IdxVec::new(),
        selected_overloads: IdxVec::new(),
        cast_methods: IdxVec::new(),
    };
    let mut errs = Vec::new();
    tc.types.resize_with(tc.prog.num_exprs, Default::default);
    tc.constraints.resize_with(tc.prog.num_exprs, Default::default);
    tc.selected_overloads.resize_with(tc.prog.overloads.len(), || None);
    tc.preferred_overloads.resize_with(tc.prog.overloads.len(), || None);
    tc.cast_methods.resize_with(tc.prog.casts.len(), || CastMethod::Noop);

    // Extend arrays as needed so they all have the same number of levels.
    let levels = dep_vec::unify_sizes(&mut [
        &mut tc.prog.tree.assigned_decls, &mut tc.prog.tree.assignments, &mut tc.prog.tree.decl_refs, 
        &mut tc.prog.tree.addr_ofs, &mut tc.prog.tree.derefs, &mut tc.prog.tree.ifs,
        &mut tc.prog.tree.dos,
    ]);

    // Assign the type of the void expression to be void.
    tc.constraints[tc.prog.void_expr] = ConstraintList::new(None, Some(smallvec![Type::Void.into()]), None);
    tc.types[tc.prog.void_expr] = Type::Void;

    // Pass 1: propagate info down from leaves to roots
    fn independent_pass_1<T>(constraints: &mut IdxVec<ConstraintList, ExprId>, tys: &mut IdxVec<Type, ExprId>, exprs: &[T], data: impl Fn(&T) -> (ExprId, Type)) {
        for item in exprs {
            let (id, ty) = data(item);
            constraints[id] = ConstraintList::new(None, Some(smallvec![ty.clone().into()]), None);
            tys[id] = ty;
        }
    }
    independent_pass_1(&mut tc.constraints, &mut tc.types, &tc.prog.explicit_rets, |&id| (id, Type::Never));
    independent_pass_1(&mut tc.constraints, &mut tc.types, &tc.prog.whiles, |item| (item.id, Type::Void));
    independent_pass_1(&mut tc.constraints, &mut tc.types, &tc.prog.casts, |item| (item.id, item.ty.clone()));

    fn lit_pass_1(constraints: &mut IdxVec<ConstraintList, ExprId>, lits: &[ExprId], lit_ty: LiteralType) {
        for &item in lits {
            constraints[item] = ConstraintList::new(
                Some(lit_ty), 
                None,
                Some(lit_ty.preferred_type().into())
            );
        }
    }
    lit_pass_1(&mut tc.constraints, &tc.prog.int_lits, LiteralType::Int);
    lit_pass_1(&mut tc.constraints, &tc.prog.dec_lits, LiteralType::Dec);
    lit_pass_1(&mut tc.constraints, &tc.prog.str_lits, LiteralType::Str);
    lit_pass_1(&mut tc.constraints, &tc.prog.char_lits, LiteralType::Char);
    for level in 0..levels {
        for item in tc.prog.tree.assigned_decls.get_level(level) {
            let constraints = &tc.constraints[item.root_expr];
            let ty = if let Some(explicit_ty) = &item.explicit_ty {
                assert!(constraints.can_unify_to(&explicit_ty.into()).is_ok());
                explicit_ty.clone()
            } else {
                constraints.solve().expect("Ambiguous type for assigned declaration").ty
            };
            tc.prog.decls[item.decl_id].ret_ty.ty = ty;
        }
        for item in tc.prog.tree.assignments.get_level(level) {
            tc.constraints[item.id].set_to(Type::Void);
            tc.types[item.id] = Type::Void;
        }
        for item in tc.prog.tree.decl_refs.get_level(level) {
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
            tc.constraints[item.id] = ConstraintList::new(None, Some(one_of), pref);
        }
        for item in tc.prog.tree.addr_ofs.get_level(level) {
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
        for item in tc.prog.tree.derefs.get_level(level) {
            let constraints = tc.constraints[item.expr].filter_map(|ty| {
                if let Type::Pointer(pointee) = &ty.ty {
                    Some(pointee.as_ref().clone())
                } else {
                    None
                }
            });
            tc.constraints[item.id] = constraints;
        }
        for item in tc.prog.tree.ifs.get_level(level) {
            if tc.constraints[item.condition].can_unify_to(&Type::Bool.into()).is_err() {
                panic!("Expected boolean condition in if expression");
            }
            let constraints = tc.constraints[item.then_expr].intersect_with(&tc.constraints[item.else_expr]);
            assert!(constraints.solve().is_ok(), "Failed to unify branches of if expression");
            tc.constraints[item.id] = constraints;
        }
        for item in tc.prog.tree.dos.get_level(level) {
            tc.constraints[item.id] = tc.constraints[item.terminal_expr].clone();
        }
    }

    // Pass 2: propagate info up from roots to leaves
    for item in &tc.prog.stmts {
        let constraints = &mut tc.constraints[item.root_expr];
        if constraints.can_unify_to(&Type::Void.into()).is_err() {
            panic!("standalone expressions must return void");
        }
        constraints.set_to(Type::Void);
    }

    for group in tc.prog.ret_groups {
        for expr in group.exprs {
            use UnificationError::*;
            use LiteralType::*;

            match tc.constraints[expr].can_unify_to(&QualType::from(&group.ty)) {
                Ok(()) => {}
                Err(Literal(Dec)) => panic!("expected return value of {:?}, found decimal literal", group.ty),
                Err(Literal(Int)) => panic!("expected return value of {:?}, found integer literal", group.ty),
                Err(Literal(Str)) => panic!("expected return value of {:?}, found string literal", group.ty),
                Err(Literal(Char)) => panic!("expected return value of {:?}, found character literal", group.ty),
                Err(InvalidChoice(choices)) => panic!("expected return value of {:?}, found {:?}", group.ty, choices),
                Err(Immutable) => panic!("COMPILER BUG: unexpected mutable return type"),
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
        tc.cast_methods[item.cast_id] = if constraints.can_unify_to(&item.ty.clone().into()).is_ok() {
            constraints.set_to(item.ty.clone());
            CastMethod::Noop
        } else if let Type::Pointer(dest_pointee_ty) = &item.ty {
            let dest_pointee_ty = dest_pointee_ty.as_ref();
            let src_ty = constraints.max_ranked_type(|ty|
                match ty.ty {
                    Type::Pointer(ref pointee) if pointee.is_mut || dest_pointee_ty.is_mut => 2,
                    Type::Int { width, .. } if width == IntWidth::Pointer => 1,
                    _ => 0,
                }
            ).expect("Invalid cast!").clone();
            constraints.set_to(src_ty);
            CastMethod::Reinterpret
        } else if let Type::Int { width, .. } = item.ty {
            let (src_ty, method) = constraints.max_ranked_type_with_assoc_data(|ty|
                match ty.ty {
                    Type::Int { .. } => (3, CastMethod::Int),
                    Type::Float { .. } => (2, CastMethod::FloatToInt),
                    Type::Pointer(_) if width == IntWidth::Pointer => (1, CastMethod::Reinterpret),
                    _ => (0, CastMethod::Noop),
                }
            ).expect("Invalid cast!");
            let src_ty = src_ty.clone();
            constraints.set_to(src_ty);
            method
        } else if let Type::Float { .. } = item.ty {
            let (src_ty, method) = constraints.max_ranked_type_with_assoc_data(|ty|
                match ty.ty {
                    Type::Float { .. } => (2, CastMethod::Float),
                    Type::Int { .. } => (1, CastMethod::IntToFloat),
                    _ => (0, CastMethod::Noop),
                }
            ).expect("Invalid cast!");
            let src_ty = src_ty.clone();
            constraints.set_to(src_ty);
            method
        } else {
            panic!("Invalid cast!")
        };
    }
    for level in (0..levels).rev() {
        for item in tc.prog.tree.assigned_decls.get_level(level) {
            tc.constraints[item.root_expr].set_to(tc.prog.decls[item.decl_id].ret_ty.ty.clone());
        }
        for item in tc.prog.tree.assignments.get_level(level) {
            let (lhs, rhs) = tc.constraints.index_mut(item.lhs, item.rhs);
            lhs.lopsided_intersect_with(rhs);
        }
        for item in tc.prog.tree.decl_refs.get_level(level) {
            let ty = tc.constraints[item.id].solve().expect("Ambiguous type for expression");
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
        for item in tc.prog.tree.addr_ofs.get_level(level) {
            let ty = tc.constraints[item.id].solve().unwrap().ty;
            match ty {
                Type::Pointer(ref pointee) => tc.constraints[item.expr].set_to(pointee.as_ref().clone()),
                _ => panic!("unexpected non-pointer for addr of expression"),
            }
            tc.types[item.id] = ty;
        }
        for item in tc.prog.tree.derefs.get_level(level) {
            let ty = tc.constraints[item.id].solve().unwrap();
            tc.constraints[item.expr].set_to(ty.clone().ptr());
            tc.types[item.id] = ty.ty;
        }
        for item in tc.prog.tree.ifs.get_level(level) {
            // We already verified that the condition unifies to bool in pass 1
            tc.constraints[item.condition].set_to(Type::Bool);
            let ty = tc.constraints[item.id].solve().expect("ambiguous type for if expression");
            tc.types[item.id] = ty.ty.clone();
            tc.constraints[item.then_expr].set_to(ty.clone());
            tc.constraints[item.else_expr].set_to(ty);
        }
        for item in tc.prog.tree.dos.get_level(level) {
            let ty = tc.constraints[item.id].solve().expect("Ambiguous type for do expression");
            tc.types[item.id] = ty.ty.clone();
            tc.constraints[item.terminal_expr].set_to(ty);
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