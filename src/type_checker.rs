use smallvec::smallvec;

use crate::error::Error;
use crate::tir::{self, Decl, Expr};
use crate::builder::{ExprId, DeclId, DeclRefId};
use crate::ty::{Type, QualType};
use crate::index_vec::IdxVec;
use crate::source_info::SourceRange;
use crate::dep_vec;
use crate::constraints::{ConstraintList, LiteralType, UnificationError};

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
}

pub struct Program {
    pub types: IdxVec<Type, ExprId>,
    pub overloads: IdxVec<Option<DeclId>, DeclRefId>,
}

#[inline(never)]
pub fn type_check(prog: tir::Program) -> (Program, Vec<Error>) {
    let mut tc = TypeChecker {
        prog,
        types: IdxVec::new(),
        constraints: IdxVec::new(),
        preferred_overloads: IdxVec::new(),
        selected_overloads: IdxVec::new(),
    };
    let mut errs = Vec::new();
    tc.types.resize_with(tc.prog.num_exprs, Default::default);
    tc.constraints.resize_with(tc.prog.num_exprs, Default::default);
    tc.selected_overloads.resize_with(tc.prog.overloads.len(), || None);
    tc.preferred_overloads.resize_with(tc.prog.overloads.len(), || None);

    // Extend arrays as needed so they all have the same number of levels.
    let levels = dep_vec::unify_sizes(&mut [
        &mut tc.prog.assigned_decls, &mut tc.prog.assignments, &mut tc.prog.decl_refs, 
        &mut tc.prog.addr_ofs, &mut tc.prog.derefs, &mut tc.prog.ifs,
        &mut tc.prog.dos,
    ]);

    // Assign the type of the void expression to be void.
    tc.constraints[tc.prog.void_expr].one_of = smallvec![Type::Void.into()];
    tc.types[tc.prog.void_expr] = Type::Void;

    // Pass 1: propagate info down from leaves to roots
    for item in &tc.prog.rets {
        let constraints = &mut tc.constraints[item.id];
        constraints.one_of = smallvec![Type::Never.into()];
        tc.types[item.id] = Type::Never;
    }
    for item in &tc.prog.whiles {
        let constraints = &mut tc.constraints[item.id];
        constraints.one_of = smallvec![Type::Void.into()];
        tc.types[item.id] = Type::Void;
    }
    fn lit_pass_1<T>(constraints: &mut IdxVec<ConstraintList, ExprId>, lits: &Vec<Expr<T>>, lit_ty: LiteralType) {
        for item in lits {
            let constraints = &mut constraints[item.id];
            constraints.preferred_type = Some(lit_ty.preferred_type().into());
            constraints.literal = Some(lit_ty);
        }
    }
    lit_pass_1(&mut tc.constraints, &tc.prog.int_lits, LiteralType::Int);
    lit_pass_1(&mut tc.constraints, &tc.prog.dec_lits, LiteralType::Dec);
    lit_pass_1(&mut tc.constraints, &tc.prog.str_lits, LiteralType::Str);
    lit_pass_1(&mut tc.constraints, &tc.prog.char_lits, LiteralType::Char);
    for level in 0..levels {
        for item in tc.prog.assigned_decls.get_level(level) {
            let constraints = &tc.constraints[item.root_expr];
            let ty = if let Some(explicit_ty) = &item.explicit_ty {
                assert!(constraints.can_unify_to(&explicit_ty.into()).is_ok());
                explicit_ty.clone()
            } else if let Some(pref) = &constraints.preferred_type {
                // I don't actually know if it's possible for an expression to not be able to unify to its preferred type?
                //assert!(dbg!(constraints).can_unify_to(dbg!(pref)).is_ok());
                pref.ty.clone()
            } else {
                constraints.one_of[0].ty.clone()
            };
            match item.decl_id {
                DeclId::Global(id) => &mut tc.prog.global_decls[id],
                DeclId::Local(id) => &mut tc.prog.local_decls[id],
            }.ret_ty.ty = ty;
        }
        for item in tc.prog.assignments.get_level(level) {
            tc.constraints[item.id].set_to(Type::Void);
            tc.types[item.id] = Type::Void;
        }
        for item in tc.prog.decl_refs.get_level(level) {
            // Filter overloads that don't match the constraints of the parameters.
            // P.S. These borrows are only here because the borrow checker is dumb
            let local_decls = &tc.prog.local_decls;
            let global_decls = &tc.prog.global_decls;
            let get_decl = |id: DeclId| -> &Decl {
                match id {
                    DeclId::Global(id) => &global_decls[id],
                    DeclId::Local(id) => &local_decls[id]
                }
            };
            let constraints = &tc.constraints;
            // Rule out overloads that don't match the arguments
            tc.prog.overloads[item.decl_ref_id].retain(|&overload| {
                assert_eq!(get_decl(overload).param_tys.len(), item.args.len());
                for (constraints, ty) in item.args.iter().map(|&arg| &constraints[arg]).zip(&get_decl(overload).param_tys) {
                    if constraints.can_unify_to(&ty.into()).is_err() { return false; }
                }
                true
            });

            tc.constraints[item.id].one_of = tc.prog.overloads[item.decl_ref_id].iter()
                .map(|&overload| get_decl(overload).ret_ty.clone())
                .collect();

            'find_preference: for (i, &arg) in item.args.iter().enumerate() {
                if let Some(ty) = &tc.constraints[arg].preferred_type {
                    for &overload in &tc.prog.overloads[item.decl_ref_id] {
                        let decl = get_decl(overload);
                        if ty.ty.trivially_convertible_to(&decl.param_tys[i]) {
                            tc.constraints[item.id].preferred_type = Some(decl.ret_ty.clone());
                            tc.preferred_overloads[item.decl_ref_id] = Some(overload);
                            break 'find_preference;
                        }
                    }
                }
            }
        }
        for item in tc.prog.addr_ofs.get_level(level) {
            let (addr, expr) = tc.constraints.index_mut(item.id, item.expr);
            // no mutability needed
            let expr = &*expr;

            let type_map = |ty: &QualType| {
                if item.is_mut && !ty.is_mut { return None; }
                Some(
                    QualType::from(
                        ty.ty.clone().ptr_with_mut(item.is_mut)
                    )
                )
            };
            addr.one_of = expr.one_of.iter().filter_map(type_map).collect();
            if let Some(pref) = &expr.preferred_type {
                addr.preferred_type = type_map(pref);
            }
        }
        for item in tc.prog.derefs.get_level(level) {
            let (expr, addr) = tc.constraints.index_mut(item.id, item.expr);
            // no mutability needed
            let addr = &*addr;

            expr.one_of = addr.one_of.iter().filter_map(|ty| {
                if let Type::Pointer(pointee) = &ty.ty {
                    Some(pointee.as_ref().clone())
                } else {
                    None
                }
            }).collect();
            if let Some(QualType { ty: Type::Pointer(pointee), .. }) = &addr.preferred_type {
                expr.preferred_type = Some(pointee.as_ref().clone());
            }
        }
        for item in tc.prog.ifs.get_level(level) {
            if tc.constraints[item.condition].can_unify_to(&Type::Bool.into()).is_err() {
                panic!("Expected boolean condition in if expression");
            }
            let constraints = tc.constraints[item.then_expr].intersect_with(&tc.constraints[item.else_expr]);
            if constraints.one_of.is_empty() && constraints.literal.is_none() {
                panic!("Failed to unify branches of if expression");
            }
            tc.constraints[item.id] = constraints;
        }
        for item in tc.prog.dos.get_level(level) {
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
    // Chain explicit and implicit rets together.
    let rets = tc.prog.rets.iter().map(|item| &item.data).chain(&tc.prog.implicit_rets);
    for item in rets {
        use UnificationError::*;
        use LiteralType::*;

        match tc.constraints[item.expr].can_unify_to(&QualType::from(&item.ty)) {
            Ok(()) => {}
            Err(Literal(Dec)) => panic!("expected return value of {:?}, found decimal literal", item.ty),
            Err(Literal(Int)) => panic!("expected return value of {:?}, found integer literal", item.ty),
            Err(Literal(Str)) => panic!("expected return value of {:?}, found string literal", item.ty),
            Err(Literal(Char)) => panic!("expected return value of {:?}, found character literal", item.ty),
            Err(InvalidChoice(choices)) => panic!("expected return value of {:?}, found {:?}", item.ty, choices),
            Err(Immutable) => panic!("COMPILER BUG: unexpected mutable return type"),
        }

        // Assume we panic above unless the returned expr can unify to the return type
        tc.constraints[item.expr].set_to(item.ty.clone());
    }
    for item in &tc.prog.whiles {
        if tc.constraints[item.condition].can_unify_to(&Type::Bool.into()).is_ok() {
            tc.constraints[item.condition].set_to(Type::Bool);
        } else {
            panic!("Expected boolean condition in while expression");
        }
    }
    for level in (0..levels).rev() {
        for item in tc.prog.assigned_decls.get_level(level) {
            tc.constraints[item.root_expr].set_to(tc.prog.decl(item.decl_id).ret_ty.ty.clone());
        }
        for item in tc.prog.assignments.get_level(level) {
            let (lhs, rhs) = tc.constraints.index_mut(item.lhs, item.rhs);
            lhs.lopsided_intersect_with(rhs);
            assert!(lhs.one_of.is_empty() || lhs.one_of_exists(|ty| ty.is_mut), "can't assign to immutable expression");
        }
        for item in tc.prog.decl_refs.get_level(level) {
            let constraints = &tc.constraints[item.id];
            let ty = if constraints.one_of.len() != 1 {
                errs.push(
                    Error::new("ambiguous type for expression")
                        .adding_primary_range(tc.prog.source_ranges[item.id].clone(), "expression here")
                );
                QualType::from(Type::Error)
            } else {
                constraints.one_of[0].clone()
            };
            tc.types[item.id] = ty.ty.clone();

            // P.S. These borrows are only here because the borrow checker is dumb
            let local_decls = &tc.prog.local_decls;
            let global_decls = &tc.prog.global_decls;
            let get_decl = |id: DeclId| -> &Decl {
                match id {
                    DeclId::Global(id) => &global_decls[id],
                    DeclId::Local(id) => &local_decls[id]
                }
            };
            let overloads = &mut tc.prog.overloads[item.decl_ref_id];
            overloads.retain(|&overload| {
                get_decl(overload).ret_ty
                    .trivially_convertible_to(&ty)
            });
            let pref = tc.preferred_overloads[item.decl_ref_id];

            let overload = if !overloads.is_empty() {
                let overload = pref
                    .filter(|overload| overloads.contains(overload))
                    .unwrap_or_else(|| overloads[0]);
                let decl = get_decl(overload);
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
            let (addr, expr) = tc.constraints.index_mut(item.id, item.expr);
            assert_eq!(addr.one_of.len(), 1);
            let ty = &addr.one_of[0].ty;
            tc.types[item.id] = ty.clone();
            if let Type::Pointer(pointee) = ty {
                expr.one_of = smallvec![pointee.as_ref().clone()];
            } else {
                panic!("unexpected non-pointer for addr of expression");
            }
        }
        for item in tc.prog.derefs.get_level(level) {
            let (expr, addr) = tc.constraints.index_mut(item.id, item.expr);
            assert_eq!(expr.one_of.len(), 1);
            let ty = &expr.one_of[0];
            tc.types[item.id] = ty.ty.clone();
            addr.one_of = smallvec![
                QualType::from(ty.clone().ptr())
            ];
        }
        for item in tc.prog.ifs.get_level(level) {
            // We already verified that the condition unifies to bool in pass 1
            tc.constraints[item.condition].set_to(Type::Bool);
            let ty = if tc.constraints[item.id].one_of.len() != 1 {
                errs.push(
                    Error::new("ambiguous type for if expression")
                        .adding_primary_range(tc.prog.source_ranges[item.id].clone(), "expression here")
                );
                QualType::from(Type::Error)
            } else {
                tc.constraints[item.id].one_of[0].clone()
            };
            tc.types[item.id] = ty.ty.clone();
            tc.constraints[item.then_expr].set_to(ty.clone());
            tc.constraints[item.else_expr].set_to(ty);
        }
        for item in tc.prog.dos.get_level(level) {
            let ty = if tc.constraints[item.id].one_of.len() != 1 {
                errs.push(
                    Error::new("ambiguous type for do expression")
                        .adding_primary_range(tc.prog.source_ranges[item.id].clone(), "expression here")
                );
                QualType::from(Type::Error)
            } else {
                tc.constraints[item.id].one_of[0].clone()
            };
            tc.types[item.id] = ty.ty.clone();
            tc.constraints[item.terminal_expr].set_to(ty);
        }
    }
    fn lit_pass_2<T>(
        constraints: &IdxVec<ConstraintList, ExprId>,
        types: &mut IdxVec<Type, ExprId>,
        errs: &mut Vec<Error>,
        source_ranges: &IdxVec<SourceRange, ExprId>,
        lits: &Vec<Expr<T>>,
        lit_ty: &str
    ) {
        for item in lits {
            let constraints = &constraints[item.id];
            types[item.id] = if constraints.one_of.len() != 1 {
                errs.push(
                    Error::new("ambiguous type for expression")
                        .adding_primary_range(source_ranges[item.id].clone(), format!("{} literal here", lit_ty))
                );
                Type::Error
            } else {
                constraints.one_of[0].ty.clone()
            };
        }
    }
    lit_pass_2(&tc.constraints, &mut tc.types, &mut errs, &tc.prog.source_ranges, &tc.prog.int_lits, "integer");
    lit_pass_2(&tc.constraints, &mut tc.types, &mut errs, &tc.prog.source_ranges, &tc.prog.dec_lits, "decimal");
    lit_pass_2(&tc.constraints, &mut tc.types, &mut errs, &tc.prog.source_ranges, &tc.prog.str_lits, "string");
    lit_pass_2(&tc.constraints, &mut tc.types, &mut errs, &tc.prog.source_ranges, &tc.prog.char_lits, "character");

    //println!("Types: {:#?}", tc.types);
    //println!("Program: {:#?}", tc.prog);
    //println!("Decl types: {:#?}", tc.prog.local_decls);
    //println!("Constraints: {:#?}", tc.constraints);

    let prog = Program {
        types: tc.types,
        overloads: tc.selected_overloads,
    };
    (prog, errs)
}