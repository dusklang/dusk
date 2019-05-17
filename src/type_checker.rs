use crate::error::Error;
use crate::hir::{Program, ItemId, Decl, DeclId, DeclRefId};
use crate::ty::{Type, IntWidth, FloatWidth};
use crate::index_vec::IdxVec;
use crate::dep_vec::{self, AnyDepVec};

#[derive(Debug)]
enum LiteralType { Int, Dec }

#[derive(Debug, Default)]
struct ConstraintList {
    literal: Option<LiteralType>,
    one_of: Vec<Type>,
}

const DEFAULT_INT_TY: Type = Type::Int {
    width: IntWidth::W64,
    is_signed: true,
};

const DEFAULT_FLOAT_TY: Type = Type::Float(FloatWidth::W64);

struct TypeChecker {
    /// The input HIR program
    prog: Program,
    /// The type of each item
    types: IdxVec<Type, ItemId>,
    /// The constraints on each items's type
    constraints: IdxVec<ConstraintList, ItemId>,
    /// The selected overload for each function call or operator expression
    selected_overloads: IdxVec<Option<DeclId>, DeclRefId>,
}

pub fn type_check(prog: Program) -> Vec<Error> {
    let mut tc = TypeChecker {
        prog,
        types: IdxVec::new(),
        constraints: IdxVec::new(),
        selected_overloads: IdxVec::new(),
    };
    let mut errs = Vec::new();
    tc.types.resize_with(tc.prog.num_items, Default::default);
    tc.constraints.resize_with(tc.prog.num_items, Default::default);
    tc.selected_overloads.resize_with(tc.prog.overloads.len(), || None);

    let levels = dep_vec::unify_sizes(&mut [
        &mut tc.prog.assigned_decls, &mut tc.prog.decl_refs,
    ]);

    // Pass 1: propagate info down from leaves to roots
    for item in &tc.prog.int_lits { tc.constraints[item.id].literal = Some(LiteralType::Int); }
    for item in &tc.prog.dec_lits { tc.constraints[item.id].literal = Some(LiteralType::Dec); }
    for level in 0..levels {
        for item in tc.prog.assigned_decls.get_level(level) {
            let constraints = &tc.constraints[item.root_expr];
            let guess = match constraints.literal {
                Some(LiteralType::Int) => Type::i32(),
                Some(LiteralType::Dec) => Type::f64(),
                None => {
                    assert!(!constraints.one_of.is_empty());
                    constraints.one_of[0].clone()
                }
            };
            match item.decl_id {
                DeclId::Global(id) => &mut tc.prog.global_decls[id],
                DeclId::Local(id) => &mut tc.prog.local_decls[id],
            }.ret_ty = guess;
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
            tc.prog.overloads[item.decl_ref_id].retain(|&overload| {
                assert_eq!(get_decl(overload).param_tys.len(), item.args.len());
                // For each parameter:
                for (constraints, ty) in item.args.iter().map(|&arg| &constraints[arg]).zip(&get_decl(overload).param_tys) {
                    // Verify all the constraints match the parameter type.
                    match constraints.literal {
                        Some(LiteralType::Int) => if !ty.expressible_by_int_lit() { return false },
                        Some(LiteralType::Dec) => if !ty.expressible_by_dec_lit() { return false },
                        None => if !constraints.one_of.contains(ty) { return false },
                    }
                }
                true
            });

            tc.constraints[item.id].one_of = tc.prog.overloads[item.decl_ref_id].iter()
                .map(|&overload| get_decl(overload).ret_ty.clone())
                .collect();
        }
        
    }

    // Pass 2: propagate info up from roots to leaves
    for level in (0..levels).rev() {
        for item in tc.prog.assigned_decls.get_level(level) {
            tc.constraints[item.root_expr].one_of = vec![tc.prog.decl(item.decl_id).ret_ty.clone()];
        }
        for item in tc.prog.decl_refs.get_level(level) {
            let constraints = &tc.constraints[item.id];
            let ty = if constraints.one_of.len() != 1 {
                errs.push(
                    Error::new("ambiguous type for expression")
                        .adding_primary_range(tc.prog.source_ranges[item.id].clone(), "expression here")
                );
                Type::Error
            } else {
                constraints.one_of[0].clone()
            };
            tc.types[item.id] = ty.clone();

            // This line is necessary because the borrow checker is dumb
            // P.S. These borrows are only here because the borrow checker is dumb
            let local_decls = &tc.prog.local_decls;
            let global_decls = &tc.prog.global_decls;
            let get_decl = |id: DeclId| -> &Decl {
                match id {
                    DeclId::Global(id) => &global_decls[id],
                    DeclId::Local(id) => &local_decls[id]
                }
            };
            tc.prog.overloads[item.decl_ref_id].retain(|&overload| get_decl(overload).ret_ty == ty);
            
            let overload = if tc.prog.overloads[item.decl_ref_id].len() != 1 {
                errs.push(
                    Error::new("ambiguous overload for binary operator")
                        .adding_primary_range(tc.prog.source_ranges[item.id].clone(), "expression here")
                );
                for &arg in &item.args {
                    tc.constraints[arg].one_of = vec![Type::Error];
                }
                None
            } else {
                let overload = tc.prog.overloads[item.decl_ref_id][0];
                for (i, &arg) in item.args.iter().enumerate() {
                    tc.constraints[arg].one_of = vec![get_decl(overload).param_tys[i].clone()];
                }
                Some(overload)
            };
            tc.selected_overloads[item.decl_ref_id] = overload;
        }
    }
    for item in &tc.prog.int_lits {
        let constraints = &tc.constraints[item.id];
        tc.types[item.id] = if constraints.one_of.len() != 1 {
            errs.push(
                Error::new("ambiguous type for expression")
                    .adding_primary_range(tc.prog.source_ranges[item.id].clone(), "int literal here")
            );
            Type::Error
        } else {
            constraints.one_of[0].clone()
        };
    }
    for item in &tc.prog.dec_lits {
        let constraints = &tc.constraints[item.id];
        tc.types[item.id] = if constraints.one_of.len() != 1 {
            errs.push(
                Error::new("ambiguous type for expression")
                    .adding_primary_range(tc.prog.source_ranges[item.id].clone(), "dec literal here")
            );
            Type::Error
        } else {
            constraints.one_of[0].clone()
        };
    }

    println!("Types: {:#?}", tc.types);
    println!("Decl types: {:#?}", tc.prog.local_decls);
    //println!("Constraints: {:#?}", tc.constraints);
    errs
}