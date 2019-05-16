use crate::error::Error;
use crate::hir::{Program, ItemKind, ItemId, Decl, DeclId, DeclRefId};
use crate::ty::{Type, IntWidth, FloatWidth};
use crate::index_vec::IdxVec;

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

    // Pass 1: propagate info down from leaves to roots
    for level in tc.prog.items.levels() {
        for item in tc.prog.items.get_level(level) {
            use ItemKind::*;
            match &item.kind {
                IntLit => tc.constraints[item.id].literal = Some(LiteralType::Int),
                DecLit => tc.constraints[item.id].literal = Some(LiteralType::Dec),
                &StoredDecl { root_expr, id } => {
                    let constraints = &tc.constraints[root_expr];
                    let guess = match constraints.literal {
                        Some(LiteralType::Int) => Type::i32(),
                        Some(LiteralType::Dec) => Type::f64(),
                        None => {
                            assert!(!constraints.one_of.is_empty());
                            constraints.one_of[0].clone()
                        }
                    };
                    match id {
                        DeclId::Global(id) => &mut tc.prog.global_decls[id],
                        DeclId::Local(id) => &mut tc.prog.local_decls[id],
                    }.ret_ty = guess;
                }
                &DeclRef { ref args, id } => {
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
                    tc.prog.overloads[id].retain(|&overload| {
                        assert_eq!(get_decl(overload).param_tys.len(), args.len());
                        // For each parameter:
                        for (constraints, ty) in args.iter().map(|&arg| &constraints[arg]).zip(&get_decl(overload).param_tys) {
                            // Verify all the constraints match the parameter type.
                            match constraints.literal {
                                Some(LiteralType::Int) => if !ty.expressible_by_int_lit() { return false },
                                Some(LiteralType::Dec) => if !ty.expressible_by_dec_lit() { return false },
                                None => if !constraints.one_of.contains(ty) { return false },
                            }
                        }
                        true
                    });

                    tc.constraints[item.id].one_of = tc.prog.overloads[id].iter()
                        .map(|&overload| get_decl(overload).ret_ty.clone())
                        .collect();
                }
            }
        }
    }

    // Pass 2: propagate info up from roots to leaves
    for level in tc.prog.items.levels().rev() {
        for item in tc.prog.items.get_level(level) {
            use ItemKind::*;
            match &item.kind {
                IntLit => {
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
                },
                DecLit => {
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
                },
                &StoredDecl { root_expr, id } => {
                    tc.constraints[root_expr].one_of = vec![tc.prog.decl(id).ret_ty.clone()];
                }
                &DeclRef { ref args, id } => {
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
                    tc.prog.overloads[id].retain(|&overload| get_decl(overload).ret_ty == ty);
                    
                    let overload = if tc.prog.overloads[id].len() != 1 {
                        errs.push(
                            Error::new("ambiguous overload for binary operator")
                                .adding_primary_range(tc.prog.source_ranges[item.id].clone(), "expression here")
                        );
                        for &arg in args {
                            tc.constraints[arg].one_of = vec![Type::Error];
                        }
                        None
                    } else {
                        let overload = tc.prog.overloads[id][0];
                        for (i, &arg) in args.iter().enumerate() {
                            tc.constraints[arg].one_of = vec![get_decl(overload).param_tys[i].clone()];
                        }
                        Some(overload)
                    };
                    tc.selected_overloads[id] = overload;
                }
            }
        }
    }
    println!("Types: {:#?}", tc.types);
    //println!("Constraints: {:#?}", tc.constraints);
    errs
}