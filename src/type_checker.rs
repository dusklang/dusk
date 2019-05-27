use crate::error::Error;
use crate::tir::{Program, ExprId, Decl, DeclId, DeclRefId};
use crate::ty::{Type, IntWidth, FloatWidth};
use crate::index_vec::IdxVec;
use crate::dep_vec::{self, AnyDepVec};

#[derive(Debug, Clone)]
enum LiteralType { Int, Dec }

#[derive(Debug, Default, Clone)]
struct ConstraintList {
    literal: Option<LiteralType>,
    one_of: Vec<Type>,
}

enum UnificationError<'a> {
    /// The expression was constrained to be a literal that can't be unified to the requested type
    Literal(LiteralType),
    /// The expression didn't have the requested type in its list of type choices
    InvalidChoice(&'a [Type]),
}

impl ConstraintList {
    fn can_unify_to(&self, ty: &Type) -> Result<(), UnificationError> {
        use UnificationError::*;
        match self.literal {
            Some(ref lit) => if ty.expressible_by_int_lit() {
                Ok(())
            } else {
                Err(Literal(lit.clone()))
            },
            None => if self.one_of.contains(ty) {
                Ok(())
            } else {
                Err(InvalidChoice(&self.one_of))
            },
        }
    }

    fn intersect_with(&self, other: &ConstraintList) -> ConstraintList {
        let mut constraints = ConstraintList::default();
        fn filtered_tys(tys: &[Type], mut f: impl FnMut(&Type) -> bool) -> Vec<Type> {
            tys.iter().filter_map(|ty| 
                    if f(ty) { 
                        Some(ty.clone())
                    } else { 
                        None 
                    }
                )
                .collect()
        }
        match self.literal {
            Some(LiteralType::Dec) => match other.literal {
                Some(LiteralType::Dec) | Some(LiteralType::Int) => constraints.literal = Some(LiteralType::Dec),
                None => {
                    constraints.literal = None;
                    constraints.one_of = filtered_tys(&other.one_of, Type::expressible_by_dec_lit);
                }
            },
            Some(LiteralType::Int) => match other.literal {
                Some(LiteralType::Dec) | Some(LiteralType::Int) => constraints.literal = other.literal.clone(),
                None => {
                    constraints.literal = None;
                    constraints.one_of = filtered_tys(&other.one_of, Type::expressible_by_int_lit);
                }
            },
            None => {
                constraints.literal = None;
                match other.literal {
                    Some(LiteralType::Dec) => constraints.one_of = filtered_tys(&self.one_of, Type::expressible_by_dec_lit),
                    Some(LiteralType::Int) => constraints.one_of = filtered_tys(&self.one_of, Type::expressible_by_int_lit),
                    None => for ty in &self.one_of {
                        if other.one_of.contains(ty) {
                            constraints.one_of.push(ty.clone());
                        }
                    },
                }
            }
        }

        constraints
    }
}

const DEFAULT_INT_TY: Type = Type::Int {
    width: IntWidth::W64,
    is_signed: true,
};

const DEFAULT_FLOAT_TY: Type = Type::Float(FloatWidth::W64);

struct TypeChecker {
    /// The input TIR program
    prog: Program,
    /// The type of each expression
    types: IdxVec<Type, ExprId>,
    /// The constraints on each expression's type
    constraints: IdxVec<ConstraintList, ExprId>,
    /// The selected overload for each decl ref
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
    tc.types.resize_with(tc.prog.num_exprs, Default::default);
    tc.constraints.resize_with(tc.prog.num_exprs, Default::default);
    tc.selected_overloads.resize_with(tc.prog.overloads.len(), || None);

    let levels = dep_vec::unify_sizes(&mut [
        &mut tc.prog.assigned_decls, &mut tc.prog.decl_refs, &mut tc.prog.stmts, &mut tc.prog.rets, &mut tc.prog.ifs,
    ]);

    // Assign the type of the void expression to be void.
    tc.constraints[tc.prog.void_expr].one_of = vec![Type::Void];
    tc.types[tc.prog.void_expr] = Type::Void;

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
                for (constraints, ty) in item.args.iter().map(|&arg| &constraints[arg]).zip(&get_decl(overload).param_tys) {
                    if let Err(_) = constraints.can_unify_to(ty) { return false; }
                }
                true
            });

            tc.constraints[item.id].one_of = tc.prog.overloads[item.decl_ref_id].iter()
                .map(|&overload| get_decl(overload).ret_ty.clone())
                .collect();
        }
        for item in tc.prog.stmts.get_level(level) {
            if let Err(_) = tc.constraints[item.root_expr].can_unify_to(&Type::Void) {
                panic!("standalone expressions must return void");
            }
        }
        for item in tc.prog.rets.get_level(level) {
            use UnificationError::*;
            use LiteralType::*;
            match tc.constraints[item.expr].can_unify_to(&item.ty) {
                Ok(()) => {}
                Err(Literal(Dec)) => panic!("expected return value of {:?}, found decimal literal", item.ty),
                Err(Literal(Int)) => panic!("expected return value of {:?}, found integer literal", item.ty),
                Err(InvalidChoice(choices)) => panic!("expected return value of {:?}, found {:?}", item.ty, choices),
            }
        }
        for item in tc.prog.ifs.get_level(level) {
            if let Err(_) = tc.constraints[item.condition].can_unify_to(&Type::Bool) {
                panic!("Expected boolean condition in if expression");
            }
            let constraints = tc.constraints[item.then_expr].intersect_with(&tc.constraints[item.else_expr]);
            if constraints.one_of.is_empty() && constraints.literal.is_none() {
                panic!("Failed to unify branches of if expression");
            }
            tc.constraints[item.id] = constraints;

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
        for item in tc.prog.stmts.get_level(level) {
            tc.constraints[item.root_expr].one_of.retain(|ty| ty == &Type::Void);
        }
        for item in tc.prog.rets.get_level(level) {
            match tc.constraints[item.expr].literal {
                Some(LiteralType::Dec) => 
                    tc.constraints[item.expr].one_of = if item.ty.expressible_by_dec_lit() {
                        vec![item.ty.clone()]
                    } else {
                        Vec::new()
                    },
                Some(LiteralType::Int) => 
                    tc.constraints[item.expr].one_of = if item.ty.expressible_by_int_lit() {
                        vec![item.ty.clone()]
                    } else {
                        Vec::new()
                    },
                None => tc.constraints[item.expr].one_of.retain(|ty| ty == &item.ty),
            }
        }
        for item in tc.prog.ifs.get_level(level) {
            tc.constraints[item.condition].one_of.retain(|ty| ty == &Type::Bool);
            let ty = if tc.constraints[item.id].one_of.len() != 1 {
                errs.push(
                    Error::new("ambiguous type for if expression")
                        .adding_primary_range(tc.prog.source_ranges[item.id].clone(), "expression here")
                );
                Type::Error
            } else {
                tc.constraints[item.id].one_of[0].clone()
            };
            tc.types[item.id] = ty.clone();
            tc.constraints[item.then_expr].one_of = vec![ty.clone()];
            tc.constraints[item.else_expr].one_of = vec![ty];
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