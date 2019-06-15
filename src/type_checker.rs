use crate::error::Error;
use crate::tir::{Program, Decl};
use crate::builder::{ExprId, DeclId, DeclRefId, RetId};
use crate::ty::Type;
use crate::index_vec::IdxVec;
use crate::dep_vec;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
enum LiteralType { Int, Dec }

impl LiteralType {
    fn preferred_type(&self) -> Type {
        match self {
            LiteralType::Int => Type::i32(),
            LiteralType::Dec => Type::f64(),
        }
    }
}

#[derive(Debug, Default, Clone)]
struct ConstraintList {
    literal: Option<LiteralType>,
    one_of: Vec<Type>,
    preferred_type: Option<Type>,
}

enum UnificationError<'a> {
    /// The expression was constrained to be a literal that can't be unified to the requested type
    Literal(LiteralType),
    /// The expression didn't have the requested type in its list of type choices
    InvalidChoice(&'a [Type]),
}

impl ConstraintList {
    fn can_unify_to(&self, ty: &Type) -> Result<(), UnificationError> {
        // Never is the "bottom type", so it unifies to anything.
        if self.one_of.contains(&Type::Never) { return Ok(()); }

        use UnificationError::*;
        match self.literal {
            Some(LiteralType::Dec) if ty.expressible_by_dec_lit() => Ok(()),
            Some(LiteralType::Int) if ty.expressible_by_int_lit() => Ok(()),
            Some(lit) => Err(Literal(lit)),
            None => if self.one_of.contains(ty) {
                Ok(())
            } else {
                Err(InvalidChoice(&self.one_of))
            },
        }
    }

    fn set_to(&mut self, ty: Type) {
        // If we're never, we should stay never.
        if self.one_of != &[Type::Never] {
            self.one_of = vec![ty];
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
        match (self.literal, &self.one_of, other.literal, &other.one_of) {
            (None, lhs, None, rhs) => {
                if lhs == &[Type::Never] {
                    constraints.one_of = rhs.clone();
                } else if rhs == &[Type::Never] {
                    constraints.one_of = lhs.clone();
                } else {
                    for ty in lhs {
                        if rhs.contains(ty) {
                            constraints.one_of.push(ty.clone());
                        }
                    }
                }
            },
            (Some(lhs_lit), lhs, None, rhs) | (None, rhs, Some(lhs_lit), lhs) => {
                if rhs == &[Type::Never] {
                    constraints.literal = Some(lhs_lit);
                    constraints.one_of = lhs.clone();
                } else {
                    constraints.literal = None;
                    constraints.one_of = filtered_tys(
                        rhs,
                        match lhs_lit {
                            LiteralType::Dec => Type::expressible_by_dec_lit,
                            LiteralType::Int => Type::expressible_by_int_lit,
                        },
                    );
                }
            }
            (Some(LiteralType::Dec), _, Some(rhs), _) => match rhs {
                LiteralType::Int | LiteralType::Dec => constraints.literal = Some(LiteralType::Dec),
            },
            (Some(LiteralType::Int), _, Some(rhs), _) => match rhs {
                LiteralType::Int => constraints.literal = Some(LiteralType::Int),
                LiteralType::Dec => constraints.literal = Some(LiteralType::Dec),
            }
        }

        if self.preferred_type == other.preferred_type {
            constraints.preferred_type = self.preferred_type.clone();
        } else {
            if let Some(preferred_type) = &self.preferred_type {
                // I don't actually know if it's possible for an expression to not be able to unify to its preferred type?
                assert!(self.can_unify_to(preferred_type).is_ok());
                if other.can_unify_to(preferred_type).is_ok() {
                    constraints.preferred_type = Some(preferred_type.clone());
                }
            } 
            if let Some(preferred_type) = &other.preferred_type {
                // I don't actually know if it's possible for an expression to not be able to unify to its preferred type?
                assert!(other.can_unify_to(preferred_type).is_ok());
                if self.can_unify_to(preferred_type).is_ok() {
                    constraints.preferred_type = Some(preferred_type.clone());
                }
            }
        }

        constraints
    }
}

struct TypeChecker {
    /// The input TIR program
    prog: Program,
    /// The type of each expression
    types: IdxVec<Type, ExprId>,
    /// The constraints on each expression's type
    constraints: IdxVec<ConstraintList, ExprId>,
    /// The preferred overload for each decl ref (currently only ever originates from literals)
    preferred_overloads: IdxVec<Option<DeclId>, DeclRefId>,
    /// The selected overload for each decl ref
    selected_overloads: IdxVec<Option<DeclId>, DeclRefId>,
    /// Whether or not each return expression is disabled
    // TODO: More efficient layout!
    disabled: IdxVec<bool, RetId>,
}

pub fn type_check(prog: Program) -> Vec<Error> {
    let mut tc = TypeChecker {
        prog,
        types: IdxVec::new(),
        constraints: IdxVec::new(),
        preferred_overloads: IdxVec::new(),
        selected_overloads: IdxVec::new(),
        disabled: IdxVec::new(),
    };
    let mut errs = Vec::new();
    tc.types.resize_with(tc.prog.num_exprs, Default::default);
    tc.constraints.resize_with(tc.prog.num_exprs, Default::default);
    tc.selected_overloads.resize_with(tc.prog.overloads.len(), || None);
    tc.preferred_overloads.resize_with(tc.prog.overloads.len(), || None);
    tc.disabled.resize_with(tc.prog.num_rets, || false);

    // Extend arrays as needed so they all have the same number of levels.
    let levels = dep_vec::unify_sizes(&mut [
        &mut tc.prog.assigned_decls, &mut tc.prog.decl_refs, &mut tc.prog.rets, &mut tc.prog.ifs,
    ]);

    // Assign the type of the void expression to be void.
    tc.constraints[tc.prog.void_expr].one_of = vec![Type::Void];
    tc.types[tc.prog.void_expr] = Type::Void;

    // Pass 1: propagate info down from leaves to roots
    for item in &tc.prog.int_lits { 
        let constraints = &mut tc.constraints[item.id];
        let lit = LiteralType::Int;
        constraints.preferred_type = Some(lit.preferred_type());
        constraints.literal = Some(lit);
    }
    for item in &tc.prog.dec_lits {
        let constraints = &mut tc.constraints[item.id];
        let lit = LiteralType::Dec;
        constraints.preferred_type = Some(lit.preferred_type());
        constraints.literal = Some(lit);
    }
    for level in 0..levels {
        for item in tc.prog.assigned_decls.get_level(level) {
            let constraints = &tc.constraints[item.root_expr];
            let guess = if let Some(pref) = &constraints.preferred_type {
                // I don't actually know if it's possible for an expression to not be able to unify to its preferred type?
                assert!(constraints.can_unify_to(pref).is_ok());
                pref.clone()
            } else {
                constraints.one_of[0].clone()
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

            'find_preference: for (i, &arg) in item.args.iter().enumerate() {
                if let Some(ty) = &tc.constraints[arg].preferred_type {
                    for &overload in &tc.prog.overloads[item.decl_ref_id] {
                        let decl = get_decl(overload);
                        if &decl.param_tys[i] == ty {
                            tc.constraints[item.id].preferred_type = Some(decl.ret_ty.clone());
                            tc.preferred_overloads[item.decl_ref_id] = Some(overload);
                            break 'find_preference;
                        }
                    }
                }
            }
        }
        for item in tc.prog.rets.get_level(level) {
            use UnificationError::*;
            use LiteralType::*;

            let constraints = &mut tc.constraints[item.id];
            constraints.one_of = vec![Type::Never];
            tc.types[item.id] = Type::Never;

            match tc.constraints[item.expr].can_unify_to(&item.ty) {
                Ok(()) => {}
                Err(Literal(Dec)) => panic!("expected return value of {:?}, found decimal literal", item.ty),
                Err(Literal(Int)) => panic!("expected return value of {:?}, found integer literal", item.ty),
                Err(InvalidChoice(choices)) => panic!("expected return value of {:?}, found {:?}", item.ty, choices),
            }
            if tc.constraints[item.expr].can_unify_to(&Type::Never).is_ok() {
                tc.disabled[item.ret_id] = true;
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
    for item in &tc.prog.stmts {
        let constraints = &mut tc.constraints[item.root_expr];
        if let Err(_) = constraints.can_unify_to(&Type::Void) {
            panic!("standalone expressions must return void");
        }
        constraints.set_to(Type::Void);
    }

    // Pass 2: propagate info up from roots to leaves
    for level in (0..levels).rev() {
        for item in tc.prog.assigned_decls.get_level(level) {
            tc.constraints[item.root_expr].set_to(tc.prog.decl(item.decl_id).ret_ty.clone());
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
            let overloads = &mut tc.prog.overloads[item.decl_ref_id];
            overloads.retain(|&overload| {
                let ret_ty = &get_decl(overload).ret_ty;
                ret_ty == &ty || ret_ty == &Type::Never
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
                    tc.constraints[arg].one_of = vec![Type::Error];
                }
                None
            };
            tc.selected_overloads[item.decl_ref_id] = overload;
        }
        for item in tc.prog.rets.get_level(level) {
            let constraints = &mut tc.constraints[item.expr];
            if constraints.can_unify_to(&item.ty).is_ok() {
                constraints.set_to(item.ty.clone())
            } else {
                constraints.one_of = Vec::new();
            }
        }
        for item in tc.prog.ifs.get_level(level) {
            // We already verified that the condition unifies to bool in pass 1
            tc.constraints[item.condition].set_to(Type::Bool);
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
            tc.constraints[item.then_expr].set_to(ty.clone());
            tc.constraints[item.else_expr].set_to(ty);
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

    std::mem::forget(tc);

    // println!("Types: {:#?}", tc.types);
    // println!("Returns disabled: {:#?}", tc.disabled);
    // println!("Decl types: {:#?}", tc.prog.local_decls);
    //println!("Constraints: {:#?}", tc.constraints);
    errs
}