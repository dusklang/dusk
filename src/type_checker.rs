use smallvec::{SmallVec, smallvec};

use crate::error::Error;
use crate::tir::{Program, Decl};
use crate::builder::{ExprId, DeclId, DeclRefId};
use crate::ty::{Type, QualType};
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
    one_of: SmallVec<[QualType; 1]>,
    preferred_type: Option<QualType>,
}

enum UnificationError<'a> {
    /// The expression was constrained to be a literal that can't be unified to the requested type
    Literal(LiteralType),
    /// The expression didn't have the requested type in its list of type choices
    InvalidChoice(&'a [QualType]),
    /// The requested type was mutable but the expression was immutable
    Immutable,
}

impl ConstraintList {
    fn one_of_exists(&self, mut condition: impl FnMut(&QualType) -> bool) -> bool {
        for ty in &self.one_of {
            if condition(ty) { return true; }
        }
        false
    }

    fn is_never(&self) -> bool {
        self.one_of.len() == 1 
            && &self.one_of.first().unwrap().ty == &Type::Never
    }

    fn can_unify_to(&self, ty: &QualType) -> Result<(), UnificationError> {
        // Never is the "bottom type", so it unifies to anything.
        if self.one_of_exists(|ty| &ty.ty == &Type::Never) { return Ok(()); }

        use UnificationError::*;
        match self.literal {
            Some(LiteralType::Dec) if ty.ty.expressible_by_dec_lit() => if ty.is_mut {
                Err(UnificationError::Immutable)
            } else {
                Ok(())
            },
            Some(LiteralType::Int) if ty.ty.expressible_by_int_lit() => if ty.is_mut {
                Err(UnificationError::Immutable)
            } else {
                Ok(())
            },
            Some(lit) => Err(Literal(lit)),
            None => if self.one_of_exists(|oty| oty.trivially_convertible_to(ty)) {
                Ok(())
            } else {
                Err(InvalidChoice(&self.one_of))
            },
        }
    }

    fn set_to(&mut self, ty: impl Into<QualType>) {
        // If we're never, we should stay never.
        if !self.is_never() {
            self.one_of = smallvec![ty.into()];
        }
    }

    fn intersect_with(&self, other: &ConstraintList) -> ConstraintList {
        let mut constraints = ConstraintList::default();

        fn filtered_tys(tys: &[QualType], mut f: impl FnMut(&QualType) -> bool) -> SmallVec<[QualType; 1]> {
            tys.iter().filter_map(|ty|
                    if f(ty) {
                        Some(ty.clone())
                    } else {
                        None
                    }
                )
                .collect()
        }
        match (self.literal, self, other.literal, other) {
            (None, lhs, None, rhs) => {
                if lhs.is_never() {
                    constraints.one_of = rhs.one_of.clone();
                } else if rhs.is_never() {
                    constraints.one_of = lhs.one_of.clone();
                } else {
                    for lty in &lhs.one_of {
                        for rty in &rhs.one_of {
                            // TODO: would it be ok to break from the inner loop after finding a match here?
                            if lty.trivially_convertible_to(rty) {
                                constraints.one_of.push(rty.clone());
                            } else if rty.trivially_convertible_to(lty) {
                                constraints.one_of.push(lty.clone());
                            }
                        }
                    }
                }
            },
            (Some(lit_lit), lit, None, non_lit) | (None, non_lit, Some(lit_lit), lit) => {
                if non_lit.is_never() {
                    constraints.literal = Some(lit_lit);
                    constraints.one_of = lit.one_of.clone();
                } else {
                    constraints.literal = None;
                    constraints.one_of = filtered_tys(
                        &non_lit.one_of,
                        {
                            fn dec_lit(ty: &QualType) -> bool { ty.ty.expressible_by_dec_lit() }
                            fn int_lit(ty: &QualType) -> bool { ty.ty.expressible_by_int_lit() }
                            match lit_lit {
                                LiteralType::Dec => dec_lit,
                                LiteralType::Int => int_lit,
                            }
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
            for (a, b) in &[(self, other), (other, self)] {
                if let Some(preferred_type) = &a.preferred_type {
                    // I don't actually know if it's possible for an expression to not be able to unify to its preferred type?
                    assert!(a.can_unify_to(preferred_type).is_ok());
                    if b.can_unify_to(preferred_type).is_ok() {
                        constraints.preferred_type = Some(preferred_type.clone());
                    }
                }
            }
        }

        constraints
    }

    // Same as intersect_with, but: 
    //  - mutates `self` and `other` in-place instead of creating a new `ConstraintList`
    //  - evaluates mutability independently between the arguments, with precedence given to self
    //  - is literally just used for assignment expressions
    //  - terrible abstraction :(
    fn lopsided_intersect_with(&mut self, other: &mut ConstraintList) {
        match (self.literal, other.literal) {
            (None, None) => {
                if !self.is_never() && !other.is_never() {
                    self.one_of.retain(|lty|
                        other.one_of_exists(|rty| rty.ty.trivially_convertible_to(&lty.ty))
                    );
                    other.one_of.retain(|rty|
                        self.one_of_exists(|lty| rty.ty.trivially_convertible_to(&lty.ty))
                    );
                }
            },
            (Some(lhs_lit), None) => {
                // Can't assign to a literal
                self.one_of = SmallVec::new();

                fn dec_lit(ty: &mut QualType) -> bool { ty.ty.expressible_by_dec_lit() }
                fn int_lit(ty: &mut QualType) -> bool { ty.ty.expressible_by_int_lit() }
                other.one_of.retain(
                    match lhs_lit {
                        LiteralType::Dec => dec_lit,
                        LiteralType::Int => int_lit,
                    }
                );
            },
            (None, Some(rhs_lit)) => {
                let lit_test = match rhs_lit {
                    LiteralType::Dec => Type::expressible_by_dec_lit,
                    LiteralType::Int => Type::expressible_by_int_lit,
                };
                if !self.is_never() {
                    let (mut lhs_one_of, mut rhs_one_of) = (SmallVec::new(), SmallVec::new());
                    for lty in &self.one_of {
                        for rty in &other.one_of {
                            if lit_test(&lty.ty) || rty.ty.trivially_convertible_to(&lty.ty) {
                                lhs_one_of.push(lty.clone());
                                rhs_one_of.push(rty.clone());
                            }
                        }
                    }
                    self.one_of = dbg!(lhs_one_of);
                    other.one_of = dbg!(rhs_one_of);
                }
            }
            (Some(LiteralType::Dec), Some(_)) | (Some(LiteralType::Int), Some(_)) => self.one_of = SmallVec::new(),
        }

        if self.preferred_type.as_ref().map(|ty| &ty.ty) != other.preferred_type.as_ref().map(|ty| &ty.ty) {
            if let Some(preferred_type) = &other.preferred_type {
                // I don't actually know if it's possible for an expression to not be able to unify to its preferred type?
                assert!(other.can_unify_to(preferred_type).is_ok());
                let preferred_type = QualType {
                    ty: preferred_type.ty.clone(),
                    is_mut: true,
                };
                if self.can_unify_to(&preferred_type).is_ok() {
                    self.preferred_type = Some(preferred_type);
                }
            }
            if let Some(preferred_type) = &self.preferred_type {
                // I don't actually know if it's possible for an expression to not be able to unify to its preferred type?
                assert!(self.can_unify_to(preferred_type).is_ok());
                let preferred_type = QualType::from(preferred_type.ty.clone());
                if other.can_unify_to(&preferred_type).is_ok() {
                    other.preferred_type = Some(preferred_type);
                }
            }
        }
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
}

#[inline(never)]
pub fn type_check(prog: Program) -> Vec<Error> {
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
        &mut tc.prog.assigned_decls, &mut tc.prog.assignments, &mut tc.prog.decl_refs, &mut tc.prog.rets, &mut tc.prog.ifs, &mut tc.prog.dos,
    ]);

    // Assign the type of the void expression to be void.
    tc.constraints[tc.prog.void_expr].one_of = smallvec![Type::Void.into()];
    tc.types[tc.prog.void_expr] = Type::Void;

    // Pass 1: propagate info down from leaves to roots
    for item in &tc.prog.int_lits { 
        let constraints = &mut tc.constraints[item.id];
        let lit = LiteralType::Int;
        constraints.preferred_type = Some(lit.preferred_type().into());
        constraints.literal = Some(lit);
    }
    for item in &tc.prog.dec_lits {
        let constraints = &mut tc.constraints[item.id];
        let lit = LiteralType::Dec;
        constraints.preferred_type = Some(lit.preferred_type().into());
        constraints.literal = Some(lit);
    }
    for level in 0..levels {
        for item in tc.prog.assigned_decls.get_level(level) {
            let constraints = &tc.constraints[item.root_expr];
            let guess = if let Some(pref) = &constraints.preferred_type {
                // I don't actually know if it's possible for an expression to not be able to unify to its preferred type?
                assert!(constraints.can_unify_to(pref).is_ok());
                pref.ty.clone()
            } else {
                constraints.one_of[0].ty.clone()
            };
            match item.decl_id {
                DeclId::Global(id) => &mut tc.prog.global_decls[id],
                DeclId::Local(id) => &mut tc.prog.local_decls[id],
            }.ret_ty.ty = guess;
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
        for item in tc.prog.rets.get_level(level) {
            use UnificationError::*;
            use LiteralType::*;

            let constraints = &mut tc.constraints[item.id];
            constraints.one_of = smallvec![Type::Never.into()];
            tc.types[item.id] = Type::Never;

            match tc.constraints[item.expr].can_unify_to(&QualType::from(&item.ty)) {
                Ok(()) => {}
                Err(Literal(Dec)) => panic!("expected return value of {:?}, found decimal literal", item.ty),
                Err(Literal(Int)) => panic!("expected return value of {:?}, found integer literal", item.ty),
                Err(InvalidChoice(choices)) => panic!("expected return value of {:?}, found {:?}", item.ty, choices),
                Err(Immutable) => panic!("COMPILER BUG: unexpected mutable return type"),
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
    for item in &tc.prog.stmts {
        let constraints = &mut tc.constraints[item.root_expr];
        if constraints.can_unify_to(&Type::Void.into()).is_err() {
            panic!("standalone expressions must return void");
        }
        constraints.set_to(Type::Void);
    }

    // Pass 2: propagate info up from roots to leaves
    for level in (0..levels).rev() {
        for item in tc.prog.assigned_decls.get_level(level) {
            tc.constraints[item.root_expr].set_to(tc.prog.decl(item.decl_id).ret_ty.ty.clone());
        }
        for item in tc.prog.assignments.get_level(level) {
            let (lhs, rhs) = tc.constraints.index_mut(item.lhs, item.rhs);
            lhs.lopsided_intersect_with(rhs);
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
        for item in tc.prog.rets.get_level(level) {
            let constraints = &mut tc.constraints[item.expr];
            if constraints.can_unify_to(&QualType::from(&item.ty)).is_ok() {
                constraints.set_to(item.ty.clone())
            } else {
                constraints.one_of = SmallVec::new();
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
    for item in &tc.prog.int_lits {
        let constraints = &tc.constraints[item.id];
        tc.types[item.id] = if constraints.one_of.len() != 1 {
            errs.push(
                Error::new("ambiguous type for expression")
                    .adding_primary_range(tc.prog.source_ranges[item.id].clone(), "int literal here")
            );
            Type::Error
        } else {
            constraints.one_of[0].ty.clone()
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
            constraints.one_of[0].ty.clone()
        };
    }

    println!("Types: {:#?}", tc.types);
    //println!("Program: {:#?}", tc.prog);
    //println!("Decl types: {:#?}", tc.prog.local_decls);
    //println!("Constraints: {:#?}", tc.constraints);

    std::mem::forget(tc);

    errs
}