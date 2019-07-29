use smallvec::{SmallVec, smallvec};

use crate::ty::{Type, QualType};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LiteralType { Int, Dec, Str, Char }

impl LiteralType {
    pub fn preferred_type(&self) -> Type {
        match self {
            LiteralType::Int => Type::i32(),
            LiteralType::Dec => Type::f64(),
            LiteralType::Str => Type::i8().ptr(),
            // Because a char literal has the same syntax as a string literal, it would feel
            // inconsistent if one-byte string literals defaulted to `i8`
            LiteralType::Char => Type::i8().ptr(),
        }
    }
}

#[derive(Debug, Default, Clone)]
pub struct ConstraintList {
    pub literal: Option<LiteralType>,
    pub one_of: SmallVec<[QualType; 1]>,
    pub preferred_type: Option<QualType>,
}

pub enum UnificationError<'a> {
    /// The expression was constrained to be a literal that can't be unified to the requested type
    Literal(LiteralType),
    /// The expression didn't have the requested type in its list of type choices
    InvalidChoice(&'a [QualType]),
    /// The requested type was mutable but the expression was immutable
    Immutable,
}

impl ConstraintList {
    pub fn one_of_exists(&self, mut condition: impl FnMut(&QualType) -> bool) -> bool {
        for ty in &self.one_of {
            if condition(ty) { return true; }
        }
        false
    }

    fn is_never(&self) -> bool {
        self.one_of.len() == 1 
            && &self.one_of.first().unwrap().ty == &Type::Never
    }

    pub fn can_unify_to(&self, ty: &QualType) -> Result<(), UnificationError> {
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
            Some(LiteralType::Str) if ty.ty.expressible_by_str_lit() => if ty.is_mut {
                Err(UnificationError::Immutable)
            } else {
                Ok(())
            },
            Some(LiteralType::Char) if ty.ty.expressible_by_char_lit() => if ty.is_mut {
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

    pub fn set_to(&mut self, ty: impl Into<QualType>) {
        // If we're never, we should stay never.
        if !self.is_never() {
            self.one_of = smallvec![ty.into()];
        }
    }

    pub fn intersect_with(&self, other: &ConstraintList) -> ConstraintList {
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
                            fn str_lit(ty: &QualType) -> bool { ty.ty.expressible_by_str_lit() }
                            fn char_lit(ty: &QualType) -> bool { ty.ty.expressible_by_char_lit() }
                            match lit_lit {
                                LiteralType::Dec => dec_lit,
                                LiteralType::Int => int_lit,
                                LiteralType::Str => str_lit,
                                LiteralType::Char => char_lit,
                            }
                        },
                    );
                }
            }
            (Some(LiteralType::Dec), _, Some(rhs), _) => match rhs {
                LiteralType::Int | LiteralType::Dec => constraints.literal = Some(LiteralType::Dec),
                LiteralType::Str | LiteralType::Char => constraints.literal = None,
            },
            (Some(LiteralType::Int), _, Some(rhs), _) => match rhs {
                LiteralType::Int | LiteralType::Dec => constraints.literal = Some(rhs),
                LiteralType::Str | LiteralType::Char => constraints.literal = None,
            },
            (Some(LiteralType::Str), _, Some(rhs), _) => match rhs {
                LiteralType::Str | LiteralType::Char => constraints.literal = Some(LiteralType::Str),
                LiteralType::Int | LiteralType::Dec => constraints.literal = None,
            }
            (Some(LiteralType::Char), _, Some(rhs), _) => match rhs {
                LiteralType::Str | LiteralType::Char => constraints.literal = Some(rhs),
                LiteralType::Int | LiteralType::Dec => constraints.literal = None,
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
    pub fn lopsided_intersect_with(&mut self, other: &mut ConstraintList) {
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
                fn str_lit(ty: &mut QualType) -> bool { ty.ty.expressible_by_str_lit() }
                fn char_lit(ty: &mut QualType) -> bool { ty.ty.expressible_by_char_lit() }
                other.one_of.retain(
                    match lhs_lit {
                        LiteralType::Dec => dec_lit,
                        LiteralType::Int => int_lit,
                        LiteralType::Str => str_lit,
                        LiteralType::Char => char_lit,
                    }
                );
            },
            (None, Some(rhs_lit)) => {
                let lit_test = match rhs_lit {
                    LiteralType::Dec => Type::expressible_by_dec_lit,
                    LiteralType::Int => Type::expressible_by_int_lit,
                    LiteralType::Str => Type::expressible_by_str_lit,
                    LiteralType::Char => Type::expressible_by_char_lit,
                };
                if !self.is_never() {
                    let (mut lhs_one_of, mut rhs_one_of) = (SmallVec::new(), SmallVec::new());
                    for lty in &self.one_of {
                        if lit_test(&lty.ty) {
                            lhs_one_of.push(lty.clone());
                            rhs_one_of.push(QualType::from(lty.ty.clone()));
                        } else {
                            for rty in &other.one_of {
                                if rty.ty.trivially_convertible_to(&lty.ty) {
                                    lhs_one_of.push(lty.clone());
                                    rhs_one_of.push(rty.clone());
                                }
                            }
                        }
                    }
                    self.one_of = lhs_one_of;
                    other.one_of = rhs_one_of;
                }
            }
            (Some(_), Some(_)) => self.one_of = SmallVec::new(),
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