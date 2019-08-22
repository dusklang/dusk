use arrayvec::ArrayVec;
use bitflags::bitflags;
use smallvec::{SmallVec, smallvec};

use crate::ty::{Type, QualType};

bitflags! {
    pub struct BuiltinTraits: u32 {
        const INT  = 0b0000_0001;
        // ExpressibleByDecimalLiteral inherits from ExpressibleByIntLiteral
        const DEC  = 0b0000_0011;
        const CHAR = 0b0000_0100;
        // ExpressibleByStringLiteral inherits from ExpressibleByCharLiteral
        const STR  = 0b0000_1100;
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LiteralType { Int, Dec, Str, Char }

impl LiteralType {
    pub fn preferred_type(self) -> Type {
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
    literal: Option<LiteralType>,
    one_of: Option<SmallVec<[QualType; 1]>>,
    preferred_type: Option<QualType>,
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
    pub const fn new(literal: Option<LiteralType>, one_of: Option<SmallVec<[QualType; 1]>>, preferred_type: Option<QualType>) -> Self {
        Self { literal, one_of, preferred_type }
    }

    pub fn solve(&self) -> Result<QualType, ()> {
        if let Some(one_of) = &self.one_of {
            if one_of.is_empty() {
                return Err(());
            } else if one_of.len() == 1 {
                return Ok(one_of[0].clone());
            }
        }

        // At this point either there are multiple candidate types (one_of.len() > 1), or one_of is None
        match self.preferred_type {
            Some(ref pref) if self.can_unify_to(pref).is_ok() => Ok(pref.clone()),
            _ => Err(()),
        }
    }

    pub fn preferred_type(&self) -> Option<&QualType> {
        self.preferred_type.as_ref()
    }

    pub fn filter_map(&self, type_map: impl FnMut(&QualType) -> Option<QualType> + Copy) -> Self {
        Self::new(
            None,
            self.one_of.as_ref().map(|tys| tys.iter().filter_map(type_map).collect()),
            self.preferred_type().and_then(type_map),
        )
    }

    pub fn one_of_exists(&self, mut condition: impl FnMut(&QualType) -> bool) -> bool {
        if let Some(one_of) = &self.one_of {
            for ty in one_of {
                if condition(ty) { return true; }
            }
        }
        false
    }

    pub fn max_ranked_type_with_assoc_data<T: Clone>(&self, mut rank: impl FnMut(&QualType) -> (usize, T)) -> Result<(&QualType, T), Vec<(&QualType, T)>> {
        let one_of = match self.one_of {
            None => return Err(Vec::new()),
            Some(ref one_of) => one_of,
        };
        let mut ranks = [Vec::new(), Vec::new(), Vec::new(), Vec::new()];
        for ty in one_of {
            let (rank, assoc_data) = rank(ty);
            if rank > 0 {
                ranks[rank - 1].push((ty, assoc_data));
            }
        }
        for (i, rank) in ranks.iter().enumerate().rev() {
            if rank.len() == 1 {
                return Ok(rank[0].clone())
            } else if rank.len() > 1 {
                return Err(ranks[i].clone());
            }
        }
        Err(Vec::new())
    }

    pub fn max_ranked_type(&self, mut rank: impl FnMut(&QualType) -> usize) -> Result<&QualType, Vec<&QualType>> {
        self.max_ranked_type_with_assoc_data(|ty| (rank(ty), ()))
            .map(|(ty, _)| ty)
            .map_err(|tys| tys.iter().map(|(ty, _)| *ty).collect())
    }

    fn is_never(&self) -> bool {
        match self.one_of {
            None => false,
            Some(ref one_of) => one_of.len() == 1 
                && one_of.first().unwrap().ty == Type::Never,
        }
    }

    fn type_expressible_by_literal(ty: &Type, lit: LiteralType) -> bool {
        match lit {
            LiteralType::Dec => ty.expressible_by_dec_lit(),
            LiteralType::Int => ty.expressible_by_int_lit(),
            LiteralType::Str => ty.expressible_by_str_lit(),
            LiteralType::Char => ty.expressible_by_char_lit(),
        }
    }

    pub fn can_unify_to(&self, ty: &QualType) -> Result<(), UnificationError> {
        // Never is the "bottom type", so it unifies to anything.
        if self.one_of_exists(|ty| ty.ty == Type::Never) { return Ok(()); }

        use UnificationError::*;
        if let Some(lit) = self.literal {
            if ty.is_mut { return Err(Immutable); }
            if !Self::type_expressible_by_literal(&ty.ty, lit) {
                return Err(Literal(lit));
            }
        }
        if let Some(one_of) = &self.one_of {
            for oty in one_of {
                if oty.trivially_convertible_to(ty) {
                    return Ok(());
                }
            }
            return Err(InvalidChoice(one_of));
        }

        Ok(())
    }

    pub fn set_to(&mut self, ty: impl Into<QualType>) {
        // If we're never, we should stay never.
        if !self.is_never() {
            self.one_of = Some(smallvec![ty.into()]);
        }
    }

    pub fn intersect_with(&self, other: &ConstraintList) -> ConstraintList {
        if self.is_never() {
            return other.clone();
        } else if other.is_never() {
            return self.clone();
        }

        let requirements: ArrayVec<[LiteralType; 2]> = self.literal.into_iter().chain(other.literal.into_iter()).collect();
        let literal = match (self.literal, other.literal) {
            (Some(LiteralType::Dec), Some(rhs)) => match rhs {
                LiteralType::Int | LiteralType::Dec => Some(LiteralType::Dec),
                LiteralType::Str | LiteralType::Char => None,
            },
            (Some(LiteralType::Int), Some(rhs)) => match rhs {
                LiteralType::Int | LiteralType::Dec => Some(rhs),
                LiteralType::Str | LiteralType::Char => None,
            },
            (Some(LiteralType::Str), Some(rhs)) => match rhs {
                LiteralType::Str | LiteralType::Char => Some(LiteralType::Str),
                LiteralType::Int | LiteralType::Dec => None,
            }
            (Some(LiteralType::Char), Some(rhs)) => match rhs {
                LiteralType::Str | LiteralType::Char => Some(rhs),
                LiteralType::Int | LiteralType::Dec => None,
            }
            (Some(lit), None) | (None, Some(lit)) => Some(lit),
            (None, None) => None,
        };

        let one_of = match (&self.one_of, &other.one_of) {
            (None, None) => None,
            (Some(one_of), None) | (None, Some(one_of)) => Some(one_of.clone()),
            (Some(lhs), Some(rhs)) => {
                let mut one_of = SmallVec::new();
                for lty in lhs {
                    for &req in &requirements {
                        if !Self::type_expressible_by_literal(&lty.ty, req) { continue }
                    }

                    for rty in rhs {
                        for &req in &requirements {
                            if !Self::type_expressible_by_literal(&rty.ty, req) { continue }
                        }

                        // TODO: would it be ok to break from this loop after finding a match here?
                        if lty.trivially_convertible_to(rty) {
                            one_of.push(rty.clone());
                        } else if rty.trivially_convertible_to(lty) {
                            one_of.push(lty.clone());
                        }
                    }
                }
                Some(one_of)
            }
        };

        let preferred_type = if self.preferred_type == other.preferred_type {
            self.preferred_type.clone()
        } else {
            let mut pref = None;
            for (a, b) in &[(self, other), (other, self)] {
                if let Some(preferred_type) = &a.preferred_type {
                    // I don't actually know if it's possible for an expression to not be able to unify to its preferred type?
                    assert!(a.can_unify_to(preferred_type).is_ok());
                    if b.can_unify_to(preferred_type).is_ok() {
                        pref = Some(preferred_type.clone());
                    }
                }
            }
            pref
        };

        Self::new(literal, one_of, preferred_type)
    }

    // Same as intersect_with, but: 
    //  - mutates `self` and `other` in-place instead of creating a new `ConstraintList`
    //  - evaluates mutability independently between the arguments, with precedence given to self
    //  - is literally just used for assignment expressions
    //  - terrible abstraction :(
    pub fn lopsided_intersect_with(&mut self, other: &mut ConstraintList) {
        let literal = match (self.literal, other.literal) {
            (Some(LiteralType::Dec), Some(rhs)) => match rhs {
                LiteralType::Int | LiteralType::Dec => Some(LiteralType::Dec),
                LiteralType::Str | LiteralType::Char => None,
            },
            (Some(LiteralType::Int), Some(rhs)) => match rhs {
                LiteralType::Int | LiteralType::Dec => Some(rhs),
                LiteralType::Str | LiteralType::Char => None,
            },
            (Some(LiteralType::Str), Some(rhs)) => match rhs {
                LiteralType::Str | LiteralType::Char => Some(LiteralType::Str),
                LiteralType::Int | LiteralType::Dec => None,
            }
            (Some(LiteralType::Char), Some(rhs)) => match rhs {
                LiteralType::Str | LiteralType::Char => Some(rhs),
                LiteralType::Int | LiteralType::Dec => None,
            }
            (Some(lit), None) | (None, Some(lit)) => Some(lit),
            (None, None) => None,
        };
        self.literal = literal;
        other.literal = literal;

        if !self.is_never() && !other.is_never() {
            let lhs = self.one_of.as_mut().expect("can't assign to expression without a one-of constraint");
            fn type_expressible_by_literal(ty: &Type, lit: Option<LiteralType>) -> bool {
                match lit {
                    None => true,
                    Some(lit) => ConstraintList::type_expressible_by_literal(ty, lit),
                }
            }

            if other.one_of.is_some() {
                lhs.retain(|lty|
                    type_expressible_by_literal(&lty.ty, literal) && other.one_of_exists(|rty| rty.ty.trivially_convertible_to(&lty.ty))
                );
                let rhs = other.one_of.as_mut().unwrap();
                rhs.retain(|rty|
                    type_expressible_by_literal(&rty.ty, literal) && self.one_of_exists(|lty| rty.ty.trivially_convertible_to(&lty.ty))
                );
            }
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
        assert!(self.one_of.as_ref().unwrap().is_empty() || self.one_of_exists(|ty| ty.is_mut), "can't assign to immutable expression");
    }
}