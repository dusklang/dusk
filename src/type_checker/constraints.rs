use smallvec::{SmallVec, smallvec};

use crate::ty::{Type, QualType, BuiltinTraits};

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ConstraintList {
    trait_impls: BuiltinTraits,
    one_of: Option<SmallVec<[QualType; 1]>>,
    preferred_type: Option<QualType>,
}

pub enum UnificationError<'a> {
    /// The expression was constrained to implement a trait that the requested type doesn't implement
    Trait(BuiltinTraits),
    /// The expression didn't have the requested type in its list of type choices
    InvalidChoice(&'a [QualType]),
}

impl ConstraintList {
    pub const fn new(trait_impls: BuiltinTraits, one_of: Option<SmallVec<[QualType; 1]>>, preferred_type: Option<QualType>) -> Self {
        Self { trait_impls, one_of, preferred_type }
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
            BuiltinTraits::empty(),
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

    pub fn can_unify_to(&self, ty: &QualType) -> Result<(), UnificationError> {
        // Never is the "bottom type", so it unifies to anything.
        if self.one_of_exists(|ty| ty.ty == Type::Never) { return Ok(()); }

        use UnificationError::*;
        if let Some(not_implemented) = ty.ty.implements_traits(self.trait_impls).err() {
            return Err(Trait(not_implemented));
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

        let trait_impls = self.trait_impls | other.trait_impls;

        let one_of = match (&self.one_of, &other.one_of) {
            (None, None) => None,
            (Some(one_of), None) | (None, Some(one_of)) => Some(one_of.clone()),
            (Some(lhs), Some(rhs)) => {
                let mut one_of = SmallVec::new();
                for lty in lhs {
                    if lty.ty.implements_traits(trait_impls).is_err() { continue; }

                    for rty in rhs {
                        if rty.ty.implements_traits(trait_impls).is_err() { continue; }

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

        Self::new(trait_impls, one_of, preferred_type)
    }

    // Same as intersect_with, but: 
    //  - mutates `self` and `other` in-place instead of creating a new `ConstraintList`
    //  - evaluates mutability independently between the arguments, with precedence given to self
    //  - is literally just used for assignment expressions
    //  - terrible abstraction :(
    pub fn lopsided_intersect_with(&mut self, other: &mut ConstraintList) {
        let trait_impls = self.trait_impls | other.trait_impls;
        self.trait_impls = trait_impls;
        other.trait_impls = trait_impls;

        if !self.is_never() && !other.is_never() {
            let lhs = self.one_of.as_mut().expect("can't assign to expression without a one-of constraint");
            if other.one_of.is_some() {
                lhs.retain(|lty|
                    lty.ty.implements_traits(trait_impls).is_ok() && other.one_of_exists(|rty| rty.ty.trivially_convertible_to(&lty.ty))
                );
                let rhs = other.one_of.as_mut().unwrap();
                rhs.retain(|rty|
                    rty.ty.implements_traits(trait_impls).is_ok() && self.one_of_exists(|lty| rty.ty.trivially_convertible_to(&lty.ty))
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

    pub fn print_diff(&self, other: &ConstraintList) {
        if self.trait_impls != other.trait_impls {
            let old = self.trait_impls.names();
            let new = other.trait_impls.names();
            for name in &old {
                if !new.contains(name) {
                    println!("no longer requires {}", name);
                }
            }
            for name in &new {
                if !old.contains(name) {
                    println!("now requires {}", name);
                }
            }
        }

        if self.one_of != other.one_of {
            if let Some(new) = &other.one_of {
                println!("now one of {:?}", new);
            }
        }

        if self.preferred_type != other.preferred_type {
            if let Some(new) = &other.preferred_type {
                println!("set preferred type to {:?}", new);
            } else {
                println!("removed preferred type {:?}", self.preferred_type.as_ref().unwrap())
            }
        }

        println!();
    }
}