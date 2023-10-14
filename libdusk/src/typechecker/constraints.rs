use smallvec::SmallVec;

use crate::ty::{Type, LegacyInternalType, QualType, IntWidth};
use crate::ast::ExprId;

use crate::driver::Driver;
use crate::ty::BuiltinTraits;
use crate::type_provider::TypeProvider;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ConstraintList {
    trait_impls: BuiltinTraits,
    one_of: Option<SmallVec<[QualType; 1]>>,
    preferred_type: Option<QualType>,
    is_error: bool,
}

pub enum UnificationError<'a> {
    /// The expression was constrained to implement a trait that the requested type doesn't implement
    Trait(BuiltinTraits),
    /// The expression didn't have the requested type in its list of type choices
    InvalidChoice(&'a [QualType]),
}

#[derive(Debug)]
pub enum SolveError<'a> {
    NoValidChoices,
    CantUnifyToPreferredType,
    Ambiguous { choices: &'a [QualType] }
}

impl ConstraintList {
    pub fn new(trait_impls: BuiltinTraits, one_of: Option<SmallVec<[QualType; 1]>>, preferred_type: Option<QualType>) -> Self {
        Self { trait_impls, one_of: one_of.map(|one_of| one_of.into()), preferred_type, is_error: false }
    }

    pub fn one_of(&self) -> &[QualType] {
        if let Some(one_of) = &self.one_of {
            one_of
        } else {
            &[]
        }
    }

    pub fn set_preferred_type(&mut self, ty: impl Into<QualType>) {
        self.preferred_type = Some(ty.into());
    }

    pub fn preferred_type(&self) -> Option<&QualType> {
        self.preferred_type.as_ref()
    }

    pub fn make_error(&mut self) {
        self.is_error = true;
    }

    pub fn is_error(&self) -> bool {
        self.is_error ||
        self.one_of
            .as_ref()
            .map(|one_of|
                one_of.is_empty() ||
                one_of
                    .iter()
                    .any(|ty|
                        ty.ty == Type::Error
                    )
            )
            .unwrap_or(false)
    }

    pub fn filter_map(&self, type_map: impl FnMut(&QualType) -> Option<QualType> + Copy) -> Self {
        Self::new(
            BuiltinTraits::empty(),
            self.one_of.as_ref().map(|one_of| -> SmallVec<[QualType; 1]> { one_of.iter().filter_map(type_map).collect() }),
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

    pub fn set_one_of(&mut self, one_of: impl Into<SmallVec<[QualType; 1]>>) {
        self.one_of = Some(one_of.into());
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
}

fn implements_traits(ty: &Type, traits: BuiltinTraits) -> Result<(), BuiltinTraits> {
    let mut not_implemented = BuiltinTraits::empty();
    fn expressible_by_str_lit(ty: &Type) -> bool {
        match ty {
            Type::Pointer(pointee) => matches!(pointee.ty, Type::Int { width: IntWidth::W8, .. }) && !pointee.is_mut,
            Type::LegacyInternal(LegacyInternalType::StringLiteral) => true,
            _ => false,
        }
    }
    let mut check_implements = |trayt: BuiltinTraits, check: fn(&Type) -> bool| {
        if traits.contains(trayt) && !check(ty) {
            not_implemented |= trayt;
        }
    };
    check_implements(BuiltinTraits::INT, |ty| matches!(ty, Type::Int { .. } | Type::Float(_)));
    check_implements(BuiltinTraits::DEC, |ty| matches!(ty, Type::Float(_)));
    check_implements(BuiltinTraits::CHAR, |ty| {
        matches!(ty, Type::Int { width: IntWidth::W8, .. }) || expressible_by_str_lit(ty)
    });
    check_implements(BuiltinTraits::STR, expressible_by_str_lit);

    if not_implemented.is_empty() {
        Ok(())
    } else {
        Err(not_implemented)
    }
}

pub enum UnificationType<'a> {
    QualType(&'a QualType),
    UnevaluatedType(ExprId),
}

impl<'a> From<&'a QualType> for UnificationType<'a> {
    fn from(ty: &'a QualType) -> Self {
        UnificationType::QualType(ty)
    }
}

impl From<ExprId> for UnificationType<'static> {
    fn from(ty: ExprId) -> Self {
        UnificationType::UnevaluatedType(ty)
    }
}

#[derive(Copy, Clone)]
pub enum ConstraintHaver<'a> {
    Expr(ExprId),
    ConstraintList(&'a ConstraintList),
}

impl From<ExprId> for ConstraintHaver<'_> {
    fn from(value: ExprId) -> Self {
        ConstraintHaver::Expr(value)
    }
}

impl<'a> From<&'a ConstraintList> for ConstraintHaver<'a> {
    fn from(value: &'a ConstraintList) -> Self {
        ConstraintHaver::ConstraintList(value)
    }
}


pub enum MutConstraintHaver<'a> {
    Expr(ExprId),
    ConstraintListMut(&'a mut ConstraintList),
}

impl From<ExprId> for MutConstraintHaver<'_> {
    fn from(value: ExprId) -> Self {
        MutConstraintHaver::Expr(value)
    }
}

impl<'a> From<&'a mut ConstraintList> for MutConstraintHaver<'a> {
    fn from(value: &'a mut ConstraintList) -> Self {
        MutConstraintHaver::ConstraintListMut(value)
    }
}

impl<'a> From<MutConstraintHaver<'a>> for ConstraintHaver<'a> {
    fn from(value: MutConstraintHaver<'a>) -> Self {
        match value {
            MutConstraintHaver::ConstraintListMut(constraints) => ConstraintHaver::ConstraintList(constraints),
            MutConstraintHaver::Expr(expr) => ConstraintHaver::Expr(expr),
        }
    }
}

macro_rules! get_constraints {
    ($tp:expr, $haver:expr) => {
        match $haver.into() {
            ConstraintHaver::ConstraintList(constraints) => constraints,
            ConstraintHaver::Expr(expr) => $tp.expr_constraints(expr),
        }
    };
}

macro_rules! get_constraints_mut {
    ($tp:expr, $haver:expr) => {
        match $haver.clone().into() {
            MutConstraintHaver::ConstraintListMut(constraints) => constraints,
            MutConstraintHaver::Expr(expr) => $tp.expr_constraints_mut(expr),
        }
    };
}


impl Driver {
    pub fn can_unify_to<'a, 'b: 'a>(&'a self, tp: &'a impl TypeProvider, constraint_haver: impl Into<ConstraintHaver<'a>>, ty: impl Into<UnificationType<'b>>) -> Result<(), UnificationError<'a>> {
        let constraints = match constraint_haver.into() {
            ConstraintHaver::Expr(expr) => tp.expr_constraints(expr),
            ConstraintHaver::ConstraintList(constraints) => constraints,
        };
        // Never is the "bottom type", so it unifies to anything.
        if constraints.one_of_exists(|ty| ty.ty == Type::Never) { return Ok(()); }

        // If this value is already an error, just say it unifies to anything
        if constraints.is_error() { return Ok(()); }

        // TODO: more robust logic that looks at the current constraints of generic types
        let ty = ty.into();
        let ty = match ty {
            UnificationType::UnevaluatedType(ty) => tp.get_evaluated_type(ty).clone().into(),
            UnificationType::QualType(ty) => ty.clone(),
        };

        use UnificationError::*;
        if let Some(not_implemented) = implements_traits(&ty.ty, constraints.trait_impls).err() {
            return Err(Trait(not_implemented));
        }
        if let Some(one_of) = &constraints.one_of {
            for oty in one_of {
                if oty.trivially_convertible_to(&ty) {
                    return Ok(());
                }
            }
            return Err(InvalidChoice(&one_of));
        }

        Ok(())
    }

    pub fn intersect_constraints<'a>(&self, tp: &impl TypeProvider, constraints: impl Into<ConstraintHaver<'a>> + Clone, other: impl Into<ConstraintHaver<'a>> + Clone) -> ConstraintList {
        let constraints = get_constraints!(tp, constraints);
        let other = get_constraints!(tp, other);
        if constraints.is_never() {
            return other.clone();
        } else if other.is_never() {
            return constraints.clone();
        }

        let trait_impls = constraints.trait_impls | other.trait_impls;

        let one_of = match (&constraints.one_of, &other.one_of) {
            (None, None) => None,
            (Some(one_of), None) | (None, Some(one_of)) => Some(one_of.clone()),
            (Some(lhs), Some(rhs)) => {
                let mut one_of = SmallVec::new();
                for lty in lhs.iter() {
                    if implements_traits(&lty.ty, trait_impls).is_err() { continue; }

                    for rty in rhs.iter() {
                        if implements_traits(&rty.ty, trait_impls).is_err() { continue; }

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

        let preferred_type = if constraints.preferred_type == other.preferred_type {
            constraints.preferred_type.clone()
        } else {
            let mut pref = None;
            for (a, b) in [(constraints, other), (other, constraints)] {
                if let Some(preferred_type) = &a.preferred_type {
                    // I don't actually know if it's possible for an expression to not be able to unify to its preferred type?
                    if self.can_unify_to(tp, b, preferred_type).is_ok() {
                        pref = Some(preferred_type.clone());
                    }
                }
            }
            pref
        };

        ConstraintList::new(trait_impls, one_of, preferred_type)
    }

    // Same as intersect_constraints, but: 
    //  - mutates `self` and `other` in-place instead of creating a new `ConstraintList`
    //  - evaluates mutability independently between the arguments, with precedence given to self
    //  - is literally just used for assignment expressions
    //  - is a terrible abstraction :(
    pub fn intersect_constraints_lopsided<'a>(&self, tp: &mut impl TypeProvider, constraint_haver: impl Into<MutConstraintHaver<'a>> + Clone, other_haver: impl Into<MutConstraintHaver<'a>> + Clone) -> Result<(), AssignmentError> {
        let (constraints, other) = match (constraint_haver.clone().into(), other_haver.clone().into()) {
            (MutConstraintHaver::Expr(expr1), MutConstraintHaver::Expr(expr2)) => tp.multi_constraints_mut(expr1, expr2),
            (MutConstraintHaver::Expr(expr), MutConstraintHaver::ConstraintListMut(constraints)) => (tp.expr_constraints_mut(expr), constraints),
            (MutConstraintHaver::ConstraintListMut(constraints), MutConstraintHaver::Expr(expr)) => (constraints, tp.expr_constraints_mut(expr)),
            (MutConstraintHaver::ConstraintListMut(constraints), MutConstraintHaver::ConstraintListMut(other)) => (constraints, other),
        };
        let trait_impls = constraints.trait_impls | other.trait_impls;
        constraints.trait_impls = trait_impls;
        other.trait_impls = trait_impls;

        if !constraints.is_never() && !other.is_never() {
            let lhs = constraints.one_of.as_mut().expect("can't assign to expression without a one-of constraint");
            lhs.retain(|lty| implements_traits(&lty.ty, trait_impls).is_ok());
            if other.one_of.is_some() {
                lhs.retain(|lty|
                    other.one_of_exists(|rty| rty.ty.trivially_convertible_to(&lty.ty))
                );

                let rhs = other.one_of.as_mut().unwrap();
                rhs.retain(|rty|
                    implements_traits(&rty.ty, trait_impls).is_ok() && constraints.one_of_exists(|lty| rty.ty.trivially_convertible_to(&lty.ty))
                );
            }
        }

        if constraints.preferred_type.as_ref().map(|ty| &ty.ty) != other.preferred_type.as_ref().map(|ty| &ty.ty) {
            if let Some(preferred_type) = other.preferred_type.clone() {
                // I don't actually know if it's possible for an expression to not be able to unify to its preferred type?
                assert!(self.can_unify_to(tp, other_haver.clone().into(), &preferred_type).is_ok());
                let preferred_type = QualType {
                    ty: preferred_type.ty.clone(),
                    is_mut: true,
                };
                if self.can_unify_to(tp, constraint_haver.clone().into(), &preferred_type).is_ok() {
                    get_constraints_mut!(tp, constraint_haver).preferred_type = Some(preferred_type);
                } else {
                    let other = get_constraints_mut!(tp, other_haver);
                    other.preferred_type = None;
                    if other.one_of.is_none() {
                        let constraints = get_constraints_mut!(tp, constraint_haver);
                        if let Some(one_of) = constraints.one_of.clone() {
                            let mut rhs_one_of = SmallVec::new();
                            for ty in one_of.iter() {
                                let ty = ty.ty.clone().into();
                                if self.can_unify_to(tp, other_haver.clone().into(), &ty).is_ok() {
                                    rhs_one_of.push(ty);
                                }
                            }

                            get_constraints_mut!(tp, other_haver).one_of = Some(rhs_one_of);
                        } else {
                            get_constraints_mut!(tp, other_haver).one_of = None;
                        }
                    }
                }
            }
            if let Some(preferred_type) = get_constraints_mut!(tp, constraint_haver.clone()).preferred_type.clone() {
                // I don't actually know if it's possible for an expression to not be able to unify to its preferred type?
                assert!(self.can_unify_to(tp, constraint_haver.clone().into(), &preferred_type).is_ok());
                let preferred_type = QualType::from(preferred_type.ty.clone());
                if self.can_unify_to(tp, other_haver.clone().into(), &preferred_type).is_ok() {
                    get_constraints_mut!(tp, other_haver).preferred_type = Some(preferred_type);
                } else {
                    get_constraints_mut!(tp, constraint_haver).preferred_type = None;
                }
            }
        }
        let constraints = get_constraints_mut!(tp, constraint_haver);
        if !constraints.one_of.as_ref().unwrap().is_empty() && !constraints.is_error() && !constraints.one_of_exists(|ty| ty.is_mut) {
            return Err(AssignmentError::Immutable);
        }
        Ok(())
    }

    pub fn solve_constraints<'a>(&self, tp: &'a impl TypeProvider, constraints: impl Into<ConstraintHaver<'a>>) -> Result<QualType, SolveError<'a>> {
        let constraints = get_constraints!(tp, constraints);

        if let Some(one_of) = &constraints.one_of {
            if one_of.len() == 1 {
                return Ok(one_of[0].clone())
            } else if one_of.is_empty() {
                return Err(SolveError::NoValidChoices)
            }
        }
        
        match constraints.preferred_type {
            Some(ref pref) => if self.can_unify_to(tp, constraints, pref).is_ok() {
                Ok(pref.clone())
            } else if let Some(one_of) = &constraints.one_of {
                Err(SolveError::Ambiguous { choices: &one_of })
            } else {
                Err(SolveError::CantUnifyToPreferredType)
            },
            None => Err(SolveError::NoValidChoices),
        }
    }

    pub fn set_constraints_to_c_variadic_compatible_type<'a>(&self, tp: &mut impl TypeProvider, constraints: impl Into<MutConstraintHaver<'a>> + Clone) {
        // If we're never, we should stay never.
        if get_constraints_mut!(tp, constraints.clone()).is_never() { return; }

        let solution = Some([self.solve_constraints(tp, constraints.clone().into()).unwrap()].into());
        get_constraints_mut!(tp, constraints).one_of = solution;
    }
}

#[derive(Debug)]
pub enum AssignmentError {
    Immutable,
}

impl ConstraintList {
    pub fn set_to(&mut self, ty: impl Into<QualType>) {
        let ty = ty.into();
        // If we're never, we should stay never.
        // If the passed in type is Error, we should stay whatever we are.
        // (this second clause is experimental and may not turn out to be a good idea. reevaluate later.)
        if !self.is_never() && !ty.ty.is_error() {
            // Preserve generic constraints if possible. TODO: efficiency! The caller should pass in this information
            // instead so we don't have to iterate.
            if let Some(one_of) = &mut self.one_of {
                one_of.retain(|other| other == &ty);
                if !one_of.is_empty() { return; }
            }
            self.one_of = Some([ty].into());
        }
    }
}
