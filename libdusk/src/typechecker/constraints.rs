use smallvec::SmallVec;
use std::collections::HashMap;

use dire::ty::{Type, FunctionType, QualType, IntWidth};
use dire::hir::GenericParamId;

use crate::ty::BuiltinTraits;

pub type GenericContext = HashMap<GenericParamId, ConstraintList>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypePossibilities {
    one_of: SmallVec<[QualType; 1]>,
    generic_context: SmallVec<[GenericContext; 1]>,
}

impl<I: IntoIterator<Item=QualType>> From<I> for TypePossibilities where I::IntoIter: ExactSizeIterator {
    fn from(one_of: I) -> Self {
        let iter = one_of.into_iter();
        let len = iter.len();
        Self {
            one_of: iter.collect(),
            generic_context: std::iter::repeat_with(|| GenericContext::new()).take(len).collect(),
        }
    }
}

impl TypePossibilities {
    pub fn new() -> Self {
        Self {
            one_of: SmallVec::new(),
            generic_context: SmallVec::new(),
        }
    }

    pub fn is_empty(&self) -> bool {
        debug_assert_eq!(self.one_of.len(), self.generic_context.len());
        self.one_of.is_empty()
    }

    pub fn reserve(&mut self, additional: usize) {
        self.one_of.reserve(additional);
        self.generic_context.reserve(additional);
    }

    pub fn retain(&mut self, mut pred: impl FnMut(&mut QualType, &mut GenericContext) -> bool) {
        // TODO: efficiency! This could just be one loop over both collections simultaneously, but instead I loop over them both once, then each one again individually.
        debug_assert_eq!(self.one_of.len(), self.generic_context.len());
        let mut pred_result = Vec::with_capacity(self.one_of.len());
        for (ty, ctx) in self.iter_mut() {
            pred_result.push(pred(ty, ctx));
        }

        let mut i = 0;
        self.one_of.retain(|_| {
            let pred_result = pred_result[i];
            i += 1;
            pred_result
        });

        let mut i = 0;
        self.generic_context.retain(|_| {
            let pred_result = pred_result[i];
            i += 1;
            pred_result
        });
    }

    pub fn push(&mut self, ty: QualType, ctx: GenericContext) {
        debug_assert_eq!(self.one_of.len(), self.generic_context.len());
        self.one_of.push(ty);
        self.generic_context.push(ctx);
    }

    pub fn iter(&self) -> impl ExactSizeIterator<Item=(&QualType, &GenericContext)> {
        debug_assert_eq!(self.one_of.len(), self.generic_context.len());
        self.one_of.iter().zip(&self.generic_context)
    }

    pub fn iter_mut(&mut self) -> impl ExactSizeIterator<Item=(&mut QualType, &mut GenericContext)> {
        debug_assert_eq!(self.one_of.len(), self.generic_context.len());
        self.one_of.iter_mut().zip(&mut self.generic_context)
    }
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ConstraintList {
    trait_impls: BuiltinTraits,
    one_of: Option<TypePossibilities>,
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

pub const fn none() -> Option<[QualType; 0]> { None }

impl ConstraintList {
    pub fn new<P: Into<TypePossibilities>>(trait_impls: BuiltinTraits, one_of: Option<P>, preferred_type: Option<QualType>) -> Self {
        Self { trait_impls, one_of: one_of.map(|one_of| one_of.into()), preferred_type, is_error: false }
    }

    pub fn one_of(&self) -> &[QualType] {
        if let Some(one_of) = &self.one_of {
            &one_of.one_of
        } else {
            &[]
        }
    }

    pub fn get_one_of(&self) -> Option<&TypePossibilities> {
        self.one_of.as_ref()
    }

    pub fn get_one_of_mut(&mut self) -> Option<&mut TypePossibilities> {
        self.one_of.as_mut()
    }

    pub fn solve(&self) -> Result<QualType, SolveError> {
        if let Some(one_of) = &self.one_of {
            if one_of.one_of.len() == 1 {
                return Ok(one_of.one_of[0].clone())
            } else if one_of.one_of.is_empty() {
                return Ok(Type::Error.into())
            }
        }
        
        match self.preferred_type {
            Some(ref pref) => if can_unify_to(self, pref).is_ok() {
                Ok(pref.clone())
            } else if let Some(one_of) = &self.one_of {
                Err(SolveError::Ambiguous { choices: &one_of.one_of })
            } else {
                Err(SolveError::CantUnifyToPreferredType)
            },
            None => Err(SolveError::NoValidChoices),
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
                one_of.one_of.is_empty() ||
                one_of.one_of
                    .iter()
                    .find(|ty|
                        &ty.ty == &Type::Error
                    )
                    .is_some()
            )
            .unwrap_or(false)
    }

    pub fn filter_map(&self, type_map: impl FnMut(&QualType) -> Option<QualType> + Copy) -> Self {
        Self::new(
            BuiltinTraits::empty(),
            self.one_of.as_ref().map(|tys| -> Vec<_> { tys.one_of.iter().filter_map(type_map).collect() }),
            self.preferred_type().and_then(type_map),
        )
    }

    pub fn one_of_exists(&self, mut condition: impl FnMut(&QualType) -> bool) -> bool {
        if let Some(one_of) = &self.one_of {
            for ty in &one_of.one_of {
                if condition(ty) { return true; }
            }
        }
        false
    }

    pub fn set_one_of(&mut self, one_of: impl Into<TypePossibilities>) {
        self.one_of = Some(one_of.into());
    }

    pub fn max_ranked_type_with_assoc_data<T: Clone>(&self, mut rank: impl FnMut(&QualType) -> (usize, T)) -> Result<(&QualType, T), Vec<(&QualType, T)>> {
        let one_of = match self.one_of {
            None => return Err(Vec::new()),
            Some(ref one_of) => one_of,
        };
        let mut ranks = [Vec::new(), Vec::new(), Vec::new(), Vec::new()];
        for ty in &one_of.one_of {
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
            Some(ref one_of) => one_of.one_of.len() == 1 
                && one_of.one_of.first().unwrap().ty == Type::Never,
        }
    }
}

fn generic_constraints_mut<'a>(constraints: &'a mut [ConstraintList], generic_params: &[GenericParamId], id: GenericParamId) -> Option<&'a mut ConstraintList> {
    if let Some(index) = generic_params.iter().enumerate().find(|(_, &oid)| oid == id).map(|(i, _)| i) {
        Some(&mut constraints[index])
    } else {
        None
    }
}

fn implements_traits(ty: &Type, traits: BuiltinTraits) -> Result<(), BuiltinTraits> {
    let mut not_implemented = BuiltinTraits::empty();
    fn expressible_by_str_lit(ty: &Type) -> bool {
        match ty {
            Type::Pointer(pointee) => matches!(pointee.ty, Type::Int { width: IntWidth::W8, .. }) && !pointee.is_mut,
            Type::StringLiteral => true,
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

fn implements_traits_in_generic_context(ty: &Type, traits: BuiltinTraits, generic_params: &[GenericParamId]) -> Result<SmallVec<[ConstraintList; 1]>, BuiltinTraits> {
    let mut constraints: SmallVec<_> = generic_params.iter().map(|_| ConstraintList::new(BuiltinTraits::empty(), none(), None)).collect();

    let mut not_implemented = BuiltinTraits::empty();
    fn expressible_by_str_lit(ty: &Type) -> bool {
        if let Type::Pointer(pointee) = ty {
            matches!(pointee.ty, Type::Int { width: IntWidth::W8, .. }) && !pointee.is_mut
        } else {
            false
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

    match ty {
        &Type::GenericParam(id) => {
            let constraints = generic_constraints_mut(&mut constraints, generic_params, id).unwrap();
            constraints.trait_impls = not_implemented;
            not_implemented = BuiltinTraits::empty();
        },
        Type::Pointer(pointee) if !pointee.is_mut => if let &Type::GenericParam(id) = ty {
            let constraints = generic_constraints_mut(&mut constraints, generic_params, id).unwrap();
            if not_implemented.contains(BuiltinTraits::STR) {
                constraints.one_of = Some([Type::u8().into(), Type::i8().into()].into());
                constraints.preferred_type = Some(Type::u8().into());
                not_implemented ^= BuiltinTraits::STR;
            }
        },
        _ => {},
    }

    if not_implemented.is_empty() {
        Ok(constraints)
    } else {
        Err(not_implemented)
    }
}

pub fn can_unify_to<'a>(constraints: &'a ConstraintList, ty: &QualType) -> Result<(), UnificationError<'a>> {
    can_unify_to_in_generic_context(constraints, ty, &[])
}

fn contains_any_of_generic_params(ty: &Type, generic_params: &[GenericParamId]) -> bool {
    match ty {
        Type::GenericParam(id) => generic_params.contains(id),
        Type::Pointer(pointee) => contains_any_of_generic_params(&pointee.ty, generic_params),
        _ => false,
    }
}

pub fn can_unify_to_in_generic_context<'a>(constraints: &'a ConstraintList, ty: &QualType, generic_params: &[GenericParamId]) -> Result<(), UnificationError<'a>> {
    // Never is the "bottom type", so it unifies to anything.
    if constraints.one_of_exists(|ty| ty.ty == Type::Never) { return Ok(()); }

    // If this value is already an error, just say it unifies to anything
    if constraints.is_error() { return Ok(()); }

    if contains_any_of_generic_params(&ty.ty, generic_params) {
        return Ok(());
    }

    use UnificationError::*;
    if let Some(not_implemented) = implements_traits(&ty.ty, constraints.trait_impls).err() {
        return Err(Trait(not_implemented));
    }
    if let Some(one_of) = &constraints.one_of {
        for oty in &one_of.one_of {
            if oty.trivially_convertible_to(ty) {
                return Ok(());
            }
        }
        return Err(InvalidChoice(&one_of.one_of));
    }

    Ok(())
}

impl ConstraintList {
    pub fn set_to(&mut self, ty: impl Into<QualType>) {
        // If we're never, we should stay never.
        if !self.is_never() {
            let ty = ty.into();

            // Preserve generic constraints if possible. TODO: efficiency! The caller should pass in this information
            // instead so we don't have to iterate.
            if let Some(one_of) = &mut self.one_of {
                one_of.retain(|other, _| other == &ty);
                if !one_of.is_empty() { return; }
            }
            self.one_of = Some([ty.into()].into());
        }
    }

    pub fn intersect_with(&self, other: &ConstraintList) -> ConstraintList {
        self.intersect_with_in_generic_context(other, &[])
    }

    pub fn intersect_with_in_generic_context(&self, other: &ConstraintList, generic_params: &[GenericParamId]) -> ConstraintList {
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
                let mut one_of = TypePossibilities::new();
                for (lty, lctx) in lhs.iter() {
                    if implements_traits_in_generic_context(&lty.ty, trait_impls, generic_params).is_err() { continue; }

                    for (rty, rctx) in rhs.iter() {
                        if implements_traits_in_generic_context(&rty.ty, trait_impls, generic_params).is_err() { continue; }

                        // TODO: would it be ok to break from this loop after finding a match here?
                        if lty.trivially_convertible_to(rty) {
                            one_of.push(rty.clone(), rctx.clone());
                        } else if rty.trivially_convertible_to(lty) {
                            one_of.push(lty.clone(), lctx.clone());
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
                    // assert!(can_unify_to(a, preferred_type).is_ok());
                    if can_unify_to_in_generic_context(b, preferred_type, generic_params).is_ok() {
                        pref = Some(preferred_type.clone());
                    }
                }
            }
            pref
        };

        Self::new(trait_impls, one_of, preferred_type)
    }
}

#[derive(Debug)]
pub enum AssignmentError {
    Immutable,
}

impl ConstraintList {
    // Same as intersect_with, but: 
    //  - mutates `self` and `other` in-place instead of creating a new `ConstraintList`
    //  - evaluates mutability independently between the arguments, with precedence given to self
    //  - is literally just used for assignment expressions
    //  - is a terrible abstraction :(
    pub fn lopsided_intersect_with(&mut self, other: &mut ConstraintList) -> Result<(), AssignmentError> {
        let trait_impls = self.trait_impls | other.trait_impls;
        self.trait_impls = trait_impls;
        other.trait_impls = trait_impls;

        if !self.is_never() && !other.is_never() {
            let lhs = self.one_of.as_mut().expect("can't assign to expression without a one-of constraint");
            lhs.retain(|lty, _| implements_traits(&lty.ty, trait_impls).is_ok());
            if other.one_of.is_some() {
                lhs.retain(|lty, _|
                    other.one_of_exists(|rty| rty.ty.trivially_convertible_to(&lty.ty))
                );

                let rhs = other.one_of.as_mut().unwrap();
                rhs.retain(|rty, _|
                    implements_traits(&rty.ty, trait_impls).is_ok() && self.one_of_exists(|lty| rty.ty.trivially_convertible_to(&lty.ty))
                );
            }
        }

        if self.preferred_type.as_ref().map(|ty| &ty.ty) != other.preferred_type.as_ref().map(|ty| &ty.ty) {
            if let Some(preferred_type) = &other.preferred_type {
                // I don't actually know if it's possible for an expression to not be able to unify to its preferred type?
                assert!(can_unify_to(other, preferred_type).is_ok());
                let preferred_type = QualType {
                    ty: preferred_type.ty.clone(),
                    is_mut: true,
                };
                if can_unify_to(self, &preferred_type).is_ok() {
                    self.preferred_type = Some(preferred_type);
                } else {
                    other.preferred_type = None;
                    if other.one_of.is_none() {
                        if let Some(one_of) = &self.one_of {
                            let mut rhs_one_of = TypePossibilities::new();
                            for (ty, ctx) in one_of.iter() {
                                let ty = ty.ty.clone().into();
                                if can_unify_to(other, &ty).is_ok() {
                                    rhs_one_of.push(ty, ctx.clone());
                                }
                            }
                            other.one_of = Some(rhs_one_of);
                        } else {
                            other.one_of = None;
                        }
                    }
                }
            }
            if let Some(preferred_type) = &self.preferred_type {
                // I don't actually know if it's possible for an expression to not be able to unify to its preferred type?
                assert!(can_unify_to(self, preferred_type).is_ok());
                let preferred_type = QualType::from(preferred_type.ty.clone());
                if can_unify_to(other, &preferred_type).is_ok() {
                    other.preferred_type = Some(preferred_type);
                } else {
                    self.preferred_type = None;
                }
            }
        }
        if !self.one_of.as_ref().unwrap().one_of.is_empty() && !self.is_error() && !self.one_of_exists(|ty| ty.is_mut) {
            return Err(AssignmentError::Immutable);
        }
        Ok(())
    }
}

fn match_generic_type(generic_param: GenericParamId, actual_ty: &Type, assumed_ty: &Type) -> Option<Type> {
    match (actual_ty, assumed_ty) {
        (actual_ty, &Type::GenericParam(param)) if param == generic_param => {
            Some(actual_ty.clone())
        },
        (Type::Pointer(actual_pointee), Type::Pointer(assumed_pointee)) => {
            if !actual_pointee.is_mut && assumed_pointee.is_mut {
                None
            } else {
                match_generic_type(generic_param, &actual_pointee.ty, &assumed_pointee.ty)
            }
        },
        _ => {
            None
        }
    }
}

fn contains_generic_param(ty: &Type, generic_param: GenericParamId) -> bool {
    match ty {
        &Type::GenericParam(param) if param == generic_param => true,
        Type::Pointer(pointee) => contains_generic_param(&pointee.ty, generic_param),
        _ => false,
    }
}

fn substitute_generic_args(ty: &mut Type, generic_params: &[GenericParamId], generic_args: &[Type]) {
    match ty {
        Type::GenericParam(generic_param) => {
            let mut replacement = None;
            for (param, arg) in generic_params.iter().zip(generic_args) {
                if generic_param == param {
                    replacement = Some(arg.clone());
                    break;
                }
            }
            if let Some(replacement) = replacement {
                *ty = replacement;
            }
        },
        Type::Pointer(pointee) => substitute_generic_args(&mut pointee.ty, generic_params, generic_args),
        Type::Function(FunctionType { param_tys, return_ty }) => {
            substitute_generic_args(return_ty, generic_params, generic_args);
            for param_ty in param_tys {
                substitute_generic_args(param_ty, generic_params, generic_args);
            }
        },
        _ => {},
    }
}

impl ConstraintList {
    /// Gets the constraints on `generic_param` implied if we are assumed to be of type `assumed_ty`
    pub fn get_implied_generic_constraints(&self, generic_param: GenericParamId, assumed_ty: &Type) -> ConstraintList {
        let mut constraints = ConstraintList::default();

        if !contains_generic_param(assumed_ty, generic_param) {
            return constraints;
        }

        if let Some(one_of) = &self.one_of {
            let mut generic_one_of = SmallVec::<[QualType; 1]>::new();
            for ty in &one_of.one_of {
                if let Some(gen_match) = match_generic_type(generic_param, &ty.ty, assumed_ty) {
                    generic_one_of.push(gen_match.into());
                }
            }
            constraints.one_of = Some(generic_one_of.into());
        }

        if let &Type::GenericParam(param) = assumed_ty {
            if param == generic_param {
                constraints.trait_impls = self.trait_impls;
            }
        }

        if let Some(preferred_ty) = &self.preferred_type {
            if let Some(gen_match) = match_generic_type(generic_param, &preferred_ty.ty, assumed_ty) {
                constraints.preferred_type = Some(gen_match.into());
            }
        }

        constraints
    }

    pub fn substitute_generic_args(&mut self, generic_params: &[GenericParamId], generic_args: &[Type]) {
        assert_eq!(generic_params.len(), generic_args.len());
        if let Some(one_of) = &mut self.one_of {
            for ty in &mut one_of.one_of {
                substitute_generic_args(&mut ty.ty, generic_params, generic_args);
            }
        }

        if let Some(preferred_ty) = &mut self.preferred_type {
            substitute_generic_args(&mut preferred_ty.ty, generic_params, generic_args);
        }
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