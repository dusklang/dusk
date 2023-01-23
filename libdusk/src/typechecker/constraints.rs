use dusk_proc_macros::ef;
use smallvec::{SmallVec, smallvec};
use std::collections::HashMap;

use crate::dire::ty::{Type, LegacyInternalType, FunctionType, QualType, IntWidth};
use crate::dire::ast::{GenericParamId, ExprId, GenericCtx, GenericCtxId, BLANK_GENERIC_CTX};

use crate::driver::Driver;
use crate::ty::BuiltinTraits;
use crate::type_provider::TypeProvider;

pub type GenericContext = HashMap<GenericParamId, ConstraintList>;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ConstraintList {
    trait_impls: BuiltinTraits,
    one_of: Option<SmallVec<[QualType; 1]>>,
    preferred_type: Option<QualType>,
    generic_ctx: GenericCtxId,
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
    pub fn new(trait_impls: BuiltinTraits, one_of: Option<SmallVec<[QualType; 1]>>, preferred_type: Option<QualType>, generic_ctx: GenericCtxId) -> Self {
        Self { trait_impls, one_of: one_of.map(|one_of| one_of.into()), preferred_type, generic_ctx: generic_ctx, is_error: false }
    }

    pub fn set_generic_ctx(&mut self, generic_ctx: GenericCtxId) {
        self.generic_ctx = generic_ctx;
    }

    pub fn one_of(&self) -> &[QualType] {
        if let Some(one_of) = &self.one_of {
            one_of
        } else {
            &[]
        }
    }

    pub fn solve(&self) -> Result<QualType, SolveError> {
        if let Some(one_of) = &self.one_of {
            if one_of.len() == 1 {
                return Ok(one_of[0].clone())
            } else if one_of.is_empty() {
                return Err(SolveError::NoValidChoices)
            }
        }
        
        match self.preferred_type {
            Some(ref pref) => if can_unify_to(self, pref).is_ok() {
                Ok(pref.clone())
            } else if let Some(one_of) = &self.one_of {
                Err(SolveError::Ambiguous { choices: &one_of })
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
            self.generic_ctx,
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

fn generic_constraints_mut<'a>(constraints: &'a mut [ConstraintList], generic_params: &[GenericParamId], id: GenericParamId) -> Option<&'a mut ConstraintList> {
    generic_params.iter().enumerate()
        .find(|(_, &oid)| oid == id)
        .map(|(i, _)| &mut constraints[i])
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

fn implements_traits_in_generic_context(ty: &Type, traits: BuiltinTraits, generic_params: &[GenericParamId]) -> Result<SmallVec<[ConstraintList; 1]>, BuiltinTraits> {
    let mut constraints: SmallVec<_> = generic_params.iter().map(|_| ConstraintList::new(BuiltinTraits::empty(), None, None, BLANK_GENERIC_CTX)).collect();

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
                constraints.one_of = Some(smallvec![Type::u8().into(), Type::i8().into()]);
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

pub enum UnificationType<'a> {
    QualType(&'a QualType),
    // TODO: get rid of this
    QualTypeWithOldSchoolGenericContext(&'a QualType, &'a [GenericParamId]),
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

impl Driver {
    // TODO: delete the other variants of this function
    pub fn can_unify_to<'a, 'b: 'a>(&'a self, tp: &'a impl TypeProvider, expr: ExprId, ty: impl Into<UnificationType<'b>>) -> Result<(), UnificationError<'a>> {
        let constraints = tp.constraints(expr);
        // Never is the "bottom type", so it unifies to anything.
        if constraints.one_of_exists(|ty| ty.ty == Type::Never) { return Ok(()); }

        // If this value is already an error, just say it unifies to anything
        if constraints.is_error() { return Ok(()); }

        // TODO: more robust logic that looks at the current constraints of generic types
        let ty = ty.into();
        let (ty, generic_params) = match ty {
            UnificationType::UnevaluatedType(ty) => {
                let mut generic_ctx_id = ef!(ty.generic_ctx_id);
                let generic_params: &[GenericParamId] = loop {
                    match self.code.ast.generic_ctxs[generic_ctx_id] {
                        GenericCtx::Blank => break &[],
                        GenericCtx::Decl { ref parameters, .. } => {
                            break parameters.as_slice();
                        },
                        GenericCtx::DeclRef { parent, .. } => {
                            generic_ctx_id = parent;
                        }
                    }
                };
                let ty: QualType = tp.get_evaluated_type(ty).clone().into();
                (ty, generic_params)
            },
            UnificationType::QualTypeWithOldSchoolGenericContext(ty, generic_params) => (ty.clone(), generic_params),
            UnificationType::QualType(ty) => {
                let generic_params: &[GenericParamId] = &[];
                (ty.clone(), generic_params)
            },
        };
        // TODO: this is dumb because I just automatically allow unification to any type that contains any of the
        // contextual generic parameters. I should actually look at the constraints of the generic parameters to
        // determine whether unification is possible.
        if contains_any_of_generic_params(&ty.ty, generic_params) {
            return Ok(());
        }

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
        for oty in one_of {
            if oty.trivially_convertible_to(ty) {
                return Ok(());
            }
        }
        return Err(InvalidChoice(&one_of));
    }

    Ok(())
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

    pub fn set_to_c_variadic_compatible_type(&mut self) {
        // If we're never, we should stay never.
        if self.is_never() { return; }

        self.one_of = Some([self.solve().unwrap()].into());
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
                let mut one_of = SmallVec::new();
                for lty in lhs.iter() {
                    if implements_traits_in_generic_context(&lty.ty, trait_impls, generic_params).is_err() { continue; }

                    for rty in rhs.iter() {
                        if implements_traits_in_generic_context(&rty.ty, trait_impls, generic_params).is_err() { continue; }

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
                    // assert!(can_unify_to(a, preferred_type).is_ok());
                    if can_unify_to_in_generic_context(b, preferred_type, generic_params).is_ok() {
                        pref = Some(preferred_type.clone());
                    }
                }
            }
            pref
        };

        // Apparently the generic contexts are not guaranteed to match; yuck.
        // On second thought, this makes perfect sense.
        // TODO: I probably need something a little more complex than a single GenericCtxId per ConstraintList (but
        // hopefully something not too much more complex)
        //   assert_eq!(self.generic_ctx, other.generic_ctx, "this might fail...");
        Self::new(trait_impls, one_of, preferred_type, self.generic_ctx)
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
            lhs.retain(|lty| implements_traits(&lty.ty, trait_impls).is_ok());
            if other.one_of.is_some() {
                lhs.retain(|lty|
                    other.one_of_exists(|rty| rty.ty.trivially_convertible_to(&lty.ty))
                );

                let rhs = other.one_of.as_mut().unwrap();
                rhs.retain(|rty|
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
                            let mut rhs_one_of = SmallVec::new();
                            for ty in one_of.iter() {
                                let ty = ty.ty.clone().into();
                                if can_unify_to(other, &ty).is_ok() {
                                    rhs_one_of.push(ty);
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
        if !self.one_of.as_ref().unwrap().is_empty() && !self.is_error() && !self.one_of_exists(|ty| ty.is_mut) {
            return Err(AssignmentError::Immutable);
        }
        Ok(())
    }
}

fn match_generic_type(generic_param: GenericParamId, actual_ty: &Type, assumed_ty: &Type) -> Option<Type> {
    match (actual_ty, assumed_ty) {
        (actual_ty, &Type::GenericParam(param)) if param == generic_param =>
            Some(actual_ty.clone()),
        (Type::Pointer(actual_pointee), Type::Pointer(assumed_pointee)) =>
            (actual_pointee.is_mut || !assumed_pointee.is_mut)
                .then(|| match_generic_type(generic_param, &actual_pointee.ty, &assumed_pointee.ty))
                .flatten(),
        _ => None,
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
        Type::Function(FunctionType { param_tys, return_ty, .. }) => {
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
        // TODO: set the proper generic ctx on this
        let mut constraints = ConstraintList::default();

        if !contains_generic_param(assumed_ty, generic_param) {
            return constraints;
        }

        if let Some(one_of) = &self.one_of {
            let mut generic_one_of = SmallVec::<[QualType; 1]>::new();
            for ty in one_of {
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
            for ty in one_of {
                substitute_generic_args(&mut ty.ty, generic_params, generic_args);
            }
        }

        if let Some(preferred_ty) = &mut self.preferred_type {
            substitute_generic_args(&mut preferred_ty.ty, generic_params, generic_args);
        }
    }
}
