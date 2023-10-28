use smallvec::{SmallVec, smallvec};

use crate::ty::{Type, LegacyInternalType, QualType, IntWidth};
use crate::ast::{ExprId, DeclId};

use crate::driver::Driver;
use crate::ty::BuiltinTraits;
use crate::type_provider::TypeProvider;

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct ConstraintList {
    trait_impls: BuiltinTraits,
    one_of: Option<OneOfConstraint>,
    preferred_type: Option<QualType>,
    is_error: bool,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct OneOfConstraint {
    types: SmallVec<[QualType; 1]>,

    // For DeclRefs, this field stores overload decls that the types in the one-of list correspond to.
    // There are also some places where I may allow the decl ids from a DeclRef to trickle up the tree, just
    // out of convenience.
    // It must either be empty, or exactly the same size as `types` above.
    decls: SmallVec<[DeclId; 1]>,
}

impl OneOfConstraint {
    pub fn new() -> Self { Default::default() }

    pub fn reserve(&mut self, additional: usize) {
        self.types.reserve(additional);
        self.decls.reserve(additional);
    }

    pub fn push(&mut self, ty: impl Into<QualType>) {
        assert!(self.decls.is_empty());
        self.types.push(ty.into());
    }

    pub fn push_with_decl(&mut self, ty: impl Into<QualType>, decl: DeclId) {
        assert_eq!(self.types.len(), self.decls.len());
        self.types.push(ty.into());
        self.decls.push(decl);
    }

    pub fn push_with_decl_maybe(&mut self, ty: impl Into<QualType>, decl: Option<DeclId>) {
        if let Some(decl) = decl {
            self.push_with_decl(ty, decl);
        } else {
            self.push(ty);
        }
    }

    pub fn get(&self, index: usize) -> (&QualType, Option<DeclId>) {
        (&self.types[index], (!self.decls.is_empty()).then(|| self.decls[index]))
    }

    pub fn first(&self) -> Option<(&QualType, Option<DeclId>)> {
        (!self.types.is_empty()).then(|| self.get(0))
    }

    pub fn types(&self) -> &[QualType] {
        &self.types
    }

    pub fn decls(&self) -> &[DeclId] {
        assert!(self.types.is_empty() || !self.decls.is_empty());

        &self.decls
    }

    pub fn is_empty(&self) -> bool {
        self.types.is_empty()
    }

    pub fn len(&self) -> usize {
        self.types.len()
    }

    pub fn retain_with_decl_ids(&mut self, mut f: impl FnMut(&mut QualType, Option<DeclId>) -> bool) {
        if self.decls.is_empty() {
            self.types.retain(|ty| f(ty, None));
        } else {
            let len = self.types.len();
            assert_eq!(len, self.decls.len());
            let mut num_deleted = 0;
            for i in 0..len {
                if !f(&mut self.types[i], Some(self.decls[i])) {
                    num_deleted += 1;
                } else if num_deleted > 0 {
                    self.types.swap(i - num_deleted, i);
                    self.decls.swap(i - num_deleted, i);
                }
            }
            self.types.truncate(len - num_deleted);
            self.decls.truncate(len - num_deleted);
        }
    }

    pub fn retain(&mut self, mut f: impl FnMut(&mut QualType) -> bool) {
        self.retain_with_decl_ids(|ty, _| f(ty));
    }
}

impl From<SmallVec<[QualType; 1]>> for OneOfConstraint {
    fn from(types: SmallVec<[QualType; 1]>) -> Self {
        Self {
            types: types.into(),
            decls: Default::default(),
        }
    }
}

impl<const N: usize> From<[QualType; N]> for OneOfConstraint {
    fn from(types: [QualType; N]) -> Self {
        Self {
            types: SmallVec::from_iter(types),
            decls: Default::default(),
        }
    }
}

#[derive(Debug)]
pub enum UnificationError<'a> {
    /// The expression was constrained to implement a trait that the requested type doesn't implement
    Trait(BuiltinTraits),
    /// The expression didn't have the requested type in its list of type choices
    InvalidChoice(&'a [QualType]),
}

pub enum UnificationSuccess {
    IsError,
    IsNever,
    HasTypeInOneOf(Option<DeclId>),
    TraitsImplementedByType,
}

impl UnificationSuccess {
    pub fn get_decl(&self) -> Option<DeclId> {
        match self {
            UnificationSuccess::HasTypeInOneOf(decl) => *decl,
            _ => None,
        }
    }
}

#[derive(Debug)]
pub enum SolveError<'a> {
    NoValidChoices,
    CantUnifyToPreferredType,
    Ambiguous { choices: &'a [QualType] }
}

#[derive(Clone)]
pub struct ConstraintSolution {
    pub qual_ty: QualType,
    pub decl: Option<DeclId>,
}

impl ConstraintSolution {
    pub fn new(ty: impl Into<QualType>) -> Self {
        ConstraintSolution { qual_ty: ty.into(), decl: None }
    }

    pub fn error() -> Self {
        Self::new(Type::Error)
    }

    pub fn with_decl(mut self, decl: DeclId) -> Self {
        self.decl = Some(decl);
        self
    }

    pub fn with_decl_maybe(mut self, decl: Option<DeclId>) -> Self {
        self.decl = decl;
        self
    }

    pub fn make_immutable(mut self) -> Self {
        self.qual_ty.is_mut = false;
        self
    }
}

impl ConstraintList {
    pub fn new() -> Self { Default::default() }

    pub fn with_trait_impls(mut self, trait_impls: BuiltinTraits) -> Self {
        self.trait_impls = trait_impls;
        self
    }

    pub fn with_one_of(mut self, one_of: impl Into<OneOfConstraint>) -> Self {
        self.one_of = Some(one_of.into());
        self
    }

    pub fn with_type(self, ty: impl Into<QualType>) -> Self {
        self.with_one_of(smallvec![ty.into()])
    }

    pub fn with_preferred_type(mut self, preferred_type: impl Into<QualType>) -> Self {
        self.preferred_type = Some(preferred_type.into());
        self
    }

    pub fn with_maybe_preferred_type(mut self, preferred_type: Option<QualType>) -> Self {
        self.preferred_type = preferred_type;
        self
    }

    pub fn one_of(&self) -> Option<&OneOfConstraint> {
        if let Some(one_of) = &self.one_of {
            Some(&one_of)
        } else {
            None
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
                one_of.types
                    .iter()
                    .any(|ty|
                        ty.ty == Type::Error
                    )
            )
            .unwrap_or(false)
    }

    pub fn filter_map(&self, mut type_map: impl FnMut(&QualType) -> Option<QualType> + Copy) -> Self {
        Self {
            trait_impls: BuiltinTraits::empty(),
            one_of: self.one_of.as_ref().map(|one_of| -> OneOfConstraint {
                let mut one_of = one_of.clone();
                one_of.retain(|ty| if let Some(new_ty) = type_map(&*ty) {
                    *ty = new_ty;
                    true
                } else {
                    false
                });
                one_of
            }),
            preferred_type: self.preferred_type().and_then(type_map),
            is_error: self.is_error,
        }
    }

    pub fn retain_in_one_of(&mut self, f: impl FnMut(&mut QualType, Option<DeclId>) -> bool) {
        if let Some(one_of) = &mut self.one_of {
            one_of.retain_with_decl_ids(f);
        }
    }

    pub fn one_of_exists(&self, mut condition: impl FnMut(&QualType) -> bool) -> bool {
        if let Some(one_of) = &self.one_of {
            for ty in &one_of.types {
                if condition(ty) { return true; }
            }
        }
        false
    }

    pub fn set_one_of(&mut self, one_of: impl Into<SmallVec<[QualType; 1]>>) {
        self.one_of = Some(one_of.into().into());
    }

    pub fn max_ranked_type_with_assoc_data<T: Clone>(&self, mut rank: impl FnMut(&QualType) -> (usize, T)) -> Result<(&QualType, Option<DeclId>, T), Vec<(&QualType, Option<DeclId>, T)>> {
        let one_of = match self.one_of {
            None => return Err(Vec::new()),
            Some(ref one_of) => one_of,
        };
        let mut ranks = [Vec::new(), Vec::new(), Vec::new(), Vec::new()];
        for i in 0..one_of.len() {
            let (ty, decl) = one_of.get(i);
            let (rank, assoc_data) = rank(ty);
            if rank > 0 {
                ranks[rank - 1].push((ty, decl, assoc_data));
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

    pub fn max_ranked_type(&self, mut rank: impl FnMut(&QualType) -> usize) -> Result<(&QualType, Option<DeclId>), Vec<(&QualType, Option<DeclId>)>> {
        self.max_ranked_type_with_assoc_data(|ty| (rank(ty), ()))
            .map(|(ty, decl, _)| (ty, decl))
            .map_err(|tys| tys.iter().map(|(ty, decl, _)| (*ty, *decl)).collect())
    }

    fn is_never(&self) -> bool {
        match self.one_of {
            None => false,
            Some(ref one_of) => one_of.len() == 1 
                && one_of.types.first().unwrap().ty == Type::Never,
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
    ($driver:expr, $tp:expr, $haver:expr) => {
        match $haver.into() {
            ConstraintHaver::ConstraintList(constraints) => constraints,
            ConstraintHaver::Expr(expr) => {
                let type_var_id = $driver.code.ast.expr_to_type_vars[expr];
                $tp.constraints(type_var_id)
            },
        }
    };
}

macro_rules! get_constraints_mut {
    ($driver:expr, $tp:expr, $haver:expr) => {
        match $haver.into() {
            MutConstraintHaver::ConstraintListMut(constraints) => constraints,
            MutConstraintHaver::Expr(expr) => {
                let type_var_id = $driver.code.ast.expr_to_type_vars[expr];
                $tp.constraints_mut(type_var_id)
            },
        }
    };
}


impl Driver {
    pub fn can_unify_to<'a, 'b: 'a>(&'a self, tp: &'a impl TypeProvider, constraint_haver: impl Into<ConstraintHaver<'a>>, ty: impl Into<UnificationType<'b>>) -> Result<UnificationSuccess, UnificationError<'a>> {
        let constraints = match constraint_haver.into() {
            ConstraintHaver::Expr(expr) => self.get_constraints(tp, expr),
            ConstraintHaver::ConstraintList(constraints) => constraints,
        };
        // Never is the "bottom type", so it unifies to anything.
        if constraints.one_of_exists(|ty| ty.ty == Type::Never) { return Ok(UnificationSuccess::IsNever); }

        // If this value is already an error, just say it unifies to anything
        if constraints.is_error() { return Ok(UnificationSuccess::IsError); }

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
            for i in 0..one_of.types.len() {
                let (oty, decl) = one_of.get(i);
                if oty.trivially_convertible_to(&ty) {
                    return Ok(UnificationSuccess::HasTypeInOneOf(decl));
                }
            }
            return Err(InvalidChoice(&one_of.types));
        }

        Ok(UnificationSuccess::TraitsImplementedByType)
    }

    pub fn intersect_constraints<'a>(&self, tp: &impl TypeProvider, constraints: impl Into<ConstraintHaver<'a>> + Clone, other: impl Into<ConstraintHaver<'a>> + Clone) -> ConstraintList {
        let constraints = get_constraints!(self, tp, constraints);
        let other = get_constraints!(self, tp, other);
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
                for lty in &lhs.types {
                    if implements_traits(&lty.ty, trait_impls).is_err() { continue; }

                    for rty in &rhs.types {
                        if implements_traits(&rty.ty, trait_impls).is_err() { continue; }

                        // TODO: would it be ok to break from this loop after finding a match here?
                        if lty.trivially_convertible_to(rty) {
                            one_of.push(rty.clone());
                        } else if rty.trivially_convertible_to(lty) {
                            one_of.push(lty.clone());
                        }
                    }
                }
                Some(one_of.into())
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

        ConstraintList { trait_impls, one_of, preferred_type, is_error: false }
    }

    // Same as intersect_constraints, but: 
    //  - mutates `self` and `other` in-place instead of creating a new `ConstraintList`
    //  - evaluates mutability independently between the arguments, with precedence given to self
    //  - is literally just used for assignment expressions
    //  - is a terrible abstraction :(
    pub fn intersect_constraints_lopsided<'a>(&self, tp: &mut impl TypeProvider, constraint_haver: impl Into<MutConstraintHaver<'a>> + Clone, other_haver: impl Into<MutConstraintHaver<'a>> + Clone) -> Result<(), AssignmentError> {
        let (constraints, other) = match (constraint_haver.clone().into(), other_haver.clone().into()) {
            (MutConstraintHaver::Expr(expr1), MutConstraintHaver::Expr(expr2)) => self.get_multi_constraints_mut(tp, expr1, expr2),
            (MutConstraintHaver::Expr(expr), MutConstraintHaver::ConstraintListMut(constraints)) => (self.get_constraints_mut(tp, expr), constraints),
            (MutConstraintHaver::ConstraintListMut(constraints), MutConstraintHaver::Expr(expr)) => (constraints, self.get_constraints_mut(tp, expr)),
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
                    get_constraints_mut!(self, tp, constraint_haver.clone()).preferred_type = Some(preferred_type);
                } else {
                    let other = get_constraints_mut!(self, tp, other_haver.clone());
                    other.preferred_type = None;
                    if other.one_of.is_none() {
                        let constraints = get_constraints_mut!(self, tp, constraint_haver.clone());
                        if let Some(one_of) = constraints.one_of.clone() {
                            let mut rhs_one_of = OneOfConstraint::new();
                            for ty in &one_of.types {
                                let ty: QualType = ty.ty.clone().into();
                                if let Ok(success) = self.can_unify_to(tp, other_haver.clone().into(), &ty) {
                                    rhs_one_of.push_with_decl_maybe(ty, success.get_decl());
                                }
                            }

                            get_constraints_mut!(self, tp, other_haver.clone()).one_of = Some(rhs_one_of);
                        } else {
                            get_constraints_mut!(self, tp, other_haver.clone()).one_of = None;
                        }
                    }
                }
            }
            if let Some(preferred_type) = get_constraints_mut!(self, tp, constraint_haver.clone()).preferred_type.clone() {
                // I don't actually know if it's possible for an expression to not be able to unify to its preferred type?
                assert!(self.can_unify_to(tp, constraint_haver.clone().into(), &preferred_type).is_ok());
                let preferred_type = QualType::from(preferred_type.ty.clone());
                if self.can_unify_to(tp, other_haver.clone().into(), &preferred_type).is_ok() {
                    get_constraints_mut!(self, tp, other_haver.clone()).preferred_type = Some(preferred_type);
                } else {
                    get_constraints_mut!(self, tp, constraint_haver.clone()).preferred_type = None;
                }
            }
        }
        let constraints = get_constraints_mut!(self, tp, constraint_haver);
        if !constraints.one_of.as_ref().unwrap().is_empty() && !constraints.is_error() && !constraints.one_of_exists(|ty| ty.is_mut) {
            return Err(AssignmentError::Immutable);
        }
        Ok(())
    }

    pub fn solve_constraints<'a>(&self, tp: &'a impl TypeProvider, constraints: impl Into<ConstraintHaver<'a>>) -> Result<ConstraintSolution, SolveError<'a>> {
        let constraints = get_constraints!(self, tp, constraints);

        if let Some(one_of) = &constraints.one_of {
            if one_of.len() == 1 {
                let (ty, decl) = one_of.get(0);
                return Ok(ConstraintSolution { qual_ty: ty.clone(), decl })
            } else if one_of.is_empty() {
                return Err(SolveError::NoValidChoices)
            }
        }
        
        match constraints.preferred_type {
            Some(ref pref) => if let Ok(success) = self.can_unify_to(tp, constraints, pref) {
                Ok(ConstraintSolution { qual_ty: pref.clone(), decl: success.get_decl() })
            } else if let Some(one_of) = &constraints.one_of {
                Err(SolveError::Ambiguous { choices: &one_of.types })
            } else {
                Err(SolveError::CantUnifyToPreferredType)
            },
            None => Err(SolveError::NoValidChoices),
        }
    }

    pub fn set_constraints_to_c_variadic_compatible_type<'a>(&self, tp: &mut impl TypeProvider, constraints: impl Into<MutConstraintHaver<'a>> + Clone) {
        // If we're never, we should stay never.
        if get_constraints_mut!(self, tp, constraints.clone()).is_never() { return; }

        let solution = self.solve_constraints(tp, constraints.clone().into()).unwrap();
        get_constraints_mut!(self, tp, constraints).set_to(solution);
    }
}

#[derive(Debug)]
pub enum AssignmentError {
    Immutable,
}

impl ConstraintList {
    pub fn set_to(&mut self, solution: ConstraintSolution) {
        // If we're never, we should stay never.
        // If the passed in type is Error, we should stay whatever we are.
        // (this second clause is experimental and may not turn out to be a good idea. reevaluate later.)
        if !self.is_never() && !solution.qual_ty.ty.is_error() {
            if let Some(one_of) = &self.one_of {
                assert!(one_of.decls.is_empty() || solution.decl.is_some(), "attempted to remove decl id from one_of");
            }
            let mut one_of = OneOfConstraint::new();
            one_of.push_with_decl_maybe(solution.qual_ty, solution.decl);
            self.one_of = Some(one_of);
        }
    }
}

impl Driver {
    pub fn get_constraints<'a>(&self, tp: &'a impl TypeProvider, constraint_haver: impl Into<ConstraintHaver<'a>>) -> &'a ConstraintList {
        get_constraints!(self, tp, constraint_haver)
    }

    pub fn get_constraints_mut<'a>(&self, tp: &'a mut impl TypeProvider, constraint_haver: impl Into<MutConstraintHaver<'a>>) -> &'a mut ConstraintList {
        get_constraints_mut!(self, tp, constraint_haver)
    }

    pub fn get_multi_constraints_mut<'a>(&self, tp: &'a mut impl TypeProvider, a: ExprId, b: ExprId) -> (&'a mut ConstraintList, &'a mut ConstraintList) {
        assert_ne!(a, b, "`a` ({:?}) must not equal `b` ({:?})", a, b);

        let a = self.code.ast.expr_to_type_vars[a];
        let b = self.code.ast.expr_to_type_vars[b];

        // Ensure both exist, in the case of MockTypeProvider
        tp.constraints_mut(a);
        tp.constraints_mut(b);

        unsafe {
            let a = tp.constraints_mut(a) as *mut _;
            let b = tp.constraints_mut(b) as *mut _;
            (&mut *a, &mut *b)
        }
    }
}