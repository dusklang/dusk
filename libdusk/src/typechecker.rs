use std::collections::HashMap;

use smallvec::{SmallVec, smallvec};
use num_bigint::BigUint;

pub mod constraints;

use constraints::*;
use crate::type_provider::{TypeProvider, RealTypeProvider, MockTypeProvider};

use crate::dire::ast::{self, ExprId, DeclId, StructId, PatternKind, GenericParamId, Ident, VOID_EXPR, GenericCtx, DeclRefId, BLANK_GENERIC_CTX, Decl};
use crate::dire::mir::Const;
use crate::dire::ty::{Type, LegacyInternalType, FunctionType, QualType, IntWidth};
use crate::dire::source_info::SourceRange;
use crate::dire::InternalNamespace;

use crate::driver::{Driver, DriverRef};
use crate::error::Error;
use crate::new_code::NewCode;
use crate::ty::BuiltinTraits;
use crate::tir::{Units, UnitItems, ExprNamespace, self, NameLookup, NewNamespaceRefKind};

use dusk_proc_macros::*;

#[derive(Copy, Clone, Debug)]
pub enum CastMethod {
    Noop,
    Reinterpret,
    Int,
    Float,
    FloatToInt,
    IntToFloat,
    Invalid,
}

impl Default for CastMethod {
    fn default() -> Self {
        CastMethod::Noop
    }
}

#[derive(Clone, Debug)]
pub struct StructLit {
    pub strukt: StructId,
    pub fields: Vec<ExprId>,
}

#[derive(Debug, Clone, Default)]
pub struct Overloads {
    pub overloads: Vec<DeclId>,
    pub nonviable_overloads: Vec<DeclId>,
}

pub type GenericConstraints = HashMap<GenericParamId, ConstraintList>;

impl tir::Expr<tir::IntLit> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        *tp.constraints_mut(self.id) = ConstraintList::new(
            BuiltinTraits::INT, 
            None,
            Some(Type::i32().into()),
            ef!(driver, self.id.generic_ctx_id),
        );
    }

    fn run_pass_2(&self, _driver: &mut Driver, tp: &mut impl TypeProvider) {
        *tp.ty_mut(self.id) = tp.constraints(self.id).solve().expect("Ambiguous type for integer literal").ty;
    }
}

impl tir::Expr<tir::DecLit> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        *tp.constraints_mut(self.id) = ConstraintList::new(
            BuiltinTraits::DEC, 
            None,
            Some(Type::f64().into()),
            ef!(driver, self.id.generic_ctx_id),
        );
    }

    fn run_pass_2(&self, _driver: &mut Driver, tp: &mut impl TypeProvider) {
        *tp.ty_mut(self.id) = tp.constraints(self.id).solve().expect("Ambiguous type for decimal literal").ty;
    }
}

impl tir::Expr<tir::StrLit> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        *tp.constraints_mut(self.id) = ConstraintList::new(
            BuiltinTraits::STR, 
            None,
            Some(Type::u8().ptr().into()),
            ef!(driver, self.id.generic_ctx_id),
        );
    }

    fn run_pass_2(&self, _driver: &mut Driver, tp: &mut impl TypeProvider) {
        *tp.ty_mut(self.id) = tp.constraints(self.id).solve().expect("Ambiguous type for string literal").ty;
    }
}

impl tir::Expr<tir::CharLit> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        *tp.constraints_mut(self.id) = ConstraintList::new(
            BuiltinTraits::CHAR, 
            None,
            Some(Type::u8().ptr().into()),
            ef!(driver, self.id.generic_ctx_id),
        );
    }

    fn run_pass_2(&self, _driver: &mut Driver, tp: &mut impl TypeProvider) {
        *tp.ty_mut(self.id) = tp.constraints(self.id).solve().expect("Ambiguous type for character literal").ty;
    }
}
impl tir::Expr<tir::BoolLit> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        *tp.constraints_mut(self.id) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Bool.into()]), None, ef!(driver, self.id.generic_ctx_id));
        *tp.ty_mut(self.id) = Type::Bool;
    }

    fn run_pass_2(&self, _driver: &mut Driver, _tp: &mut impl TypeProvider) {
    }
}

impl tir::Expr<tir::Break> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        *tp.constraints_mut(self.id) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Never.into()]), None, ef!(driver, self.id.generic_ctx_id));
        *tp.ty_mut(self.id) = Type::Never;
    }

    fn run_pass_2(&self, _driver: &mut Driver, _tp: &mut impl TypeProvider) {
    }
}

impl tir::Expr<tir::Continue> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        *tp.constraints_mut(self.id) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Never.into()]), None, ef!(driver, self.id.generic_ctx_id));
        *tp.ty_mut(self.id) = Type::Never;
    }

    fn run_pass_2(&self, _driver: &mut Driver, _tp: &mut impl TypeProvider) {
    }
}

impl tir::Expr<tir::ConstExpr> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        let ty = self.0.clone();
        *tp.constraints_mut(self.id) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![ty.clone().into()]), None, ef!(driver, self.id.generic_ctx_id));
        *tp.ty_mut(self.id) = ty;
    }

    fn run_pass_2(&self, _driver: &mut Driver, _tp: &mut impl TypeProvider) {
    }
}

impl tir::Expr<tir::ErrorExpr> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        *tp.constraints_mut(self.id) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Error.into()]), None, ef!(driver, self.id.generic_ctx_id));
        *tp.ty_mut(self.id) = Type::Error;
    }

    fn run_pass_2(&self, _driver: &mut Driver, _tp: &mut impl TypeProvider) {
    }
}

impl tir::GenericParam {
    fn run_pass_1(&self, _driver: &mut Driver, tp: &mut impl TypeProvider) {
        *tp.decl_type_mut(self.id) = Type::Ty.into();
    }

    fn run_pass_2(&self, _driver: &mut Driver, _tp: &mut impl TypeProvider) {
    }
}

impl tir::AssignedDecl {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        let ty = if let &Some(explicit_ty) = &self.explicit_ty {
            let explicit_ty = tp.get_evaluated_type(explicit_ty).clone();
            if let Some(err) = driver.can_unify_to(tp, self.root_expr, &explicit_ty.clone().into()).err() {
                let range = driver.get_range(self.root_expr);
                let mut error = Error::new(format!("Couldn't unify expression to assigned decl type `{:?}`", explicit_ty))
                    .adding_primary_range(range, "expression here");
                match err {
                    UnificationError::InvalidChoice(choices)
                        => error.add_secondary_range(range, format!("note: expression could've unified to any of {:?}", choices)),
                    UnificationError::Trait(not_implemented)
                        => error.add_secondary_range(
                            range,
                            format!(
                                "note: couldn't unify because expression requires implementations of {:?}",
                                not_implemented.names(),
                            ),
                        ),
                }
                driver.diag.push(error);
            }
            explicit_ty
        } else if let Ok(ty) = tp.constraints(self.root_expr).solve() {
            ty.ty
        } else if tp.constraints(self.root_expr).is_error() {
            // We should've already reported this error, so don't add to the noise.
            Type::Error
        } else {
            // TODO: more detail!
            let range = df!(driver, self.decl_id.range);
            driver.diag.push(
                Error::new("ambiguous type for assigned declaration")
                    .adding_primary_range(range, "declaration here")
            );
            Type::Error
        };
        tp.decl_type_mut(self.decl_id).ty = ty;
    }

    fn run_pass_2(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        let decl_id = self.decl_id;
        let root_expr = self.root_expr;
        let ty = tp.fetch_decl_type(driver, decl_id, None).ty;
        tp.constraints_mut(root_expr).set_to(ty);
    }
}

impl tir::PatternBinding {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        let constraints = tp.constraints(self.scrutinee);
        let scrutinee_ty = constraints.solve().expect("Ambiguous type for assigned declaration").ty;
        let mut binding_ty = None;
        let binding = &driver.code.ast.pattern_binding_decls[self.binding_id];
        // Note: no need to worry about mutability matching because that can be handled in the parser
        for path in &binding.paths {
            let mut path_ty = scrutinee_ty.clone();
            for step in &path.components {
                match step {
                    &ast::PatternBindingPathComponent::VariantPayload(index) => {
                        match path_ty {
                            Type::Enum(enuum) => {
                                let payload_ty = driver.code.ast.enums[enuum].variants[index].payload_ty.unwrap();
                                let payload_ty = tp.get_evaluated_type(payload_ty);
                                path_ty = payload_ty.clone();
                            },
                            _ => panic!("expected enum"),
                        }
                    }
                }
            }
            if let Some(binding_ty) = &binding_ty {
                assert_eq!(binding_ty, &path_ty, "pattern binding contains paths of different types");
            } else {
                binding_ty = Some(path_ty);
            }
        }
        tp.decl_type_mut(self.decl_id).ty = binding_ty.unwrap();
    }

    fn run_pass_2(&self, _driver: &mut Driver, _tp: &mut impl TypeProvider) {
    }
}

impl tir::Expr<tir::Assignment> {
    fn run_pass_1(&self, _driver: &mut Driver, tp: &mut impl TypeProvider) {
        tp.constraints_mut(self.id).set_to(Type::Void);
        *tp.ty_mut(self.id) = Type::Void;
    }

    fn run_pass_2(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        let (lhs, rhs) = tp.multi_constraints_mut(self.lhs, self.rhs);
        if let Err(err) = lhs.lopsided_intersect_with(rhs) {
            match err {
                AssignmentError::Immutable => {
                    let err = Error::new("unable to assign to immutable expression")
                        .adding_primary_range(self.lhs, "expression here");
                    driver.diag.push(err);
                }
            }
        }
    }
}

impl tir::Expr<tir::Cast> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        let ty = tp.get_evaluated_type(self.ty).clone();
        *tp.constraints_mut(self.id) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![ty.clone().into()]), None, ef!(driver, self.id.generic_ctx_id));
        *tp.ty_mut(self.id) = ty;
    }

    fn run_pass_2(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        let ty = tp.get_evaluated_type(self.ty).clone();
        // TODO: pass `self.ty` directly to can_unify_to() once its support for generics is more robust
        let ty_and_method: Result<(Type, CastMethod), Vec<QualType>> = if driver.can_unify_to(tp, self.expr, &QualType::from(&ty)).is_ok() {
            Ok((ty, CastMethod::Noop))
        } else if let Type::Pointer(dest_pointee_ty) = ty {
            let constraints = tp.constraints_mut(self.expr);
            let dest_pointee_ty = dest_pointee_ty.as_ref();
            constraints.max_ranked_type(|ty| {
                match ty.ty {
                    Type::Pointer(ref pointee)
                        if
                            // pure mut cast (either giving or taking away)
                            pointee.ty.trivially_convertible_to(&dest_pointee_ty.ty) ||
                            // src must be at least as mutable as dest, if not more
                            pointee.is_mut == dest_pointee_ty.is_mut || pointee.is_mut
                        // prefer to cast pointer -> pointer over int -> pointer, if possible
                        => 2,
                    Type::Int { width, .. } if width == IntWidth::Pointer => 1,
                    _ => 0,
                }
            })
                .map(|src_ty| (src_ty.ty.clone(), CastMethod::Reinterpret))
                .map_err(|_| Vec::new())
        } else if let Type::Int { width, .. } = ty {
            let constraints = tp.constraints_mut(self.expr);
            constraints.max_ranked_type_with_assoc_data(|ty|
                match ty.ty {
                    Type::Int { .. } => (3, CastMethod::Int),
                    Type::Float { .. } => (2, CastMethod::FloatToInt),
                    Type::Pointer(_) if width == IntWidth::Pointer => (1, CastMethod::Reinterpret),
                    _ => (0, CastMethod::Noop),
                }
            )
                .map(|(ty, method)| (ty.ty.clone(), method))
                .map_err(|options| options.iter().map(|(ty, _)| ty.to_owned().to_owned()).collect())
        } else if let Type::Float { .. } = ty {
            let constraints = tp.constraints_mut(self.expr);
            constraints.max_ranked_type_with_assoc_data(|ty|
                match ty.ty {
                    Type::Float { .. } => (2, CastMethod::Float),
                    Type::Int { .. } => (1, CastMethod::IntToFloat),
                    _ => (0, CastMethod::Noop),
                }
            )
                .map(|(ty, method)| (ty.ty.clone(), method))
                .map_err(|options| options.iter().map(|(ty, _)| ty.to_owned().to_owned()).collect())
        } else {
            Err(Vec::new())
        };
        let constraints = tp.constraints_mut(self.expr);
        match ty_and_method {
            Ok((ty, method)) => {
                constraints.set_to(ty);
                *tp.cast_method_mut(self.cast_id) = method;
            },
            Err(_) => {
                driver.diag.push(Error::new("Invalid cast").adding_primary_range(driver.get_range(self.id), "cast here"));
                constraints.set_to(Type::Error);
                *tp.cast_method_mut(self.cast_id) = CastMethod::Invalid;
            }
        }
    }
}

impl tir::Expr<tir::While> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        *tp.constraints_mut(self.id) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Void.into()]), None, ef!(driver, self.id.generic_ctx_id));
        *tp.ty_mut(self.id) = Type::Void;
    }

    fn run_pass_2(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        if driver.can_unify_to(tp, self.condition, &Type::Bool.into()).is_ok() {
            tp.constraints_mut(self.condition).set_to(Type::Bool);
        } else {
            panic!("Expected boolean condition in while expression");
        }
    }
}

impl tir::Expr<tir::For> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        *tp.constraints_mut(self.id) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Void.into()]), None, ef!(driver, self.id.generic_ctx_id));
        *tp.ty_mut(self.id) = Type::Void;

        let range_range = driver.get_range(self.lower_bound) + driver.get_range(self.upper_bound);

        let loop_binding_ty = if let Some(explicit_ty_expr) = self.binding_explicit_ty {
            let explicit_ty = tp.get_evaluated_type(explicit_ty_expr);
            // TODO: less code duplication (both here and in the many other places this code has been copied and pasted to)
            if let Some(err) = driver.can_unify_to(tp, self.lower_bound, &explicit_ty.clone().into()).err() {
                let range = driver.get_range(self.lower_bound);
                let mut error = Error::new(format!("Couldn't unify lower bound to loop variable type `{:?}`", explicit_ty))
                    .adding_primary_range(range, "expression here");
                match err {
                    UnificationError::InvalidChoice(choices)
                        => error.add_secondary_range(range, format!("note: expression could've unified to any of {:?}", choices)),
                    UnificationError::Trait(not_implemented)
                        => error.add_secondary_range(
                            range,
                            format!(
                                "note: couldn't unify because expression requires implementations of {:?}",
                                not_implemented.names(),
                            ),
                        ),
                }
                driver.diag.push(error);
            }
            if let Some(err) = driver.can_unify_to(tp, self.upper_bound, &explicit_ty.clone().into()).err() {
                let range = driver.get_range(self.upper_bound);
                let mut error = Error::new(format!("Couldn't unify upper bound to loop variable type `{:?}`", explicit_ty))
                    .adding_primary_range(range, "expression here");
                match err {
                    UnificationError::InvalidChoice(choices)
                        => error.add_secondary_range(range, format!("note: expression could've unified to any of {:?}", choices)),
                    UnificationError::Trait(not_implemented)
                        => error.add_secondary_range(
                            range,
                            format!(
                                "note: couldn't unify because expression requires implementations of {:?}",
                                not_implemented.names(),
                            ),
                        ),
                }
                driver.diag.push(error);
            }

            let ty = explicit_ty.to_owned();

            if !ty.is_int() && !ty.is_error() {
                driver.diag.push(
                    Error::new(format!("unsupported loop variable type `{:?}`", ty))
                        .adding_primary_range(driver.get_range(explicit_ty_expr), "range bounds must be integers")
                );
            }

            ty
        } else {
            let lower_bound_constraints = tp.constraints(self.lower_bound);
            let upper_bound_constraints = tp.constraints(self.upper_bound);
            let loop_binding_constraints = lower_bound_constraints.intersect_with(upper_bound_constraints);
            
            let ty = match loop_binding_constraints.solve() {
                Ok(ty) => ty.ty,
                Err(_) => {
                    // TODO: better error message, probably
                    driver.diag.push(
                        Error::new("unable to reconcile the types of the lower and upper bounds in range")
                            .adding_primary_range(range_range, "")
                    );
                    Type::Error
                }
            };

            if !ty.is_int() && !ty.is_error() {
                driver.diag.push(
                    Error::new(format!("values of unsupported type `{:?}` used in range", ty))
                        .adding_primary_range(range_range, "range bounds must be integers")
                );
            }

            ty
        };

        let is_mut = driver.tir.decls[self.binding_decl].is_mut;
        *tp.decl_type_mut(self.binding_decl) = QualType { ty: loop_binding_ty, is_mut };
    }

    fn run_pass_2(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        let ty = tp.fetch_decl_type(driver, self.binding_decl, None).ty;
        tp.constraints_mut(self.lower_bound).set_to(ty.clone());
        tp.constraints_mut(self.upper_bound).set_to(ty);
    }
}


enum ExhaustionReason {
    // Explicitly called out, e.g., '.a' for variant 'a'
    Explicit {
        // First pattern to explicitly call out the value/variant
        first_coverage: SourceRange,
        // Must be set to true if either another explicit pattern calls out the value/variant, or
        // there is a catch-all case later
        more_than_one_coverage: bool,
    },
    CatchAll(SourceRange),
}

struct VariantExhaustion {
    reason: ExhaustionReason,
    payload: Option<Box<Exhaustion>>,
}

#[derive(Default)]
struct EnumExhaustion {
    variants: HashMap<usize, VariantExhaustion>,
}

enum Exhaustion {
    #[allow(unused)]
    Enum(EnumExhaustion),
    Total,
}

impl EnumExhaustion {
    fn is_total(&self, driver: &Driver, scrutinee_ty: &Type, tp: &impl TypeProvider) -> bool {
        let enum_id = match scrutinee_ty {
            &Type::Enum(enum_id) => enum_id,
            _ => panic!("expected enum"),
        };

        let variants = &driver.code.ast.enums[enum_id].variants;
        if self.variants.len() < variants.len() {
            false
        } else {
            // Return false early if you find something that proves non-total exhaustion.
            // Otherwise, return true.
            for (i, variant) in variants.iter().enumerate() {
                let exhaustion = self.variants.get(&i).unwrap();
                // No payload == total exhaustion. Otherwise:
                if let Some(payload) = &exhaustion.payload {
                    let ty = variant.payload_ty.unwrap();
                    let ty = tp.get_evaluated_type(ty);
                    if !payload.is_total(driver, ty, tp) {
                        return false;
                    }
                }
            }
            true
        }
    }

    fn make_total(&mut self, driver: &Driver, scrutinee_ty: &Type, catch_all_range: SourceRange, tp: &impl TypeProvider) {
        let enum_id = match scrutinee_ty {
            &Type::Enum(enum_id) => enum_id,
            _ => panic!("expected enum"),
        };
        let variants = &driver.code.ast.enums[enum_id].variants;
        for (i, variant) in variants.iter().enumerate() {
            if let Some(exhaustion) = self.variants.get_mut(&i) {
                if let ExhaustionReason::Explicit { more_than_one_coverage, .. } = &mut exhaustion.reason {
                    *more_than_one_coverage = true;
                }
                if let Some(payload) = &mut exhaustion.payload {
                    let ty = variant.payload_ty.unwrap();
                    let ty = tp.get_evaluated_type(ty);
                    payload.make_total(driver, ty, catch_all_range, tp);
                }
            } else {
                let payload = variant.payload_ty.is_some()
                    .then(|| Box::new(Exhaustion::Total));
                self.variants.insert(i, VariantExhaustion { reason: ExhaustionReason::CatchAll(catch_all_range), payload });
            }
        }
    }
}

impl Exhaustion {
    fn is_total(&self, driver: &Driver, scrutinee_ty: &Type, tp: &impl TypeProvider) -> bool {
        match self {
            Exhaustion::Enum(exhaustion) => exhaustion.is_total(driver, scrutinee_ty, tp),
            Exhaustion::Total => true,
        }
    }

    fn make_total(&mut self, driver: &Driver, scrutinee_ty: &Type, catch_all_range: SourceRange, tp: &impl TypeProvider) {
        match self {
            Exhaustion::Enum(exhaustion) => exhaustion.make_total(driver, scrutinee_ty, catch_all_range, tp),
            Exhaustion::Total => {},
        }
    }
}

impl tir::Expr<tir::Switch> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        let scrutinee_ty = tp.constraints(self.scrutinee).solve().expect("Ambiguous type for scrutinee in switch expression").ty;
        match scrutinee_ty {
            Type::Enum(id) => {
                // Make sure each switch case matches a variant name in the scrutinized enum, and
                // that each variant in the enum is matched exactly once.
                let mut exhaustion = EnumExhaustion::default();
                let variants = &driver.code.ast.enums[id].variants;
                for case in &self.cases {
                    match case.pattern.kind {
                        PatternKind::ContextualMember { name, range } => {
                            let variant_name_str = driver.interner.resolve(name.symbol).unwrap();
                            let index = variants.iter().position(|variant| variant.name == name.symbol);
                            if let Some(index) = index {
                                if variants[index].payload_ty.is_some() {
                                    // Trying to match variant with payload, without acknowledging
                                    // the payload. For example, matching a value of type
                                    // 'enum { a(u32) }' with the pattern '.a' rather than '.a(12)'
                                    // or something.
                                    let decl = variants[index].decl;
                                    let err = Error::new(format!("Variant `{}` has a payload which goes ignored in pattern", variant_name_str))
                                        .adding_primary_range(range, "pattern here")
                                        .adding_secondary_range(decl, "variant here");
                                    driver.diag.push(err);

                                    // Even though the payload was ignored, still add record of attempt to match it. This will suppress unhandled variant errors.
                                    exhaustion.variants.entry(index).or_insert_with(|| {
                                        let payload = Box::new(Exhaustion::Total);
                                        VariantExhaustion { reason: ExhaustionReason::Explicit { first_coverage: range, more_than_one_coverage: false }, payload: Some(payload) }
                                    });
                                } else if let Some(prior_match) = exhaustion.variants.get_mut(&index) {
                                    // This variant has no payload, and has already been matched.
                                    let mut err = Error::new(format!("Variant `{}` already covered in `switch` expression", variant_name_str))
                                        .adding_primary_range(range, "redundant case here");
                                    match prior_match.reason {
                                        ExhaustionReason::Explicit { first_coverage, ref mut more_than_one_coverage } => {
                                            let msg = if *more_than_one_coverage {
                                                "first covered here"
                                            } else {
                                                "covered here"
                                            };
                                            err.add_primary_range(first_coverage, msg);
                                            *more_than_one_coverage = true;
                                        },
                                        ExhaustionReason::CatchAll(range) => err.add_primary_range(range, "covered by catch-all pattern here"),
                                    }
                                    driver.diag.push(err);
                                } else {
                                    exhaustion.variants.insert(index, VariantExhaustion { reason: ExhaustionReason::Explicit { first_coverage: range, more_than_one_coverage: false }, payload: None });
                                }
                            } else {
                                let err = Error::new(format!("Variant `{}` does not exist in enum {:?}", variant_name_str, scrutinee_ty))
                                    .adding_primary_range(range, "referred to by pattern here");
                                driver.diag.push(err);
                            }
                        },
                        PatternKind::NamedCatchAll(Ident { range, .. }) | PatternKind::AnonymousCatchAll(range) => {
                            if exhaustion.is_total(driver, &scrutinee_ty, tp) {
                                let err = Error::new("switch case unreachable")
                                    .adding_primary_range(range, "all possible values already handled before this point");
                                driver.diag.push(err);
                            } else {
                                exhaustion.make_total(driver, &scrutinee_ty, range, tp);
                            }
                        },
                        PatternKind::IntLit { range, .. } => {
                            let error = Error::new("cannot match enum type with integer pattern")
                                .adding_primary_range(range, "pattern here")
                                .adding_secondary_range(self.scrutinee, "scrutinee here");
                            driver.diag.push(error);
                        },
                    }
                }
                // If there are more matches than variants, then the sky is falling.
                debug_assert!(exhaustion.variants.len() <= variants.len());

                // There are more variants than there are matched variants? Then there are
                // unmatched variants. AKA, the switch expression is non-exhaustive.
                if exhaustion.variants.len() < variants.len() {
                    let mut unmatched_variants = Vec::new();
                    for (index, variant) in variants.iter().enumerate() {
                        if exhaustion.variants.get(&index).is_none() {
                            unmatched_variants.push(variant.name);
                        }
                    }

                    // TODO: using the write!() macro here, or something, would be nicer.
                    let mut err_msg = String::new();
                    if unmatched_variants.len() == 1 {
                        err_msg.push_str(&format!("Variant `{}` is ", driver.interner.resolve(unmatched_variants[0]).unwrap()));
                    } else {
                        err_msg.push_str("Variants ");
                        for (i, &variant) in unmatched_variants[0..(unmatched_variants.len() - 1)].iter().enumerate() {
                            if i != 0 {
                                err_msg.push_str(", ");
                            }
                            err_msg.push_str(&format!("`{}`", driver.interner.resolve(variant).unwrap()));
                        }
                        err_msg.push_str(&format!(" and `{}` are ", driver.interner.resolve(unmatched_variants.last().copied().unwrap()).unwrap()));
                    }
                    err_msg.push_str("unhandled in switch expression. switch expressions must be exhaustive.");

                    let err = Error::new(err_msg)
                        .adding_primary_range(self.id, "");
                    driver.diag.push(err);
                }
            },
            Type::Int { width, is_signed: false } => {
                let mut exhaustion = HashMap::new();
                let mut catch_all_range = None;
                let mut all_exhausted = false;
                let mut num_values = BigUint::from(1u64);
                num_values <<= width.bit_width(driver.arch);
                
                for case in &self.cases {
                    match case.pattern.kind {
                        PatternKind::IntLit { value, range } => {
                            let already_handled_message = || format!("value `{}` already handled in switch expression", value);
                            if BigUint::from(value) >= num_values {
                                let err = Error::new(format!("value `{}` does not fit into type {:?}", value, scrutinee_ty))
                                    .adding_primary_range(range, "in pattern here");
                                driver.diag.push(err);
                            }
                            if let Some(prior_use) = exhaustion.get(&value) {
                                // TODO: print as hex if necessary to match the user
                                let err = Error::new(already_handled_message())
                                    .adding_primary_range(range, "redundant case here")
                                    .adding_secondary_range(*prior_use, "previously handled here");
                                driver.diag.push(err);
                            } else if let Some(catch_all_range) = catch_all_range {
                                let err = Error::new(already_handled_message())
                                    .adding_primary_range(range, "redundant case here")
                                    .adding_secondary_range(catch_all_range, "previously handled by catch-all case here");
                                driver.diag.push(err);
                            }
                            exhaustion.insert(value, range);
                        },
                        PatternKind::NamedCatchAll(Ident { range, .. }) | PatternKind::AnonymousCatchAll(range) => {
                            if all_exhausted {
                                let mut err = Error::new("switch case unreachable")
                                    .adding_primary_range(range, "all possible values already handled before this point");
                                if let Some(catch_all_range) = catch_all_range {
                                    err.add_secondary_range(catch_all_range, "additional values handled by pattern here");
                                }
                                driver.diag.push(err);
                            } else {
                                catch_all_range = Some(range);
                            }
                        },
                        PatternKind::ContextualMember { range, .. } => {
                            let error = Error::new(format!("cannot match integer type {:?} with contextual member pattern", scrutinee_ty))
                                .adding_primary_range(range, "pattern here")
                                .adding_secondary_range(self.scrutinee, "scrutinee here");
                            driver.diag.push(error);
                        },
                    }

                    if !all_exhausted && (catch_all_range.is_some() || BigUint::from(exhaustion.len()) >= num_values) {
                        all_exhausted = true;
                    }
                }
                if !all_exhausted {
                    let err = Error::new(format!("not all values of {:?} handled in switch expression. switch expressions must be exhaustive", scrutinee_ty))
                        .adding_primary_range(self.id, "consider adding a catch-all case to the end of this switch");
                    driver.diag.push(err);
                }
            }
            _ => todo!("Type {:?} is not supported in switch expression scrutinee position", scrutinee_ty),
        }

        let constraints = if self.cases.is_empty() {
            ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Void.into()]), None, ef!(driver, self.id.generic_ctx_id))
        } else {
            let mut constraints = tp.constraints(self.cases[0].terminal_expr).clone();

            for case in &self.cases[1..self.cases.len()] {
                constraints = constraints.intersect_with(tp.constraints(case.terminal_expr));
            }
            constraints
        };
        if constraints.solve().is_err() {
            // TODO: better error message
            driver.diag.push(
                Error::new("Failed to unify cases of switch expression")
                    .adding_primary_range(driver.get_range(self.id), "")
            );
        }
        *tp.constraints_mut(self.id) = constraints;
    }

    fn run_pass_2(&self, _driver: &mut Driver, tp: &mut impl TypeProvider) {
        let scrutinee_ty = tp.constraints(self.scrutinee).solve().map(|ty| ty.ty).unwrap_or(Type::Error);
        tp.constraints_mut(self.scrutinee).set_to(scrutinee_ty);
        let ty = tp.constraints(self.id).solve().expect("ambiguous type for switch expression");
        *tp.ty_mut(self.id) = ty.ty.clone();
        for case in &self.cases {
            tp.constraints_mut(case.terminal_expr).set_to(ty.clone());
        }
    }
}

impl tir::Expr<tir::ExplicitRet> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        *tp.constraints_mut(self.id) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Never.into()]), None, ef!(driver, self.id.generic_ctx_id));
        *tp.ty_mut(self.id) = Type::Never;
    }

    fn run_pass_2(&self, _driver: &mut Driver, _tp: &mut impl TypeProvider) {
    }
}

impl tir::Expr<tir::Module> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        *tp.constraints_mut(self.id) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Mod.into()]), None, ef!(driver, self.id.generic_ctx_id));
        *tp.ty_mut(self.id) = Type::Mod;

        if let Some(extern_library_path) = self.extern_library_path {
            if
                !tp.constraints(extern_library_path).is_error() &&
                !string_types().iter().any(|ty| driver.can_unify_to(tp, extern_library_path, &ty.into()).is_ok())
            {
                driver.diag.push(
                    Error::new("Invalid expression passed to extern_mod; expected string")
                        .adding_primary_range(extern_library_path, "")
                )
            }
        }
    }

    fn run_pass_2(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        if let Some(extern_library_path) = self.extern_library_path {
            let selected_type = string_types().into_iter()
                .find(|ty| driver.can_unify_to(tp, extern_library_path, &ty.into()).is_ok());
            tp.constraints_mut(extern_library_path).set_to(selected_type.unwrap_or_else(|| Type::Error));
        }
    }
}

impl tir::Expr<tir::Enum> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        *tp.constraints_mut(self.id) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Ty.into()]), None, ef!(driver, self.id.generic_ctx_id));
        *tp.ty_mut(self.id) = Type::Ty;
        for &payload_ty in &self.variant_payload_tys {
            if let Some(err) = driver.can_unify_to(tp, payload_ty, &Type::Ty.into()).err() {
                let mut error = Error::new("Expected variant payload type");
                let range = driver.get_range(payload_ty);
                match err {
                    UnificationError::InvalidChoice(choices)
                        => error.add_secondary_range(range, format!("note: expression could've unified to any of {:?}", choices)),
                    UnificationError::Trait(not_implemented)
                        => error.add_secondary_range(
                            range,
                            format!(
                                "note: couldn't unify because expression requires implementations of {:?}",
                                not_implemented.names(),
                            ),
                        ),
                }
                driver.diag.push(error);
            }
        }
        tp.constraints_mut(self.id).set_to(Type::Ty);
    }

    fn run_pass_2(&self, _driver: &mut Driver, tp: &mut impl TypeProvider) {
        for &variant_ty in &self.variant_payload_tys {
            let field_type = tp.constraints(variant_ty).solve().map(|ty| ty.ty).unwrap_or(Type::Error);
            // Don't bother checking if it's a type, because we already did that in pass 1
            tp.constraints_mut(variant_ty).set_to(field_type);
        }
        let ty = tp.constraints(self.id).solve().expect("Ambiguous type for enum expression");
        debug_assert_eq!(ty.ty, Type::Ty);
    }
}

fn string_types() -> [Type; 3] {
    [
        Type::i8().ptr().into(),
        Type::u8().ptr().into(),
        Type::LegacyInternal(LegacyInternalType::StringLiteral),
    ]
}

impl tir::Expr<tir::DeclRef> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        // Initialize overloads
        let decl_ref = &driver.code.ast.decl_refs[self.decl_ref_id];
        let overload_decls = match driver.find_overloads(decl_ref.namespace, &NameLookup::Exact(decl_ref.name)) {
            Some(overloads) => overloads,
            None => {
                *tp.decl_ref_has_error_mut(self.decl_ref_id) = true;
                Default::default()
            },
        };
        let mut overloads = Vec::new();
        for &overload_decl in &overload_decls {
            overloads.push(overload_decl);
        }

        // Find type possibilities
        let mut one_of = SmallVec::new();
        one_of.reserve(overloads.len());
        for &overload in &overloads {
            let ty = tp.fetch_decl_type(driver, overload, Some(self.decl_ref_id)).ty;
            let decl = &driver.tir.decls[overload];
            let mut is_mut = decl.is_mut;
            if let ast::Namespace::MemberRef { base_expr } = driver.code.ast.decl_refs[self.decl_ref_id].namespace {
                // TODO: Robustness! Base_expr could be an overload set with these types, but also include struct types
                if driver.can_unify_to(tp, base_expr, &Type::Ty.into()).is_err() && driver.can_unify_to(tp, base_expr, &Type::Mod.into()).is_err() {
                    let base_ty = tp.constraints(base_expr).solve().unwrap();
                    // Handle member refs with pointers to structs
                    is_mut &= if let Type::Pointer(pointee) = &base_ty.ty {
                        pointee.is_mut
                    } else {
                        base_ty.is_mut
                    };
                }
            }
            let mut constraints = GenericContext::new();
            for &param in &decl.generic_params {
                let mut constraint_list = ConstraintList::default();
                constraint_list.set_generic_ctx(df!(driver, overload.generic_ctx_id));
                constraints.insert(param, constraint_list);
            }
            one_of.push(QualType { ty, is_mut });
        }

        *tp.constraints_mut(self.id) = ConstraintList::new(BuiltinTraits::empty(), Some(one_of), None, ef!(driver, self.id.generic_ctx_id));
        *tp.overloads_mut(self.decl_ref_id) = Overloads { overloads, nonviable_overloads: Default::default() };
    }

    fn run_pass_2(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        let ty = tp.constraints(self.id).solve().unwrap_or(Type::Error.into());
        *tp.ty_mut(self.id) = ty.ty.clone();

        let mut overloads = tp.overloads(self.decl_ref_id).clone();
        let one_of = tp.constraints(self.id).one_of().to_owned();
        overloads.overloads.retain(|&overload| {
            let overload_ty = tp.fetch_decl_type(driver, overload, Some(self.decl_ref_id));
            if one_of.iter().any(|ty| overload_ty.trivially_convertible_to(ty)) {
                true
            } else {
                overloads.nonviable_overloads.push(overload);
                false
            }
        });

        overloads.overloads.retain(|&overload| {
            let overload_ty = tp.fetch_decl_type(driver, overload, Some(self.decl_ref_id));

            if overload_ty.trivially_convertible_to(&ty) {
                true
            } else {
                overloads.nonviable_overloads.push(overload);
                false
            }
        });

        let pref = tp.constraints(self.id)
            .preferred_type()
            .iter()
            .flat_map(|&ty| {
                overloads.overloads.iter().find(|&&overload| {
                    tp.decl_type(overload).trivially_convertible_to(ty)
                })
            }).next()
            .cloned();

        let (overload, generic_arguments) = if !overloads.overloads.is_empty() {
            // Select an overload. If `pref` is in the list of overloads, choose it. Otherwise choose the first in the list.
            let overload = pref.as_ref().cloned()
                .filter(|overload| overloads.overloads.contains(overload))
                .unwrap_or_else(|| overloads.overloads[0]);
            let decl = &driver.tir.decls[overload];
            // TODO: probably rename this from ret_ty.
            let ret_ty = tp.fetch_decl_type(driver, overload, Some(self.decl_ref_id));
            let mut ret_ty_constraints = ConstraintList::default();
            ret_ty_constraints.set_generic_ctx(df!(driver, overload.generic_ctx_id));
            ret_ty_constraints.set_to(ret_ty.clone());
            let mut generic_args = Vec::new();

            let generic_constraints = tp.generic_constraints_mut(self.decl_ref_id);
            for &generic_param in &decl.generic_params {
                let implied_constraints = ret_ty_constraints.get_implied_generic_constraints(generic_param, &ret_ty.ty);
                let generic_param_constraints = generic_constraints.get_mut(&generic_param).unwrap();
                *generic_param_constraints = generic_param_constraints.intersect_with(&implied_constraints);
                let generic_arg = generic_param_constraints.solve().unwrap().ty;
                generic_args.push(generic_arg);
            }
            for expr in tp.generic_substitution_list(self.decl_ref_id).clone() {
                tp.constraints_mut(expr).substitute_generic_args(&decl.generic_params, &generic_args);
            }
            (Some(overload), Some(generic_args))
        } else if !*tp.decl_ref_has_error(self.decl_ref_id) {
            let name = driver.code.ast.decl_refs[self.decl_ref_id].name;
            let name = driver.interner.resolve(name).unwrap();
            if overloads.nonviable_overloads.is_empty() {
                driver.diag.push(
                    Error::new(format!("no declarations named \"{}\" found in scope", name))
                        .adding_primary_range(driver.get_range(self.id), "referenced here")
                );
            } else {
                let mut err = Error::new(format!("no matching declarations named \"{}\" found in scope", name))
                    .adding_primary_range(driver.get_range(self.id), "referenced here");
                for &overload in &overloads.nonviable_overloads {
                    let range = df!(driver, overload.range);
                    err.add_secondary_range(range, "non-viable overload found here");
                }
                driver.diag.push(err);
            }
            (None, None)
        } else {
            (None, None)
        };

        *tp.overloads_mut(self.decl_ref_id) = overloads;
        *tp.selected_overload_mut(self.decl_ref_id) = overload;
        *tp.generic_arguments_mut(self.decl_ref_id) = generic_arguments;
    }
}

impl tir::Expr<tir::Call> {
    fn decl_ref_id(&self, driver: &Driver) -> DeclRefId {
        let generic_ctx_id = ef!(driver, self.callee.generic_ctx_id);
        let generic_ctx = &driver.code.ast.generic_ctxs[generic_ctx_id];
        if let GenericCtx::DeclRef { id, .. } = *generic_ctx {
            id
        } else {
            panic!("unexpected generic context '{:?}' for callee", generic_ctx);
        }
    }
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        let decl_ref_id = self.decl_ref_id(driver);

        // TODO: maybe mem::take() here instead of cloning? As the overloads in the type provider will just be overwritten later anyway.
        let mut overloads = tp.overloads(decl_ref_id).clone();
        // Rule out overloads that don't match the arguments
        let mut i = 0usize;
        let mut one_of: SmallVec<[QualType; 1]> = SmallVec::new();
        let decls = &driver.tir.decls;
        let callee_one_of = tp.constraints(self.callee).one_of();
        overloads.overloads.retain(|&overload| {
            if decls[overload].param_list.param_tys.len() != self.args.len() {
                overloads.nonviable_overloads.push(overload);
                i += 1;
                return false;
            }

            if let Decl::MethodIntrinsic(intr) = df!(driver, overload.ast) {
                let self_ty = driver.code.ast.intrinsics[intr].param_tys[0];
                let self_ty = tp.get_evaluated_type(self_ty);
                let ast::Namespace::MemberRef { base_expr } = driver.code.ast.decl_refs[decl_ref_id].namespace else {
                    panic!("expected MemberRef as base of method intrinsic call");
                };
                if driver.can_unify_to(tp, base_expr, &QualType::from(self_ty.clone())).is_err() {
                    if let Some(pointee_ty) = self_ty.deref() {
                        if driver.can_unify_to(tp, base_expr, pointee_ty).is_err() {
                            overloads.nonviable_overloads.push(overload);
                            i += 1;
                            return false;
                        }
                    }
                }
            }

            for (arg, ty) in self.args.iter().copied().zip(decls[overload].param_list.param_tys.iter().copied()) {
                if driver.can_unify_to(tp, arg, ty).is_err() {
                    overloads.nonviable_overloads.push(overload);
                    i += 1;
                    return false;
                }
            }
            one_of.push(callee_one_of[i].ty.return_ty().unwrap().into());
            i += 1;
            true
        });

        // Find preferred type
        // TODO: this logic seems so broken it should never have worked at all?!?!
        // I apparently just choose the first (argument, overload) pair such that
        //   - argument has a preferred type, and
        //   - that preferred type is trivially convertible to overload's parameter type at that position
        let mut pref = None;
        'find_preference: for (i, &arg) in self.args.iter().enumerate() {
            if let Some(ty) = tp.constraints(arg).preferred_type() {
                for &overload in &overloads.overloads {
                    let decl = &driver.tir.decls[overload];
                    if ty.ty.trivially_convertible_to(tp.get_evaluated_type(decl.param_list.param_tys[i])) {
                        let ty = tp.fetch_decl_type(driver, overload, None);
                        pref = Some(ty.ty.return_ty().unwrap().into());
                        tp.constraints_mut(self.callee).set_preferred_type(ty);
                        break 'find_preference;
                    }
                }
            }
        }

        // For each overload, infer generic constraints from the arguments
        // TODO: do this for self arguments as well
        for &overload in &overloads.overloads {
            let decl = &decls[overload];
            for (&param_ty, &arg) in decl.param_list.param_tys.iter().zip(&self.args) {
                let param_ty = tp.get_evaluated_type(param_ty).clone();
                let arg_constraints = tp.constraints(arg).clone();
                let generic_constraints = tp.generic_constraints_mut(decl_ref_id);
                for &generic_param in &decl.generic_params {
                    let constraints = arg_constraints.get_implied_generic_constraints(generic_param, &param_ty);
                    let generic_param_constraints = generic_constraints.entry(generic_param).or_default();
                    *generic_param_constraints = generic_param_constraints.intersect_with_in_generic_context(&constraints, &decl.generic_params);
                }
            }
        }

        // If any of the arguments have errors, propagate that to the declref
        // TODO: do this for self arguments as well
        if self.args.iter().any(|&arg| tp.constraints(arg).is_error()) {
            *tp.decl_ref_has_error_mut(decl_ref_id) = true;
        }

        *tp.constraints_mut(self.id) = ConstraintList::new(BuiltinTraits::empty(), Some(one_of), pref, ef!(driver, self.id.generic_ctx_id));
        *tp.overloads_mut(decl_ref_id) = overloads;
    }

    fn run_pass_2(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        let decl_ref_id = self.decl_ref_id(driver);

        let ty = tp.constraints(self.id).solve().unwrap_or(Type::Error.into());
        *tp.ty_mut(self.id) = ty.ty.clone();

        let mut callee_one_of: SmallVec<[QualType; 1]> = tp.constraints_mut(self.callee).one_of().iter().cloned().collect();
        callee_one_of.retain(|callee_ty| {
            let return_ty: QualType = callee_ty.ty.return_ty().unwrap().clone().into();
            // TODO: this is a temporary hack that should be removed.
            let overload_decl = tp.overloads(decl_ref_id).overloads.iter().copied()
                .find(|&decl| tp.decl_type(decl).trivially_convertible_to(callee_ty));
            if let Some(overload_decl) = overload_decl {
                if let Some(fun) = callee_ty.ty.as_function() {
                    // TODO: do this for self arguments as well
                    for (arg, param) in self.args.iter().copied().zip(&fun.param_tys) {
                        if driver.can_unify_to(tp, arg, UnificationType::QualTypeWithOldSchoolGenericContext(&param.into(), &driver.tir.decls[overload_decl].generic_params)).is_err() {
                            return false;
                        }
                    }
                }
                return_ty.trivially_convertible_to(&ty)
            } else {
                false
            }
        });

        let pref = tp.constraints(self.callee).preferred_type().cloned();
        
        // Select callee function type.
        let callee_ty = pref.as_ref().cloned()
            .filter(|pref| callee_one_of.iter().any(|ty| ty.trivially_convertible_to(pref) || pref.trivially_convertible_to(ty)))
            .or_else(|| callee_one_of.iter().next().cloned());
        if let Some(callee_ty) = callee_ty {
            let param_tys = &callee_ty.ty.as_function().unwrap().param_tys;
            debug_assert_eq!(param_tys.len(), self.args.len());
            // TODO: do this for self arguments as well
            for (&arg, param_ty) in self.args.iter().zip(param_tys) {
                if let Type::Inout(param_ty) = param_ty {
                    tp.constraints_mut(arg).set_to(QualType { ty: param_ty.as_ref().clone(), is_mut: true });
                } else {
                    tp.constraints_mut(arg).set_to(param_ty);
                }
            }
            tp.constraints_mut(self.callee).set_to(callee_ty);
        } else {
            // TODO: maybe push an error here? Right now I don't bother because DeclRef does it for me.
            tp.constraints_mut(self.callee).set_to(Type::Error);
        }
        tp.generic_substitution_list_mut(decl_ref_id).extend(&self.args);

        if let Some(pref) = pref {
            tp.constraints_mut(self.callee).set_preferred_type(pref);
        }
    }
}

impl tir::Expr<tir::AddrOf> {
    fn run_pass_1(&self, _driver: &mut Driver, tp: &mut impl TypeProvider) {
        let constraints = tp.constraints(self.expr).filter_map(|ty| {
            if self.is_mut && !ty.is_mut { return None; }
            Some(
                QualType::from(
                    ty.ty.clone().ptr_with_mut(self.is_mut)
                )
            )
        });
        *tp.constraints_mut(self.id) = constraints;
    }

    fn run_pass_2(&self, _driver: &mut Driver, tp: &mut impl TypeProvider) {
        let pointer_ty = tp.constraints(self.id).solve()
            .map(|ty| ty.ty)
            .unwrap_or(Type::Error);
        let pointee_ty = match pointer_ty {
            Type::Pointer(ref pointee) => pointee.as_ref().clone(),
            Type::Error => Type::Error.into(),
            _ => panic!("unexpected non-pointer, non-error type for addr of expression"),
        };
        tp.constraints_mut(self.expr).set_to(pointee_ty);
        *tp.ty_mut(self.id) = pointer_ty;
    }
}

impl tir::Expr<tir::Dereference> {
    fn run_pass_1(&self, _driver: &mut Driver, tp: &mut impl TypeProvider) {
        let constraints = tp.constraints(self.expr).filter_map(|ty| {
            ty.ty.deref().cloned()
        });
        *tp.constraints_mut(self.id) = constraints;
    }

    fn run_pass_2(&self, _driver: &mut Driver, tp: &mut impl TypeProvider) {
        let mut ty = tp.constraints(self.id).solve().unwrap_or(Type::Error.into());
        *tp.ty_mut(self.id) = ty.ty.clone();

        if ty.ty != Type::Error {
            ty = ty.ptr().into();
        }
        tp.constraints_mut(self.expr).set_to(ty);
    }
}

impl tir::Expr<tir::Pointer> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        if let Some(err) = driver.can_unify_to(tp, self.expr, &Type::Ty.into()).err() {
            let mut error = Error::new("Expected type operand to pointer operator");
            let range = driver.get_range(self.expr);
            match err {
                UnificationError::InvalidChoice(choices)
                    => error.add_primary_range(range, format!("expression could've unified to any of {:?}", choices)),
                UnificationError::Trait(not_implemented)
                    => error.add_primary_range(
                        range,
                        format!(
                            "couldn't unify because expression requires implementations of {:?}",
                            not_implemented.names(),
                        ),
                    ),
            }
            driver.diag.push(error);
        }
        tp.constraints_mut(self.id).set_to(Type::Ty);
    }

    fn run_pass_2(&self, _driver: &mut Driver, tp: &mut impl TypeProvider) {
        let expr_ty = tp.constraints(self.expr).solve().map(|ty| ty.ty).unwrap_or(Type::Error);
        // Don't bother checking if it's a type, because we already did that in pass 1
        tp.constraints_mut(self.expr).set_to(expr_ty);
        let ty = tp.constraints(self.id).solve().expect("Ambiguous type for pointer expression");
        debug_assert_eq!(ty.ty, Type::Ty);
        *tp.ty_mut(self.id) = Type::Ty;
    }
}

impl tir::Expr<tir::FunctionTy> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        fn check_type(driver: &mut Driver, tp: &impl TypeProvider, ty: ExprId) {
            if let Some(err) = driver.can_unify_to(tp, ty, &Type::Ty.into()).err() {
                let mut error = Error::new("Expected type");
                let range = driver.get_range(ty);
                match err {
                    UnificationError::InvalidChoice(choices)
                        => error.add_primary_range(range, format!("expression could've unified to any of {:?}", choices)),
                    UnificationError::Trait(not_implemented)
                        => error.add_primary_range(
                            range,
                            format!(
                                "couldn't unify because expression requires implementations of {:?}",
                                not_implemented.names(),
                            ),
                        ),
                }
                driver.diag.push(error);
            }
        }
        for &param_ty in &self.param_tys {
            check_type(driver, tp, param_ty);
        }
        check_type(driver, tp, self.ret_ty);
        tp.constraints_mut(self.id).set_to(Type::Ty);
    }

    fn run_pass_2(&self, _driver: &mut Driver, tp: &mut impl TypeProvider) {
        fn solve_ty(tp: &mut impl TypeProvider, ty: ExprId) {
            let expr_ty = tp.constraints(ty).solve().map(|ty| ty.ty).unwrap_or(Type::Error);
            // Don't bother checking if it's a type, because we already did that in pass 1
            tp.constraints_mut(ty).set_to(expr_ty);
        }
        for &param_ty in &self.param_tys {
            solve_ty(tp, param_ty);
        }
        solve_ty(tp, self.ret_ty);

        let ty = tp.constraints(self.id).solve().expect("Ambiguous type for function type expression");
        debug_assert_eq!(ty.ty, Type::Ty);
        *tp.ty_mut(self.id) = Type::Ty;
    }
}

impl tir::Expr<tir::Struct> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        for &field_ty in &self.field_tys {
            if let Some(err) = driver.can_unify_to(tp, field_ty, &Type::Ty.into()).err() {
                let mut error = Error::new("Expected field type");
                let range = driver.get_range(field_ty);
                match err {
                    UnificationError::InvalidChoice(choices)
                        => error.add_secondary_range(range, format!("note: expression could've unified to any of {:?}", choices)),
                    UnificationError::Trait(not_implemented)
                        => error.add_secondary_range(
                            range,
                            format!(
                                "note: couldn't unify because expression requires implementations of {:?}",
                                not_implemented.names(),
                            ),
                        ),
                }
                driver.diag.push(error);
            }
        }
        tp.constraints_mut(self.id).set_to(Type::Ty);
    }

    fn run_pass_2(&self, _driver: &mut Driver, tp: &mut impl TypeProvider) {
        for &field_ty in &self.field_tys {
            let field_type = tp.constraints(field_ty).solve().map(|ty| ty.ty).unwrap_or(Type::Error);
            // Don't bother checking if it's a type, because we already did that in pass 1
            tp.constraints_mut(field_ty).set_to(field_type);
        }
        let ty = tp.constraints(self.id).solve().expect("Ambiguous type for struct expression");
        debug_assert_eq!(ty.ty, Type::Ty);
        *tp.ty_mut(self.id) = Type::Ty;
    }
}

impl tir::Expr<tir::StructLit> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        let ty = if let Some(err) = driver.can_unify_to(tp, self.ty, &Type::Ty.into()).err() {
            let mut error = Error::new("Expected struct type");
            let range = driver.get_range(self.ty);
            match err {
                UnificationError::InvalidChoice(choices)
                    => error.add_secondary_range(range, format!("note: expression could've unified to any of {:?}", choices)),
                UnificationError::Trait(not_implemented)
                    => error.add_secondary_range(
                        range,
                        format!(
                            "note: couldn't unify because expression requires implementations of {:?}",
                            not_implemented.names(),
                        ),
                    ),
            }
            driver.diag.push(error);
            Type::Error
        } else {
            let ty = tp.get_evaluated_type(self.ty).clone();
            match &ty {
                Type::Struct(strukt) => {
                    let struct_fields = &driver.code.ast.structs[strukt.identity].fields;
                    let mut matches = Vec::new();
                    matches.resize(struct_fields.len(), ExprId::new(u32::MAX as usize));

                    let mut successful = true;

                    // Find matches for each field in the literal
                    'lit_fields: for lit_field in &self.fields {
                        for (i, struct_field) in struct_fields.iter().enumerate() {
                            if struct_field.name == lit_field.name {
                                matches[i] = lit_field.expr;
                                continue 'lit_fields;
                            }
                        }

                        // We can assume there is no match for this field at this point; if we had found one, we
                        // would've already continued to the next field.
                        successful = false;
                        
                        // TODO: Use range of the field identifier, which we don't have fine-grained access to yet
                        let range = driver.get_range(self.id);
                        driver.diag.push(
                            Error::new(format!("Unknown field {} in struct literal", driver.interner.resolve(lit_field.name).unwrap()))
                                .adding_primary_range(range, "")
                        );

                    }

                    let lit_range = driver.get_range(self.id);
                    // Make sure each field in the struct has a match in the literal
                    for (i, &maatch) in matches.iter().enumerate() {
                        let field = &struct_fields[i];
                        if maatch == ExprId::new(u32::MAX as usize) {
                            successful = false;

                            driver.diag.push(
                                Error::new(format!("Field {} not included in struct literal", driver.interner.resolve(field.name).unwrap()))
                                    .adding_primary_range(lit_range, "")
                                    .adding_secondary_range(field.decl, "field declared here")
                            );
                        } else if let Some(err) = driver.can_unify_to(tp, maatch, field.ty).err() {
                            successful = false;
                            let range = driver.get_range(maatch);
                            let field_ty = tp.get_evaluated_type(field.ty).clone();
                            let mut error = Error::new("Invalid struct field type")
                                .adding_primary_range(range, format!("expected {:?}", field_ty));
                            match err {
                                UnificationError::InvalidChoice(choices)
                                    => error.add_secondary_range(range, format!("note: expression could've unified to any of {:?}", choices)),
                                UnificationError::Trait(not_implemented)
                                    => error.add_secondary_range(
                                        range,
                                        format!(
                                            "note: couldn't unify because expression requires implementations of {:?}",
                                            not_implemented.names(),
                                        ),
                                    ),
                            }
                            driver.diag.push(error);
                        }
                    }

                    if successful {
                        *tp.struct_lit_mut(self.struct_lit_id) = Some(
                            StructLit { strukt: strukt.identity, fields: matches }
                        );
                    }
                },
                other => {
                    let range = driver.get_range(self.ty);
                    driver.diag.push(
                        Error::new(format!("Expected struct type in literal, found {:?}", *other))
                            .adding_primary_range(range, "")
                    );
                },
            }
            
            ty
        };

        tp.constraints_mut(self.id).set_to(ty);
    }

    fn run_pass_2(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        let ty = tp.constraints(self.id).solve().unwrap();
        *tp.ty_mut(self.id) = ty.ty;

        // Yay borrow checker:
        if let Some(lit) = tp.struct_lit(self.struct_lit_id).clone() {
            let fields = &driver.code.ast.structs[lit.strukt].fields;
            let is_generic = fields.iter()
                .any(|field| tp.get_evaluated_type(field.ty).has_generic_parameters());
            driver.mir.struct_was_non_generic[lit.strukt] = !is_generic;

            debug_assert_eq!(lit.fields.len(), fields.len());

            for (i, field) in fields.iter().enumerate() {
                let field = field.decl;
                let field_ty = tp.fetch_decl_type(driver, field, None).ty;

                tp.constraints_mut(lit.fields[i]).set_to(field_ty);
            }
        }
    }
}

impl tir::Expr<tir::If> {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        if let Some(err) = driver.can_unify_to(tp, self.condition, &Type::Bool.into()).err() {
            let mut error = Error::new("Expected boolean condition in if expression");
            let range = driver.get_range(self.condition);
            match err {
                UnificationError::InvalidChoice(choices)
                    => error.add_secondary_range(range, format!("note: expression could've unified to any of {:?}", choices)),
                UnificationError::Trait(not_implemented)
                    => error.add_secondary_range(
                        range,
                        format!(
                            "note: couldn't unify because expression requires implementations of {:?}",
                            not_implemented.names(),
                        ),
                    ),
            }
            driver.diag.push(error);
        }
        let constraints = tp.constraints(self.then_expr).intersect_with(tp.constraints(self.else_expr));

        if constraints.solve().is_err() {
            // TODO: handle void expressions, which don't have appropriate source location info.
            driver.diag.push(
                Error::new("Failed to unify branches of if expression")
                    .adding_primary_range(driver.get_range(self.then_expr), "first terminal expression here")
                    .adding_primary_range(driver.get_range(self.else_expr), "second terminal expression here")
            );
        }
        *tp.constraints_mut(self.id) = constraints;
    }

    fn run_pass_2(&self, _driver: &mut Driver, tp: &mut impl TypeProvider) {
        let condition_ty = tp.constraints(self.condition).solve().map(|ty| ty.ty).unwrap_or(Type::Error);
        // Don't bother checking if bool, because we already did that in pass 1
        tp.constraints_mut(self.condition).set_to(condition_ty);
        // If the if branches can't unify, it will be diagnosed above. So just propagate the error.
        let ty = tp.constraints(self.id).solve().unwrap_or_else(|_| Type::Error.into());
        *tp.ty_mut(self.id) = ty.ty.clone();
        tp.constraints_mut(self.then_expr).set_to(ty.clone());
        tp.constraints_mut(self.else_expr).set_to(ty);
    }
}

impl tir::Expr<tir::Do> {
    fn run_pass_1(&self, _driver: &mut Driver, tp: &mut impl TypeProvider) {
        *tp.constraints_mut(self.id) = tp.constraints(self.terminal_expr).clone();
    }

    fn run_pass_2(&self, _driver: &mut Driver, tp: &mut impl TypeProvider) {
        let ty = tp.constraints(self.id).solve().expect("Ambiguous type for do expression");
        *tp.ty_mut(self.id) = ty.ty.clone();
        tp.constraints_mut(self.terminal_expr).set_to(ty);
    }
}

impl tir::Stmt {
    fn run_pass_1(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        if let Some(err) = driver.can_unify_to(tp, self.root_expr, &Type::Void.into()).err() {
            if self.has_semicolon {
                return;
            }
            let mut error = Error::new("statements must return void")
                .adding_primary_range(self.root_expr, "to ignore the result, try adding ';' at the end of this statement");
            match err {
                UnificationError::InvalidChoice(choices)
                => error.add_secondary_range(self.root_expr, format!("note: expression could've unified to any of {:?}", choices)),
                UnificationError::Trait(not_implemented)
                => error.add_secondary_range(
                    self.root_expr,
                    format!(
                        "note: couldn't unify because expression requires implementations of {:?}",
                        not_implemented.names(),
                    ),
                ),
            }
            driver.diag.push(error);
        } else {
            let constraints = tp.constraints_mut(self.root_expr);
            constraints.set_to(Type::Void);
        }
    }

    fn run_pass_2(&self, _driver: &mut Driver, _tp: &mut impl TypeProvider) {
    }
}

impl tir::RetGroup {
    fn run_pass_1(&self, _driver: &mut Driver, _tp: &mut impl TypeProvider) {}

    fn run_pass_2(&self, driver: &mut Driver, tp: &mut impl TypeProvider) {
        let ty = tp.get_evaluated_type(self.ty).clone();
        for &expr in &self.exprs {
            if let Some(err) = driver.can_unify_to(tp, expr, &QualType::from(&ty)).err() {
                let range = driver.get_range(expr);
                let error = if expr == VOID_EXPR {
                    Error::new(format!("expected return value of type `{:?}`, found nothing instead", ty))
                        .adding_primary_range(self.decl_range, "declaration here")
                } else {
                    let mut error = Error::new(format!("can't unify expression to return type `{:?}`", ty))
                        .adding_primary_range(self.decl_range, "declaration here")
                        .adding_primary_range(range, "expression here");
                    match err {
                        UnificationError::InvalidChoice(choices)
                            => error.add_secondary_range(range, format!("note: expression could've unified to any of {:?}", choices)),
                        UnificationError::Trait(not_implemented)
                            => error.add_secondary_range(
                                range,
                                format!(
                                    "note: couldn't unify because expression requires implementations of {:?}",
                                    not_implemented.names(),
                                ),
                            ),
                    }
                    error
                };
                driver.diag.push(error);
            }

            // Assume we panic above unless the returned expr can unify to the return type
            tp.constraints_mut(expr).set_to(ty.clone());
        }
    }
}

impl tir::FunctionDecl {
    fn run_pass_1(&self, driver: &Driver, tp: &mut impl TypeProvider) {
        tp.set_decl_type_to_explicit_type_if_exists(driver, self.id);
    }

    fn run_pass_2(&self, _driver: &mut Driver, _tp: &mut impl TypeProvider) {}
}

impl Driver {
    pub fn decl_type(&self, id: DeclId, tp: &(impl TypeProvider + ?Sized)) -> Type {
        let explicit_ty = self.code.ast.explicit_tys[id].map(|ty| tp.get_evaluated_type(ty)).unwrap_or(&tp.decl_type(id).ty).clone();
        match &df!(id.ast) {
            ast::Decl::Computed { param_tys, .. } | ast::Decl::LegacyIntrinsic { function_like: true, param_tys, .. } =>
                self.function_decl_type(param_tys, explicit_ty, tp),
            ast::Decl::ComputedPrototype { param_list, .. } => self.function_decl_type(&param_list.param_tys, explicit_ty, tp),
            &ast::Decl::Intrinsic(intr) => {
                let param_tys = &self.code.ast.intrinsics[intr].param_tys;
                self.function_decl_type(param_tys, explicit_ty, tp)
            },
            &ast::Decl::MethodIntrinsic(intr) => {
                let param_tys = &self.code.ast.intrinsics[intr].param_tys[1..];
                self.function_decl_type(param_tys, explicit_ty, tp)
            },
            _ => explicit_ty,
        }
    }

    fn function_decl_type(&self, param_tys: &[ExprId], explicit_ty: Type, tp: &(impl TypeProvider + ?Sized)) -> Type {
        let param_tys: Vec<_> = param_tys.iter().copied()
            .map(|ty| tp.get_evaluated_type(ty).clone())
            .collect();
        let return_ty = Box::new(explicit_ty);
        Type::Function(FunctionType { param_tys, return_ty })
    }

    fn run_pass_1(&mut self, unit: &UnitItems, start_level: u32, tp: &mut impl TypeProvider) {
        macro_rules! run_pass_1_flat {
            ($($name:ident$(,)*)+) => {
                $(
                    for item in &unit.$name {
                        item.run_pass_1(self, tp);
                    }
                )+
            }
        }
        run_pass_1_flat!(int_lits, dec_lits, str_lits, char_lits, bool_lits, consts, generic_params, error_exprs, func_decls, breaks, continues);

        for level in start_level..unit.num_levels() {
            macro_rules! run_pass_1 {
                ($($name:ident$(,)*)+) => {
                    $(
                        for item in unit.$name.get_level(level) {
                            item.run_pass_1(self, tp);
                        }
                    )+
                }
            }
            run_pass_1!(
                assigned_decls, assignments, casts, whiles, fors, explicit_rets, modules,
                decl_refs, calls, addr_ofs, derefs, pointers, structs, struct_lits, ifs, dos,
                ret_groups, switches, enums, pattern_bindings, function_tys,
            );
        }

        // This code must be here, because it checks that all statements can unify to void
        run_pass_1_flat!(stmts);
    }

    fn run_pass_2(&mut self, unit: &UnitItems, tp: &mut impl TypeProvider) {
        for level in (0..unit.num_levels()).rev() {
            macro_rules! run_pass_2 {
                ($($name:ident$(,)*)+) => {
                    $(
                        for item in unit.$name.get_level(level) {
                            self.initialize_global_expressions(tp); // this is a hack to erase the effects of modifying constraints on VOID_EXPR and ERROR_EXPR
                            item.run_pass_2(self, tp);
                        }
                    )+
                }
            }
            run_pass_2!(
                assigned_decls, assignments, casts, whiles, fors, explicit_rets, modules,
                decl_refs, calls, addr_ofs, derefs, pointers, structs, struct_lits, ifs, dos,
                ret_groups, switches, enums, pattern_bindings, function_tys,
            );
        }
        macro_rules! run_pass_2_flat {
            ($($name:ident$(,)*)+) => {
                $(
                    for item in &unit.$name {
                        item.run_pass_2(self, tp);
                    }
                )+
            }
        }
        run_pass_2_flat!(int_lits, dec_lits, str_lits, char_lits, bool_lits, consts, generic_params, stmts, error_exprs, func_decls, breaks, continues);
    }

    fn initialize_global_expressions(&self, tp: &mut impl TypeProvider) {
        *tp.constraints_mut(ast::VOID_EXPR) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Void.into()]), None, BLANK_GENERIC_CTX);
        *tp.ty_mut(ast::VOID_EXPR) = Type::Void;
        *tp.constraints_mut(ast::ERROR_EXPR) = ConstraintList::new(BuiltinTraits::empty(), Some(smallvec![Type::Error.into()]), None, BLANK_GENERIC_CTX);
        *tp.ty_mut(ast::ERROR_EXPR) = Type::Error;
    }

    pub fn get_real_type_provider(&self) -> RealTypeProvider {
        // Assign the type of the void expression to be void.
        let mut tp = RealTypeProvider::new(self);
        self.initialize_global_expressions(&mut tp);
        tp
    }
}

impl DriverRef<'_> {
    pub fn type_check(&mut self, units: &Units, tp: &mut RealTypeProvider, new_code: NewCode) -> Result<(), ()> {
        tp.resize(&self.read(), new_code);
        // depended on by StructLit
        let num_structs = self.read().code.ast.structs.len();
        self.write().mir.struct_was_non_generic.resize(num_structs, false);
        for unit in &units.units {
            // Pass 1: propagate info down from leaves to roots
            self.write().run_pass_1(&unit.items, 0, tp);
            
            // Pass 2: propagate info up from roots to leaves
            self.write().run_pass_2(&unit.items, tp);

            if self.read().diag.has_errors() {
                return Err(());
            }

            for i in 0..unit.eval_dependees.len() {
                let expr = unit.eval_dependees[i];
                let val = self.eval_expr(expr, tp);
                tp.insert_eval_result(expr, val);
            }
        }

        for unit in &units.mock_units {
            let mut mock_tp = MockTypeProvider::new(tp);
            self.write().run_pass_1(&unit.items, 0, &mut mock_tp);
            let constraints = mock_tp.constraints(unit.main_expr);
            let one_of = constraints
                .one_of().iter()
                .map(|ty| ty.ty.clone())
                .collect::<Vec<_>>();
            if constraints.is_error() {
                self.write().tir.expr_namespaces.entry(unit.main_expr).or_default().push(ExprNamespace::Error);
            } else {
                for ty in one_of {
                    let ns = match ty {
                        Type::Mod => {
                            self.write().run_pass_2(&unit.items, &mut mock_tp);
                            let module = self.eval_expr(unit.main_expr, &mock_tp);
                            match module {
                                Const::Mod(scope) => ExprNamespace::Mod(scope),
                                _ => panic!("Unexpected const kind, expected module!"),
                            }
                        },
                        Type::Struct(strukt) => ExprNamespace::New(self.read().code.ast.structs[strukt.identity].namespace, NewNamespaceRefKind::Instance),
                        Type::Enum(id) => ExprNamespace::New(self.read().code.ast.enums[id].namespace, NewNamespaceRefKind::Instance),
                        Type::Internal(id) => ExprNamespace::New(self.read().code.ast.internal_types[id].namespace, NewNamespaceRefKind::Instance),
                        Type::Pointer(ref pointee) => {
                            match &pointee.ty {
                                Type::Struct(strukt) => ExprNamespace::New(self.read().code.ast.structs[strukt.identity].namespace, NewNamespaceRefKind::Instance),
                                &Type::Enum(id) => ExprNamespace::New(self.read().code.ast.enums[id].namespace, NewNamespaceRefKind::Instance),
                                &Type::Internal(id) => ExprNamespace::New(self.read().code.ast.internal_types[id].namespace, NewNamespaceRefKind::Instance),
                                _ => continue,
                            }
                        },
                        Type::Ty => {
                            self.write().run_pass_2(&unit.items, &mut mock_tp);
                            let ty = self.eval_expr(unit.main_expr, &mock_tp);
    
                            match ty {
                                Const::Ty(Type::Struct(ref strukt)) => ExprNamespace::New(self.read().code.ast.structs[strukt.identity].namespace, NewNamespaceRefKind::Static),
                                Const::Ty(Type::Enum(id)) => ExprNamespace::New(self.read().code.ast.enums[id].namespace, NewNamespaceRefKind::Static),
                                Const::Ty(Type::Internal(id)) => ExprNamespace::New(self.read().code.ast.internal_types[id].namespace, NewNamespaceRefKind::Static),
                                _ => panic!("Unexpected const kind, expected enum!"),
                            }
                        },
                        Type::LegacyInternal(ty) => match ty {
                            LegacyInternalType::StringLiteral => ExprNamespace::Internal(InternalNamespace::StringLiteral),
                        },
                        Type::Error => ExprNamespace::Error,
                        _ => continue,
                    };
                    self.write().tir.expr_namespaces.entry(unit.main_expr).or_default().push(ns);
                }
            }
        }
        Ok(())
    }
}
