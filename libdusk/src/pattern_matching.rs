// Reference:
//  - http://moscova.inria.fr/~maranget/papers/ml05e-maranget.pdf
//  - https://compiler.club/compiling-pattern-matching/

use std::collections::HashMap;
use index_vec::define_index_type;
use string_interner::DefaultSymbol as Sym;
use crate::driver::Driver;
use crate::ast::{DeclId, ExprId, Pattern, PatternKind, PatternMatchingContextId};
use crate::ty::Type;
use crate::source_info::SourceRange;
use crate::error::Error;
use crate::type_provider::TypeProvider;
use crate::index_vec::*;

define_index_type!(pub struct SwitchScrutineeValueId = u32;);
define_index_type!(pub struct SwitchDestinationId = u32;);

pub const VOID_SCRUTINEE_VALUE: SwitchScrutineeValueId = SwitchScrutineeValueId::from_raw_unchecked(0);
pub const ORIGINAL_SCRUTINEE_VALUE: SwitchScrutineeValueId = SwitchScrutineeValueId::from_raw_unchecked(1);

#[derive(Hash, PartialEq, Eq, Debug, Clone)]
pub enum SwitchDecisionValue {
    EnumVariant(usize),
    UnsignedInt(u64),
    SignedInt(i64),
}

#[derive(Debug, Clone)]
pub enum SwitchDecisionNode {
    Branch {
        scrutinee: SwitchScrutineeValueId,
        paths: HashMap<SwitchDecisionValue, SwitchDecisionNode>,
        default_path: Option<Box<SwitchDecisionNode>>,
    },
    Destination {
        destination: SwitchDestinationId,
        bindings: Vec<SwitchBinding>,
    },
    Failure,
}

#[derive(Debug, Clone)]
pub struct SwitchBinding {
    pub value: SwitchScrutineeValueId,
    pub decl: DeclId,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum SwitchScrutineeValue {
    OriginalScrutinee,
    VoidValue,
    EnumPayload { enum_value: SwitchScrutineeValueId, variant_name: Sym },
}

#[derive(Debug, Clone, Copy)]
pub enum TypedSwitchScrutineeValueKind {
    OriginalScrutinee,
    VoidValue,
    EnumPayload { enum_value: SwitchScrutineeValueId, variant_index: usize },
}

#[derive(Debug, Clone)]
pub struct TypedSwitchScrutineeValue {
    pub kind: TypedSwitchScrutineeValueKind,
    pub ty: Type,
}

#[derive(Clone)]
pub struct SwitchScrutinee {
    pub value: SwitchScrutineeValueId,
    pub ty: Type,
}

pub struct PatternMatchingContext {
    pub scrutinee: ExprId,
    pub scrutinee_values: IndexVec<SwitchScrutineeValueId, SwitchScrutineeValue>,
    scrutinee_map: HashMap<SwitchScrutineeValue, SwitchScrutineeValueId>,
}

impl PatternMatchingContext {
    pub fn new(scrutinee: ExprId) -> Self {
        let mut result = Self {
            scrutinee,
            scrutinee_values: Default::default(),
            scrutinee_map: Default::default(),
        };
        result.scrutinee_values.push_at(VOID_SCRUTINEE_VALUE, SwitchScrutineeValue::VoidValue);
        result.scrutinee_values.push_at(ORIGINAL_SCRUTINEE_VALUE, SwitchScrutineeValue::OriginalScrutinee);
        result.scrutinee_map.insert(SwitchScrutineeValue::VoidValue, VOID_SCRUTINEE_VALUE);
        result.scrutinee_map.insert(SwitchScrutineeValue::OriginalScrutinee, ORIGINAL_SCRUTINEE_VALUE);
        result
    }

    pub fn add_scrutinee_value(&mut self, scrutinee: SwitchScrutineeValue) -> SwitchScrutineeValueId {
        *self.scrutinee_map.entry(scrutinee)
            .or_insert_with(|| self.scrutinee_values.push(scrutinee))
    }
}

impl Driver {
    pub fn get_typed_pattern_matching_context(&self, tp: &mut dyn TypeProvider, context: PatternMatchingContextId) {
        if tp.pattern_matching_context_mut(context).is_some() {
            return;
        }

        let pattern_matching_context = &self.code.ast.pattern_matching_contexts[context];
        let scrutinee = pattern_matching_context.scrutinee;
        let constraints = self.get_constraints(tp, scrutinee);
        let scrutinee_ty = self.solve_constraints(tp, constraints).expect("Unable to resolve type for pattern matching scrutinee").qual_ty.ty;

        let mut scrutinee_values = IndexVec::<SwitchScrutineeValueId, TypedSwitchScrutineeValue>::new();
        for (scrutinee, scrutinee_value) in pattern_matching_context.scrutinee_values.iter_enumerated() {
            let scrutinee_value = match scrutinee_value {
                SwitchScrutineeValue::OriginalScrutinee => TypedSwitchScrutineeValue {
                    kind: TypedSwitchScrutineeValueKind::OriginalScrutinee,
                    ty: scrutinee_ty.clone(),
                },
                SwitchScrutineeValue::VoidValue => TypedSwitchScrutineeValue {
                    kind: TypedSwitchScrutineeValueKind::VoidValue,
                    ty: Type::Void,
                },
                &SwitchScrutineeValue::EnumPayload { enum_value, variant_name } => {
                    let Type::Enum(enum_id) = scrutinee_values[enum_value].ty else {
                        panic!("enum scrutinee is not of enum type");
                    };

                    let variants = &self.code.ast.enums[enum_id].variants;
                    let variant_index = variants.iter().position(|variant| variant.name == variant_name).unwrap();
                    let payload_ty = variants[variant_index].payload_ty.unwrap();
                    let payload_ty = tp.get_evaluated_type(payload_ty).clone();
                    TypedSwitchScrutineeValue {
                        kind: TypedSwitchScrutineeValueKind::EnumPayload { enum_value, variant_index },
                        ty: payload_ty,
                    }
                },
            };
            scrutinee_values.push_at(scrutinee, scrutinee_value);
        }
        
        let context = tp.pattern_matching_context_mut(context);
        *context = Some(scrutinee_values);
    }
}

pub fn match_scrutinee(driver: &mut Driver, tp: &mut dyn TypeProvider, scrutinee: ExprId, context: PatternMatchingContextId, mut scrutinees: Vec<SwitchScrutinee>, mut pattern_matrix: Vec<Vec<Pattern>>, destinations: Vec<SwitchDestinationId>) -> SwitchDecisionNode {
    if pattern_matrix.is_empty() {
        return SwitchDecisionNode::Failure;
    }

    // Validate arguments
    let pattern_matrix_width = pattern_matrix[0].len();
    for row in &pattern_matrix[1..] {
        assert_eq!(row.len(), pattern_matrix_width);
    }
    assert_eq!(scrutinees.len(), pattern_matrix_width);
    assert_ne!(pattern_matrix_width, 0);
    assert_eq!(pattern_matrix.len(), destinations.len());

    // Check if every pattern in first row is irrefutable
    let mut bindings = Vec::<SwitchBinding>::new();
    let mut first_refutable_pattern_index = None;
    for (i, (pattern, scrutinee)) in pattern_matrix[0].iter().zip(&scrutinees).enumerate() {
        match pattern.kind {
            PatternKind::NamedCatchAll { binding_decl, .. } => bindings.push(SwitchBinding { value: scrutinee.value, decl: binding_decl }),
            PatternKind::AnonymousCatchAll(_) => {},
            _ => {
                first_refutable_pattern_index = Some(i);
                break;
            },
        }
    }

    let Some(first_refutable_pattern_index) = first_refutable_pattern_index else {
        return SwitchDecisionNode::Destination { destination: destinations[0], bindings };
    };

    for row in &mut pattern_matrix {
        row.swap(0, first_refutable_pattern_index);
    }
    scrutinees.swap(0, first_refutable_pattern_index);

    // TODO: unreachable code checking (destination not present in tree)
    // TODO: exhaustiveness checking

    match scrutinees[0].ty {
        Type::Enum(id) => {
            let mut paths = HashMap::<SwitchDecisionValue, SwitchDecisionNode>::new();
            let mut child_matrices = HashMap::<usize, Vec<(Vec<Pattern>, SwitchDestinationId)>>::new();
            let mut catch_all_child_matrix_rows = Vec::<(Vec<Pattern>, SwitchDestinationId)>::new();
            let mut default_matrix = Vec::<Vec<Pattern>>::new();
            let variants = &driver.code.ast.enums[id].variants;
            let mut default_matrix_destinations = Vec::<SwitchDestinationId>::new();
            for (pattern_row, &destination) in pattern_matrix.iter().zip(&destinations) {
                match pattern_row[0].kind {
                    PatternKind::ContextualMember { name, range, ref payload } => {
                        let variant_name_str = driver.interner.resolve(name.symbol).unwrap();
                        let index = variants.iter().position(|variant| variant.name == name.symbol);
                        if let Some(index) = index {
                            let mut new_row = Vec::new();
                            if let Some(payload_pattern) = payload {
                                new_row.push((**payload_pattern).clone());
                            } else {
                                new_row.push(Pattern { kind: PatternKind::AnonymousCatchAll(SourceRange::default()), scrutinee: VOID_SCRUTINEE_VALUE });
                            }
                            new_row.extend_from_slice(&pattern_row[1..]);
                            let scrutinee_value = payload.map(|payload| payload.scrutinee).unwrap_or(VOID_SCRUTINEE_VALUE);
                            child_matrices.entry(index)
                                .or_insert_with(|| Vec::new())
                                .push((new_row, destination));
                        } else {
                            let err = Error::new(format!("Variant `{}` does not exist in enum {:?}", variant_name_str, scrutinees[0].ty))
                                .adding_primary_range(range, "referred to by pattern here");
                            driver.diag.push(err);
                        }
                    },
                    PatternKind::IntLit { range, .. } => {
                        let error = Error::new("cannot match enum type with integer pattern")
                            .adding_primary_range(range, "pattern here")
                            .adding_secondary_range(scrutinee, "scrutinee here");
                        driver.diag.push(error);
                    },
                    PatternKind::NamedCatchAll { .. } | PatternKind::AnonymousCatchAll(_) => {
                        default_matrix.push(pattern_row.clone());
                        default_matrix_destinations.push(destination);

                        let mut new_row = Vec::new();
                        new_row.push(pattern_row[0].clone());
                        new_row.extend_from_slice(&pattern_row[1..]);
                        catch_all_child_matrix_rows.push((new_row, destination));
                    },
                }
            }

            for (variant_index, mut child_matrix) in child_matrices {
                let context = tp.pattern_matching_context(context).as_ref().unwrap();
                let scrutinee_ty = context[scrutinee_value].ty.clone();
                let payload_scrutinee = SwitchScrutinee { value: scrutinee_value, ty: scrutinee_ty };
                let mut child_scrutinees = vec![payload_scrutinee];
                child_scrutinees.extend_from_slice(&scrutinees[1..]);

                child_matrix.extend_from_slice(&catch_all_child_matrix_rows);
                child_matrix.sort_by_key(|(_, row_index)| *row_index);
                let child_matrix_destinations = child_matrix.iter()
                    .map(|(_, destination)| *destination)
                    .collect();
                let child_matrix: Vec<_> = child_matrix.into_iter()
                    .map(|(pattern, _)| pattern)
                    .collect();

                paths.insert(SwitchDecisionValue::EnumVariant(variant_index), match_scrutinee(driver, tp, scrutinee, child_scrutinees, child_matrix, child_matrix_destinations));
            }

            let branch_scrutinee = scrutinees[0].value;
            let default_path = (!default_matrix.is_empty()).then(|| {
                Box::new(match_scrutinee(driver, tp, scrutinee, scrutinees, default_matrix, default_matrix_destinations))
            });

            SwitchDecisionNode::Branch { scrutinee: branch_scrutinee, paths, default_path }
        },
        Type::Int { .. } => {
            let mut paths = HashMap::<SwitchDecisionValue, SwitchDecisionNode>::new();

            let mut child_matrices = HashMap::<SwitchDecisionValue, Vec<(Vec<Pattern>, SwitchDestinationId)>>::new();
            let mut catch_all_child_matrix_rows = Vec::<(Vec<Pattern>, SwitchDestinationId)>::new();
            let mut default_matrix = Vec::<Vec<Pattern>>::new();
            let mut default_matrix_destinations = Vec::<SwitchDestinationId>::new();
            for (pattern_row, &destination) in pattern_matrix.iter().zip(&destinations) {
                match pattern_row[0].kind {
                    PatternKind::IntLit { value, .. } => {
                        let mut new_row = Vec::new();
                        new_row.push(Pattern { kind: PatternKind::AnonymousCatchAll(SourceRange::default()), scrutinee: VOID_SCRUTINEE_VALUE });
                        new_row.extend_from_slice(&pattern_row[1..]);
                        child_matrices.entry(SwitchDecisionValue::UnsignedInt(value))
                            .or_insert_with(|| Vec::new())
                            .push((new_row, destination));
                    },
                    PatternKind::ContextualMember { range, .. } => {
                        let error = Error::new("cannot match integer type with contextual member")
                            .adding_primary_range(range, "pattern here")
                            .adding_secondary_range(scrutinee, "scrutinee here");
                        driver.diag.push(error);
                    },
                    PatternKind::NamedCatchAll { .. } | PatternKind::AnonymousCatchAll(_) => {
                        default_matrix.push(pattern_row.clone());
                        default_matrix_destinations.push(destination);

                        let mut new_row = Vec::new();
                        new_row.push(pattern_row[0].clone());
                        new_row.extend_from_slice(&pattern_row[1..]);
                        catch_all_child_matrix_rows.push((new_row, destination));
                    },
                }
            }

            for (value, mut child_matrix) in child_matrices {
                let mut child_scrutinees = vec![SwitchScrutinee { value: VOID_SCRUTINEE_VALUE, ty: Type::Void }];
                child_scrutinees.extend_from_slice(&scrutinees[1..]);

                child_matrix.extend_from_slice(&catch_all_child_matrix_rows);
                child_matrix.sort_by_key(|(_, row_index)| *row_index);
                let child_matrix_destinations = child_matrix.iter()
                    .map(|(_, destination)| *destination)
                    .collect();
                let child_matrix: Vec<_> = child_matrix.into_iter()
                    .map(|(pattern, _)| pattern)
                    .collect();
                paths.insert(value, match_scrutinee(driver, tp, scrutinee, child_scrutinees, child_matrix, child_matrix_destinations));
            }

            let branch_scrutinee = scrutinees[0].value;
            let default_path = (!default_matrix.is_empty()).then(|| {
                Box::new(match_scrutinee(driver, tp, scrutinee, scrutinees, default_matrix, default_matrix_destinations))
            });

            SwitchDecisionNode::Branch { scrutinee: branch_scrutinee, paths, default_path }
        }
        _ => todo!("Type {:?} is not supported in switch expression scrutinee position", scrutinees[0].ty),
    }
}
