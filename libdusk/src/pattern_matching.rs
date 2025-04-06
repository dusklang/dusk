use std::collections::HashMap;
use index_vec::define_index_type;
use crate::driver::Driver;
use crate::ast::{PatternKind, ExprId};
use crate::ty::Type;
use crate::source_info::SourceRange;
use crate::error::Error;
use crate::ast::Ident;
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
    },
    Failure,
}

#[derive(Clone, Copy)]
pub enum SwitchScrutineeValue {
    OriginalScrutinee,
    VoidValue,
    EnumPayload { enum_value: SwitchScrutineeValueId, variant_index: usize },
}

#[derive(Clone)]
pub struct SwitchScrutinee {
    pub value: SwitchScrutineeValueId,
    pub ty: Type,
}

// Reference:
//  - http://moscova.inria.fr/~maranget/papers/ml05e-maranget.pdf
//  - https://compiler.club/compiling-pattern-matching/
pub fn match_scrutinee(driver: &mut Driver, tp: &mut dyn TypeProvider, scrutinee: ExprId, scrutinee_values: &mut IndexVec<SwitchScrutineeValueId, SwitchScrutineeValue>, mut scrutinees: Vec<SwitchScrutinee>, mut pattern_matrix: Vec<Vec<PatternKind>>, destinations: Vec<SwitchDestinationId>) -> SwitchDecisionNode {
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
    let mut first_refutable_pattern_index = None;
    for (i, pattern) in pattern_matrix[0].iter().enumerate() {
        if !matches!(pattern, PatternKind::NamedCatchAll(_) | PatternKind::AnonymousCatchAll(_)) {
            first_refutable_pattern_index = Some(i);
            break;
        }
    }

    let Some(first_refutable_pattern_index) = first_refutable_pattern_index else {
        // TODO: use a real destination here.
        return SwitchDecisionNode::Destination { destination: destinations[0] };
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
            let variants = &driver.code.ast.enums[id].variants;

            let mut child_matrices = HashMap::<usize, Vec<(Vec<PatternKind>, SwitchDestinationId)>>::new();
            let mut catch_all_child_matrix_rows = Vec::<(Vec<PatternKind>, SwitchDestinationId)>::new();
            let mut default_matrix = Vec::<Vec<PatternKind>>::new();
            let mut default_matrix_destinations = Vec::<SwitchDestinationId>::new();
            for (pattern_row, &destination) in pattern_matrix.iter().zip(&destinations) {
                match pattern_row[0] {
                    PatternKind::ContextualMember { name, range, ref payload } => {
                        let variant_name_str = driver.interner.resolve(name.symbol).unwrap();
                        let index = variants.iter().position(|variant| variant.name == name.symbol);
                        if let Some(index) = index {
                            let mut new_row = Vec::new();
                            if let Some(payload_pattern) = payload {
                                new_row.push((**payload_pattern).clone());
                            } else {
                                new_row.push(PatternKind::AnonymousCatchAll(SourceRange::default()));
                            }
                            new_row.extend_from_slice(&pattern_row[1..]);
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
                    PatternKind::NamedCatchAll(Ident { .. }) | PatternKind::AnonymousCatchAll(_) => {
                        default_matrix.push(pattern_row.clone());
                        default_matrix_destinations.push(destination);

                        let mut new_row = Vec::new();
                        new_row.push(PatternKind::AnonymousCatchAll(SourceRange::default()));
                        new_row.extend_from_slice(&pattern_row[1..]);
                        catch_all_child_matrix_rows.push((new_row, destination));
                    },
                }
            }

            for (variant_index, mut child_matrix) in child_matrices {
                let variants = &driver.code.ast.enums[id].variants;
                let payload_scrutinee = if let Some(payload_ty) = variants[variant_index].payload_ty {
                    let payload_ty = tp.get_evaluated_type(payload_ty).clone();
                    let scrutinee_value = scrutinee_values.push(SwitchScrutineeValue::EnumPayload { enum_value: scrutinees[0].value, variant_index });
                    SwitchScrutinee { value: scrutinee_value, ty: payload_ty }
                } else {
                    SwitchScrutinee { value: VOID_SCRUTINEE_VALUE, ty: Type::Void }
                };
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

                paths.insert(SwitchDecisionValue::EnumVariant(variant_index), match_scrutinee(driver, tp, scrutinee, scrutinee_values, child_scrutinees, child_matrix, child_matrix_destinations));
            }

            let branch_scrutinee = scrutinees[0].value;
            let default_path = (!default_matrix.is_empty()).then(|| {
                Box::new(match_scrutinee(driver, tp, scrutinee, scrutinee_values, scrutinees, default_matrix, default_matrix_destinations))
            });

            SwitchDecisionNode::Branch { scrutinee: branch_scrutinee, paths, default_path }
        },
        Type::Int { .. } => {
            let mut paths = HashMap::<SwitchDecisionValue, SwitchDecisionNode>::new();

            let mut child_matrices = HashMap::<SwitchDecisionValue, Vec<(Vec<PatternKind>, SwitchDestinationId)>>::new();
            let mut catch_all_child_matrix_rows = Vec::<(Vec<PatternKind>, SwitchDestinationId)>::new();
            let mut default_matrix = Vec::<Vec<PatternKind>>::new();
            let mut default_matrix_destinations = Vec::<SwitchDestinationId>::new();
            for (pattern_row, &destination) in pattern_matrix.iter().zip(&destinations) {
                match pattern_row[0] {
                    PatternKind::IntLit { value, .. } => {
                        let mut new_row = Vec::new();
                        new_row.push(PatternKind::AnonymousCatchAll(SourceRange::default()));
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
                    PatternKind::NamedCatchAll(Ident { .. }) | PatternKind::AnonymousCatchAll(_) => {
                        default_matrix.push(pattern_row.clone());
                        default_matrix_destinations.push(destination);

                        let mut new_row = Vec::new();
                        new_row.push(PatternKind::AnonymousCatchAll(SourceRange::default()));
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
                paths.insert(value, match_scrutinee(driver, tp, scrutinee, scrutinee_values, child_scrutinees, child_matrix, child_matrix_destinations));
            }

            let branch_scrutinee = scrutinees[0].value;
            let default_path = (!default_matrix.is_empty()).then(|| {
                Box::new(match_scrutinee(driver, tp, scrutinee, scrutinee_values, scrutinees, default_matrix, default_matrix_destinations))
            });

            SwitchDecisionNode::Branch { scrutinee: branch_scrutinee, paths, default_path }
        }
        _ => todo!("Type {:?} is not supported in switch expression scrutinee position", scrutinees[0].ty),
    }
}
