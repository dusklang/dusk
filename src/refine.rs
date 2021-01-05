use std::collections::{HashSet, HashMap};

use mire::{BlockId, OpId};
use mire::mir::{Const, Function, Instr};
use mire::ty::Type;
use mire::hir::Intrinsic;

use crate::mir::{FunctionRef, function_by_ref};
use crate::driver::Driver;

#[derive(Default)]
pub struct Refine {
}

#[derive(Clone, PartialEq, Debug)]
enum ConstraintValue {
    Instr(OpId),
    Constant(Const),
    Diff { lhs: Box<ConstraintValue>, rhs: Box<ConstraintValue> },
}

impl ConstraintValue {
    fn make_substitutions(&mut self, substitutions: &HashMap<OpId, ConstraintValue>) {
        match self {
            ConstraintValue::Constant(_) => {},
            &mut ConstraintValue::Instr(op) => {
                if let Some(val) = substitutions.get(&op) {
                    *self = val.clone();
                }
            },
            ConstraintValue::Diff { lhs, rhs } => {
                lhs.make_substitutions(substitutions);
                rhs.make_substitutions(substitutions);
            }
        }
    }

    fn as_const(&self) -> Option<Const> {
        match self {
            ConstraintValue::Instr(_) => None,
            ConstraintValue::Constant(konst) => Some(konst.clone()),
            ConstraintValue::Diff { lhs, rhs } => {
                lhs.as_const().zip(rhs.as_const())
                    .into_iter()
                    .filter_map(|(lhs, rhs)| {
                        if let (Const::Int { lit: lhs, ty: lhs_ty }, Const::Int { lit: rhs, ty: rhs_ty }) = (lhs, rhs) {
                            assert_eq!(lhs_ty, rhs_ty, "types must be equal");
                            Some(Const::Int { lit: lhs - rhs, ty: lhs_ty })
                        } else {
                            None
                        }
                    }).next()
            }
        }
    }
}

#[derive(Debug)]
enum Constraint {
    Equal { lhs: ConstraintValue, rhs: ConstraintValue, },
    Lte { lhs: ConstraintValue, rhs: ConstraintValue, },
}

impl Constraint {
    fn make_substitutions(&mut self, substitutions: &HashMap<OpId, ConstraintValue>) {
        match self {
            Constraint::Equal { lhs, rhs } | Constraint::Lte { lhs, rhs } => {
                lhs.make_substitutions(substitutions);
                rhs.make_substitutions(substitutions);
            }
        }
    }

    fn verify(&self) -> bool {
        match self {
            Constraint::Equal { lhs, rhs } => {
                if let (Some(lhs), Some(rhs)) = (lhs.as_const(), rhs.as_const()) {
                    return lhs == rhs;
                }
            },
            Constraint::Lte { lhs, rhs } => {
                if let (Some(lhs), Some(rhs)) = (lhs.as_const(), rhs.as_const()) {
                    if let (Const::Int { lit: lhs, ty: lhs_ty }, Const::Int { lit: rhs, ty: rhs_ty }) = (lhs, rhs) {
                        assert_eq!(lhs_ty, rhs_ty, "types must be equal");
                        return lhs <= rhs;
                    }
                }
            }
        }

        true
    }
}

impl Driver {
    fn find_leaves(&self, func: &Function, leaves: &mut HashSet<BlockId>) -> bool {
        let mut found_any = false;
        for &block_id in &func.blocks {
            if leaves.contains(&block_id) { continue; }

            let block = &self.code.blocks[block_id];
            let terminal = block.ops.last().copied().unwrap();
            let terminal = self.code.ops[terminal].as_mir_instr().unwrap();
            let is_leaf = match terminal {
                Instr::Br(block) => leaves.contains(block),
                Instr::CondBr { true_bb, false_bb, .. } => leaves.contains(true_bb) && leaves.contains(false_bb),
                _ => true,
            };
            if is_leaf {
                leaves.insert(block_id);
                found_any = true;
            }
        }
        found_any
    }

    fn check_no_loops(&self, func: &Function) {
        let mut leaves = HashSet::new();
        while func.blocks.len() > leaves.len() {
            assert!(self.find_leaves(func, &mut leaves), "Function has a loop!");
        }
    }

    pub fn refine(&mut self) {
        for func_id in self.code.mir_code.functions.indices() {
            self.refine_func(&FunctionRef::Id(func_id));
        }
    }

    fn simplify_and_validate_constraints(&self, substitutions: &mut HashMap<OpId, ConstraintValue>, constraints: &mut Vec<Constraint>) {
        constraints.retain(|constraint| {
            match constraint {
                Constraint::Equal { lhs, rhs } => {
                    // TODO: Opposite way
                    if let &ConstraintValue::Instr(instr) = lhs {
                        substitutions.insert(instr, rhs.clone());
                        return false;
                    }
                }
                _ => {},
            }
            true
        });

        for constraint in constraints.iter_mut() {
            constraint.make_substitutions(&substitutions);
        }

        for constraint in constraints.iter() {
            assert!(constraint.verify(), "Constraint failed:\n{:#?}", constraint);
        }
    }

    pub fn refine_func(&mut self, func_ref: &FunctionRef) {
        let func = function_by_ref(&self.code.mir_code, &func_ref);
        self.check_no_loops(func);
        assert_eq!(func.blocks.len(), 1, "Function has more than one block, which isn't yet supported");

        let mut constraints = Vec::new();
        let mut substitutions = HashMap::new();

        let block = func.blocks[0];
        let block = &self.code.blocks[block];
        for &op in &block.ops {
            let instr = self.code.ops[op].as_mir_instr().unwrap();
            match instr {
                Instr::Const(konst) => constraints.push(Constraint::Equal { lhs: ConstraintValue::Instr(op), rhs: ConstraintValue::Constant(konst.clone()) }),
                Instr::Intrinsic { arguments, intr, ty } => {
                    if matches!(intr, Intrinsic::Add) && ty == &Type::i32() && arguments.len() == 2 {
                        constraints.push(
                            Constraint::Lte {
                                lhs: ConstraintValue::Instr(arguments[0]),
                                rhs: ConstraintValue::Diff {
                                    lhs: Box::new(ConstraintValue::Constant(Const::Int { lit: i32::MAX as u64, ty: Type::i32() })),
                                    rhs: Box::new(ConstraintValue::Instr(arguments[1])),
                                }
                            }
                        )
                    }
                },
                _ => {}
            }
            self.simplify_and_validate_constraints(&mut substitutions, &mut constraints);
        }
    }
}