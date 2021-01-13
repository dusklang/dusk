use std::collections::{HashSet, HashMap};
use std::ops::{Add, Sub, Neg};
use std::iter::Iterator;

use rsmt2::prelude::*;
use display_adapter::display_adapter;

use mire::{BlockId, OpId};
use mire::mir::{Const, Function, Instr};
use mire::ty::{Type, IntWidth};
use mire::hir::Intrinsic;

use crate::interpreter::Value;
use crate::mir::{FunctionRef, function_by_ref};
use crate::driver::Driver;

#[derive(Default)]
pub struct Refine {
    constraints: HashMap<OpId, OpConstraints>,
    name_gen: NameGen,
}

#[derive(Default)]
struct NameGen(usize);

impl Iterator for NameGen {
    type Item = String;
    fn next(&mut self) -> Option<String> {
        let mut val = self.0;
        let mut out = String::new();
        loop {
            let base = 26;
            let cur_digit = val % base;
            val /= base;
            let chr = ('a' as u8 + cur_digit as u8) as char;
            out.push(chr);

            if val == 0 { break; }
        }

        self.0 += 1;

        Some(out)
    }
}

#[derive(Debug, Clone)]
enum ConstraintValue {
    Str(String),
    Op(OpId),
    Add(Box<ConstraintValue>, Box<ConstraintValue>),
    Sub(Box<ConstraintValue>, Box<ConstraintValue>),
    Neg(Box<ConstraintValue>),
}

impl ConstraintValue {
    fn replace(&self, key: OpId, value: OpId) -> Self {
        match self {
            ConstraintValue::Str(str) => ConstraintValue::Str(str.clone()),
            &ConstraintValue::Op(op) => ConstraintValue::Op(op),
            ConstraintValue::Add(l, r) => ConstraintValue::Add(
                Box::new(l.replace(key, value)),
                Box::new(r.replace(key, value)),
            ),
            ConstraintValue::Sub(l, r) => ConstraintValue::Sub(
                Box::new(l.replace(key, value)),
                Box::new(r.replace(key, value)),
            ),
            ConstraintValue::Neg(val) => ConstraintValue::Neg(Box::new(val.replace(key, value)))
        }
    }

    fn get_involved_ops(&self) -> Vec<OpId> {
        match self {
            &ConstraintValue::Op(op) => vec![op],
            ConstraintValue::Str(_) => vec![],
            ConstraintValue::Add(l, r) | ConstraintValue::Sub(l, r) => {
                l.get_involved_ops().into_iter()
                    .chain(r.get_involved_ops().into_iter())
                    .collect()
            },
            ConstraintValue::Neg(val) => val.get_involved_ops(),
        }
    }
}

impl<T: ToOwned<Owned=String>> From<&T> for ConstraintValue {
    fn from(value: &T) -> Self {
        ConstraintValue::Str(value.to_owned())
    }
}

impl From<String> for ConstraintValue {
    fn from(value: String) -> Self {
        ConstraintValue::Str(value)
    }
}

impl From<OpId> for ConstraintValue {
    fn from(value: OpId) -> Self {
        ConstraintValue::Op(value)
    }
}

impl<T: ToOwned<Owned=ConstraintValue>> Add<T> for ConstraintValue {
    type Output = ConstraintValue;

    fn add(self, rhs: T) -> Self::Output {
        ConstraintValue::Add(Box::new(self), Box::new(rhs.to_owned()))
    }
}

impl<T: ToOwned<Owned=ConstraintValue>> Add<T> for &ConstraintValue {
    type Output = ConstraintValue;

    fn add(self, rhs: T) -> Self::Output {
        ConstraintValue::Add(Box::new(self.clone()), Box::new(rhs.to_owned()))
    }
}

impl<T: ToOwned<Owned=ConstraintValue>> Sub<T> for ConstraintValue {
    type Output = ConstraintValue;

    fn sub(self, rhs: T) -> Self::Output {
        ConstraintValue::Sub(Box::new(self), Box::new(rhs.to_owned()))
    }
}

impl<T: ToOwned<Owned=ConstraintValue>> Sub<T> for &ConstraintValue {
    type Output = ConstraintValue;

    fn sub(self, rhs: T) -> Self::Output {
        ConstraintValue::Sub(Box::new(self.clone()), Box::new(rhs.to_owned()))
    }
}

impl Neg for ConstraintValue {
    type Output = ConstraintValue;

    fn neg(self) -> Self::Output {
        ConstraintValue::Neg(Box::new(self))
    }
}

impl Neg for &ConstraintValue {
    type Output = ConstraintValue;

    fn neg(self) -> Self::Output {
        ConstraintValue::Neg(Box::new(self.clone()))
    }
}

#[derive(Debug, Clone)]
enum Constraint {
    Gte(ConstraintValue, ConstraintValue),
    Lte(ConstraintValue, ConstraintValue),
}

impl Constraint {
    fn replace(&self, key: OpId, value: OpId) -> Self {
        match self {
            Constraint::Gte(l, r) => Constraint::Gte(l.replace(key, value), r.replace(key, value)),
            Constraint::Lte(l, r) => Constraint::Lte(l.replace(key, value), r.replace(key, value)),
        }
    }

    fn get_involved_ops(&self) -> impl Iterator<Item=OpId> {
        match self {
            Constraint::Gte(l, r) | Constraint::Lte(l, r) => {
                l.get_involved_ops().into_iter()
                    .chain(r.get_involved_ops())
            }
        }
    }
}

#[derive(Debug)]
struct OpConstraints {
    // Name given to this op in the SMT solver
    name: String,
    /// All constraints involving this op
    constraints: Vec<Constraint>,
}

impl OpConstraints {
    fn new(name: String) -> Self {
        OpConstraints {
            name,
            constraints: Vec::new(),
        }
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

    fn get_int_range(&self, width: IntWidth, is_signed: bool) -> (String, String) {
        assert!(self.arch.pointer_size() == 64);
        match (is_signed, width) {
            (false, IntWidth::W8)  => (u8::MIN.to_string(),  u8::MAX.to_string()),
            (false, IntWidth::W16) => (u16::MIN.to_string(), u16::MAX.to_string()),
            (false, IntWidth::W32) => (u32::MIN.to_string(), u32::MAX.to_string()),
            (false, IntWidth::W64) | (false, IntWidth::Pointer) => (u64::MIN.to_string(), u64::MAX.to_string()),

            (true, IntWidth::W8)  => (i8::MIN.to_string(),  i8::MAX.to_string()),
            (true, IntWidth::W16) => (i16::MIN.to_string(), i16::MAX.to_string()),
            (true, IntWidth::W32) => (i32::MIN.to_string(), i32::MAX.to_string()),
            (true, IntWidth::W64) | (true, IntWidth::Pointer) => (i64::MIN.to_string(), i64::MAX.to_string()),
        }
    }

    /// Returns the generated name
    fn start_constraints(&mut self, op: OpId) -> &str {
        let name = self.refine.name_gen.next().unwrap();
        &self.refine.constraints.entry(op)
            .or_insert(OpConstraints::new(name))
            .name
    }

    #[display_adapter]
    fn display_constraint_value(&self, val: &ConstraintValue, f: &mut Formatter) {
        match val {
            ConstraintValue::Op(op) => write!(f, "{}", self.refine.constraints.get(op).unwrap().name)?,
            ConstraintValue::Str(str) => write!(f, "{}", str)?,
            ConstraintValue::Add(l, r) => write!(f, "{}", self.display_bin_expr("+", l, r))?,
            ConstraintValue::Sub(l, r) => write!(f, "{}", self.display_bin_expr("-", l, r))?,
            ConstraintValue::Neg(val) => write!(f, "(- {})", self.display_constraint_value(val))?,
        }
        Ok(())
    }

    #[display_adapter]
    fn display_bin_expr(&self, constraint: &str, l: &ConstraintValue, r: &ConstraintValue, f: &mut Formatter) {
        write!(f, "({} {} {})", constraint, self.display_constraint_value(l), self.display_constraint_value(r))
    }

    /// Returns the constraint in S-expression form
    fn add_constraint(&mut self, constraint: Constraint) -> String {
        for op in constraint.get_involved_ops() {
            let constraints = self.refine.constraints.get_mut(&op).unwrap();
            constraints.constraints.push(constraint.clone());
        }

        match constraint {
            Constraint::Gte(l, r) => self.display_bin_expr(">=", &l, &r).to_string(),
            Constraint::Lte(l, r) => self.display_bin_expr("<=", &l, &r).to_string(),
        }
    }

    pub fn refine_func(&mut self, func_ref: &FunctionRef) {
        let func = function_by_ref(&self.code.mir_code, &func_ref);
        self.check_no_loops(func);
        assert_eq!(func.blocks.len(), 1, "Function has more than one block, which isn't yet supported");

        let block_id = func.blocks[0];

        let parser = ();

        let conf = SmtConf::default_z3();
        let mut solver = conf.spawn(parser).unwrap();

        let is_sat = solver.check_sat().unwrap();
        assert!(is_sat);

        let block = &self.code.blocks[block_id];
        for i in 0..block.ops.len() {
            let block = &self.code.blocks[block_id];
            let op = block.ops[i];
            let instr = self.code.ops[op].as_mir_instr().unwrap();
            match instr {
                Instr::Const(konst) => {
                    match konst {
                        Const::Int { ty, .. } => {
                            match ty {
                                &Type::Int { is_signed, .. } => {
                                    let val = Value::from_const(konst, self).as_big_int(is_signed);
                                    let name = self.start_constraints(op);
                                    // TODO: this is kind of silly, but it was the path of least resistance.
                                    solver.define_const(name, "Int", &val.to_string()).unwrap();
                                },
                                unhandled => panic!("Literal with unhandled type {:?}", unhandled),
                            }
                        },
                        _ => {},
                    }
                },
                Instr::Intrinsic { arguments, ty, intr } => {
                    match intr {
                        Intrinsic::Add => {
                            assert_eq!(arguments.len(), 2);
                            match ty {
                                &Type::Int { width, is_signed } => {
                                    let (lo, hi) = self.get_int_range(width, is_signed);
                                    let (a, b) = (&self.refine.constraints[&arguments[0]].name, &self.refine.constraints[&arguments[1]].name);
                                    let add_expr = format!("(+ {} {})", a, b);
                                    let name = self.start_constraints(op);
                                    solver.define_const(name, "Int", &add_expr).unwrap();
                                    let constraint = self.add_constraint(Constraint::Gte(op.into(), lo.into()));
                                    solver.assert(&constraint).unwrap();
                                    let constraint = self.add_constraint(Constraint::Lte(op.into(), hi.into()));
                                    solver.assert(&constraint).unwrap();
                                },
                                _ => panic!("unhandled type"),
                            }
                        },
                        Intrinsic::Sub => {
                            assert_eq!(arguments.len(), 2);
                            match ty {
                                &Type::Int { width, is_signed } => {
                                    let (lo, hi) = self.get_int_range(width, is_signed);
                                    let (a, b) = (&self.refine.constraints[&arguments[0]].name, &self.refine.constraints[&arguments[1]].name);
                                    let sub_expr = format!("(- {} {})", a, b);
                                    let name = self.start_constraints(op);
                                    solver.define_const(name, "Int", &sub_expr).unwrap();
                                    let constraint = self.add_constraint(Constraint::Gte(op.into(), lo.into()));
                                    solver.assert(&constraint).unwrap();
                                    let constraint = self.add_constraint(Constraint::Lte(op.into(), hi.into()));
                                    solver.assert(&constraint).unwrap();
                                },
                                _ => panic!("unhandled type"),
                            }
                        },
                        Intrinsic::Neg => {
                            assert_eq!(arguments.len(), 1);
                            match ty {
                                &Type::Int { width, is_signed } => {
                                    let (lo, hi) = self.get_int_range(width, is_signed);
                                    let a = &self.refine.constraints[&arguments[0]].name;
                                    let neg_expr = format!("(- {})", a);
                                    let name = self.start_constraints(op);
                                    solver.define_const(name, "Int", &neg_expr).unwrap();
                                    let constraint = self.add_constraint(Constraint::Gte(op.into(), lo.into()));
                                    solver.assert(&constraint).unwrap();
                                    let constraint = self.add_constraint(Constraint::Lte(op.into(), hi.into()));
                                    solver.assert(&constraint).unwrap();
                                },
                                _ => panic!("unhandled type"),
                            }
                        }
                        _ => {},
                    }
                },
                _ => {},
            }
        }

        assert!(solver.check_sat().unwrap(), "SAT failed! :(");
    }
}