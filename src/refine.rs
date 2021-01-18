use std::collections::{HashSet, HashMap};
use std::ops::{Add, Sub, Neg};
use std::iter::Iterator;

use rsmt2::prelude::*;
use rsmt2::parse::{IdentParser, ModelParser};
use arrayvec::ArrayVec;
use display_adapter::display_adapter;

use mire::{BlockId, OpId};
use mire::mir::{Const, Function, FuncId, Instr};
use mire::ty::{Type, IntWidth};
use mire::hir::{Intrinsic, Expr, Decl, ExprId};

use crate::typechecker::type_provider::TypeProvider;
use crate::interpreter::Value;
use crate::mir::{FunctionRef, function_by_ref};
use crate::driver::Driver;

// TODO: Switch to another smt crate, or write my own.
// ugh why do I have to do this
#[derive(Clone, Copy)]
struct Parser;

impl<'a> IdentParser<String, String, & 'a str> for Parser {
    fn parse_ident(self, input: &'a str) -> SmtRes<String> {
        Ok(input.into())
    }
    fn parse_type(self, input: &'a str) -> SmtRes<String> {
        Ok(input.into())
    }
}

impl<'a> ModelParser<String, String, String, & 'a str> for Parser {
    fn parse_value(self, input: &'a str, _ident: &String, _params: &[(String, String)], _typ: &String) -> SmtRes<String> {
        Ok(input.into())
    }
}

struct Conditions {
    preconditions: Vec<Constraint>,
    postconditions: Vec<Constraint>,
}

#[derive(Copy, Clone)]
enum Pointee {
    None,
    Unknown,
    Known(OpId),
}

impl Default for Pointee {
    fn default() -> Self { Pointee::None }
}

#[derive(Default)]
struct PointerTypestate {
    last_pointee: Pointee,
}

#[derive(Default)]
pub struct Refine {
    constraints: HashMap<OpId, OpConstraints>,
    conditions: HashMap<FuncId, Conditions>,
    name_gen: NameGen,
    pointer_typestate: HashMap<OpId, PointerTypestate>,
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ConstraintValue {
    Str(String),
    Op(OpId),
    Add(Box<ConstraintValue>, Box<ConstraintValue>),
    Sub(Box<ConstraintValue>, Box<ConstraintValue>),
    Neg(Box<ConstraintValue>),
    Parameter { index: usize, },
    ReturnValue,
}

impl ConstraintValue {
    fn replace(&self, key: &ConstraintValue, value: OpId) -> Self {
        if self == key {
            ConstraintValue::Op(value)
        } else {
            match self {
                ConstraintValue::Str(_) | ConstraintValue::Op(_) | ConstraintValue::Parameter { .. } | ConstraintValue::ReturnValue => self.clone(),
                ConstraintValue::Add(l, r) => ConstraintValue::Add(
                    Box::new(l.replace(key, value)),
                    Box::new(r.replace(key, value)),
                ),
                ConstraintValue::Sub(l, r) => ConstraintValue::Sub(
                    Box::new(l.replace(key, value)),
                    Box::new(r.replace(key, value)),
                ),
                ConstraintValue::Neg(val) => ConstraintValue::Neg(Box::new(val.replace(key, value))),
            }
        }
    }

    fn get_involved_ops(&self) -> Vec<OpId> {
        match self {
            &ConstraintValue::Op(op) => vec![op],
            ConstraintValue::Str(_) | ConstraintValue::ReturnValue => vec![],
            ConstraintValue::Add(l, r) | ConstraintValue::Sub(l, r) => {
                l.get_involved_ops().into_iter()
                    .chain(r.get_involved_ops().into_iter())
                    .collect()
            },
            ConstraintValue::Neg(val) => val.get_involved_ops(),
            ConstraintValue::Parameter { .. } => panic!("can't get involved ops of a parameter, which is supposed to be substituted for an Op"),
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ConstraintValueOrigin {
    Pointer {
        /// Pointer to the start of the allocation
        start: ConstraintValue,
        /// Integer offset from the start of the allocation
        offset: ConstraintValue,
        /// Integer length of the allocation in bytes
        len: ConstraintValue,
    },
}

impl ConstraintValueOrigin {
    fn replace(&self, key: &ConstraintValue, value: OpId) -> Self {
        match self {
            ConstraintValueOrigin::Pointer { start, offset, len } => ConstraintValueOrigin::Pointer {
                start: start.replace(key, value),
                offset: offset.replace(key, value),
                len: len.replace(key, value),
            }
        }
    }

    fn get_involved_ops(&self) -> Vec<OpId> {
        match self {
            ConstraintValueOrigin::Pointer { start, offset, len } => {
                [start, offset, len]
                    .iter()
                    .map(|val| val.get_involved_ops())
                    .flatten()
                    .collect()
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum Constraint {
    Gte(ConstraintValue, ConstraintValue),
    Lte(ConstraintValue, ConstraintValue),
    Eq(ConstraintValue, ConstraintValue),
    OriginatesFrom(ConstraintValue, ConstraintValueOrigin),
}

impl Constraint {
    fn replace(&self, key: &ConstraintValue, value: OpId) -> Self {
        match self {
            Constraint::Gte(l, r) => Constraint::Gte(l.replace(key, value), r.replace(key, value)),
            Constraint::Lte(l, r) => Constraint::Lte(l.replace(key, value), r.replace(key, value)),
            Constraint::Eq(l, r) => Constraint::Eq(l.replace(key, value), r.replace(key, value)),
            Constraint::OriginatesFrom(val, origin) => Constraint::OriginatesFrom(val.replace(key, value), origin.replace(key, value)),
        }
    }

    fn get_involved_ops(&self) -> HashSet<OpId> {
        match self {
            Constraint::Gte(l, r) | Constraint::Lte(l, r) | Constraint::Eq(l, r) => {
                [l, r].iter().map(|val| val.get_involved_ops())
                    .flatten()
                    .collect()
            },
            Constraint::OriginatesFrom(val, origin) => {
                [val.get_involved_ops(), origin.get_involved_ops()]
                    .iter()
                    .flatten()
                    .copied()
                    .collect()
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
    fn expr_to_constraint_val(&self, expr: ExprId, tp: &impl TypeProvider) -> ConstraintValue {
        let expr = &self.code.hir_code.exprs[expr];
        match expr {
            &Expr::IntLit { lit } => ConstraintValue::Str(lit.to_string()),
            &Expr::DeclRef { ref arguments, id } => {
                let overload = tp.selected_overload(id).unwrap();
                match self.code.hir_code.decls[overload] {
                    Decl::Const(expr) => self.expr_to_constraint_val(expr, tp),
                    Decl::Parameter { index } => ConstraintValue::Parameter { index },
                    Decl::ReturnValue => ConstraintValue::ReturnValue,
                    Decl::Intrinsic { intr, .. } => {
                        match intr {
                            Intrinsic::Add => {
                                assert_eq!(arguments.len(), 2);
                                ConstraintValue::Add(
                                    Box::new(self.expr_to_constraint_val(arguments[0], tp)),
                                    Box::new(self.expr_to_constraint_val(arguments[1], tp)),
                                )
                            },
                            Intrinsic::Sub => {
                                assert_eq!(arguments.len(), 2);
                                ConstraintValue::Sub(
                                    Box::new(self.expr_to_constraint_val(arguments[0], tp)),
                                    Box::new(self.expr_to_constraint_val(arguments[1], tp)),
                                )
                            },
                            Intrinsic::Neg => {
                                assert_eq!(arguments.len(), 1);
                                ConstraintValue::Neg(
                                    Box::new(self.expr_to_constraint_val(arguments[0], tp)),
                                )
                            },
                            _ => panic!("Unsupported intrinsic call in condition attribute"),
                        }
                    },
                    _ => panic!("Unsupported decl in condition attribute"),
                }
            },
            _ => panic!("Unsupported expression kind in condition attribute"),
        }
    }

    fn expr_to_constraint(&self, expr: ExprId, tp: &impl TypeProvider) -> Constraint {
        let expr = &self.code.hir_code.exprs[expr];
        match expr {
            &Expr::DeclRef { ref arguments, id } => {
                let overload = tp.selected_overload(id).unwrap();
                match self.code.hir_code.decls[overload] {
                    Decl::Intrinsic { intr, .. } => {
                        assert_eq!(arguments.len(), 2);
                        match intr {
                            Intrinsic::LessOrEq => Constraint::Lte(
                                self.expr_to_constraint_val(arguments[0], tp),
                                self.expr_to_constraint_val(arguments[1], tp),
                            ),
                            Intrinsic::GreaterOrEq => Constraint::Gte(
                                self.expr_to_constraint_val(arguments[0], tp),
                                self.expr_to_constraint_val(arguments[1], tp),
                            ),
                            Intrinsic::Eq => Constraint::Eq(
                                self.expr_to_constraint_val(arguments[0], tp),
                                self.expr_to_constraint_val(arguments[1], tp),
                            ),
                            _ => panic!("Unsupported intrinsic call in condition attribute"),
                        }
                    },
                    _ => panic!("Unsupported decl in condition attribute"),
                }
            },
            _ => panic!("Unsupported expression kind in condition attribute"),
        }
    }

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

    pub fn refine(&mut self, tp: &impl TypeProvider) {
        for func_id in self.code.mir_code.functions.indices() {
            self.refine_func(&FunctionRef::Id(func_id), tp);
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
    fn start_constraints(&mut self, op: OpId) {
        let name = self.refine.name_gen.next().unwrap();
        self.refine.constraints.entry(op).or_insert(OpConstraints::new(name));
    }

    #[display_adapter]
    fn display_constraint_value(&self, val: &ConstraintValue, f: &mut Formatter) {
        match val {
            ConstraintValue::Op(op) => write!(f, "{}", self.refine.constraints.get(op).unwrap().name)?,
            ConstraintValue::Str(str) => write!(f, "{}", str)?,
            ConstraintValue::Add(l, r) => write!(f, "{}", self.display_bin_expr("+", l, r))?,
            ConstraintValue::Sub(l, r) => write!(f, "{}", self.display_bin_expr("-", l, r))?,
            ConstraintValue::Neg(val) => write!(f, "(- {})", self.display_constraint_value(val))?,
            ConstraintValue::ReturnValue => write!(f, "return_value")?,
            ConstraintValue::Parameter { .. } => panic!("Can't print parameter, which must be replaced with an Op"),
        }
        Ok(())
    }

    #[display_adapter]
    fn display_bin_expr(&self, constraint: &str, l: &ConstraintValue, r: &ConstraintValue, f: &mut Formatter) {
        write!(f, "({} {} {})", constraint, self.display_constraint_value(l), self.display_constraint_value(r))
    }

    #[display_adapter]
    fn display_constraint(&self, constraint: &Constraint, f: &mut Formatter) {
        match constraint {
            Constraint::Gte(l, r) => write!(f, "{}", self.display_bin_expr(">=", &l, &r)),
            Constraint::Lte(l, r) => write!(f, "{}", self.display_bin_expr("<=", &l, &r)),
            Constraint::Eq(l, r) => write!(f, "{}", self.display_bin_expr("=", &l, &r)),
            Constraint::OriginatesFrom(_, _) => Ok(()),
        }
    }

    #[display_adapter]
    fn display_condition_check(&self, preconditions: &[Constraint], variables: &HashSet<OpId>, constraints: &HashSet<Constraint>, w: &mut Formatter) {
        write!(w, "(forall (")?;
        for &var in variables {
            write!(w, "({} Int) ", self.refine.constraints[&var].name)?;
        }
        write!(w, ") (=> (and ")?;
        for constraint in constraints {
            write!(w, "{} ", self.display_constraint(constraint))?;
        }
        write!(w, ") (and ")?;
        for constraint in preconditions {
            write!(w, "{} ", self.display_constraint(constraint))?;
        }
        write!(w, ")))")?;
        Ok(())
    }

    fn check_conditions(&self, solver: &mut Solver<Parser>, conditions: Vec<Constraint>, constraints: &mut HashSet<Constraint>) {
        if conditions.is_empty() { return; }

        let mut variables = HashSet::new();
        let mut relevant_constraints = HashSet::new();
        for precondition in &conditions {
            variables.extend(precondition.get_involved_ops());
        }
        loop {
            let mut added_anything = false;
            // TODO: this is probably crazy slow
            for constraint in &*constraints {
                let involved = constraint.get_involved_ops();
                for var in &variables {
                    if involved.contains(var) {
                        for var in involved {
                            added_anything |= variables.insert(var);
                        }
                        relevant_constraints.insert(constraint.clone());
                        break;
                    }
                }
            }

            if !added_anything { break; }
        }
        let condition = self.display_condition_check(&conditions, &variables, &relevant_constraints).to_string();
        solver.assert(&condition).unwrap();
    }

    pub fn refine_func(&mut self, func_ref: &FunctionRef, tp: &impl TypeProvider) {
        if let FunctionRef::Id(id) = func_ref {
            if self.refine.conditions.get(id).is_some() { return; }
        }

        let func = function_by_ref(&self.code.mir_code, &func_ref);
        self.check_no_loops(func);
        assert_eq!(func.blocks.len(), 1, "Function has more than one block, which isn't yet supported");

        let mut preconditions = Vec::new();
        let mut postconditions = Vec::new();
        let block_id = func.blocks[0];
        let block = &self.code.blocks[block_id];
        let num_params = self.code.num_parameters(func);
        if let Some(decl) = func.decl {
            if let Some(attributes) = self.code.hir_code.decl_attributes.get(&decl) {
                for attr in attributes {
                    let arg = attr.arg.expect("missing attribute argument");
                    let constraint = self.expr_to_constraint(arg, tp);
                    if attr.attr == self.hir.precondition_sym {
                        preconditions.push(constraint);
                    } else if attr.attr == self.hir.postcondition_sym {
                        postconditions.push(constraint);
                    } else {
                        panic!("Unrecognized attribute");
                    }
                }
            }
        }

        if let &FunctionRef::Id(id) = func_ref {
            self.refine.conditions.insert(
                id,
                Conditions {
                    preconditions: preconditions.clone(),
                    postconditions: postconditions.clone(),
                }
            );
        }
        let replace_parameters = |mut constraint: Constraint| {
            for index in 0..num_params {
                constraint = constraint.replace(&ConstraintValue::Parameter { index }, block.ops[index]);
            }
            constraint
        };
        let mut constraints: HashSet<Constraint> = preconditions.into_iter().map(replace_parameters).collect();
        let postconditions: HashSet<Constraint> = postconditions.into_iter().map(replace_parameters).collect();

        let conf = SmtConf::default_z3();
        let mut solver = conf.spawn(Parser).unwrap();

        for i in 0..block.ops.len() {
            let block = &self.code.blocks[block_id];
            let op = block.ops[i];
            let instr = self.code.ops[op].as_mir_instr().unwrap();
            match instr {
                Instr::Parameter(ty) => {
                    match ty {
                        &Type::Int { width, is_signed } => {
                            let (lo, hi) = self.get_int_range(width, is_signed);
                            self.start_constraints(op);
                            constraints.insert(Constraint::Lte(lo.into(), op.into()));
                            constraints.insert(Constraint::Lte(op.into(), hi.into()));
                        },
                        _ => {},
                    }
                },
                Instr::Const(konst) => {
                    match konst {
                        Const::Int { ty, .. } => {
                            match ty {
                                &Type::Int { is_signed, .. } => {
                                    // TODO: this is kind of silly, but it was the path of least resistance.
                                    let val = Value::from_const(konst, self).as_big_int(is_signed);
                                    self.start_constraints(op);
                                    constraints.insert(Constraint::Eq(op.into(), val.to_string().into()));
                                },
                                unhandled => panic!("Literal with unhandled type {:?}", unhandled),
                            }
                        },
                        &Const::Str { id, .. } => {
                            let len = self.code.mir_code.strings[id].as_bytes_with_nul().len().to_string();
                            self.start_constraints(op);
                            constraints.insert(
                                Constraint::OriginatesFrom(op.into(), ConstraintValueOrigin::Pointer { start: op.into(), offset: String::from("0").into(), len: len.into() })
                            );
                        }
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
                                    let (a, b) = (arguments[0], arguments[1]);
                                    self.start_constraints(op);
                                    let sum = ConstraintValue::from(a) + ConstraintValue::from(b);
                                    self.check_conditions(
                                        &mut solver,
                                        vec![
                                            Constraint::Lte(lo.into(), sum.clone()),
                                            Constraint::Lte(sum.clone(), hi.into()),
                                        ],
                                        &mut constraints,
                                    );
                                    constraints.insert(Constraint::Eq(op.into(), sum));
                                },
                                _ => panic!("unhandled type"),
                            }
                        },
                        Intrinsic::Sub => {
                            assert_eq!(arguments.len(), 2);
                            match ty {
                                &Type::Int { width, is_signed } => {
                                    let (lo, hi) = self.get_int_range(width, is_signed);
                                    let (a, b) = (arguments[0], arguments[1]);
                                    self.start_constraints(op);
                                    let diff = ConstraintValue::from(a) - ConstraintValue::from(b);
                                    self.check_conditions(
                                        &mut solver,
                                        vec![
                                            Constraint::Lte(lo.into(), diff.clone()),
                                            Constraint::Lte(diff.clone(), hi.into()),
                                        ],
                                        &mut constraints,
                                    );
                                    constraints.insert(Constraint::Eq(op.into(), diff));
                                },
                                _ => panic!("unhandled type"),
                            }
                        },
                        Intrinsic::Neg => {
                            assert_eq!(arguments.len(), 1);
                            match ty {
                                &Type::Int { width, is_signed } => {
                                    let (lo, hi) = self.get_int_range(width, is_signed);
                                    let a = arguments[0];
                                    self.start_constraints(op);
                                    let neg = -ConstraintValue::from(a);
                                    self.check_conditions(
                                        &mut solver,
                                        vec![
                                            Constraint::Lte(lo.into(), neg.clone()),
                                            Constraint::Lte(neg.clone(), hi.into()),
                                        ],
                                        &mut constraints,
                                    );

                                    constraints.insert(Constraint::Eq(op.into(), neg));
                                },
                                _ => panic!("unhandled type"),
                            }
                        },
                        Intrinsic::Malloc => {
                            assert_eq!(arguments.len(), 1);
                            let len = arguments[0];
                            self.start_constraints(op);
                            constraints.insert(
                                Constraint::OriginatesFrom(op.into(), ConstraintValueOrigin::Pointer { start: op.into(), offset: String::from("0").into(), len: len.into() })
                            );
                        }
                        _ => {},
                    }
                },
                &Instr::Ret(val) => {
                    let postconditions = postconditions.iter()
                        .map(|condition| condition.replace(&ConstraintValue::ReturnValue, val))
                        .collect();
                    self.check_conditions(&mut solver, postconditions, &mut constraints);
                },
                &Instr::Call { ref arguments, func } => {
                    // Borrow checker, argh...
                    let arguments = arguments.clone();

                    self.refine_func(&FunctionRef::Id(func), tp);
                    self.start_constraints(op);

                    let conditions = self.refine.conditions.get(&func).unwrap();
                    let replace_params = |mut condition: Constraint| {
                        for (i, &arg) in arguments.iter().enumerate() {
                            condition = condition.replace(&ConstraintValue::Parameter { index: i }, arg);
                        }
                        condition
                    };
                    let preconditions: Vec<Constraint> = conditions.preconditions.iter().cloned().map(replace_params).collect();
                    self.check_conditions(&mut solver, preconditions, &mut constraints);
                    let postconditions = conditions.postconditions.iter().cloned().map(replace_params);
                    for mut condition in postconditions {
                        condition = condition.replace(&ConstraintValue::ReturnValue, op);
                        constraints.insert(condition);
                    }
                },
                &Instr::Store { location, value } => {
                    self.refine.pointer_typestate.entry(location).or_default().last_pointee = Pointee::Known(value);
                },
                &Instr::Load(location) => {
                    let pointee = self.refine.pointer_typestate
                        .get(&location)
                        .map(|ts| ts.last_pointee)
                        .unwrap_or_default();
                    match pointee {
                        Pointee::None => panic!("can't read from uninitialized memory!"),
                        Pointee::Unknown => {},
                        Pointee::Known(pointee) => {
                            self.start_constraints(op);
                            constraints = constraints.into_iter().flat_map(|constraint| {
                                let mut res: ArrayVec<[Constraint; 2]> = ArrayVec::new();
                                if constraint.get_involved_ops().contains(&pointee) {
                                    res.push(constraint.replace(&ConstraintValue::Op(pointee), op));
                                }
                                res.push(constraint);
                                res
                            }).collect();
                        }
                    }
                },
                Instr::Alloca(ty) => {
                    let len = self.size_of(ty).to_string();
                    self.start_constraints(op);
                    constraints.insert(
                        Constraint::OriginatesFrom(op.into(), ConstraintValueOrigin::Pointer { start: op.into(), offset: String::from("0").into(), len: len.into() })
                    );
                }
                &Instr::AddressOfStatic(statik) => {
                    // Static variables are not uninitialized, but we can't be sure what their values are
                    self.refine.pointer_typestate.entry(op).or_default().last_pointee = Pointee::Unknown;

                    let ty = self.code.mir_code.statics[statik].ty();
                    let len = self.size_of(&ty).to_string();
                    self.start_constraints(op);
                    constraints.insert(
                        Constraint::OriginatesFrom(op.into(), ConstraintValueOrigin::Pointer { start: op.into(), offset: String::from("0").into(), len: len.into() })
                    );
                },
                _ => {},
            }
        }
        assert!(solver.check_sat().unwrap(), "SAT failed! :(");
    }
}