use std::collections::{HashSet, HashMap};
use std::ops::{Add, Sub, Neg};
use std::iter::Iterator;

use rsmt2::prelude::*;
use rsmt2::parse::{IdentParser, ModelParser};
use arrayvec::ArrayVec;
use display_adapter::display_adapter;
use string_interner::DefaultSymbol as Sym;

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
    Unknown,
    Known(OpId),
}

struct PointeeRegion {
    pointee: Pointee,
    offset: ConstraintValue,
    len: ConstraintValue,
}

#[derive(Default)]
struct PointerTypestate {
    regions: Vec<PointeeRegion>,
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
        /// Pointer to the base of the allocation
        base: ConstraintValue,
        /// Integer offset from the base of the allocation
        offset: ConstraintValue,
        /// Integer length of the allocation in bytes
        len: ConstraintValue,
    },
}

impl ConstraintValueOrigin {
    /// Creates a pointer origin with offset of 0
    fn new_pointer(base: impl Into<ConstraintValue>, len: impl Into<ConstraintValue>) -> Self {
        ConstraintValueOrigin::Pointer { base: base.into(), offset: String::from("0").into(), len: len.into() }
    }

    fn replace(&self, key: &ConstraintValue, value: OpId) -> Self {
        match self {
            ConstraintValueOrigin::Pointer { base, offset, len } => ConstraintValueOrigin::Pointer {
                base: base.replace(key, value),
                offset: offset.replace(key, value),
                len: len.replace(key, value),
            }
        }
    }

    fn get_involved_ops(&self) -> Vec<OpId> {
        match self {
            ConstraintValueOrigin::Pointer { base, offset, len } => {
                [base, offset, len]
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
    fn display_condition_check(&self, condition: &Constraint, constraints: &HashSet<Constraint>, w: &mut Formatter) {
        write!(w, "(not ")?;
        if constraints.is_empty() {
            write!(w, "{}", self.display_constraint(condition))?;
        } else {
            write!(w, "(=> (and ")?;
            for constraint in constraints {
                write!(w, "{} ", self.display_constraint(constraint))?;
            }
            write!(w, ") {})", self.display_constraint(condition))?;
        }
        write!(w, ")")
    }

    fn check_conditions(&self, solver: &mut Solver<Parser>, func_name: Option<Sym>, conditions: Vec<Constraint>, constraints: &HashSet<Constraint>) -> Result<(), Vec<(String, Vec<(String, String)>, String, String)>> {
        for condition in conditions {
            let mut variables = HashSet::new();
            let mut relevant_constraints = HashSet::new();
            variables.extend(condition.get_involved_ops());
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
            solver.push(1).unwrap();
            let condition_str: String = format!("\n    {}", self.display_condition_check(&condition, &relevant_constraints));
            for var in &variables {
                solver.declare_const(&self.refine.constraints.get(var).unwrap().name, "Int").unwrap();
            }
            solver.assert(&condition_str).unwrap();
            let val = if solver.check_sat().unwrap() {
                println!("Refinement checker failed on condition {}", self.display_constraint(&condition));
                println!("    in function {}", self.fn_name(func_name));
                let mut model = solver.get_model().unwrap();

                // Limit the returned model to just the variables that show up in the current condition
                model.retain(|assignment| 
                    condition.get_involved_ops().iter()
                    .map(|op| &self.refine.constraints.get(op).unwrap().name)
                    .collect::<Vec<_>>()
                    .contains(&&assignment.0)
                );
                Err(model)
            } else {
                Ok(())
            };
            solver.pop(1).unwrap();
    
            val?;
        }

        Ok(())
    }

    fn origin_of<'a, 'b>(&'a self, val: &'a ConstraintValue, constraints: &'b HashSet<Constraint>) -> Option<&'b ConstraintValueOrigin> {
        let mut matching = constraints.iter().filter_map(|constraint| {
            match constraint {
                Constraint::OriginatesFrom(op_val, origin) if op_val == val => Some(origin),
                _ => None,
            }
        });
        if let Some(origin) = matching.next() {
            assert!(matching.next().is_none(), "can't have multiple OriginatesFrom constraints on the same operation");
            Some(origin)
        } else {
            None
        }
    }

    /// NOTE: clears typestate regions for the entire allocation
    fn set_unknown_pointee(&mut self, pointer: OpId, len: usize) {
        let ts = self.refine.pointer_typestate.entry(pointer).or_default();
        ts.regions.clear();
        ts.regions.push(
            PointeeRegion {
                pointee: Pointee::Unknown,
                offset: "0".to_string().into(),
                len: len.to_string().into(),
            }
        );
    }

    fn add_pointer_write(&mut self, solver: &mut Solver<Parser>, func_name: Option<Sym>, constraints: &HashSet<Constraint>, base: ConstraintValue, offset: ConstraintValue, value: OpId) {
        if let ConstraintValue::Op(location) = base {
            let ty = self.type_of(value);
            let len = ConstraintValue::from(self.size_of(&ty).to_string());
            let mut regions = std::mem::replace(
                &mut self.refine.pointer_typestate.entry(location).or_default().regions,
                Vec::new(),
            );
            let end = offset.clone() + len.clone();
            let one = ConstraintValue::from("1".to_string());
            regions.retain(|region| {
                let region_end = region.offset.clone() + region.len.clone();

                let left_side_in_range = self.check_conditions(
                    solver,
                    func_name,
                    vec![
                        Constraint::Lte(offset.clone(), region.offset.clone()),
                        // TODO: add a Constraint::Lt variant to avoid this nonsense
                        Constraint::Lte(region.offset.clone() + one.clone(), end.clone())
                    ],
                    constraints
                ).is_ok();
                let right_side_in_range = self.check_conditions(
                    solver,
                    func_name,
                    vec![
                        // TODO: add a Constraint::Lt variant to avoid this nonsense
                        Constraint::Lte(offset.clone(), region_end.clone() - one.clone()),
                        Constraint::Lte(region_end, end.clone()),
                    ],
                    constraints
                ).is_ok();

                if left_side_in_range ^ right_side_in_range {
                    panic!("can't write to partially-aliased section of pointer");
                }

                !(left_side_in_range && right_side_in_range)
            });
            regions.push(
                PointeeRegion {
                    pointee: Pointee::Known(value),
                    offset,
                    len,
                }
            );
            self.refine.pointer_typestate.get_mut(&location).unwrap().regions = regions;
        } else {
            panic!("Can't write to pointer that originated from something other than an OpId");
        }
    }

    fn get_pointee(&self, solver: &mut Solver<Parser>, func_name: Option<Sym>, constraints: &HashSet<Constraint>, base: ConstraintValue, offset: ConstraintValue, expected_len: usize) -> Option<Pointee> {
        if let ConstraintValue::Op(location) = base {
            self.refine.pointer_typestate
                .get(&location)
                .map(|ts| {
                    for region in &ts.regions {
                        let matches = self.check_conditions(
                            solver,
                            func_name,
                            vec![
                                Constraint::Eq(offset.clone(), region.offset.clone()),
                                Constraint::Eq(region.len.clone(), expected_len.to_string().into()),
                            ],
                            constraints
                        ).is_ok();

                        if matches {
                            return Some(region.pointee);
                        }
                    }
                    None
                })
                .unwrap_or_default()
        } else {
            None
        }
    }

    fn propagate_ptr_origin_through_add(&self, solver: &mut Solver<Parser>, func_name: Option<Sym>, constraints: &mut HashSet<Constraint>, ptr_origin: ConstraintValueOrigin, scalar: ConstraintValue, op: OpId) {
        match ptr_origin {
            ConstraintValueOrigin::Pointer { base, offset, len } => {
                let sum = offset + scalar;
                if self.check_conditions(
                    solver,
                    func_name,
                    vec![
                        Constraint::Lte(sum.clone(), len.clone())
                    ],
                    &constraints,
                ).is_ok() {
                    constraints.insert(
                        Constraint::OriginatesFrom(
                            op.into(),
                            ConstraintValueOrigin::Pointer {
                                base,
                                offset: sum,
                                len: len.clone()
                            }
                        )
                    );
                }
            }
        }
    }

    pub fn refine_func(&mut self, func_ref: &FunctionRef, tp: &impl TypeProvider) {
        if let FunctionRef::Id(id) = func_ref {
            if self.refine.conditions.get(id).is_some() { return; }
        }

        let func = function_by_ref(&self.code.mir_code, &func_ref);
        let func_name = func.name;
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
        solver.set_option(":produce-proofs", "true").unwrap();

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
                                Constraint::OriginatesFrom(op.into(), ConstraintValueOrigin::new_pointer(op, len))
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
                                        func_name,
                                        vec![
                                            Constraint::Lte(lo.into(), sum.clone()),
                                            Constraint::Lte(sum.clone(), hi.into()),
                                        ],
                                        &constraints,
                                    ).unwrap();
                                    constraints.insert(Constraint::Eq(op.into(), sum));
                                    let a_origin = self.origin_of(&a.into(), &constraints).cloned();
                                    let b_origin = self.origin_of(&b.into(), &constraints).cloned();
                                    let a_origin_ptr = matches!(a_origin, Some(ConstraintValueOrigin::Pointer { .. }));
                                    let b_origin_ptr = matches!(b_origin, Some(ConstraintValueOrigin::Pointer { .. }));
                                    if a_origin_ptr ^ b_origin_ptr {
                                        assert!(!is_signed);
                                        if a_origin_ptr {
                                            self.propagate_ptr_origin_through_add(&mut solver, func_name, &mut constraints, a_origin.unwrap(), b.into(), op);
                                        } else {
                                            self.propagate_ptr_origin_through_add(&mut solver, func_name, &mut constraints, b_origin.unwrap(), a.into(), op);
                                        }
                                    }
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
                                        func_name,
                                        vec![
                                            Constraint::Lte(lo.into(), diff.clone()),
                                            Constraint::Lte(diff.clone(), hi.into()),
                                        ],
                                        &constraints,
                                    ).unwrap();
                                    constraints.insert(Constraint::Eq(op.into(), diff));
                                    if let Some(origin) = self.origin_of(&a.into(), &constraints) {
                                        match origin {
                                            ConstraintValueOrigin::Pointer { base, offset, len } => {
                                                assert!(!is_signed);
                                                let diff = offset.clone() - ConstraintValue::from(b);
                                                if self.check_conditions(
                                                    &mut solver,
                                                    func_name,
                                                    vec![
                                                        Constraint::Lte("0".to_string().into(), diff.clone()),
                                                    ],
                                                    &constraints,
                                                ).is_ok() {
                                                    let (base, len) = (base.clone(), len.clone());
                                                    constraints.insert(
                                                        Constraint::OriginatesFrom(
                                                            op.into(),
                                                            ConstraintValueOrigin::Pointer {
                                                                base,
                                                                offset: diff,
                                                                len: len.clone()
                                                            }
                                                        )
                                                    );
                                                }
                                            }
                                        }
                                    }
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
                                        func_name,
                                        vec![
                                            Constraint::Lte(lo.into(), neg.clone()),
                                            Constraint::Lte(neg.clone(), hi.into()),
                                        ],
                                        &constraints,
                                    ).unwrap();

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
                                Constraint::OriginatesFrom(
                                    op.into(),
                                    ConstraintValueOrigin::new_pointer(op, len)
                                )
                            );
                        }
                        _ => {},
                    }
                },
                &Instr::Ret(val) => {
                    let postconditions = postconditions.iter()
                        .map(|condition| condition.replace(&ConstraintValue::ReturnValue, val))
                        .collect();
                    self.check_conditions(&mut solver, func_name, postconditions, &constraints).unwrap();
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
                    self.check_conditions(&mut solver, func_name, preconditions, &constraints).unwrap();
                    let postconditions = conditions.postconditions.iter().cloned().map(replace_params);
                    for mut condition in postconditions {
                        condition = condition.replace(&ConstraintValue::ReturnValue, op);
                        constraints.insert(condition);
                    }
                },
                &Instr::Store { location, value } => {
                    let origin = self.origin_of(&location.into(), &constraints).expect("can't load from pointer of unknown origin");
                    match origin {
                        ConstraintValueOrigin::Pointer { base, offset, .. } => {
                            let (base, offset) = (base.clone(), offset.clone());
                            self.add_pointer_write(&mut solver, func_name, &constraints, base, offset, value);
                        }
                    };
                },
                &Instr::Load(location) => {
                    let origin = self.origin_of(&location.into(), &constraints).expect("can't load from pointer of unknown origin");
                    let pointee = match origin {
                        ConstraintValueOrigin::Pointer { base, offset, .. } => {
                            let (base, offset) = (base.clone(), offset.clone());
                            let pointee_ty = self.type_of(location).deref().unwrap().ty;
                            let expected_len = self.size_of(&pointee_ty);
                            self.get_pointee(&mut solver, func_name, &constraints, base, offset, expected_len)
                        }
                    };
                    match pointee {
                        None => panic!("can't read from uninitialized memory!"),
                        Some(Pointee::Unknown) => {},
                        Some(Pointee::Known(pointee)) => {
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
                        Constraint::OriginatesFrom(op.into(), ConstraintValueOrigin::new_pointer(op, len))
                    );
                }
                &Instr::AddressOfStatic(statik) => {
                    let ty = self.code.mir_code.statics[statik].ty();
                    let len = self.size_of(&ty);

                    // Static variables are not uninitialized, but we can't be sure what their values are
                    self.set_unknown_pointee(op, len);

                    self.start_constraints(op);
                    constraints.insert(
                        Constraint::OriginatesFrom(
                            op.into(),
                            ConstraintValueOrigin::new_pointer(op, len.to_string())
                        )
                    );
                },
                &Instr::Reinterpret(val, ref dest_ty) => {
                    let src_ty = self.type_of(val);
                    match (&src_ty, dest_ty) {
                        (Type::Pointer(_), &Type::Int { width, is_signed }) => {
                            assert_eq!((width, is_signed), (IntWidth::Pointer, false), "Unexpected pointer-to-int cast");
                            if let Some(origin) = self.origin_of(&val.into(), &constraints) {
                                let origin = origin.clone();
                                self.start_constraints(op);
                                match &origin {
                                    ConstraintValueOrigin::Pointer { offset, len, .. } => {
                                        let (lo, hi) = self.get_int_range(width, is_signed);
                                        constraints.insert(Constraint::Lte(lo.into(), op.into()));
                                        constraints.insert(
                                            Constraint::Lte(
                                                op.into(),
                                                ConstraintValue::from(hi) - len.clone() + offset.clone(),
                                            )
                                        );
                                    }
                                }
                                constraints.insert(Constraint::OriginatesFrom(op.into(), origin));
                            }
                        },
                        (&Type::Int { width, is_signed }, Type::Pointer(ptr)) => {
                            assert_eq!((width, is_signed), (IntWidth::Pointer, false), "Unexpected int-to-pointer cast");
                            let origin = self.origin_of(&val.into(), &constraints)
                                .expect("Can't cast an integer type of unknown origin to a pointer")
                                .clone();
                            let ptr_ty = ptr.ty.clone();
                            self.start_constraints(op);
                            match origin.clone() {
                                ConstraintValueOrigin::Pointer { offset, len, .. } => {
                                    let size = self.size_of(&ptr_ty).to_string();
                                    let zero = ConstraintValue::from("0".to_string());
                                    self.check_conditions(
                                        &mut solver,
                                        func_name,
                                        vec![
                                            Constraint::Lte(offset.clone() + ConstraintValue::from(size), len),
                                            Constraint::Lte(zero, offset),
                                        ],
                                        &constraints
                                    ).unwrap();
                                }
                            }
                            constraints.insert(Constraint::OriginatesFrom(op.into(), origin));
                        },
                        (Type::Pointer(_), Type::Pointer(dest)) => {
                            let origin = self.origin_of(&val.into(), &constraints)
                                .expect("Can't cast an pointer type of unknown origin to another pointer")
                                .clone();
                            let dest = dest.ty.clone();
                            self.start_constraints(op);
                            match origin.clone() {
                                ConstraintValueOrigin::Pointer { offset, len, .. } => {
                                    let size = self.size_of(&dest).to_string();
                                    let zero = ConstraintValue::from("0".to_string());
                                    self.check_conditions(
                                        &mut solver,
                                        func_name,
                                        vec![
                                            Constraint::Lte(offset.clone() + ConstraintValue::from(size), len),
                                            Constraint::Lte(zero, offset),
                                        ],
                                        &constraints
                                    ).unwrap();
                                }
                            }
                            constraints.insert(Constraint::OriginatesFrom(op.into(), origin));
                        },
                        (_, _) => panic!("Unsupported reinterpret cast"),
                    }
                },
                _ => {},
            }
        }
    }
}