use std::collections::{HashSet, HashMap};
use std::ops::{Add, Sub, Neg};
use std::iter::Iterator;
use std::str::FromStr;

use rsmt2::prelude::*;
use rsmt2::parse::{IdentParser, ModelParser};
use display_adapter::display_adapter;
use string_interner::DefaultSymbol as Sym;
use num_bigint::BigInt;

use dir::{BlockId, OpId};
use dir::dil::{Const, Function, FuncId, Instr};
use dir::ty::{Type, IntWidth};
use dir::hir::{Intrinsic, Expr, Decl, ExprId};

use crate::typechecker::type_provider::TypeProvider;
use crate::interpreter::Value;
use crate::dil::{FunctionRef, function_by_ref};
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

#[derive(Default, Debug)]
struct Constraints {
    requirements: Vec<Constraint>,
    guarantees: Vec<Constraint>,
    ret_val: Option<OpId>,
}

#[derive(Default)]
pub struct Refine {
    constraints: HashMap<FuncId, Constraints>,
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
    fn fac_and_offset(&self) -> Option<(&ConstraintValue, BigInt, BigInt)> {
        match self {
            ConstraintValue::Str(_) | ConstraintValue::Op(_) | ConstraintValue::Parameter { .. } | ConstraintValue::ReturnValue => Some((self, BigInt::from(1), BigInt::from(0))),
            ConstraintValue::Add(l, r) => match (&**l, &**r) {
                (ConstraintValue::Str(a), b) | (b, ConstraintValue::Str(a)) => {
                    let offset = BigInt::from_str(a).unwrap();
                    Some((b, BigInt::from(1), offset))
                },
                _ => None,
            },
            ConstraintValue::Sub(l, r) => match (&**l, &**r) {
                (ConstraintValue::Str(a), b) => {
                    let offset = BigInt::from_str(a).unwrap();
                    Some((b, BigInt::from(-1), offset))
                },
                (b, ConstraintValue::Str(a)) => {
                    let offset = -BigInt::from_str(a).unwrap();
                    Some((b, BigInt::from(1), offset))
                },
                _ => None,
            },
            ConstraintValue::Neg(a) => Some((a, BigInt::from(-1), BigInt::from(0))),
        }
    }

    fn simplify(&mut self) {
        match self {
            ConstraintValue::Add(l, r) => {
                l.simplify();
                r.simplify();
                match l.fac_and_offset().zip(r.fac_and_offset()) {
                    Some(((l, l_fac, l_offset), (r, r_fac, r_offset))) => {
                        let offset = l_offset + r_offset;
                        match (l, r) {
                            (ConstraintValue::Str(l), ConstraintValue::Str(r)) => {
                                let val = l_fac * BigInt::from_str(l).unwrap() + r_fac * BigInt::from_str(r).unwrap() + offset;
                                *self = ConstraintValue::Str(val.to_string());
                            },
                            (ConstraintValue::Str(l), r) => {
                                let constant = l_fac * BigInt::from_str(l).unwrap() + offset;
                                if r_fac == BigInt::from(-1) {
                                    if &constant == &BigInt::from(0) {
                                        *self = -r.clone();
                                    } else {
                                        *self = ConstraintValue::from(constant.to_string()) - r.clone();
                                    }
                                } else {
                                    if &constant == &BigInt::from(0) {
                                        *self = r.clone();
                                    } else {
                                        *self = ConstraintValue::from(constant.to_string()) + r.clone();
                                    }
                                }
                            },
                            (l, ConstraintValue::Str(r)) => {
                                let constant = r_fac * BigInt::from_str(r).unwrap() + offset;
                                if l_fac == BigInt::from(-1) {
                                    if &constant == &BigInt::from(0) {
                                        *self = -l.clone();
                                    } else {
                                        *self = ConstraintValue::from(constant.to_string()) - l.clone();
                                    }
                                } else {
                                    if &constant == &BigInt::from(0) {
                                        *self = l.clone();
                                    } else {
                                        *self = ConstraintValue::from(constant.to_string()) + l.clone();
                                    }
                                }
                            },
                            _ => {},
                        }
                    },
                    None => {},
                }
            },
            ConstraintValue::Sub(l, r) => {
                l.simplify();
                r.simplify();
                match l.fac_and_offset().zip(r.fac_and_offset()) {
                    Some(((l, l_fac, l_offset), (r, mut r_fac, r_offset))) => {
                        let offset = l_offset - r_offset;
                        r_fac *= -1;

                        // Same as addition from here on
                        match (l, r) {
                            (ConstraintValue::Str(l), ConstraintValue::Str(r)) => {
                                let val = l_fac * BigInt::from_str(l).unwrap() + r_fac * BigInt::from_str(r).unwrap() + offset;
                                *self = ConstraintValue::Str(val.to_string());
                            },
                            (ConstraintValue::Str(l), r) => {
                                let constant = l_fac * BigInt::from_str(l).unwrap() + offset;
                                if r_fac == BigInt::from(-1) {
                                    if &constant == &BigInt::from(0) {
                                        *self = -r.clone();
                                    } else {
                                        *self = ConstraintValue::from(constant.to_string()) - r.clone();
                                    }
                                } else {
                                    if &constant == &BigInt::from(0) {
                                        *self = r.clone();
                                    } else {
                                        *self = ConstraintValue::from(constant.to_string()) + r.clone();
                                    }
                                }
                            },
                            (l, ConstraintValue::Str(r)) => {
                                let constant = r_fac * BigInt::from_str(r).unwrap() + offset;
                                if l_fac == BigInt::from(-1) {
                                    if &constant == &BigInt::from(0) {
                                        *self = -l.clone();
                                    } else {
                                        *self = ConstraintValue::from(constant.to_string()) - l.clone();
                                    }
                                } else {
                                    if &constant == &BigInt::from(0) {
                                        *self = l.clone();
                                    } else {
                                        *self = ConstraintValue::from(constant.to_string()) + l.clone();
                                    }
                                }
                            },
                            _ => {},
                        }
                    },
                    None => {},
                }
            },
            ConstraintValue::Neg(a) => {
                a.simplify();
                match &**a {
                    ConstraintValue::Str(a) => {
                        let val = -BigInt::from_str(a).unwrap();
                        *self = ConstraintValue::Str(val.to_string());
                    }
                    _ => {},
                }
            },
            ConstraintValue::Str(_) | ConstraintValue::Op(_) | ConstraintValue::Parameter { .. } | ConstraintValue::ReturnValue => {},
        }
    }

    fn replace(&self, key: &ConstraintValue, value: impl Into<ConstraintValue> + Clone) -> Self {
        if self == key {
            value.into()
        } else {
            match self {
                ConstraintValue::Str(_) | ConstraintValue::Op(_) | ConstraintValue::Parameter { .. } | ConstraintValue::ReturnValue => self.clone(),
                ConstraintValue::Add(l, r) => ConstraintValue::Add(
                    Box::new(l.replace(key, value.clone())),
                    Box::new(r.replace(key, value)),
                ),
                ConstraintValue::Sub(l, r) => ConstraintValue::Sub(
                    Box::new(l.replace(key, value.clone())),
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
            ConstraintValue::Parameter { .. } => vec![],
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

    fn replace(&self, key: &ConstraintValue, value: impl Into<ConstraintValue> + Clone) -> Self {
        match self {
            ConstraintValueOrigin::Pointer { base, offset, len } => ConstraintValueOrigin::Pointer {
                base: base.replace(key, value.clone()),
                offset: offset.replace(key, value.clone()),
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
    Const(bool),
}

impl Constraint {
    fn simplify(&mut self) {
        match self {
            Constraint::Gte(l, r) => {
                l.simplify();
                r.simplify();
                match (&*l, &*r) {
                    (ConstraintValue::Str(l), ConstraintValue::Str(r)) => {
                        let val = BigInt::from_str(l).unwrap() >= BigInt::from_str(r).unwrap();
                        *self = Constraint::Const(val);
                    },
                    (ConstraintValue::Str(l), r) => if let Some((val, fac, offset)) = r.fac_and_offset() {
                        let constant = BigInt::from_str(l).unwrap() - offset;
                        let mut val = val.clone();
                        if fac == BigInt::from(-1) { val = -val.clone(); }
                        *self = Constraint::Gte(constant.to_string().into(), val);
                    },
                    (l, ConstraintValue::Str(r)) => if let Some((val, fac, offset)) = l.fac_and_offset() {
                        let constant = BigInt::from_str(r).unwrap() - offset;
                        let mut val = val.clone();
                        if fac == BigInt::from(-1) { val = -val.clone(); }
                        *self = Constraint::Gte(val, constant.to_string().into());
                    },
                    _ => {},
                }
            },
            Constraint::Lte(l, r) => {
                l.simplify();
                r.simplify();
                match (&*l, &*r) {
                    (ConstraintValue::Str(l), ConstraintValue::Str(r)) => {
                        let val = BigInt::from_str(l).unwrap() <= BigInt::from_str(r).unwrap();
                        *self = Constraint::Const(val);
                    },
                    (ConstraintValue::Str(l), r) => if let Some((val, fac, offset)) = r.fac_and_offset() {
                        let constant = BigInt::from_str(l).unwrap() - offset;
                        let mut val = val.clone();
                        if fac == BigInt::from(-1) { val = -val.clone(); }
                        *self = Constraint::Lte(constant.to_string().into(), val);
                    },
                    (l, ConstraintValue::Str(r)) => if let Some((val, fac, offset)) = l.fac_and_offset() {
                        let constant = BigInt::from_str(r).unwrap() - offset;
                        let mut val = val.clone();
                        if fac == BigInt::from(-1) { val = -val.clone(); }
                        *self = Constraint::Lte(val, constant.to_string().into());
                    },
                    _ => {},
                }
            },
            Constraint::Eq(l, r) => {
                l.simplify();
                r.simplify();
                match (&*l, &*r) {
                    (ConstraintValue::Str(l), ConstraintValue::Str(r)) => {
                        let val = BigInt::from_str(l).unwrap() == BigInt::from_str(r).unwrap();
                        *self = Constraint::Const(val);
                    },
                    (ConstraintValue::Str(l), r) => if let Some((val, fac, offset)) = r.fac_and_offset() {
                        let constant = BigInt::from_str(l).unwrap() - offset;
                        let mut val = val.clone();
                        if fac == BigInt::from(-1) { val = -val.clone(); }
                        *self = Constraint::Eq(constant.to_string().into(), val);
                    },
                    (l, ConstraintValue::Str(r)) => if let Some((val, fac, offset)) = l.fac_and_offset() {
                        let constant = BigInt::from_str(r).unwrap() - offset;
                        let mut val = val.clone();
                        if fac == BigInt::from(-1) { val = -val.clone(); }
                        *self = Constraint::Eq(val, constant.to_string().into());
                    },
                    (l, r) if l == r => {
                        *self = Constraint::Const(true);
                    }
                    _ => {},
                }
            },
            Constraint::Const(_) => {},
        }
    }

    fn replace(&self, key: &ConstraintValue, value: impl Into<ConstraintValue> + Clone) -> Self {
        match self {
            Constraint::Gte(l, r) => Constraint::Gte(l.replace(key, value.clone()), r.replace(key, value.clone())),
            Constraint::Lte(l, r) => Constraint::Lte(l.replace(key, value.clone()), r.replace(key, value.clone())),
            Constraint::Eq(l, r) => Constraint::Eq(l.replace(key, value.clone()), r.replace(key, value.clone())),
            &Constraint::Const(val) => Constraint::Const(val),
        }
    }

    fn get_involved_ops(&self) -> HashSet<OpId> {
        match self {
            Constraint::Gte(l, r) | Constraint::Lte(l, r) | Constraint::Eq(l, r) => {
                [l, r].iter().map(|val| val.get_involved_ops())
                    .flatten()
                    .collect()
            },
            Constraint::Const(_) => HashSet::new(),
        }
    }
}

struct RefineSession {
    solver: Solver<Parser>,
    func_name: Option<Sym>,
}

impl Driver {
    fn remove_constraints_implied_by_others(&mut self, rs: &mut RefineSession, constraints: &mut Vec<Constraint>) {
        // Remove requirements that are implied by the group of all other requirements
        let mut requirements_set: HashSet<Constraint> = constraints.iter().cloned().collect();
        constraints.retain(|requirement| {
            requirements_set.remove(requirement);

            let should_include = self.check_constraints(rs, &requirements_set, vec![requirement.clone()]).is_err();

            requirements_set.insert(requirement.clone());
            should_include
        });
    }

    fn simplify_constraint_set(&mut self, constraints: &mut Vec<Constraint>) {
        for constraint in &mut *constraints {
            constraint.simplify();
        }
        constraints.retain(|constraint| !matches!(constraint, Constraint::Const(_)));
    }

    fn simplify_constraints(&mut self, rs: &mut RefineSession, constraints: &mut Constraints) {
        self.simplify_constraint_set(&mut constraints.requirements);
        self.simplify_constraint_set(&mut constraints.guarantees);

        // Remove requirements that are implied by the types of values
        constraints.requirements.retain(|requirement| {
            let mut implicit_constraints = HashSet::new();
            for op in requirement.get_involved_ops() {
                let ty = self.type_of(op);
                match ty {
                    Type::Int { width, is_signed } => {
                        let (lo, hi) = self.get_int_range(width, is_signed);
                        implicit_constraints.insert(Constraint::Lte(lo.into(), op.into()));
                        implicit_constraints.insert(Constraint::Lte(op.into(), hi.into()));
                    },
                    _ => panic!("unhandled type"),
                }
            }

            self.check_constraints(rs, &implicit_constraints, vec![requirement.clone()]).is_err()
        });

        
        // Remove guarantees that are equal to the constraints implied by the types of values.
        // Note that we must be more conservative for guarantees than requirements for now,
        // because I'm not sure what the rules are yet for when, e.g., parameters can have
        // guarantees placed on them.
        //
        // Regardless, clearly something like "i32 param <= i32_max" is redundant.
        constraints.guarantees.retain(|guarantee| {
            let mut implicit_constraints = HashSet::new();
            for op in guarantee.get_involved_ops() {
                let ty = self.type_of(op);
                match ty {
                    Type::Int { width, is_signed } => {
                        let (lo, hi) = self.get_int_range(width, is_signed);
                        implicit_constraints.insert(Constraint::Lte(lo.into(), op.into()));
                        implicit_constraints.insert(Constraint::Lte(op.into(), hi.into()));
                    },
                    _ => panic!("unhandled type"),
                }
            }
            
            !implicit_constraints.contains(guarantee)
        });

        self.remove_constraints_implied_by_others(rs, &mut constraints.requirements);
        self.remove_constraints_implied_by_others(rs, &mut constraints.guarantees);
    }

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
            let terminal = self.code.ops[terminal].as_dil_instr().unwrap();
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
        for func_id in self.code.dil_code.functions.indices() {
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

    #[display_adapter]
    fn display_constraint_value(&self, val: &ConstraintValue, f: &mut Formatter) {
        match val {
            &ConstraintValue::Op(op) => write!(f, "{}", self.display_instr_name(op))?,
            ConstraintValue::Str(str) => write!(f, "{}", str)?,
            ConstraintValue::Add(l, r) => write!(f, "{}", self.display_bin_expr("+", l, r))?,
            ConstraintValue::Sub(l, r) => write!(f, "{}", self.display_bin_expr("-", l, r))?,
            ConstraintValue::Neg(val) => write!(f, "(- {})", self.display_constraint_value(val))?,
            ConstraintValue::ReturnValue => write!(f, "return_value")?,
            &ConstraintValue::Parameter { index } => write!(f, "param{}", index)?,
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
            &Constraint::Const(val) => write!(f, "{}", val),
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
}

#[derive(Debug)]
struct ConstraintCheckError {
    model: Vec<(String, Vec<(String, String)>, String, String)>,
    failed_constraint: Constraint,
}

impl Driver {
    fn check_constraints(&mut self, rs: &mut RefineSession, axioms: &HashSet<Constraint>, constraints: Vec<Constraint>) -> Result<(), ConstraintCheckError> {
        for condition in constraints {
            let mut variables = HashSet::new();
            variables.extend(condition.get_involved_ops());

            let mut relevant_constraints = HashSet::new();

            loop {
                let mut added_anything = false;
                // TODO: this is probably crazy slow
                for constraint in axioms {
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
            rs.solver.push(1).unwrap();
            let condition_str: String = format!("\n    {}", self.display_condition_check(&condition, &relevant_constraints));
            for &var in &variables {
                rs.solver.declare_const(&format!("{}", self.display_instr_name(var)), "Int").unwrap();
            }
            rs.solver.assert(&condition_str).unwrap();
            let val = if rs.solver.check_sat().unwrap() {
                let mut model = rs.solver.get_model().unwrap();

                // Limit the returned model to just the variables that show up in the current condition
                model.retain(|assignment| 
                    condition.get_involved_ops().iter()
                        .map(|&op| format!("{}", self.display_instr_name(op)))
                        .collect::<Vec<_>>()
                        .contains(&&assignment.0)
                );
                Err(ConstraintCheckError {
                    model,
                    failed_constraint: condition,
                })
            } else {
                Ok(())
            };
            rs.solver.pop(1).unwrap();
    
            val?;
        }

        Ok(())
    }

    fn constrain_op(&mut self, op: OpId, tp: &impl TypeProvider, explicit_guarantees: &HashSet<Constraint>) -> Constraints {
        let instr = self.code.ops[op].as_dil_instr().unwrap();
        let mut requirements = Vec::<Constraint>::new();
        let mut guarantees = Vec::<Constraint>::new();
        let mut ret_val = None;
        match instr {
            Instr::Parameter(ty) => {
                match ty {
                    &Type::Int { width, is_signed } => {
                        let (lo, hi) = self.get_int_range(width, is_signed);
                        guarantees.push(Constraint::Lte(lo.into(), op.into()));
                        guarantees.push(Constraint::Lte(op.into(), hi.into()));
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
                                guarantees.push(Constraint::Eq(op.into(), val.to_string().into()));
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
                                let (a, b) = (arguments[0], arguments[1]);
                                let sum = ConstraintValue::from(a) + ConstraintValue::from(b);
                                requirements.push(Constraint::Lte(lo.into(), sum.clone()));
                                requirements.push(Constraint::Lte(sum.clone(), hi.into()));
                                guarantees.push(Constraint::Eq(op.into(), sum));
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
                                let diff = ConstraintValue::from(a) - ConstraintValue::from(b);
                                requirements.push(Constraint::Lte(lo.into(), diff.clone()));
                                requirements.push(Constraint::Lte(diff.clone(), hi.into()));
                                guarantees.push(Constraint::Eq(op.into(), diff));
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
                                let neg = -ConstraintValue::from(a);
                                requirements.push(Constraint::Lte(lo.into(), neg.clone()));
                                requirements.push(Constraint::Lte(neg.clone(), hi.into()));
                                guarantees.push(Constraint::Eq(op.into(), neg));
                            },
                            _ => panic!("unhandled type"),
                        }
                    },
                    _ => {},
                }
            },
            &Instr::Ret(val) => {
                requirements.extend(
                    // TODO: filter to only those guarantees that include the return value?
                    explicit_guarantees.iter().map(|req| req.replace(&ConstraintValue::ReturnValue, val))
                );
                ret_val = Some(val)
            },
            &Instr::Call { ref arguments, func, .. } => {
                // Thanks borrow checker. :(
                let arguments = arguments.clone();
                self.refine_func(&FunctionRef::Id(func), tp);
                let constraints = self.refine.constraints.get(&func).unwrap();
                let substitute_special_values = |constraint: &Constraint| {
                    let mut constraint = constraint.clone();
                    for i in 0..arguments.len() {
                        constraint = constraint.replace(&ConstraintValue::Parameter { index: i }, arguments[i]);
                    }
                    constraint.replace(&ConstraintValue::ReturnValue, op)
                };
                requirements = constraints.requirements.iter().map(substitute_special_values).collect();
                guarantees = constraints.guarantees.iter().map(substitute_special_values).collect();
            },
            _ => {},
        }
        Constraints { requirements, guarantees, ret_val }
    }

    fn constrain_block(&mut self, block_id: BlockId, tp: &impl TypeProvider, explicit_guarantees: &HashSet<Constraint>) -> Constraints {
        let mut constraints = Constraints::default();
        let block = &self.code.blocks[block_id];
        for i in 0..block.ops.len() {
            let block = &self.code.blocks[block_id];
            let op = block.ops[i];
            
            let op_constraints = self.constrain_op(op, tp, explicit_guarantees);
            constraints.requirements.extend(op_constraints.requirements);
            constraints.guarantees.extend(op_constraints.guarantees);

            assert!(constraints.ret_val.is_none());
            constraints.ret_val = op_constraints.ret_val;
        }
        constraints
    }

    fn infer_constraints(&self, src: &[Constraint], block_constraints: &Constraints, dont_replace: &HashSet<OpId>) -> Vec<Constraint> {
        src.iter()
            .map(|constraint| {
                // TODO: what if invalid guarantees from later on in the function mess with us?
                let mut constraint = constraint.clone();
                loop {
                    let involved_ops = constraint.get_involved_ops();
                    let mut replaced = false;
                    // Find a substitute value for all non-parameter, non-constant values
                    for op in involved_ops.difference(dont_replace).copied() {
                        replaced = true;
                        let mut sub = None;
                        for guarantee in &block_constraints.guarantees {
                            if let &Constraint::Eq(ConstraintValue::Op(a), ref b) = guarantee {
                                if a == op {
                                    sub = Some(b.clone());
                                    break;
                                }
                            }
                        }
                        constraint = constraint.replace(&op.into(), sub.expect("Failed to find equality guarantee" ));
                    }
                    if !replaced {
                        break;
                    }
                }
                constraint
            })
            .collect()
    }

    pub fn refine_func(&mut self, func_ref: &FunctionRef, tp: &impl TypeProvider) {
        if let FunctionRef::Id(id) = func_ref {
            if self.refine.constraints.get(id).is_some() { return; }
        }

        let func = function_by_ref(&self.code.dil_code, &func_ref);
        let func_name = func.name;
        self.check_no_loops(func);
        assert_eq!(func.blocks.len(), 1, "Function has more than one block, which isn't yet supported");

        let mut explicit_requirements = Vec::new();
        let mut explicit_guarantees = Vec::new();
        let block_id = func.blocks[0];
        let block = &self.code.blocks[block_id];
        let num_params = self.code.num_parameters(func);
        if let Some(decl) = func.decl {
            if let Some(attributes) = self.code.hir_code.decl_attributes.get(&decl) {
                for attr in attributes {
                    let arg = attr.arg.expect("missing attribute argument");
                    let constraint = self.expr_to_constraint(arg, tp);
                    if attr.attr == self.hir.requires_sym {
                        explicit_requirements.push(constraint);
                    } else if attr.attr == self.hir.guarantees_sym {
                        explicit_guarantees.push(constraint);
                    } else {
                        panic!("Unrecognized attribute");
                    }
                }
            }
        }

        let replace_parameters = |mut constraint: Constraint| {
            for index in 0..num_params {
                constraint = constraint.replace(&ConstraintValue::Parameter { index }, block.ops[index]);
            }
            constraint
        };
        let explicit_requirements: HashSet<_> = explicit_requirements.into_iter().map(replace_parameters).collect();
        let explicit_guarantees: HashSet<_> = explicit_guarantees.into_iter().map(replace_parameters).collect();
        let conf = SmtConf::default_z3();

        // Get the parameter range
        let block = &self.code.blocks[block_id];
        let mut dont_replace: HashSet<OpId> = HashSet::new();
        dont_replace.extend(&block.ops[0..num_params]);

        let block_constraints = self.constrain_block(block_id, tp, &explicit_guarantees);
        let requirements = self.infer_constraints(
            &block_constraints.requirements,
            &block_constraints,
            &dont_replace,
        );
        dont_replace.insert(block_constraints.ret_val.unwrap());
        let guarantees = self.infer_constraints(
            &block_constraints.guarantees,
            &block_constraints,
            &dont_replace,
        );
        let mut constraints = Constraints {
            requirements,
            guarantees,
            ..Default::default()
        };
        //constraints.requirements.extend(explicit_requirements);
        let mut active_constraints = HashSet::new();
        active_constraints.extend(constraints.requirements.iter().cloned());
        let mut rs = RefineSession {
            solver: conf.spawn(Parser).unwrap(),
            func_name,
        };

        self.simplify_constraints(&mut rs, &mut constraints);

        println!("FUNCTION: {}", self.fn_name(func_name));
        println!("Requirements:");
        for constraint in &constraints.requirements {
            println!("    {}", self.display_constraint(&constraint));
        }
        println!("\nGuarantees:");
        for constraint in &constraints.guarantees {
            println!("    {}", self.display_constraint(&constraint));
        }
        println!("\n");

        let block = &self.code.blocks[block_id];
        for i in 0..block.ops.len() {
            let block = &self.code.blocks[block_id];
            let op = block.ops[i];
            let mut constraints = self.constrain_op(op, tp, &explicit_guarantees);
            self.simplify_constraints(&mut rs, &mut constraints);

            // TODO: speed. this is really, really stupid!
            let mut active_constraints_vec = Vec::new();
            active_constraints_vec.extend(active_constraints.iter().cloned());
            self.simplify_constraint_set(&mut active_constraints_vec);
            active_constraints = active_constraints_vec.into_iter().collect();

            if let Err(error) = self.check_constraints(&mut rs, &active_constraints, constraints.requirements.clone()) {
                println!("Refinement checker failed on condition {}", self.display_constraint(&error.failed_constraint));
                println!("    in function {}", self.fn_name(rs.func_name));
                println!("    model: {:?}", error.model);
                std::process::exit(1);
            }
            active_constraints.extend(constraints.guarantees);
        }

        let block = &self.code.blocks[block_id];
        let replace_parameters = |mut constraint: Constraint| {
            for index in 0..num_params {
                constraint = constraint.replace(&block.ops[index].into(), ConstraintValue::Parameter { index });
            }
            constraint = constraint.replace(&block_constraints.ret_val.unwrap().into(), ConstraintValue::ReturnValue);
            constraint
        };
        let requirements: Vec<_> = constraints.requirements.into_iter().map(replace_parameters).collect();
        let guarantees: Vec<_> = constraints.guarantees.into_iter().map(replace_parameters).collect();

        if let &FunctionRef::Id(id) = func_ref {
            self.refine.constraints.insert(
                id,
                Constraints {
                    requirements: requirements.clone(),
                    guarantees: guarantees.clone(),
                    ..Default::default()
                }
            );
        }
    }
}