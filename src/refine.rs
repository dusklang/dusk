use std::collections::{HashSet, HashMap};

use rsmt2::prelude::*;

use mire::BlockId;
use mire::mir::{Const, Function, Instr};
use mire::ty::{Type, IntWidth};
use mire::hir::Intrinsic;

use crate::interpreter::Value;
use crate::mir::{FunctionRef, function_by_ref};
use crate::driver::Driver;

#[derive(Default)]
pub struct Refine {
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

    pub fn refine_func(&mut self, func_ref: &FunctionRef) {
        let func = function_by_ref(&self.code.mir_code, &func_ref);
        self.check_no_loops(func);
        assert_eq!(func.blocks.len(), 1, "Function has more than one block, which isn't yet supported");

        let block = func.blocks[0];
        let block = &self.code.blocks[block];

        let parser = ();

        let conf = SmtConf::default_z3();
        let mut solver = conf.spawn(parser).unwrap();

        let is_sat = solver.check_sat().unwrap();
        assert!(is_sat);


        let mut names = HashMap::new();
        let mut name_gen = NameGen::default();
        for &op in block.ops.iter() {
            let instr = self.code.ops[op].as_mir_instr().unwrap();
            match instr {
                Instr::Const(konst) => {
                    match konst {
                        Const::Int { ty, .. } => {
                            match ty {
                                &Type::Int { is_signed, .. } => {
                                    let name = names.entry(op).or_insert(name_gen.next().unwrap());
                                    // TODO: this is kind of silly, but it was the path of least resistance.
                                    let val = Value::from_const(konst, self).as_big_int(is_signed);
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
                                    let (a, b) = (&names[&arguments[0]], &names[&arguments[1]]);
                                    let add_expr = format!("(+ {} {})", a, b);
                                    let name = names.entry(op).or_insert(name_gen.next().unwrap());
                                    solver.define_const(name, "Int", &add_expr).unwrap();
                                    solver.assert(&format!("(>= {} {})", name, lo)).unwrap();
                                    solver.assert(&format!("(<= {} {})", name, hi)).unwrap();
                                },
                                _ => panic!("unhandled type"),
                            }
                        },
                        Intrinsic::Sub => {
                            assert_eq!(arguments.len(), 2);
                            match ty {
                                &Type::Int { width, is_signed } => {
                                    let (lo, hi) = self.get_int_range(width, is_signed);
                                    let (a, b) = (&names[&arguments[0]], &names[&arguments[1]]);
                                    let sub_expr = format!("(- {} {})", a, b);
                                    let name = names.entry(op).or_insert(name_gen.next().unwrap());
                                    solver.define_const(name, "Int", &sub_expr).unwrap();
                                    solver.assert(&format!("(>= {} {})", name, lo)).unwrap();
                                    solver.assert(&format!("(<= {} {})", name, hi)).unwrap();
                                },
                                _ => panic!("unhandled type"),
                            }
                        },
                        Intrinsic::Neg => {
                            assert_eq!(arguments.len(), 1);
                            match ty {
                                &Type::Int { width, is_signed } => {
                                    let (lo, hi) = self.get_int_range(width, is_signed);
                                    let a = &names[&arguments[0]];
                                    let neg_expr = format!("(- {})", a);
                                    let name = names.entry(op).or_insert(name_gen.next().unwrap());
                                    solver.define_const(name, "Int", &neg_expr).unwrap();
                                    solver.assert(&format!("(>= {} {})", name, lo)).unwrap();
                                    solver.assert(&format!("(<= {} {})", name, hi)).unwrap();
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