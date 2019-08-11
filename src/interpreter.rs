use std::mem;

use arrayvec::ArrayVec;

use crate::mir::{Function, FuncId, Instr, InstrId, Program};
use crate::index_vec::{IdxVec, Idx};

#[derive(Clone, Debug)]
pub enum Value {
    Inline(ArrayVec<[u8; 64 / 8]>),
    Dynamic(Box<[u8]>),
    Nothing,
}

struct StackFrame {
    pc: InstrId,
    results: IdxVec<Value, InstrId>,
}

impl StackFrame {
    fn new(func: &Function, arguments: Vec<Value>) -> Self {
        let mut results = IdxVec::new();
        results.resize_with(func.code.len(), || Value::Nothing);

        let num_parameters = func.num_parameters();
        assert_eq!(num_parameters, arguments.len());
        for (i, arg) in arguments.into_iter().enumerate() {
            results.raw[i + 1] = arg;
        }
        Self {
            pc: InstrId::new(num_parameters + 1),
            results,
        }
    }
}

pub struct Interpreter<'mir> {
    stack: Vec<(FuncId, StackFrame)>,
    prog: &'mir Program,
}

impl<'mir> Interpreter<'mir> {
    pub fn new(prog: &'mir Program) -> Self {
        Self {
            stack: Vec::new(),
            prog,
        }
    }

    pub fn call(&mut self, func_id: FuncId, arguments: Vec<Value>) -> Value {
        let frame = StackFrame::new(&self.prog.comp_decls[func_id], arguments);
        self.stack.push((func_id, frame));
        loop {
            if let Some(val) = self.execute_next() {
                self.stack.pop().unwrap();
                return val;
            }
        }
    }

    /// Execute the next instruction. Iff the instruction is a return, this function returns its `Value`
    fn execute_next(&mut self) -> Option<Value> {
        let (func_id, frame) = self.stack.last_mut().unwrap();
        let func = &self.prog.comp_decls[*func_id];
        let val = match &func.code[frame.pc] {
            Instr::Ret(instr) => {
                let val = mem::replace(&mut frame.results[*instr], Value::Nothing);
                return Some(val)
            },
            instr => panic!("Unrecognized instruction: {:?}", instr),
        };
        frame.pc.advance();
        val
    }
}