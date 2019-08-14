use std::mem;
use std::slice;
use std::convert::TryInto;

use arrayvec::ArrayVec;

use crate::index_vec::{IdxVec, Idx};
use crate::mir::{Const, Function, FuncId, Instr, InstrId, Program};
use crate::ty::{Type, IntWidth};


#[derive(Debug)]
pub enum Value {
    Inline(ArrayVec<[u8; 64 / 8]>),
    Dynamic(Box<[u8]>),
    Nothing,
}

impl Clone for Value {
    fn clone(&self) -> Value {
        match self {
            Value::Inline(storage) => Value::Inline(storage.clone()),
            Value::Dynamic(_) => Value::from_bytes(self.as_bytes()),
            Value::Nothing => Value::Nothing,
        }
    }
}

impl Value {
    fn as_bytes(&self) -> &[u8] {
        match self {
            Value::Inline(storage) => storage.as_ref(),
            Value::Dynamic(ptr) => unsafe {
                let address_bits = mem::transmute::<&Box<_>, *const u8>(ptr);
                slice::from_raw_parts(address_bits, mem::size_of::<usize>())
            },
            Value::Nothing => &[],
        }
    }

    fn as_raw_ptr(&self) -> *mut u8 {
        unsafe { mem::transmute(usize::from_le_bytes(self.as_bytes().try_into().unwrap())) }
    }

    fn as_u8(&self) -> u8 {
        u8::from_le_bytes(self.as_bytes().try_into().unwrap())
    }

    fn as_u16(&self) -> u16 {
        u16::from_le_bytes(self.as_bytes().try_into().unwrap())
    }

    fn as_u32(&self) -> u32 {
        u32::from_le_bytes(self.as_bytes().try_into().unwrap())
    }

    fn as_u64(&self) -> u64 {
        u64::from_le_bytes(self.as_bytes().try_into().unwrap())
    }

    fn as_i8(&self) -> i8 {
        i8::from_le_bytes(self.as_bytes().try_into().unwrap())
    }

    fn as_i16(&self) -> i16 {
        i16::from_le_bytes(self.as_bytes().try_into().unwrap())
    }

    fn as_i32(&self) -> i32 {
        i32::from_le_bytes(self.as_bytes().try_into().unwrap())
    }

    fn as_i64(&self) -> i64 {
        i64::from_le_bytes(self.as_bytes().try_into().unwrap())
    }

    fn as_f64(&self) -> f64 {
        f64::from_bits(self.as_u64())
    }

    fn as_f32(&self) -> f32 {
        f32::from_bits(self.as_u32())
    }

    fn as_bool(&self) -> bool {
        let bytes = self.as_bytes();
        assert!(bytes.len() == 1);
        unsafe { mem::transmute(bytes[0]) }
    }

    fn from_bytes(bytes: &[u8]) -> Value {
        let mut storage = ArrayVec::new();
        // TODO: there's an nonsense error message if I try to call `ArrayVec::from_iterator` or `ArrayVec::extend` with `bytes`. 
        // But this loop works, because we don't live in a world where things make sense.
        for &byte in bytes {
            storage.push(byte);
        }
        Value::Inline(storage)
    }

    fn from_u8(val: u8) -> Value {
        Value::from_bytes(val.to_le_bytes().as_ref())
    }

    fn from_u16(val: u16) -> Value {
        Value::from_bytes(val.to_le_bytes().as_ref())
    }

    fn from_u32(val: u32) -> Value {
        Value::from_bytes(val.to_le_bytes().as_ref())
    }

    fn from_u64(val: u64) -> Value {
        Value::from_bytes(val.to_le_bytes().as_ref())
    }

    fn from_i8(val: i8) -> Value {
        Value::from_bytes(val.to_le_bytes().as_ref())
    }

    fn from_i16(val: i16) -> Value {
        Value::from_bytes(val.to_le_bytes().as_ref())
    }

    fn from_i32(val: i32) -> Value {
        Value::from_bytes(val.to_le_bytes().as_ref())
    }

    fn from_i64(val: i64) -> Value {
        Value::from_bytes(val.to_le_bytes().as_ref())
    }

    fn from_f32(val: f32) -> Value {
        Value::from_u32(val.to_bits())
    }

    fn from_f64(val: f64) -> Value {
        Value::from_u64(val.to_bits())
    }

    fn from_bool(val: bool) -> Value {
        Value::from_u8(unsafe { mem::transmute(val) })
    }
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
            Instr::Void => Value::Nothing,
            Instr::Const(konst) => {
                match *konst {
                    Const::Int { lit, ref ty } => match ty {
                        Type::Int { width, is_signed } => {
                            // We assume in the match below that pointer-sized ints are 64 bits
                            assert_eq!(self.prog.arch.pointer_size(), 64);
                            use IntWidth::*;
                            match (width, is_signed) {
                                (W8, false) => Value::from_u8(lit.try_into().unwrap()),
                                (W16, false) => Value::from_u16(lit.try_into().unwrap()),
                                (W32, false) => Value::from_u32(lit.try_into().unwrap()),
                                (W64, false) | (Pointer, false) => Value::from_u64(lit.try_into().unwrap()),

                                (W8, true) => Value::from_i8(lit.try_into().unwrap()),
                                (W16, true) => Value::from_i16(lit.try_into().unwrap()),
                                (W32, true) => Value::from_i32(lit.try_into().unwrap()),
                                (W64, true) | (Pointer, true) => Value::from_i64(lit.try_into().unwrap()),
                            }
                        },
                        _ => panic!("unexpected int constant type"),
                    },
                    Const::Float { lit, ref ty } => match ty.size(self.prog.arch) {
                        4 => Value::from_f32(lit as f32),
                        8 => Value::from_f64(lit.try_into().unwrap()),
                        _ => panic!("Unrecognized float constant size"),
                    },
                    Const::Bool(val) => Value::from_bool(val),
                    Const::Str { .. } => panic!("string constants are not yet supported"),
                }
            },
            Instr::Alloca(ty) => {
                let mut storage = Vec::new();
                storage.resize(ty.size(self.prog.arch), 0);
                Value::Dynamic(storage.into_boxed_slice())
            },
            &Instr::LogicalNot(val) => {
                let val = frame.results[val].as_bool();
                Value::from_bool(!val)
            },
            &Instr::Call { ref arguments, func } => {
                let mut copied_args = Vec::new();
                copied_args.reserve_exact(arguments.len());
                for &arg in arguments {
                    copied_args.push(frame.results[arg].clone());
                }
                self.call(func, copied_args)
            },
            // Intrinsic { arguments: SmallVec<[InstrId; 2]>, ty: Type, intr: Intrinsic },
            &Instr::Reinterpret(instr, _) => frame.results[instr].clone(),
            &Instr::Truncate(instr, ref ty) => {
                let bytes = frame.results[instr].as_bytes();
                Value::from_bytes(&bytes[0..ty.size(self.prog.arch)])
            },
            // SignExtend(InstrId, Type),
            // ZeroExtend(InstrId, Type),
            &Instr::FloatCast(instr, ref ty) => {
                let val = frame.results[instr].clone();
                match (val.as_bytes().len(), ty.size(self.prog.arch)) {
                    (x, y) if x == y => val,
                    (4, 8) => Value::from_f64(val.as_f32() as f64),
                    (8, 4) => Value::from_f32(val.as_f64() as f32),
                    (4, _) | (8, _) => panic!("Unexpected destination float cast type size"),
                    (_, 4) | (_, 8) => panic!("Unexpected source float cast type size"),
                    (_, _) => panic!("Unexpected float cast type sizes"),
                }
            },
            // FloatToInt(InstrId, Type),
            // IntToFloat(InstrId, Type),
            // Load(InstrId),
            &Instr::Store { location, value } => {
                let ptr = frame.results[location].as_raw_ptr();
                let val = frame.results[value].as_bytes();
                for (i, &byte) in val.iter().enumerate() {
                    println!("{}", byte);
                    unsafe { *(ptr.offset(i as isize)) = byte; }
                }
                Value::Nothing
            },
            // AddressOfStatic(StaticId),
            &Instr::Ret(instr) => {
                let val = mem::replace(&mut frame.results[instr], Value::Nothing);
                return Some(val)
            },
            &Instr::Br(bb) => {
                frame.pc = func.basic_blocks[bb];
                return None
            },
            &Instr::CondBr { condition, true_bb, false_bb } => {
                let condition = frame.results[condition].as_bool();
                let branch = if condition { true_bb } else { false_bb };
                frame.pc = func.basic_blocks[branch];
                return None
            }
            // /// Only valid at the beginning of a function, right after the void instruction
            // Parameter(Type),
            instr => panic!("Unrecognized instruction: {:?}", instr),
        };

        let (_, frame) = self.stack.last_mut().unwrap();
        frame.results[frame.pc] = val;
        frame.pc.advance();
        None
    }
}