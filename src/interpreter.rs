use std::alloc;
use std::collections::HashMap;
use std::convert::TryInto;
use std::ffi::CString;
use std::mem;
use std::slice;

use arrayvec::ArrayVec;

use crate::arch::Arch;
use crate::builder::Intrinsic;
use crate::index_vec::{IdxVec, Idx};
use crate::mir::{Const, Function, FuncId, Instr, InstrId, Program, StaticId, StrId};
use crate::ty::{Type, IntWidth, FloatWidth};


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

    fn from_const(konst: &Const, arch: Arch, strings: &IdxVec<CString, StrId>) -> Value {
        match *konst {
            Const::Int { lit, ref ty } => match ty {
                Type::Int { width, is_signed } => {
                    // We assume in the match below that pointer-sized ints are 64 bits
                    assert_eq!(arch.pointer_size(), 64);
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
                _ => panic!("unexpected int constant type {:?}", ty),
            },
            Const::Float { lit, ref ty } => match ty.size(arch) {
                4 => Value::from_f32(lit as f32),
                8 => Value::from_f64(lit.try_into().unwrap()),
                _ => panic!("Unrecognized float constant size"),
            },
            Const::Bool(val) => Value::from_bool(val),
            Const::Str { id, .. } => {
                let ptr = strings[id].as_ptr();
                Value::from_usize(unsafe { mem::transmute(ptr) })
            },
        }
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

    fn from_usize(val: usize) -> Value {
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
    statics: IdxVec<Value, StaticId>,
    allocations: HashMap<usize, alloc::Layout>,
    prog: &'mir Program,
}

impl<'mir> Interpreter<'mir> {
    pub fn new(prog: &'mir Program) -> Self {
        let mut statics = IdxVec::new();
        for statik in &prog.statics {
            statics.push(Value::from_const(statik, prog.arch, &prog.strings));
        }
        Self {
            stack: Vec::new(),
            statics,
            allocations: HashMap::new(),
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
        let func_id = *func_id;
        let func = &self.prog.comp_decls[func_id];
        let val = match &func.code[frame.pc] {
            Instr::Void => Value::Nothing,
            Instr::Const(konst) => Value::from_const(konst, self.prog.arch, &self.prog.strings),
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
            &Instr::Intrinsic { ref arguments, intr, .. } => {
                match intr {
                    // TODO: Reduce code duplication
                    Intrinsic::Mult => {
                        assert_eq!(arguments.len(), 2);
                        let (lhs, rhs) = (arguments[0], arguments[1]);
                        let ty = self.prog.type_of(lhs, func_id);
                        assert_eq!(ty, self.prog.type_of(rhs, func_id));
                        let (lhs, rhs) = (&frame.results[lhs], &frame.results[rhs]);
                        match ty {
                            Type::Int { width, is_signed } => {
                                // We assume in the match below that pointer-sized ints are 64 bits
                                assert_eq!(self.prog.arch.pointer_size(), 64);
                                use IntWidth::*;
                                match (width, is_signed) {
                                    (W8, true) => Value::from_i8(lhs.as_i8() * rhs.as_i8()),
                                    (W16, true) => Value::from_i16(lhs.as_i16() * rhs.as_i16()),
                                    (W32, true) => Value::from_i32(lhs.as_i32() * rhs.as_i32()),
                                    (W64, true) | (Pointer, true) => Value::from_i64(lhs.as_i64() * rhs.as_i64()),

                                    (W8, false) => Value::from_u8(lhs.as_u8() * rhs.as_u8()),
                                    (W16, false) => Value::from_u16(lhs.as_u16() * rhs.as_u16()),
                                    (W32, false) => Value::from_u32(lhs.as_u32() * rhs.as_u32()),
                                    (W64, false) | (Pointer, false) => Value::from_u64(lhs.as_u64() * rhs.as_u64()),
                                }
                            },
                            Type::Float(width) => match width {
                                FloatWidth::W32 => Value::from_f32(lhs.as_f32() * rhs.as_f32()),
                                FloatWidth::W64 => Value::from_f64(lhs.as_f64() * rhs.as_f64()),
                            },
                            _ => panic!("Unexpected type for intrinsic arguments"),
                        }
                    },
                    Intrinsic::Div => {
                        assert_eq!(arguments.len(), 2);
                        let (lhs, rhs) = (arguments[0], arguments[1]);
                        let ty = self.prog.type_of(lhs, func_id);
                        assert_eq!(ty, self.prog.type_of(rhs, func_id));
                        let (lhs, rhs) = (&frame.results[lhs], &frame.results[rhs]);
                        match ty {
                            Type::Int { width, is_signed } => {
                                // We assume in the match below that pointer-sized ints are 64 bits
                                assert_eq!(self.prog.arch.pointer_size(), 64);
                                use IntWidth::*;
                                match (width, is_signed) {
                                    (W8, true) => Value::from_i8(lhs.as_i8() / rhs.as_i8()),
                                    (W16, true) => Value::from_i16(lhs.as_i16() / rhs.as_i16()),
                                    (W32, true) => Value::from_i32(lhs.as_i32() / rhs.as_i32()),
                                    (W64, true) | (Pointer, true) => Value::from_i64(lhs.as_i64() / rhs.as_i64()),

                                    (W8, false) => Value::from_u8(lhs.as_u8() / rhs.as_u8()),
                                    (W16, false) => Value::from_u16(lhs.as_u16() / rhs.as_u16()),
                                    (W32, false) => Value::from_u32(lhs.as_u32() / rhs.as_u32()),
                                    (W64, false) | (Pointer, false) => Value::from_u64(lhs.as_u64() / rhs.as_u64()),
                                }
                            },
                            Type::Float(width) => match width {
                                FloatWidth::W32 => Value::from_f32(lhs.as_f32() / rhs.as_f32()),
                                FloatWidth::W64 => Value::from_f64(lhs.as_f64() / rhs.as_f64()),
                            },
                            _ => panic!("Unexpected type for intrinsic arguments"),
                        }
                    },
                    Intrinsic::Mod => {
                        assert_eq!(arguments.len(), 2);
                        let (lhs, rhs) = (arguments[0], arguments[1]);
                        let ty = self.prog.type_of(lhs, func_id);
                        assert_eq!(ty, self.prog.type_of(rhs, func_id));
                        let (lhs, rhs) = (&frame.results[lhs], &frame.results[rhs]);
                        match ty {
                            Type::Int { width, is_signed } => {
                                // We assume in the match below that pointer-sized ints are 64 bits
                                assert_eq!(self.prog.arch.pointer_size(), 64);
                                use IntWidth::*;
                                match (width, is_signed) {
                                    (W8, true) => Value::from_i8(lhs.as_i8() % rhs.as_i8()),
                                    (W16, true) => Value::from_i16(lhs.as_i16() % rhs.as_i16()),
                                    (W32, true) => Value::from_i32(lhs.as_i32() % rhs.as_i32()),
                                    (W64, true) | (Pointer, true) => Value::from_i64(lhs.as_i64() % rhs.as_i64()),

                                    (W8, false) => Value::from_u8(lhs.as_u8() % rhs.as_u8()),
                                    (W16, false) => Value::from_u16(lhs.as_u16() % rhs.as_u16()),
                                    (W32, false) => Value::from_u32(lhs.as_u32() % rhs.as_u32()),
                                    (W64, false) | (Pointer, false) => Value::from_u64(lhs.as_u64() % rhs.as_u64()),
                                }
                            },
                            Type::Float(width) => match width {
                                FloatWidth::W32 => Value::from_f32(lhs.as_f32() % rhs.as_f32()),
                                FloatWidth::W64 => Value::from_f64(lhs.as_f64() % rhs.as_f64()),
                            },
                            _ => panic!("Unexpected type for intrinsic arguments"),
                        }
                    },
                    Intrinsic::Add => {
                        assert_eq!(arguments.len(), 2);
                        let (lhs, rhs) = (arguments[0], arguments[1]);
                        let ty = self.prog.type_of(lhs, func_id);
                        assert_eq!(ty, self.prog.type_of(rhs, func_id));
                        let (lhs, rhs) = (&frame.results[lhs], &frame.results[rhs]);
                        match ty {
                            Type::Int { width, is_signed } => {
                                // We assume in the match below that pointer-sized ints are 64 bits
                                assert_eq!(self.prog.arch.pointer_size(), 64);
                                use IntWidth::*;
                                match (width, is_signed) {
                                    (W8, true) => Value::from_i8(lhs.as_i8() + rhs.as_i8()),
                                    (W16, true) => Value::from_i16(lhs.as_i16() + rhs.as_i16()),
                                    (W32, true) => Value::from_i32(lhs.as_i32() + rhs.as_i32()),
                                    (W64, true) | (Pointer, true) => Value::from_i64(lhs.as_i64() + rhs.as_i64()),

                                    (W8, false) => Value::from_u8(lhs.as_u8() + rhs.as_u8()),
                                    (W16, false) => Value::from_u16(lhs.as_u16() + rhs.as_u16()),
                                    (W32, false) => Value::from_u32(lhs.as_u32() + rhs.as_u32()),
                                    (W64, false) | (Pointer, false) => Value::from_u64(lhs.as_u64() + rhs.as_u64()),
                                }
                            },
                            Type::Float(width) => match width {
                                FloatWidth::W32 => Value::from_f32(lhs.as_f32() + rhs.as_f32()),
                                FloatWidth::W64 => Value::from_f64(lhs.as_f64() + rhs.as_f64()),
                            },
                            _ => panic!("Unexpected type for intrinsic arguments"),
                        }
                    },
                    Intrinsic::Sub => {
                        assert_eq!(arguments.len(), 2);
                        let (lhs, rhs) = (arguments[0], arguments[1]);
                        let ty = self.prog.type_of(lhs, func_id);
                        assert_eq!(ty, self.prog.type_of(rhs, func_id));
                        let (lhs, rhs) = (&frame.results[lhs], &frame.results[rhs]);
                        match ty {
                            Type::Int { width, is_signed } => {
                                // We assume in the match below that pointer-sized ints are 64 bits
                                assert_eq!(self.prog.arch.pointer_size(), 64);
                                use IntWidth::*;
                                match (width, is_signed) {
                                    (W8, true) => Value::from_i8(lhs.as_i8() - rhs.as_i8()),
                                    (W16, true) => Value::from_i16(lhs.as_i16() - rhs.as_i16()),
                                    (W32, true) => Value::from_i32(lhs.as_i32() - rhs.as_i32()),
                                    (W64, true) | (Pointer, true) => Value::from_i64(lhs.as_i64() - rhs.as_i64()),

                                    (W8, false) => Value::from_u8(lhs.as_u8() - rhs.as_u8()),
                                    (W16, false) => Value::from_u16(lhs.as_u16() - rhs.as_u16()),
                                    (W32, false) => Value::from_u32(lhs.as_u32() - rhs.as_u32()),
                                    (W64, false) | (Pointer, false) => Value::from_u64(lhs.as_u64() - rhs.as_u64()),
                                }
                            },
                            Type::Float(width) => match width {
                                FloatWidth::W32 => Value::from_f32(lhs.as_f32() - rhs.as_f32()),
                                FloatWidth::W64 => Value::from_f64(lhs.as_f64() - rhs.as_f64()),
                            },
                            _ => panic!("Unexpected type for intrinsic arguments"),
                        }
                    },
                    Intrinsic::Less => {
                        assert_eq!(arguments.len(), 2);
                        let (lhs, rhs) = (arguments[0], arguments[1]);
                        let ty = self.prog.type_of(lhs, func_id);
                        assert_eq!(ty, self.prog.type_of(rhs, func_id));
                        let (lhs, rhs) = (&frame.results[lhs], &frame.results[rhs]);
                        let val = match ty {
                            Type::Int { width, is_signed } => {
                                // We assume in the match below that pointer-sized ints are 64 bits
                                assert_eq!(self.prog.arch.pointer_size(), 64);
                                use IntWidth::*;
                                match (width, is_signed) {
                                    (W8, true) => lhs.as_i8() < rhs.as_i8(),
                                    (W16, true) => lhs.as_i16() < rhs.as_i16(),
                                    (W32, true) => lhs.as_i32() < rhs.as_i32(),
                                    (W64, true) | (Pointer, true) => lhs.as_i64() < rhs.as_i64(),

                                    (W8, false) => lhs.as_u8() < rhs.as_u8(),
                                    (W16, false) => lhs.as_u16() < rhs.as_u16(),
                                    (W32, false) => lhs.as_u32() < rhs.as_u32(),
                                    (W64, false) | (Pointer, false) => lhs.as_u64() < rhs.as_u64(),
                                }
                            },
                            Type::Float(width) => match width {
                                FloatWidth::W32 => lhs.as_f32() < rhs.as_f32(),
                                FloatWidth::W64 => lhs.as_f64() < rhs.as_f64(),
                            },
                            _ => panic!("Unexpected type for intrinsic arguments"),
                        };
                        Value::from_bool(val)
                    },
                    Intrinsic::LessOrEq => {
                        assert_eq!(arguments.len(), 2);
                        let (lhs, rhs) = (arguments[0], arguments[1]);
                        let ty = self.prog.type_of(lhs, func_id);
                        assert_eq!(ty, self.prog.type_of(rhs, func_id));
                        let (lhs, rhs) = (&frame.results[lhs], &frame.results[rhs]);
                        let val = match ty {
                            Type::Int { width, is_signed } => {
                                // We assume in the match below that pointer-sized ints are 64 bits
                                assert_eq!(self.prog.arch.pointer_size(), 64);
                                use IntWidth::*;
                                match (width, is_signed) {
                                    (W8, true) => lhs.as_i8() <= rhs.as_i8(),
                                    (W16, true) => lhs.as_i16() <= rhs.as_i16(),
                                    (W32, true) => lhs.as_i32() <= rhs.as_i32(),
                                    (W64, true) | (Pointer, true) => lhs.as_i64() <= rhs.as_i64(),

                                    (W8, false) => lhs.as_u8() <= rhs.as_u8(),
                                    (W16, false) => lhs.as_u16() <= rhs.as_u16(),
                                    (W32, false) => lhs.as_u32() <= rhs.as_u32(),
                                    (W64, false) | (Pointer, false) => lhs.as_u64() <= rhs.as_u64(),
                                }
                            },
                            Type::Float(width) => match width {
                                FloatWidth::W32 => lhs.as_f32() <= rhs.as_f32(),
                                FloatWidth::W64 => lhs.as_f64() <= rhs.as_f64(),
                            },
                            _ => panic!("Unexpected type for intrinsic arguments"),
                        };
                        Value::from_bool(val)
                    },
                    Intrinsic::Greater => {
                        assert_eq!(arguments.len(), 2);
                        let (lhs, rhs) = (arguments[0], arguments[1]);
                        let ty = self.prog.type_of(lhs, func_id);
                        assert_eq!(ty, self.prog.type_of(rhs, func_id));
                        let (lhs, rhs) = (&frame.results[lhs], &frame.results[rhs]);
                        let val = match ty {
                            Type::Int { width, is_signed } => {
                                // We assume in the match below that pointer-sized ints are 64 bits
                                assert_eq!(self.prog.arch.pointer_size(), 64);
                                use IntWidth::*;
                                match (width, is_signed) {
                                    (W8, true) => lhs.as_i8() > rhs.as_i8(),
                                    (W16, true) => lhs.as_i16() > rhs.as_i16(),
                                    (W32, true) => lhs.as_i32() > rhs.as_i32(),
                                    (W64, true) | (Pointer, true) => lhs.as_i64() > rhs.as_i64(),

                                    (W8, false) => lhs.as_u8() > rhs.as_u8(),
                                    (W16, false) => lhs.as_u16() > rhs.as_u16(),
                                    (W32, false) => lhs.as_u32() > rhs.as_u32(),
                                    (W64, false) | (Pointer, false) => lhs.as_u64() > rhs.as_u64(),
                                }
                            },
                            Type::Float(width) => match width {
                                FloatWidth::W32 => lhs.as_f32() > rhs.as_f32(),
                                FloatWidth::W64 => lhs.as_f64() > rhs.as_f64(),
                            },
                            _ => panic!("Unexpected type for intrinsic arguments"),
                        };
                        Value::from_bool(val)
                    },
                    Intrinsic::GreaterOrEq => {
                        assert_eq!(arguments.len(), 2);
                        let (lhs, rhs) = (arguments[0], arguments[1]);
                        let ty = self.prog.type_of(lhs, func_id);
                        assert_eq!(ty, self.prog.type_of(rhs, func_id));
                        let (lhs, rhs) = (&frame.results[lhs], &frame.results[rhs]);
                        let val = match ty {
                            Type::Int { width, is_signed } => {
                                // We assume in the match below that pointer-sized ints are 64 bits
                                assert_eq!(self.prog.arch.pointer_size(), 64);
                                use IntWidth::*;
                                match (width, is_signed) {
                                    (W8, true) => lhs.as_i8() >= rhs.as_i8(),
                                    (W16, true) => lhs.as_i16() >= rhs.as_i16(),
                                    (W32, true) => lhs.as_i32() >= rhs.as_i32(),
                                    (W64, true) | (Pointer, true) => lhs.as_i64() >= rhs.as_i64(),

                                    (W8, false) => lhs.as_u8() >= rhs.as_u8(),
                                    (W16, false) => lhs.as_u16() >= rhs.as_u16(),
                                    (W32, false) => lhs.as_u32() >= rhs.as_u32(),
                                    (W64, false) | (Pointer, false) => lhs.as_u64() >= rhs.as_u64(),
                                }
                            },
                            Type::Float(width) => match width {
                                FloatWidth::W32 => lhs.as_f32() >= rhs.as_f32(),
                                FloatWidth::W64 => lhs.as_f64() >= rhs.as_f64(),
                            },
                            _ => panic!("Unexpected type for intrinsic arguments"),
                        };
                        Value::from_bool(val)
                    },
                    Intrinsic::Eq => {
                        assert_eq!(arguments.len(), 2);
                        let (lhs, rhs) = (arguments[0], arguments[1]);
                        let ty = self.prog.type_of(lhs, func_id);
                        assert_eq!(ty, self.prog.type_of(rhs, func_id));
                        let (lhs, rhs) = (&frame.results[lhs], &frame.results[rhs]);
                        let val = match ty {
                            Type::Int { width, is_signed } => {
                                // We assume in the match below that pointer-sized ints are 64 bits
                                assert_eq!(self.prog.arch.pointer_size(), 64);
                                use IntWidth::*;
                                match (width, is_signed) {
                                    (W8, true) => lhs.as_i8() == rhs.as_i8(),
                                    (W16, true) => lhs.as_i16() == rhs.as_i16(),
                                    (W32, true) => lhs.as_i32() == rhs.as_i32(),
                                    (W64, true) | (Pointer, true) => lhs.as_i64() == rhs.as_i64(),

                                    (W8, false) => lhs.as_u8() == rhs.as_u8(),
                                    (W16, false) => lhs.as_u16() == rhs.as_u16(),
                                    (W32, false) => lhs.as_u32() == rhs.as_u32(),
                                    (W64, false) | (Pointer, false) => lhs.as_u64() == rhs.as_u64(),
                                }
                            },
                            Type::Float(width) => match width {
                                FloatWidth::W32 => lhs.as_f32() == rhs.as_f32(),
                                FloatWidth::W64 => lhs.as_f64() == rhs.as_f64(),
                            },
                            Type::Bool => lhs.as_bool() == rhs.as_bool(),
                            _ => panic!("Unexpected type for intrinsic arguments"),
                        };
                        Value::from_bool(val)
                    },
                    Intrinsic::NotEq => {
                        assert_eq!(arguments.len(), 2);
                        let (lhs, rhs) = (arguments[0], arguments[1]);
                        let ty = self.prog.type_of(lhs, func_id);
                        assert_eq!(ty, self.prog.type_of(rhs, func_id));
                        let (lhs, rhs) = (&frame.results[lhs], &frame.results[rhs]);
                        let val = match ty {
                            Type::Int { width, is_signed } => {
                                // We assume in the match below that pointer-sized ints are 64 bits
                                assert_eq!(self.prog.arch.pointer_size(), 64);
                                use IntWidth::*;
                                match (width, is_signed) {
                                    (W8, true) => lhs.as_i8() != rhs.as_i8(),
                                    (W16, true) => lhs.as_i16() != rhs.as_i16(),
                                    (W32, true) => lhs.as_i32() != rhs.as_i32(),
                                    (W64, true) | (Pointer, true) => lhs.as_i64() != rhs.as_i64(),

                                    (W8, false) => lhs.as_u8() != rhs.as_u8(),
                                    (W16, false) => lhs.as_u16() != rhs.as_u16(),
                                    (W32, false) => lhs.as_u32() != rhs.as_u32(),
                                    (W64, false) | (Pointer, false) => lhs.as_u64() != rhs.as_u64(),
                                }
                            },
                            Type::Float(width) => match width {
                                FloatWidth::W32 => lhs.as_f32() != rhs.as_f32(),
                                FloatWidth::W64 => lhs.as_f64() != rhs.as_f64(),
                            },
                            Type::Bool => lhs.as_bool() != rhs.as_bool(),
                            _ => panic!("Unexpected type for intrinsic arguments"),
                        };
                        Value::from_bool(val)
                    },
                    Intrinsic::BitwiseAnd => {
                        assert_eq!(arguments.len(), 2);
                        let (lhs, rhs) = (arguments[0], arguments[1]);
                        let ty = self.prog.type_of(lhs, func_id);
                        assert_eq!(ty, self.prog.type_of(rhs, func_id));
                        let (lhs, rhs) = (&frame.results[lhs], &frame.results[rhs]);
                        match ty {
                            Type::Int { width, is_signed } => {
                                // We assume in the match below that pointer-sized ints are 64 bits
                                assert_eq!(self.prog.arch.pointer_size(), 64);
                                use IntWidth::*;
                                match (width, is_signed) {
                                    (W8, true) => Value::from_i8(lhs.as_i8() & rhs.as_i8()),
                                    (W16, true) => Value::from_i16(lhs.as_i16() & rhs.as_i16()),
                                    (W32, true) => Value::from_i32(lhs.as_i32() & rhs.as_i32()),
                                    (W64, true) | (Pointer, true) => Value::from_i64(lhs.as_i64() & rhs.as_i64()),

                                    (W8, false) => Value::from_u8(lhs.as_u8() & rhs.as_u8()),
                                    (W16, false) => Value::from_u16(lhs.as_u16() & rhs.as_u16()),
                                    (W32, false) => Value::from_u32(lhs.as_u32() & rhs.as_u32()),
                                    (W64, false) | (Pointer, false) => Value::from_u64(lhs.as_u64() & rhs.as_u64()),
                                }
                            },
                            _ => panic!("Unexpected type for intrinsic arguments"),
                        }
                    },
                    Intrinsic::BitwiseOr => {
                        assert_eq!(arguments.len(), 2);
                        let (lhs, rhs) = (arguments[0], arguments[1]);
                        let ty = self.prog.type_of(lhs, func_id);
                        assert_eq!(ty, self.prog.type_of(rhs, func_id));
                        let (lhs, rhs) = (&frame.results[lhs], &frame.results[rhs]);
                        match ty {
                            Type::Int { width, is_signed } => {
                                // We assume in the match below that pointer-sized ints are 64 bits
                                assert_eq!(self.prog.arch.pointer_size(), 64);
                                use IntWidth::*;
                                match (width, is_signed) {
                                    (W8, true) => Value::from_i8(lhs.as_i8() | rhs.as_i8()),
                                    (W16, true) => Value::from_i16(lhs.as_i16() | rhs.as_i16()),
                                    (W32, true) => Value::from_i32(lhs.as_i32() | rhs.as_i32()),
                                    (W64, true) | (Pointer, true) => Value::from_i64(lhs.as_i64() | rhs.as_i64()),

                                    (W8, false) => Value::from_u8(lhs.as_u8() | rhs.as_u8()),
                                    (W16, false) => Value::from_u16(lhs.as_u16() | rhs.as_u16()),
                                    (W32, false) => Value::from_u32(lhs.as_u32() | rhs.as_u32()),
                                    (W64, false) | (Pointer, false) => Value::from_u64(lhs.as_u64() | rhs.as_u64()),
                                }
                            },
                            _ => panic!("Unexpected type for intrinsic arguments"),
                        }
                    },
                    Intrinsic::LogicalNot => panic!("Unexpected logical not intrinsic, should've been replaced by instruction"),
                    Intrinsic::Neg => {
                        assert_eq!(arguments.len(), 1);
                        let arg = arguments[0];
                        let ty = self.prog.type_of(arg, func_id);
                        let arg = &frame.results[arg];
                        match ty {
                            Type::Int { width, is_signed } => {
                                // We assume in the match below that pointer-sized ints are 64 bits
                                assert_eq!(self.prog.arch.pointer_size(), 64);
                                assert!(is_signed);
                                match width {
                                    IntWidth::W8 => Value::from_i8(-arg.as_i8()),
                                    IntWidth::W16 => Value::from_i16(-arg.as_i16()),
                                    IntWidth::W32 => Value::from_i32(-arg.as_i32()),
                                    IntWidth::W64 | IntWidth::Pointer => Value::from_i64(-arg.as_i64()),
                                }
                            },
                            Type::Float(width) => match width {
                                FloatWidth::W32 => Value::from_f32(-arg.as_f32()),
                                FloatWidth::W64 => Value::from_f64(-arg.as_f64()),
                            },
                            _ => panic!("Unexpected type for intrinsic arguments"),
                        }
                    },
                    Intrinsic::Pos => {
                        assert_eq!(arguments.len(), 1);
                        frame.results[arguments[0]].clone()
                    },
                    Intrinsic::Panic => {
                        assert!(arguments.len() <= 1);
                        let panic_msg = "Userspace panic";
                        if let Some(&msg) = arguments.first() {
                            let mut ptr = frame.results[msg].as_raw_ptr();
                            let mut msg = format!("{}: ", panic_msg);
                            unsafe {
                                while *ptr != 0 {
                                    msg.push(*ptr as char);
                                    ptr = ptr.offset(1);
                                }
                            }
                            panic!(msg)
                        } else {
                            panic!(panic_msg)
                        }
                    },
                    Intrinsic::Print => {
                        assert_eq!(arguments.len(), 1);
                        let id = arguments[0];
                        let val = &frame.results[id];
                        let ty = self.prog.type_of(id, func_id);
                        match ty {
                            Type::Pointer(_) => unsafe {
                                let mut ptr = val.as_raw_ptr();
                                while *ptr != 0 {
                                    print!("{}", *ptr as char);
                                    ptr = ptr.offset(1);
                                }
                            },
                            Type::Int { .. } => print!("{}", val.as_u8() as char),
                            _ => panic!("Unexpected type passed to `print`: {:?}", ty),
                        }
                        Value::Nothing 
                    },
                    Intrinsic::Malloc => {
                        assert_eq!(arguments.len(), 1);
                        assert_eq!(self.prog.arch.pointer_size(), 64);
                        let size = frame.results[arguments[0]].as_u64() as usize;
                        let layout = alloc::Layout::from_size_align(size, 8).unwrap();
                        let buf = unsafe { alloc::alloc(layout) };
                        let address: usize = unsafe { mem::transmute(buf) };
                        self.allocations.insert(address, layout);
                        Value::from_usize(address)
                    }
                    Intrinsic::Free => {
                        assert_eq!(arguments.len(), 1);
                        assert_eq!(self.prog.arch.pointer_size(), 64);
                        let ptr = frame.results[arguments[0]].as_raw_ptr();
                        let address: usize = unsafe { mem::transmute(ptr) };
                        let layout = self.allocations.remove(&address).unwrap();
                        unsafe { alloc::dealloc(ptr, layout) };
                        Value::Nothing
                    },
                    _ => panic!("Call to unimplemented intrinsic {:?}", intr),
                }
            },
            &Instr::Reinterpret(instr, _) => frame.results[instr].clone(),
            &Instr::Truncate(instr, ref ty) => {
                let bytes = frame.results[instr].as_bytes();
                Value::from_bytes(&bytes[0..ty.size(self.prog.arch)])
            },
            &Instr::SignExtend(val, ref dest_ty) => {
                let src_ty = &self.prog.type_of(val, func_id);
                let val = &frame.results[val];
                match (src_ty, dest_ty) {
                    (
                        Type::Int { width: src_width, is_signed: src_is_signed },
                        Type::Int { width: dest_width, is_signed: dest_is_signed }
                    ) => {
                        assert!(dest_is_signed);
                        match (src_width.bit_width(self.prog.arch), src_is_signed, dest_width.bit_width(self.prog.arch)) {
                            (s, _, d) if s == d => panic!("Can't sign-extend to the same size"),
                            (8, true, 16) => Value::from_i16(val.as_i8().try_into().unwrap()),
                            (8, true, 32) => Value::from_i32(val.as_i8().try_into().unwrap()),
                            (8, true, 64) => Value::from_i64(val.as_i8().try_into().unwrap()),
                            (16, true, 32) => Value::from_i32(val.as_i16().try_into().unwrap()),
                            (16, true, 64) => Value::from_i64(val.as_i16().try_into().unwrap()),
                            (32, true, 64) => Value::from_i64(val.as_i32().try_into().unwrap()),

                            (8, false, 16) => Value::from_i16(val.as_u8().try_into().unwrap()),
                            (8, false, 32) => Value::from_i32(val.as_u8().try_into().unwrap()),
                            (8, false, 64) => Value::from_i64(val.as_u8().try_into().unwrap()),
                            (16, false, 32) => Value::from_i32(val.as_u16().try_into().unwrap()),
                            (16, false, 64) => Value::from_i64(val.as_u16().try_into().unwrap()),
                            (32, false, 64) => Value::from_i64(val.as_u32().try_into().unwrap()),

                            (_, _, _) => panic!("Invalid int types in sign extend"),
                        }
                    },
                    (_, _) => panic!("Invalid operand types to sign extension")
                }
            },
            &Instr::ZeroExtend(val, ref dest_ty) => {
                let src_ty = &self.prog.type_of(val, func_id);
                let val = &frame.results[val];
                match (src_ty, dest_ty) {
                    (
                        Type::Int { width: src_width, is_signed: src_is_signed },
                        Type::Int { width: dest_width, is_signed: dest_is_signed }
                    ) => {
                        assert!(!dest_is_signed);
                        match (src_width.bit_width(self.prog.arch), src_is_signed, dest_width.bit_width(self.prog.arch)) {
                            (s, _, d) if s == d => panic!("Can't zero-extend to the same size"),
                            (8, true, 16) => Value::from_u16(val.as_i8().try_into().unwrap()),
                            (8, true, 32) => Value::from_u32(val.as_i8().try_into().unwrap()),
                            (8, true, 64) => Value::from_u64(val.as_i8().try_into().unwrap()),
                            (16, true, 32) => Value::from_u32(val.as_i16().try_into().unwrap()),
                            (16, true, 64) => Value::from_u64(val.as_i16().try_into().unwrap()),
                            (32, true, 64) => Value::from_u64(val.as_i32().try_into().unwrap()),

                            (8, false, 16) => Value::from_u16(val.as_u8().try_into().unwrap()),
                            (8, false, 32) => Value::from_u32(val.as_u8().try_into().unwrap()),
                            (8, false, 64) => Value::from_u64(val.as_u8().try_into().unwrap()),
                            (16, false, 32) => Value::from_u32(val.as_u16().try_into().unwrap()),
                            (16, false, 64) => Value::from_u64(val.as_u16().try_into().unwrap()),
                            (32, false, 64) => Value::from_u64(val.as_u32().try_into().unwrap()),

                            (_, _, _) => panic!("Invalid int types in zero extend"),
                        }
                    },
                    (_, _) => panic!("Invalid operand types to zero extension")
                }
            },
            &Instr::FloatCast(instr, ref ty) => {
                let val = &frame.results[instr];
                match (val.as_bytes().len(), ty.size(self.prog.arch)) {
                    (x, y) if x == y => val.clone(),
                    (4, 8) => Value::from_f64(val.as_f32() as f64),
                    (8, 4) => Value::from_f32(val.as_f64() as f32),
                    (4, _) | (8, _) => panic!("Unexpected destination float cast type size"),
                    (_, 4) | (_, 8) => panic!("Unexpected source float cast type size"),
                    (_, _) => panic!("Unexpected float cast type sizes"),
                }
            },
            &Instr::FloatToInt(instr, ref dest_ty) => {
                let val = &frame.results[instr];
                let src_size = self.prog.type_of(instr, func_id).size(self.prog.arch);
                match dest_ty {
                    Type::Int { width, is_signed } => {
                        match (src_size * 8, width.bit_width(self.prog.arch), is_signed) {
                            (32, 8, true) => Value::from_i8(val.as_f32() as _),
                            (32, 16, true) => Value::from_i16(val.as_f32() as _),
                            (32, 32, true) => Value::from_i32(val.as_f32() as _),
                            (32, 64, true) => Value::from_i64(val.as_f32() as _),

                            (32, 8, false) => Value::from_u8(val.as_f32() as _),
                            (32, 16, false) => Value::from_u16(val.as_f32() as _),
                            (32, 32, false) => Value::from_u32(val.as_f32() as _),
                            (32, 64, false) => Value::from_u64(val.as_f32() as _),

                            (64, 8, true) => Value::from_i8(val.as_f64() as _),
                            (64, 16, true) => Value::from_i16(val.as_f64() as _),
                            (64, 32, true) => Value::from_i32(val.as_f64() as _),
                            (64, 64, true) => Value::from_i64(val.as_f64() as _),

                            (64, 8, false) => Value::from_u8(val.as_f64() as _),
                            (64, 16, false) => Value::from_u16(val.as_f64() as _),
                            (64, 32, false) => Value::from_u32(val.as_f64() as _),
                            (64, 64, false) => Value::from_u64(val.as_f64() as _),

                            (_, _, _) => panic!("Invalid float to int operand sizes"),
                        }
                    },
                    _ => panic!("Invalid destination type in float to int cast: {:?}", dest_ty),
                }
            }
            &Instr::IntToFloat(instr, ref dest_ty) => {
                let val = &frame.results[instr];
                let src_ty = &self.prog.type_of(instr, func_id);
                let dest_size = dest_ty.size(self.prog.arch);
                match src_ty {
                    Type::Int { width, is_signed } => {
                        match (width.bit_width(self.prog.arch), is_signed, dest_size * 8) {
                            (8, true, 32) => Value::from_f32(val.as_i8() as _),
                            (16, true, 32) => Value::from_f32(val.as_i16() as _),
                            (32, true, 32) => Value::from_f32(val.as_i32() as _),
                            (64, true, 32) => Value::from_f32(val.as_i64() as _),

                            (8, false, 32) => Value::from_f32(val.as_u8() as _),
                            (16, false, 32) => Value::from_f32(val.as_u16() as _),
                            (32, false, 32) => Value::from_f32(val.as_u32() as _),
                            (64, false, 32) => Value::from_f32(val.as_u64() as _),

                            (8, true, 64) => Value::from_f64(val.as_i8() as _),
                            (16, true, 64) => Value::from_f64(val.as_i16() as _),
                            (32, true, 64) => Value::from_f64(val.as_i32() as _),
                            (64, true, 64) => Value::from_f64(val.as_i64() as _),

                            (8, false, 64) => Value::from_f64(val.as_u8() as _),
                            (16, false, 64) => Value::from_f64(val.as_u16() as _),
                            (32, false, 64) => Value::from_f64(val.as_u32() as _),
                            (64, false, 64) => Value::from_f64(val.as_u64() as _),

                            (_, _, _) => panic!("Invalid int to float operand sizes"),
                        }
                    },
                    _ => panic!("Invalid source type in int to float cast: {:?}", src_ty),
                }
            }
            &Instr::Load(location) => {
                let size = self.prog.type_of(frame.pc, func_id).size(self.prog.arch);
                let ptr = frame.results[location].as_raw_ptr();
                let mut buf = ArrayVec::new();
                for i in 0..size {
                    buf.push(unsafe { *ptr.add(i) });
                }
                Value::Inline(buf)
            },
            &Instr::Store { location, value } => {
                let ptr = frame.results[location].as_raw_ptr();
                let val = frame.results[value].as_bytes();
                for (i, &byte) in val.iter().enumerate() {
                    unsafe { *ptr.add(i) = byte; }
                }
                Value::Nothing
            },
            &Instr::AddressOfStatic(statik) => Value::from_usize(unsafe { mem::transmute(self.statics[statik].as_bytes().as_ptr()) }),
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
            Instr::Parameter(_) => panic!("Invalid parameter instruction in the middle of a function!"),
        };

        let (_, frame) = self.stack.last_mut().unwrap();
        frame.results[frame.pc] = val;
        frame.pc.advance();
        None
    }
}
