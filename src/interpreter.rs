use std::alloc;
use std::collections::HashMap;
use std::convert::TryInto;
use std::ffi::{CStr, CString};
use std::mem;
use std::slice;

use smallvec::SmallVec;
use paste::paste;
use num_bigint::{BigInt, Sign};

use crate::builder::{Intrinsic, ModScopeId};
use crate::driver::Driver;
use crate::index_vec::{IdxVec, Idx};
use crate::mir::{self, Const, Function, FunctionRef, Instr, InstrId, StaticId, StrId};
use crate::ty::{Type, IntWidth, FloatWidth};

#[derive(Debug, Clone)]
pub enum InternalValue {
    Ty(Type),
    Mod(ModScopeId),
}

#[derive(Debug)]
pub enum Value {
    /// An inline value
    Inline(SmallVec<[u8; 64 / 8]>),
    /// A *pointer* to a piece of memory
    Dynamic(Box<[u8]>),
    Internal { val: InternalValue, indirection: u8 },
    Nothing,
}

impl Clone for Value {
    fn clone(&self) -> Value {
        match self {
            Value::Inline(storage) => Value::Inline(storage.clone()),
            Value::Dynamic(_) => Value::from_bytes(self.as_bytes()),
            &Value::Internal { ref val, indirection } => Value::Internal { val: val.clone(), indirection },
            Value::Nothing => Value::Nothing,
        }
    }
}

macro_rules! int_conversions {
    ($($ty_name:ty),+) => {
        impl Value {
            paste! {
                $(
                    #[allow(dead_code)]
                    fn [<as_ $ty_name>](&self) -> $ty_name {
                        $ty_name::from_le_bytes(self.as_bytes().try_into().unwrap())
                    }

                    #[allow(dead_code)]
                    fn [<from_ $ty_name>](val: $ty_name) -> Value {
                        Value::from_bytes(val.to_le_bytes().as_ref())
                    }
                )+
            }
        }
    }
}
int_conversions!(u8, u16, u32, u64, usize, i8, i16, i32, i64, isize);

impl Value {
    fn as_bytes(&self) -> &[u8] {
        match self {
            Value::Inline(storage) => storage.as_ref(),
            Value::Dynamic(ptr) => unsafe {
                let address_bits = mem::transmute::<&Box<_>, *const u8>(ptr);
                slice::from_raw_parts(address_bits, mem::size_of::<usize>())
            },
            Value::Internal { .. } => panic!("Can't get bytes of a compiler internal data structure!"),
            Value::Nothing => &[],
        }
    }

    /// Interprets the value as a pointer and loads from it
    fn load(&self, size: usize) -> Value {
        match self {
            Value::Inline(_) => {
                let ptr = self.as_raw_ptr();
                let mut buf = SmallVec::new();
                for i in 0..size {
                    buf.push(unsafe { *ptr.add(i) });
                }
                Value::Inline(buf)
            },
            Value::Dynamic(val) => {
                let mut buf = SmallVec::new();
                for &byte in &**val {
                    buf.push(byte);
                }
                Value::Inline(buf)
            },
            Value::Internal { val, indirection } => Value::Internal { val: val.clone(), indirection: indirection - 1 },
            Value::Nothing => panic!("can't load from nothing"),
        }
    }

    fn store(&mut self, val: Value) {
        match val {
            Value::Internal { val, indirection } => *self = Value::Internal { val, indirection: indirection + 1 },
            _ => {
                let ptr = self.as_raw_ptr();
                let val = val.as_bytes();
                for (i, &byte) in val.iter().enumerate() {
                    unsafe { *ptr.add(i) = byte; }
                }
            }
        }
    }

    fn as_big_int(&self, signed: bool) -> BigInt {
        if signed {
            BigInt::from_signed_bytes_le(self.as_bytes())
        } else {
            BigInt::from_bytes_le(Sign::Plus, self.as_bytes())
        }
    }

    fn as_raw_ptr(&self) -> *mut u8 {
        unsafe { mem::transmute(usize::from_le_bytes(self.as_bytes().try_into().unwrap())) }
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

    fn as_internal(&self) -> &InternalValue {
        match self {
            Value::Internal { val, indirection } => {
                assert_eq!(*indirection, 0, "can't get pointer to internal compiler data structure without dereferencing");
                val
            },
            _ => panic!("Can't get non-internal compiler data structure as internal compiler data structure"),
        }
    }

    fn as_ty(&self) -> &Type {
        match self.as_internal() {
            InternalValue::Ty(ty) => ty,
            _ => panic!("Can't get non-type as type"),
        }
    }

    fn as_mod(&self) -> ModScopeId {
        match *self.as_internal() {
            InternalValue::Mod(id) => id,
            _ => panic!("Can't get non-module as module"),
        }
    }

    fn from_bytes(bytes: &[u8]) -> Value {
        let mut storage = SmallVec::new();
        for &byte in bytes {
            storage.push(byte);
        }
        Value::Inline(storage)
    }

    fn from_big_int(big_int: BigInt, width: IntWidth, is_signed: bool, mir: &mir::Builder) -> Value {
        let size = match width {
            IntWidth::W8 => 1,
            IntWidth::W16 => 2,
            IntWidth::W32 => 4,
            IntWidth::W64 => 8,
            IntWidth::Pointer => mir.arch.pointer_size() / 8,
        };
        let (mut bytes, extension) = if is_signed {
            (
                big_int.to_signed_bytes_le(),
                match big_int.sign() {
                    Sign::Plus | Sign::NoSign => 0x00,
                    Sign::Minus => 0xFF,
                }
            )
        } else {
            let (sign, bytes) = big_int.to_bytes_le();
            assert!(!matches!(sign, Sign::Minus), "Can't put negative value in an unsigned int");
            (bytes, 0x00)
        };

        assert!(bytes.len() <= size, "Integer is too big");
        while bytes.len() < size {
            bytes.push(extension);
        }

        Value::from_bytes(&bytes)
    }

    fn from_const(konst: &Const, mir: &mir::Builder) -> Value {
        match *konst {
            Const::Int { lit, ref ty } => match ty {
                &Type::Int { width, is_signed } => Value::from_big_int(BigInt::from(lit), width, is_signed, mir),
                _ => panic!("unexpected int constant type {:?}", ty),
            },
            Const::Float { lit, ref ty } => match mir.size_of(ty) {
                4 => Value::from_f32(lit as f32),
                8 => Value::from_f64(lit.try_into().unwrap()),
                _ => panic!("Unrecognized float constant size"),
            },
            Const::Bool(val) => Value::from_bool(val),
            Const::Str { id, .. } => {
                let ptr = mir.strings[id].as_ptr();
                Value::from_usize(unsafe { mem::transmute(ptr) })
            },
            Const::Ty(ref ty) => Value::from_ty(ty.clone()),
            Const::Mod(id) => Value::from_mod(id),
        }
    }

    pub fn to_const(&self, ty: Type, strings: &mut IdxVec<StrId, CString>) -> Const {
        match ty {
            Type::Int { is_signed, .. } => {
                let big_int = self.as_big_int(is_signed);
                let lit = if is_signed {
                    let int: i64 = big_int.try_into().unwrap();
                    int as u64
                } else {
                    let int: u64 = big_int.try_into().unwrap();
                    int
                };
                Const::Int { lit, ty }
            },
            Type::Float(width) => {
                let lit = match width {
                    FloatWidth::W32 => self.as_f32() as f64,
                    FloatWidth::W64 => self.as_f64(),
                };
                Const::Float { lit, ty }
            },
            Type::Bool => Const::Bool(self.as_bool()),
            Type::Pointer(ref pointee) => {
                assert!(!pointee.is_mut);
                assert!(pointee.ty == Type::i8() || pointee.ty == Type::u8());
                println!("Warning: about to blindly copy a pointer into the global strings!");
                let string = unsafe { CString::from(CStr::from_ptr(self.as_raw_ptr() as *const _)) };
                let id = strings.push(string);
                Const::Str { id, ty }
            },
            Type::Ty => Const::Ty(self.as_ty().clone()),
            Type::Mod => Const::Mod(self.as_mod()),
            _ => panic!("Can't output value of type `{:?}` as constant", ty),
        }
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

    fn from_internal(val: InternalValue) -> Value {
        Value::Internal { val, indirection: 0 }
    }

    fn from_ty(ty: Type) -> Value {
        Self::from_internal(InternalValue::Ty(ty))
    }

    fn from_mod(id: ModScopeId) -> Value {
        Self::from_internal(InternalValue::Mod(id))
    }
}

struct StackFrame {
    pc: InstrId,
    results: IdxVec<InstrId, Value>,
}

#[derive(Copy, Clone)]
pub enum InterpMode {
    CompileTime,
    RunTime,
}

pub struct Interpreter {
    stack: Vec<(FunctionRef, StackFrame)>,
    statics: HashMap<StaticId, Value>,
    allocations: HashMap<usize, alloc::Layout>,
    pub mode: InterpMode,
}

impl Interpreter {
    pub fn new() -> Self {
        Self {
            stack: Vec::new(),
            statics: HashMap::new(),
            allocations: HashMap::new(),
            mode: InterpMode::CompileTime,
        }
    }
}

macro_rules! bin_op {
    ($salf:ident, $args:ident, $frame:ident, $func_ref:ident, $conv:ident, $first_ty:ident | $($ty:ident)|+, {$sign:tt}) => {{
        bin_op!(@preamble $salf, $args, $frame, $func_ref, lhs, rhs, ty, final_val);
        bin_op!(@kontinue $salf, ty, lhs, rhs, $conv, $first_ty | $($ty)|+, {$sign}, final_val);
        final_val.expect("Unexpected type for arguments")
    }};
    ($salf:ident, $args:ident, $frame:ident, $func_ref:ident, $conv:ident, $ty:ident, {$sign:tt}) => {{
        bin_op!(@preamble $salf, $args, $frame, $func_ref, lhs, rhs, ty, final_val);
        bin_op!(@kontinue $salf, ty, lhs, rhs, $conv, $ty, {$sign}, final_val);
        final_val.expect("Unexpected type for arguments")
    }};
    (@kontinue $salf:ident, $ty_var:ident, $lhs:ident, $rhs:ident, $conv:ident, $first_ty:ident | $($ty:ident)|+, {$sign:tt}, $final_val:ident) => {
        bin_op!(@kontinue $salf, $ty_var, $lhs, $rhs, $conv, $first_ty, {$sign}, $final_val);
        bin_op!(@kontinue $salf, $ty_var, $lhs, $rhs, $conv, $($ty)|+, {$sign}, $final_val);
    };
    (@kontinue $salf:ident, $ty:ident, $lhs:ident, $rhs:ident, $conv:ident, SignedInt, {$sign:tt}, $final_val:ident) => {
        if let Type::Int { width, is_signed } = $ty {
            // We assume in the match below that pointer-sized ints are 64 bits
            assert_eq!($salf.mir.arch.pointer_size(), 64);
            use IntWidth::*;
            match (width, is_signed) {
                (W8, true) => bin_op!(@out $final_val, $conv, i8, $lhs, $rhs, {$sign}),
                (W16, true) => bin_op!(@out $final_val, $conv, i16, $lhs, $rhs, {$sign}),
                (W32, true) => bin_op!(@out $final_val, $conv, i32, $lhs, $rhs, {$sign}),
                (W64, true) | (Pointer, true) => bin_op!(@out $final_val, $conv, i64, $lhs, $rhs, {$sign}),
                _ => {},
            }
        }
    };
    (@kontinue $salf:ident, $ty:ident, $lhs:ident, $rhs:ident, $conv:ident, UnsignedInt, {$sign:tt}, $final_val:ident) => {
        if let Type::Int { width, is_signed } = $ty {
            // We assume in the match below that pointer-sized ints are 64 bits
            assert_eq!($salf.mir.arch.pointer_size(), 64);
            use IntWidth::*;
            match (width, is_signed) {
                (W8, false) => bin_op!(@out $final_val, $conv, u8, $lhs, $rhs, {$sign}),
                (W16, false) => bin_op!(@out $final_val, $conv, u16, $lhs, $rhs, {$sign}),
                (W32, false) => bin_op!(@out $final_val, $conv, u32, $lhs, $rhs, {$sign}),
                (W64, false) | (Pointer, false) => bin_op!(@out $final_val, $conv, u64, $lhs, $rhs, {$sign}),
                _ => {},
            }
        }
    };
    (@kontinue $salf:ident, $ty:ident, $lhs:ident, $rhs:ident, $conv:ident, Float, {$sign:tt}, $final_val:ident) => {
        if let Type::Float(width) = $ty {
            match width {
                FloatWidth::W32 => bin_op!(@out $final_val, $conv, f32, $lhs, $rhs, {$sign}),
                FloatWidth::W64 => bin_op!(@out $final_val, $conv, f64, $lhs, $rhs, {$sign}),
            }
        }
    };
    (@kontinue $salf:ident, $ty:ident, $lhs:ident, $rhs:ident, $conv:ident, Bool, {$sign:tt}, $final_val:ident) => {
        if let Type::Bool = $ty {
            bin_op!(@out $final_val, $conv, $ty, $lhs, $rhs, {$sign});
        }
    };
    (@kontinue $salf:ident, $ty:ident, $lhs:ident, $rhs:ident, $conv:ident, Int, {$sign:tt}, $final_val:ident) => {
        bin_op!(@kontinue $salf, $ty, $lhs, $rhs, $conv, UnsignedInt | SignedInt, {$sign}, $final_val);
    };
    (@preamble $salf:ident, $args:ident, $frame:ident, $func_ref:ident, $lhs:ident, $rhs:ident, $ty:ident, $final_val:ident) => {
        assert_eq!($args.len(), 2);
        let ($lhs, $rhs) = ($args[0], $args[1]);
        let $ty = $salf.mir.type_of($lhs, &$func_ref);
        assert_eq!($ty, $salf.mir.type_of($rhs, &$func_ref));
        let ($lhs, $rhs) = (&$frame.results[$lhs], &$frame.results[$rhs]);
        let mut $final_val = None;
    };
    (@out $final_val:ident, no_convert, $ty:ident, $lhs:ident, $rhs:ident, {$sign:tt}) => {
        paste!($final_val = Some($lhs.[<as_ $ty>]() $sign $rhs.[<as_ $ty>]()));
    };
    (@out $final_val:ident, convert, $ty:ident, $lhs:ident, $rhs:ident, {$sign:tt}) => {
        paste!($final_val = Some(Value::[<from_ $ty>]($lhs.[<as_ $ty>]() $sign $rhs.[<as_ $ty>]())));
    };
    (@out $final_val:ident, bool_convert, $ty:ident, $lhs:ident, $rhs:ident, {$sign:tt}) => {
        paste!($final_val = Some(Value::from_bool($lhs.[<as_ $ty>]() $sign $rhs.[<as_ $ty>]())));
    };
}

impl Driver {
    fn new_stack_frame(&self, func: &Function, arguments: Vec<Value>) -> StackFrame {
        let mut results = IdxVec::new();
        results.resize_with(func.code.len(), || Value::Nothing);

        let num_parameters = func.num_parameters();
        if num_parameters != arguments.len() {
            let interner = &self.interner;
            let func_name = func.name.map(|name| interner.resolve(name).unwrap()).unwrap_or("<anonymous func>");
            panic!(
                "Compiler bug! Tried to call {} with {} arguments, but {} were expected.",
                func_name,
                arguments.len(),
                num_parameters
            );
        }
        for (i, arg) in arguments.into_iter().enumerate() {
            results.raw[i + 1] = arg;
        }
        StackFrame {
            pc: InstrId::new(num_parameters + 1),
            results,
        }
    }

    pub fn call(&mut self, func_ref: FunctionRef, arguments: Vec<Value>) -> Value {
        let func = self.mir.function_by_ref(&func_ref);
        let frame = self.new_stack_frame(func, arguments);
        self.interp.stack.push((func_ref, frame));
        loop {
            if let Some(val) = self.execute_next() {
                self.interp.stack.pop().unwrap();
                return val;
            }
        }
    }

    /// Execute the next instruction. Iff the instruction is a return, this function returns its `Value`
    fn execute_next(&mut self) -> Option<Value> {
        let (func_ref, frame) = self.interp.stack.last_mut().unwrap();
        let func = self.mir.function_by_ref(func_ref);
        let val = match &func.code[frame.pc] {
            Instr::Void => Value::Nothing,
            Instr::Const(konst) => Value::from_const(konst, &self.mir),
            Instr::Alloca(ty) => {
                let mut storage = Vec::new();
                storage.resize(self.mir.size_of(ty), 0);
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
                self.call(FunctionRef::Id(func), copied_args)
            },
            &Instr::Intrinsic { ref arguments, intr, .. } => {
                match intr {
                    Intrinsic::Mult => bin_op!(self, arguments, frame, func_ref, convert, Int | Float, {*}),
                    Intrinsic::Div => bin_op!(self, arguments, frame, func_ref, convert, Int | Float, {/}),
                    Intrinsic::Mod => bin_op!(self, arguments, frame, func_ref, convert, Int | Float, {%}),
                    Intrinsic::Add => bin_op!(self, arguments, frame, func_ref, convert, Int | Float, {+}),
                    Intrinsic::Sub => bin_op!(self, arguments, frame, func_ref, convert, Int | Float, {-}),
                    Intrinsic::Less => bin_op!(self, arguments, frame, func_ref, bool_convert, Int | Float, {<}),
                    Intrinsic::LessOrEq => bin_op!(self, arguments, frame, func_ref, bool_convert, Int | Float, {<=}),
                    Intrinsic::Greater => bin_op!(self, arguments, frame, func_ref, bool_convert, Int | Float, {>}),
                    Intrinsic::GreaterOrEq => bin_op!(self, arguments, frame, func_ref, bool_convert, Int | Float, {>=}),
                    Intrinsic::Eq => bin_op!(self, arguments, frame, func_ref, bool_convert, Int | Float | Bool, {==}),
                    Intrinsic::NotEq => bin_op!(self, arguments, frame, func_ref, bool_convert, Int | Float | Bool, {!=}),
                    Intrinsic::BitwiseAnd => bin_op!(self, arguments, frame, func_ref, convert, Int, {&}),
                    Intrinsic::BitwiseOr => bin_op!(self, arguments, frame, func_ref, convert, Int, {|}),
                    Intrinsic::LogicalNot => panic!("Unexpected logical not intrinsic, should've been replaced by instruction"),
                    Intrinsic::Neg => {
                        assert_eq!(arguments.len(), 1);
                        let arg = arguments[0];
                        let ty = self.mir.type_of(arg, &func_ref);
                        let arg = &frame.results[arg];
                        match ty {
                            Type::Int { width, is_signed } => {
                                Value::from_big_int(-arg.as_big_int(is_signed), width, is_signed, &self.mir)
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
                        let ty = self.mir.type_of(id, &func_ref);
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
                        assert_eq!(self.mir.arch.pointer_size(), 64);
                        let size = frame.results[arguments[0]].as_u64() as usize;
                        let layout = alloc::Layout::from_size_align(size, 8).unwrap();
                        let buf = unsafe { alloc::alloc(layout) };
                        let address: usize = unsafe { mem::transmute(buf) };
                        self.interp.allocations.insert(address, layout);
                        Value::from_usize(address)
                    }
                    Intrinsic::Free => {
                        assert_eq!(arguments.len(), 1);
                        assert_eq!(self.mir.arch.pointer_size(), 64);
                        let ptr = frame.results[arguments[0]].as_raw_ptr();
                        let address: usize = unsafe { mem::transmute(ptr) };
                        let layout = self.interp.allocations.remove(&address).unwrap();
                        unsafe { alloc::dealloc(ptr, layout) };
                        Value::Nothing
                    },
                    Intrinsic::I8 => Value::from_ty(Type::i8()),
                    Intrinsic::I16 => Value::from_ty(Type::i16()),
                    Intrinsic::I32 => Value::from_ty(Type::i32()),
                    Intrinsic::I64 => Value::from_ty(Type::i64()),
                    Intrinsic::Isize => Value::from_ty(Type::isize()),
                    Intrinsic::U8 => Value::from_ty(Type::u8()),
                    Intrinsic::U16 => Value::from_ty(Type::u16()),
                    Intrinsic::U32 => Value::from_ty(Type::u32()),
                    Intrinsic::U64 => Value::from_ty(Type::u64()),
                    Intrinsic::Usize => Value::from_ty(Type::usize()),
                    Intrinsic::F32 => Value::from_ty(Type::f32()),
                    Intrinsic::F64 => Value::from_ty(Type::f64()),
                    Intrinsic::Never => Value::from_ty(Type::Never),
                    Intrinsic::Bool => Value::from_ty(Type::Bool),
                    Intrinsic::Void => Value::from_ty(Type::Void),
                    Intrinsic::Ty => Value::from_ty(Type::Ty),
                    Intrinsic::Module => Value::from_ty(Type::Mod),
                    Intrinsic::PrintType => {
                        assert_eq!(arguments.len(), 1);
                        print!("{:?}", frame.results[arguments[0]].as_ty());
                        Value::Nothing
                    },
                    Intrinsic::AlignOf => {
                        assert_eq!(arguments.len(), 1);
                        let ty = frame.results[arguments[0]].as_ty();
                        Value::from_usize(self.mir.align_of(ty))
                    },
                    Intrinsic::StrideOf => {
                        assert_eq!(arguments.len(), 1);
                        let ty = frame.results[arguments[0]].as_ty();
                        Value::from_usize(self.mir.stride_of(ty))
                    },
                    Intrinsic::SizeOf => {
                        assert_eq!(arguments.len(), 1);
                        let ty = frame.results[arguments[0]].as_ty();
                        Value::from_usize(self.mir.size_of(ty))
                    },
                    Intrinsic::OffsetOf => {
                        assert_eq!(arguments.len(), 2);
                        let ty = frame.results[arguments[0]].as_ty();
                        let field_name = unsafe { CStr::from_ptr(frame.results[arguments[1]].as_raw_ptr() as *const _) };
                        let field_name = self.interner.get_or_intern(field_name.to_str().unwrap());
                        let mut offset = None;
                        match *ty {
                            Type::Struct(strukt) => {
                                for (index, &field) in self.hir.structs[strukt].fields.iter().enumerate() {
                                    if field_name == self.hir.field_decls[field].name {
                                        offset = Some(self.mir.structs[&strukt].layout.field_offsets[index]);
                                        break;
                                    }
                                }
                            }
                            _ => panic!("Can't get field offset on a non-struct type"),
                        }
                        let offset = offset.expect("No such field name in call to offset_of");
                        Value::from_usize(offset)
                    },
                    _ => panic!("Call to unimplemented intrinsic {:?}", intr),
                }
            },
            &Instr::Reinterpret(instr, _) => frame.results[instr].clone(),
            &Instr::Truncate(instr, ref ty) => {
                let bytes = frame.results[instr].as_bytes();
                let new_size = self.mir.size_of(ty);
                Value::from_bytes(&bytes[0..new_size])
            },
            &Instr::SignExtend(val, ref dest_ty) => {
                let src_ty = &self.mir.type_of(val, &func_ref);
                let val = &frame.results[val];
                match (src_ty, dest_ty) {
                    (
                        &Type::Int { is_signed: src_is_signed, .. },
                        &Type::Int { width: dest_width, is_signed: dest_is_signed }
                    ) => Value::from_big_int(val.as_big_int(src_is_signed), dest_width, dest_is_signed, &self.mir),
                    (_, _) => panic!("Invalid operand types to sign extension")
                }
            },
            &Instr::ZeroExtend(val, ref dest_ty) => {
                let src_ty = &self.mir.type_of(val, &func_ref);
                let val = &frame.results[val];
                match (src_ty, dest_ty) {
                    (
                        &Type::Int { is_signed: src_is_signed, .. },
                        &Type::Int { width: dest_width, is_signed: dest_is_signed }
                    ) => Value::from_big_int(val.as_big_int(src_is_signed), dest_width, dest_is_signed, &self.mir),
                    (_, _) => panic!("Invalid operand types to zero extension")
                }
            },
            &Instr::FloatCast(instr, ref ty) => {
                let val = &frame.results[instr];
                match (val.as_bytes().len(), self.mir.size_of(ty)) {
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
                let src_ty = self.mir.type_of(instr, &func_ref);
                let src_size = self.mir.size_of(&src_ty);

                match dest_ty {
                    &Type::Int { width, is_signed } => {
                        let big_int = if is_signed {
                            let int = match src_size * 8 {
                                32 => val.as_f32() as i64,
                                64 => val.as_f64() as i64,
                                _ => panic!("Invalid float size"),
                            };
                            BigInt::from(int)
                        } else {
                            let int = match src_size * 8 {
                                32 => val.as_f32() as u64,
                                64 => val.as_f64() as u64,
                                _ => panic!("Invalid float size"),
                            };
                            BigInt::from(int)
                        };
                        Value::from_big_int(big_int, width, is_signed, &self.mir)
                    },
                    _ => panic!("Invalid destination type in float to int cast: {:?}", dest_ty),
                }
            }
            &Instr::IntToFloat(instr, ref dest_ty) => {
                let val = &frame.results[instr];
                let src_ty = &self.mir.type_of(instr, &func_ref);
                let dest_size = self.mir.size_of(dest_ty);
                match src_ty {
                    &Type::Int { is_signed, .. } => {
                        let big_int = val.as_big_int(is_signed);
                        if is_signed {
                            let int: i64 = big_int.try_into().unwrap();
                            match dest_size * 8 {
                                32 => Value::from_f32(int as _),
                                64 => Value::from_f64(int as _),
                                _ => panic!("Invalid destination size in int to float cast"),
                            }
                        } else {
                            let int: u64 = big_int.try_into().unwrap();
                            match dest_size * 8 {
                                32 => Value::from_f32(int as _),
                                64 => Value::from_f64(int as _),
                                _ => panic!("Invalid destination size in int to float cast"),
                            }
                        }
                    },
                    _ => panic!("Invalid source type in int to float cast: {:?}", src_ty),
                }
            }
            &Instr::Load(location) => {
                let ty = self.mir.type_of(frame.pc, &func_ref);
                let size = self.mir.size_of(&ty);
                frame.results[location].load(size)
            },
            &Instr::Store { location, value } => {
                let val = frame.results[value].clone();
                frame.results[location].store(val);
                Value::Nothing
            },
            &Instr::AddressOfStatic(statik) => {
                if let InterpMode::CompileTime = self.interp.mode {
                    panic!("Can't access static at compile time!");
                }
                let mir = &self.mir;
                let statik = self.interp.statics.entry(statik)
                    .or_insert_with(|| Value::from_const(&mir.statics[statik], mir));
                Value::from_usize(unsafe { mem::transmute(statik.as_bytes().as_ptr()) })
            },
            &Instr::Pointer { op, is_mut } => {
                Value::from_ty(frame.results[op].as_ty().clone().ptr_with_mut(is_mut))
            },
            &Instr::Struct { ref fields, id } => {
                if !self.mir.structs.contains_key(&id) {
                    let mut field_tys = SmallVec::new();
                    for &field in fields {
                        field_tys.push(frame.results[field].as_ty().clone());
                    }
                    let layout = self.mir.layout_struct(&field_tys);
                    self.mir.structs.insert(
                        id,
                        mir::Struct {
                            field_tys,
                            layout,
                        }
                    );
                }
                Value::from_ty(Type::Struct(id))
            },
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
            },
            &Instr::DirectFieldAccess { val, index } => {
                let bytes = frame.results[val].as_bytes();
                let strukt = match self.mir.type_of(val, func_ref) {
                    Type::Struct(strukt) => strukt,
                    _ => panic!("Can't directly get field of non-struct"),
                };
                let ty = self.mir.type_of(frame.pc, func_ref);
                let size = self.mir.size_of(&ty);
                let offset = self.mir.structs[&strukt].layout.field_offsets[index];
                Value::from_bytes(&bytes[offset..(offset + size)])
            },
            Instr::Parameter(_) => panic!("Invalid parameter instruction in the middle of a function!"),
        };

        let (_, frame) = self.interp.stack.last_mut().unwrap();
        frame.results[frame.pc] = val;
        frame.pc.advance();
        None
    }
}
