use std::alloc;
use std::collections::HashMap;
use std::convert::TryInto;
use std::ffi::{CStr, CString};
use std::mem;
use std::slice;
use std::fmt::Write;

use indenter::indented;
use smallvec::SmallVec;
use paste::paste;
use num_bigint::{BigInt, Sign};
use display_adapter::display_adapter;

use dire::arch::Arch;
use dire::hir::{Intrinsic, ModScopeId, StructId, EnumId, GenericParamId, ExternFunctionRef};
use dire::mir::{Const, Instr, StaticId, Struct, VOID_INSTR};
use dire::ty::{Type, QualType, IntWidth, FloatWidth};
use dire::{OpId, BlockId};

use crate::driver::Driver;
use crate::mir::{FunctionRef, function_by_ref};
use crate::typechecker::type_provider::TypeProvider;

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

impl Driver {
    fn eval_struct_lit(&self, id: StructId, fields: impl Iterator<Item=Value>) -> Value {
        let strukt = &self.code.mir_code.structs[&id];
        let mut buf = SmallVec::new();
        buf.resize(strukt.layout.size, 0);
        for (i, field) in fields.enumerate() {
            let offset = strukt.layout.field_offsets[i];
            let ty = &strukt.field_tys[i];
            let size = self.size_of(ty);
            let val = field.as_bytes();
            buf[offset..(offset + size)].copy_from_slice(val);
        }
        Value::Inline(buf)
    }
}

struct Enum {
    discriminant: u32,
}

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

    pub fn as_big_int(&self, signed: bool) -> BigInt {
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

    fn as_enum(&self) -> Enum {
        Enum {
            discriminant: self.as_u32()
        }
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

    fn as_ty(&self) -> Type {
        match self.as_internal() {
            InternalValue::Ty(ty) => ty.clone(),
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

    fn from_big_int(big_int: BigInt, width: IntWidth, is_signed: bool, arch: Arch) -> Value {
        let size = match width {
            IntWidth::W8 => 1,
            IntWidth::W16 => 2,
            IntWidth::W32 => 4,
            IntWidth::W64 => 8,
            IntWidth::Pointer => arch.pointer_size() / 8,
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

    pub fn from_const(konst: &Const, driver: &Driver) -> Value {
        match *konst {
            Const::Int { lit, ref ty } => match ty {
                &Type::Int { width, is_signed } => Value::from_big_int(BigInt::from(lit), width, is_signed, driver.arch),
                _ => panic!("unexpected int constant type {:?}", ty),
            },
            Const::Float { lit, ref ty } => match driver.size_of(ty) {
                4 => Value::from_f32(lit as f32),
                8 => Value::from_f64(lit.try_into().unwrap()),
                _ => panic!("Unrecognized float constant size"),
            },
            Const::Bool(val) => Value::from_bool(val),
            Const::Str { id, .. } => {
                let ptr = driver.code.mir_code.strings[id].as_ptr();
                Value::from_usize(unsafe { mem::transmute(ptr) })
            },
            Const::Ty(ref ty) => Value::from_ty(ty.clone()),
            Const::Void => Value::Nothing,
            Const::Mod(id) => Value::from_mod(id),
            Const::BasicVariant { enuum, index } => Value::from_variant(driver, enuum, index, Value::Nothing),
            Const::StructLit { ref fields, id } => {
                let fields = fields.iter().map(|val| Value::from_const(val, driver));
                driver.eval_struct_lit(id, fields)
            }
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

    fn from_variant(d: &Driver, enuum: EnumId, index: usize, payload: Value) -> Value {
        let layout = &d.code.mir_code.enums[&enuum];
        let payload_offset = layout.payload_offsets[index];
        let mut bytes = Vec::new();
        bytes.extend(Value::from_u32(index as u32).as_bytes());
        while bytes.len() < payload_offset {
            bytes.push(0);
        }
        bytes.extend(payload.as_bytes());
        Value::from_bytes(&bytes)
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
    func_ref: FunctionRef,
    block: BlockId,
    pc: usize,
    results: HashMap<OpId, Value>,
    generic_ctx: HashMap<GenericParamId, Type>,
}

impl StackFrame {
    fn branch_to(&mut self, bb: BlockId) {
        self.block = bb;
        self.pc = 0;
    }

    fn canonicalize_type(&self, ty: &Type) -> Type {
        match ty {
            &Type::GenericParam(id) => {
                if let Some(result) = self.generic_ctx.get(&id) {
                    result.clone()
                } else {
                    ty.clone()
                }
            },
            Type::Pointer(pointee) =>
                Type::Pointer(
                    Box::new(QualType { ty: self.canonicalize_type(&pointee.ty), is_mut: pointee.is_mut })
                ),
            ty => ty.clone(),
        }
    }
}

#[derive(Copy, Clone)]
pub enum InterpMode {
    CompileTime,
    RunTime,
}

pub struct Interpreter {
    stack: Vec<StackFrame>,
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
    ($salf:ident, $args:ident, $conv:ident, $first_ty:ident | $($ty:ident)|+, {$sign:tt}) => {{
        bin_op!(@preamble $salf, $args, lhs, rhs, ty, final_val);
        bin_op!(@kontinue $salf, ty, lhs, rhs, $conv, $first_ty | $($ty)|+, {$sign}, final_val);
        final_val.expect("Unexpected type for arguments")
    }};
    ($salf:ident, $args:ident, $conv:ident, $ty:ident, {$sign:tt}) => {{
        bin_op!(@preamble $salf, $args, lhs, rhs, ty, final_val);
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
            assert_eq!($salf.arch.pointer_size(), 64);
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
            assert_eq!($salf.arch.pointer_size(), 64);
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
    (@preamble $salf:ident, $args:ident, $lhs:ident, $rhs:ident, $ty:ident, $final_val:ident) => {
        let frame = $salf.interp.stack.last().unwrap();
        assert_eq!($args.len(), 2);
        let ($lhs, $rhs) = ($args[0], $args[1]);
        let $ty = $salf.type_of($lhs);
        assert_eq!($ty, $salf.type_of($rhs));
        let ($lhs, $rhs) = (&frame.results[&$lhs], &frame.results[&$rhs]);
        let mut $final_val = None;
    };
    (@out $final_val:ident, no_convert, $ty:ident, $lhs:ident, $rhs:ident, {$sign:tt}) => {
        paste!($final_val = Some($lhs.[<as_ $ty>]() $sign $rhs.[<as_ $ty>]()));
    };
    (@out $final_val:ident, convert, $ty:ident, $lhs:ident, $rhs:ident, {$sign:tt}) => {
        paste!($final_val = Some(Value::[<from_ $ty>]($lhs.[<as_ $ty>]() $sign $rhs.[<as_ $ty>]())))
    };
    (@out $final_val:ident, bool_convert, $ty:ident, $lhs:ident, $rhs:ident, {$sign:tt}) => {
        paste!($final_val = Some(Value::from_bool($lhs.[<as_ $ty>]() $sign $rhs.[<as_ $ty>]())))
    };
}

impl Driver {
    pub fn value_to_const(&mut self, val: Value, ty: Type, tp: &impl TypeProvider) -> Const {
        match ty {
            Type::Int { is_signed, .. } => {
                let big_int = val.as_big_int(is_signed);
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
                    FloatWidth::W32 => val.as_f32() as f64,
                    FloatWidth::W64 => val.as_f64(),
                };
                Const::Float { lit, ty }
            },
            Type::Bool => Const::Bool(val.as_bool()),
            Type::Pointer(ref pointee) => {
                assert!(!pointee.is_mut);
                assert!(pointee.ty == Type::i8() || pointee.ty == Type::u8());
                println!("Warning: about to blindly copy a pointer into the global strings!");
                let string = unsafe { CString::from(CStr::from_ptr(val.as_raw_ptr() as *const _)) };
                let id = self.code.mir_code.strings.push(string);
                Const::Str { id, ty }
            },
            Type::Ty => Const::Ty(val.as_ty().clone()),
            Type::Mod => Const::Mod(val.as_mod()),
            Type::Struct(id) => {
                // Yay borrow checker
                let strukt = self.code.mir_code.structs[&id].clone();
                let buf = val.as_bytes();
                let mut fields = Vec::new();
                for i in 0..strukt.field_tys.len() {
                    let offset = strukt.layout.field_offsets[i];
                    let ty = strukt.field_tys[i].clone();
                    let size = self.size_of(&ty);
                    let val = Value::from_bytes(&buf[offset..(offset+size)]);
                    let konst = self.value_to_const(val, ty.clone(), tp);
                    fields.push(konst);
                }
                Const::StructLit { fields, id }
            },
            Type::Void => Const::Void,
            _ => panic!("Can't output value of type `{:?}` as constant", ty),
        }
    }

    fn new_stack_frame(&self, func_ref: FunctionRef, arguments: Vec<Value>, generic_arguments: Vec<Type>) -> StackFrame {
        let func = function_by_ref(&self.code.mir_code, &func_ref);

        let mut results = HashMap::new();

        let num_parameters = self.code.num_parameters(func);
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
        let start_block = func.blocks[0];
        for (i, arg) in arguments.into_iter().enumerate() {
            let op = self.code.blocks[start_block].ops[i];
            let param = self.code.ops[op].as_mir_instr().unwrap();
            assert!(matches!(param, Instr::Parameter(_)));
            results.insert(op, arg);
        }
        results.insert(VOID_INSTR, Value::Nothing);

        let mut generic_ctx = HashMap::new();
        assert_eq!(func.generic_params.len(), generic_arguments.len());
        for (&generic_param, generic_argument) in func.generic_params.iter().zip(generic_arguments) {
            generic_ctx.insert(generic_param, generic_argument);
        }

        StackFrame {
            func_ref,
            block: start_block,
            pc: num_parameters,
            generic_ctx,
            results,
        }
    }

    #[display_adapter]
    pub fn stack_trace(&self, f: &mut Formatter) {
        for (i, frame) in self.interp.stack.iter().rev().enumerate() {
            let func = function_by_ref(&self.code.mir_code, &frame.func_ref);
            writeln!(f, "{}: {}", i, self.fn_name(func.name))?;
        }
        Ok(())
    }

    pub fn call(&mut self, func_ref: FunctionRef, arguments: Vec<Value>, generic_arguments: Vec<Type>) -> Value {
        let frame = self.new_stack_frame(func_ref, arguments, generic_arguments);
        self.interp.stack.push(frame);
        loop {
            if let Some(val) = self.execute_next() {
                self.interp.stack.pop().unwrap();
                return val;
            }
        }
    }
    
    pub fn extern_call(&mut self, func_ref: ExternFunctionRef, mut args: Vec<Box<[u8]>>) -> Value {
        let indirect_args: Vec<*mut u8> = args.iter_mut()
        .map(|arg| arg.as_mut_ptr())
        .collect();
        
        let library = &self.code.mir_code.extern_mods[&func_ref.extern_mod];
        let func = &library.imported_functions[func_ref.index];
        let module = unsafe { kernel32::LoadLibraryA(library.library_path.as_ptr()) };
        let func_name = CString::new(func.name.clone()).unwrap();
        let func_ptr = unsafe { kernel32::GetProcAddress(module, func_name.as_ptr()) };
        let func_address: u64 = unsafe { std::mem::transmute(func_ptr) };
        
        let mut thunk_data: Vec<u8> = Vec::new();
        // mov QWORD PTR [rsp+16], rdx
        thunk_data.push(0x48);
        thunk_data.push(0x89);
        thunk_data.push(0x54);
        thunk_data.push(0x24);
        thunk_data.push(0x10);

        // mov QWORD PTR [rsp+8], rcx
        thunk_data.push(0x48);
        thunk_data.push(0x89);
        thunk_data.push(0x4C);
        thunk_data.push(0x24);
        thunk_data.push(0x08);

        let mut extension: u8 = 40;
        if args.len() > 4 {
            extension += ((args.len() - 3) / 2 * 16) as u8;
        }
        // sub rsp, extension
        thunk_data.push(0x48);
        thunk_data.push(0x83);
        thunk_data.push(0xEC);
        thunk_data.push(extension);

        assert_eq!(args.len(), func.param_tys.len());
        for i in (0..args.len()).rev() {
            // mov rax, QWORD PTR [rsp+extension+8]   (get pointer to arguments)
            thunk_data.push(0x48);
            thunk_data.push(0x8B);
            thunk_data.push(0x44);
            thunk_data.push(0x24);
            thunk_data.push(extension + 8);

            // mov rax, QWORD PTR [rax+i*8]             (get pointer to i'th argument)
            thunk_data.push(0x48);
            thunk_data.push(0x8B);
            thunk_data.push(0x40);
            thunk_data.push((i * 8) as u8);

            match func.param_tys[i] {
                Type::Int { width: IntWidth::W32, .. } => {
                    match i {
                        0 => {
                            // mov ecx, DWORD PTR [rax]       (read i'th argument as a 32-bit value)
                            thunk_data.push(0x8B);
                            thunk_data.push(0x08);
                        },
                        1 => {
                            // mov edx, DWORD PTR [rax]       (read i'th argument as a 32-bit value)
                            thunk_data.push(0x8B);
                            thunk_data.push(0x10);
                        },
                        2 => {
                            // mov r8d, DWORD PTR [rax]       (read i'th argument as a 32-bit value)
                            thunk_data.push(0x44);
                            thunk_data.push(0x8B);
                            thunk_data.push(0x00);
                        },
                        3 => {
                            // mov r9d, DWORD PTR [rax]       (read i'th argument as a 32-bit value)
                            thunk_data.push(0x44);
                            thunk_data.push(0x8B);
                            thunk_data.push(0x08);
                        },
                        _ => todo!(),
                    }
                },
                Type::Pointer(_) => {
                    match i {
                        0 => {
                            // mov rcx, QWORD PTR [rax]       (read i'th argument as a 64-bit value)
                            thunk_data.push(0x48);
                            thunk_data.push(0x8B);
                            thunk_data.push(0x08);
                        },
                        1 => {
                            // mov rdx, QWORD PTR [rax]       (read i'th argument as a 64-bit value)
                            thunk_data.push(0x48);
                            thunk_data.push(0x8B);
                            thunk_data.push(0x10);
                        },
                        2 => {
                            // mov r8, QWORD PTR [rax]       (read i'th argument as a 64-bit value)
                            thunk_data.push(0x4c);
                            thunk_data.push(0x8B);
                            thunk_data.push(0x00);
                        },
                        3 => {
                            // mov r9, QWORD PTR [rax]       (read i'th argument as a 64-bit value)
                            thunk_data.push(0x4c);
                            thunk_data.push(0x8B);
                            thunk_data.push(0x08);
                        },
                        _ => {
                            // mov rax, QWORD PTR [rax]       (read i'th argument as a 64-bit value)
                            thunk_data.push(0x48);
                            thunk_data.push(0x8B);
                            thunk_data.push(0x00);

                            // mov QWORD PTR[rsp + 32 + (i-4)*8]
                            let offset = (32 + (i-4) * 8) as u8;
                            thunk_data.push(0x48);
                            thunk_data.push(0x89);
                            thunk_data.push(0x44);
                            thunk_data.push(0x24);
                            thunk_data.push(offset);
                        },
                    }
                },
                _ => todo!("parameter type {:?}", func.param_tys[i]),
            }
        }

        // movabs r10, func
        thunk_data.push(0x49);
        thunk_data.push(0xBA);
        thunk_data.extend(func_address.to_le_bytes());

        // call r10
        thunk_data.push(0x41);
        thunk_data.push(0xFF);
        thunk_data.push(0xD2);

        // mov rcx, QWORD PTR [rsp+extension+16]              (get pointer to return value)
        thunk_data.push(0x48);
        thunk_data.push(0x8B);
        thunk_data.push(0x4C);
        thunk_data.push(0x24);
        thunk_data.push(extension + 16);

        // TODO: large values require passing a pointer as the first parameter
        match func.return_ty {
            Type::Int { width: IntWidth::W32, .. } => {
                // mov DWORD PTR [rcx], eax                           (copy return value to the passed in location)
                thunk_data.push(0x89);
                thunk_data.push(0x01);
            },
            Type::Pointer(_) => {
                // mov QWORD PTR [rcx], rax                           (copy return value to the passed in location)
                thunk_data.push(0x48);
                thunk_data.push(0x89);
                thunk_data.push(0x01);
            },
            _ => todo!("return type {:?}", func.return_ty),
        }

        // add rsp, extension
        thunk_data.push(0x48);
        thunk_data.push(0x83);
        thunk_data.push(0xC4);
        thunk_data.push(extension);

        // ret
        thunk_data.push(0xC3);

        let mut thunk = region::alloc(thunk_data.len(), region::Protection::READ_WRITE_EXECUTE).unwrap();
        unsafe {
            let thunk_ptr = thunk.as_mut_ptr::<u8>();
            thunk_ptr.copy_from(thunk_data.as_ptr(), thunk_data.len());
            type TestThunk = fn(*const *mut u8, *mut u8);
            let thunk: TestThunk = std::mem::transmute(thunk_ptr);
            println!("about to call the thunk");
            let mut return_value: *mut () = std::ptr::null_mut();
            thunk(indirect_args.as_ptr(), std::mem::transmute(&mut return_value));
            println!("just called the thunk and got the value {:?}, baybee", return_value);

            kernel32::FreeLibrary(module);
        }
        Value::Nothing
    }

    #[display_adapter]
    fn panic_message(&self, msg: Option<OpId>, f: &mut Formatter) {
        let frame = self.interp.stack.last().unwrap();
        let msg = msg.map(|msg| frame.results[&msg].as_raw_ptr());
        write!(f, "Userspace panic")?;
        if let Some(mut msg) = msg {
            write!(f, ": ")?;
            unsafe {
                while *msg != 0 {
                    write!(f, "{}", *msg as char)?;
                    msg = msg.offset(1);
                }
            }
        }
        writeln!(f, "\nStack trace:")?;
        writeln!(indented(f), "{}", self.stack_trace())?;
        Ok(())
    }

    /// Execute the next instruction. Iff the instruction is a return, this function returns its `Value`
    fn execute_next(&mut self) -> Option<Value> {
        let frame = self.interp.stack.last_mut().unwrap();
        let next_op = self.code.blocks[frame.block].ops[frame.pc];
        let val = match self.code.ops[next_op].as_mir_instr().unwrap() {
            Instr::Void => Value::Nothing,
            Instr::Const(konst) => Value::from_const(konst, &*self),
            Instr::Alloca(ty) => {
                let mut storage = Vec::new();
                storage.resize(self.size_of(ty), 0);
                Value::Dynamic(storage.into_boxed_slice())
            },
            &Instr::LogicalNot(val) => {
                let val = frame.results[&val].as_bool();
                Value::from_bool(!val)
            },
            &Instr::Call { ref arguments, ref generic_arguments, func } => {
                let mut copied_args = Vec::new();
                copied_args.reserve_exact(arguments.len());
                for &arg in arguments {
                    copied_args.push(frame.results[&arg].clone());
                }
                let generic_arguments = generic_arguments.clone();
                self.call(FunctionRef::Id(func), copied_args, generic_arguments)
            },
            &Instr::ExternCall { ref arguments, func } => {
                let mut copied_args = Vec::new();
                copied_args.reserve_exact(arguments.len());
                for &arg in arguments {
                    copied_args.push(frame.results[&arg].as_bytes().to_owned().into_boxed_slice());
                }
                self.extern_call(func, copied_args)
            },
            &Instr::GenericParam(id) => {
                Value::from_ty(Type::GenericParam(id))
            },
            &Instr::Intrinsic { ref arguments, intr, .. } => {
                match intr {
                    Intrinsic::Mult => bin_op!(self, arguments, convert, Int | Float, {*}),
                    Intrinsic::Div => bin_op!(self, arguments, convert, Int | Float, {/}),
                    Intrinsic::Mod => bin_op!(self, arguments, convert, Int | Float, {%}),
                    Intrinsic::Add => bin_op!(self, arguments, convert, Int | Float, {+}),
                    Intrinsic::Sub => bin_op!(self, arguments, convert, Int | Float, {-}),
                    Intrinsic::Less => bin_op!(self, arguments, bool_convert, Int | Float, {<}),
                    Intrinsic::LessOrEq => bin_op!(self, arguments, bool_convert, Int | Float, {<=}),
                    Intrinsic::Greater => bin_op!(self, arguments, bool_convert, Int | Float, {>}),
                    Intrinsic::GreaterOrEq => bin_op!(self, arguments, bool_convert, Int | Float, {>=}),
                    Intrinsic::Eq => {
                        let ty = self.type_of(arguments[0]);
                        match ty {
                            Type::Enum(_) => {
                                assert_eq!(arguments.len(), 2);
                                let frame = self.interp.stack.last().unwrap();
                                let a = &frame.results[&arguments[0]];
                                let b = &frame.results[&arguments[1]];
                                let a = a.as_big_int(false);
                                let b = b.as_big_int(false);
                                Value::from_bool(a == b)
                            }
                            _ => bin_op!(self, arguments, bool_convert, Int | Float | Bool, {==}),
                        }
                    },
                    Intrinsic::NotEq => {
                        let ty = self.type_of(arguments[0]);
                        match ty {
                            Type::Enum(_) => {
                                assert_eq!(arguments.len(), 2);
                                let frame = self.interp.stack.last().unwrap();
                                let a = &frame.results[&arguments[0]];
                                let b = &frame.results[&arguments[1]];
                                let a = a.as_big_int(false);
                                let b = b.as_big_int(false);
                                Value::from_bool(a != b)
                            }
                            _ => bin_op!(self, arguments, bool_convert, Int | Float | Bool, {!=}),
                        }
                    },
                    Intrinsic::BitwiseAnd => bin_op!(self, arguments, convert, Int, {&}),
                    Intrinsic::BitwiseOr => bin_op!(self, arguments, convert, Int, {|}),
                    Intrinsic::LogicalNot => panic!("Unexpected logical not intrinsic, should've been replaced by instruction"),
                    Intrinsic::Neg => {
                        assert_eq!(arguments.len(), 1);
                        let frame = self.interp.stack.last().unwrap();
                        let arg = arguments[0];
                        let ty =  self.type_of(arg);
                        let arg = &frame.results[&arg];
                        match ty {
                            Type::Int { width, is_signed } => {
                                Value::from_big_int(-arg.as_big_int(is_signed), width, is_signed, self.arch)
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
                        frame.results[&arguments[0]].clone()
                    },
                    Intrinsic::Panic => {
                        assert!(arguments.len() <= 1);
                        panic!("{}", self.panic_message(arguments.first().copied()));
                    },
                    Intrinsic::Print => {
                        let frame = self.interp.stack.last().unwrap();
                        assert_eq!(arguments.len(), 1);
                        let id = arguments[0];
                        let val = &frame.results[&id];
                        let ty = self.type_of(id);
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
                        assert_eq!(self.arch.pointer_size(), 64);
                        let size = frame.results[&arguments[0]].as_u64() as usize;
                        let layout = alloc::Layout::from_size_align(size, 8).unwrap();
                        let buf = unsafe { alloc::alloc(layout) };
                        let address: usize = unsafe { mem::transmute(buf) };
                        self.interp.allocations.insert(address, layout);
                        Value::from_usize(address)
                    }
                    Intrinsic::Free => {
                        assert_eq!(arguments.len(), 1);
                        assert_eq!(self.arch.pointer_size(), 64);
                        let ptr = frame.results[&arguments[0]].as_raw_ptr();
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
                        let frame = self.interp.stack.last().unwrap();
                        assert_eq!(arguments.len(), 1);
                        let ty = frame.results[&arguments[0]].as_ty();
                        let ty = frame.canonicalize_type(&ty);
                        print!("{:?}", ty);
                        Value::Nothing
                    },
                    Intrinsic::AlignOf => {
                        let frame = self.interp.stack.last().unwrap();
                        assert_eq!(arguments.len(), 1);
                        let ty = frame.results[&arguments[0]].as_ty();
                        let ty = frame.canonicalize_type(&ty);
                        Value::from_usize(self.align_of(&ty))
                    },
                    Intrinsic::StrideOf => {
                        let frame = self.interp.stack.last().unwrap();
                        assert_eq!(arguments.len(), 1);
                        let ty = frame.results[&arguments[0]].as_ty();
                        let ty = frame.canonicalize_type(&ty);
                        Value::from_usize(self.stride_of(&ty))
                    },
                    Intrinsic::SizeOf => {
                        let frame = self.interp.stack.last().unwrap();
                        assert_eq!(arguments.len(), 1);
                        let ty = frame.results[&arguments[0]].as_ty();
                        let ty = frame.canonicalize_type(&ty);
                        Value::from_usize(self.size_of(&ty))
                    },
                    Intrinsic::OffsetOf => {
                        assert_eq!(arguments.len(), 2);
                        let ty = frame.results[&arguments[0]].as_ty();
                        let field_name = unsafe { CStr::from_ptr(frame.results[&arguments[1]].as_raw_ptr() as *const _) };
                        let field_name = self.interner.get_or_intern(field_name.to_str().unwrap());
                        let mut offset = None;
                        match ty {
                            Type::Struct(strukt) => {
                                for (index, field) in self.code.hir_code.structs[strukt].fields.iter().enumerate() {
                                    if field_name == field.name {
                                        offset = Some(self.code.mir_code.structs[&strukt].layout.field_offsets[index]);
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
            &Instr::Reinterpret(instr, _) => frame.results[&instr].clone(),
            &Instr::Truncate(instr, ref ty) => {
                let frame = self.interp.stack.last().unwrap();
                let bytes = frame.results[&instr].as_bytes();
                let new_size = self.size_of(ty);
                Value::from_bytes(&bytes[0..new_size])
            },
            &Instr::SignExtend(val, ref dest_ty) => {
                let frame = self.interp.stack.last().unwrap();
                let src_ty = &self.type_of(val);
                let val = &frame.results[&val];
                match (src_ty, dest_ty) {
                    (
                        &Type::Int { is_signed: src_is_signed, .. },
                        &Type::Int { width: dest_width, is_signed: dest_is_signed }
                    ) => Value::from_big_int(val.as_big_int(src_is_signed), dest_width, dest_is_signed, self.arch),
                    (_, _) => panic!("Invalid operand types to sign extension")
                }
            },
            &Instr::ZeroExtend(val, ref dest_ty) => {
                let frame = self.interp.stack.last().unwrap();
                let src_ty = &self.type_of(val);
                let val = &frame.results[&val];
                match (src_ty, dest_ty) {
                    (
                        &Type::Int { is_signed: src_is_signed, .. },
                        &Type::Int { width: dest_width, is_signed: dest_is_signed }
                    ) => Value::from_big_int(val.as_big_int(src_is_signed), dest_width, dest_is_signed, self.arch),
                    (_, _) => panic!("Invalid operand types to zero extension")
                }
            },
            &Instr::FloatCast(instr, ref ty) => {
                let frame = self.interp.stack.last().unwrap();
                let val = &frame.results[&instr];
                match (val.as_bytes().len(), self.size_of(ty)) {
                    (x, y) if x == y => val.clone(),
                    (4, 8) => Value::from_f64(val.as_f32() as f64),
                    (8, 4) => Value::from_f32(val.as_f64() as f32),
                    (4, _) | (8, _) => panic!("Unexpected destination float cast type size"),
                    (_, 4) | (_, 8) => panic!("Unexpected source float cast type size"),
                    (_, _) => panic!("Unexpected float cast type sizes"),
                }
            },
            &Instr::FloatToInt(instr, ref dest_ty) => {
                let frame = self.interp.stack.last().unwrap();
                let val = &frame.results[&instr];
                let src_ty = self.type_of(instr);
                let src_size = self.size_of(&src_ty);

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
                        Value::from_big_int(big_int, width, is_signed, self.arch)
                    },
                    _ => panic!("Invalid destination type in float to int cast: {:?}", dest_ty),
                }
            }
            &Instr::IntToFloat(instr, ref dest_ty) => {
                let frame = self.interp.stack.last().unwrap();
                let val = &frame.results[&instr];
                let src_ty = &self.type_of(instr);
                let dest_size = self.size_of(dest_ty);
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
                let frame = self.interp.stack.last().unwrap();
                let op = self.code.blocks[frame.block].ops[frame.pc];
                let ty = self.type_of(op);
                let ty = frame.canonicalize_type(&ty);
                let size = self.size_of(&ty);
                let frame = self.interp.stack.last_mut().unwrap();
                frame.results[&location].load(size)
            },
            &Instr::Store { location, value } => {
                let val = frame.results[&value].clone();
                let result = frame.results.entry(location).or_insert(Value::Nothing);
                result.store(val);
                Value::Nothing
            },
            &Instr::AddressOfStatic(statik) => {
                if let InterpMode::CompileTime = self.interp.mode {
                    panic!("Can't access static at compile time!");
                }
                let static_value = Value::from_const(&self.code.mir_code.statics[statik].val, &*self);
                let statik = self.interp.statics.entry(statik)
                    .or_insert(static_value);
                Value::from_usize(unsafe { mem::transmute(statik.as_bytes().as_ptr()) })
            },
            &Instr::Pointer { op, is_mut } => {
                Value::from_ty(frame.results[&op].as_ty().clone().ptr_with_mut(is_mut))
            },
            &Instr::Struct { ref fields, id } => {
                if !self.code.mir_code.structs.contains_key(&id) {
                    let mut field_tys = SmallVec::new();
                    for &field in fields {
                        field_tys.push(frame.results[&field].as_ty().clone());
                    }
                    let layout = self.layout_struct(&field_tys);
                    self.code.mir_code.structs.insert(
                        id,
                        Struct {
                            field_tys,
                            layout,
                        }
                    );
                }
                Value::from_ty(Type::Struct(id))
            },
            &Instr::Enum { ref variants, id } => {
                if !self.code.mir_code.enums.contains_key(&id) {
                    let mut variant_tys = Vec::new();
                    for &variant in variants {
                        variant_tys.push(frame.results[&variant].as_ty().clone());
                    }
                    let layout = self.layout_enum(&variant_tys);
                    self.code.mir_code.enums.insert(
                        id,
                        layout,
                    );
                }
                Value::from_ty(Type::Enum(id))
            }
            &Instr::StructLit { ref fields, id } => {
                let frame = self.interp.stack.last().unwrap();
                self.eval_struct_lit(
                    id,
                    fields.iter()
                        .map(|&instr| frame.results[&instr].clone())
                )
            },
            &Instr::Ret(instr) => {
                let val = frame.results.entry(instr).or_insert_with(|| panic!("tried to return instr {}, which has no value", instr.index()));
                let val = mem::replace(val, Value::Nothing);
                return Some(val)
            },
            &Instr::Br(bb) => {
                frame.branch_to(bb);
                return None
            },
            &Instr::CondBr { condition, true_bb, false_bb } => {
                let condition = frame.results[&condition].as_bool();
                let branch = if condition { true_bb } else { false_bb };
                frame.branch_to(branch);
                return None
            },
            &Instr::SwitchBr { scrutinee, ref cases, catch_all_bb } => {
                let scrutinee = frame.results[&scrutinee].as_u32();
                for case in cases {
                    let val = Value::from_const(&case.value, self).as_u32();
                    let frame = self.interp.stack.last_mut().unwrap();
                    if val == scrutinee {
                        frame.branch_to(case.bb);
                        return None
                    }
                }
                let frame = self.interp.stack.last_mut().unwrap();
                frame.branch_to(catch_all_bb);
                return None
            },
            &Instr::Variant { enuum, index, payload } => {
                let payload = frame.results[&payload].clone();
                Value::from_variant(self, enuum, index, payload)
            },
            &Instr::DiscriminantAccess { val } => {
                let enuum = frame.results[&val].as_enum();
                Value::from_u32(enuum.discriminant)
            },
            &Instr::DirectFieldAccess { val, index } => {
                let frame = self.interp.stack.last().unwrap();
                let bytes = frame.results[&val].as_bytes();
                let strukt = match self.type_of(val) {
                    Type::Struct(strukt) => strukt,
                    _ => panic!("Can't directly get field of non-struct"),
                };
                let strukt = &self.code.mir_code.structs[&strukt];
                let ty = strukt.field_tys[index].clone();
                let size = self.size_of(&ty);
                let offset = strukt.layout.field_offsets[index];
                Value::from_bytes(&bytes[offset..(offset + size)])
            },
            &Instr::IndirectFieldAccess { val, index } => {
                let addr = frame.results[&val].as_usize();
                let base_ty = self.type_of(val).deref().unwrap().ty;
                let strukt = match base_ty {
                    Type::Struct(strukt) => strukt,
                    _ => panic!("Can't directly get field of non-struct"),
                };
                let offset = self.code.mir_code.structs[&strukt].layout.field_offsets[index];
                Value::from_usize(addr + offset)
            },
            Instr::Parameter(_) => panic!("Invalid parameter instruction in the middle of a function!"),
        };

        let frame = self.interp.stack.last_mut().unwrap();
        let op = self.code.blocks[frame.block].ops[frame.pc];
        frame.results.insert(op, val);
        frame.pc += 1;
        None
    }
}
