use std::alloc;
use std::collections::HashMap;
use std::convert::TryInto;
use std::ffi::{CStr, CString, c_void, OsString};
use std::mem;
use std::slice;
use std::fmt::{Write, Debug};
use std::cell::RefCell;
use std::sync::{RwLock, LazyLock};
use std::borrow::Cow;
#[cfg(windows)]
use std::cmp::min;
use std::io::Write as IoWrite;

use indenter::indented;
use smallvec::SmallVec;
use paste::paste;
use num_bigint::{BigInt, Sign};
use crate::display_adapter;
use crate::index_vec::range_iter;
use index_vec::IndexVec;

use crate::target::Arch;
use crate::ast::{LegacyIntrinsic, EnumId, GenericParamId, ExternFunctionRef, ExternModId, NewNamespaceId};
use crate::dvm::{MessageKind, Call, self};
use crate::mir::{Const, Instr, InstrId, FuncId, StaticId, ExternFunction};
use crate::ty::{Type, FunctionType, QualType, IntWidth, FloatWidth, StructType, LegacyInternalType};
use crate::code::{OpId, BlockId};
use crate::internal_types::{DuskBridge, InternalField, internal_fields};

use crate::driver::{DRIVER, Driver, DriverRef};
use crate::mir::{FunctionRef, function_by_ref};
use crate::type_provider::TypeProvider;
#[cfg(target_arch="x86_64")]
use crate::backend::x64::*;
#[cfg(target_arch="aarch64")]
use crate::backend::arm64::*;

pub type Result<T> = std::result::Result<T, EvalError>;

#[derive(Debug, Clone)]
pub enum InternalValue {
    Mod(NewNamespaceId),
    FunctionPointer { generic_arguments: Vec<Type>, func: FuncId },
    StrLit(CString),
    Args(Vec<CString>),
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
            Value::Dynamic(_) => Value::from_bytes(&self.as_bytes()),
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
                        $ty_name::from_le_bytes(self.as_bytes().as_ref().try_into().unwrap())
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
    fn eval_struct_lit(&self, strukt: &StructType, fields: impl Iterator<Item=Value> + Debug) -> Value {
        let layout = self.layout_struct(strukt);
        let mut buf = SmallVec::new();
        buf.resize(layout.size, 0);
        for (i, field) in fields.enumerate() {
            let offset = layout.field_offsets[i];
            let ty = &strukt.field_tys[i];
            let size = self.size_of(ty);
            let val = field.as_bytes_with_driver(self);
            buf[offset..][..size].copy_from_slice(&val);
        }
        Value::Inline(buf)
    }
}

struct Enum {
    discriminant: u32,
}
impl Value {
    fn as_bytes_with_driver_maybe(&self, d: Option<&Driver>) -> Cow<[u8]> {
        match self {
            Value::Inline(storage) => Cow::Borrowed(storage.as_ref()),
            Value::Dynamic(ptr) => unsafe {
                let address_bits = mem::transmute::<&Box<_>, *const u8>(ptr);
                Cow::Borrowed(slice::from_raw_parts(address_bits, mem::size_of::<usize>()))
            },
            &Value::Internal { val: InternalValue::FunctionPointer { ref generic_arguments, func }, indirection: 0 } if d.is_some() => {
                assert!(generic_arguments.is_empty());
                Cow::Owned(d.unwrap().fetch_inverse_thunk(func).as_bytes().to_vec())
            },
            Value::Internal { .. } => panic!("Can't get bytes of a compiler internal data structure!"),
            Value::Nothing => Cow::Borrowed(&[]),
        }
    }
    fn as_bytes_with_driver(&self, d: &Driver) -> Cow<[u8]> {
        self.as_bytes_with_driver_maybe(Some(d))
    }
    fn as_bytes(&self) -> Cow<[u8]> {
        self.as_bytes_with_driver_maybe(None)
    }

    /// Interprets the value as a pointer and loads from it
    fn load(&self, size: usize) -> Value {
        match self {
            Value::Inline(_) => {
                let ptr = self.as_raw_ptr();
                let slice = unsafe { std::slice::from_raw_parts(ptr, size) };
                let buf = SmallVec::from_slice(slice);
                Value::Inline(buf)
            },
            Value::Dynamic(val) => {
                let buf = SmallVec::from_slice(val);
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
                let slice = unsafe { std::slice::from_raw_parts_mut(ptr, val.len()) };
                slice.copy_from_slice(&val);
            }
        }
    }

    pub fn as_big_int(&self, signed: bool) -> BigInt {
        if signed {
            BigInt::from_signed_bytes_le(self.as_bytes().as_ref())
        } else {
            BigInt::from_bytes_le(Sign::Plus, self.as_bytes().as_ref())
        }
    }

    pub fn as_str(&self) -> &str {
        unsafe { CStr::from_ptr(self.as_raw_ptr() as *const _).to_str().unwrap() }
    }

    pub fn as_raw_ptr(&self) -> *mut u8 {
        unsafe { mem::transmute(usize::from_le_bytes(self.as_bytes().as_ref().try_into().unwrap())) }
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
        bytes[0] != 0
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

    pub unsafe fn as_arbitrary_value<T>(&self) -> &T {
        // TODO: alignment!
        unsafe { &*(self.as_bytes().as_ptr() as *const T) }
    }

    fn as_ty(&self) -> Type {
        unsafe { self.as_arbitrary_value::<Type>().clone() }
    }

    pub fn as_mod(&self) -> NewNamespaceId {
        match *self.as_internal() {
            InternalValue::Mod(id) => id,
            _ => panic!("Can't get non-module as module"),
        }
    }

    fn from_bytes(bytes: &[u8]) -> Value {
        let storage = SmallVec::from_slice(bytes);
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
            Const::Int { ref lit, ref ty } => match ty {
                &Type::Int { width, is_signed } => Value::from_big_int(lit.clone(), width, is_signed, driver.arch),
                _ => panic!("unexpected int constant type {:?}", ty),
            },
            Const::Float { lit, ref ty } => match driver.size_of(ty) {
                4 => Value::from_f32(lit as f32),
                8 => Value::from_f64(lit),
                _ => panic!("Unrecognized float constant size"),
            },
            Const::Bool(val) => Value::from_bool(val),
            Const::Str { id, .. } => {
                let ptr = driver.code.mir.strings[id].as_ptr();
                Value::from_usize(ptr as usize)
            },
            Const::StrLit(ref lit) => Value::from_internal(InternalValue::StrLit(lit.clone())),
            Const::Ty(ref ty) => Value::from_new_internal(ty.clone(), driver),
            Const::Void => Value::Nothing,
            Const::Mod(id) => Value::from_mod(id),
            Const::BasicVariant { enuum, index } => Value::from_variant(driver, enuum, index, Value::Nothing),
            Const::StructLit { ref fields, id } => {
                let field_tys: Vec<_> = fields.iter().map(|val| val.ty()).collect();
                let fields = fields.iter().map(|val| Value::from_const(val, driver));
                let strukt = StructType {
                    field_tys,
                    identity: id,
                };
                driver.eval_struct_lit(&strukt, fields)
            },
            Const::Invalid => panic!("internal compiler error: should never try to convert invalid constant to interpreter value"),
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
        let layout = &d.code.mir.enums[&enuum];
        let payload_offset = layout.payload_offsets[index];
        let mut bytes = Vec::new();
        bytes.extend(Value::from_u32(index as u32).as_bytes().as_ref());
        while bytes.len() < payload_offset {
            bytes.push(0);
        }
        bytes.extend(payload.as_bytes().as_ref());
        Value::from_bytes(&bytes)
    }

    fn from_internal(val: InternalValue) -> Value {
        Value::Internal { val, indirection: 0 }
    }

    fn from_new_internal<T: DuskBridge>(val: T, d: &Driver) -> Value {
        T::bridge_to_dusk(val, d)
    }

    pub unsafe fn from_arbitrary_value<T>(val: T) -> Value {
        let addr = &raw const val as *const u8;
        let bytes = unsafe {
            slice::from_raw_parts(addr, mem::size_of::<T>())
        };
        let value = Value::from_bytes(bytes);
        std::mem::forget(val);
        value
    }

    pub fn from_mod(id: NewNamespaceId) -> Value {
        Self::from_internal(InternalValue::Mod(id))
    }
}

pub struct StackFrame {
    func_ref: FunctionRef,
    block: BlockId,
    pc: usize,
    results: IndexVec<InstrId, Value>,
    generic_ctx: HashMap<GenericParamId, Type>,
}

impl StackFrame {
    fn branch_to(&mut self, bb: BlockId) {
        self.block = bb;
        self.pc = 0;
    }

    fn canonicalize_type(&self, ty: &Type) -> Type {
        match ty {
            Type::GenericParam(id) => if let Some(result) = self.generic_ctx.get(&id) {
                result.clone()
            } else {
                ty.clone()
            },
            Type::TypeVar(_) => panic!("shouldn't be possible"),
            Type::Pointer(pointee) =>
                Type::Pointer(
                    Box::new(QualType { ty: self.canonicalize_type(&pointee.ty), is_mut: pointee.is_mut })
                ),
            &Type::Function(FunctionType { ref param_tys, has_c_variadic_param, ref return_ty }) =>
                Type::Function(
                    FunctionType {
                        param_tys: param_tys.iter().map(|ty| self.canonicalize_type(ty)).collect(),
                        has_c_variadic_param,
                        return_ty: Box::new(self.canonicalize_type(return_ty)),
                    }
                ),
            &Type::Struct(StructType { ref field_tys, identity }) => Type::Struct(
                StructType {
                    field_tys: field_tys.iter().map(|ty| self.canonicalize_type(ty)).collect(),
                    identity,
                }
            ),
            ty => ty.clone(),
        }
    }

    fn get_val(&self, op: OpId, d: &Driver) -> &Value {
        let instr_id = d.code.ops[op].get_mir_instr_id().unwrap();
        &self.results[instr_id]
    }

    fn get_val_mut(&mut self, op: OpId, d: &Driver) -> &mut Value {
        let instr_id = d.code.ops[op].get_mir_instr_id().unwrap();
        &mut self.results[instr_id]
    }
}

#[derive(Copy, Clone, Debug)]
pub enum InterpMode {
    CompileTime,
    RunTime,
}

#[cfg(windows)]
struct Allocation(region::Allocation);
#[cfg(windows)]
unsafe impl Sync for Allocation {}
#[cfg(windows)]
unsafe impl Send for Allocation {}

#[allow(unused)]
struct CachedLib {
    #[allow(unused)]
    base: *mut c_void,
    objc_classes: Vec<*const c_void>,
}
unsafe impl Sync for CachedLib {}
unsafe impl Send for CachedLib {}

pub struct Interpreter {
    statics: HashMap<StaticId, Value>,
    allocations: HashMap<usize, alloc::Layout>,
    switch_cache: HashMap<OpId, HashMap<Box<[u8]>, BlockId>>,
    #[cfg(windows)]
    inverse_thunk_cache: HashMap<FuncId, Allocation>,
    #[allow(unused)]
    lib_cache: HashMap<ExternModId, CachedLib>,
    mode: InterpMode,
    command_line_args: Vec<CString>,
}

impl Interpreter {
    pub fn new(mode: InterpMode) -> Self {
        Self {
            statics: HashMap::new(),
            allocations: HashMap::new(),
            switch_cache: HashMap::new(),
            #[cfg(windows)]
            inverse_thunk_cache: HashMap::new(),
            lib_cache: HashMap::new(),
            command_line_args: Vec::new(),
            mode,
        }
    }
}

macro_rules! bin_op {
    ($salf:ident, $stack:ident, $args:ident, $conv:ident, $first_ty:ident | $($ty:ident)|+, {$sign:tt}) => {{
        bin_op!(@preamble $salf, $stack, $args, lhs, rhs, ty, final_val);
        bin_op!(@kontinue $salf, ty, lhs, rhs, $conv, $first_ty | $($ty)|+, {$sign}, final_val);
        final_val.expect("Unexpected type for arguments")
    }};
    ($salf:ident, $stack:ident, $args:ident, $conv:ident, $ty:ident, {$sign:tt}) => {{
        bin_op!(@preamble $salf, $stack, $args, lhs, rhs, ty, final_val);
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
            assert_eq!($salf.read().arch.pointer_size(), 64);
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
            assert_eq!($salf.read().arch.pointer_size(), 64);
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
            bin_op!(@out $final_val, $conv, bool, $lhs, $rhs, {$sign});
        }
    };
    (@kontinue $salf:ident, $ty:ident, $lhs:ident, $rhs:ident, $conv:ident, Int, {$sign:tt}, $final_val:ident) => {
        bin_op!(@kontinue $salf, $ty, $lhs, $rhs, $conv, UnsignedInt | SignedInt, {$sign}, $final_val);
    };
    (@preamble $salf:ident, $stack:ident, $args:ident, $lhs:ident, $rhs:ident, $ty:ident, $final_val:ident) => {
        let frame = $stack.last().unwrap();
        assert_eq!($args.len(), 2);
        let ($lhs, $rhs) = ($args[0], $args[1]);
        let salf = $salf.read();
        let $ty = salf.type_of($lhs);
        assert_eq!($ty, $salf.read().type_of($rhs));
        let ($lhs, $rhs) = (frame.get_val($lhs, &*$salf.read()), frame.get_val($rhs, &*$salf.read()));
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

#[cfg_attr(not(windows), allow(unused))]
extern "C" fn interp_ffi_entry_point(func: u32, params: *const *const (), return_value_addr: *mut ()) {
    let func_id = FuncId::new(func as usize);

    let mut driver = DriverRef::new(&DRIVER);

    let func_ty = driver.read().code.mir.functions[func_id].ty.clone();
    let return_ty = func_ty.return_ty.as_ref().clone();
    let mut arguments = Vec::with_capacity(func_ty.param_tys.len());
    macro_rules! get_param {
        ($index:ident) => {
            unsafe {
                **(params.add($index) as *const *const _)
            }
        }
    }
    for (i, ty) in func_ty.param_tys.iter().enumerate() {
        let val = match ty {
            Type::Int { width: IntWidth::W8, .. } => Value::from_u8(get_param!(i)),
            Type::Int { width: IntWidth::W16, .. } => Value::from_u16(get_param!(i)),
            Type::Int { width: IntWidth::W32, .. } => Value::from_u32(get_param!(i)),
            Type::Int { width: IntWidth::W64 | IntWidth::Pointer, .. } | Type::Pointer(_) => {
                assert_eq!(driver.read().arch.pointer_size(), 64);
                Value::from_u64(get_param!(i))
            },
            _ => todo!("parameter type {:?}", ty),
        };
        arguments.push(val);
    }

    let Ok(return_value) = driver.call_direct(FunctionRef::Id(func_id), arguments, Vec::new()) else {
        // TODO: catch and handle this in a more reasonable way. This is tricky for a few reasons. For one, DLS and the
        // compiler want different things. DLS would want to gracefully send the error over to the client, while the
        // compiler would want to print it out. This implies adding some sort of callback or trait object which enables
        // the libdusk client to define what they want to do with diagnostics like this. However, even if we solved
        // that issue, we'd still have to deal with the fact that above us on the stack is a native function which
        // is expecting us to return a value that we don't have. This is all leading me to the mildly unfortunate
        // conclusion that I'm going to have to move the interpreter to a separate process, and devise some sort of IPC
        // scheme. If I did that, the interpreter process(es) could panic as much as it wanted, while the main process
        // is able to gracefully detect and report said panics. This would also obviate the need for a callback or
        // trait object.
        //
        // See also: https://github.com/dusklang/dusk/issues/124
        panic!("userspace code failed during call from ffi");
    };
    macro_rules! set_ret_val {
        ($val:expr) => {
            unsafe {
                *(return_value_addr as *mut _) = $val;
            }
        }
    }
    match return_ty {
        Type::Int { width: IntWidth::W8, .. } => set_ret_val!(return_value.as_u8()),
        Type::Int { width: IntWidth::W16, .. } => set_ret_val!(return_value.as_u16()),
        Type::Int { width: IntWidth::W32, .. } => set_ret_val!(return_value.as_u32()),
        Type::Int { width: IntWidth::W64 | IntWidth::Pointer, .. } | Type::Pointer(_) => {
            assert_eq!(driver.read().arch.pointer_size(), 64);
            set_ret_val!(return_value.as_u64());
        },
        _ => todo!("parameter type {:?}", return_ty),
    }
}

// Thank you, Hagen von Eitzen: https://math.stackexchange.com/a/291494
#[cfg(any(windows, all(target_os="macos", target_arch="aarch64")))]
fn nearest_multiple_of_16(val: i32) -> i32 { ((val - 1) | 15) + 1 }
#[cfg(windows)]
fn nearest_multiple_of_8(val: i32) -> i32 { ((val - 1) | 7) + 1 }

impl Driver {
    pub fn value_to_const(&mut self, val: Value, ty: Type, tp: &dyn TypeProvider) -> Const {
        match ty {
            Type::Int { is_signed, .. } => {
                let lit = val.as_big_int(is_signed);
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
                #[cfg(debug_assertions)]
                println!("NOTICE: about to blindly copy null-terminated data from an arbitrary address to the global strings!");
                let string = unsafe { CString::from(CStr::from_ptr(val.as_raw_ptr() as *const _)) };
                let id = self.code.mir.strings.push(string);
                Const::Str { id, ty }
            },
            Type::Ty => Const::Ty(val.as_ty()),
            Type::Mod => Const::Mod(val.as_mod()),
            Type::Struct(strukt) => {
                let layout = self.layout_struct(&strukt);
                let buf = val.as_bytes();
                let mut fields = Vec::new();
                for i in 0..strukt.field_tys.len() {
                    let offset = layout.field_offsets[i];
                    let ty = strukt.field_tys[i].clone();
                    let size = self.size_of(&ty);
                    let val = Value::from_bytes(&buf[offset..][..size]);
                    let konst = self.value_to_const(val, ty.clone(), tp);
                    fields.push(konst);
                }
                Const::StructLit { fields, id: strukt.identity }
            },
            Type::Enum(enuum) => {
                let enum_val = &self.code.ast.enums[enuum];
                let valid = enum_val.variants.iter().map(|variant| variant.payload_ty).all(|ty| ty.is_none());
                assert!(valid, "In order to output vaue of type {:?} as constant, it must not have any payloads", Type::Enum(enuum));
                Const::BasicVariant { enuum, index: val.as_enum().discriminant as usize }
            },
            Type::Void => Const::Void,
            Type::LegacyInternal(LegacyInternalType::StringLiteral) => match val.as_internal() {
                InternalValue::StrLit(string) => Const::StrLit(string.clone()),
                _ => panic!("unexpected non-StrLit in StringLiteral"),
            },
            _ => panic!("Can't output value of type `{:?}` as constant", ty),
        }
    }

    fn new_stack_frame(&self, func_ref: FunctionRef, arguments: Vec<Value>, generic_arguments: Vec<Type>) -> StackFrame {
        let func = function_by_ref(&self.code.mir, &func_ref);

        let mut results = IndexVec::new();

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
        results.push(Value::Nothing); // void
        for (i, arg) in arguments.into_iter().enumerate() {
            let op = self.code.blocks[start_block].ops[i];
            let param = self.code.ops[op].as_mir_instr().unwrap();
            assert!(matches!(param, Instr::Parameter(_)));
            results.push(arg);
        }
        results.resize_with(func.num_instrs, || Value::Nothing);

        let mut generic_ctx = HashMap::new();
        assert_eq!(func.generic_params.end - func.generic_params.start, generic_arguments.len());
        for (generic_param, generic_argument) in range_iter(func.generic_params.clone()).zip(generic_arguments) {
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
    pub fn stack_trace(&self, stack: &[StackFrame], f: &mut Formatter) {
        for (i, frame) in stack.iter().rev().enumerate() {
            let func = function_by_ref(&self.code.mir, &frame.func_ref);
            write!(f, "{}: {}", i, self.fn_name(func.name))?;

            if i + 1 < stack.len() {
                writeln!(f)?;
            }
        }
        Ok(())
    }
}

impl DriverRef<'_> {
    pub fn set_command_line_arguments(&mut self, args: &[OsString]) {
        INTERP.write().unwrap().command_line_args = args.iter().map(|arg| {
            CString::new(arg.to_string_lossy().as_bytes()).unwrap()
        }).collect();
    }
    pub fn call(&self, func_ref: FunctionRef, arguments: Vec<Value>, generic_arguments: Vec<Type>) -> Result<Value> {
        self.unlock();
        let val = dvm::send_message(MessageKind::Call(Call { func_ref, arguments, generic_arguments })).unwrap().0;
        val
    }
    pub fn call_direct(&mut self, func_ref: FunctionRef, arguments: Vec<Value>, generic_arguments: Vec<Type>) -> Result<Value> {
        let frame = self.read().new_stack_frame(func_ref, arguments, generic_arguments);
        INTERP_STACK.with(|stack| {
            stack.borrow_mut().push(frame);
            loop {
                // TODO: I don't love the fact that I repeatedly borrow the RefCell here...
                match self.execute_next(stack) {
                    Ok(val) => if let Some(val) = val {
                        stack.borrow_mut().pop().unwrap();
                        return Ok(val);
                    },
                    Err(err) => {
                        stack.borrow_mut().pop().unwrap();
                        return Err(err)
                    },
                }
            }
        })
    }
}

impl Driver {
    /// Generates a so-called "inverse thunk", which is a native function with the appropriate calling convention for
    /// func, which accepts all of func's parameters, allocates space on the stack for func's return value, and calls
    /// into the interpreter. It then returns the value given by the interpreter. For example, given the following Dusk
    /// function:
    ///     ```dusk
    ///     fn add(a: u32, b: u32): u32 { a + b }
    ///     ```
    /// `fetch_inverse_thunk` would generate a thunk that closely corresponds to the following C code:
    ///     ```c
    ///     #include <stdint.h>
    ///     #define ADD_FUNC_ID (insert the FuncId of add here)
    ///
    ///     void interp_ffi_entry_point(uint32_t func, void const* const* params, void* return_value_addr);
    ///
    ///     uint32_t add(uint32_t a, uint32_t b) {
    ///         void* parameters[] = {&a, &b};
    ///         uint32_t return_value;
    ///         interp_ffi_entry_point(ADD_FUNC_ID, parameters, &return_value);
    ///         return return_value;
    ///     }
    ///     ```
    #[cfg(windows)]
    #[cfg(target_arch="x86_64")]
    pub fn fetch_inverse_thunk(&self, func_id: FuncId) -> Value {
        if let Some(alloc) = INTERP.read().unwrap().inverse_thunk_cache.get(&func_id) {
            return Value::from_usize(alloc.0.as_ptr::<()>() as usize);
        }

        let func = &self.code.mir.functions[func_id];
        let param_tys = func.ty.param_tys.clone();

        let mut thunk = X64Encoder::new();
        // Store the first four parameters in shadow space in reverse order
        for (i, param_ty) in param_tys.iter().take(4).enumerate().rev() {
            let offset = (i as i32 + 1) * 8;
            match *param_ty {
                Type::Int { width: IntWidth::W16, .. } => {
                    // Store i'th argument as 16-bit value
                    let registers = [Reg16::Cx, Reg16::Dx, Reg16::R8w, Reg16::R9w];
                    thunk.store16(Reg64::Rsp + offset, registers[i]);
                },
                Type::Int { width: IntWidth::W32, .. } => {
                    // Store i'th argument as 32-bit value
                    let registers = [Reg32::Ecx, Reg32::Edx, Reg32::R8d, Reg32::R9d];
                    thunk.store32(Reg64::Rsp + offset, registers[i]);
                },
                Type::Pointer(_) | Type::Int { width: IntWidth::W64 | IntWidth::Pointer, .. } => {
                    assert_eq!(self.arch.pointer_size(), 64);
                    // Store i'th argument as 64-bit value
                    let registers = [Reg64::Rcx, Reg64::Rdx, Reg64::R8, Reg64::R9];
                    thunk.store64(Reg64::Rsp + offset, registers[i]);
                },
                _ => todo!("parameter type {:?}", param_ty),
            }
        }
        let param_address_array_space = param_tys.len() as i32 * 8;
        // round up to the nearest multiple of 8 if necessary, to make sure that the parameter address array is 8-byte
        // aligned.
        let return_value_space = nearest_multiple_of_8(self.size_of(&func.ty.return_ty) as i32);
        assert!(return_value_space <= 8, "return values bigger than 8 bytes are not yet supported in inverse thunks!");
        let call_space = 4 * 8;
        let total_stack_allocation = nearest_multiple_of_16(param_address_array_space + return_value_space + call_space) + 8;
        let return_value_offset = call_space;
        let param_address_array_offset = return_value_offset + return_value_space;
        let shadow_offset = total_stack_allocation + 8;

        // Allocate the necessary space on the stack
        thunk.sub64_imm(Reg64::Rsp, total_stack_allocation);

        // Fill the parameter array with the addresses of each parameter.
        for (i, param_ty) in param_tys.iter().enumerate() {
            let rsp_offset = i as i32 * 8;
            // Get the address
            if self.size_of(param_ty) > 8 {
                // If the value is larger than 64 bits, it was passed by address. Therefore, we should load this
                // address from shadow space directly, instead of loading the address of the address.
                thunk.load64(Reg64::Rax, Reg64::Rsp + (shadow_offset + rsp_offset))
            } else {
                thunk.lea64(Reg64::Rax, Reg64::Rsp + (shadow_offset + rsp_offset));
            }
            // Store the address in the array.
            thunk.store64(Reg64::Rsp + (param_address_array_offset + rsp_offset), Reg64::Rax);
        }

        // Pass arguments to interpreter entry point
        thunk.lea64(Reg64::R8, Reg64::Rsp + return_value_offset);
        thunk.lea64(Reg64::Rdx, Reg64::Rsp + param_address_array_offset);
        thunk.mov32_imm(Reg32::Ecx, func_id.index() as i32);

        // Call interp_ffi_entry_point
        thunk.movabs(Reg64::Rax, (interp_ffi_entry_point as usize as isize).try_into().unwrap());
        thunk.call_direct(Reg64::Rax);

        // Move return value into *ax
        match &*func.ty.return_ty {
            Type::Int { width: IntWidth::W16, .. } => thunk.load16(Reg16::Ax, Reg64::Rsp + return_value_offset),
            Type::Int { width: IntWidth::W32, .. } => thunk.load32(Reg32::Eax, Reg64::Rsp + return_value_offset),
            Type::Pointer(_) | Type::Int { width: IntWidth::W64 | IntWidth::Pointer, .. } => {
                assert_eq!(self.arch.pointer_size(), 64);
                thunk.load64(Reg64::Rax, Reg64::Rsp + return_value_offset);
            },
            _ if self.size_of(&func.ty.return_ty) == 0 => {},
            _ => todo!("return type {:?}", func.ty.return_ty),
        }

        // Return the stack to its previous state
        thunk.add64_imm(Reg64::Rsp, total_stack_allocation);

        // Return
        thunk.ret();

        let thunk = thunk.allocate();
        let val = Value::from_usize(thunk.as_ptr::<u8>() as usize);
        INTERP.write().unwrap().inverse_thunk_cache.insert(func_id, Allocation(thunk));

        val
    }
    #[cfg(any(not(windows), not(target_arch="x86_64")))]
    pub fn fetch_inverse_thunk(&self, _func_id: FuncId) -> Value {
        panic!("getting a function pointer to a Dusk function is not yet supported on your platform");
    }
}

#[cfg(windows)]
unsafe fn open_dyn_lib(path: *const i8) -> *mut c_void {
    unsafe { winapi::um::libloaderapi::LoadLibraryA(path) as *mut _ }
}
#[cfg(not(windows))]
unsafe fn open_dyn_lib(path: *const i8) -> *mut c_void {
    unsafe { libc::dlopen(path, libc::RTLD_LAZY) }
}
#[cfg(windows)]
unsafe fn get_dyn_lib_symbol(dyn_lib: *mut c_void, name: *const i8) -> *const c_void {
    unsafe { winapi::um::libloaderapi::GetProcAddress(dyn_lib as *mut _, name) as *const _ }
}
#[cfg(not(windows))]
unsafe fn get_dyn_lib_symbol(dyn_lib: *mut c_void, name: *const i8) -> *const c_void {
    unsafe { libc::dlsym(dyn_lib, name) }
}
#[cfg(windows)]
unsafe fn free_dyn_lib(dyn_lib: *mut c_void) {
    winapi::um::libloaderapi::FreeLibrary(dyn_lib as *mut _);
}
#[cfg(not(windows))]
unsafe fn free_dyn_lib(dyn_lib: *mut c_void) {
    unsafe { libc::dlclose(dyn_lib); }
}

impl DriverRef<'_> {
    #[cfg(windows)]
    #[cfg(target_arch="x86_64")]
    fn generate_thunk(&self, func: &ExternFunction, func_address: i64, arg_tys: &[Type]) -> region::Allocation {
        assert!(!func.ty.has_c_variadic_param, "C variadic parameters are not yet supported on your platform");

        let mut thunk = X64Encoder::new();
        thunk.store64(Reg64::Rsp + 16, Reg64::Rdx);
        thunk.store64(Reg64::Rsp + 8, Reg64::Rcx);

        let mut extension: i32 = 40;
        if arg_tys.len() > 4 {
            extension += ((arg_tys.len() - 3) / 2 * 16) as i32;
        }
        thunk.sub64_imm(Reg64::Rsp, extension);

        assert_eq!(arg_tys.len(), func.ty.param_tys.len());
        for i in (0..arg_tys.len()).rev() {
            // get pointer to arguments
            thunk.load64(Reg64::Rax, Reg64::Rsp + (extension + 8));

            // get pointer to i'th argument
            thunk.load64(Reg64::Rax, Reg64::Rax + (i as i32 * 8));

            match func.ty.param_tys[i] {
                Type::Int { width: IntWidth::W16, .. } => {
                    // Read i'th argument as 16-bit value
                    let registers = [Reg16::Cx, Reg16::Dx, Reg16::R8w, Reg16::R9w, Reg16::Ax];
                    thunk.load16(registers[min(i, 4)], Reg64::Rax);

                    // If this is one of the first four parameters, then we're done. Otherwise, we must store the parameter on the stack.
                    if i >= 4 {
                        let offset = (32 + (i-4) * 8) as i32;
                        thunk.store16(Reg64::Rsp + offset, Reg16::Ax);
                    }
                },
                Type::Int { width: IntWidth::W32, .. } => {
                    // Read i'th argument as 32-bit value
                    let registers = [Reg32::Ecx, Reg32::Edx, Reg32::R8d, Reg32::R9d, Reg32::Eax];
                    thunk.load32(registers[min(i, 4)], Reg64::Rax);

                    // If this is one of the first four parameters, then we're done. Otherwise, we must store the parameter on the stack.
                    if i >= 4 {
                        let offset = (32 + (i-4) * 8) as i32;
                        thunk.store32(Reg64::Rsp + offset, Reg32::Eax);
                    }
                },
                Type::Pointer(_) | Type::Int { width: IntWidth::W64 | IntWidth::Pointer, .. } => {
                    assert_eq!(self.read().arch.pointer_size(), 64);
                    // Read i'th argument as 64-bit value
                    let registers = [Reg64::Rcx, Reg64::Rdx, Reg64::R8, Reg64::R9, Reg64::Rax];
                    thunk.load64(registers[min(i, 4)], Reg64::Rax);

                    // If this is one of the first four parameters, then we're done. Otherwise, we must store the parameter on the stack.
                    if i >= 4 {
                        let offset = (32 + (i-4) * 8) as i32;
                        thunk.store64(Reg64::Rsp + offset, Reg64::Rax);
                    }
                },
                _ => todo!("parameter type {:?}", func.ty.param_tys[i]),
            }
        }

        // Call function
        thunk.movabs(Reg64::R10, func_address);
        thunk.call_direct(Reg64::R10);

        // get pointer to return value
        thunk.load64(Reg64::Rcx, Reg64::Rsp + (extension + 16));

        // TODO: large values require passing a pointer as the first parameter
        // copy return value to the passed in location
        match &*func.ty.return_ty {
            Type::Int { width: IntWidth::W16, .. } => thunk.store16(Reg64::Rcx, Reg16::Ax),
            Type::Int { width: IntWidth::W32, .. } => thunk.store32(Reg64::Rcx, Reg32::Eax),
            Type::Pointer(_) | Type::Int { width: IntWidth::W64 | IntWidth::Pointer, .. } => {
                assert_eq!(self.read().arch.pointer_size(), 64);
                thunk.store64(Reg64::Rcx, Reg64::Rax);
            },
            _ => todo!("return type {:?}", func.ty.return_ty),
        }

        thunk.add64_imm(Reg64::Rsp, extension);
        thunk.ret();

        thunk.allocate()
    }
    #[cfg(unix)]
    #[cfg(target_arch="x86_64")]
    fn generate_thunk(&self, func: &ExternFunction, func_address: i64, arg_tys: &[Type]) -> region::Allocation {
        assert!(!func.ty.has_c_variadic_param, "C variadic parameters are not yet supported on your platform");

        let mut thunk = X64Encoder::new();
        thunk.push64(Reg64::Rbp);
        thunk.mov64(Reg64::Rbp, Reg64::Rsp);

        // Copy the two arguments to the stack
        let extension = 16;
        thunk.sub64_imm(Reg64::Rsp, extension);
        thunk.store64(Reg64::Rbp - 8, Reg64::Rdi);
        thunk.store64(Reg64::Rbp - 16, Reg64::Rsi);

        assert!(arg_tys.len() <= 6, "more than 6 arguments are not yet supported on x64 UNIX platforms");
        assert_eq!(arg_tys.len(), func.ty.param_tys.len());
        for i in 0..arg_tys.len() {
            // get pointer to arguments
            thunk.load64(Reg64::Rax, Reg64::Rbp - 8);

            // get pointer to i'th argument
            thunk.load64(Reg64::Rax, Reg64::Rax + (i as i32 * 8));

            match func.ty.param_tys[i] {
                Type::Int { width: IntWidth::W16, .. } => {
                    // Read i'th argument as 16-bit value
                    let registers = [Reg16::Di, Reg16::Si, Reg16::Dx, Reg16::Cx, Reg16::R8w, Reg16::R9w];
                    thunk.load16(registers[i], Reg64::Rax);
                },
                Type::Int { width: IntWidth::W32, .. } => {
                    // Read i'th argument as 32-bit value
                    let registers = [Reg32::Edi, Reg32::Esi, Reg32::Edx, Reg32::Ecx, Reg32::R8d, Reg32::R9d];
                    thunk.load32(registers[i], Reg64::Rax);
                },
                Type::Pointer(_) | Type::Int { width: IntWidth::W64 | IntWidth::Pointer, .. } => {
                    assert_eq!(self.read().arch.pointer_size(), 64);
                    // Read i'th argument as 64-bit value
                    let registers = [Reg64::Rdi, Reg64::Rsi, Reg64::Rdx, Reg64::Rcx, Reg64::R8, Reg64::R9];
                    thunk.load64(registers[i], Reg64::Rax);
                },
                _ => todo!("parameter type {:?}", func.ty.param_tys[i]),
            }
        }

        // Call the function
        thunk.movabs(Reg64::R10, func_address);
        thunk.call_direct(Reg64::R10);

        // get pointer to return value
        if !matches!(*func.ty.return_ty, Type::Void) {
            thunk.load64(Reg64::Rcx, Reg64::Rbp - 16);
        }

        match &*func.ty.return_ty {
            Type::Int { width: IntWidth::W16, .. } => thunk.store16(Reg64::Rcx, Reg16::Ax),
            Type::Int { width: IntWidth::W32, .. } => thunk.store32(Reg64::Rcx, Reg32::Eax),
            Type::Pointer(_) | Type::Int { width: IntWidth::W64 | IntWidth::Pointer, .. } => {
                assert_eq!(self.read().arch.pointer_size(), 64);
                thunk.store64(Reg64::Rcx, Reg64::Rax);
            },
            Type::Void => {},

            _ => todo!("return type {:?}", func.ty.return_ty),
        }

        thunk.add64_imm(Reg64::Rsp, extension);
        thunk.pop64(Reg64::Rbp);
        thunk.ret();

        thunk.allocate()
    }
    #[cfg(all(target_os="macos", target_arch="aarch64"))]
    fn generate_thunk(&self, func: &ExternFunction, func_address: i64, arg_tys: &[Type]) -> region::Allocation {
        let mut thunk = Arm64Encoder::new();

        let mut needed_stack_space = 32u16; // args (8+8), fp (8), lr (8)
        if func.ty.has_c_variadic_param {
            for ty in &arg_tys[func.ty.param_tys.len()..] {
                match ty {
                    Type::Int { .. } | Type::Pointer(_) => needed_stack_space += 8,
                    _ => todo!(),
                }
            }
        }
        needed_stack_space = nearest_multiple_of_16(needed_stack_space as i32).try_into().unwrap();

        // Prologue
        thunk.sub64_imm(false, Reg::SP, Reg::SP, needed_stack_space);
        thunk.stp64(PairAddressMode::SignedOffset, Reg::FP, Reg::LR, Reg::SP, (needed_stack_space - 16).try_into().unwrap());
        thunk.add64_imm(false, Reg::FP, Reg::SP, needed_stack_space - 16);
        thunk.str64(Reg::R0, Reg::SP, needed_stack_space - 24);
        thunk.str64(Reg::R1, Reg::SP, needed_stack_space - 32);


        // Using an array here is kind of dumb, but whatever.
        let gprs = [Reg::R0, Reg::R1, Reg::R2, Reg::R3, Reg::R4, Reg::R5, Reg::R6, Reg::R7];
        let mut next_gpr = 0u32;

        let mut stack_offset = 0u16;

        // Pass arguments

        // x8 = addr of arguments array;
        thunk.ldr64(Reg::R8, Reg::SP, needed_stack_space - 24);
        for (i, ty) in arg_tys.iter().enumerate() {
            // x9 = addr of current argument;
            thunk.ldr64(Reg::R9, Reg::R8, (8 as usize * i).try_into().unwrap());
            match ty {
                Type::Int { width, .. } => {
                    let reg = if i < func.ty.param_tys.len() {
                        // TODO: pass additional non-variadic arguments on the stack
                        assert!(next_gpr < 8);
                        let reg = gprs[next_gpr as usize];
                        next_gpr += 1;
                        reg
                    } else {
                        // C variadic argument
                        Reg::R9
                    };
                    match width {
                        IntWidth::Pointer | IntWidth::W64 => {
                            assert_eq!(self.read().arch.pointer_size(), 64);
                            thunk.ldr64(reg, Reg::R9, 0);
                        },
                        IntWidth::W32 => thunk.ldr32(reg, Reg::R9, 0),
                        IntWidth::W16 => thunk.ldr16(reg, Reg::R9, 0),
                        IntWidth::W8 => thunk.ldr8(reg, Reg::R9, 0),
                    }
                    if i >= func.ty.param_tys.len() {
                        thunk.str64(reg, Reg::SP, stack_offset);
                        stack_offset += 8;
                    }
                },
                Type::Pointer(_) => {
                    assert_eq!(self.read().arch.pointer_size(), 64);
                    let reg = if i < func.ty.param_tys.len() {
                        // TODO: pass additional non-variadic arguments on the stack
                        assert!(next_gpr < 8);
                        let reg = gprs[next_gpr as usize];
                        next_gpr += 1;
                        reg
                    } else {
                        // C variadic argument
                        Reg::R9
                    };
                    thunk.ldr64(reg, Reg::R9, 0);
                    if i >= func.ty.param_tys.len() {
                        thunk.str64(reg, Reg::SP, stack_offset);
                        stack_offset += 8;
                    }
                },
                _ => todo!(),
            }
        }

        // Do the actual call
        thunk.macro_mov64_abs(Reg::R9, func_address as u64);
        thunk.blr(Reg::R9);

        // Store the return value, if any
        match *func.ty.return_ty {
            Type::Int { width, .. } => {
                // x8 = addr of return value;
                thunk.ldr64(Reg::R8, Reg::SP, needed_stack_space - 32);
                match width {
                    IntWidth::Pointer | IntWidth::W64 => {
                        assert_eq!(self.read().arch.pointer_size(), 64);
                        thunk.str64(Reg::R0, Reg::R8, 0);
                    },
                    IntWidth::W32 => thunk.str32(Reg::R0, Reg::R8, 0),
                    IntWidth::W16 => thunk.str16(Reg::R0, Reg::R8, 0),
                    IntWidth::W8 => thunk.str8(Reg::R0, Reg::R8, 0),
                }
            },
            Type::Pointer(_) => {
                // x8 = addr of return value;
                thunk.ldr64(Reg::R8, Reg::SP, needed_stack_space - 32);
                assert_eq!(self.read().arch.pointer_size(), 64);
                thunk.str64(Reg::R0, Reg::R8, 0);
            },
            Type::Void => {},
            _ => todo!(),
        }

        // Epilogue
        thunk.ldp64(PairAddressMode::SignedOffset, Reg::FP, Reg::LR, Reg::SP, (needed_stack_space - 16).try_into().unwrap());
        thunk.add64_imm(false, Reg::SP, Reg::SP, needed_stack_space);
        thunk.ret(Reg::LR);

        thunk.allocate()
    }
    #[cfg(all(any(not(any(windows, unix)), not(target_arch="x86_64")), not(all(target_os="macos", target_arch="aarch64"))))]
    fn generate_thunk(&self, _func: &ExternFunction, _func_address: i64, _arg_tys: &[Type]) -> region::Allocation {
        panic!("calling native functions not yet supported on your platform");
    }

    pub fn extern_call(&self, func_ref: ExternFunctionRef, mut args: Vec<Box<[u8]>>, arg_tys: Vec<Type>) -> Value {
        let indirect_args: Vec<*mut u8> = args.iter_mut()
            .map(|arg| arg.as_mut_ptr())
            .collect();

        let library = &self.read().code.mir.extern_mods[&func_ref.extern_mod];
        let func = &library.imported_functions[func_ref.index];
        // TODO: cache library and proc addresses (and thunks when possible)
        let dyn_lib = unsafe { open_dyn_lib(library.library_path.as_ptr()) };
        if dyn_lib.is_null() {
            panic!("unable to load library {:?}", library.library_path);
        }
        let func_name = CString::new(func.name.clone()).unwrap();
        let func_ptr = unsafe { get_dyn_lib_symbol(dyn_lib, func_name.as_ptr()) };
        if func_ptr.is_null() {
            panic!("unable to load function {:?} from library {:?}", func_name, library.library_path);
        }
        let func_address: i64 = func_ptr as i64;

        let thunk = self.generate_thunk(func, func_address, &arg_tys);
        unsafe {
            let thunk_ptr = thunk.as_ptr::<u8>();
            type Thunk = fn(*const *mut u8, *mut u8);
            let thunk: Thunk = mem::transmute(thunk_ptr);
            let mut return_val_storage = SmallVec::new();
            return_val_storage.resize(self.read().size_of(&func.ty.return_ty), 0);
            thunk(indirect_args.as_ptr(), return_val_storage.as_mut_ptr());

            free_dyn_lib(dyn_lib);

            Value::Inline(return_val_storage)
        }
    }
}

impl Driver {
    #[display_adapter]
    fn panic_message(&self, stack: &[StackFrame], msg: Option<OpId>, f: &mut Formatter) {
        let frame = stack.last().unwrap();
        let msg = msg.map(|msg| frame.get_val(msg, self).as_raw_ptr());
        write!(f, "compile-time code panicked")?;
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
        write!(indented(f), "{}", self.stack_trace(stack))?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct EvalError;

impl DriverRef<'_> {
    /// Execute the next instruction. Iff the instruction is a return, this function returns its `Value`. Otherwise, it returns `None`.
    // NOTE FOR CORRECTNESS: If you return an Err() result, you MUST first report an error! Otherwise
    // compilation could end up "succeeding", even though compile-time code execution failed.
    fn execute_next(&mut self, stack_cell: &RefCell<Vec<StackFrame>>) -> Result<Option<Value>> {
        let val = {
            let mut stack = stack_cell.borrow_mut();
            let frame = stack.last_mut().unwrap();
            let next_op = self.read().code.blocks[frame.block].ops[frame.pc];
            let d = self.read();
            match d.code.ops[next_op].as_mir_instr().unwrap() {
                Instr::Void => Value::Nothing,
                Instr::Const(konst) => Value::from_const(&konst.clone(), &*self.read()),
                Instr::Alloca(ty) => {
                    let mut storage = Vec::new();
                    storage.resize(self.read().size_of(ty), 0);
                    Value::Dynamic(storage.into_boxed_slice())
                },
                &Instr::LogicalNot(val) => {
                    let val = frame.get_val(val, &*self.read()).as_bool();
                    Value::from_bool(!val)
                },
                &Instr::FunctionRef { ref generic_arguments, func } => {
                    Value::from_internal(InternalValue::FunctionPointer { generic_arguments: generic_arguments.clone(), func })
                },
                &Instr::Call { ref arguments, ref generic_arguments, func } => {
                    let mut copied_args = Vec::new();
                    copied_args.reserve_exact(arguments.len());
                    for &arg in arguments {
                        copied_args.push(frame.get_val(arg, &*self.read()).clone());
                    }
                    let generic_arguments = generic_arguments.iter()
                        .map(|arg| frame.canonicalize_type(arg))
                        .collect();
                    // Stop immutably borrowing the stack, so it can be borrowed again in call()
                    drop(stack);
                    drop(d);
                    self.call_direct(FunctionRef::Id(func), copied_args, generic_arguments)?
                },
                &Instr::ExternCall { ref arguments, func } => {
                    let mut copied_args = Vec::new();
                    copied_args.reserve_exact(arguments.len());
                    let mut arg_tys = Vec::new();
                    arg_tys.reserve_exact(arguments.len());
                    for &arg in arguments {
                        copied_args.push(frame.get_val(arg, &*self.read()).as_bytes().as_ref().to_owned().into_boxed_slice());
                        arg_tys.push(d.type_of(arg).clone());
                    }
                    drop(stack);
                    drop(d);
                    self.read_only();
                    self.extern_call(func, copied_args, arg_tys)
                },
                #[cfg(target_os = "macos")]
                &Instr::ObjcClassRef { extern_mod, index } => {
                    let library = &self.read().code.mir.extern_mods[&extern_mod];
                    let mut interp = INTERP.write().unwrap();
                    let cache = interp.lib_cache.entry(extern_mod).or_insert_with(|| {
                        // TODO: cache library and proc addresses (and thunks when possible)
                        let base = unsafe { open_dyn_lib(library.library_path.as_ptr()) };
                        if base.is_null() {
                            panic!("unable to load library {:?}", library.library_path);
                        }
                        let mut objc_classes = Vec::new();
                        for class_name in &self.read().code.ast.extern_mods[extern_mod].objc_class_references {
                            let class_name = CString::new(class_name.clone()).unwrap();
                            let class_ptr = unsafe { objc::runtime::objc_getClass(class_name.as_ptr()) };
                            objc_classes.push(class_ptr as *const c_void);
                        }
                        CachedLib {
                            base,
                            objc_classes
                        }
                    });
                    Value::from_usize(cache.objc_classes[index] as usize)
                },
                #[cfg(not(target_os = "macos"))]
                &Instr::ObjcClassRef { .. } => unimplemented!("cannot refer to Objective-C class on a non-macOS platform"),
                &Instr::GenericParam(id) => {
                    let ty = Type::GenericParam(id);
                    let ty = frame.canonicalize_type(&ty);
                    Value::from_new_internal(ty, &d)
                },
                &Instr::LegacyIntrinsic { ref arguments, intr, .. } => {
                    match intr {
                        LegacyIntrinsic::Mult => bin_op!(self, stack, arguments, convert, Int | Float, {*}),
                        LegacyIntrinsic::Div => bin_op!(self, stack, arguments, convert, Int | Float, {/}),
                        LegacyIntrinsic::Mod => bin_op!(self, stack, arguments, convert, Int | Float, {%}),
                        LegacyIntrinsic::Add => bin_op!(self, stack, arguments, convert, Int | Float, {+}),
                        LegacyIntrinsic::Sub => bin_op!(self, stack, arguments, convert, Int | Float, {-}),
                        LegacyIntrinsic::Less => bin_op!(self, stack, arguments, bool_convert, Int | Float, {<}),
                        LegacyIntrinsic::LessOrEq => bin_op!(self, stack, arguments, bool_convert, Int | Float, {<=}),
                        LegacyIntrinsic::Greater => bin_op!(self, stack, arguments, bool_convert, Int | Float, {>}),
                        LegacyIntrinsic::GreaterOrEq => bin_op!(self, stack, arguments, bool_convert, Int | Float, {>=}),
                        LegacyIntrinsic::Eq => {
                            let ty = d.type_of(arguments[0]);
                            match ty {
                                Type::Enum(_) => {
                                    assert_eq!(arguments.len(), 2);
                                    let frame = stack.last().unwrap();
                                    let a = frame.get_val(arguments[0], &*self.read());
                                    let b = frame.get_val(arguments[1], &*self.read());
                                    let a = a.as_big_int(false);
                                    let b = b.as_big_int(false);
                                    Value::from_bool(a == b)
                                }
                                _ => bin_op!(self, stack, arguments, bool_convert, Int | Float | Bool, {==}),
                            }
                        },
                        LegacyIntrinsic::NotEq => {
                            let ty = d.type_of(arguments[0]);
                            match ty {
                                Type::Enum(_) => {
                                    assert_eq!(arguments.len(), 2);
                                    let frame = stack.last().unwrap();
                                    let a = frame.get_val(arguments[0], &*self.read());
                                    let b = frame.get_val(arguments[1], &*self.read());
                                    let a = a.as_big_int(false);
                                    let b = b.as_big_int(false);
                                    Value::from_bool(a != b)
                                }
                                _ => bin_op!(self, stack, arguments, bool_convert, Int | Float | Bool, {!=}),
                            }
                        },
                        LegacyIntrinsic::BitwiseAnd => bin_op!(self, stack, arguments, convert, Int | Bool, {&}),
                        LegacyIntrinsic::BitwiseOr => bin_op!(self, stack, arguments, convert, Int | Bool, {|}),
                        LegacyIntrinsic::BitwiseXor => bin_op!(self, stack, arguments, convert, Int | Bool, {^}),
                        LegacyIntrinsic::LeftShift => bin_op!(self, stack, arguments, convert, Int, {<<}),
                        LegacyIntrinsic::RightShift => bin_op!(self, stack, arguments, convert, Int, {>>}),
                        LegacyIntrinsic::LogicalNot => panic!("Unexpected logical not intrinsic, should've been replaced by instruction"),
                        LegacyIntrinsic::Neg => {
                            assert_eq!(arguments.len(), 1);
                            let frame = stack.last().unwrap();
                            let arg = arguments[0];
                            let ty = d.type_of(arg);
                            let arg = frame.get_val(arg, &*self.read());
                            match *ty {
                                Type::Int { width, is_signed } => {
                                    Value::from_big_int(-arg.as_big_int(is_signed), width, is_signed, self.read().arch)
                                },
                                Type::Float(width) => match width {
                                    FloatWidth::W32 => Value::from_f32(-arg.as_f32()),
                                    FloatWidth::W64 => Value::from_f64(-arg.as_f64()),
                                },
                                _ => panic!("Unexpected type for intrinsic arguments"),
                            }
                        },
                        LegacyIntrinsic::BitwiseNot => {
                            assert_eq!(arguments.len(), 1);
                            let arg = arguments[0];
                            let ty = d.type_of(arg);
                            let arg = frame.get_val(arg, &*self.read());
                            match ty {
                                &Type::Int { width, .. } => {
                                    match width {
                                        IntWidth::Pointer | IntWidth::W64 => {
                                            assert_eq!(self.read().arch.pointer_size(), 64);
                                            Value::from_u64(!arg.as_u64())
                                        },
                                        IntWidth::W32 => Value::from_u32(!arg.as_u32()),
                                        IntWidth::W16 => Value::from_u16(!arg.as_u16()),
                                        IntWidth::W8  => Value::from_u8(!arg.as_u8()),
                                    }
                                },
                                _ => panic!("Unexpected type for intrinsic arguments")
                            }
                        },
                        LegacyIntrinsic::Pos => {
                            assert_eq!(arguments.len(), 1);
                            frame.get_val(arguments[0], &*self.read()).clone()
                        },
                        LegacyIntrinsic::Panic => {
                            assert!(arguments.len() <= 1);
                            let panic_message = self.read().panic_message(&stack, arguments.first().copied()).to_string();
                            drop(d);
                            self.write().diag.report_error(panic_message, next_op, "panic occured here");
                            return Err(EvalError);
                        },
                        LegacyIntrinsic::Print => {
                            let frame = stack.last().unwrap();
                            assert_eq!(arguments.len(), 1);
                            let id = arguments[0];
                            let val = frame.get_val(id, &*self.read());
                            let ty = d.type_of(id);
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
                            std::io::stdout().flush().unwrap();
                            Value::Nothing
                        },
                        LegacyIntrinsic::Malloc => {
                            assert_eq!(arguments.len(), 1);
                            assert_eq!(self.read().arch.pointer_size(), 64);
                            let size = frame.get_val(arguments[0], &*self.read()).as_u64() as usize;
                            let layout = alloc::Layout::from_size_align(size, 8).unwrap();
                            let buf = unsafe { alloc::alloc(layout) };
                            let address = buf as usize;
                            INTERP.write().unwrap().allocations.insert(address, layout);
                            Value::from_usize(address)
                        }
                        LegacyIntrinsic::Free => {
                            assert_eq!(arguments.len(), 1);
                            assert_eq!(self.read().arch.pointer_size(), 64);
                            let ptr = frame.get_val(arguments[0], &*self.read()).as_raw_ptr();
                            let address = ptr as usize;
                            let layout = INTERP.write().unwrap().allocations.remove(&address).unwrap();
                            unsafe { alloc::dealloc(ptr, layout) };
                            Value::Nothing
                        },
                        LegacyIntrinsic::PrintType => {
                            let frame = stack.last().unwrap();
                            assert_eq!(arguments.len(), 1);
                            let ty = frame.get_val(arguments[0], &*self.read()).as_ty();
                            let ty = frame.canonicalize_type(&ty);
                            print!("{:?}", ty);
                            Value::Nothing
                        },
                        LegacyIntrinsic::AlignOf => {
                            let frame = stack.last().unwrap();
                            assert_eq!(arguments.len(), 1);
                            let ty = frame.get_val(arguments[0], &*self.read()).as_ty();
                            let ty = frame.canonicalize_type(&ty);
                            Value::from_usize(self.read().align_of(&ty))
                        },
                        LegacyIntrinsic::StrideOf => {
                            let frame = stack.last().unwrap();
                            assert_eq!(arguments.len(), 1);
                            let ty = frame.get_val(arguments[0], &*self.read()).as_ty();
                            let ty = frame.canonicalize_type(&ty);
                            Value::from_usize(self.read().stride_of(&ty))
                        },
                        LegacyIntrinsic::SizeOf => {
                            let frame = stack.last().unwrap();
                            assert_eq!(arguments.len(), 1);
                            let ty = frame.get_val(arguments[0], &*self.read()).as_ty();
                            let ty = frame.canonicalize_type(&ty);

                            Value::from_usize(self.read().size_of(&ty))
                        },
                        LegacyIntrinsic::OffsetOf => {
                            assert_eq!(arguments.len(), 2);
                            let ty = frame.get_val(arguments[0], &*self.read()).as_ty();
                            let field_name = unsafe { CStr::from_ptr(frame.get_val(arguments[1], &*self.read()).as_raw_ptr() as *const _) };
                            let field_name = self.read().interner.get(field_name.to_str().unwrap());
                            let mut offset = None;
                            if let Some(field_name) = field_name {
                                match ty {
                                    Type::Struct(strukt) => {
                                        let layout = self.read().layout_struct(&strukt);
                                        for (index, field) in self.read().code.ast.structs[strukt.identity].fields.iter().enumerate() {
                                            if field_name == field.name {
                                                offset = Some(layout.field_offsets[index]);
                                                break;
                                            }
                                        }
                                    }
                                    _ => panic!("Can't get field offset on a non-struct type"),
                                }
                            }
                            let offset = offset.expect("No such field name in call to offset_of");
                            Value::from_usize(offset)
                        },
                        LegacyIntrinsic::GetNumArgs => {
                            Value::from_usize(INTERP.read().unwrap().command_line_args.len())
                        },
                        LegacyIntrinsic::GetArg => {
                            assert_eq!(arguments.len(), 1);
                            let index = frame.get_val(arguments[0], &*self.read()).as_usize();
                            let command_line_args = &INTERP.read().unwrap().command_line_args;
                            Value::from_internal(InternalValue::StrLit(command_line_args[index].clone()))
                        },
                        LegacyIntrinsic::Import => {
                            assert_eq!(arguments.len(), 1);
                            let val = frame.get_val(arguments[0], &*self.read());
                            let ptr = val.as_raw_ptr();

                            let str = unsafe { CStr::from_ptr(ptr as _) };
                            let path = str.to_str().unwrap();
                            let (file, _) = d.lookup_file(next_op);
                            let base_path = d.src_map.files[file].location.as_path();
                            let path = base_path
                                .map(|base| base.parent().unwrap().join(path))
                                .unwrap_or_else(|| path.into());
                            drop(d);
                            let file = self.write().src_map.add_file_on_disk(path).unwrap();
                            self.write().parse_added_files().unwrap();

                            let added_module = self.read().code.ast.global_scopes[&file];
                            Value::from_mod(added_module)
                        },
                        _ => panic!("Call to unimplemented intrinsic {:?}", intr),
                    }
                },
                &Instr::Intrinsic { ref arguments, intr } => {
                    let arguments: Vec<&Value> = arguments.iter().map(|&arg| frame.get_val(arg, &d)).collect();
                    let implementation = d.code.ast.intrinsics[intr].implementation;
                    drop(d);
                    implementation(self, arguments)
                },
                &Instr::Reinterpret(instr, _) => frame.get_val(instr, &*self.read()).clone(),
                &Instr::Truncate(instr, ref ty) => {
                    let frame = stack.last().unwrap();
                    let bytes = frame.get_val(instr, &*self.read()).as_bytes();
                    let new_size = self.read().size_of(ty);
                    Value::from_bytes(&bytes[0..new_size])
                },
                &Instr::SignExtend(val, ref dest_ty) => {
                    let frame = stack.last().unwrap();
                    let src_ty = d.type_of(val);
                    let val = frame.get_val(val, &*self.read());
                    match (src_ty, dest_ty) {
                        (
                            &Type::Int { is_signed: src_is_signed, .. },
                            &Type::Int { width: dest_width, is_signed: dest_is_signed }
                        ) => Value::from_big_int(val.as_big_int(src_is_signed), dest_width, dest_is_signed, self.read().arch),
                        (_, _) => panic!("Invalid operand types to sign extension")
                    }
                },
                &Instr::ZeroExtend(val, ref dest_ty) => {
                    let frame = stack.last().unwrap();
                    let src_ty = d.type_of(val);
                    let val = frame.get_val(val, &*self.read());
                    match (src_ty, dest_ty) {
                        (
                            &Type::Int { is_signed: src_is_signed, .. },
                            &Type::Int { width: dest_width, is_signed: dest_is_signed }
                        ) => Value::from_big_int(val.as_big_int(src_is_signed), dest_width, dest_is_signed, self.read().arch),
                        (_, _) => panic!("Invalid operand types to zero extension")
                    }
                },
                &Instr::FloatCast(instr, ref ty) => {
                    let frame = stack.last().unwrap();
                    let val = frame.get_val(instr, &*self.read());
                    match (val.as_bytes().len(), self.read().size_of(ty)) {
                        (x, y) if x == y => val.clone(),
                        (4, 8) => Value::from_f64(val.as_f32() as f64),
                        (8, 4) => Value::from_f32(val.as_f64() as f32),
                        (4, _) | (8, _) => panic!("Unexpected destination float cast type size"),
                        (_, 4) | (_, 8) => panic!("Unexpected source float cast type size"),
                        (_, _) => panic!("Unexpected float cast type sizes"),
                    }
                },
                &Instr::FloatToInt(instr, ref dest_ty) => {
                    let frame = stack.last().unwrap();
                    let val = frame.get_val(instr, &*self.read());
                    let src_ty = d.type_of(instr);
                    let src_size = self.read().size_of(src_ty);

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
                            Value::from_big_int(big_int, width, is_signed, self.read().arch)
                        },
                        _ => panic!("Invalid destination type in float to int cast: {:?}", dest_ty),
                    }
                }
                &Instr::IntToFloat(instr, ref dest_ty) => {
                    let frame = stack.last().unwrap();
                    let val = frame.get_val(instr, &*self.read());
                    let src_ty = d.type_of(instr);
                    let dest_size = self.read().size_of(dest_ty);
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
                    let frame = stack.last().unwrap();
                    let op = self.read().code.blocks[frame.block].ops[frame.pc];
                    let ty = d.type_of(op);
                    let ty = frame.canonicalize_type(ty);
                    let size = self.read().size_of(&ty);
                    let frame = stack.last_mut().unwrap();
                    frame.get_val(location, &*self.read()).load(size)
                },
                &Instr::Store { location, value } => {
                    let val = frame.get_val(value, &*self.read()).clone();
                    let result = frame.get_val_mut(location, &*self.read());
                    result.store(val);
                    Value::Nothing
                },
                &Instr::AddressOfStatic(statik) => {
                    if let InterpMode::CompileTime = INTERP.read().unwrap().mode {
                        panic!("Can't access static at compile time!");
                    }
                    let static_value = Value::from_const(&self.read().code.mir.statics[statik].val.clone(), &*self.read());
                    let statik = INTERP.write().unwrap().statics.entry(statik)
                        .or_insert(static_value)
                        .as_bytes()
                        .as_ptr();
                    Value::from_usize(statik as usize)
                },
                &Instr::Pointer { op, is_mut } => {
                    let ty = frame.get_val(op, &*self.read()).as_ty().ptr_with_mut(is_mut);
                    Value::from_new_internal(ty, &d)
                },
                &Instr::FunctionTy { ref param_tys, has_c_variadic_param, ret_ty } => {
                    let param_tys = param_tys.iter()
                        .map(|&ty| frame.get_val(ty, &*self.read()).as_ty())
                        .collect();
                    let ret_ty = frame.get_val(ret_ty, &*self.read()).as_ty();
                    let ty = Type::Function(FunctionType { param_tys, has_c_variadic_param, return_ty: Box::new(ret_ty) });
                    Value::from_new_internal(ty, &d)
                }
                &Instr::Struct { ref fields, id } => {
                    let mut field_tys = Vec::new();
                    for &field in fields {
                        field_tys.push(frame.get_val(field, &*self.read()).as_ty());
                    }
                    drop(d);
                    let strukt = StructType {
                        field_tys,
                        identity: id,
                    };
                    Value::from_new_internal(Type::Struct(strukt), &self.read())
                },
                &Instr::Enum { ref variants, id } => {
                    if !self.read().code.mir.enums.contains_key(&id) {
                        let mut variant_tys = Vec::new();
                        for &variant in variants {
                            variant_tys.push(frame.get_val(variant, &*self.read()).as_ty());
                        }
                        let layout = self.read().layout_enum(&variant_tys);
                        drop(d);
                        self.write().code.mir.enums.insert(
                            id,
                            layout,
                        );
                    }
                    Value::from_new_internal(Type::Enum(id), &self.read())
                }
                &Instr::StructLit { ref fields, id } => {
                    let frame = stack.last().unwrap();
                    let field_tys: Vec<_> = fields.iter()
                        .map(|&instr| {
                            let ty = d.type_of(instr);
                            frame.canonicalize_type(ty)
                        })
                        .collect();
                    let fields: Vec<_> = fields.iter()
                        .map(|&instr| frame.get_val(instr, &*self.read()).clone())
                        .collect();
                    drop(d);
                    let strukt = StructType {
                        field_tys,
                        identity: id,
                    };
                    self.write().eval_struct_lit(&strukt, fields.into_iter())
                },
                &Instr::Ret(instr) => {
                    let val = frame.get_val(instr, &*self.read()).clone();
                    return Ok(Some(val));
                },
                &Instr::Br(bb) => {
                    frame.branch_to(bb);
                    return Ok(None);
                },
                &Instr::CondBr { condition, true_bb, false_bb } => {
                    let condition = frame.get_val(condition, &*self.read()).as_bool();
                    let branch = if condition { true_bb } else { false_bb };
                    frame.branch_to(branch);
                    return Ok(None);
                },
                &Instr::SwitchBr { scrutinee, ref cases, catch_all_bb } => {
                    // TODO: this is a very crude (and possibly slow) way of supporting arbitrary integer scrutinees
                    let scrutinee = frame.get_val(scrutinee, &*self.read()).as_bytes().to_owned();
                    let interp = INTERP.read().unwrap();
                    let block = if let Some(table) = interp.switch_cache.get(&next_op) {
                        let block = table.get(scrutinee.as_ref()).copied();
                        drop(interp);
                        block
                    } else {
                        drop(interp);
                        let mut table = HashMap::new();
                        for case in cases.clone() {
                            let val = Value::from_const(&case.value, &*self.read());
                            let val = val.as_bytes();
                            table.insert(val.as_ref().to_owned().into_boxed_slice(), case.bb);
                        }
                        INTERP.write().unwrap().switch_cache.entry(next_op).or_insert(table)
                            .get(scrutinee.as_ref()).copied()
                    }.unwrap_or(catch_all_bb);

                    let frame = stack.last_mut().unwrap();
                    frame.branch_to(block);
                    return Ok(None);
                },
                &Instr::Variant { enuum, index, payload } => {
                    let payload = frame.get_val(payload, &*self.read()).clone();
                    Value::from_variant(&*self.read(), enuum, index, payload)
                },
                &Instr::PayloadAccess { val, variant_index } => {
                    todo!()
                },
                &Instr::DiscriminantAccess { val } => {
                    let enuum = frame.get_val(val, &*self.read()).as_enum();
                    Value::from_u32(enuum.discriminant)
                },
                &Instr::DirectFieldAccess { val, index } => {
                    let frame = stack.last().unwrap();
                    let bytes = frame.get_val(val, &*self.read()).as_bytes();
                    let strukt = match d.type_of(val) {
                        Type::Struct(strukt) => strukt,
                        _ => panic!("Can't directly get field of non-struct"),
                    };
                    let layout = self.read().layout_struct(strukt);
                    let size = self.read().size_of(&strukt.field_tys[index]);
                    let offset = layout.field_offsets[index];
                    Value::from_bytes(&bytes[offset..][..size])
                },
                &Instr::IndirectFieldAccess { val, index } => {
                    let addr = frame.get_val(val, &*self.read()).as_usize();
                    let base_ty = &d.type_of(val).deref().unwrap().ty;
                    let strukt = match base_ty {
                        Type::Struct(strukt) => strukt,
                        _ => panic!("Can't directly get field of non-struct"),
                    };
                    let offset = self.read().layout_struct(strukt).field_offsets[index];
                    Value::from_usize(addr + offset)
                },
                &Instr::InternalFieldAccess { val, field } => {
                    let val = frame.get_val(val, &*self.read()).as_internal();
                    match (val, field) {
                        (InternalValue::StrLit(lit), InternalField::StringLiteral(field)) => {
                            use internal_fields::StringLiteral::*;
                            match field {
                                length => {
                                    Value::from_usize(lit.as_bytes().len())
                                },
                                data => Value::from_usize(lit.as_ptr() as usize),
                            }
                        },
                        pair => unimplemented!("unimplemented internal field access: {:?}", pair),
                    }
                },
                Instr::Parameter(_) => panic!("Invalid parameter instruction in the middle of a function!"),
                Instr::Invalid => panic!("Must not have invalid instruction in an interpreted function!"),
            }
        };

        let mut stack = stack_cell.borrow_mut();
        let frame = stack.last_mut().unwrap();
        let op = self.read().code.blocks[frame.block].ops[frame.pc];
        *frame.get_val_mut(op, &*self.read()) = val;
        frame.pc += 1;
        Ok(None)
    }
}

static INTERP: LazyLock<RwLock<Interpreter>> = LazyLock::new(|| RwLock::new(Interpreter::new(InterpMode::CompileTime)));

thread_local! {
    static INTERP_STACK: RefCell<Vec<StackFrame>> = RefCell::new(Vec::new());
}
pub fn restart_interp(mode: InterpMode) {
    *INTERP.write().unwrap() = Interpreter::new(mode);
}
