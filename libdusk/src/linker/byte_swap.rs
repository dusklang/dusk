#![allow(unused)]

use std::marker::PhantomData;
use std::mem;

// Thank you, Hagen von Eitzen: https://math.stackexchange.com/a/291494
macro_rules! nearest_multiple_of {
    (@unsafe $val:expr, $factor:expr) => {
        (((($val) as usize).wrapping_sub(1)) | ((($factor) as usize).wrapping_sub(1))).wrapping_add(1)
    };

    ($val:expr, $factor:expr) => {{
        const _: () = assert!(is_power_of_2($factor));
        nearest_multiple_of!(@unsafe $val, $factor)
    }};
}

macro_rules! nearest_multiple_of_rt {
    ($val:expr, $factor:expr) => {{
        assert!(is_power_of_2($factor));
        nearest_multiple_of!(@unsafe $val, $factor)
    }};
}

pub const fn is_power_of_2(num: usize) -> bool {
    if num == 0 { return false; }
    let mut i = 0u64;
    while i < 64 {
        if (1 << i) & num == num {
            return true;
        }
        i += 1;
    }
    false
}

pub trait ByteSwap {
    fn byte_swap(&mut self);
}

macro_rules! byte_swap_impl {
    (@noop: $ty:ty) => {
        impl ByteSwap for $ty {
            fn byte_swap(&mut self) {}
        }
    };
    (@num: $ty:ty) => {
        impl ByteSwap for $ty {
            fn byte_swap(&mut self) {
                *self = <$ty>::from_be_bytes(self.to_le_bytes());
            }
        }
    };
    (noops: $($noop_ty:ty),*;
     nums: $($num_ty:ty),* $(;)?) => {
        $(byte_swap_impl!(@noop: $noop_ty);)*
        $(byte_swap_impl!(@num: $num_ty);)*
    };
}

impl<T: ByteSwap, const N: usize> ByteSwap for [T; N] {
    fn byte_swap(&mut self) {
        for value in self {
            value.byte_swap();
        }
    }
}

byte_swap_impl!(noops: u8, i8; nums: u16, u32, u64, usize, i16, i32, i64, isize);

pub struct Ref<T: ByteSwap, const BIG_ENDIAN: bool = false> {
    pub addr: usize,
    pub _phantom: PhantomData<T>,
}

pub struct ResolvedRefMut<'a, T: ByteSwap, const BIG_ENDIAN: bool = false> {
    value: &'a mut T,
}

impl<'a, T: ByteSwap, const BIG_ENDIAN: bool> ResolvedRefMut<'a, T, BIG_ENDIAN> {
    pub unsafe fn get_value(&mut self) -> &mut T {
        self.value
    }

    pub fn set(&mut self, new_value: T) {
        *self.value = new_value;
        if BIG_ENDIAN != cfg!(target_endian = "big") {
            self.value.byte_swap();
        }
    }

    pub fn map<U: ByteSwap, M: FnOnce(&'a mut T) -> &mut U>(&'a mut self, mapper: M) -> ResolvedRefMut<'a, U, BIG_ENDIAN> {
        ResolvedRefMut { value: mapper(self.value) }
    }
}

impl<T: ByteSwap, const BIG_ENDIAN: bool> Clone for Ref<T, BIG_ENDIAN> {
    fn clone(&self) -> Self {
        Self {
            addr: self.addr,
            _phantom: PhantomData,
        }
    }
}
impl<T: ByteSwap, const BIG_ENDIAN: bool> Copy for Ref<T, BIG_ENDIAN> {}

impl<T: ByteSwap, const BIG_ENDIAN: bool> Ref<T, BIG_ENDIAN> {
    pub fn new(addr: usize) -> Self {
        Self {
            addr,
            _phantom: PhantomData,
        }
    }

    pub fn size(self) -> usize { mem::size_of::<T>() }
    pub fn start(self) -> usize { self.addr }
    pub fn end(self) -> usize { self.addr + self.size() }
}

#[derive(Default)]
pub struct Buffer {
    pub data: Vec<u8>,
}

impl Buffer {
    pub fn pos(&self) -> usize { self.data.len() }

    pub fn alloc<T: ByteSwap>(&mut self) -> Ref<T> {
        let reff = Ref::new(self.data.len());
        self.pad_with_zeroes(mem::size_of::<T>());
        reff
    }

    pub fn alloc_be<T: ByteSwap>(&mut self) -> Ref<T, true> {
        let reff = Ref::new(self.data.len());
        self.pad_with_zeroes(mem::size_of::<T>());
        reff
    }

    pub fn push<T: ByteSwap>(&mut self, value: T) -> Ref<T> {
        let addr = self.alloc();
        self.get_mut(addr).set(value);
        addr
    }

    pub fn push_be<T: ByteSwap>(&mut self, value: T) -> Ref<T, true> {
        let addr = self.alloc_be();
        self.get_mut(addr).set(value);
        addr
    }

    pub fn pad_with_zeroes(&mut self, size: usize) {
        self.data.extend(std::iter::repeat(0).take(size as usize));
    }

    pub fn pad_to_next_boundary<const B: usize>(&mut self) {
        let padded_pos = nearest_multiple_of_rt!(self.data.len() as u64, B);
        self.pad_with_zeroes(padded_pos as usize - self.pos());
    }

    pub fn pad_to_next_boundary_rt(&mut self, alignment: usize) {
        let padded_pos = nearest_multiple_of_rt!(self.data.len(), alignment);
        self.pad_with_zeroes(padded_pos as usize - self.pos());
    }
    
    pub fn push_uleb128(&mut self, mut value: u32) {
        loop {
            let mut next_byte = (value & 0x7F) as u8;
            value >>= 7;
            if value != 0 {
                next_byte |= 0x80;
            }
            self.push(next_byte);
            
            if value == 0 { break; }
        }
    }

    pub fn push_null_terminated_string(&mut self, val: &str) -> usize {
        let pos = self.pos();
        self.data.extend(val.as_bytes());
        self.data.push(0);
        pos
    }

    pub fn get_mut<'a, T: ByteSwap, const BIG_ENDIAN: bool>(&'a mut self, addr: Ref<T, BIG_ENDIAN>) -> ResolvedRefMut<'a, T, BIG_ENDIAN> {
        debug_assert!(addr.addr + mem::size_of::<T>() <= self.data.len());

        // TODO: don't produce a &mut T at all. I didn't think we would need to support unaligned access, but we do.
        let region = &mut self.data[addr.addr..];
        let ptr = region.as_mut_ptr() as *mut T;

        ResolvedRefMut { value: unsafe { &mut *ptr } }
    }
}