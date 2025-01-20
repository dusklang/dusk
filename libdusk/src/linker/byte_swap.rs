#![allow(unused)]

use std::marker::PhantomData;
use std::mem;

// Thank you, Hagen von Eitzen: https://math.stackexchange.com/a/291494
macro_rules! nearest_multiple_of {
    (@unsafe $val:expr, $factor:expr) => {
        (((($val) as usize).wrapping_sub(1)) | ((($factor) as usize).wrapping_sub(1))).wrapping_add(1)
    };

    ($val:expr, $factor:expr) => {{
        const _: () = assert!(crate::linker::byte_swap::is_power_of_2($factor));
        nearest_multiple_of!(@unsafe $val, $factor)
    }};
}

macro_rules! nearest_multiple_of_rt {
    ($val:expr, $factor:expr) => {{
        assert!(crate::linker::byte_swap::is_power_of_2($factor));
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

impl ByteSwap for &[u8] {
    fn byte_swap(&mut self) {}
}

byte_swap_impl!(noops: u8, i8; nums: u16, u32, u64, usize, i16, i32, i64, isize);

pub struct Ref<T: ByteSwap, const BIG_ENDIAN: bool = false> {
    pub addr: usize,
    pub rva: usize,
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

    pub fn modify(&mut self, modifier: impl FnOnce(&mut T)) {
        if BIG_ENDIAN != cfg!(target_endian = "big") {
            self.value.byte_swap();
        }
        modifier(self.value);
        if BIG_ENDIAN != cfg!(target_endian = "big") {
            self.value.byte_swap();
        }
    }

    pub fn map<U: ByteSwap, M: FnOnce(&'a mut T) -> &'a mut U>(&'a mut self, mapper: M) -> ResolvedRefMut<'a, U, BIG_ENDIAN> {
        ResolvedRefMut { value: mapper(self.value) }
    }
}

impl<T: ByteSwap, const BIG_ENDIAN: bool> Clone for Ref<T, BIG_ENDIAN> {
    fn clone(&self) -> Self {
        Self {
            addr: self.addr,
            rva: self.rva,
            _phantom: PhantomData,
        }
    }
}
impl<T: ByteSwap, const BIG_ENDIAN: bool> Copy for Ref<T, BIG_ENDIAN> {}

impl<T: ByteSwap, const BIG_ENDIAN: bool> Ref<T, BIG_ENDIAN> {
    pub fn new(addr: usize, rva: usize) -> Self {
        Self {
            addr,
            rva,
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

    // currently only used by PE linker
    rva: usize,
}

impl Buffer {
    pub fn new() -> Self { Default::default() }

    pub fn pos(&self) -> usize { self.data.len() }

    pub fn rva(&self) -> usize { self.rva }

    pub fn jump_to_rva(&mut self, new_rva: usize) {
        assert!(new_rva >= self.rva);
        self.rva = new_rva;
    }

    pub fn alloc<T: ByteSwap>(&mut self) -> Ref<T> {
        let reff = Ref::new(self.data.len(), self.rva);
        self.pad_with_zeroes(mem::size_of::<T>());
        reff
    }

    pub fn alloc_be<T: ByteSwap>(&mut self) -> Ref<T, true> {
        let reff = Ref::new(self.data.len(), self.rva);
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

    pub fn extend<T: ByteSwap + Copy>(&mut self, values: &[T]) {
        let size = mem::size_of::<T>() * values.len();
        self.data.reserve(size);
        for &value in values {
            self.push(value);
        }
    }

    pub fn erase_last(&mut self, n: usize) {
        self.data.truncate(self.data.len() - n);
        self.rva -= n;
    }

    pub fn pad_with_zeroes(&mut self, size: usize) {
        self.data.extend(std::iter::repeat(0).take(size as usize));
        self.rva += size;
    }

    pub fn pad_to_next_boundary(&mut self, alignment: usize) {
        let padded_pos = nearest_multiple_of_rt!(self.data.len(), alignment);
        let amount_to_add = padded_pos as usize - self.pos();
        self.data.extend(std::iter::repeat(0).take(amount_to_add as usize));
    }

    pub fn pad_rva_to_next_boundary(&mut self, alignment: usize) {
        self.rva = nearest_multiple_of_rt!(self.rva, alignment);
    }

    pub fn pad_both_to_next_boundary(&mut self, alignment: usize) {
        self.pad_to_next_boundary(alignment);
        self.pad_rva_to_next_boundary(alignment);
    }

    pub fn push_uleb128(&mut self, mut val: u32) {
        while val > 0x7F {
            self.push(0x80u8 | (val & 0x7f) as u8);
            val >>= 7;
        }
        self.push((val & 0x7f) as u8);
    }

    pub fn push_null_terminated_string(&mut self, val: &str) -> usize {
        let pos = self.pos();
        self.data.extend(val.as_bytes());
        self.data.push(0);
        self.rva += val.len() + 1;
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
