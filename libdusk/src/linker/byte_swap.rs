#![allow(unused)]

use std::array;
use std::marker::PhantomData;
use std::mem::{self, MaybeUninit};

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

// TODO: rename
pub trait ByteSwap: Copy {
    fn write_to(&self, buf: &mut [u8], big_endian: bool);

    fn read_from(buf: &[u8], big_endian: bool) -> Self;
}

pub fn read_bs_from_oversized_buf<T: ByteSwap>(buf: &[u8], big_endian: bool) -> T {
    T::read_from(&buf[..size_of::<T>()], big_endian)
}

macro_rules! byte_swap_impl {
    (@noop: $ty:ty) => {
        impl ByteSwap for $ty {
            fn write_to(&self, buf: &mut [u8], _big_endian: bool) {
                // Note: yes, I know to_ne_bytes() just returns a single-element array in this case
                buf.copy_from_slice(&self.to_ne_bytes())
            }

            fn read_from(buf: &[u8], _big_endian: bool) -> $ty {
                <$ty>::from_ne_bytes(buf.try_into().unwrap())
            }
        }
    };
    (@num: $ty:ty) => {
        impl ByteSwap for $ty {
            fn write_to(&self, buf: &mut [u8], big_endian: bool) {
                if big_endian {
                    buf.copy_from_slice(&self.to_be_bytes());
                } else {
                    buf.copy_from_slice(&self.to_le_bytes());
                }
            }

            fn read_from(buf: &[u8], big_endian: bool) -> $ty {
                if big_endian {
                    <$ty>::from_be_bytes(buf.try_into().unwrap())
                } else {
                    <$ty>::from_le_bytes(buf.try_into().unwrap())
                }
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
    fn write_to(&self, buf: &mut [u8], big_endian: bool) {
        assert_eq!(buf.len(), size_of::<T>() * N);
        for (dest, element) in buf.chunks_mut(size_of::<T>()).zip(self) {
            element.write_to(dest, big_endian);
        }
    }

    fn read_from(buf: &[u8], big_endian: bool) -> Self {
        assert_eq!(buf.len(), size_of::<T>() * N);
        array::from_fn(|index| {
            let index = index * size_of::<T>();
            T::read_from(&buf[index..(index+size_of::<T>())], big_endian)
        })
    }
}

byte_swap_impl!(noops: u8, i8; nums: u16, u32, u64, usize, i16, i32, i64, isize);

pub struct Ref<T: ByteSwap, const BIG_ENDIAN: bool = false> {
    pub addr: usize,
    pub rva: usize,
    pub _phantom: PhantomData<T>,
}

pub struct ResolvedRefMut<'a, T: ByteSwap, const BIG_ENDIAN: bool = false> {
    value: &'a mut [u8],
    _phantom: PhantomData<T>,
}

impl<'a, T: ByteSwap, const BIG_ENDIAN: bool> ResolvedRefMut<'a, T, BIG_ENDIAN> {
    pub fn set(&mut self, new_value: T) {
        new_value.write_to(self.value, BIG_ENDIAN);
    }

    pub fn modify(&mut self, modifier: impl FnOnce(&mut T)) {
        let mut value = T::read_from(self.value, BIG_ENDIAN);
        modifier(&mut value);
        value.write_to(self.value, BIG_ENDIAN);
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

    pub fn size(self) -> usize { size_of::<T>() }
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
        self.pad_with_zeroes(size_of::<T>());
        reff
    }

    pub fn alloc_be<T: ByteSwap>(&mut self) -> Ref<T, true> {
        let reff = Ref::new(self.data.len(), self.rva);
        self.pad_with_zeroes(size_of::<T>());
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
        let size = size_of::<T>() * values.len();
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
        let value = &mut self.data[addr.addr..(addr.addr + size_of::<T>())];
        ResolvedRefMut { value, _phantom: PhantomData }
    }
}
