use std::mem;

pub trait IntoBytes {
    type Bytes: IntoIterator<Item=u8>;
    fn into_bytes(self) -> Self::Bytes;
}
macro_rules! into_bytes_impl {
    ($($ty:ty),*) => {
        $(
            impl IntoBytes for $ty {
                type Bytes = [u8; mem::size_of::<Self>()];
                fn into_bytes(self) -> Self::Bytes { self.to_le_bytes() }
            }
        )*
    }
}
into_bytes_impl!(u8, u16, u32, u64, i8, i16, i32, i64);