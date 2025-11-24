pub trait CatlaDrop {
    fn drop(&self);
}

macro_rules! impl_drop {
    ($ty:ty) => {
        impl CatlaDrop for $ty {
            fn drop(&self) {}
        }
    };
}

impl_drop!(i8);
impl_drop!(i16);
impl_drop!(i32);
impl_drop!(i64);
impl_drop!(u8);
impl_drop!(u16);
impl_drop!(u32);
impl_drop!(u64);
impl_drop!(bool);
impl_drop!(char);
impl_drop!(String);
