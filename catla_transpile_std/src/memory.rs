use std::{
    cell::UnsafeCell,
    mem::{transmute, MaybeUninit},
    sync::atomic::{fence, AtomicBool, AtomicUsize, Ordering},
};

use crate::drop::CatlaDrop;

#[repr(C)]
pub struct CatlaRefObject<T: CatlaDrop> {
    ref_count: UnsafeCell<usize>,
    spin_lock_flag: AtomicBool,
    is_mutex: UnsafeCell<bool>,
    pub value: T,
}

unsafe impl<T: CatlaDrop> Send for CatlaRefObject<T> {}
unsafe impl<T: CatlaDrop> Sync for CatlaRefObject<T> {}

impl<T: CatlaDrop> CatlaRefObject<T> {
    #[inline(always)]
    pub fn init_on_stack(uninit: &mut MaybeUninit<Self>, value: T) -> &Self {
        unsafe {
            uninit.write(Self::new(value));
            uninit.assume_init_ref()
        }
    }

    #[inline(always)]
    pub fn init_on_heap(value: T) -> &'static Self {
        Box::leak(Box::new(Self::new(value)))
    }

    #[inline(always)]
    const fn new(value: T) -> Self {
        Self {
            ref_count: UnsafeCell::new(1),
            spin_lock_flag: AtomicBool::new(false),
            is_mutex: UnsafeCell::new(false),
            value,
        }
    } 

    #[inline(always)]
    pub fn lock(&self) {
        loop {
            if self
                .spin_lock_flag
                .compare_exchange_weak(false, true, Ordering::Acquire, Ordering::Relaxed)
                .is_ok()
            {
                return;
            }
        }
    }

    #[inline(always)]
    pub fn unlock(&self) {
        self.spin_lock_flag.store(false, Ordering::Release);
    }

    #[inline(always)]
    pub fn to_mutex(&self) {
        unsafe { *self.is_mutex.get() = true; }
    }
}

impl<T: CatlaDrop> CatlaRefManagement for &CatlaRefObject<T> {
    #[inline(always)]
    fn clone_ref(&self) {
        unsafe {
            if *self.is_mutex.get() {
                let atomic_ref_count = transmute::<*mut usize, &AtomicUsize>(self.ref_count.get());
                atomic_ref_count.fetch_add(1, Ordering::Relaxed);
            } else {
                let ref_count = self.ref_count.get();
                *ref_count += 1;
            }
        }
    }

    #[inline(always)]
    fn drop_ref(&self) {
        unsafe {
            if *self.is_mutex.get() {
                let atomic_ref_count = transmute::<*mut usize, &AtomicUsize>(self.ref_count.get());

                let old_count = atomic_ref_count.fetch_sub(1, Ordering::Release);

                if old_count == 1 {
                    fence(Ordering::Acquire);

                    self.value.drop();
                }
            } else {
                let ref_count = self.ref_count.get();

                let old_count = *ref_count;

                *ref_count = old_count - 1;

                if old_count == 1 {
                    self.value.drop();
                }
            }
        }
    }

    #[inline(always)]
    fn drop_as_unique(&self) {
        self.value.drop();
    }
}

pub trait CatlaRefManagement {
    fn clone_ref(&self);

    fn drop_ref(&self);

    fn drop_as_unique(&self);
}

macro_rules! empty_impl {
    ($($x:ty),*) => {
        $(impl CatlaRefManagement for $x {
            #[inline(always)]
            fn clone_ref(&self) {}

            #[inline(always)]
            fn drop_ref(&self) {}

            #[inline(always)]
            fn drop_as_unique(&self) {}
        })*
    };
}

empty_impl!{
    u8,
    u16,
    u32,
    u64,
    i8,
    i16,
    i32,
    i64,
    usize,
    char,
    ()
}
