use std::{
    alloc::Layout,
    cell::UnsafeCell,
    mem::{transmute, MaybeUninit},
    ptr::NonNull,
    sync::atomic::{fence, AtomicBool, AtomicUsize, Ordering},
};

use allocator_api2::alloc::{Allocator, Global};

use crate::drop::CatlaDrop;

#[repr(C)]
pub struct CatlaRefObject<T: CatlaDrop> {
    ref_count: UnsafeCell<usize>,
    spin_lock_flag: AtomicBool,
    is_mutex: UnsafeCell<bool>,
    on_heap: bool,
    pub value: T,
}

unsafe impl<T: CatlaDrop> Send for CatlaRefObject<T> {}
unsafe impl<T: CatlaDrop> Sync for CatlaRefObject<T> {}

impl<T: CatlaDrop> CatlaRefObject<T> {
    #[inline(always)]
    pub fn init_on_stack(uninit: &mut MaybeUninit<Self>, value: T) -> &'static Self {
        unsafe {
            uninit.write(Self::new::<false>(value));
            transmute::<&Self, &'static Self>(uninit.assume_init_ref())
        }
    }

    #[inline(always)]
    pub fn init_on_heap(value: T) -> &'static Self {
        Box::leak(Box::new(Self::new::<true>(value)))
    }

    #[inline(always)]
    const fn new<const ON_HEAP: bool>(value: T) -> Self {
        Self {
            ref_count: UnsafeCell::new(1),
            spin_lock_flag: AtomicBool::new(false),
            is_mutex: UnsafeCell::new(false),
            on_heap: ON_HEAP,
            value,
        }
    }

    #[inline(always)]
    pub fn free(&self) {
        if self.on_heap {
            unsafe {
                Global.deallocate(NonNull::from(self).cast(), Layout::for_value(self));
            }
        }
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
                self.clone_ref_non_mutex();
            }
        }
    }

    #[inline(always)]
    fn drop_ref(&self) {
        unsafe {
            if *self.is_mutex.get() {
                self.drop_ref_mutex();
            } else {
                self.drop_ref_non_mutex();
            }
        }
    }

    #[inline(always)]
    fn drop_ref_mutex(&self) {
        unsafe {
            let atomic_ref_count = transmute::<*mut usize, &AtomicUsize>(self.ref_count.get());

            let old_count = atomic_ref_count.fetch_sub(1, Ordering::Release);

            if old_count == 1 {
                fence(Ordering::Acquire);

                self.value.drop_mutex();
                self.free();
            }
        }
    }

    #[inline(always)]
    fn clone_ref_non_mutex(&self) {
        unsafe {
            let ref_count = self.ref_count.get();
            *ref_count += 1;
        }
    }

    #[inline(always)]
    fn drop_ref_non_mutex(&self) {
        self.drop_ref_without_free_this();
        self.free();
    }

    #[inline(always)]
    fn drop_ref_without_free_this(&self) {
        unsafe {
            let ref_count = self.ref_count.get();

            let old_count = *ref_count;

            *ref_count = old_count - 1;

            if old_count == 1 {
                self.value.drop();
            }
        }
    }

    #[inline(always)]
    fn lock(&self) {
        loop {
            if self
                .spin_lock_flag
                .compare_exchange_weak(false, true, Ordering::Acquire, Ordering::Relaxed)
                .is_ok()
            {
                return;
            }

            while self.spin_lock_flag.load(Ordering::Relaxed) {
                std::hint::spin_loop();
            }
        }
    }

    #[inline(always)]
    fn unlock(&self) {
        self.spin_lock_flag.store(false, Ordering::Release);
    }

    #[inline(always)]
    fn to_mutex(&self) {
        unsafe {
            *self.is_mutex.get() = true;
        }
    }

    #[inline(always)]
    fn is_mutex(&self) -> bool {
        unsafe { *self.is_mutex.get() }
    }
}

pub trait CatlaRefManagement {
    fn clone_ref(&self);

    fn drop_ref(&self);

    fn drop_ref_mutex(&self);

    fn clone_ref_non_mutex(&self);

    fn drop_ref_non_mutex(&self);

    fn drop_ref_without_free_this(&self);

    fn lock(&self);

    fn unlock(&self);

    fn to_mutex(&self);

    fn is_mutex(&self) -> bool;
}

macro_rules! empty_impl {
    ($($x:ty),*) => {
        $(impl CatlaRefManagement for $x {
            #[inline(always)]
            fn clone_ref(&self) {}

            #[inline(always)]
            fn drop_ref(&self) {}

            #[inline(always)]
            fn drop_ref_mutex(&self) {}

            #[inline(always)]
            fn clone_ref_non_mutex(&self) {}

            #[inline(always)]
            fn drop_ref_non_mutex(&self) {}

            #[inline(always)]
            fn drop_ref_without_free_this(&self) {}

            #[inline(always)]
            fn lock(&self) {}

            #[inline(always)]
            fn unlock(&self) {}

            #[inline(always)]
            fn to_mutex(&self) {}

            #[inline(always)]
            fn is_mutex(&self) -> bool { false }
        })*
    };
}

empty_impl! {
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
