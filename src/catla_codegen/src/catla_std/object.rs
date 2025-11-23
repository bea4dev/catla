use std::{
    cell::UnsafeCell,
    mem::{ManuallyDrop, transmute},
    sync::atomic::{AtomicUsize, Ordering, fence},
};

use crate::catla_std::drop::CatlaDrop;

#[derive(Debug)]
pub struct CatlaObject<T: CatlaDrop> {
    value: UnsafeCell<ManuallyDrop<T>>,
    mutex: UnsafeCell<bool>,
    count: UnsafeCell<usize>,
}

impl<T: CatlaDrop> CatlaObject<T> {
    pub fn new(value: T, mutex: bool) -> Self {
        Self {
            value: UnsafeCell::new(ManuallyDrop::new(value)),
            mutex: UnsafeCell::new(mutex),
            count: UnsafeCell::new(1),
        }
    }

    #[inline(always)]
    pub fn add_count(&self) {
        if unsafe { *self.mutex.get() } {
            let count = unsafe { transmute::<*mut usize, &AtomicUsize>(self.count.get()) };
            count.fetch_add(1, Ordering::Relaxed);
        } else {
            unsafe { *self.count.get() += 1 };
        }
    }

    #[inline(always)]
    pub fn sub_count(&self) {
        if unsafe { *self.mutex.get() } {
            let count = unsafe { transmute::<*mut usize, &AtomicUsize>(self.count.get()) };
            let old_count = count.fetch_sub(1, Ordering::Release);

            if old_count == 1 {
                fence(Ordering::Acquire);

                unsafe {
                    ManuallyDrop::drop(&mut *self.value.get());
                }
            }
        } else {
            unsafe { *self.count.get() -= 1 };
            let count = unsafe { *self.count.get() };

            if count == 0 {
                unsafe {
                    ManuallyDrop::drop(&mut *self.value.get());
                }
            }
        }
    }

    #[inline(always)]
    pub fn value(&self) -> &mut T {
        unsafe { &mut *self.value.get() }
    }
}

#[derive(Debug)]
pub struct CatlaObjectRef<T: 'static + CatlaDrop> {
    value: &'static CatlaObject<T>,
    heap: bool,
    drop: bool,
}

impl<T: 'static + CatlaDrop> CatlaObjectRef<T> {
    pub fn heap(value: T, mutex: bool, drop: bool) -> Self {
        let value = Box::leak(Box::new(CatlaObject::new(value, mutex)));
        Self {
            value,
            heap: true,
            drop,
        }
    }

    #[inline(always)]
    pub fn value(&self) -> &mut T {
        self.value.value()
    }
}

impl<T: 'static + CatlaDrop> Clone for CatlaObjectRef<T> {
    fn clone(&self) -> Self {
        if self.heap || self.drop {
            self.value.add_count();
        }

        Self {
            value: self.value.clone(),
            heap: self.heap.clone(),
            drop: self.drop.clone(),
        }
    }
}

impl<T: 'static + CatlaDrop> Drop for CatlaObjectRef<T> {
    fn drop(&mut self) {
        if self.heap || self.drop {
            self.value.sub_count();
        }
    }
}
