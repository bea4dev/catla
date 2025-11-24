use std::{
    cell::UnsafeCell,
    mem::{ManuallyDrop, transmute},
    ops::Deref,
    process::abort,
    sync::atomic::{AtomicUsize, Ordering, fence},
};

use crate::drop::CatlaDrop;

#[derive(Debug)]
pub struct CatlaObject<T: CatlaDrop> {
    value: UnsafeCell<ManuallyDrop<T>>,
    count_and_flags: UnsafeCell<usize>,
}

const BIT_SHIFT_MUTEX: usize = usize::BITS as usize - 1;
const BIT_SHIFT_HEAP: usize = usize::BITS as usize - 2;
const BIT_SHIFT_DROP: usize = usize::BITS as usize - 3;
const COUNTER_BIT_MASK: usize =
    !((1 << BIT_SHIFT_MUTEX) | (1 << BIT_SHIFT_HEAP) | (1 << BIT_SHIFT_DROP));

impl<T: CatlaDrop> CatlaObject<T> {
    pub fn new(value: T, mutex: bool, heap: bool, drop: bool) -> Self {
        let init_count = 1;
        let mutex = (mutex as usize) << BIT_SHIFT_MUTEX;
        let heap = (heap as usize) << BIT_SHIFT_HEAP;
        let drop = (drop as usize) << BIT_SHIFT_DROP;
        let count_and_flags = init_count | mutex | heap | drop;

        Self {
            value: UnsafeCell::new(ManuallyDrop::new(value)),
            count_and_flags: UnsafeCell::new(count_and_flags),
        }
    }

    #[inline(always)]
    pub const fn mutex(count_and_flags: usize) -> bool {
        ((count_and_flags >> BIT_SHIFT_MUTEX) & 1) == 1
    }

    #[inline(always)]
    pub const fn heap(count_and_flags: usize) -> bool {
        ((count_and_flags >> BIT_SHIFT_HEAP) & 1) == 1
    }

    #[inline(always)]
    pub const fn drop(count_and_flags: usize) -> bool {
        ((count_and_flags >> BIT_SHIFT_DROP) & 1) == 1
    }

    #[inline(always)]
    pub fn add_count(&self) {
        let count_and_flags = unsafe { *self.count_and_flags.get() };

        if !Self::heap(count_and_flags) && !Self::drop(count_and_flags) {
            return;
        }

        if Self::mutex(count_and_flags) {
            let count =
                unsafe { transmute::<*mut usize, &AtomicUsize>(self.count_and_flags.get()) };
            let old_count = count.fetch_add(1, Ordering::Relaxed);

            if old_count == 0 || (old_count & COUNTER_BIT_MASK) == COUNTER_BIT_MASK {
                abort();
            }
        } else {
            unsafe { *self.count_and_flags.get() += 1 };

            if (count_and_flags & COUNTER_BIT_MASK) == COUNTER_BIT_MASK {
                abort();
            }
        }
    }

    #[inline(always)]
    pub fn sub_count(&self) {
        let count_and_flags = unsafe { *self.count_and_flags.get() };

        if !Self::heap(count_and_flags) && !Self::drop(count_and_flags) {
            return;
        }

        if Self::mutex(count_and_flags) {
            let count =
                unsafe { transmute::<*mut usize, &AtomicUsize>(self.count_and_flags.get()) };
            let old_count = count.fetch_sub(1, Ordering::Release);

            if (old_count & COUNTER_BIT_MASK) == 1 {
                fence(Ordering::Acquire);

                unsafe {
                    CatlaDrop::drop((&*self.value.get()).deref());
                    ManuallyDrop::drop(&mut *self.value.get());
                }
            }
        } else {
            unsafe { *self.count_and_flags.get() -= 1 };

            if (count_and_flags & COUNTER_BIT_MASK) == 1 {
                unsafe {
                    CatlaDrop::drop((&*self.value.get()).deref());
                    ManuallyDrop::drop(&mut *self.value.get());
                }
            }
        }
    }

    #[inline(always)]
    pub fn value(&self) -> &mut T {
        unsafe { &mut *self.value.get() }
    }

    pub fn count(&self) -> usize {
        let count_and_flags = unsafe { *self.count_and_flags.get() };

        if !Self::heap(count_and_flags) && !Self::drop(count_and_flags) {
            return 1;
        }

        count_and_flags & COUNTER_BIT_MASK
    }

    pub fn is_mutex(&self) -> bool {
        let count_and_flags = unsafe { *self.count_and_flags.get() };
        Self::mutex(count_and_flags)
    }

    pub fn into_mutex(&self) {
        let count_and_flags = unsafe { *self.count_and_flags.get() };

        if !Self::heap(count_and_flags) && !Self::drop(count_and_flags) {
            abort();
        }

        if !Self::mutex(count_and_flags) {
            unsafe { *self.count_and_flags.get() |= 1 << BIT_SHIFT_MUTEX };
        }
    }
}

#[derive(Debug)]
pub struct CatlaObjectRef<T: 'static + CatlaDrop> {
    value: &'static CatlaObject<T>,
}

impl<T: 'static + CatlaDrop> CatlaObjectRef<T> {
    pub fn heap(value: T, mutex: bool, drop: bool) -> Self {
        let value = Box::leak(Box::new(CatlaObject::new(value, mutex, true, drop)));
        Self { value }
    }

    #[inline(always)]
    pub fn value(&self) -> &mut T {
        self.value.value()
    }

    pub fn count(&self) -> usize {
        self.value.count()
    }

    pub fn is_mutex(&self) -> bool {
        self.value.is_mutex()
    }

    pub fn into_mutex(&self) {
        self.value.into_mutex();
    }

    pub fn borrow(&self) -> Self {
        Self { value: self.value }
    }
}

impl<T: 'static + CatlaDrop> Clone for CatlaObjectRef<T> {
    fn clone(&self) -> Self {
        self.value.add_count();

        Self { value: self.value }
    }
}

impl<T: 'static + CatlaDrop> Drop for CatlaObjectRef<T> {
    fn drop(&mut self) {
        self.value.sub_count();
    }
}

#[cfg(test)]
mod test {
    use crate::object::CatlaObjectRef;

    #[test]
    fn catla_ref_test() {
        let object = CatlaObjectRef::heap(100, false, true);
        assert_eq!(object.count(), 1);

        {
            object.into_mutex();
            assert_eq!(object.is_mutex(), true);

            let object = object.clone();
            assert_eq!(object.count(), 2);
        }

        assert_eq!(object.count(), 1);
    }
}
