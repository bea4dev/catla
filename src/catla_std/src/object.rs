use std::{
    mem::{ManuallyDrop, MaybeUninit, transmute},
    ops::Deref,
    process::abort,
    sync::atomic::{AtomicUsize, Ordering, fence},
};

use crate::dispose::Drop;

const BIT_SHIFT_MUTEX: usize = usize::BITS as usize - 1;
const BIT_SHIFT_HEAP: usize = usize::BITS as usize - 2;
const BIT_SHIFT_DROP: usize = usize::BITS as usize - 3;
const COUNTER_BIT_MASK: usize =
    !((1 << BIT_SHIFT_MUTEX) | (1 << BIT_SHIFT_HEAP) | (1 << BIT_SHIFT_DROP));

#[inline(always)]
pub const fn init_count_and_flags(mutex: bool, heap: bool, drop: bool) -> usize {
    let init_count = 1;
    let mutex = (mutex as usize) << BIT_SHIFT_MUTEX;
    let heap = (heap as usize) << BIT_SHIFT_HEAP;
    let drop = (drop as usize) << BIT_SHIFT_DROP;
    init_count | mutex | heap | drop
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
pub const fn drop_flag(count_and_flags: usize) -> bool {
    ((count_and_flags >> BIT_SHIFT_DROP) & 1) == 1
}

pub trait CatlaObject: Drop + 'static {
    type Value: 'static;

    fn new(value: Self::Value, mutex: bool, heap: bool, drop: bool) -> Self;
    fn count_and_flags_ptr(&self) -> *mut usize;
    fn value_ptr(&self) -> *mut Self::Value;
}

#[derive(Debug)]
pub struct CatlaObjectRef<T: CatlaObject> {
    value: &'static T,
}

#[derive(Debug)]
pub enum ReturnPlace<'a, T: CatlaObject> {
    Heap,
    Stack(&'a mut ManuallyDrop<MaybeUninit<T>>),
}

impl<'a, T: CatlaObject> ReturnPlace<'a, T> {
    #[inline(always)]
    pub fn reborrow<'b>(&'b mut self) -> ReturnPlace<'b, T> {
        match self {
            ReturnPlace::Heap => ReturnPlace::Heap,
            ReturnPlace::Stack(slot) => ReturnPlace::Stack(&mut **slot),
        }
    }
}

impl<T: CatlaObject> CatlaObjectRef<T> {
    #[inline(always)]
    fn add_count(&self) {
        let count_ptr = self.value.count_and_flags_ptr();
        let count_and_flags = unsafe { *count_ptr };

        if !heap(count_and_flags) && !drop_flag(count_and_flags) {
            return;
        }

        if mutex(count_and_flags) {
            let count = unsafe { transmute::<*mut usize, &AtomicUsize>(count_ptr) };
            let old_count = count.fetch_add(1, Ordering::Relaxed);

            if old_count == 0 || (old_count & COUNTER_BIT_MASK) == COUNTER_BIT_MASK {
                abort();
            }
        } else {
            unsafe { *count_ptr += 1 };

            if (count_and_flags & COUNTER_BIT_MASK) == COUNTER_BIT_MASK {
                abort();
            }
        }
    }

    #[inline(always)]
    fn sub_count(&self) {
        let count_ptr = self.value.count_and_flags_ptr();
        let count_and_flags = unsafe { *count_ptr };

        if !heap(count_and_flags) && !drop_flag(count_and_flags) {
            return;
        }

        let should_drop = drop_flag(count_and_flags);
        let should_free = heap(count_and_flags);

        if mutex(count_and_flags) {
            let count = unsafe { transmute::<*mut usize, &AtomicUsize>(count_ptr) };
            let old_count = count.fetch_sub(1, Ordering::Release);

            if (old_count & COUNTER_BIT_MASK) == 1 {
                fence(Ordering::Acquire);

                unsafe {
                    if should_drop {
                        Drop::drop(self.value);
                    }

                    if should_free {
                        drop(Box::from_raw(self.value as *const T as *mut T));
                    } else if should_drop {
                        std::ptr::drop_in_place(self.value.value_ptr());
                    }
                }
            }
        } else {
            let old_count = count_and_flags;
            unsafe { *count_ptr -= 1 };

            if (old_count & COUNTER_BIT_MASK) == 1 {
                unsafe {
                    if should_drop {
                        Drop::drop(self.value);
                    }

                    if should_free {
                        drop(Box::from_raw(self.value as *const T as *mut T));
                    } else if should_drop {
                        std::ptr::drop_in_place(self.value.value_ptr());
                    }
                }
            }
        }
    }

    #[inline(always)]
    pub fn heap(value: T::Value, mutex: bool, drop: bool) -> Self {
        let value = Box::leak(Box::new(T::new(value, mutex, true, drop)));
        Self { value }
    }

    #[inline(always)]
    pub fn stack(
        value: T::Value,
        drop: bool,
        ptr: &mut ManuallyDrop<MaybeUninit<T>>,
    ) -> Self {
        ptr.write(T::new(value, false, false, drop));
        Self {
            value: unsafe { std::mem::transmute(ptr.assume_init_ref()) },
        }
    }

    #[inline(always)]
    pub fn from_return_place(
        value: T::Value,
        mutex: bool,
        drop: bool,
        return_place: ReturnPlace<'_, T>,
    ) -> Self {
        match return_place {
            ReturnPlace::Heap => Self::heap(value, mutex, drop),
            ReturnPlace::Stack(slot) => Self::stack(value, drop, slot),
        }
    }

    #[inline(always)]
    pub fn value(&self) -> &mut T::Value {
        unsafe { &mut *self.value.value_ptr() }
    }

    pub fn count(&self) -> usize {
        let count_and_flags = unsafe { *self.value.count_and_flags_ptr() };

        if !heap(count_and_flags) && !drop_flag(count_and_flags) {
            return 1;
        }

        count_and_flags & COUNTER_BIT_MASK
    }

    pub fn is_mutex(&self) -> bool {
        let count_and_flags = unsafe { *self.value.count_and_flags_ptr() };
        mutex(count_and_flags)
    }

    pub fn into_mutex(&self) {
        let count_and_flags = unsafe { *self.value.count_and_flags_ptr() };

        if !heap(count_and_flags) && !drop_flag(count_and_flags) {
            abort();
        }

        if !mutex(count_and_flags) {
            unsafe { *self.value.count_and_flags_ptr() |= 1 << BIT_SHIFT_MUTEX };
        }
    }

    pub fn borrow(&self) -> Self {
        Self { value: self.value }
    }
}

impl<T: CatlaObject> Clone for CatlaObjectRef<T> {
    fn clone(&self) -> Self {
        self.add_count();

        Self { value: self.value }
    }
}

impl<T: CatlaObject> Drop for CatlaObjectRef<T> {
    fn drop(&self) {
        Drop::drop(self.value);
    }
}

impl<T: CatlaObject> std::ops::Drop for CatlaObjectRef<T> {
    fn drop(&mut self) {
        self.sub_count();
    }
}

impl<T: CatlaObject> Deref for CatlaObjectRef<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.value
    }
}

#[cfg(test)]
mod test {
    use std::cell::UnsafeCell;

    use crate::{
        dispose::Drop,
        object::{CatlaObject, CatlaObjectRef, init_count_and_flags},
    };

    #[derive(Debug)]
    struct TestObject {
        value: UnsafeCell<i32>,
        count_and_flags: UnsafeCell<usize>,
    }

    unsafe impl Sync for TestObject {}
    unsafe impl Send for TestObject {}

    impl Drop for TestObject {
        fn drop(&self) {}
    }

    impl CatlaObject for TestObject {
        type Value = i32;

        fn new(value: Self::Value, mutex: bool, heap: bool, drop: bool) -> Self {
            Self {
                value: UnsafeCell::new(value),
                count_and_flags: UnsafeCell::new(init_count_and_flags(mutex, heap, drop)),
            }
        }

        fn count_and_flags_ptr(&self) -> *mut usize {
            self.count_and_flags.get()
        }

        fn value_ptr(&self) -> *mut Self::Value {
            self.value.get()
        }
    }

    #[test]
    fn catla_ref_test() {
        let object = CatlaObjectRef::<TestObject>::heap(100, false, true);
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
