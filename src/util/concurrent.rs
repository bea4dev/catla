use std::hint;
use std::sync::atomic::{AtomicBool, Ordering};

pub struct SpinLock {
    lock_flag: AtomicBool
}

impl SpinLock {
    #[inline(always)]
    pub fn new() -> Self {
        return Self {
            lock_flag: AtomicBool::new(false)
        }
    }

    #[inline(always)]
    pub fn lock(&mut self) {
        loop {
            if !self.lock_flag.swap(true, Ordering::Acquire) {
                break;
            }
            while self.lock_flag.load(Ordering::Relaxed) {
                hint::spin_loop();
            }
        }
    }

    #[inline(always)]
    pub fn unlock(&mut self) {
        self.lock_flag.store(false, Ordering::Release);
    }
}