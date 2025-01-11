use crate::memory::CatlaRefManagement;

#[repr(transparent)]
pub struct CatlaObjectHolder<T: CatlaRefManagement + Copy> {
    object: T
}

impl<T: CatlaRefManagement + Copy> CatlaObjectHolder<T> {
    #[inline(always)]
    pub fn clone(&self) -> T {
        self.object.clone_ref();
        self.object
    }

    #[inline(always)]
    pub fn clone_non_mutex(&self) -> T {
        self.object.clone_ref_non_mutex();
        self.object
    }

    #[inline(always)]
    pub fn consume(self) -> T {
        self.object
    }

    #[inline(always)]
    pub fn borrow(&self) -> T {
        self.object
    }
}

impl<T: CatlaRefManagement + Copy> Drop for CatlaObjectHolder<T> {
    #[inline(always)]
    fn drop(&mut self) {
        self.object.drop_ref();
    }
}


pub trait Hold where Self: CatlaRefManagement + Copy {
    fn hold(self) -> CatlaObjectHolder<Self>;
}

impl<T: CatlaRefManagement + Copy> Hold for T {
    #[inline(always)]
    fn hold(self) -> CatlaObjectHolder<Self> {
        CatlaObjectHolder { object: self }
    }
}
