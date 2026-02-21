use crate::memory::CatlaRefManagement;

#[repr(transparent)]
pub struct CatlaObjectHolder<T: CatlaRefManagement + Copy> {
    object: T,
}

impl<T: CatlaRefManagement + Copy> CatlaObjectHolder<T> {
    #[inline(always)]
    pub fn clone_non_mutex(&self) -> Self {
        self.object.clone_ref_non_mutex();
        Self {
            object: self.object,
        }
    }

    #[inline(always)]
    pub fn clone_ref(&self) -> T {
        self.object.clone_ref();
        self.object
    }

    #[inline(always)]
    pub fn clone_ref_non_mutex(&self) -> T {
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

    #[inline(always)]
    pub fn drop_without_free(self) {
        self.object.drop_ref_without_free_this();
    }

    #[inline(always)]
    pub fn drop_with_free(self) {
        self.object.drop_ref();
    }
}

impl<T: CatlaRefManagement + Copy> Clone for CatlaObjectHolder<T> {
    #[inline(always)]
    fn clone(&self) -> Self {
        self.object.clone_ref();
        Self {
            object: self.object,
        }
    }
}

impl<T: CatlaRefManagement + Copy> Drop for CatlaObjectHolder<T> {
    #[inline(always)]
    fn drop(&mut self) {
        self.object.drop_ref();
    }
}

#[repr(transparent)]
pub struct CatlaObjectDummyHolder<T: CatlaRefManagement + Copy> {
    object: T,
}

impl<T: CatlaRefManagement + Copy> CatlaObjectDummyHolder<T> {
    #[inline(always)]
    pub fn borrow(&self) -> T {
        self.object
    }

    #[inline(always)]
    pub fn drop_without_free(self) {}

    #[inline(always)]
    pub fn drop_with_free(self) {}
}

pub trait Hold
where
    Self: CatlaRefManagement + Copy,
{
    fn hold(self) -> CatlaObjectHolder<Self>;

    fn hold_dummy(self) -> CatlaObjectDummyHolder<Self>;
}

impl<T: CatlaRefManagement + Copy> Hold for T {
    #[inline(always)]
    fn hold(self) -> CatlaObjectHolder<Self> {
        CatlaObjectHolder { object: self }
    }

    #[inline(always)]
    fn hold_dummy(self) -> CatlaObjectDummyHolder<Self> {
        CatlaObjectDummyHolder { object: self }
    }
}
