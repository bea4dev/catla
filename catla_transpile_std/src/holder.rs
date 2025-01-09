use crate::memory::CatlaRefManagement;

#[repr(transparent)]
pub struct CatlaObjectHolder<T: CatlaRefManagement + Copy> {
    object: T
}

impl<T: CatlaRefManagement + Copy> CatlaObjectHolder<T> {
    pub fn new(object: T) -> Self {
        Self {
            object,
        }
    }

    pub fn clone(&self) -> T {
        self.object.clone_ref();
        self.object
    }

    pub fn clone_non_mutex(&self) -> T {
        self.object.clone_ref_non_mutex();
        self.object
    }

    pub fn last_use(self) -> T {
        self.object
    }
}

impl<T: CatlaRefManagement + Copy> Drop for CatlaObjectHolder<T> {
    fn drop(&mut self) {
        self.object.drop_ref();
    }
}
