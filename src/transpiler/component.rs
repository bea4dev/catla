use std::{fmt::Debug, hash::Hash, mem::transmute, ops::{Index, IndexMut}};

use bumpalo::Bump;
use catla_parser::parser::AST;
use hashbrown::{HashMap, hash_map::DefaultHashBuilder};


#[derive(Clone, Copy)]
pub(crate) struct EntityID {
    ptr: usize
}

impl PartialEq for EntityID {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}

impl Eq for EntityID {}

impl Hash for EntityID {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ptr.hash(state);
    }
}

impl<T: Sized + AST> From<&T> for EntityID {
    fn from(value: &T) -> Self {
        Self { ptr: unsafe { transmute(value) } }
    }
}

impl Debug for EntityID {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EntityID").field("ptr", &self.ptr).finish()
    }
}

pub(crate) struct ComponentContainer<'allocator, T: Sized> {
    map: HashMap<EntityID, T, DefaultHashBuilder, &'allocator Bump>,
}

impl<'allocator, T: Sized> ComponentContainer<'allocator, T> {
    
    pub(crate) fn new(allocator: &'allocator Bump) -> ComponentContainer<'allocator, T> {
        Self {
            map: HashMap::new_in(allocator)
        }
    }

    pub(crate) fn insert(&mut self, key: EntityID, value: T) {
        self.map.insert(key, value);
    }

}

impl<'allocator, T: Sized> Index<EntityID> for ComponentContainer<'allocator, T> {
    type Output = T;

    fn index(&self, index: EntityID) -> &Self::Output {
        self.map.get(&index).expect(format!("Unknown entity id : {:?}", index).as_str())
    }
}

impl<'allocator, T: Sized + Send> IndexMut<EntityID> for ComponentContainer<'allocator, T> {

    fn index_mut(&mut self, index: EntityID) -> &mut Self::Output {
        self.map.get_mut(&index).expect(format!("Unknown entity id : {:?}", index).as_str())
    }

}