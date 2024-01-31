use std::{fmt::Debug, hash::Hash, mem::transmute, ops::{Index, IndexMut}};

use bumpalo::Bump;
use catla_parser::parser::AST;
use hashbrown::{HashMap, hash_map::DefaultHashBuilder};


#[derive(Clone, Copy)]
pub(crate) struct EntityID<'allocator> {
    ptr: usize,
    ast: &'allocator dyn AST
}

impl PartialEq for EntityID<'_> {
    fn eq(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}

impl Eq for EntityID<'_> {}

impl Hash for EntityID<'_> {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.ptr.hash(state);
    }
}

impl<'allocator, T: Sized + AST> From<&'allocator T> for EntityID<'allocator> {
    fn from(value: &'allocator T) -> Self {
        return EntityID { ptr: unsafe { transmute(value) }, ast: value };
    }
}

impl Debug for EntityID<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EntityID").field("ptr", &self.ptr).finish()
    }
}



pub(crate) struct ComponentContainer<'allocator, T: Sized> {
    map: HashMap<EntityID<'allocator>, T, DefaultHashBuilder, &'allocator Bump>,
}

impl<'allocator, T: Sized> ComponentContainer<'allocator, T> {
    
    pub(crate) fn new(allocator: &'allocator Bump) -> ComponentContainer<'allocator, T> {
        return Self {
            map: HashMap::new_in(allocator)
        };
    }

    pub(crate) fn insert(&mut self, key: EntityID<'allocator>, value: T) {
        self.map.insert(key, value);
    }

}

impl<'allocator, T: Sized> Index<EntityID<'allocator>> for ComponentContainer<'allocator, T> {
    type Output = T;

    fn index(&self, index: EntityID<'allocator>) -> &Self::Output {
        return self.map.get(&index).expect(format!("Unknown entity id : {:?}", index).as_str());
    }
}

impl<'allocator, T: Sized> IndexMut<EntityID<'allocator>> for ComponentContainer<'allocator, T> {

    fn index_mut(&mut self, index: EntityID<'allocator>) -> &mut Self::Output {
        return self.map.get_mut(&index).expect(format!("Unknown entity id : {:?}", index).as_str());
    }

}