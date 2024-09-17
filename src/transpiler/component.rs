use std::{any::TypeId, fmt::Debug, hash::Hash, mem::transmute, ops::{Index, IndexMut}};

use bumpalo::Bump;
use catla_parser::parser::{AST, AST_TYPE_NAME_MAP};
use hashbrown::{HashMap, hash_map::DefaultHashBuilder};


#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct EntityID {
    ptr: usize,
    type_id: TypeId
}

impl EntityID {
    pub fn dummy() -> Self {
        Self {
            ptr: 0,
            type_id: TypeId::of::<EntityID>()
        }
    }

    pub fn get_type_name(&self) -> &'static str {
        if self.type_id == TypeId::of::<EntityID>() {
            return "Dummy";
        }

        match AST_TYPE_NAME_MAP.get(&self.type_id) {
            Some(type_name) => type_name,
            None => "Unknown"
        }
    }
}

impl<T: Sized + AST> From<&T> for EntityID {
    fn from(value: &T) -> Self {
        Self {
            ptr: unsafe { transmute(value) },
            type_id: typeid::of::<T>()
        }
    }
}

#[derive(Debug)]
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
