use std::{cell::RefCell, mem::transmute, ops::{Index, IndexMut}};

use bumpalo::{collections::String, Bump};
use catla_parser::parser::AST;
use hashbrown::{HashMap, hash_map::DefaultHashBuilder};

use super::name_resolver::DefineWithName;


#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub(crate) struct EntityID(usize);

impl<T: Sized + AST> From<&T> for EntityID {
    fn from(value: &T) -> Self {
        return EntityID(unsafe { transmute(value) });
    }
}


pub(crate) struct ComponentContainer<'allocator, T: Sized> {
    map: HashMap<EntityID, T, DefaultHashBuilder, &'allocator Bump>,
}

impl<'allocator, T: Sized> ComponentContainer<'allocator, T> {
    
    pub(crate) fn new(allocator: &'allocator Bump) -> ComponentContainer<'allocator, T> {
        return Self {
            map: HashMap::new_in(allocator)
        };
    }

}

impl<'allocator, T: Sized> Index<EntityID> for ComponentContainer<'allocator, T> {
    type Output = T;

    fn index(&self, index: EntityID) -> &Self::Output {
        return self.map.get(&index).expect(format!("Unknown entity id : {:?}", index).as_str());
    }
}

impl<'allocator, T: Sized> IndexMut<EntityID> for ComponentContainer<'allocator, T> {

    fn index_mut(&mut self, index: EntityID) -> &mut Self::Output {
        return self.map.get_mut(&index).expect(format!("Unknown entity id : {:?}", index).as_str());
    }

}



pub(crate) struct NameEnvironment<'allocator> {
    map: RefCell<HashMap<String<'allocator>, DefineWithName, DefaultHashBuilder, &'allocator Bump>>,
    parent: Option<EntityID>
}

impl<'allocator> NameEnvironment<'allocator> {
    
    pub(crate) fn new(parent: Option<EntityID>, allocator: &'allocator Bump) -> NameEnvironment<'allocator> {
        return Self {
            map: RefCell::new(HashMap::new_in(allocator)),
            parent
        };
    }

    pub(crate) fn get_name_owner(&self, name: &str, environments: &ComponentContainer<NameEnvironment<'allocator>>) -> Option<DefineWithName> {
        return match self.map.borrow().get(name) {
            Some(entity_id) => Some(entity_id.clone()),
            _ => {
                let parent = match self.parent {
                    Some(parent) => &environments[parent],
                    _ => return None
                };
                parent.get_name_owner(name, environments)
            }
        };
    }

    pub(crate) fn set_name_owner(&self, name: String<'allocator>, owner: DefineWithName) {
        self.map.borrow_mut().insert(name, owner);
    }

}