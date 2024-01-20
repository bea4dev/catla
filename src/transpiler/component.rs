use std::{mem::transmute, cell::{RefCell, Ref, RefMut}, ops::{Index, IndexMut}};

use bumpalo::{collections::String, Bump};
use hashbrown::{HashMap, hash_map::DefaultHashBuilder};


#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
pub(crate) struct EntityID(usize);


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


pub(crate) struct EntityIDMapper<'allocator> {
    map: HashMap<*const u8, EntityID, DefaultHashBuilder, &'allocator Bump>,
    current_id: usize
}

impl<'allocator> EntityIDMapper<'allocator> {
    
    pub(crate) fn new(allocator: &'allocator Bump) -> EntityIDMapper<'allocator> {
        return Self {
            map: HashMap::new_in(allocator),
            current_id: 0
        };
    }

    pub(crate) fn alloc_id<T: Sized>(&mut self, ast_ptr: Option<&T>) -> EntityID {
        return match ast_ptr {
            Some(ast_ptr) => {
                *self.map.entry(unsafe { transmute(ast_ptr) }).or_insert_with(|| { self.current_id += 1; EntityID(self.current_id) })
            },
            _ => { self.current_id += 1; EntityID(self.current_id) }
        };
    }

}



pub(crate) struct NameEnvironment<'allocator> {
    map: HashMap<String<'allocator>, EntityID, DefaultHashBuilder, &'allocator Bump>,
    parent: Option<EntityID>
}

impl<'allocator> NameEnvironment<'allocator> {
    
    pub(crate) fn new(parent: Option<EntityID>, allocator: &'allocator Bump) -> NameEnvironment<'allocator> {
        return Self {
            map: HashMap::new_in(allocator),
            parent
        };
    }

    fn get_id<T: Sized>(&self, name: &String<'allocator>, ast_ptr: Option<&T>, environments: &ComponentContainer<NameEnvironment<'allocator>>) -> Option<EntityID> {
        return match self.map.get(name) {
            Some(entity_id) => Some(*entity_id),
            _ => {
                let parent = match self.parent {
                    Some(parent) => &environments[parent],
                    _ => return None
                };
                parent.get_id(name, ast_ptr, environments)
            }
        };
    }

}

pub(crate) fn get_name_entity_id<'allocator, T: Sized>(
    current_environment_id: EntityID,
    name: &String<'allocator>,
    ast_ptr: Option<&T>,
    environments: &mut ComponentContainer<NameEnvironment<'allocator>>,
    id_mapper: &mut EntityIDMapper
) -> EntityID {

    let current_environment = &environments[current_environment_id];
    return match current_environment.get_id(name, ast_ptr.clone(), environments) {
        Some(name_entity_id) => name_entity_id,
        _ => {
            let current_environment = &mut environments[current_environment_id];
            let name_entity_id = id_mapper.alloc_id(ast_ptr);
            current_environment.map.insert(name.clone(), name_entity_id);
            name_entity_id
        }
    };
}