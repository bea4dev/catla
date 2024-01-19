use std::cell::{RefMut, RefCell};

use self::component::{ComponentContainer, NameEnvironment};

pub mod component;
pub mod name_resolver;


pub(crate) struct ComponentContainers<'allocator> {
    name_environments: RefCell<ComponentContainer<'allocator, NameEnvironment<'allocator>>>
}

impl<'allocator> ComponentContainers<'allocator> {
    
    pub(crate) fn get_name_environment(&mut self) -> RefMut<'_, ComponentContainer<'allocator, NameEnvironment<'allocator>>> {
        return self.name_environments.borrow_mut()
    }

}