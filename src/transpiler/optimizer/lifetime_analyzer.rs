use bumpalo::Bump;
use either::Either;
use fxhash::FxHashMap;

use crate::transpiler::component::EntityID;

pub mod lifetime_collector;


pub static STATIC_LIFETIME: Lifetime = Lifetime { drop_position: usize::MAX };


#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Lifetime {
    pub drop_position: usize
}


impl Lifetime {
    
}



pub struct LifetimeScope<'allocator, 'instance> {
    entities: Vec<EntityID, &'allocator Bump>,
    lifetime_instance: &'instance mut LifetimeInstance
}

impl<'allocator, 'instance> LifetimeScope<'allocator, 'instance> {

    pub fn new(lifetime_instance: &'instance mut LifetimeInstance, allocator: &'allocator Bump) -> Self {
        Self {
            entities: Vec::new_in(allocator),
            lifetime_instance
        }
    }

    pub fn add(&mut self, entity_id: EntityID) {
        self.entities.push(entity_id);
    }

    pub fn drop(&mut self, entity_id: EntityID) {
        let lifetime = self.lifetime_instance.next_lifetime();
        self.lifetime_instance.lifetime_entity_map.insert(entity_id, lifetime);
    }

    pub fn add_expected(&mut self, expected: LifetimeExpected) {
        self.lifetime_instance.lifetime_expected.push(expected);
    }

    pub fn collect(self) {
        for entity_id in self.entities {
            let lifetime = self.lifetime_instance.next_lifetime();
            self.lifetime_instance.lifetime_entity_map.insert(entity_id, lifetime);
        }
    }

}


pub struct LifetimeInstance {
    counter: usize,
    lifetime_expected: Vec<LifetimeExpected>,
    lifetime_entity_map: FxHashMap<EntityID, Lifetime>,
    scoop_groups: Vec<ScoopGroup>
}

impl LifetimeInstance {

    pub fn new() -> Self {
        Self {
            counter: 0,
            lifetime_expected: Vec::new(),
            lifetime_entity_map: FxHashMap::default(),
            scoop_groups: Vec::new()
        }
    }

    pub fn next_lifetime(&mut self) -> Lifetime {
        let drop_position = self.counter;
        self.counter += 1;
        Lifetime { drop_position }
    }

}


pub struct LifetimeExpected {
    pub shorter: Either<EntityID, Lifetime>,
    pub longer: Either<EntityID, Lifetime>
}


pub struct ScoopGroup {
    group_entities: Vec<EntityID>,
    value_entity: EntityID
}
