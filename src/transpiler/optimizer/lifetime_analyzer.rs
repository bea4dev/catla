use bumpalo::Bump;
use catla_parser::parser::{Program, Spanned};
use either::Either;
use fxhash::FxHashMap;
use lifetime_collector::collect_lifetime_program;

use crate::transpiler::{
    component::EntityID, context::TranspileModuleContext, name_resolver::FoundDefineInfo,
    semantics::types::type_inference::TypeInferenceResultContainer,
};

pub mod lifetime_collector;

pub static STATIC_LIFETIME: Lifetime = Lifetime {
    drop_position: usize::MAX,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Lifetime {
    pub drop_position: usize,
}

impl Lifetime {}

pub struct LifetimeScope<'allocator, 'instance> {
    entities: Vec<EntityID, &'allocator Bump>,
    lifetime_instance: &'instance mut LifetimeInstance,
}

impl<'allocator, 'instance> LifetimeScope<'allocator, 'instance> {
    pub fn new(
        lifetime_instance: &'instance mut LifetimeInstance,
        allocator: &'allocator Bump,
    ) -> Self {
        Self {
            entities: Vec::new_in(allocator),
            lifetime_instance,
        }
    }

    pub fn add(&mut self, entity_id: EntityID) {
        self.entities.push(entity_id);
    }

    pub fn drop(&mut self, entity_id: EntityID) {
        let lifetime = self.lifetime_instance.next_lifetime();
        self.lifetime_instance
            .lifetime_entity_map
            .insert(entity_id, lifetime);
    }

    pub fn add_expected(&mut self, expected: LifetimeExpected) {
        self.lifetime_instance.lifetime_expected.push(expected);
    }

    pub fn collect(self) {
        for entity_id in self.entities {
            let lifetime = self.lifetime_instance.next_lifetime();
            self.lifetime_instance
                .lifetime_entity_map
                .insert(entity_id, lifetime);
        }
    }
}

pub struct StackLifetimeScope<'allocator> {
    entities: Vec<EntityID, &'allocator Bump>,
}

impl<'allocator> StackLifetimeScope<'allocator> {
    pub fn new(allocator: &'allocator Bump) -> Self {
        Self {
            entities: Vec::new_in(allocator),
        }
    }

    pub fn add(&mut self, entity_id: EntityID) {
        self.entities.push(entity_id);
    }
}

pub struct LifetimeInstance {
    counter: usize,
    lifetime_expected: Vec<LifetimeExpected>,
    lifetime_entity_map: FxHashMap<EntityID, Lifetime>,
    scoop_groups: Vec<ScoopGroup>,
}

impl LifetimeInstance {
    pub fn new() -> Self {
        Self {
            counter: 0,
            lifetime_expected: Vec::new(),
            lifetime_entity_map: FxHashMap::default(),
            scoop_groups: Vec::new(),
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
    pub longer: Either<EntityID, Lifetime>,
}

pub struct ScoopGroup {
    group_entities: Vec<EntityID>,
    value_entity: EntityID,
}

pub fn collect_lifetime(
    ast: Program,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    type_inference_result: &TypeInferenceResultContainer,
    allocator: &Bump,
    context: &TranspileModuleContext,
) -> FxHashMap<EntityID, LifetimeInstance> {
    let mut lifetime_instance_map = FxHashMap::default();

    let mut lifetime_instance = LifetimeInstance::new();
    let mut lifetime_scope = LifetimeScope::new(&mut lifetime_instance, allocator);
    let mut stack_lifetime_scope = StackLifetimeScope::new(allocator);

    collect_lifetime_program(
        ast,
        None,
        import_element_map,
        name_resolved_map,
        type_inference_result,
        &mut lifetime_scope,
        &mut stack_lifetime_scope,
        &mut lifetime_instance_map,
        allocator,
        context,
    );

    lifetime_scope.collect();
    lifetime_instance_map.insert(EntityID::from(ast), lifetime_instance);

    lifetime_instance_map
}
