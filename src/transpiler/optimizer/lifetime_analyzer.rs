use core::panic;
use std::{cell::RefCell, sync::Arc};

use allocator_api2::vec::Vec;
use bumpalo::Bump;
use catla_parser::parser::{Program, Spanned};
use fxhash::FxHashMap;
use lifetime_collector::collect_lifetime_program;

use crate::transpiler::{
    component::EntityID,
    context::TranspileModuleContext,
    name_resolver::FoundDefineInfo,
    semantics::types::{type_inference::TypeInferenceResultContainer, type_info::{FunctionType, Type}},
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
    lifetime_trees: Vec<LifetimeTreeRef, &'allocator Bump>,
    pub(crate) instance: &'instance mut LifetimeInstance,
}

impl<'allocator, 'instance> LifetimeScope<'allocator, 'instance> {
    pub fn new(
        lifetime_instance: &'instance mut LifetimeInstance,
        allocator: &'allocator Bump,
    ) -> Self {
        Self {
            lifetime_trees: Vec::new_in(allocator),
            instance: lifetime_instance,
        }
    }

    pub fn add(&mut self, lifetime_tree_ref: LifetimeTreeRef) {
        self.lifetime_trees.push(lifetime_tree_ref);
    }

    pub fn drop(&mut self, lifetime_tree_ref: LifetimeTreeRef) {
        let lifetime = self.instance.next_lifetime();
        self.instance.set_lifetime(lifetime_tree_ref, lifetime);
    }

    pub fn add_expected(&mut self, expected: LifetimeExpected) {
        self.instance.lifetime_expected.push(expected);
    }

    pub fn collect(self) {
        for lifetime_tree_ref in self.lifetime_trees.iter().rev() {
            let lifetime = self.instance.next_lifetime();
            self.instance.set_lifetime(*lifetime_tree_ref, lifetime);
        }
    }
}

pub struct StackLifetimeScope<'allocator> {
    lifetime_trees: Vec<LifetimeTreeRef, &'allocator Bump>,
}

impl<'allocator> StackLifetimeScope<'allocator> {
    pub fn new(allocator: &'allocator Bump) -> Self {
        Self {
            lifetime_trees: Vec::new_in(allocator),
        }
    }

    pub fn add(&mut self, lifetime_tree_ref: LifetimeTreeRef) {
        self.lifetime_trees.push(lifetime_tree_ref);
    }

    pub fn collect(self, lifetime_instance: &mut LifetimeInstance) {
        let lifetime = lifetime_instance.next_lifetime();

        for lifetime_tree_ref in self.lifetime_trees {
            lifetime_instance.set_lifetime(lifetime_tree_ref, lifetime);
        }
    }
}

pub struct LifetimeInstance {
    lifetime_counter: usize,
    tree_ref_counter: usize,
    lifetime_expected: Vec<LifetimeExpected>,
    function_calls: Vec<FunctionCallLifetime>,
    entity_lifetime_ref_map: FxHashMap<EntityID, LifetimeTreeRef>,
    lifetime_tree_map: FxHashMap<LifetimeTreeRef, LifetimeTree>,
    scoop_groups: Vec<ScoopGroup>,
}

impl LifetimeInstance {
    pub fn new() -> Self {
        Self {
            lifetime_counter: 0,
            tree_ref_counter: 0,
            lifetime_expected: Vec::new(),
            function_calls: Vec::new(),
            entity_lifetime_ref_map: FxHashMap::default(),
            lifetime_tree_map: FxHashMap::default(),
            scoop_groups: Vec::new(),
        }
    }

    pub fn next_lifetime(&mut self) -> Lifetime {
        let drop_position = self.lifetime_counter;
        self.lifetime_counter += 1;
        Lifetime { drop_position }
    }

    pub fn create_entity_lifetime_tree(&mut self, entity_id: EntityID) -> LifetimeTreeRef {
        let lifetime_tree = LifetimeTree::default();
        let lifetime_tree_ref = self.new_lifetime_tree_ref();

        self.entity_lifetime_ref_map
            .insert(entity_id, lifetime_tree_ref);
        self.lifetime_tree_map
            .insert(lifetime_tree_ref, lifetime_tree);

        lifetime_tree_ref
    }

    pub fn create_lifetime_tree(&mut self) -> LifetimeTreeRef {
        let lifetime_tree = LifetimeTree::default();
        let lifetime_tree_ref = self.new_lifetime_tree_ref();
        self.lifetime_tree_map.insert(lifetime_tree_ref, lifetime_tree);
        lifetime_tree_ref
    }

    pub fn new_lifetime_tree_ref(&mut self) -> LifetimeTreeRef {
        let count_old = self.tree_ref_counter;
        self.tree_ref_counter += 1;
        LifetimeTreeRef(count_old)
    }

    pub fn get_entity_lifetime_tree_ref(&self, entity_id: EntityID) -> LifetimeTreeRef {
        *self.entity_lifetime_ref_map.get(&entity_id).unwrap()
    }

    pub fn get_entity_lifetime_tree(&mut self, entity_id: EntityID) -> &mut LifetimeTree {
        let lifetime_ref = self.get_entity_lifetime_tree_ref(entity_id);
        self.lifetime_tree_map.get_mut(&lifetime_ref).unwrap()
    }

    pub fn get_lifetime_tree(&mut self, lifetime_tree_ref: LifetimeTreeRef) -> &mut LifetimeTree {
        self.lifetime_tree_map.get_mut(&lifetime_tree_ref).unwrap()
    }

    pub fn set_lifetime(&mut self, lifetime_tree_ref: LifetimeTreeRef, lifetime: Lifetime) {
        let lifetime_tree = self.get_lifetime_tree(lifetime_tree_ref);

        if lifetime_tree.root_lifetime.is_some() {
            panic!("Lifetime has already set!");
        }

        lifetime_tree.root_lifetime = Some(lifetime);
    }

    pub fn add_same_bound_entity(&mut self, entity_id: EntityID, same_bound_entity: EntityID) {
        let same_bound_entity_ref = *self
            .entity_lifetime_ref_map
            .get(&same_bound_entity)
            .unwrap();
        let lifetime_tree = self.get_entity_lifetime_tree(entity_id);

        lifetime_tree.same_bound_entity.push(same_bound_entity_ref);
    }

    pub fn add_function_call(&mut self, function_call: FunctionCallLifetime) {
        self.function_calls.push(function_call);
    }
}

pub struct LifetimeExpected {
    pub shorter: TypedElementAccess,
    pub longer: TypedElementAccess,
}

pub struct TypedElementAccess {
    pub root: EntityID,
    pub element_path: Vec<String>,
}

#[derive(Debug, Default)]
pub struct LifetimeTree {
    pub root_lifetime: Option<Lifetime>,
    pub same_bound_entity: Vec<LifetimeTreeRef>,
    pub children: FxHashMap<String, LifetimeTreeRef>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LifetimeTreeRef(usize);

pub struct FunctionCallLifetime {
    pub arguments: Vec<LifetimeTreeRef>,
    pub return_value: LifetimeTreeRef,
    pub function: Arc<FunctionType>,
}

pub struct ScoopGroup {
    group_entities: RefCell<Vec<EntityID>>,
    value_entity: EntityID,
}

impl ScoopGroup {
    pub fn new(value_entity: EntityID) -> Self {
        Self {
            group_entities: RefCell::new(Vec::new()),
            value_entity,
        }
    }

    pub fn add(&self, entity_id: EntityID) {
        let mut group_entities = self.group_entities.borrow_mut();
        group_entities.push(entity_id);
    }
}

pub fn collect_lifetime(
    ast: Program,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
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
        module_user_type_map,
        module_element_type_map,
        module_element_type_maps,
        type_inference_result,
        &mut lifetime_scope,
        &mut stack_lifetime_scope,
        &mut lifetime_instance_map,
        allocator,
        context,
    );

    lifetime_scope.collect();
    stack_lifetime_scope.collect(&mut lifetime_instance);
    lifetime_instance_map.insert(EntityID::from(ast), lifetime_instance);

    lifetime_instance_map
}
