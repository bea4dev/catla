use std::{cell::RefCell, sync::Arc};

use allocator_api2::vec::Vec;
use bumpalo::Bump;
use catla_parser::parser::{Program, Spanned};
use fxhash::{FxHashMap, FxHashSet};
use lifetime_collector::collect_lifetime_program;

use crate::transpiler::{
    component::EntityID,
    context::TranspileModuleContext,
    name_resolver::FoundDefineInfo,
    semantics::types::{
        type_inference::TypeInferenceResultContainer,
        type_info::{FunctionType, Type},
    },
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
        let lifetime_tree = self.instance.get_lifetime_tree(lifetime_tree_ref);
        lifetime_tree.lifetimes.push(lifetime);
    }

    pub fn add_expected(&mut self, expected: LifetimeExpected) {
        self.instance.lifetime_expected.push(expected);
    }

    pub fn collect(self) {
        for lifetime_tree_ref in self.lifetime_trees.iter().rev() {
            let lifetime = self.instance.next_lifetime();
            let lifetime_tree_ref = self.instance.resolve_lifetime_ref(*lifetime_tree_ref);
            let lifetime_tree = self.instance.get_lifetime_tree(lifetime_tree_ref);
            lifetime_tree.lifetimes.push(lifetime);
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
            let lifetime_tree_ref = lifetime_instance.resolve_lifetime_ref(lifetime_tree_ref);
            let lifetime_tree = lifetime_instance.get_lifetime_tree(lifetime_tree_ref);
            lifetime_tree.lifetimes.push(lifetime);
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
    lifetime_ref_reference_map: FxHashMap<LifetimeTreeRef, LifetimeTreeRef>,
    this_argument_lifetime_ref: Option<LifetimeTreeRef>,
    argument_lifetime_ref: FxHashMap<EntityID, LifetimeTreeRef>,
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
            lifetime_ref_reference_map: FxHashMap::default(),
            this_argument_lifetime_ref: None,
            argument_lifetime_ref: FxHashMap::default(),
        }
    }

    pub fn init_function_argument(
        &mut self,
        arguments: impl Iterator<Item = EntityID>,
        has_this_argument: bool,
    ) {
        if has_this_argument {
            let this_lifetime_ref = self.create_lifetime_tree();
            self.this_argument_lifetime_ref = Some(this_lifetime_ref);
        }

        self.argument_lifetime_ref.clear();

        for argument in arguments {
            self.create_entity_lifetime_tree(argument);
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

        self.lifetime_tree_map
            .insert(lifetime_tree_ref, lifetime_tree);

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

    pub fn resolve_lifetime_ref(&self, mut lifetime_tree_ref: LifetimeTreeRef) -> LifetimeTreeRef {
        loop {
            match self.lifetime_ref_reference_map.get(&lifetime_tree_ref) {
                Some(resolved_ref) => lifetime_tree_ref = *resolved_ref,
                None => return lifetime_tree_ref,
            }
        }
    }

    pub fn get_entity_lifetime_tree(&mut self, entity_id: EntityID) -> &mut LifetimeTree {
        let lifetime_ref = self.get_entity_lifetime_tree_ref(entity_id);
        let lifetime_ref = self.resolve_lifetime_ref(lifetime_ref);
        self.lifetime_tree_map.get_mut(&lifetime_ref).unwrap()
    }

    pub fn get_lifetime_tree(&mut self, lifetime_tree_ref: LifetimeTreeRef) -> &mut LifetimeTree {
        let lifetime_tree_ref = self.resolve_lifetime_ref(lifetime_tree_ref);
        self.lifetime_tree_map.get_mut(&lifetime_tree_ref).unwrap()
    }

    pub fn add_function_call(&mut self, function_call: FunctionCallLifetime) {
        self.function_calls.push(function_call);
    }

    pub fn merge(
        &mut self,
        lifetime_ref_left: LifetimeTreeRef,
        lifetime_ref_right: LifetimeTreeRef,
    ) {
        let left_resolved_ref = self.resolve_lifetime_ref(lifetime_ref_left);
        let right_resolved_ref = self.resolve_lifetime_ref(lifetime_ref_right);

        if left_resolved_ref == right_resolved_ref {
            return;
        }

        let left_lifetime_tree = self.lifetime_tree_map.remove(&left_resolved_ref);
        let right_lifetime_tree = self.lifetime_tree_map.remove(&right_resolved_ref);

        let merged_tree = match (left_lifetime_tree, right_lifetime_tree) {
            (Some(mut left_lifetime_tree), Some(mut right_lifetime_tree)) => {
                for (left_child_name, left_child_ref) in left_lifetime_tree.children.iter() {
                    if let Some(right_child_ref) =
                        right_lifetime_tree.children.remove(left_child_name)
                    {
                        self.merge(*left_child_ref, right_child_ref);
                    }
                }

                left_lifetime_tree
                    .children
                    .extend(right_lifetime_tree.children);
                left_lifetime_tree
                    .lifetimes
                    .extend(right_lifetime_tree.lifetimes);
                left_lifetime_tree
                    .alloc_point_ref
                    .extend(right_lifetime_tree.alloc_point_ref);
                left_lifetime_tree
                    .borrow_ref
                    .extend(right_lifetime_tree.borrow_ref);

                if left_lifetime_tree.is_merged {
                    left_lifetime_tree.is_alloc_point =
                        left_lifetime_tree.is_alloc_point && right_lifetime_tree.is_alloc_point;
                } else {
                    left_lifetime_tree.is_alloc_point = right_lifetime_tree.is_alloc_point;
                }
                left_lifetime_tree.is_merged = true;

                if !left_lifetime_tree.is_alloc_point && right_lifetime_tree.is_alloc_point {
                    left_lifetime_tree.lifetimes.push(STATIC_LIFETIME);
                }

                left_lifetime_tree
            }
            _ => unreachable!(),
        };

        self.lifetime_ref_reference_map
            .insert(right_resolved_ref, left_resolved_ref);
        self.lifetime_tree_map
            .insert(left_resolved_ref, merged_tree);
    }

    pub fn get_or_create_child(
        &mut self,
        parent: LifetimeTreeRef,
        child_name: &str,
    ) -> LifetimeTreeRef {
        let parent_resolved = self.resolve_lifetime_ref(parent);

        {
            let parent_lifetime_tree = self.get_lifetime_tree(parent_resolved);

            if let Some(child) = parent_lifetime_tree.children.get(child_name) {
                return *child;
            }
        }

        let new_child_ref = self.create_lifetime_tree();
        let parent_lifetime_tree = self.get_lifetime_tree(parent_resolved);
        parent_lifetime_tree
            .children
            .insert(child_name.to_string(), new_child_ref);

        let parent_alloc_point_ref = parent_lifetime_tree.alloc_point_ref.clone();
        let parent_borrow_ref = parent_lifetime_tree.borrow_ref.clone();

        for parent_borrow_ref in parent_borrow_ref {
            
        }

        new_child_ref
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
    pub lifetimes: Vec<Lifetime>,
    pub children: FxHashMap<String, LifetimeTreeRef>,
    pub is_merged: bool,
    pub is_alloc_point: bool,
    pub alloc_point_ref: FxHashSet<LifetimeTreeRef>,
    pub borrow_ref: FxHashSet<LifetimeTreeRef>,
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
    let return_value_tree_ref = lifetime_scope.instance.create_lifetime_tree();

    collect_lifetime_program(
        ast,
        None,
        return_value_tree_ref,
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
