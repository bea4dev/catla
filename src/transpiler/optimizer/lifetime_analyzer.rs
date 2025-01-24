use std::{
    mem::swap,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Arc, Mutex, RwLock,
    },
};

use allocator_api2::vec;
use allocator_api2::vec::Vec;
use bumpalo::Bump;
use catla_parser::parser::{Program, Spanned};
use futures::future::join_all;
use fxhash::{FxHashMap, FxHashSet};
use lifetime_collector::collect_lifetime_program;

use crate::transpiler::{
    component::EntityID,
    context::{TranspileContext, TranspileModuleContext},
    name_resolver::FoundDefineInfo,
    semantics::types::{
        type_inference::TypeInferenceResultContainer,
        type_info::{FunctionType, Type},
    },
};

pub mod debug;
pub mod lifetime_collector;

pub static STATIC_LIFETIME: Lifetime = Lifetime {
    drop_position: usize::MAX,
};
pub static STATIC_LIFETIME_REF: LifetimeTreeRef = LifetimeTreeRef(usize::MAX);

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
        self.instance.lifetime_expected.insert(expected);
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

#[derive(Debug, Clone)]
pub struct LifetimeInstance {
    lifetime_counter: usize,
    tree_ref_counter: usize,
    lifetime_expected: FxHashSet<LifetimeExpected>,
    function_calls: Vec<FunctionCallLifetime>,
    entity_lifetime_ref_map: FxHashMap<EntityID, LifetimeTreeRef>,
    lifetime_tree_map: FxHashMap<LifetimeTreeRef, LifetimeTree>,
    lifetime_ref_reference_map: FxHashMap<LifetimeTreeRef, LifetimeTreeRef>,
    this_argument_lifetime_ref: LifetimeTreeRef,
    inserts: Vec<(LifetimeTreeRef, LifetimeTreeRef)>,
    insert_ref_map: FxHashMap<LifetimeTreeRef, Arc<FxHashSet<LifetimeTreeRef>>>,
}

impl LifetimeInstance {
    pub fn new() -> Self {
        let mut instance = Self {
            lifetime_counter: 0,
            tree_ref_counter: 0,
            lifetime_expected: FxHashSet::default(),
            function_calls: Vec::new(),
            entity_lifetime_ref_map: FxHashMap::default(),
            lifetime_tree_map: FxHashMap::default(),
            lifetime_ref_reference_map: FxHashMap::default(),
            this_argument_lifetime_ref: LifetimeTreeRef(0),
            inserts: Vec::new(),
            insert_ref_map: FxHashMap::default(),
        };

        instance.this_argument_lifetime_ref = instance.create_lifetime_tree();

        let static_lifetime_tree = LifetimeTree {
            lifetimes: vec![STATIC_LIFETIME],
            ..Default::default()
        };
        instance
            .lifetime_tree_map
            .insert(STATIC_LIFETIME_REF, static_lifetime_tree);

        instance
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
                left_lifetime_tree
                    .borrow_ref
                    .extend(right_lifetime_tree.borrow_ref);

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

                for (child_name, child_ref) in left_lifetime_tree.children.iter() {
                    let borrowed_ref_children = left_lifetime_tree
                        .borrow_ref
                        .iter()
                        .map(|borrowed_ref| self.get_or_create_child(*borrowed_ref, child_name))
                        .collect::<FxHashSet<_>>();

                    let child_tree = self.get_lifetime_tree(*child_ref);
                    child_tree.borrow_ref.extend(borrowed_ref_children);
                }

                left_lifetime_tree
                    .lifetimes
                    .extend(right_lifetime_tree.lifetimes);
                left_lifetime_tree
                    .alloc_point_ref
                    .extend(right_lifetime_tree.alloc_point_ref);
                left_lifetime_tree.is_argument_tree |= right_lifetime_tree.is_argument_tree;

                left_lifetime_tree.is_alloc_point |= right_lifetime_tree.is_alloc_point;
                left_lifetime_tree.is_merged = true;

                left_lifetime_tree.contains_function_return_value |=
                    right_lifetime_tree.contains_function_return_value;

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
        let is_argument_tree = parent_lifetime_tree.is_argument_tree;
        let contains_function_return_value = parent_lifetime_tree.contains_function_return_value;
        let mut child_borrow_ref_set = FxHashSet::default();

        for parent_borrow_ref in parent_borrow_ref {
            let child_borrow_ref = self.get_or_create_child(parent_borrow_ref, child_name);
            child_borrow_ref_set.insert(child_borrow_ref);
        }

        let child_tree = self.get_lifetime_tree(new_child_ref);
        child_tree.borrow_ref.extend(child_borrow_ref_set);
        child_tree.alloc_point_ref.extend(parent_alloc_point_ref);
        child_tree.is_argument_tree = is_argument_tree;
        child_tree.contains_function_return_value = contains_function_return_value;

        new_child_ref
    }

    pub fn add_lifetime_expected(&mut self, expected: LifetimeExpected) {
        self.lifetime_expected.insert(expected);
    }

    pub fn init_lifetime_expected_relationships(&mut self, changed_flag: &mut bool) {
        for (lifetime_ref, lifetime_tree) in self.lifetime_tree_map.iter() {
            for child_ref in lifetime_tree.children.values() {
                let expected = LifetimeExpected {
                    shorter: *lifetime_ref,
                    should_ignore_insert_on_shorter: false,
                    longer: *child_ref,
                };

                if !self.lifetime_expected.contains(&expected) {
                    self.lifetime_expected.insert(expected);

                    *changed_flag = true;
                }
            }
        }
    }

    pub fn add_insert(&mut self, left: LifetimeTreeRef, right: LifetimeTreeRef) {
        self.inserts.push((left, right));
    }

    pub fn init_insert_map(&mut self) {
        let mut group_sets = Vec::new();

        self.init_insert_groups(&mut group_sets);
        self.init_insert_groups_children(&mut group_sets);

        for group_set in group_sets {
            let group_set = Arc::new(group_set);

            for lifetime_ref in group_set.iter() {
                self.insert_ref_map.insert(*lifetime_ref, group_set.clone());
            }
        }
    }

    fn init_insert_groups(&self, group_sets: &mut Vec<FxHashSet<LifetimeTreeRef>>) {
        for (left, right) in self.inserts.iter().cloned() {
            self.init_insert_groups_lr(left, right, group_sets);
            self.init_insert_groups_borrowed_ref(left, group_sets);
            self.init_insert_groups_borrowed_ref(right, group_sets);
        }
    }

    fn init_insert_groups_children(&self, group_sets: &mut Vec<FxHashSet<LifetimeTreeRef>>) {
        for group_set in group_sets.clone() {
            self.init_insert_groups_children_recursive(&group_set, group_sets);
        }
    }

    fn init_insert_groups_children_recursive(
        &self,
        current_set: &FxHashSet<LifetimeTreeRef>,
        group_sets: &mut Vec<FxHashSet<LifetimeTreeRef>>,
    ) {
        let mut child_names = FxHashSet::default();
        for lifetime_ref in current_set.iter() {
            let lifetime_ref = self.resolve_lifetime_ref(*lifetime_ref);
            let lifetime_tree = self.lifetime_tree_map.get(&lifetime_ref).unwrap();
            child_names.extend(lifetime_tree.children.keys());
        }

        for child_name in child_names {
            let children = current_set
                .iter()
                .map(|lifetime_ref| {
                    let lifetime_ref = self.resolve_lifetime_ref(*lifetime_ref);
                    let lifetime_tree = self.lifetime_tree_map.get(&lifetime_ref).unwrap();
                    lifetime_tree.children.get(child_name)
                })
                .flatten()
                .cloned()
                .collect::<FxHashSet<_>>();

            let mut children_iter = children.iter();

            if let Some(first) = children_iter.next() {
                let mut prev = *first;
                for next in children_iter.cloned() {
                    self.init_insert_groups_lr(prev, next, group_sets);
                    prev = next;
                }
            }

            self.init_insert_groups_children_recursive(&children, group_sets);
        }
    }

    fn init_insert_groups_lr(
        &self,
        left: LifetimeTreeRef,
        right: LifetimeTreeRef,
        group_sets: &mut Vec<FxHashSet<LifetimeTreeRef>>,
    ) {
        let left = self.resolve_lifetime_ref(left);
        let right = self.resolve_lifetime_ref(right);

        let left_group_index = group_sets.iter().position(|set| set.contains(&left));
        let right_group_index = group_sets.iter().position(|set| set.contains(&right));

        match (left_group_index, right_group_index) {
            (Some(left_group_index), Some(right_group_index)) => {
                // merge right into left
                let mut right_group_swap = FxHashSet::default();
                swap(&mut group_sets[right_group_index], &mut right_group_swap);

                group_sets[left_group_index].extend(right_group_swap);
                group_sets.remove(right_group_index);
            }
            (Some(group_index), None) | (None, Some(group_index)) => {
                let set = &mut group_sets[group_index];
                set.insert(left);
                set.insert(right);
            }
            (None, None) => {
                let mut new_set = FxHashSet::default();
                new_set.insert(left);
                new_set.insert(right);
                group_sets.push(new_set);
            }
        }
    }

    fn init_insert_groups_borrowed_ref(
        &self,
        origin: LifetimeTreeRef,
        group_sets: &mut Vec<FxHashSet<LifetimeTreeRef>>,
    ) {
        let origin = self.resolve_lifetime_ref(origin);
        let origin_tree = self.lifetime_tree_map.get(&origin).unwrap();

        for borrowed_ref in origin_tree.borrow_ref.iter() {
            self.init_insert_groups_lr(origin, *borrowed_ref, group_sets);
            self.init_insert_groups_borrowed_ref(*borrowed_ref, group_sets);
        }
    }

    pub fn collect_lifetimes(
        &self,
        lifetime_tree_ref: LifetimeTreeRef,
        ignore_insert_on_first_layer: bool,
        lifetimes: &mut Vec<Lifetime>,
        override_map: &FxHashMap<LifetimeTreeRef, LifetimeTree>,
        loop_suppressor: &mut LoopSuppressor,
    ) {
        let lifetime_tree_ref = self.resolve_lifetime_ref(lifetime_tree_ref);

        if !loop_suppressor.mark_as_explored(lifetime_tree_ref) {
            return;
        }

        let lifetime_tree = match override_map.get(&lifetime_tree_ref) {
            Some(lifetime_tree) => lifetime_tree,
            None => self.lifetime_tree_map.get(&lifetime_tree_ref).unwrap(),
        };

        if lifetime_tree.is_alloc_point {
            lifetimes.extend(lifetime_tree.lifetimes.iter());
        } else {
            lifetimes.extend(
                lifetime_tree
                    .lifetimes
                    .iter()
                    .filter(|lifetime| **lifetime == STATIC_LIFETIME),
            );
        }

        for borrowed_ref in lifetime_tree.borrow_ref.iter() {
            self.collect_lifetimes(
                *borrowed_ref,
                false,
                lifetimes,
                override_map,
                loop_suppressor,
            );
        }

        for alloc_point_ref in lifetime_tree.alloc_point_ref.iter() {
            self.collect_lifetimes(
                *alloc_point_ref,
                false,
                lifetimes,
                override_map,
                loop_suppressor,
            );
        }

        if !ignore_insert_on_first_layer {
            if let Some(inserted_refs) = self.insert_ref_map.get(&lifetime_tree_ref) {
                for inserted_ref in inserted_refs.iter() {
                    self.collect_lifetimes(
                        *inserted_ref,
                        false,
                        lifetimes,
                        override_map,
                        loop_suppressor,
                    );
                }
            }
        }
    }

    pub fn into_static(
        &self,
        lifetime_tree_ref: LifetimeTreeRef,
        override_map: &mut FxHashMap<LifetimeTreeRef, LifetimeTree>,
        loop_suppressor: &mut LoopSuppressor,
    ) {
        let lifetime_tree_ref = self.resolve_lifetime_ref(lifetime_tree_ref);

        if !loop_suppressor.mark_as_explored(lifetime_tree_ref) {
            return;
        }

        if override_map.contains_key(&lifetime_tree_ref) {
            let borrowed_ref = {
                let lifetime_tree = override_map.get_mut(&lifetime_tree_ref).unwrap();
                lifetime_tree.lifetimes = vec![STATIC_LIFETIME];
                lifetime_tree.is_alloc_point = false;
                lifetime_tree.alloc_point_ref.clear();

                lifetime_tree.borrow_ref.clone()
            };

            for borrowed_ref in borrowed_ref {
                self.into_static(borrowed_ref, override_map, loop_suppressor);
            }

            if let Some(inserted_refs) = self.insert_ref_map.get(&lifetime_tree_ref) {
                for inserted_ref in inserted_refs.iter() {
                    self.into_static(*inserted_ref, override_map, loop_suppressor);
                }
            }
        } else {
            let mut lifetime_tree = self
                .lifetime_tree_map
                .get(&lifetime_tree_ref)
                .unwrap()
                .clone();
            lifetime_tree.lifetimes = vec![STATIC_LIFETIME];
            lifetime_tree.is_alloc_point = false;
            lifetime_tree.alloc_point_ref.clear();

            let borrowed_ref = lifetime_tree.borrow_ref.clone();

            override_map.insert(lifetime_tree_ref, lifetime_tree);

            for borrowed_ref in borrowed_ref {
                self.into_static(borrowed_ref, override_map, loop_suppressor);
            }

            if let Some(inserted_refs) = self.insert_ref_map.get(&lifetime_tree_ref) {
                for inserted_ref in inserted_refs.iter() {
                    self.into_static(*inserted_ref, override_map, loop_suppressor);
                }
            }
        }
    }

    pub fn contains_argument_tree(
        &self,
        lifetime_tree_ref: LifetimeTreeRef,
        loop_suppressor: &mut LoopSuppressor,
    ) -> bool {
        let lifetime_tree_ref = self.resolve_lifetime_ref(lifetime_tree_ref);

        if !loop_suppressor.mark_as_explored(lifetime_tree_ref) {
            return false;
        }

        let lifetime_tree = self.lifetime_tree_map.get(&lifetime_tree_ref).unwrap();

        if lifetime_tree.is_argument_tree {
            return true;
        }

        for borrowed_ref in lifetime_tree.borrow_ref.iter() {
            if self.contains_argument_tree(*borrowed_ref, loop_suppressor) {
                return true;
            }
        }

        if let Some(inserted_refs) = self.insert_ref_map.get(&lifetime_tree_ref) {
            for inserted_ref in inserted_refs.iter() {
                if self.contains_argument_tree(*inserted_ref, loop_suppressor) {
                    return true;
                }
            }
        }

        false
    }

    pub fn contains_static_lifetime(
        &self,
        lifetime_tree_ref: LifetimeTreeRef,
        loop_suppressor: &mut LoopSuppressor,
    ) -> bool {
        let lifetime_tree_ref = self.resolve_lifetime_ref(lifetime_tree_ref);

        if !loop_suppressor.mark_as_explored(lifetime_tree_ref) {
            return false;
        }

        let lifetime_tree = self.lifetime_tree_map.get(&lifetime_tree_ref).unwrap();

        if lifetime_tree.lifetimes.contains(&STATIC_LIFETIME) {
            return true;
        }

        for borrowed_ref in lifetime_tree.borrow_ref.iter() {
            if self.contains_static_lifetime(*borrowed_ref, loop_suppressor) {
                return true;
            }
        }

        for alloc_ref in lifetime_tree.alloc_point_ref.iter() {
            if self.contains_static_lifetime(*alloc_ref, loop_suppressor) {
                return true;
            }
        }

        if let Some(inserted_refs) = self.insert_ref_map.get(&lifetime_tree_ref) {
            for inserted_ref in inserted_refs.iter() {
                if self.contains_static_lifetime(*inserted_ref, loop_suppressor) {
                    return true;
                }
            }
        }

        false
    }

    pub fn contains_function_return_value(
        &self,
        lifetime_tree_ref: LifetimeTreeRef,
        loop_suppressor: &mut LoopSuppressor,
    ) -> bool {
        let lifetime_tree_ref = self.resolve_lifetime_ref(lifetime_tree_ref);

        if !loop_suppressor.mark_as_explored(lifetime_tree_ref) {
            return false;
        }

        let lifetime_tree = self.lifetime_tree_map.get(&lifetime_tree_ref).unwrap();

        if lifetime_tree.contains_function_return_value {
            return true;
        }

        for borrowed_ref in lifetime_tree.borrow_ref.iter() {
            if self.contains_function_return_value(*borrowed_ref, loop_suppressor) {
                return true;
            }
        }

        if let Some(inserted_refs) = self.insert_ref_map.get(&lifetime_tree_ref) {
            for inserted_ref in inserted_refs.iter() {
                if self.contains_function_return_value(*inserted_ref, loop_suppressor) {
                    return true;
                }
            }
        }

        false
    }

    pub fn collect_argument_tree_ref(
        &self,
        lifetime_tree_ref: LifetimeTreeRef,
        argument_refs: &mut Vec<LifetimeTreeRef>,
        loop_suppressor: &mut LoopSuppressor,
    ) {
        let lifetime_tree_ref = self.resolve_lifetime_ref(lifetime_tree_ref);

        if !loop_suppressor.mark_as_explored(lifetime_tree_ref) {
            return;
        }

        let lifetime_tree = self.lifetime_tree_map.get(&lifetime_tree_ref).unwrap();

        if lifetime_tree.is_argument_tree {
            argument_refs.push(lifetime_tree_ref);
        }

        for borrowed_ref in lifetime_tree.borrow_ref.iter() {
            self.collect_argument_tree_ref(*borrowed_ref, argument_refs, loop_suppressor);
        }

        if let Some(inserted_refs) = self.insert_ref_map.get(&lifetime_tree_ref) {
            for inserted_ref in inserted_refs.iter() {
                self.collect_argument_tree_ref(*inserted_ref, argument_refs, loop_suppressor);
            }
        }
    }

    fn export_contains_static_tree(&self, from: LifetimeTreeRef) -> ContainsStaticTree {
        let lifetime_tree_ref = self.resolve_lifetime_ref(from);
        let lifetime_tree = self.lifetime_tree_map.get(&lifetime_tree_ref).unwrap();

        let mut contains_static_tree = ContainsStaticTree {
            contains_static: self
                .contains_static_lifetime(lifetime_tree_ref, &mut LoopSuppressor::new()),
            children: FxHashMap::default(),
        };

        for (child_name, child_ref) in lifetime_tree.children.iter() {
            let child_contains_static_tree = self.export_contains_static_tree(*child_ref);

            contains_static_tree
                .children
                .insert(child_name.clone(), child_contains_static_tree);
        }

        contains_static_tree
    }

    fn import_contains_static_tree(
        &mut self,
        lifetime_tree_ref: LifetimeTreeRef,
        from: &ContainsStaticTree,
        changed_flag: &mut bool,
        loop_suppressor: &mut LoopSuppressor,
    ) {
        let lifetime_tree_ref = self.resolve_lifetime_ref(lifetime_tree_ref);

        if !loop_suppressor.mark_as_explored(lifetime_tree_ref) {
            return;
        }

        let lifetime_tree = self.lifetime_tree_map.get_mut(&lifetime_tree_ref).unwrap();

        if from.contains_static {
            if !lifetime_tree.lifetimes.contains(&STATIC_LIFETIME) {
                *changed_flag = true;

                lifetime_tree.lifetimes.push(STATIC_LIFETIME);
            }
        }

        for borrowed_ref in lifetime_tree.borrow_ref.clone() {
            self.import_contains_static_tree(borrowed_ref, from, changed_flag, loop_suppressor);
        }

        if let Some(inserted_ref) = self.insert_ref_map.get(&lifetime_tree_ref).cloned() {
            for inserted_ref in inserted_ref.iter() {
                self.import_contains_static_tree(
                    *inserted_ref,
                    from,
                    changed_flag,
                    loop_suppressor,
                );
            }
        }

        for (child_name, contains_static_tree) in from.children.iter() {
            let lifetime_tree_ref = self.get_or_create_child(lifetime_tree_ref, child_name);

            self.import_contains_static_tree(
                lifetime_tree_ref,
                contains_static_tree,
                changed_flag,
                loop_suppressor,
            );
        }
    }

    fn export_contains_function_return_tree(
        &self,
        from: LifetimeTreeRef,
    ) -> ContainsFunctionReturnTree {
        let lifetime_tree_ref = self.resolve_lifetime_ref(from);
        let lifetime_tree = self.lifetime_tree_map.get(&lifetime_tree_ref).unwrap();

        let mut contains_function_return_tree = ContainsFunctionReturnTree {
            contains_function_return: self
                .contains_function_return_value(lifetime_tree_ref, &mut LoopSuppressor::new()),
            children: FxHashMap::default(),
        };

        for (child_name, child_ref) in lifetime_tree.children.iter() {
            let child_contains_function_return_tree =
                self.export_contains_function_return_tree(*child_ref);

            contains_function_return_tree
                .children
                .insert(child_name.clone(), child_contains_function_return_tree);
        }

        contains_function_return_tree
    }

    fn import_contains_function_return_tree(
        &mut self,
        lifetime_tree_ref: LifetimeTreeRef,
        from: &ContainsFunctionReturnTree,
        changed_flag: &mut bool,
        loop_suppressor: &mut LoopSuppressor,
    ) {
        let lifetime_tree_ref = self.resolve_lifetime_ref(lifetime_tree_ref);

        if !loop_suppressor.mark_as_explored(lifetime_tree_ref) {
            return;
        }

        let lifetime_tree = self.lifetime_tree_map.get_mut(&lifetime_tree_ref).unwrap();

        if from.contains_function_return {
            if !lifetime_tree.contains_function_return_value {
                *changed_flag = true;

                lifetime_tree.contains_function_return_value = true;
            }
        }

        for borrowed_ref in lifetime_tree.borrow_ref.clone() {
            self.import_contains_function_return_tree(
                borrowed_ref,
                from,
                changed_flag,
                loop_suppressor,
            );
        }

        if let Some(inserted_ref) = self.insert_ref_map.get(&lifetime_tree_ref).cloned() {
            for inserted_ref in inserted_ref.iter() {
                self.import_contains_function_return_tree(
                    *inserted_ref,
                    from,
                    changed_flag,
                    loop_suppressor,
                );
            }
        }

        for (child_name, contains_function_return_tree) in from.children.iter() {
            let lifetime_tree_ref = self.get_or_create_child(lifetime_tree_ref, child_name);

            self.import_contains_function_return_tree(
                lifetime_tree_ref,
                contains_function_return_tree,
                changed_flag,
                loop_suppressor,
            );
        }
    }

    fn collect_function_return_value_on_heap(
        &mut self,
        return_value: LifetimeTreeRef,
        from: &ContainsFunctionReturnTree,
        on_heap_trees: &mut FxHashSet<LifetimeTreeRef>,
        changed_flag: &mut bool,
        loop_suppressor: &mut LoopSuppressor,
    ) {
        let return_value = self.resolve_lifetime_ref(return_value);

        if !loop_suppressor.mark_as_explored(return_value) {
            return;
        }

        if from.contains_function_return {
            on_heap_trees.insert(return_value);
        }

        for (child_name, child_tree) in from.children.iter() {
            let child_ref = self.get_or_create_child(return_value, child_name);

            self.collect_function_return_value_on_heap(
                child_ref,
                child_tree,
                on_heap_trees,
                changed_flag,
                loop_suppressor,
            );
        }
    }
}

#[derive(Debug, Clone)]
pub struct ContainsStaticTree {
    contains_static: bool,
    children: FxHashMap<String, ContainsStaticTree>,
}

#[derive(Debug, Clone)]
pub struct ContainsFunctionReturnTree {
    contains_function_return: bool,
    children: FxHashMap<String, ContainsFunctionReturnTree>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LifetimeExpected {
    pub shorter: LifetimeTreeRef,
    pub should_ignore_insert_on_shorter: bool,
    pub longer: LifetimeTreeRef,
}

#[derive(Debug, Default, Clone)]
pub struct LifetimeTree {
    pub lifetimes: Vec<Lifetime>,
    pub children: FxHashMap<String, LifetimeTreeRef>,
    pub is_merged: bool,
    pub is_alloc_point: bool,
    pub contains_function_return_value: bool,
    pub alloc_point_ref: FxHashSet<LifetimeTreeRef>,
    pub borrow_ref: FxHashSet<LifetimeTreeRef>,
    pub is_argument_tree: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LifetimeTreeRef(usize);

#[derive(Debug, Clone)]
pub struct FunctionCallLifetime {
    pub arguments: Vec<LifetimeTreeRef>,
    pub return_value: LifetimeTreeRef,
    pub function: Arc<FunctionType>,
}

#[derive(Debug, Clone)]
pub struct LifetimeSource {
    instance: LifetimeInstance,
    arguments: Vec<LifetimeTreeRef>,
    return_value: LifetimeTreeRef,
}

impl LifetimeSource {
    pub fn finalize(mut self) -> Self {
        self.disable_function_return_value_stack_alloc(
            self.return_value,
            &mut false,
            &mut LoopSuppressor::new(),
        );
        self.instance
            .init_lifetime_expected_relationships(&mut false);
        self.instance.init_insert_map();
        self
    }

    pub fn snapshot(&self) -> Self {
        self.clone()
    }

    fn disable_function_return_value_stack_alloc(
        &mut self,
        lifetime_ref: LifetimeTreeRef,
        changed_flag: &mut bool,
        loop_suppressor: &mut LoopSuppressor,
    ) {
        let lifetime_ref = self.instance.resolve_lifetime_ref(lifetime_ref);

        if !loop_suppressor.mark_as_explored(lifetime_ref) {
            return;
        }

        if self
            .instance
            .contains_function_return_value(lifetime_ref, &mut LoopSuppressor::new())
        {
            let expected = LifetimeExpected {
                shorter: STATIC_LIFETIME_REF,
                should_ignore_insert_on_shorter: false,
                longer: lifetime_ref,
            };

            if !self.instance.lifetime_expected.contains(&expected) {
                self.instance.lifetime_expected.insert(expected);

                *changed_flag = true;
            }
        }

        let lifetime_tree = self.instance.get_lifetime_tree(lifetime_ref);

        let borrow_ref = lifetime_tree.borrow_ref.clone();
        let children = lifetime_tree.children.clone();

        for borrowed_ref in borrow_ref {
            self.disable_function_return_value_stack_alloc(
                borrowed_ref,
                changed_flag,
                loop_suppressor,
            );
        }

        for child_ref in children.into_values() {
            self.disable_function_return_value_stack_alloc(
                child_ref,
                changed_flag,
                loop_suppressor,
            );
        }

        if let Some(inserted_refs) = self.instance.insert_ref_map.get(&lifetime_ref).cloned() {
            for inserted_ref in inserted_refs.iter() {
                self.disable_function_return_value_stack_alloc(
                    *inserted_ref,
                    changed_flag,
                    loop_suppressor,
                );
            }
        }
    }

    fn collect_argument_lifetime_expected(&self) -> FxHashSet<LifetimeExpected> {
        let mut expected_set = FxHashSet::default();

        for expected in self.instance.lifetime_expected.iter() {
            if self
                .instance
                .contains_argument_tree(expected.longer, &mut LoopSuppressor::new())
            {
                let mut long_argument_refs = Vec::new();
                self.instance.collect_argument_tree_ref(
                    expected.longer,
                    &mut long_argument_refs,
                    &mut LoopSuppressor::new(),
                );

                if self
                    .instance
                    .contains_argument_tree(expected.shorter, &mut LoopSuppressor::new())
                {
                    let mut short_argument_refs = Vec::new();
                    self.instance.collect_argument_tree_ref(
                        expected.shorter,
                        &mut short_argument_refs,
                        &mut LoopSuppressor::new(),
                    );

                    for longer in long_argument_refs.iter().cloned() {
                        for shorter in short_argument_refs.iter().cloned() {
                            let expected = LifetimeExpected {
                                shorter,
                                should_ignore_insert_on_shorter: expected
                                    .should_ignore_insert_on_shorter,
                                longer,
                            };
                            expected_set.insert(expected);
                        }
                    }
                }

                if self
                    .instance
                    .contains_static_lifetime(expected.shorter, &mut LoopSuppressor::new())
                {
                    for longer in long_argument_refs {
                        let expected = LifetimeExpected {
                            shorter: STATIC_LIFETIME_REF,
                            should_ignore_insert_on_shorter: expected
                                .should_ignore_insert_on_shorter,
                            longer,
                        };
                        expected_set.insert(expected);
                    }
                }
            }
        }

        expected_set
    }

    fn create_replace_map(
        before_instance: &LifetimeInstance,
        before_tree_ref: LifetimeTreeRef,
        after_instance: &mut LifetimeInstance,
        after_tree_ref: LifetimeTreeRef,
        replace_map: &mut FxHashMap<LifetimeTreeRef, LifetimeTreeRef>,
    ) {
        let before_tree_ref = before_instance.resolve_lifetime_ref(before_tree_ref);
        let before_tree = before_instance
            .lifetime_tree_map
            .get(&before_tree_ref)
            .unwrap();

        let after_tree_ref = after_instance.resolve_lifetime_ref(after_tree_ref);

        replace_map.insert(before_tree_ref, after_tree_ref);

        for (before_child_name, before_child_ref) in before_tree.children.iter() {
            let after_child_ref =
                after_instance.get_or_create_child(after_tree_ref, before_child_name);

            Self::create_replace_map(
                before_instance,
                *before_child_ref,
                after_instance,
                after_child_ref,
                replace_map,
            );
        }
    }

    fn convert_argument_lifetime_expected(
        replace_map: &FxHashMap<LifetimeTreeRef, LifetimeTreeRef>,
        expected: FxHashSet<LifetimeExpected>,
    ) -> FxHashSet<LifetimeExpected> {
        expected
            .into_iter()
            .map(|expected| {
                let shorter_replaced = match replace_map.get(&expected.shorter) {
                    Some(after) => *after,
                    None => STATIC_LIFETIME_REF,
                };
                let longer_replaced = match replace_map.get(&expected.longer) {
                    Some(after) => *after,
                    None => STATIC_LIFETIME_REF,
                };

                LifetimeExpected {
                    shorter: shorter_replaced,
                    should_ignore_insert_on_shorter: false,
                    longer: longer_replaced,
                }
            })
            .collect()
    }

    fn export_arguments_static_tree(&self) -> Vec<ContainsStaticTree> {
        self.arguments
            .iter()
            .map(|argument_ref| self.instance.export_contains_static_tree(*argument_ref))
            .collect()
    }

    fn import_arguments_static_tree(
        &mut self,
        static_trees: &Vec<ContainsStaticTree>,
        changed_flag: &mut bool,
    ) {
        for i in 0..static_trees.len() {
            let static_tree = &static_trees[i];
            let argument_ref = match self.arguments.get(i).cloned() {
                Some(argument_ref) => argument_ref,
                None => break,
            };

            self.instance.import_contains_static_tree(
                argument_ref,
                static_tree,
                changed_flag,
                &mut LoopSuppressor::new(),
            );
        }
    }

    fn export_function_return_static_tree(&self) -> ContainsStaticTree {
        self.instance.export_contains_static_tree(self.return_value)
    }

    fn import_function_return_static_tree(
        &mut self,
        from: &ContainsStaticTree,
        changed_flag: &mut bool,
    ) {
        self.instance.import_contains_static_tree(
            self.return_value,
            from,
            changed_flag,
            &mut LoopSuppressor::new(),
        );
    }

    fn export_function_return_tree(&self) -> ContainsFunctionReturnTree {
        self.instance
            .export_contains_function_return_tree(self.return_value)
    }

    fn import_function_return_tree(
        &mut self,
        function_return_tree: &ContainsFunctionReturnTree,
        changed_flag: &mut bool,
    ) {
        self.instance.import_contains_function_return_tree(
            self.return_value,
            function_return_tree,
            changed_flag,
            &mut LoopSuppressor::new(),
        );
    }
}

pub struct LoopSuppressor {
    explored: FxHashSet<LifetimeTreeRef>,
}

impl LoopSuppressor {
    pub fn new() -> Self {
        Self {
            explored: FxHashSet::default(),
        }
    }

    pub fn mark_as_explored(&mut self, current: LifetimeTreeRef) -> bool {
        if self.explored.contains(&current) {
            return false;
        }
        self.explored.insert(current);
        true
    }
}

pub struct LifetimeEvaluator {
    lifetime_source_map:
        RwLock<FxHashMap<Arc<String>, FxHashMap<EntityID, RwLock<LifetimeSource>>>>,
    queue: Mutex<Vec<Arc<String>>>,
    max_queue_size: AtomicUsize,
    pub function_equals_info: GlobalFunctionEqualsInfo,
}

impl LifetimeEvaluator {
    pub fn new() -> Self {
        Self {
            lifetime_source_map: RwLock::new(FxHashMap::default()),
            queue: Mutex::new(Vec::new()),
            max_queue_size: AtomicUsize::new(0),
            function_equals_info: GlobalFunctionEqualsInfo::new(),
        }
    }

    pub fn add_sources(
        &self,
        module_name: Arc<String>,
        lifetime_sources: FxHashMap<EntityID, LifetimeSource>,
    ) {
        let lifetime_sources = lifetime_sources
            .into_iter()
            .map(|(entity_id, lifetime_source)| {
                (entity_id, RwLock::new(lifetime_source.finalize()))
            })
            .collect();

        let mut lifetime_source_map = self.lifetime_source_map.write().unwrap();
        lifetime_source_map.insert(module_name.clone(), lifetime_sources);

        let mut queue = self.queue.lock().unwrap();
        queue.push(module_name);

        self.max_queue_size.fetch_add(1, Ordering::Relaxed);
    }

    pub async fn eval(&self, context: &TranspileContext) {
        let num_threads = context.settings.num_threads;

        loop {
            let futures = (0..num_threads).map(|_| self.eval_lifetime_source());

            let mut return_queue = Vec::with_capacity(self.max_queue_size.load(Ordering::Relaxed));
            let mut is_any_changed = false;
            for (modules, is_changed) in join_all(futures).await {
                return_queue.extend(modules);
                is_any_changed |= is_changed;
            }

            let mut queue = self.queue.lock().unwrap();
            queue.extend(return_queue);

            if !is_any_changed {
                break;
            }
        }
    }

    async fn eval_lifetime_source(&self) -> (Vec<Arc<String>>, bool) {
        let mut is_changed = false;
        let mut completed_modules = Vec::new();

        let lifetime_source_map = self.lifetime_source_map.read().unwrap();

        loop {
            let module_name = {
                let mut queue = self.queue.lock().unwrap();

                match queue.pop() {
                    Some(module_name) => module_name,
                    _ => break,
                }
            };

            let mut change_list = Vec::new();
            let mut shared_change_list = Vec::new();

            {
                let lifetime_sources = lifetime_source_map.get(&module_name).unwrap().iter();

                for (entity_id, lifetime_source) in lifetime_sources {
                    let lifetime_source = lifetime_source.read().unwrap();

                    let mut apply_static_trees = Vec::new();
                    let mut apply_function_return_tree = None;

                    // apply static tree from origin function
                    if let Some(origin_define) = self
                        .function_equals_info
                        .resolve(&(module_name.clone(), *entity_id))
                    {
                        let origin_lifetime_source = lifetime_source_map
                            .get(&origin_define.0)
                            .unwrap()
                            .get(&origin_define.1)
                            .unwrap()
                            .read()
                            .unwrap();

                        let origin_argument_static_trees =
                            origin_lifetime_source.export_arguments_static_tree();
                        for i in 0..lifetime_source.arguments.len() {
                            let argument_ref = lifetime_source.arguments[i];
                            let origin_static_tree = match origin_argument_static_trees.get(i) {
                                Some(origin_static_tree) => origin_static_tree,
                                None => todo!(),
                            };

                            apply_static_trees.push((argument_ref, origin_static_tree.clone()));
                        }

                        apply_function_return_tree =
                            Some(origin_lifetime_source.export_function_return_tree());

                        apply_static_trees.push((
                            lifetime_source.return_value,
                            origin_lifetime_source.export_function_return_static_tree(),
                        ));
                    }

                    let mut expected_add = Vec::new();
                    let mut function_return_trees = Vec::new();

                    for function_call in lifetime_source.instance.function_calls.iter() {
                        let function_define = &function_call.function.define_info;
                        let define = (
                            function_define.module_name.clone(),
                            function_define.entity_id,
                        );
                        let define = match self.function_equals_info.resolve(&define) {
                            Some(resolved) => resolved,
                            None => define,
                        };

                        let function_lifetime_source = lifetime_source_map
                            .get(&define.0)
                            .unwrap()
                            .get(&define.1)
                            .unwrap()
                            .read()
                            .unwrap();

                        let expected =
                            function_lifetime_source.collect_argument_lifetime_expected();

                        expected_add.push((
                            expected,
                            function_call.arguments.clone(),
                            function_lifetime_source.snapshot(),
                        ));

                        let function_argument_static_trees =
                            function_lifetime_source.export_arguments_static_tree();
                        for i in 0..function_call.arguments.len() {
                            let argument_ref = function_call.arguments[i];
                            let static_tree = match function_argument_static_trees.get(i) {
                                Some(static_tree) => static_tree,
                                None => break,
                            };

                            apply_static_trees.push((argument_ref, static_tree.clone()));
                        }

                        apply_static_trees.push((
                            function_call.return_value,
                            function_lifetime_source.export_function_return_static_tree(),
                        ));

                        let argument_static_trees = function_call
                            .arguments
                            .iter()
                            .map(|argument_ref| {
                                lifetime_source
                                    .instance
                                    .export_contains_static_tree(*argument_ref)
                            })
                            .collect::<Vec<_>>();

                        let function_return_tree =
                            function_lifetime_source.export_function_return_tree();
                        function_return_trees
                            .push((function_return_tree, function_call.return_value));

                        shared_change_list.push((define, None, argument_static_trees, None, None));
                    }

                    let mut changed_map = FxHashMap::default();

                    for expected in lifetime_source.instance.lifetime_expected.iter() {
                        let mut is_satisfied = true;

                        let mut shorter_lifetims = Vec::new();
                        let mut longer_lifetims = Vec::new();

                        lifetime_source.instance.collect_lifetimes(
                            expected.shorter,
                            expected.should_ignore_insert_on_shorter,
                            &mut shorter_lifetims,
                            &mut changed_map,
                            &mut LoopSuppressor::new(),
                        );
                        lifetime_source.instance.collect_lifetimes(
                            expected.longer,
                            false,
                            &mut longer_lifetims,
                            &mut changed_map,
                            &mut LoopSuppressor::new(),
                        );

                        'check: for shorter in shorter_lifetims {
                            for longer in longer_lifetims.iter().cloned() {
                                if shorter > longer {
                                    is_satisfied = false;
                                    break 'check;
                                }
                            }
                        }

                        if !is_satisfied {
                            lifetime_source.instance.into_static(
                                expected.longer,
                                &mut changed_map,
                                &mut LoopSuppressor::new(),
                            );
                            is_changed = true;
                        }
                    }

                    change_list.push((
                        entity_id,
                        changed_map,
                        expected_add,
                        apply_static_trees,
                        apply_function_return_tree,
                        function_return_trees,
                    ));

                    if let Some(origin_function) = self
                        .function_equals_info
                        .resolve(&(module_name.clone(), *entity_id))
                    {
                        let expected = lifetime_source.collect_argument_lifetime_expected();

                        let argument_static_trees = lifetime_source.export_arguments_static_tree();
                        let return_value_static_tree =
                            lifetime_source.export_function_return_static_tree();

                        let function_return_tree = lifetime_source.export_function_return_tree();

                        shared_change_list.push((
                            (origin_function.0.clone(), origin_function.1),
                            Some((
                                expected,
                                lifetime_source.arguments.clone(),
                                lifetime_source.snapshot(),
                            )),
                            argument_static_trees,
                            Some(return_value_static_tree),
                            Some(function_return_tree),
                        ));
                    }
                }
            }

            // apply changes
            let lifetime_source_module_map = lifetime_source_map.get(&module_name).unwrap();
            for (
                entity_id,
                changed_map,
                expected_add,
                apply_static_trees,
                apply_function_return_tree,
                function_return_trees,
            ) in change_list
            {
                let lifetime_source = lifetime_source_module_map.get(entity_id).unwrap();
                let mut lifetime_source = lifetime_source.write().unwrap();

                lifetime_source
                    .instance
                    .lifetime_tree_map
                    .extend(changed_map);

                for (expected, arguments, origin_source_snapshot) in expected_add {
                    let mut replace_map = FxHashMap::default();
                    for i in 0..origin_source_snapshot.arguments.len() {
                        let define_argument = origin_source_snapshot.arguments[i];
                        let call_argument = match arguments.get(i) {
                            Some(argument) => *argument,
                            None => break,
                        };

                        LifetimeSource::create_replace_map(
                            &origin_source_snapshot.instance,
                            define_argument,
                            &mut lifetime_source.instance,
                            call_argument,
                            &mut replace_map,
                        );
                    }

                    let expected =
                        LifetimeSource::convert_argument_lifetime_expected(&replace_map, expected);

                    for expected in expected {
                        if !lifetime_source
                            .instance
                            .lifetime_expected
                            .contains(&expected)
                        {
                            lifetime_source.instance.lifetime_expected.insert(expected);
                            is_changed = true;
                        }
                    }
                }

                for (lifetime_ref, static_tree) in apply_static_trees {
                    lifetime_source.instance.import_contains_static_tree(
                        lifetime_ref,
                        &static_tree,
                        &mut is_changed,
                        &mut LoopSuppressor::new(),
                    );
                }

                if let Some(function_return_tree) = &apply_function_return_tree {
                    lifetime_source
                        .import_function_return_tree(function_return_tree, &mut is_changed);
                }

                for (function_return_tree, return_value) in function_return_trees {
                    let mut on_heap_trees = FxHashSet::default();

                    lifetime_source
                        .instance
                        .collect_function_return_value_on_heap(
                            return_value,
                            &function_return_tree,
                            &mut on_heap_trees,
                            &mut is_changed,
                            &mut LoopSuppressor::new(),
                        );

                    for on_heap_tree in on_heap_trees {
                        let expected = LifetimeExpected {
                            shorter: STATIC_LIFETIME_REF,
                            should_ignore_insert_on_shorter: false,
                            longer: on_heap_tree,
                        };

                        if !lifetime_source
                            .instance
                            .lifetime_expected
                            .contains(&expected)
                        {
                            lifetime_source.instance.lifetime_expected.insert(expected);
                        }
                    }
                }

                let return_lifetime_ref = lifetime_source.return_value;
                lifetime_source.disable_function_return_value_stack_alloc(
                    return_lifetime_ref,
                    &mut is_changed,
                    &mut LoopSuppressor::new(),
                );
                lifetime_source
                    .instance
                    .init_lifetime_expected_relationships(&mut is_changed);
            }

            for (
                origin_define,
                expected,
                argument_static_trees,
                function_return_static_tree,
                function_return_tree,
            ) in shared_change_list
            {
                let lifetime_source_module_map = lifetime_source_map.get(&origin_define.0).unwrap();
                let mut origin_lifetime_source = lifetime_source_module_map
                    .get(&origin_define.1)
                    .unwrap()
                    .write()
                    .unwrap();

                if let Some((expected, arguments, current_lifetime_snapshot)) = expected {
                    let mut replace_map = FxHashMap::default();
                    for i in 0..origin_lifetime_source.arguments.len() {
                        let origin_argument = origin_lifetime_source.arguments[i];
                        let argument = match arguments.get(i) {
                            Some(argument) => argument,
                            None => break,
                        };

                        LifetimeSource::create_replace_map(
                            &current_lifetime_snapshot.instance,
                            *argument,
                            &mut origin_lifetime_source.instance,
                            origin_argument,
                            &mut replace_map,
                        );
                    }

                    let expected =
                        LifetimeSource::convert_argument_lifetime_expected(&replace_map, expected);

                    for expected in expected {
                        if !origin_lifetime_source
                            .instance
                            .lifetime_expected
                            .contains(&expected)
                        {
                            origin_lifetime_source
                                .instance
                                .lifetime_expected
                                .insert(expected);
                            is_changed = true;
                        }
                    }
                }

                origin_lifetime_source
                    .import_arguments_static_tree(&argument_static_trees, &mut is_changed);

                if let Some(function_return_static_tree) = &function_return_static_tree {
                    origin_lifetime_source.import_function_return_static_tree(
                        function_return_static_tree,
                        &mut is_changed,
                    );
                }

                if let Some(function_return_tree) = &function_return_tree {
                    origin_lifetime_source
                        .import_function_return_tree(function_return_tree, &mut is_changed);
                }
            }

            completed_modules.push(module_name);
        }

        (completed_modules, is_changed)
    }

    pub fn get_lifetime_tree_info(
        &self,
        module_name: &Arc<String>,
        entity_id: EntityID,
    ) -> Option<(LifetimeTree, bool, bool)> {
        let lifetime_source_maps_lock = self.lifetime_source_map.read().unwrap();

        let lifetime_source_maps = lifetime_source_maps_lock.get(module_name).unwrap();

        for lifetime_source in lifetime_source_maps.values() {
            let lifetime_source = lifetime_source.read().unwrap();

            let lifetime_ref = match lifetime_source
                .instance
                .entity_lifetime_ref_map
                .get(&entity_id)
            {
                Some(lifetime_ref) => lifetime_source.instance.resolve_lifetime_ref(*lifetime_ref),
                None => continue,
            };

            return Some((
                lifetime_source
                    .instance
                    .lifetime_tree_map
                    .get(&lifetime_ref)
                    .unwrap()
                    .clone(),
                lifetime_source
                    .instance
                    .contains_static_lifetime(lifetime_ref, &mut LoopSuppressor::new()),
                lifetime_source
                    .instance
                    .contains_function_return_value(lifetime_ref, &mut LoopSuppressor::new()),
            ));
        }

        None
    }

    pub fn export_analyze_results(&self, module_name: &Arc<String>) -> LifetimeAnalyzeResults {
        let mut entity_result_map = FxHashMap::default();

        let lifetime_sources = self.lifetime_source_map.read().unwrap();

        for lifetime_source in lifetime_sources.get(module_name).unwrap().values() {
            let lifetime_source = lifetime_source.read().unwrap();

            for (entity_id, lifetime_tree_ref) in
                lifetime_source.instance.entity_lifetime_ref_map.iter()
            {
                let lifetime_tree_ref = lifetime_source
                    .instance
                    .resolve_lifetime_ref(*lifetime_tree_ref);
                let lifetime_tree = lifetime_source
                    .instance
                    .lifetime_tree_map
                    .get(&lifetime_tree_ref)
                    .unwrap();

                let result = LifetimeAnalyzeResult {
                    is_alloc_point: lifetime_tree.is_alloc_point,
                    contains_static: lifetime_source
                        .instance
                        .contains_static_lifetime(lifetime_tree_ref, &mut LoopSuppressor::new()),
                };

                entity_result_map.insert(*entity_id, result);
            }
        }

        LifetimeAnalyzeResults { entity_result_map }
    }
}

pub struct FunctionReturnTreeAllocPriorInfo {
    lifetime_tree_element_map: FxHashMap<EntityID, Option<String>>,
    disabled_element_path: FxHashSet<String>,
}

impl FunctionReturnTreeAllocPriorInfo {
    pub fn new() -> Self {
        Self {
            lifetime_tree_element_map: FxHashMap::default(),
            disabled_element_path: FxHashSet::default(),
        }
    }

    pub fn collect(
        &mut self,
        instance: &LifetimeInstance,
        tree_ref: LifetimeTreeRef,
        current_path: String,
        loop_suppressor: &mut LoopSuppressor,
    ) {
        if !loop_suppressor.mark_as_explored(tree_ref) {
            self.disabled_element_path.insert(current_path);
            return;
        }

        let tree_ref = instance.resolve_lifetime_ref(tree_ref);
        let tree = instance.lifetime_tree_map.get(&tree_ref).unwrap();

        if !tree.is_alloc_point {
            return;
        }

        if instance.contains_static_lifetime(tree_ref, &mut LoopSuppressor::new()) {
            return;
        }
    }
}

pub struct LifetimeAnalyzeResults {
    entity_result_map: FxHashMap<EntityID, LifetimeAnalyzeResult>,
}

impl LifetimeAnalyzeResults {
    pub fn get_result(&self, entity_id: EntityID) -> LifetimeAnalyzeResult {
        self.entity_result_map
            .get(&entity_id)
            .cloned()
            .unwrap_or(LifetimeAnalyzeResult {
                is_alloc_point: false,
                contains_static: true,
            })
    }
}

#[derive(Debug, Clone, Copy)]
pub struct LifetimeAnalyzeResult {
    pub is_alloc_point: bool,
    pub contains_static: bool,
}

pub struct GlobalFunctionEqualsInfo {
    info: RwLock<Vec<(Arc<FunctionType>, Arc<FunctionType>)>>,
    equals_map: RwLock<FxHashMap<(Arc<String>, EntityID), (Arc<String>, EntityID)>>,
}

impl GlobalFunctionEqualsInfo {
    pub fn new() -> Self {
        Self {
            info: RwLock::new(Vec::new()),
            equals_map: RwLock::new(FxHashMap::default()),
        }
    }

    pub fn add_info(&self, info: impl Iterator<Item = (Arc<FunctionType>, Arc<FunctionType>)>) {
        let mut info_lock = self.info.write().unwrap();
        info_lock.extend(info);
    }

    pub fn resolve(&self, define: &(Arc<String>, EntityID)) -> Option<(Arc<String>, EntityID)> {
        self.equals_map.read().unwrap().get(define).cloned()
    }

    pub fn build_function_equals_map(&self) {
        let mut group_sets: Vec<FxHashSet<_>> = Vec::new();

        for (left, right) in self.info.read().unwrap().iter() {
            let left = (
                left.define_info.module_name.clone(),
                left.define_info.entity_id,
            );
            let right = (
                right.define_info.module_name.clone(),
                right.define_info.entity_id,
            );

            let left_group_index = group_sets.iter().position(|set| set.contains(&left));
            let right_group_index = group_sets.iter().position(|set| set.contains(&right));

            match (left_group_index, right_group_index) {
                (Some(left_group_index), Some(right_group_index)) => {
                    // merge right into left
                    let mut right_group_swap = FxHashSet::default();
                    swap(&mut group_sets[right_group_index], &mut right_group_swap);

                    group_sets[left_group_index].extend(right_group_swap);
                    group_sets.remove(right_group_index);
                }
                (Some(group_index), None) | (None, Some(group_index)) => {
                    let set = &mut group_sets[group_index];
                    set.insert(left);
                    set.insert(right);
                }
                (None, None) => {
                    let mut new_set = FxHashSet::default();
                    new_set.insert(left);
                    new_set.insert(right);
                    group_sets.push(new_set);
                }
            }
        }

        let mut new_equals_map = FxHashMap::default();

        for group_set in group_sets {
            let mut group_iter = group_set.into_iter();

            let group_first = group_iter.next().unwrap();
            new_equals_map.insert(group_first.clone(), group_first.clone());

            for group_element in group_iter {
                new_equals_map.insert(group_element, group_first.clone());
            }
        }

        *self.equals_map.write().unwrap() = new_equals_map;
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
) -> FxHashMap<EntityID, LifetimeSource> {
    let mut lifetime_instance_map = FxHashMap::default();

    let mut lifetime_instance = LifetimeInstance::new();
    let mut lifetime_scope = LifetimeScope::new(&mut lifetime_instance, allocator);
    let mut stack_lifetime_scope = StackLifetimeScope::new(allocator);
    let return_value_tree_ref = lifetime_scope.instance.create_lifetime_tree();

    collect_lifetime_program(
        ast,
        false,
        None,
        return_value_tree_ref,
        &mut Vec::new(),
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

    let lifetime_source = LifetimeSource {
        instance: lifetime_instance,
        arguments: vec![],
        return_value: return_value_tree_ref,
    };

    lifetime_instance_map.insert(EntityID::from(ast), lifetime_source);

    lifetime_instance_map
}
