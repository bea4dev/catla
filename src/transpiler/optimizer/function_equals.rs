use std::{
    mem::swap,
    sync::{Arc, RwLock},
};

use fxhash::{FxHashMap, FxHashSet};

use crate::transpiler::{component::EntityID, semantics::types::type_info::FunctionType};

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
