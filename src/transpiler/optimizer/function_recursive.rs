pub mod function_call_collector;

use std::sync::{Arc, RwLock};

use concurrent_queue::ConcurrentQueue;
use futures::future::join_all;
use fxhash::{FxHashMap, FxHashSet};

use crate::transpiler::{component::EntityID, context::TranspileContext};

pub struct FunctionRecursiveInfo {
    info: RwLock<FxHashMap<Arc<String>, ModuleFunctionCallInfo>>,
    recursive_functions: RwLock<FxHashSet<EntityID>>,
}

impl FunctionRecursiveInfo {
    pub fn new() -> Self {
        Self {
            info: RwLock::new(FxHashMap::default()),
            recursive_functions: RwLock::new(FxHashSet::default()),
        }
    }

    pub fn register(&self, module_name: Arc<String>, function_call_info: ModuleFunctionCallInfo) {
        self.info
            .write()
            .unwrap()
            .insert(module_name, function_call_info);
    }

    pub fn is_recursive(
        &self,
        module_name: &Arc<String>,
        function_entity_id: &EntityID,
        context: &TranspileContext,
    ) -> bool {
        let function_entity_id = match context
            .function_equals_info
            .resolve(&(module_name.clone(), *function_entity_id))
        {
            Some(resolved) => resolved.1,
            None => *function_entity_id,
        };

        self.recursive_functions
            .read()
            .unwrap()
            .contains(&function_entity_id)
    }

    pub async fn eval(&self, context: &TranspileContext) {
        let info = self.info.read().unwrap();
        let num_threads = context.settings.num_threads;

        {
            // merge
            let queue = ConcurrentQueue::bounded(info.len());
            for entry in info.iter() {
                queue.push(entry).ok().unwrap();
            }
            queue.close();

            let futures = (0..num_threads)
                .map(|_| self.merge_function_call_info(&queue, context))
                .collect::<Vec<_>>();
            join_all(futures).await;
        }

        {
            // check recursive
            let queue = ConcurrentQueue::bounded(info.len());
            for entry in info.iter() {
                queue.push(entry).ok().unwrap();
            }
            queue.close();

            let futures = (0..num_threads)
                .map(|_| self.check_recursive(&queue, context))
                .collect::<Vec<_>>();
            join_all(futures).await;
        }
    }

    async fn merge_function_call_info(
        &self,
        queue: &ConcurrentQueue<(&Arc<String>, &ModuleFunctionCallInfo)>,
        context: &TranspileContext,
    ) {
        let info = self.info.read().unwrap();

        loop {
            let (module_name, module_function_call_info) = match queue.pop() {
                Ok(element) => element,
                Err(_) => break,
            };

            for (current_function_entity_id, current_called_functions) in
                module_function_call_info.map.iter()
            {
                let current_function_key = (module_name.clone(), *current_function_entity_id);

                if let Some((resolved_module_name, resolved_entity_id)) =
                    context.function_equals_info.resolve(&current_function_key)
                {
                    if *current_function_entity_id == resolved_entity_id {
                        continue;
                    }

                    let resolved_module_info = info.get(&resolved_module_name).unwrap();
                    let resolved_function_call_info =
                        resolved_module_info.map.get(&resolved_entity_id).unwrap();

                    let mut resolved_function_call_info =
                        resolved_function_call_info.write().unwrap();

                    let current_function_info = current_called_functions.read().unwrap();

                    resolved_function_call_info.extend(current_function_info.iter().cloned());
                }
            }
        }
    }

    async fn check_recursive(
        &self,
        queue: &ConcurrentQueue<(&Arc<String>, &ModuleFunctionCallInfo)>,
        context: &TranspileContext,
    ) {
        let info = self.info.read().unwrap();

        loop {
            let (module_name, module_function_call_info) = match queue.pop() {
                Ok(element) => element,
                Err(_) => break,
            };

            for function_entity_id in module_function_call_info.map.keys() {
                if self.is_recursive(module_name, function_entity_id, context) {
                    continue;
                }

                fn is_recursive(
                    module_name: &Arc<String>,
                    function_entity_id: &EntityID,
                    info: &FxHashMap<Arc<String>, ModuleFunctionCallInfo>,
                    context: &TranspileContext,
                    explored: &mut FxHashSet<EntityID>,
                    stack: &mut Vec<EntityID>,
                ) -> bool {
                    let (module_name, function_entity_id) = match context
                        .function_equals_info
                        .resolve(&(module_name.clone(), *function_entity_id))
                    {
                        Some(resolved) => resolved,
                        None => (module_name.clone(), *function_entity_id),
                    };

                    if explored.contains(&function_entity_id) {
                        if *stack.first().unwrap() == function_entity_id {
                            return true;
                        }
                        return false;
                    }
                    stack.push(function_entity_id);
                    explored.insert(function_entity_id);

                    let called_functions = info
                        .get(&module_name)
                        .unwrap()
                        .map
                        .get(&function_entity_id)
                        .unwrap()
                        .read()
                        .unwrap();

                    for (called_function_module_name, called_function_entity_id) in
                        called_functions.iter()
                    {
                        let is_recursive = is_recursive(
                            &called_function_module_name,
                            &called_function_entity_id,
                            info,
                            context,
                            explored,
                            stack,
                        );

                        if is_recursive {
                            return true;
                        }
                    }

                    stack.pop().unwrap();
                    false
                }

                let mut stack = Vec::new();
                let is_recursive = is_recursive(
                    module_name,
                    function_entity_id,
                    &info,
                    context,
                    &mut FxHashSet::default(),
                    &mut stack,
                );

                if is_recursive {
                    self.recursive_functions.write().unwrap().extend(stack);
                }
            }
        }
    }
}

pub struct ModuleFunctionCallInfo {
    map: FxHashMap<EntityID, RwLock<FxHashSet<(Arc<String>, EntityID)>>>,
}

impl ModuleFunctionCallInfo {
    pub fn new() -> Self {
        Self {
            map: FxHashMap::default(),
        }
    }

    pub fn register(
        &mut self,
        function_entity_id: EntityID,
        function_calls: Vec<(Arc<String>, EntityID)>,
    ) {
        self.map.insert(
            function_entity_id,
            RwLock::new(FxHashSet::from_iter(function_calls)),
        );
    }
}
