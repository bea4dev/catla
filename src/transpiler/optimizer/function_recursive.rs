pub mod function_call_collector;

use std::sync::{Arc, Mutex, RwLock};

use fxhash::FxHashMap;

use crate::transpiler::component::EntityID;

pub struct FunctionRecursiveInfo {
    info: RwLock<FxHashMap<Arc<String>, ModuleFunctionCallInfo>>,
}

impl FunctionRecursiveInfo {
    pub fn new() -> Self {
        Self {
            info: RwLock::new(FxHashMap::default()),
        }
    }

    pub fn register(&self, module_name: Arc<String>, function_call_info: ModuleFunctionCallInfo) {
        self.info
            .write()
            .unwrap()
            .insert(module_name, function_call_info);
    }
}

pub struct ModuleFunctionCallInfo {
    map: FxHashMap<EntityID, Arc<Mutex<Vec<(Arc<String>, EntityID)>>>>,
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
        self.map
            .insert(function_entity_id, Arc::new(Mutex::new(function_calls)));
    }
}
