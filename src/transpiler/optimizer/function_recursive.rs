pub mod function_call_collector;

use std::sync::{Arc, Mutex, RwLock};

use fxhash::FxHashMap;

use crate::transpiler::component::EntityID;

pub struct FunctionRecursiveInfo {
    info: RwLock<FxHashMap<Arc<String>, ModuleFunctionCallInfo>>,
}

pub struct ModuleFunctionCallInfo {
    map: FxHashMap<EntityID, Arc<Mutex<Vec<(Arc<String>, EntityID)>>>>,
}
