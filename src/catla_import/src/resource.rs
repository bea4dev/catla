use std::sync::{Arc, RwLock};

use catla_util::source_code::SourceCode;
use hashbrown::HashMap;

#[derive(Debug, Clone)]
pub struct PackageResourceSet {
    map: Arc<RwLock<HashMap<String, PackageResource>>>,
}

impl PackageResourceSet {
    pub fn new() -> Self {
        Self {
            map: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    pub fn register(&self, name: String, resource: PackageResource) {
        let mut map = self.map.write().unwrap();
        map.insert(name, resource);
    }

    pub fn get(&self, name: &str) -> Option<PackageResource> {
        let map = self.map.read().unwrap();
        map.get(name).cloned()
    }

    pub fn get_all(&self) -> HashMap<String, PackageResource> {
        let map = self.map.read().unwrap();
        map.clone()
    }
}

#[derive(Debug, Clone)]
pub enum PackageResource {
    Package,
    Module { source_code: SourceCode },
}
