use std::sync::Arc;

use catla_import::resource::PackageResourceSet;
use catla_util::future::SharedManualFuture;
use hashbrown::HashMap;

#[derive(Debug)]
pub struct ModuleResourceSet<T: Sync + Send> {
    resources: HashMap<String, SharedManualFuture<T>>,
}

impl<T: Sync + Send> ModuleResourceSet<T> {
    pub fn new(package_resource_set: &PackageResourceSet) -> Self {
        let mut init_map = HashMap::new();

        for package in package_resource_set.get_all().into_keys() {
            init_map.insert(package, SharedManualFuture::new());
        }

        Self {
            resources: init_map,
        }
    }

    pub async fn get(&self, modules: &Vec<String>) -> HashMap<String, Arc<T>> {
        let mut map = HashMap::new();

        for module in modules.iter() {
            let value = self.resources.get(module).unwrap().get().await;
            map.insert(module.clone(), value);
        }

        map
    }

    pub async fn set(&self, module_name: &String, value: Arc<T>) {
        self.resources
            .get(module_name)
            .unwrap()
            .complete(value)
            .await;
    }
}
