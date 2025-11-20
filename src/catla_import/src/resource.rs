use std::{
    fs::read_dir,
    path::Path,
    sync::{Arc, RwLock},
};

use catla_util::{module_path::ModulePath, source_code::SourceCode};
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

    pub fn search_source_code(
        &mut self,
        current_module_name: &str,
        path: &Path,
    ) -> Result<(), String> {
        if !path.is_dir() {
            return Ok(());
        }

        for entry in read_dir(path).map_err(|_| format!("Failed to read '{}'", path.display()))? {
            let entry = entry.map_err(|error| error.to_string())?;
            let path = entry.path();

            let Some(stem) = path.file_stem().unwrap().to_str() else {
                continue;
            };

            if stem.contains("::") || stem.contains(".") {
                continue;
            }

            let current_module_name = format!("{}::{}", current_module_name, stem);

            if path.is_dir() {
                self.search_source_code(&current_module_name, &path)?;

                if let Some(PackageResource::Module { source_code: _ }) =
                    self.get(&current_module_name)
                {
                    continue;
                }

                self.register(current_module_name, PackageResource::Package);
            } else {
                let extension = path.extension().unwrap().to_str();
                if extension.is_none() || extension.unwrap() != "catla" {
                    continue;
                }

                let module_path = ModulePath::new(current_module_name.split("::"), &path);

                if let Ok(code) = std::fs::read_to_string(&path) {
                    let source_code = SourceCode::new(module_path, code);

                    self.register(current_module_name, PackageResource::Module { source_code });
                }
            }
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub enum PackageResource {
    Package,
    Module { source_code: SourceCode },
}
