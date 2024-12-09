use std::{
    fs::{read_dir, read_to_string},
    path::Path,
    sync::{Arc, Mutex},
};

use fxhash::{FxHashMap, FxHashSet};

use super::SourceCode;

pub trait SourceCodeProvider {
    fn get_source_code(
        &self,
        module_name: &str,
    ) -> Option<Arc<Result<Arc<SourceCode>, std::io::Error>>>;

    fn get_source_code_path(&self, module_name: &str) -> String;

    fn exists_source_code_or_package(&self, module_name: &str) -> bool;

    fn get_entries(&self) -> FxHashSet<String>;
}

pub struct DefaultSourceCodeProvider {
    entries: FxHashSet<String>,
    entry_rename_map: FxHashMap<String, String>,
    path_map: FxHashMap<String, Box<Path>>,
    code_map: FxHashMap<String, Mutex<Option<Arc<Result<Arc<SourceCode>, std::io::Error>>>>>,
}

impl DefaultSourceCodeProvider {
    pub fn new() -> Self {
        Self {
            entries: Default::default(),
            entry_rename_map: Default::default(),
            path_map: Default::default(),
            code_map: Default::default(),
        }
    }

    pub fn add_entry(&mut self, entry_name: String, path: &Path) -> Result<(), String> {
        if self.entries.contains(&entry_name) {
            return Err(format!("'{}' already exists", &entry_name));
        }

        let stem = path
            .file_stem()
            .ok_or_else(|| format!("no valid stem '{}'", path.display()))?
            .to_str()
            .ok_or_else(|| format!("Invalid file name '{}'", path.display()))?;

        self.entry_rename_map
            .insert(entry_name.clone(), stem.to_string());

        self.search_source_code(&entry_name, path)?;

        self.entries.insert(entry_name);

        Ok(())
    }

    fn search_source_code(&mut self, current_module_name: &str, path: &Path) -> Result<(), String> {
        if !path.is_dir() {
            return Ok(());
        }

        for entry in read_dir(path).map_err(|_| format!("Failed to read '{}'", path.display()))? {
            let entry = entry.map_err(|error| error.to_string())?;
            let path = entry.path();

            let stem = path.file_stem().unwrap().to_str();
            if stem.is_none() {
                continue;
            }

            let stem = stem.unwrap();

            if stem.contains("::") {
                continue;
            }

            let current_module_name = format!("{}::{}", current_module_name, stem);

            if path.is_dir() {
                self.search_source_code(&current_module_name, &path)?;
            } else {
                let extension = path.extension().unwrap().to_str();
                if extension.is_none() || extension.unwrap() != "catla" {
                    continue;
                }

                self.path_map
                    .insert(current_module_name.clone(), path.into_boxed_path());
                self.code_map
                    .insert(current_module_name, Default::default());
            }
        }

        Ok(())
    }
}

impl SourceCodeProvider for DefaultSourceCodeProvider {
    fn get_source_code(
        &self,
        module_name: &str,
    ) -> Option<Arc<Result<Arc<SourceCode>, std::io::Error>>> {
        let path = self.path_map.get(module_name)?.clone();

        let mut source_code_lock = self.code_map.get(module_name).unwrap().lock().unwrap();

        if let Some(source_code) = source_code_lock.as_ref() {
            return Some(source_code.clone());
        }

        let source_code = read_to_string(&path).map(|code| {
            Arc::new(SourceCode {
                code,
                module_name: module_name.to_string(),
            })
        });

        let source_code = Arc::new(source_code);

        *source_code_lock = Some(source_code.clone());

        Some(source_code)
    }

    fn get_source_code_path(&self, module_name: &str) -> String {
        let (entry_name, latter) = match module_name.split_once("::") {
            Some(result) => result,
            None => return self.entry_rename_map.get(module_name).unwrap().clone(),
        };

        let original_entry_name = self.entry_rename_map.get(entry_name).unwrap();
        format!(
            "{}/{}.catla",
            original_entry_name,
            latter.replace("::", "/")
        )
    }

    fn exists_source_code_or_package(&self, module_name: &str) -> bool {
        self.path_map.contains_key(module_name)
    }

    fn get_entries(&self) -> FxHashSet<String> {
        self.entries.clone()
    }
}

