use std::{collections::HashMap, sync::Arc};

use super::{ResourceType, SourceCode};


pub trait SourceCodeProvider {

    fn get_source_code(&self, module_name: &str) -> Option<Arc<SourceCode>>;

    fn exists_source_code_or_package(&self, module_name: &str) -> bool;

}

pub struct TestSourceCodeProvider {
    test_code_map: HashMap<String, Arc<SourceCode>>
}

impl TestSourceCodeProvider {
    
    pub fn new() -> TestSourceCodeProvider {
        Self {
            test_code_map: HashMap::new()
        }
    }

    pub fn insert(&mut self, module_name: String, source_code: String) {
        // crate empty package source
        if module_name.contains("::") {
            let split = module_name.split("::").collect::<Vec<_>>();
            let mut current_package_name = String::new();
            for i in 0..(split.len() - 1) {
                if i != 0 {
                    current_package_name += "::";
                }
                current_package_name += split[0];
                
                self.test_code_map.entry(current_package_name.clone()).or_insert_with(|| {
                    Arc::new(SourceCode {
                        code: "".to_string(),
                        module_name: current_package_name.clone(),
                        resource_type: ResourceType::Package
                    })
                });
            }
            println!("{}", &current_package_name);
        }

        let source_code = Arc::new(
            SourceCode {
                code: source_code,
                module_name: module_name.clone(),
                resource_type: ResourceType::SourceCode
            }
        );        
        self.test_code_map.insert(module_name, source_code);
    }

}

impl SourceCodeProvider for TestSourceCodeProvider {

    fn get_source_code(&self, module_name: &str) -> Option<Arc<SourceCode>> {
        self.test_code_map.get(module_name).cloned()
    }

    fn exists_source_code_or_package(&self, module_name: &str) -> bool {
        self.test_code_map.contains_key(module_name)
    }

}