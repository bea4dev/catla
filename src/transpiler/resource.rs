use std::{collections::HashMap, sync::Arc};

use super::SourceCode;


pub trait SourceCodeProvider {

    fn get_source_code(&self, module_name: &str) -> Option<Arc<SourceCode>>;

    fn exists_source_code(&self, module_name: &str) -> bool;

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
        let source_code = Arc::new(
            SourceCode { code: source_code, module_name: module_name.clone() }
        );
        self.test_code_map.insert(module_name, source_code);
    }

}

impl SourceCodeProvider for TestSourceCodeProvider {

    fn get_source_code(&self, module_name: &str) -> Option<Arc<SourceCode>> {
        self.test_code_map.get(module_name).cloned()
    }

    fn exists_source_code(&self, module_name: &str) -> bool {
        self.test_code_map.contains_key(module_name)
    }

}