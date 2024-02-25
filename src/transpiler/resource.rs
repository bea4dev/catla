use std::collections::HashMap;


pub static FILE_EXTENSION: &str = ".catla";

pub trait SourceCodeProvider {

    fn get_source_code(&self, module_name: &str) -> Option<String>;

    fn exsists_source_code(&self, module_name: &str) -> bool;

}

pub struct TestSourceCodeProvider {
    test_code_map: HashMap<String, String>
}

impl TestSourceCodeProvider {
    
    pub fn new() -> TestSourceCodeProvider {
        Self {
            test_code_map: HashMap::new()
        }
    }

    pub fn insert(&mut self, module_name: String, source_code: String) {
        self.test_code_map.insert(module_name, source_code);
    }

}

impl SourceCodeProvider for TestSourceCodeProvider {

    fn get_source_code(&self, module_name: &str) -> Option<String> {
        self.test_code_map.get(module_name).cloned()
    }

    fn exsists_source_code(&self, module_name: &str) -> bool {
        self.test_code_map.contains_key(module_name)
    }

}