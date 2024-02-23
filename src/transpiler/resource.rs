use std::collections::HashMap;


pub trait SourceCodeProvider {

    fn get_source_code(&self, module_name: &str) -> Option<String>;

}

pub struct TestSourceCodeProvider {
    test_code_map: HashMap<String, String>
}

impl TestSourceCodeProvider {
    
    pub fn new() -> TestSourceCodeProvider {
        return Self {
            test_code_map: HashMap::new()
        };
    }

    pub fn insert(&mut self, module_name: String, source_code: String) {
        self.test_code_map.insert(module_name, source_code);
    }

}

impl SourceCodeProvider for TestSourceCodeProvider {

    fn get_source_code(&self, module_name: &str) -> Option<String> {
        return self.test_code_map.get(module_name).cloned();
    }

}