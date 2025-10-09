use std::sync::Arc;

use crate::module_path::ModulePath;

#[derive(Debug, Clone)]
pub struct SourceCode {
    pub module_path: ModulePath,
    pub code: Arc<String>,
}

impl SourceCode {
    pub fn new(module_path: ModulePath, code: impl Into<Arc<String>>) -> Self {
        Self {
            module_path,
            code: code.into(),
        }
    }
}
