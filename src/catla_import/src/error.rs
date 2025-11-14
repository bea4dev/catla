use std::ops::Range;

use catla_util::module_path::ModulePath;

#[derive(Debug)]
pub struct ImportError {
    pub kind: ImportErrorKind,
    pub span: Range<usize>,
    pub module_path: ModulePath,
}

#[derive(Debug)]
pub enum ImportErrorKind {
    NoElementFound { element_name: String },
    CannotImportModuleWithWildCard,
    ModuleNotFound { module_name: String },
}
