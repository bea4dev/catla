use std::ops::Range;

use catla_util::module_path::{ModulePath, Moduled};

#[derive(Debug)]
pub struct TypeError {
    pub kind: TypeErrorKind,
    pub span: Range<usize>,
    pub module_path: ModulePath,
}

#[derive(Debug)]
pub enum TypeErrorKind {
    NotFoundModuleElementType,
    MissingModuleElementType,
    UnknownModuleElementType,
    UnknownThis,
    InvalidGenericsCount,
    MissingStaticVariableType,
    TypeMismatch {
        left: Moduled<String>,
        right: Moduled<String>,
    },
}
