use std::{
    fmt::Display,
    hash::Hash,
    ops::Range,
    path::{Path, PathBuf},
    sync::Arc,
};

#[derive(Debug, Clone, Eq)]
pub struct ModulePath {
    pub path: Arc<Vec<String>>,
    pub path_name: Arc<String>,
    pub file_path: Arc<PathBuf>,
}

impl ModulePath {
    pub fn new<'a>(iter: impl Iterator<Item = &'a str>, file_path: &Path) -> Self {
        let path = iter.map(|str| str.to_string()).collect::<Vec<_>>();
        let path_name = path.join("::");

        Self {
            path: Arc::new(path),
            path_name: Arc::new(path_name),
            file_path: Arc::new(file_path.into()),
        }
    }
}

pub trait ToModuled: Sized {
    fn moduled(self, module_path: ModulePath, span: Range<usize>) -> Moduled<Self>;
}

impl<T> ToModuled for T {
    fn moduled(self, module_path: ModulePath, span: Range<usize>) -> Moduled<Self> {
        Moduled::new(self, module_path, span)
    }
}

impl Display for ModulePath {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.file_path.to_string_lossy())
    }
}

impl PartialEq for ModulePath {
    fn eq(&self, other: &Self) -> bool {
        &self.path == &other.path
    }
}

impl Hash for ModulePath {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.path.hash(state);
    }
}

#[derive(Debug, Clone)]
pub struct Moduled<T> {
    pub module_path: ModulePath,
    pub span: Range<usize>,
    pub value: T,
}

impl<T> Moduled<T> {
    pub fn new(value: T, module_path: ModulePath, span: Range<usize>) -> Self {
        Self {
            module_path,
            span,
            value,
        }
    }

    pub fn map<V, F: FnOnce(T) -> V>(self, f: F) -> Moduled<V> {
        Moduled::new(f(self.value), self.module_path, self.span)
    }
}
