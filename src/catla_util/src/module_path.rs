use std::{
    fmt::Display,
    hash::Hash,
    path::{Path, PathBuf},
    sync::Arc,
};

#[derive(Debug, Clone, Eq)]
pub struct ModulePath {
    path: Arc<Vec<String>>,
    file_path: Arc<PathBuf>,
}

impl ModulePath {
    pub fn new<'a>(iter: impl Iterator<Item = &'a str>, file_path: &Path) -> Self {
        Self {
            path: Arc::new(iter.map(|str| str.to_string()).collect()),
            file_path: Arc::new(file_path.into()),
        }
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
