pub mod resource;
pub mod import;
pub mod error;

pub enum ImportElement {
    ModuleAlias { path: Vec<String> },
    ModuleElement { path: Vec<String>, element: String },
    Unknown,
}
