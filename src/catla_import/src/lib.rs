pub mod resource;
pub mod element;
pub mod error;
pub mod import;

pub enum ImportElement {
    ModuleAlias { path: Vec<String> },
    ModuleElement { path: Vec<String>, element: String },
    Unknown,
}
