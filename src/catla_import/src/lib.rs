pub mod resource;

pub enum ImportElement {
    ModuleAlias { path: Vec<String> },
    ModuleElement { path: Vec<String>, element: String },
    Unknown,
}
