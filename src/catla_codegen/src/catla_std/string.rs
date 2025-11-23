#[derive(Debug, Clone)]
pub struct String {}

pub enum StringInner {
    Static { str: &'static str },
    Heap {},
}
