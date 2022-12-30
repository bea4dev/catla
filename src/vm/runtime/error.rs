use std::fmt::{Display, Formatter};
use crate::HeapObject;

#[derive(Clone)]
pub enum RuntimeError {
    NullPointerException(String),
    UserCustomException(*mut HeapObject)
}

impl Display for RuntimeError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            RuntimeError::NullPointerException(string) => write!(f, "NullPointerException {}", string),
            RuntimeError::UserCustomException(object) => write!(f, "CustomException {}", *object as usize)
        }
    }
}