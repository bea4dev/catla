use std::{fmt::{Debug, Display}, sync::Arc};

pub mod lifetime_collector;


#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LifetimeStartPosition {
    nested_position: Option<Arc<Vec<u32>>>,
    index: u32
}

impl LifetimeStartPosition {
    
    pub const fn create_origin() -> Self {
        Self {
            nested_position: None,
            index: 1
        }
    }

    pub const fn static_position() -> Self {
        Self {
            nested_position: None,
            index: 0
        }
    }

    pub const fn is_static(&self) -> bool {
        self.nested_position.is_none() && self.index == 0
    }

    fn get_nested_position(&self) -> &Vec<u32> {
        static EMPTY_VEC: Vec<u32> = Vec::new();

        self.nested_position.as_ref()
            .map(|vec| { vec.as_ref() })
            .unwrap_or(&EMPTY_VEC)
    }

    pub fn create_nested(&self) -> Self {
        let nested_position = self.get_nested_position();
        let mut new_nested_position = Vec::with_capacity(nested_position.len() + 1);
        new_nested_position.extend(nested_position.iter());
        new_nested_position.push(self.index);

        Self {
            nested_position: Some(Arc::new(new_nested_position)),
            index: 1
        }
    }

    pub fn next(&self) -> Self {
        Self {
            nested_position: self.nested_position.clone(),
            index: self.index + 1
        }
    }

    pub fn into_next(&mut self) {
        self.index += 1;
    }

}


impl Debug for LifetimeStartPosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_static() {
            return write!(f, "Lifetime 'static")
        }

        let mut nested = self.nested_position
            .as_ref()
            .map(|nested_position| {
                nested_position.iter()
                    .map(|index| { index.to_string() })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default();
        nested.push(self.index.to_string());

        write!(f, "Lifetime [ {} ]", nested.join("."))
    }
}

impl Display for LifetimeStartPosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as Debug>::fmt(&self, f)
    }
}
