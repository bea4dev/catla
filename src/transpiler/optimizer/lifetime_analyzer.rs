use std::{fmt::{Debug, Display}, sync::Arc};

pub mod lifetime_collector;


#[derive(Clone, PartialEq, Eq, Hash)]
pub struct LifetimePosition {
    nested_position: Arc<Vec<u32>>,
    index: u32
}

impl LifetimePosition {

    pub fn create_origin() -> Self {
        Self {
            nested_position: Arc::new(Vec::new()),
            index: 1
        }
    }

    pub fn create_nested(&self) -> Self {
        let mut new_nested_position = Vec::with_capacity(self.nested_position.len() + 1);
        new_nested_position.extend(self.nested_position.iter());
        new_nested_position.push(self.index);

        Self {
            nested_position: Arc::new(new_nested_position),
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

impl PartialOrd for LifetimePosition {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        todo!()
    }
}


impl Debug for LifetimePosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let nested = self.nested_position.iter()
            .map(|index| { index.to_string() })
            .collect::<Vec<_>>()
            .join(".");

        write!(f, "{}.{}", nested, self.index)
    }
}

impl Display for LifetimePosition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        <Self as Debug>::fmt(&self, f)
    }
}
