/// (Hindley-Milner based) Union-Find like type inference
///
/// ```catla
/// let a = 100
/// let b = a
/// let c = b
/// ```
///
/// set(a, b, c): int
///
///           │
///           ▼
///
/// a: int | b: int | c: int
use allocator_api2::vec::Vec;
use bumpalo::Bump;
use catla_parser::ast::{EntityID, Spanned};
use hashbrown::{DefaultHashBuilder, HashMap, HashSet};

use crate::types::Type;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVariableID(usize);

/// !!! DO NOT INSERT GLOBAL ALLOCATOR (cause memory leak) !!!
#[derive(Debug)]
pub(crate) struct TypeEnvironment<'type_env_alloc> {
    parent: Option<TypeEnvironmentID>,
    return_type: Type,
    entity_var_map: HashMap<EntityID, TypeVariableID, DefaultHashBuilder, &'type_env_alloc Bump>,
    type_variable_set_components:
        Vec<&'type_env_alloc mut TypeVariableSet<'type_env_alloc>, &'type_env_alloc Bump>,
    var_set_map:
        HashMap<TypeVariableID, TypeVariableSetID, DefaultHashBuilder, &'type_env_alloc Bump>,
}

impl<'type_env_alloc> TypeEnvironment<'type_env_alloc> {
    fn new(
        parent: Option<TypeEnvironmentID>,
        return_type: Type,
        allocator: &'type_env_alloc Bump,
    ) -> Self {
        Self {
            parent,
            return_type,
            entity_var_map: HashMap::new_in(allocator),
            type_variable_set_components: Vec::new_in(allocator),
            var_set_map: HashMap::new_in(allocator),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct TypeVariableSetID(usize);

#[derive(Debug)]
pub(crate) struct TypeVariableSet<'type_env_alloc> {
    set: HashSet<TypeVariableID, DefaultHashBuilder, &'type_env_alloc Bump>,
    ty: Option<Spanned<Type>>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct TypeEnvironmentID(usize);

#[derive(Debug)]
pub(crate) struct TypeEnvironmentSet<'type_env_alloc> {
    components: Vec<&'type_env_alloc mut TypeEnvironment<'type_env_alloc>, &'type_env_alloc Bump>,
}

impl<'type_env_alloc> TypeEnvironmentSet<'type_env_alloc> {
    pub fn new(allocator: &'type_env_alloc Bump) -> Self {
        Self {
            components: Vec::new_in(allocator),
        }
    }

    pub fn new_environment(
        &mut self,
        parent: Option<TypeEnvironmentID>,
        return_type: Type,
    ) -> TypeEnvironmentID {
        let allocator = *self.components.allocator();

        self.components
            .push(allocator.alloc(TypeEnvironment::new(parent, return_type, allocator)));

        TypeEnvironmentID(self.components.len() - 1)
    }
}
