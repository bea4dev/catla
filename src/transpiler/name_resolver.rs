use bumpalo::Bump;
use catla_parser::parser::{Program, Spanned};

use super::{component::{ComponentContainer, NameEnvironment, EntityIDMapper, EntityID}, error::TranspileReport};


pub struct NameResolveError {
    identifier: Spanned<String>
}


fn name_resolve_program<'allocator>(
    ast: Program<'allocator, '_>,
    parent_env_id: Option<EntityID>,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    id_mapper: &mut EntityIDMapper<'allocator>,
    errors: &mut Vec<Box<dyn TranspileReport>>,
    allocator: &'allocator Bump
) {
    let current_env_id = id_mapper.alloc_id(Some(ast));
    name_environments[current_env_id] = NameEnvironment::new(parent_env_id, allocator);

    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            Err(error) => {
                continue;
            }
        };
    }
}

