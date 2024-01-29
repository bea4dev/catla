use bumpalo::{collections::String, Bump};
use catla_parser::parser::{Expression, Program, Spanned, StatementAST};
use either::Either::Left;

use super::{component::{get_name_entity_id, ComponentContainer, EntityID, EntityIDMapper, NameEnvironment}, TranspileError, TranspileWarning};



fn name_resolve_program<'allocator>(
    ast: Program<'allocator, '_>,
    parent_env_id: Option<EntityID>,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    id_mapper: &mut EntityIDMapper<'allocator>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    let current_environment_id = id_mapper.alloc_id(Some(ast));
    name_environments[current_environment_id] = NameEnvironment::new(parent_env_id, allocator);

    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            _ => continue
        };

        match statement {
            StatementAST::Import(_) => todo!(),
            StatementAST::FunctionDefine(function_define) => {
                if let Ok(function_name) = &function_define.name {
                    if let Left(name) = function_name {
                        get_name_entity_id(
                            current_environment_id,
                            name.value,
                            Some(function_define),
                            name_environments,
                            id_mapper,
                            allocator
                        );
                    }
                }
            },
            StatementAST::DataStructDefine(data_struct_define) => {
                if let Ok(data_struct_name) = &data_struct_define.name {
                    get_name_entity_id(
                        current_environment_id,
                        data_struct_name.value,
                        Some(data_struct_define),
                        name_environments,
                        id_mapper,
                        allocator
                    );
                }
            },
            _ => {}
        }
    }

    let name_environment = &mut name_environments[current_environment_id];

    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            _ => continue
        };

        match statement {
            StatementAST::Assignment(assignment) => {
                
            },
            StatementAST::Exchange(_) => todo!(),
            StatementAST::Import(_) => todo!(),
            StatementAST::StatementAttributes(_) => todo!(),
            StatementAST::VariableDefine(_) => todo!(),
            StatementAST::FunctionDefine(_) => todo!(),
            StatementAST::DataStructDefine(_) => todo!(),
            StatementAST::DropStatement(_) => todo!(),
            StatementAST::Expression(_) => todo!(),
        }
    }
}


fn name_resolve_expression<'allocator>(
    ast: Expression<'allocator, '_>,
    parent_env_id: Option<EntityID>,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    id_mapper: &mut EntityIDMapper<'allocator>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {

}