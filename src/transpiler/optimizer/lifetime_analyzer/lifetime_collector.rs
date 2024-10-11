use bumpalo::Bump;
use catla_parser::parser::{Program, Spanned, StatementAST};
use fxhash::FxHashMap;

use crate::transpiler::{component::EntityID, context::TranspileModuleContext, name_resolver::FoundDefineInfo, semantics::types::{type_inference::TypeInferenceResultContainer, type_info::Type}};

use super::{LifetimeInstance, LifetimeScope, ScoopGroup, StackLifetimeScope};

fn add_entity_to_scope(
    entity_id: EntityID,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope
) {
    let ty = type_inference_result.entity_type_map.get(&entity_id).unwrap();
    let strict_drop = should_strict_drop(&ty);

    if strict_drop {
        lifetime_scope.add(entity_id);
    } else {
        stack_lifetime_scope.add(entity_id);
    }
}

fn should_strict_drop(ty: &Type) -> bool {
    true
}

pub fn collect_lifetime_program<'allocator>(
    ast: Program<'_, 'allocator>,
    scoop_group: Option<&mut ScoopGroup>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_instance_map: &mut FxHashMap<EntityID, LifetimeInstance>,
    allocator: &Bump,
    context: &TranspileModuleContext
) {
    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            Err(_) => continue
        };

        match statement {
            StatementAST::Assignment(assignment) => {
                
            },
            StatementAST::Exchange(exchange) => todo!(),
            StatementAST::Import(import) => todo!(),
            StatementAST::StatementAttributes(vec) => todo!(),
            StatementAST::VariableDefine(variable_define) => todo!(),
            StatementAST::FunctionDefine(function_define) => todo!(),
            StatementAST::UserTypeDefine(user_type_define) => todo!(),
            StatementAST::TypeDefine(type_define) => todo!(),
            StatementAST::Implements(implements) => todo!(),
            StatementAST::DropStatement(drop_statement) => todo!(),
            StatementAST::Expression(_) => todo!(),
        }
    }
}
