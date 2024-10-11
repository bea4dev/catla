use bumpalo::Bump;
use catla_parser::parser::{Expression, ExpressionEnum, OrExpression, Program, Spanned, StatementAST};
use fxhash::FxHashMap;

use crate::transpiler::{
    component::EntityID,
    context::TranspileModuleContext,
    name_resolver::FoundDefineInfo,
    semantics::types::{type_inference::TypeInferenceResultContainer, type_info::Type},
};

use super::{LifetimeInstance, LifetimeScope, ScoopGroup, StackLifetimeScope};

fn add_entity_to_scope(
    entity_id: EntityID,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
) {
    let ty = type_inference_result
        .entity_type_map
        .get(&entity_id)
        .unwrap();
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
    block_scoop_group: Option<&ScoopGroup>,
    function_scoop_group: Option<&ScoopGroup>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_instance_map: &mut FxHashMap<EntityID, LifetimeInstance>,
    allocator: &Bump,
    context: &TranspileModuleContext,
) {
    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            Err(_) => continue,
        };

        match statement {
            StatementAST::Assignment(assignment) => {}
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

fn collect_lifetime_expression<'allocator>(
    ast: Expression<'_, 'allocator>,
    block_scoop_group: Option<&ScoopGroup>,
    function_scoop_group: Option<&ScoopGroup>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_instance_map: &mut FxHashMap<EntityID, LifetimeInstance>,
    allocator: &Bump,
    context: &TranspileModuleContext,
) {
    match ast {
        ExpressionEnum::OrExpression(or_expression) => todo!(),
        ExpressionEnum::ReturnExpression(return_expression) => {
            if let Some(expression) = return_expression.expression {
                collect_lifetime_expression(
                    expression,
                    function_scoop_group.clone(),
                    function_scoop_group,
                    import_element_map,
                    name_resolved_map,
                    type_inference_result,
                    lifetime_scope,
                    stack_lifetime_scope,
                    lifetime_instance_map,
                    allocator,
                    context,
                );
            }
        }
        ExpressionEnum::Closure(closure) => todo!(),
    }
}

fn collect_lifetime_or_expression<'allocator>(
    ast: &'allocator OrExpression<'_, 'allocator>,
    block_scoop_group: Option<&ScoopGroup>,
    function_scoop_group: Option<&ScoopGroup>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_instance_map: &mut FxHashMap<EntityID, LifetimeInstance>,
    allocator: &Bump,
    context: &TranspileModuleContext,
) {
    
}
