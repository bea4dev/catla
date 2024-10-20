use allocator_api2::vec;
use bumpalo::Bump;
use catla_parser::parser::{
    AddOrSubExpression, AndExpression, CompareExpression, Expression, ExpressionEnum, OrExpression,
    Program, Spanned, StatementAST, AST,
};
use fxhash::FxHashMap;

use crate::transpiler::{
    component::EntityID,
    context::TranspileModuleContext,
    name_resolver::FoundDefineInfo,
    semantics::types::{type_inference::TypeInferenceResultContainer, type_info::Type},
};

use super::{
    FunctionCallLifetime, LifetimeInstance, LifetimeScope, LifetimeTreeRef, ScoopGroup,
    StackLifetimeScope,
};

fn add_lifetime_tree_to_scope(
    lifetime_tree_ref: LifetimeTreeRef,
    ty: &Type,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
) {
    let strict_drop = should_strict_drop(&ty);

    if strict_drop {
        lifetime_scope.add(lifetime_tree_ref);
    } else {
        stack_lifetime_scope.add(lifetime_tree_ref);
    }
}

fn should_strict_drop(ty: &Type) -> bool {
    true
}

pub fn collect_lifetime_program<'allocator>(
    ast: Program<'_, 'allocator>,
    expr_bound_entity: Option<EntityID>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_instance_map: &mut FxHashMap<EntityID, LifetimeInstance>,
    allocator: &Bump,
    context: &TranspileModuleContext,
) {
    for (index, statement) in ast.statements.iter().enumerate() {
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
            StatementAST::Expression(expression) => {
                let expr_bound_entity = if index + 1 == ast.statements.len() {
                    expr_bound_entity
                } else {
                    None
                };

                collect_lifetime_expression(
                    *expression,
                    expr_bound_entity,
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
    }
}

fn collect_lifetime_expression<'allocator>(
    ast: Expression<'_, 'allocator>,
    expr_bound_entity: Option<EntityID>,
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
                    expr_bound_entity,
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
    expr_bound_entity: Option<EntityID>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_instance_map: &mut FxHashMap<EntityID, LifetimeInstance>,
    allocator: &Bump,
    context: &TranspileModuleContext,
) {
    collect_lifetime_and_expression(
        &ast.left_expr,
        expr_bound_entity,
        import_element_map,
        name_resolved_map,
        type_inference_result,
        lifetime_scope,
        stack_lifetime_scope,
        lifetime_instance_map,
        allocator,
        context,
    );

    for (_, right_expr) in ast.right_exprs.iter() {
        if let Ok(right_expr) = right_expr {
            collect_lifetime_and_expression(
                right_expr,
                expr_bound_entity,
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
}

fn collect_lifetime_and_expression<'allocator>(
    ast: &'allocator AndExpression<'_, 'allocator>,
    expr_bound_entity: Option<EntityID>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_instance_map: &mut FxHashMap<EntityID, LifetimeInstance>,
    allocator: &Bump,
    context: &TranspileModuleContext,
) {
    collect_lifetime_compare_expression(
        &ast.left_expr,
        expr_bound_entity,
        import_element_map,
        name_resolved_map,
        type_inference_result,
        lifetime_scope,
        stack_lifetime_scope,
        lifetime_instance_map,
        allocator,
        context,
    );

    for (_, right_expr) in ast.right_exprs.iter() {
        if let Ok(right_expr) = right_expr {
            collect_lifetime_compare_expression(
                right_expr,
                expr_bound_entity,
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
}

fn collect_lifetime_compare_expression<'allocator>(
    ast: &'allocator CompareExpression<'_, 'allocator>,
    expr_bound_entity: Option<EntityID>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_instance_map: &mut FxHashMap<EntityID, LifetimeInstance>,
    allocator: &Bump,
    context: &TranspileModuleContext,
) {
    if ast.right_exprs.is_empty() {
        collect_lifetime_add_or_sub_expression(
            &ast.left_expr,
            expr_bound_entity,
            import_element_map,
            name_resolved_map,
            type_inference_result,
            lifetime_scope,
            stack_lifetime_scope,
            lifetime_instance_map,
            allocator,
            context,
        );
    } else {
        let lifetime_instance = &mut lifetime_scope.instance;
        let mut lifetime_scope = LifetimeScope::new(lifetime_instance, allocator);

        lifetime_scope.instance.create_entity_lifetime_tree(EntityID::from(&ast.left_expr));

        collect_lifetime_add_or_sub_expression(
            &ast.left_expr,
            Some(EntityID::from(&ast.left_expr)),
            import_element_map,
            name_resolved_map,
            type_inference_result,
            &mut lifetime_scope,
            stack_lifetime_scope,
            lifetime_instance_map,
            allocator,
            context,
        );

        lifetime_scope.collect();

        let mut prev_lifetime_ref = lifetime_instance.get_entity_lifetime_tree_ref(EntityID::from(&ast.left_expr));
        let mut prev_type = type_inference_result.get_entity_type(EntityID::from(&ast.left_expr)).clone();

        for (_, right_expr) in ast.right_exprs.iter() {
            let right_expr = match right_expr {
                Ok(expr) => expr,
                Err(_) => continue
            };

            let mut lifetime_scope = LifetimeScope::new(lifetime_instance, allocator);

            add_lifetime_tree_to_scope(prev_lifetime_ref, &prev_type, &mut lifetime_scope, stack_lifetime_scope);

            lifetime_scope.instance.create_entity_lifetime_tree(EntityID::from(right_expr));
            let right_lifetime_tree_ref = lifetime_scope.instance.get_entity_lifetime_tree_ref(EntityID::from(right_expr));
            let right_type = type_inference_result.get_entity_type(EntityID::from(right_expr));

            add_lifetime_tree_to_scope(right_lifetime_tree_ref, right_type, &mut lifetime_scope, stack_lifetime_scope);

            collect_lifetime_add_or_sub_expression(
                right_expr,
                Some(EntityID::from(right_expr)),
                import_element_map,
                name_resolved_map,
                type_inference_result,
                &mut lifetime_scope,
                stack_lifetime_scope,
                lifetime_instance_map,
                allocator,
                context,
            );

            let (return_value_ref, return_type) = collect_lifetime_operator(prev_lifetime_ref, EntityID::from(right_expr), type_inference_result, &mut lifetime_scope);
            prev_lifetime_ref = return_value_ref;
            prev_type = return_type;

            lifetime_scope.collect();
        }
    }
}

fn collect_lifetime_add_or_sub_expression<'allocator>(
    ast: &'allocator AddOrSubExpression<'_, 'allocator>,
    expr_bound_entity: Option<EntityID>,
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

fn collect_lifetime_operator<'allocator>(
    prev_entity_ref: LifetimeTreeRef,
    right_expr: EntityID,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
) -> (LifetimeTreeRef, Type) {
    let function_type = match type_inference_result
        .operator_function_type_map
        .get(&right_expr)
        .unwrap()
    {
        Type::Function {
            function_info,
            generics: _,
        } => function_info.clone(),
        _ => unreachable!(),
    };

    let return_value_ref = lifetime_scope.instance.create_lifetime_tree();
    let right_expr_tree_ref = lifetime_scope
        .instance
        .get_entity_lifetime_tree_ref(right_expr);

    let function_call = FunctionCallLifetime {
        arguments: vec![prev_entity_ref, right_expr_tree_ref],
        return_value: return_value_ref,
        function: function_type,
    };
    lifetime_scope.instance.add_function_call(function_call);

    (return_value_ref, type_inference_result.get_entity_type(right_expr).clone())
}
