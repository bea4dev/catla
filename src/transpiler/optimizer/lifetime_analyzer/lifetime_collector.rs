use std::sync::Arc;

use allocator_api2::vec;
use bumpalo::Bump;
use catla_parser::parser::{
    AddOrSubExpression, AndExpression, CompareExpression, Expression, ExpressionEnum, Factor,
    MulOrDivExpression, OrExpression, Primary, Program, Spanned, StatementAST, AST,
};
use fxhash::FxHashMap;

use crate::transpiler::{
    component::EntityID,
    context::TranspileModuleContext,
    name_resolver::FoundDefineInfo,
    semantics::types::{
        import_module_collector::get_module_name_from_primary,
        type_inference::TypeInferenceResultContainer, type_info::Type,
    },
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
    expr_bound_tree_ref: Option<LifetimeTreeRef>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
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
                let expr_bound_tree_ref = if index + 1 == ast.statements.len() {
                    expr_bound_tree_ref
                } else {
                    None
                };

                collect_lifetime_expression(
                    *expression,
                    false,
                    expr_bound_tree_ref,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_maps,
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
    as_assign_left: bool,
    expr_bound_tree_ref: Option<LifetimeTreeRef>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
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
                    as_assign_left,
                    expr_bound_tree_ref,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_maps,
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
    as_assign_left: bool,
    expr_bound_tree_ref: Option<LifetimeTreeRef>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_instance_map: &mut FxHashMap<EntityID, LifetimeInstance>,
    allocator: &Bump,
    context: &TranspileModuleContext,
) {
    collect_lifetime_and_expression(
        &ast.left_expr,
        as_assign_left,
        expr_bound_tree_ref,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_maps,
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
                as_assign_left,
                expr_bound_tree_ref,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_maps,
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
    as_assign_left: bool,
    expr_bound_tree_ref: Option<LifetimeTreeRef>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_instance_map: &mut FxHashMap<EntityID, LifetimeInstance>,
    allocator: &Bump,
    context: &TranspileModuleContext,
) {
    collect_lifetime_compare_expression(
        &ast.left_expr,
        as_assign_left,
        expr_bound_tree_ref,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_maps,
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
                as_assign_left,
                expr_bound_tree_ref,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_maps,
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

macro_rules! collect_lifetime_for_op2 {
    ($function_name:ident, $ast_type:ident, $next_layer_function_name:ident) => {
        fn $function_name<'allocator>(
            ast: &'allocator $ast_type<'_, 'allocator>,
            as_assign_left: bool,
            expr_bound_tree_ref: Option<LifetimeTreeRef>,
            import_element_map: &FxHashMap<EntityID, Spanned<String>>,
            name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
            module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
            module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
            type_inference_result: &TypeInferenceResultContainer,
            lifetime_scope: &mut LifetimeScope,
            stack_lifetime_scope: &mut StackLifetimeScope,
            lifetime_instance_map: &mut FxHashMap<EntityID, LifetimeInstance>,
            allocator: &Bump,
            context: &TranspileModuleContext,
        ) {
            if ast.right_exprs.is_empty() {
                $next_layer_function_name(
                    &ast.left_expr,
                    as_assign_left,
                    expr_bound_tree_ref,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_maps,
                    type_inference_result,
                    lifetime_scope,
                    stack_lifetime_scope,
                    lifetime_instance_map,
                    allocator,
                    context,
                );
            } else {
                {
                    let mut lifetime_scope = LifetimeScope::new(lifetime_scope.instance, allocator);

                    let lifetime_tree_ref = lifetime_scope
                        .instance
                        .create_entity_lifetime_tree(EntityID::from(&ast.left_expr));

                    $next_layer_function_name(
                        &ast.left_expr,
                        as_assign_left,
                        Some(lifetime_tree_ref),
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_maps,
                        type_inference_result,
                        &mut lifetime_scope,
                        stack_lifetime_scope,
                        lifetime_instance_map,
                        allocator,
                        context,
                    );

                    lifetime_scope.collect();
                }

                let mut prev_lifetime_ref = lifetime_scope
                    .instance
                    .get_entity_lifetime_tree_ref(EntityID::from(&ast.left_expr));
                let mut prev_type = type_inference_result
                    .get_entity_type(EntityID::from(&ast.left_expr))
                    .clone();

                for (_, right_expr) in ast.right_exprs.iter() {
                    let right_expr = match right_expr {
                        Ok(expr) => expr,
                        Err(_) => continue,
                    };

                    let mut lifetime_scope = LifetimeScope::new(lifetime_scope.instance, allocator);

                    add_lifetime_tree_to_scope(
                        prev_lifetime_ref,
                        &prev_type,
                        &mut lifetime_scope,
                        stack_lifetime_scope,
                    );

                    let right_lifetime_tree_ref = lifetime_scope
                        .instance
                        .create_entity_lifetime_tree(EntityID::from(right_expr));
                    let right_type =
                        type_inference_result.get_entity_type(EntityID::from(right_expr));

                    add_lifetime_tree_to_scope(
                        right_lifetime_tree_ref,
                        right_type,
                        &mut lifetime_scope,
                        stack_lifetime_scope,
                    );

                    {
                        let mut lifetime_scope =
                            LifetimeScope::new(lifetime_scope.instance, allocator);

                        $next_layer_function_name(
                            right_expr,
                            as_assign_left,
                            Some(right_lifetime_tree_ref),
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_maps,
                            type_inference_result,
                            &mut lifetime_scope,
                            stack_lifetime_scope,
                            lifetime_instance_map,
                            allocator,
                            context,
                        );

                        lifetime_scope.collect();
                    }

                    let (return_value_ref, return_type) = collect_lifetime_operator(
                        prev_lifetime_ref,
                        EntityID::from(right_expr),
                        type_inference_result,
                        &mut lifetime_scope,
                    );
                    prev_lifetime_ref = return_value_ref;
                    prev_type = return_type;

                    lifetime_scope.collect();
                }

                if let Some(expr_bound_tree_ref) = expr_bound_tree_ref {
                    let bound_lifetime_tree = lifetime_scope
                        .instance
                        .get_lifetime_tree(expr_bound_tree_ref);
                    bound_lifetime_tree
                        .same_bound_entity
                        .push(prev_lifetime_ref);
                } else {
                    add_lifetime_tree_to_scope(
                        prev_lifetime_ref,
                        &prev_type,
                        lifetime_scope,
                        stack_lifetime_scope,
                    );
                }
            }
        }
    };
}

collect_lifetime_for_op2!(
    collect_lifetime_compare_expression,
    CompareExpression,
    collect_lifetime_add_or_sub_expression
);

collect_lifetime_for_op2!(
    collect_lifetime_add_or_sub_expression,
    AddOrSubExpression,
    collect_lifetime_mul_or_div_expression
);

collect_lifetime_for_op2!(
    collect_lifetime_mul_or_div_expression,
    MulOrDivExpression,
    collect_lifetime_factor
);

fn collect_lifetime_factor<'allocator>(
    ast: &'allocator Factor<'_, 'allocator>,
    as_assign_left: bool,
    expr_bound_tree_ref: Option<LifetimeTreeRef>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_instance_map: &mut FxHashMap<EntityID, LifetimeInstance>,
    allocator: &Bump,
    context: &TranspileModuleContext,
) {
    if ast.negative_keyword_span.is_none() {
        if let Ok(primary) = &ast.primary {
            collect_lifetime_primary(
                primary,
                as_assign_left,
                expr_bound_tree_ref,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_maps,
                type_inference_result,
                lifetime_scope,
                stack_lifetime_scope,
                lifetime_instance_map,
                allocator,
                context,
            );
        }
    } else {
        let mut lifetime_scope = LifetimeScope::new(lifetime_scope.instance, allocator);

        if let Ok(primary) = &ast.primary {
            let mut primary_lifetime_scope = LifetimeScope::new(lifetime_scope.instance, allocator);

            let primary_lifetime_tree_ref = primary_lifetime_scope
                .instance
                .create_entity_lifetime_tree(EntityID::from(primary));

            collect_lifetime_primary(
                primary,
                as_assign_left,
                Some(primary_lifetime_tree_ref),
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_maps,
                type_inference_result,
                &mut primary_lifetime_scope,
                stack_lifetime_scope,
                lifetime_instance_map,
                allocator,
                context,
            );

            let primary_type = type_inference_result.get_entity_type(EntityID::from(primary));

            add_lifetime_tree_to_scope(
                primary_lifetime_tree_ref,
                &primary_type,
                &mut primary_lifetime_scope,
                stack_lifetime_scope,
            );

            let function_type = match type_inference_result
                .operator_function_type_map
                .get(&EntityID::from(primary))
                .unwrap()
            {
                Type::Function {
                    function_info,
                    generics: _,
                } => function_info.clone(),
                _ => unreachable!(),
            };

            let return_value_ref = primary_lifetime_scope.instance.create_lifetime_tree();

            let function_call = FunctionCallLifetime {
                arguments: vec![primary_lifetime_tree_ref],
                return_value: return_value_ref,
                function: function_type,
            };
            primary_lifetime_scope
                .instance
                .add_function_call(function_call);

            primary_lifetime_scope.collect();

            if let Some(expr_bound_tree_ref) = expr_bound_tree_ref {
                let bound_lifetime_tree = lifetime_scope
                    .instance
                    .get_lifetime_tree(expr_bound_tree_ref);
                bound_lifetime_tree.same_bound_entity.push(return_value_ref);
            } else {
                let return_type = type_inference_result
                    .operator_return_type_map
                    .get(&EntityID::from(primary))
                    .unwrap();

                add_lifetime_tree_to_scope(
                    return_value_ref,
                    &return_type,
                    &mut lifetime_scope,
                    stack_lifetime_scope,
                );
            }
        }

        lifetime_scope.collect();
    }
}

fn collect_lifetime_primary<'allocator>(
    ast: &'allocator Primary<'_, 'allocator>,
    as_assign_left: bool,
    expr_bound_tree_ref: Option<LifetimeTreeRef>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_instance_map: &mut FxHashMap<EntityID, LifetimeInstance>,
    allocator: &Bump,
    context: &TranspileModuleContext,
) {
    let module_name =
        get_module_name_from_primary(ast, name_resolved_map, import_element_map, context);

    if let Some((module_name, count)) = module_name {
        if ast.chain.is_empty() {
            return;
        }

        let module_user_type_map = module_user_type_map.get(&module_name).unwrap();
        let module_element_type_map = module_element_type_maps.get(&module_name).unwrap();

        let next_primary = &ast.chain[count];

        if let Some((literal, _, function_call)) = &next_primary.second_expr {
            let is_static_element = if module_user_type_map.contains_key(literal.value) {
                false
            } else if module_element_type_map.contains_key(literal.value) {
                true
            } else {
                false
            };

            
        }
    } else {
    }
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

    (
        return_value_ref,
        type_inference_result
            .operator_return_type_map
            .get(&right_expr)
            .unwrap()
            .clone(),
    )
}
