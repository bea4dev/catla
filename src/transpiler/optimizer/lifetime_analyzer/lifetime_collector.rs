use std::sync::Arc;

use allocator_api2::vec;
use allocator_api2::vec::Vec;
use bumpalo::Bump;
use catla_parser::parser::{
    AddOrSubExpression, AndExpression, CompareExpression, Expression, ExpressionEnum, Factor,
    FunctionCall, MulOrDivExpression, OrExpression, Primary, PrimaryLeft, PrimaryLeftExpr, Program,
    SimplePrimary, Spanned, StatementAST, AST,
};
use fxhash::FxHashMap;

use crate::transpiler::{
    component::EntityID,
    context::TranspileModuleContext,
    name_resolver::{DefineKind, FoundDefineInfo},
    semantics::types::{
        import_module_collector::get_module_name_from_primary,
        type_inference::TypeInferenceResultContainer, type_info::Type,
    },
};

use super::{
    FunctionCallLifetime, LifetimeInstance, LifetimeScope, LifetimeTreeRef, ScoopGroup,
    StackLifetimeScope, STATIC_LIFETIME,
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
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_instance_map: &mut FxHashMap<EntityID, LifetimeInstance>,
    allocator: &'allocator Bump,
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
                    module_element_type_map,
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
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_instance_map: &mut FxHashMap<EntityID, LifetimeInstance>,
    allocator: &'allocator Bump,
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
                    module_element_type_map,
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
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_instance_map: &mut FxHashMap<EntityID, LifetimeInstance>,
    allocator: &'allocator Bump,
    context: &TranspileModuleContext,
) {
    collect_lifetime_and_expression(
        &ast.left_expr,
        as_assign_left,
        expr_bound_tree_ref,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
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
                module_element_type_map,
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
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_instance_map: &mut FxHashMap<EntityID, LifetimeInstance>,
    allocator: &'allocator Bump,
    context: &TranspileModuleContext,
) {
    collect_lifetime_compare_expression(
        &ast.left_expr,
        as_assign_left,
        expr_bound_tree_ref,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
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
                module_element_type_map,
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
            module_element_type_map: &FxHashMap<String, Type>,
            module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
            type_inference_result: &TypeInferenceResultContainer,
            lifetime_scope: &mut LifetimeScope,
            stack_lifetime_scope: &mut StackLifetimeScope,
            lifetime_instance_map: &mut FxHashMap<EntityID, LifetimeInstance>,
            allocator: &'allocator Bump,
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
                    module_element_type_map,
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
                        module_element_type_map,
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
                            module_element_type_map,
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
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_instance_map: &mut FxHashMap<EntityID, LifetimeInstance>,
    allocator: &'allocator Bump,
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
                module_element_type_map,
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
                module_element_type_map,
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
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_instance_map: &mut FxHashMap<EntityID, LifetimeInstance>,
    allocator: &'allocator Bump,
    context: &TranspileModuleContext,
) {
    let module_name =
        get_module_name_from_primary(ast, name_resolved_map, import_element_map, context);

    let mut prev_lifetime_tree_ref = None;
    let mut current_chain = 0;

    if let Some((module_name, count)) = module_name {
        if ast.chain.is_empty() {
            return;
        }

        let user_type_map = module_user_type_map.get(&module_name).unwrap();
        let element_type_map = module_element_type_maps.get(&module_name).unwrap();

        let next_primary = &ast.chain[count];

        if let Some((literal, _, function_call)) = &next_primary.second_expr {
            let is_static_element = if user_type_map.contains_key(literal.value) {
                false
            } else if element_type_map.contains_key(literal.value) {
                true
            } else {
                false
            };

            let literal_lifetime_tree_ref = lifetime_scope
                .instance
                .create_entity_lifetime_tree(EntityID::from(next_primary));

            if is_static_element {
                lifetime_scope
                    .instance
                    .set_lifetime(literal_lifetime_tree_ref, STATIC_LIFETIME);
            } else {
                let mut lifetime_scope = LifetimeScope::new(lifetime_scope.instance, allocator);

                add_lifetime_tree_to_scope(
                    literal_lifetime_tree_ref,
                    type_inference_result.get_entity_type(EntityID::from(literal)),
                    &mut lifetime_scope,
                    stack_lifetime_scope,
                );

                lifetime_scope.collect();
            }

            if let Some(function_call) = function_call {
                let return_value = collect_lifetime_function_call(
                    function_call,
                    EntityID::from(literal),
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    type_inference_result,
                    lifetime_scope,
                    stack_lifetime_scope,
                    lifetime_instance_map,
                    allocator,
                    context,
                );

                prev_lifetime_tree_ref = Some(return_value);
            } else {
                prev_lifetime_tree_ref = Some(literal_lifetime_tree_ref);
            }

            current_chain = count + 1;
        }
    } else {
        let left_lifetime_tree_ref = lifetime_scope
            .instance
            .create_entity_lifetime_tree(EntityID::from(&ast.left));
    }

    loop {}
}

fn collect_lifetime_primary_left<'allocator>(
    ast: &'allocator PrimaryLeft<'_, 'allocator>,
    expr_bound_tree_ref: Option<LifetimeTreeRef>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_instance_map: &mut FxHashMap<EntityID, LifetimeInstance>,
    allocator: &'allocator Bump,
    context: &TranspileModuleContext,
) {
    let lifetime_tree_ref = match &ast.first_expr {
        PrimaryLeftExpr::Simple((simple_primary, _, function_call)) => {}
        PrimaryLeftExpr::NewArrayInitExpression(new_array_init_expression) => todo!(),
        PrimaryLeftExpr::NewArrayExpression(new_array_expression) => todo!(),
        PrimaryLeftExpr::NewExpression(new_expression) => todo!(),
        PrimaryLeftExpr::IfExpression(if_expression) => todo!(),
        PrimaryLeftExpr::LoopExpression(loop_expression) => todo!(),
    };
}

fn collect_lifetime_simple_primary<'allocator>(
    ast: &'allocator SimplePrimary<'_, 'allocator>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_instance_map: &mut FxHashMap<EntityID, LifetimeInstance>,
    allocator: &'allocator Bump,
    context: &TranspileModuleContext,
) -> LifetimeTreeRef {
    // DO NOT SET LIFETIME at this layer.
    // This lifetime is set at the parent layer.
    let ast_lifetime_ref = lifetime_scope
        .instance
        .create_entity_lifetime_tree(EntityID::from(ast));

    match ast {
        SimplePrimary::Expressions {
            expressions,
            error_tokens: _,
            span: _,
        } => {
            for expression in expressions.iter() {
                let mut lifetime_scope = LifetimeScope::new(lifetime_scope.instance, allocator);

                let expression_lifetime_ref = lifetime_scope
                    .instance
                    .create_entity_lifetime_tree(EntityID::from(*expression));

                collect_lifetime_expression(
                    *expression,
                    false,
                    Some(expression_lifetime_ref),
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    type_inference_result,
                    &mut lifetime_scope,
                    stack_lifetime_scope,
                    lifetime_instance_map,
                    allocator,
                    context,
                );

                let ast_lifetime_tree = lifetime_scope.instance.get_lifetime_tree(ast_lifetime_ref);
                ast_lifetime_tree
                    .same_bound_entity
                    .push(expression_lifetime_ref);

                lifetime_scope.collect();
            }
        }
        SimplePrimary::Identifier(literal) => {
            let is_local_variable =
                if let Some(resolved) = name_resolved_map.get(&EntityID::from(literal)) {
                    let kind = resolved.define_info.define_kind;

                    if kind == DefineKind::Variable || kind == DefineKind::FunctionArgument {
                        let variable_entity_id = resolved.define_info.entity_id;
                        let variable_lifetime_ref = lifetime_scope
                            .instance
                            .get_entity_lifetime_tree_ref(variable_entity_id);
                        let ast_lifetime_ref =
                            lifetime_scope.instance.get_lifetime_tree(ast_lifetime_ref);

                        ast_lifetime_ref
                            .same_bound_entity
                            .push(variable_lifetime_ref);

                        true
                    } else {
                        false
                    }
                } else {
                    false
                };

            if !is_local_variable {
                let literal_lifetime_ref = lifetime_scope
                    .instance
                    .create_entity_lifetime_tree(EntityID::from(literal));
                let literal_lifetime_tree = lifetime_scope
                    .instance
                    .get_lifetime_tree(literal_lifetime_ref);

                literal_lifetime_tree.root_lifetime = Some(STATIC_LIFETIME);

                let ast_lifetime_tree = lifetime_scope.instance.get_lifetime_tree(ast_lifetime_ref);

                ast_lifetime_tree
                    .same_bound_entity
                    .push(literal_lifetime_ref);
            }
        }
        SimplePrimary::ThisKeyword(_) => {
            if let Some(this_argument_lifetime_ref) =
                lifetime_scope.instance.this_argument_lifetime_ref
            {
                let ast_lifetime_tree = lifetime_scope.instance.get_lifetime_tree(ast_lifetime_ref);

                ast_lifetime_tree
                    .same_bound_entity
                    .push(this_argument_lifetime_ref);
            }
        }
        _ => {}
    }

    ast_lifetime_ref
}

fn collect_lifetime_function_call<'allocator>(
    ast: &'allocator FunctionCall<'_, 'allocator>,
    call_target_entity_id: EntityID,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_instance_map: &mut FxHashMap<EntityID, LifetimeInstance>,
    allocator: &'allocator Bump,
    context: &TranspileModuleContext,
) -> LifetimeTreeRef {
    let ty = type_inference_result.get_entity_type(call_target_entity_id);

    let return_value = lifetime_scope.instance.create_lifetime_tree();

    if let Type::Function {
        function_info,
        generics: _,
    } = ty
    {
        let mut lifetime_scope = LifetimeScope::new(lifetime_scope.instance, allocator);
        let mut arguments = Vec::new();

        if let Ok(arg_exprs) = &ast.arg_exprs {
            for argument in arg_exprs.iter() {
                let argument_lifetime_ref = lifetime_scope
                    .instance
                    .create_entity_lifetime_tree(EntityID::from(*argument));

                {
                    let mut lifetime_scope = LifetimeScope::new(lifetime_scope.instance, allocator);

                    collect_lifetime_expression(
                        argument,
                        false,
                        Some(argument_lifetime_ref),
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
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

                let argument_type =
                    type_inference_result.get_entity_type(EntityID::from(*argument));

                add_lifetime_tree_to_scope(
                    argument_lifetime_ref,
                    &argument_type,
                    &mut lifetime_scope,
                    stack_lifetime_scope,
                );

                arguments.push(argument_lifetime_ref);
            }
        }

        let function_call = FunctionCallLifetime {
            arguments,
            return_value,
            function: function_info.clone(),
        };

        lifetime_scope.instance.add_function_call(function_call);

        lifetime_scope.collect();
    }

    return_value
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
