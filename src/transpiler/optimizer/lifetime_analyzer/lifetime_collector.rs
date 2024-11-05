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
    is_variable: bool,
) {
    let lifetime_tree = lifetime_scope.instance.get_lifetime_tree(lifetime_tree_ref);

    if !lifetime_tree.is_alloc_point && !is_variable {
        return;
    }

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
    return_value_tree_ref: LifetimeTreeRef,
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
                    return_value_tree_ref,
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
    return_value_tree_ref: LifetimeTreeRef,
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
        ExpressionEnum::OrExpression(or_expression) => {
            collect_lifetime_or_expression(
                or_expression,
                as_assign_left,
                expr_bound_tree_ref,
                return_value_tree_ref,
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
        ExpressionEnum::ReturnExpression(return_expression) => {
            if let Some(expression) = return_expression.expression {
                collect_lifetime_expression(
                    expression,
                    as_assign_left,
                    Some(return_value_tree_ref),
                    return_value_tree_ref,
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
        ExpressionEnum::Closure(closure) => {}
    }
}

fn collect_lifetime_or_expression<'allocator>(
    ast: &'allocator OrExpression<'_, 'allocator>,
    as_assign_left: bool,
    expr_bound_tree_ref: Option<LifetimeTreeRef>,
    return_value_tree_ref: LifetimeTreeRef,
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
        collect_lifetime_and_expression(
            &ast.left_expr,
            as_assign_left,
            expr_bound_tree_ref,
            return_value_tree_ref,
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
        collect_lifetime_and_expression(
            &ast.left_expr,
            as_assign_left,
            None,
            return_value_tree_ref,
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
                    None,
                    return_value_tree_ref,
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

fn collect_lifetime_and_expression<'allocator>(
    ast: &'allocator AndExpression<'_, 'allocator>,
    as_assign_left: bool,
    expr_bound_tree_ref: Option<LifetimeTreeRef>,
    return_value_tree_ref: LifetimeTreeRef,
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
        collect_lifetime_compare_expression(
            &ast.left_expr,
            as_assign_left,
            expr_bound_tree_ref,
            return_value_tree_ref,
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
        collect_lifetime_compare_expression(
            &ast.left_expr,
            as_assign_left,
            None,
            return_value_tree_ref,
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
                    None,
                    return_value_tree_ref,
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

macro_rules! collect_lifetime_for_op2 {
    ($function_name:ident, $ast_type:ident, $next_layer_function_name:ident) => {
        fn $function_name<'allocator>(
            ast: &'allocator $ast_type<'_, 'allocator>,
            as_assign_left: bool,
            expr_bound_tree_ref: Option<LifetimeTreeRef>,
            return_value_tree_ref: LifetimeTreeRef,
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
                    return_value_tree_ref,
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
                        return_value_tree_ref,
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
                        false,
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
                        false,
                    );

                    {
                        let mut lifetime_scope =
                            LifetimeScope::new(lifetime_scope.instance, allocator);

                        $next_layer_function_name(
                            right_expr,
                            as_assign_left,
                            Some(right_lifetime_tree_ref),
                            return_value_tree_ref,
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
                    lifetime_scope
                        .instance
                        .merge(expr_bound_tree_ref, prev_lifetime_ref);
                } else {
                    add_lifetime_tree_to_scope(
                        prev_lifetime_ref,
                        &prev_type,
                        lifetime_scope,
                        stack_lifetime_scope,
                        false,
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
    return_value_tree_ref: LifetimeTreeRef,
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
                return_value_tree_ref,
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
                return_value_tree_ref,
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
                false,
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
                lifetime_scope
                    .instance
                    .merge(expr_bound_tree_ref, return_value_ref);
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
                    false,
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
    return_value_tree_ref: LifetimeTreeRef,
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

    let mut prev_lifetime_tree_ref = lifetime_scope.instance.create_lifetime_tree();
    let mut prev_type = Type::Unknown;
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
                let literal_lifetime_tree = lifetime_scope
                    .instance
                    .get_lifetime_tree(literal_lifetime_tree_ref);
                literal_lifetime_tree.lifetimes.push(STATIC_LIFETIME);
            } else {
                let mut lifetime_scope = LifetimeScope::new(lifetime_scope.instance, allocator);

                add_lifetime_tree_to_scope(
                    literal_lifetime_tree_ref,
                    type_inference_result.get_entity_type(EntityID::from(literal)),
                    &mut lifetime_scope,
                    stack_lifetime_scope,
                    false,
                );

                lifetime_scope.collect();
            }

            if let Some(function_call) = function_call {
                let return_value = collect_lifetime_function_call(
                    function_call,
                    EntityID::from(literal),
                    return_value_tree_ref,
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

                prev_lifetime_tree_ref = return_value;
            } else {
                prev_lifetime_tree_ref = literal_lifetime_tree_ref;
            }

            current_chain = count + 1;
        }
    } else {
        let primary_left_lifetime_ref = collect_lifetime_primary_left(
            &ast.left,
            return_value_tree_ref,
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

        prev_lifetime_tree_ref = primary_left_lifetime_ref;
    }

    loop {
        if current_chain >= ast.chain.len() {
            break;
        }

        let right_primary = &ast.chain[current_chain];

        let mut lifetime_scope = LifetimeScope::new(lifetime_scope.instance, allocator);

        add_lifetime_tree_to_scope(
            prev_lifetime_tree_ref,
            &prev_type,
            &mut lifetime_scope,
            stack_lifetime_scope,
            false,
        );

        let (last_lifetime_ref, last_type) =
            if let Some((literal, _, function_call)) = &right_primary.second_expr {
                let child_lifetime_ref = lifetime_scope
                    .instance
                    .get_or_create_child(prev_lifetime_tree_ref, literal.value);
                lifetime_scope
                    .instance
                    .link_entity_and_lifetime_ref(EntityID::from(literal), child_lifetime_ref);

                if let Some(function_call) = function_call {
                    let mut lifetime_scope = LifetimeScope::new(lifetime_scope.instance, allocator);

                    add_lifetime_tree_to_scope(
                        child_lifetime_ref,
                        type_inference_result.get_entity_type(EntityID::from(literal)),
                        &mut lifetime_scope,
                        stack_lifetime_scope,
                        false,
                    );

                    let return_value = collect_lifetime_function_call(
                        function_call,
                        EntityID::from(literal),
                        return_value_tree_ref,
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

                    (
                        return_value,
                        type_inference_result
                            .get_entity_type(EntityID::from(function_call))
                            .clone(),
                    )
                } else {
                    (
                        child_lifetime_ref,
                        type_inference_result
                            .get_entity_type(EntityID::from(literal))
                            .clone(),
                    )
                }
            } else {
                (prev_lifetime_tree_ref, prev_type)
            };

        // TODO - add mapping operator

        lifetime_scope.collect();

        prev_lifetime_tree_ref = last_lifetime_ref;
        prev_type = last_type;

        current_chain += 1;
    }

    if let Some(expr_bound_tree_ref) = expr_bound_tree_ref {
        lifetime_scope
            .instance
            .merge(expr_bound_tree_ref, prev_lifetime_tree_ref);
    } else {
        if as_assign_left {
            add_lifetime_tree_to_scope(
                prev_lifetime_tree_ref,
                &prev_type,
                lifetime_scope,
                stack_lifetime_scope,
                false,
            );
        }
    }
}

fn collect_lifetime_primary_left<'allocator>(
    ast: &'allocator PrimaryLeft<'_, 'allocator>,
    return_value_tree_ref: LifetimeTreeRef,
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
    let ast_lifetime_ref = lifetime_scope
        .instance
        .create_entity_lifetime_tree(EntityID::from(ast));

    match &ast.first_expr {
        PrimaryLeftExpr::Simple((simple_primary, _, function_call)) => {
            let simple_primary_lifetime_ref = collect_lifetime_simple_primary(
                simple_primary,
                return_value_tree_ref,
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

            let last_lifetime_ref = if let Some(function_call) = function_call {
                let mut lifetime_scope = LifetimeScope::new(lifetime_scope.instance, allocator);

                add_lifetime_tree_to_scope(
                    simple_primary_lifetime_ref,
                    &type_inference_result.get_entity_type(EntityID::from(simple_primary)),
                    &mut lifetime_scope,
                    stack_lifetime_scope,
                    false,
                );

                let return_value = collect_lifetime_function_call(
                    function_call,
                    EntityID::from(simple_primary),
                    return_value_tree_ref,
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

                return_value
            } else {
                simple_primary_lifetime_ref
            };

            lifetime_scope
                .instance
                .merge(ast_lifetime_ref, last_lifetime_ref);
        }
        PrimaryLeftExpr::NewArrayInitExpression(new_array_init_expression) => {
            let array_type = type_inference_result.get_entity_type(EntityID::from(&ast.first_expr));
            let base_type = if let Type::Array(base_type) = array_type {
                base_type.as_ref()
            } else {
                &Type::Unknown
            };

            let array_lifetime_ref = lifetime_scope
                .instance
                .create_entity_lifetime_tree(EntityID::from(new_array_init_expression));

            if new_array_init_expression.for_keyword_span.is_some() {
                let mut lifetime_scope = LifetimeScope::new(lifetime_scope.instance, allocator);

                let return_init_func_value =
                    if let Ok(init_expression) = new_array_init_expression.init_expression {
                        let init_expression_lifetime_ref = lifetime_scope
                            .instance
                            .create_entity_lifetime_tree(EntityID::from(init_expression));

                        {
                            let mut lifetime_scope =
                                LifetimeScope::new(lifetime_scope.instance, allocator);

                            collect_lifetime_expression(
                                init_expression,
                                false,
                                Some(init_expression_lifetime_ref),
                                return_value_tree_ref,
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

                        let init_expression_type =
                            type_inference_result.get_entity_type(EntityID::from(init_expression));

                        add_lifetime_tree_to_scope(
                            init_expression_lifetime_ref,
                            init_expression_type,
                            &mut lifetime_scope,
                            stack_lifetime_scope,
                            false,
                        );

                        let return_value = lifetime_scope.instance.create_lifetime_tree();

                        if let Type::Function {
                            function_info,
                            generics: _,
                        } = init_expression_type
                        {
                            let function_call = FunctionCallLifetime {
                                arguments: vec![lifetime_scope.instance.create_lifetime_tree()],
                                return_value,
                                function: function_info.clone(),
                            };

                            lifetime_scope.instance.add_function_call(function_call);
                        }

                        return_value
                    } else {
                        lifetime_scope.instance.create_lifetime_tree()
                    };

                let return_init_func_value_tree = lifetime_scope
                    .instance
                    .get_lifetime_tree(return_init_func_value);
                return_init_func_value_tree
                    .depend_trees
                    .insert(array_lifetime_ref);

                let child_lifetime_ref = lifetime_scope
                    .instance
                    .get_or_create_child(array_lifetime_ref, "");

                lifetime_scope
                    .instance
                    .merge(child_lifetime_ref, return_init_func_value);

                lifetime_scope.collect();
            } else {
                let init_value =
                    if let Ok(init_expression) = new_array_init_expression.init_expression {
                        let init_expression_ref = lifetime_scope
                            .instance
                            .create_entity_lifetime_tree(EntityID::from(init_expression));

                        {
                            let mut lifetime_scope =
                                LifetimeScope::new(lifetime_scope.instance, allocator);

                            collect_lifetime_expression(
                                init_expression,
                                false,
                                Some(init_expression_ref),
                                return_value_tree_ref,
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

                        init_expression_ref
                    } else {
                        lifetime_scope.instance.create_lifetime_tree()
                    };

                let init_value_tree = lifetime_scope.instance.get_lifetime_tree(init_value);
                init_value_tree.depend_trees.insert(array_lifetime_ref);

                let child_lifetime_ref = lifetime_scope
                    .instance
                    .get_or_create_child(array_lifetime_ref, "");

                lifetime_scope
                    .instance
                    .merge(child_lifetime_ref, init_value);
            }

            lifetime_scope
                .instance
                .merge(ast_lifetime_ref, array_lifetime_ref);
        }
        PrimaryLeftExpr::NewArrayExpression(new_array_expression) => todo!(),
        PrimaryLeftExpr::NewExpression(new_expression) => todo!(),
        PrimaryLeftExpr::IfExpression(if_expression) => todo!(),
        PrimaryLeftExpr::LoopExpression(loop_expression) => todo!(),
    };

    ast_lifetime_ref
}

fn collect_lifetime_simple_primary<'allocator>(
    ast: &'allocator SimplePrimary<'_, 'allocator>,
    return_value_tree_ref: LifetimeTreeRef,
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
                    return_value_tree_ref,
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

                lifetime_scope
                    .instance
                    .merge(ast_lifetime_ref, expression_lifetime_ref);

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

                        lifetime_scope
                            .instance
                            .merge(ast_lifetime_ref, variable_lifetime_ref);

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

                literal_lifetime_tree.lifetimes.push(STATIC_LIFETIME);

                lifetime_scope
                    .instance
                    .merge(ast_lifetime_ref, literal_lifetime_ref);
            }
        }
        SimplePrimary::ThisKeyword(_) => {
            if let Some(this_argument_lifetime_ref) =
                lifetime_scope.instance.this_argument_lifetime_ref
            {
                lifetime_scope
                    .instance
                    .merge(ast_lifetime_ref, this_argument_lifetime_ref);
            }
        }
        _ => {}
    }

    ast_lifetime_ref
}

fn collect_lifetime_function_call<'allocator>(
    ast: &'allocator FunctionCall<'_, 'allocator>,
    call_target_entity_id: EntityID,
    return_value_tree_ref: LifetimeTreeRef,
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
                        return_value_tree_ref,
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
                    false,
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

    let return_value_lifetime_tree = lifetime_scope.instance.get_lifetime_tree(return_value);
    return_value_lifetime_tree.is_alloc_point = true;

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

    let return_value_lifetime_tree = lifetime_scope.instance.get_lifetime_tree(return_value_ref);
    return_value_lifetime_tree.is_alloc_point = true;

    (
        return_value_ref,
        type_inference_result
            .operator_return_type_map
            .get(&right_expr)
            .unwrap()
            .clone(),
    )
}
