use core::panic;
use std::sync::Arc;

use allocator_api2::vec;
use allocator_api2::vec::Vec;
use bumpalo::Bump;
use catla_parser::parser::{
    AddOrSubExpression, AndExpression, CompareExpression, Expression, ExpressionEnum, Factor,
    FunctionCall, MappingOperator, MappingOperatorKind, MulOrDivExpression, OrExpression, Primary,
    PrimaryLeft, PrimaryLeftExpr, Program, SimplePrimary, Spanned, StatementAST, VariableBinding,
};
use either::Either;
use fxhash::FxHashMap;

use crate::transpiler::{
    component::EntityID,
    context::TranspileModuleContext,
    name_resolver::{DefineKind, EnvironmentSeparatorKind, FoundDefineInfo},
    semantics::types::{
        import_module_collector::get_module_name_from_primary,
        type_inference::TypeInferenceResultContainer, type_info::Type,
    },
};

use super::{
    FunctionCallLifetime, LifetimeExpected, LifetimeInstance, LifetimeScope, LifetimeSource,
    LifetimeTreeRef, LoopSuppressor, StackLifetimeScope, STATIC_LIFETIME,
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

#[derive(Debug, Default)]
pub struct ClosureScope {
    captured: Vec<(EntityID, String)>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum VariableType {
    LocalVariable,
    StaticOrCapturedVariable,
    NotVariable,
}

pub fn collect_lifetime_program<'allocator>(
    ast: Program<'_, 'allocator>,
    force_be_expression: bool,
    expr_bound_tree_ref: Option<LifetimeTreeRef>,
    return_value_tree_ref: LifetimeTreeRef,
    closure_scope_stack: &mut Vec<ClosureScope>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_source_map: &mut FxHashMap<EntityID, LifetimeSource>,
    allocator: &'allocator Bump,
    context: &TranspileModuleContext,
) {
    for (index, statement) in ast.statements.iter().enumerate() {
        let statement = match statement {
            Ok(statement) => statement,
            Err(_) => continue,
        };

        match statement {
            StatementAST::Assignment(assignment) => {
                let mut left_variable_type = VariableType::NotVariable;

                let left_lifetime_ref = lifetime_scope
                    .instance
                    .create_entity_lifetime_tree(EntityID::from(assignment.left_expr));

                {
                    let mut lifetime_scope = LifetimeScope::new(lifetime_scope.instance, allocator);

                    collect_lifetime_expression(
                        assignment.left_expr,
                        true,
                        true,
                        Some(left_lifetime_ref),
                        return_value_tree_ref,
                        closure_scope_stack,
                        &mut left_variable_type,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        type_inference_result,
                        &mut lifetime_scope,
                        stack_lifetime_scope,
                        lifetime_source_map,
                        allocator,
                        context,
                    );

                    lifetime_scope.collect();
                }

                let right_lifetime_ref = if let Ok(expression) = assignment.right_expr {
                    let mut lifetime_scope = LifetimeScope::new(lifetime_scope.instance, allocator);

                    let expression_lifetime_ref = lifetime_scope
                        .instance
                        .create_entity_lifetime_tree(EntityID::from(expression));

                    collect_lifetime_expression(
                        expression,
                        true,
                        false,
                        Some(expression_lifetime_ref),
                        return_value_tree_ref,
                        closure_scope_stack,
                        &mut VariableType::NotVariable,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        type_inference_result,
                        &mut lifetime_scope,
                        stack_lifetime_scope,
                        lifetime_source_map,
                        allocator,
                        context,
                    );

                    add_lifetime_tree_to_scope(
                        expression_lifetime_ref,
                        type_inference_result.get_entity_type(EntityID::from(expression)),
                        &mut lifetime_scope,
                        stack_lifetime_scope,
                    );

                    lifetime_scope.collect();

                    expression_lifetime_ref
                } else {
                    lifetime_scope.instance.create_lifetime_tree()
                };

                if left_variable_type != VariableType::NotVariable {
                    // deref borrowed
                    let left_lifetime_tree =
                        lifetime_scope.instance.get_lifetime_tree(left_lifetime_ref);

                    let left_lifetime_ref = if left_variable_type == VariableType::LocalVariable {
                        if left_lifetime_tree.borrow_ref.len() != 1 {
                            panic!("invalid left lifetime tree");
                        }

                        left_lifetime_tree.borrow_ref.iter().next().unwrap().clone()
                    } else {
                        left_lifetime_ref
                    };

                    let expected = LifetimeExpected {
                        shorter: left_lifetime_ref,
                        should_ignore_insert_on_shorter: left_variable_type
                            == VariableType::LocalVariable,
                        longer: right_lifetime_ref,
                    };
                    lifetime_scope.instance.lifetime_expected.insert(expected);
                }

                lifetime_scope
                    .instance
                    .add_insert(left_lifetime_ref, right_lifetime_ref);
            }
            StatementAST::Exchange(exchange) => {}
            StatementAST::VariableDefine(variable_define) => {
                let variable_lifetime_ref = if let Ok(binding) = &variable_define.binding {
                    create_lifetime_tree_for_variable_binding(
                        lifetime_scope.instance,
                        binding,
                        false,
                    )
                } else {
                    lifetime_scope.instance.create_lifetime_tree()
                };

                let (expression_lifetime_ref, expression_type) =
                    if let Some(expression_result) = &variable_define.expression {
                        if let Ok(expression) = expression_result {
                            let mut lifetime_scope =
                                LifetimeScope::new(lifetime_scope.instance, allocator);

                            let expression_lifetime_ref = lifetime_scope
                                .instance
                                .create_entity_lifetime_tree(EntityID::from(*expression));

                            collect_lifetime_expression(
                                *expression,
                                true,
                                false,
                                Some(expression_lifetime_ref),
                                return_value_tree_ref,
                                closure_scope_stack,
                                &mut VariableType::NotVariable,
                                import_element_map,
                                name_resolved_map,
                                module_user_type_map,
                                module_element_type_map,
                                module_element_type_maps,
                                type_inference_result,
                                &mut lifetime_scope,
                                stack_lifetime_scope,
                                lifetime_source_map,
                                allocator,
                                context,
                            );

                            lifetime_scope.collect();

                            (
                                expression_lifetime_ref,
                                type_inference_result.get_entity_type(EntityID::from(*expression)),
                            )
                        } else {
                            (
                                lifetime_scope.instance.create_lifetime_tree(),
                                &Type::Unknown,
                            )
                        }
                    } else {
                        (
                            lifetime_scope.instance.create_lifetime_tree(),
                            &Type::Unknown,
                        )
                    };

                lifetime_scope
                    .instance
                    .merge(variable_lifetime_ref, expression_lifetime_ref);

                add_lifetime_tree_to_scope(
                    variable_lifetime_ref,
                    expression_type,
                    lifetime_scope,
                    stack_lifetime_scope,
                );
            }
            StatementAST::FunctionDefine(function_define) => {
                let mut lifetime_instance = LifetimeInstance::new();

                let mut argument_lifetimes = Vec::new();

                if function_define.args.this_mutability.is_some() {
                    argument_lifetimes.push(lifetime_instance.this_argument_lifetime_ref);

                    let this_lifetime_tree = lifetime_instance
                        .get_lifetime_tree(lifetime_instance.this_argument_lifetime_ref);
                    this_lifetime_tree.is_argument_tree = true;
                }

                for argument in function_define.args.arguments.iter() {
                    let lifetime_ref = create_lifetime_tree_for_variable_binding(
                        &mut lifetime_instance,
                        &argument.binding,
                        true,
                    );

                    argument_lifetimes.push(lifetime_ref);
                }

                let mut argument_tree_refs = Vec::new();
                for argument_ref in argument_lifetimes.iter() {
                    lifetime_instance.collect_argument_tree_ref(
                        *argument_ref,
                        &mut argument_tree_refs,
                        &mut LoopSuppressor::new(),
                    );
                }

                let mut lifetime_scope = LifetimeScope::new(&mut lifetime_instance, allocator);
                let mut stack_lifetime_scope = StackLifetimeScope::new(allocator);

                let return_value_tree_ref = lifetime_scope.instance.create_lifetime_tree();

                if let Some(block_or_semicolon) = &function_define.block_or_semicolon.value {
                    if let Either::Right(block) = block_or_semicolon {
                        collect_lifetime_program(
                            block.program,
                            false,
                            None,
                            return_value_tree_ref,
                            &mut Vec::new(),
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            module_element_type_maps,
                            type_inference_result,
                            &mut lifetime_scope,
                            &mut stack_lifetime_scope,
                            lifetime_source_map,
                            allocator,
                            context,
                        );
                    }
                }

                lifetime_scope.collect();
                stack_lifetime_scope.collect(&mut lifetime_instance);

                let argument_lifetime = lifetime_instance.next_lifetime();
                for argument_tree_ref in argument_tree_refs {
                    let argument_tree = lifetime_instance.get_lifetime_tree(argument_tree_ref);
                    argument_tree.lifetimes.push(argument_lifetime);
                }

                let lifetime_source = LifetimeSource {
                    instance: lifetime_instance,
                    arguments: argument_lifetimes,
                    return_value: return_value_tree_ref,
                };

                lifetime_source_map.insert(EntityID::from(function_define), lifetime_source);
            }
            StatementAST::UserTypeDefine(user_type_define) => {
                let mut lifetime_instance = LifetimeInstance::new();
                let mut lifetime_scope = LifetimeScope::new(&mut lifetime_instance, allocator);
                let mut stack_lifetime_scope = StackLifetimeScope::new(allocator);

                let return_value_tree_ref = lifetime_scope.instance.create_lifetime_tree();

                if let Some(block) = &user_type_define.block.value {
                    collect_lifetime_program(
                        block.program,
                        false,
                        None,
                        return_value_tree_ref,
                        closure_scope_stack,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        type_inference_result,
                        &mut lifetime_scope,
                        &mut stack_lifetime_scope,
                        lifetime_source_map,
                        allocator,
                        context,
                    );
                }

                lifetime_scope.collect();
                stack_lifetime_scope.collect(&mut lifetime_instance);

                let lifetime_source = LifetimeSource {
                    instance: lifetime_instance,
                    arguments: vec![],
                    return_value: return_value_tree_ref,
                };

                lifetime_source_map.insert(EntityID::from(user_type_define), lifetime_source);
            }
            StatementAST::Implements(implements) => {
                let mut lifetime_instance = LifetimeInstance::new();
                let mut lifetime_scope = LifetimeScope::new(&mut lifetime_instance, allocator);
                let mut stack_lifetime_scope = StackLifetimeScope::new(allocator);

                let return_value_tree_ref = lifetime_scope.instance.create_lifetime_tree();

                if let Some(block) = &implements.block.value {
                    collect_lifetime_program(
                        block.program,
                        false,
                        None,
                        return_value_tree_ref,
                        closure_scope_stack,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        type_inference_result,
                        &mut lifetime_scope,
                        &mut stack_lifetime_scope,
                        lifetime_source_map,
                        allocator,
                        context,
                    );
                }

                lifetime_scope.collect();
                stack_lifetime_scope.collect(&mut lifetime_instance);

                let lifetime_source = LifetimeSource {
                    instance: lifetime_instance,
                    arguments: vec![],
                    return_value: return_value_tree_ref,
                };

                lifetime_source_map.insert(EntityID::from(implements), lifetime_source);
            }
            StatementAST::DropStatement(drop_statement) => {}
            StatementAST::Expression(expression) => {
                let is_last_expression = index + 1 == ast.statements.len();

                let expr_bound_tree_ref = if is_last_expression && force_be_expression {
                    expr_bound_tree_ref
                } else {
                    None
                };

                collect_lifetime_expression(
                    *expression,
                    is_last_expression && force_be_expression,
                    false,
                    expr_bound_tree_ref,
                    return_value_tree_ref,
                    closure_scope_stack,
                    &mut VariableType::NotVariable,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    type_inference_result,
                    lifetime_scope,
                    stack_lifetime_scope,
                    lifetime_source_map,
                    allocator,
                    context,
                );
            }
            _ => {}
        }
    }
}

fn create_lifetime_tree_for_variable_binding(
    lifetime_instance: &mut LifetimeInstance,
    binding: &VariableBinding,
    is_argument: bool,
) -> LifetimeTreeRef {
    match &binding.binding {
        Either::Left(literal) => {
            let lifetime_tree_ref =
                lifetime_instance.create_entity_lifetime_tree(EntityID::from(literal));

            let lifetime_tree = lifetime_instance.get_lifetime_tree(lifetime_tree_ref);
            lifetime_tree.is_argument_tree = is_argument;

            lifetime_tree_ref
        }
        Either::Right(bindings) => {
            let lifetime_ref = lifetime_instance.create_lifetime_tree();

            let lifetime_tree = lifetime_instance.get_lifetime_tree(lifetime_ref);
            lifetime_tree.is_argument_tree = is_argument;

            for (index, binding) in bindings.iter().enumerate() {
                let index_str = index.to_string();

                let child_ref =
                    lifetime_instance.get_or_create_child(lifetime_ref, index_str.as_str());
                let binding_ref = create_lifetime_tree_for_variable_binding(
                    lifetime_instance,
                    binding,
                    is_argument,
                );
                lifetime_instance.merge(child_ref, binding_ref);
            }

            lifetime_ref
        }
    }
}

fn collect_lifetime_expression<'allocator>(
    ast: Expression<'_, 'allocator>,
    force_be_expression: bool,
    as_assign_left: bool,
    expr_bound_tree_ref: Option<LifetimeTreeRef>,
    return_value_tree_ref: LifetimeTreeRef,
    closure_scope_stack: &mut Vec<ClosureScope>,
    variable_type: &mut VariableType,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_source_map: &mut FxHashMap<EntityID, LifetimeSource>,
    allocator: &'allocator Bump,
    context: &TranspileModuleContext,
) {
    match ast {
        ExpressionEnum::OrExpression(or_expression) => {
            collect_lifetime_or_expression(
                or_expression,
                force_be_expression,
                as_assign_left,
                expr_bound_tree_ref,
                return_value_tree_ref,
                closure_scope_stack,
                variable_type,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                type_inference_result,
                lifetime_scope,
                stack_lifetime_scope,
                lifetime_source_map,
                allocator,
                context,
            );
        }
        ExpressionEnum::ReturnExpression(return_expression) => {
            if let Some(expression) = return_expression.expression {
                collect_lifetime_expression(
                    expression,
                    true,
                    as_assign_left,
                    Some(return_value_tree_ref),
                    return_value_tree_ref,
                    closure_scope_stack,
                    &mut VariableType::NotVariable,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    type_inference_result,
                    lifetime_scope,
                    stack_lifetime_scope,
                    lifetime_source_map,
                    allocator,
                    context,
                );
            }
        }
        ExpressionEnum::Closure(closure) => {
            let closure_lifetime_tree_ref = {
                let parent_lifetime_instance = &mut lifetime_scope.instance;

                // create closure lifetime tree
                let closure_lifetime_tree_ref =
                    parent_lifetime_instance.create_entity_lifetime_tree(EntityID::from(closure));
                let closure_lifetime_tree =
                    parent_lifetime_instance.get_lifetime_tree(closure_lifetime_tree_ref);
                closure_lifetime_tree.is_alloc_point = true;

                closure_lifetime_tree_ref
            };

            if let Some(expr_bound) = expr_bound_tree_ref {
                lifetime_scope
                    .instance
                    .merge(expr_bound, closure_lifetime_tree_ref);
            } else {
                add_lifetime_tree_to_scope(
                    closure_lifetime_tree_ref,
                    type_inference_result.get_entity_type(EntityID::from(closure)),
                    lifetime_scope,
                    stack_lifetime_scope,
                );
            }

            let parent_lifetime_instance = &mut lifetime_scope.instance;

            let mut lifetime_instance = LifetimeInstance::new();

            let mut argument_lifetimes = Vec::new();

            argument_lifetimes.push(lifetime_instance.this_argument_lifetime_ref);

            let closure_captured_lifetime_tree =
                lifetime_instance.get_lifetime_tree(lifetime_instance.this_argument_lifetime_ref);
            closure_captured_lifetime_tree.is_argument_tree = true;

            match &closure.arguments.arguments {
                Either::Left(literal) => {
                    let lifetime_tree_ref =
                        lifetime_instance.create_entity_lifetime_tree(EntityID::from(literal));

                    let lifetime_tree = lifetime_instance.get_lifetime_tree(lifetime_tree_ref);
                    lifetime_tree.is_argument_tree = true;

                    argument_lifetimes.push(lifetime_tree_ref);
                }
                Either::Right(arguments) => {
                    for argument in arguments.iter() {
                        match argument {
                            Either::Left(argument) => {
                                let lifetime_ref = create_lifetime_tree_for_variable_binding(
                                    &mut lifetime_instance,
                                    &argument.binding,
                                    true,
                                );

                                argument_lifetimes.push(lifetime_ref);
                            }
                            Either::Right(literal) => {
                                let lifetime_tree_ref = lifetime_instance
                                    .create_entity_lifetime_tree(EntityID::from(literal));

                                let lifetime_tree =
                                    lifetime_instance.get_lifetime_tree(lifetime_tree_ref);
                                lifetime_tree.is_argument_tree = true;

                                argument_lifetimes.push(lifetime_tree_ref);
                            }
                        }
                    }
                }
            }

            let mut argument_tree_refs = Vec::new();
            for argument_ref in argument_lifetimes.iter() {
                lifetime_instance.collect_argument_tree_ref(
                    *argument_ref,
                    &mut argument_tree_refs,
                    &mut LoopSuppressor::new(),
                );
            }

            let mut lifetime_scope = LifetimeScope::new(&mut lifetime_instance, allocator);
            let mut stack_lifetime_scope = StackLifetimeScope::new(allocator);

            let return_value_tree_ref = lifetime_scope.instance.create_lifetime_tree();

            closure_scope_stack.push(ClosureScope::default());

            if let Some(expression_or_block) = &closure.expression_or_block.value {
                match expression_or_block {
                    Either::Left(expression) => {
                        collect_lifetime_expression(
                            *expression,
                            true,
                            false,
                            Some(return_value_tree_ref),
                            return_value_tree_ref,
                            closure_scope_stack,
                            &mut VariableType::NotVariable,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            module_element_type_maps,
                            type_inference_result,
                            &mut lifetime_scope,
                            &mut stack_lifetime_scope,
                            lifetime_source_map,
                            allocator,
                            context,
                        );
                    }
                    Either::Right(block) => {
                        collect_lifetime_program(
                            block.program,
                            true,
                            Some(return_value_tree_ref),
                            return_value_tree_ref,
                            closure_scope_stack,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            module_element_type_maps,
                            type_inference_result,
                            &mut lifetime_scope,
                            &mut stack_lifetime_scope,
                            lifetime_source_map,
                            allocator,
                            context,
                        );
                    }
                }
            }

            lifetime_scope.collect();
            stack_lifetime_scope.collect(&mut lifetime_instance);

            let argument_lifetime = lifetime_instance.next_lifetime();
            for argument_tree_ref in argument_tree_refs {
                let argument_tree = lifetime_instance.get_lifetime_tree(argument_tree_ref);
                argument_tree.lifetimes.push(argument_lifetime);
            }

            let lifetime_source = LifetimeSource {
                instance: lifetime_instance,
                arguments: argument_lifetimes,
                return_value: return_value_tree_ref,
            };

            lifetime_source_map.insert(EntityID::from(closure), lifetime_source);

            // collect captured
            let closure_scope = closure_scope_stack.pop().unwrap();

            for (captured, captured_name) in closure_scope.captured {
                let captured_lifetime_tree_ref = match parent_lifetime_instance
                    .entity_lifetime_ref_map
                    .get(&captured)
                {
                    Some(lifetime_ref) => {
                        parent_lifetime_instance.resolve_lifetime_ref(*lifetime_ref)
                    }
                    None => continue,
                };

                let closure_child_ref = parent_lifetime_instance
                    .get_or_create_child(closure_lifetime_tree_ref, &captured_name);
                let closure_child_tree =
                    parent_lifetime_instance.get_lifetime_tree(closure_child_ref);
                closure_child_tree
                    .borrow_ref
                    .insert(captured_lifetime_tree_ref);
            }
        }
    }
}

fn collect_lifetime_or_expression<'allocator>(
    ast: &'allocator OrExpression<'_, 'allocator>,
    force_be_expression: bool,
    as_assign_left: bool,
    expr_bound_tree_ref: Option<LifetimeTreeRef>,
    return_value_tree_ref: LifetimeTreeRef,
    closure_scope_stack: &mut Vec<ClosureScope>,
    variable_type: &mut VariableType,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_source_map: &mut FxHashMap<EntityID, LifetimeSource>,
    allocator: &'allocator Bump,
    context: &TranspileModuleContext,
) {
    if ast.right_exprs.is_empty() {
        collect_lifetime_and_expression(
            &ast.left_expr,
            force_be_expression,
            as_assign_left,
            expr_bound_tree_ref,
            return_value_tree_ref,
            closure_scope_stack,
            variable_type,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            type_inference_result,
            lifetime_scope,
            stack_lifetime_scope,
            lifetime_source_map,
            allocator,
            context,
        );
    } else {
        collect_lifetime_and_expression(
            &ast.left_expr,
            true,
            as_assign_left,
            None,
            return_value_tree_ref,
            closure_scope_stack,
            &mut VariableType::NotVariable,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            type_inference_result,
            lifetime_scope,
            stack_lifetime_scope,
            lifetime_source_map,
            allocator,
            context,
        );

        for (_, right_expr) in ast.right_exprs.iter() {
            if let Ok(right_expr) = right_expr {
                collect_lifetime_and_expression(
                    right_expr,
                    true,
                    as_assign_left,
                    None,
                    return_value_tree_ref,
                    closure_scope_stack,
                    &mut VariableType::NotVariable,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    type_inference_result,
                    lifetime_scope,
                    stack_lifetime_scope,
                    lifetime_source_map,
                    allocator,
                    context,
                );
            }
        }
    }
}

fn collect_lifetime_and_expression<'allocator>(
    ast: &'allocator AndExpression<'_, 'allocator>,
    force_be_expression: bool,
    as_assign_left: bool,
    expr_bound_tree_ref: Option<LifetimeTreeRef>,
    return_value_tree_ref: LifetimeTreeRef,
    closure_scope_stack: &mut Vec<ClosureScope>,
    variable_type: &mut VariableType,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_source_map: &mut FxHashMap<EntityID, LifetimeSource>,
    allocator: &'allocator Bump,
    context: &TranspileModuleContext,
) {
    if ast.right_exprs.is_empty() {
        collect_lifetime_compare_expression(
            &ast.left_expr,
            force_be_expression,
            as_assign_left,
            expr_bound_tree_ref,
            return_value_tree_ref,
            closure_scope_stack,
            variable_type,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            type_inference_result,
            lifetime_scope,
            stack_lifetime_scope,
            lifetime_source_map,
            allocator,
            context,
        );
    } else {
        collect_lifetime_compare_expression(
            &ast.left_expr,
            true,
            as_assign_left,
            None,
            return_value_tree_ref,
            closure_scope_stack,
            &mut VariableType::NotVariable,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            type_inference_result,
            lifetime_scope,
            stack_lifetime_scope,
            lifetime_source_map,
            allocator,
            context,
        );

        for (_, right_expr) in ast.right_exprs.iter() {
            if let Ok(right_expr) = right_expr {
                collect_lifetime_compare_expression(
                    right_expr,
                    true,
                    as_assign_left,
                    None,
                    return_value_tree_ref,
                    closure_scope_stack,
                    &mut VariableType::NotVariable,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    type_inference_result,
                    lifetime_scope,
                    stack_lifetime_scope,
                    lifetime_source_map,
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
            force_be_expression: bool,
            as_assign_left: bool,
            expr_bound_tree_ref: Option<LifetimeTreeRef>,
            return_value_tree_ref: LifetimeTreeRef,
            closure_scope_stack: &mut Vec<ClosureScope>,
            variable_type: &mut VariableType,
            import_element_map: &FxHashMap<EntityID, Spanned<String>>,
            name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
            module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
            module_element_type_map: &FxHashMap<String, Type>,
            module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
            type_inference_result: &TypeInferenceResultContainer,
            lifetime_scope: &mut LifetimeScope,
            stack_lifetime_scope: &mut StackLifetimeScope,
            lifetime_source_map: &mut FxHashMap<EntityID, LifetimeSource>,
            allocator: &'allocator Bump,
            context: &TranspileModuleContext,
        ) {
            if ast.right_exprs.is_empty() {
                $next_layer_function_name(
                    &ast.left_expr,
                    force_be_expression,
                    as_assign_left,
                    expr_bound_tree_ref,
                    return_value_tree_ref,
                    closure_scope_stack,
                    variable_type,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    type_inference_result,
                    lifetime_scope,
                    stack_lifetime_scope,
                    lifetime_source_map,
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
                        true,
                        as_assign_left,
                        Some(lifetime_tree_ref),
                        return_value_tree_ref,
                        closure_scope_stack,
                        &mut VariableType::NotVariable,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        type_inference_result,
                        &mut lifetime_scope,
                        stack_lifetime_scope,
                        lifetime_source_map,
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
                            true,
                            as_assign_left,
                            Some(right_lifetime_tree_ref),
                            return_value_tree_ref,
                            closure_scope_stack,
                            &mut VariableType::NotVariable,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            module_element_type_maps,
                            type_inference_result,
                            &mut lifetime_scope,
                            stack_lifetime_scope,
                            lifetime_source_map,
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
    force_be_expression: bool,
    as_assign_left: bool,
    expr_bound_tree_ref: Option<LifetimeTreeRef>,
    return_value_tree_ref: LifetimeTreeRef,
    closure_scope_stack: &mut Vec<ClosureScope>,
    variable_type: &mut VariableType,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_source_map: &mut FxHashMap<EntityID, LifetimeSource>,
    allocator: &'allocator Bump,
    context: &TranspileModuleContext,
) {
    if ast.negative_keyword_span.is_none() {
        if let Ok(primary) = &ast.primary {
            collect_lifetime_primary(
                primary,
                force_be_expression,
                as_assign_left,
                expr_bound_tree_ref,
                return_value_tree_ref,
                closure_scope_stack,
                variable_type,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                type_inference_result,
                lifetime_scope,
                stack_lifetime_scope,
                lifetime_source_map,
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
                true,
                as_assign_left,
                Some(primary_lifetime_tree_ref),
                return_value_tree_ref,
                closure_scope_stack,
                &mut VariableType::NotVariable,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                type_inference_result,
                &mut primary_lifetime_scope,
                stack_lifetime_scope,
                lifetime_source_map,
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

            let return_value_tree = primary_lifetime_scope
                .instance
                .get_lifetime_tree(return_value_ref);
            return_value_tree.contains_function_return_value = true;
            return_value_tree.is_alloc_point = true;

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
                );
            }
        }

        lifetime_scope.collect();
    }
}

fn collect_lifetime_primary<'allocator>(
    ast: &'allocator Primary<'_, 'allocator>,
    force_be_expression: bool,
    as_assign_left: bool,
    expr_bound_tree_ref: Option<LifetimeTreeRef>,
    return_value_tree_ref: LifetimeTreeRef,
    closure_scope_stack: &mut Vec<ClosureScope>,
    variable_type: &mut VariableType,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_source_map: &mut FxHashMap<EntityID, LifetimeSource>,
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
                );

                lifetime_scope.collect();
            }

            if let Some(function_call) = function_call {
                let return_value = collect_lifetime_function_call(
                    function_call,
                    EntityID::from(literal),
                    None,
                    return_value_tree_ref,
                    closure_scope_stack,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    type_inference_result,
                    lifetime_scope,
                    stack_lifetime_scope,
                    lifetime_source_map,
                    allocator,
                    context,
                );

                prev_lifetime_tree_ref = return_value;
                prev_type = type_inference_result
                    .get_entity_type(EntityID::from(function_call))
                    .clone();
            } else {
                prev_lifetime_tree_ref = literal_lifetime_tree_ref;
                prev_type = type_inference_result
                    .get_entity_type(EntityID::from(literal))
                    .clone()
            }

            current_chain = count + 1;
        }

        if let Some(mapping_operator) = &next_primary.mapping_operator {
            collect_lifetime_mapping_operator(
                mapping_operator,
                prev_lifetime_tree_ref,
                return_value_tree_ref,
                closure_scope_stack,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                type_inference_result,
                lifetime_scope,
                stack_lifetime_scope,
                lifetime_source_map,
                allocator,
                context,
            );

            prev_type = type_inference_result
                .get_entity_type(EntityID::from(&mapping_operator.value))
                .clone();
        }
    } else {
        let mut variable_type_temp = VariableType::NotVariable;

        let primary_left_lifetime_ref = collect_lifetime_primary_left(
            &ast.left,
            force_be_expression,
            return_value_tree_ref,
            closure_scope_stack,
            &mut variable_type_temp,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            type_inference_result,
            lifetime_scope,
            stack_lifetime_scope,
            lifetime_source_map,
            allocator,
            context,
        );

        if ast.chain.is_empty() && ast.left.mapping_operator.is_none() {
            *variable_type = variable_type_temp;
        }

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
        );

        let (last_lifetime_ref, last_type) =
            if let Some((literal, _, function_call)) = &right_primary.second_expr {
                let child_lifetime_ref = lifetime_scope
                    .instance
                    .get_or_create_child(prev_lifetime_tree_ref, literal.value);
                let borrow_lifetime_ref = lifetime_scope
                    .instance
                    .create_entity_lifetime_tree(EntityID::from(literal));
                let borrow_lifetime_tree = lifetime_scope
                    .instance
                    .get_lifetime_tree(borrow_lifetime_ref);
                borrow_lifetime_tree.borrow_ref.insert(child_lifetime_ref);

                if let Some(function_call) = function_call {
                    let mut lifetime_scope = LifetimeScope::new(lifetime_scope.instance, allocator);

                    add_lifetime_tree_to_scope(
                        borrow_lifetime_ref,
                        type_inference_result.get_entity_type(EntityID::from(literal)),
                        &mut lifetime_scope,
                        stack_lifetime_scope,
                    );

                    let return_value = collect_lifetime_function_call(
                        function_call,
                        EntityID::from(literal),
                        Some(prev_lifetime_tree_ref),
                        return_value_tree_ref,
                        closure_scope_stack,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        type_inference_result,
                        &mut lifetime_scope,
                        stack_lifetime_scope,
                        lifetime_source_map,
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
                        borrow_lifetime_ref,
                        type_inference_result
                            .get_entity_type(EntityID::from(literal))
                            .clone(),
                    )
                }
            } else {
                (prev_lifetime_tree_ref, prev_type)
            };

        if let Some(mapping_operator) = &right_primary.mapping_operator {
            collect_lifetime_mapping_operator(
                mapping_operator,
                last_lifetime_ref,
                return_value_tree_ref,
                closure_scope_stack,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                type_inference_result,
                &mut lifetime_scope,
                stack_lifetime_scope,
                lifetime_source_map,
                allocator,
                context,
            );
        }

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
        if !as_assign_left {
            add_lifetime_tree_to_scope(
                prev_lifetime_tree_ref,
                &prev_type,
                lifetime_scope,
                stack_lifetime_scope,
            );
        }
    }
}

fn collect_lifetime_primary_left<'allocator>(
    ast: &'allocator PrimaryLeft<'_, 'allocator>,
    force_be_expression: bool,
    return_value_tree_ref: LifetimeTreeRef,
    closure_scope_stack: &mut Vec<ClosureScope>,
    variable_type: &mut VariableType,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_source_map: &mut FxHashMap<EntityID, LifetimeSource>,
    allocator: &'allocator Bump,
    context: &TranspileModuleContext,
) -> LifetimeTreeRef {
    let ast_lifetime_ref = lifetime_scope
        .instance
        .create_entity_lifetime_tree(EntityID::from(ast));

    match &ast.first_expr {
        PrimaryLeftExpr::Simple((simple_primary, _, function_call)) => {
            let mut variable_type_temp = VariableType::NotVariable;

            let simple_primary_lifetime_ref = collect_lifetime_simple_primary(
                simple_primary,
                return_value_tree_ref,
                closure_scope_stack,
                &mut variable_type_temp,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                type_inference_result,
                lifetime_scope,
                stack_lifetime_scope,
                lifetime_source_map,
                allocator,
                context,
            );

            if function_call.is_none() {
                *variable_type = variable_type_temp;
            }

            let last_lifetime_ref = if let Some(function_call) = function_call {
                let mut lifetime_scope = LifetimeScope::new(lifetime_scope.instance, allocator);

                add_lifetime_tree_to_scope(
                    simple_primary_lifetime_ref,
                    &type_inference_result.get_entity_type(EntityID::from(simple_primary)),
                    &mut lifetime_scope,
                    stack_lifetime_scope,
                );

                let return_value = collect_lifetime_function_call(
                    function_call,
                    EntityID::from(simple_primary),
                    None,
                    return_value_tree_ref,
                    closure_scope_stack,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    type_inference_result,
                    &mut lifetime_scope,
                    stack_lifetime_scope,
                    lifetime_source_map,
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
            let array_lifetime_ref = lifetime_scope
                .instance
                .create_entity_lifetime_tree(EntityID::from(new_array_init_expression));

            if new_array_init_expression.for_keyword_span.is_some() {
                let mut lifetime_scope = LifetimeScope::new(lifetime_scope.instance, allocator);

                let return_init_func_value = if let Ok(init_expression) =
                    new_array_init_expression.init_expression
                {
                    let init_expression_lifetime_ref = lifetime_scope
                        .instance
                        .create_entity_lifetime_tree(EntityID::from(init_expression));

                    {
                        let mut lifetime_scope =
                            LifetimeScope::new(lifetime_scope.instance, allocator);

                        collect_lifetime_expression(
                            init_expression,
                            true,
                            false,
                            Some(init_expression_lifetime_ref),
                            return_value_tree_ref,
                            closure_scope_stack,
                            &mut VariableType::NotVariable,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            module_element_type_maps,
                            type_inference_result,
                            &mut lifetime_scope,
                            stack_lifetime_scope,
                            lifetime_source_map,
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
                    );

                    let return_value = lifetime_scope.instance.create_lifetime_tree();

                    let return_value_tree = lifetime_scope.instance.get_lifetime_tree(return_value);
                    return_value_tree.contains_function_return_value = true;

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

                let array_type =
                    type_inference_result.get_entity_type(EntityID::from(&ast.first_expr));
                let base_type = if let Type::Array(base_type) = array_type {
                    base_type.as_ref()
                } else {
                    &Type::Unknown
                };

                add_lifetime_tree_to_scope(
                    return_init_func_value,
                    base_type,
                    &mut lifetime_scope,
                    stack_lifetime_scope,
                );

                let borrowed_return_value = lifetime_scope.instance.create_lifetime_tree();
                let borrowed_return_value_tree = lifetime_scope
                    .instance
                    .get_lifetime_tree(borrowed_return_value);
                borrowed_return_value_tree
                    .borrow_ref
                    .insert(return_init_func_value);

                let child_lifetime_ref = lifetime_scope
                    .instance
                    .get_or_create_child(array_lifetime_ref, "");

                lifetime_scope
                    .instance
                    .merge(child_lifetime_ref, borrowed_return_value);

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
                                true,
                                false,
                                Some(init_expression_ref),
                                return_value_tree_ref,
                                closure_scope_stack,
                                &mut VariableType::NotVariable,
                                import_element_map,
                                name_resolved_map,
                                module_user_type_map,
                                module_element_type_map,
                                module_element_type_maps,
                                type_inference_result,
                                &mut lifetime_scope,
                                stack_lifetime_scope,
                                lifetime_source_map,
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
                init_value_tree.alloc_point_ref.insert(array_lifetime_ref);

                let child_lifetime_ref = lifetime_scope
                    .instance
                    .get_or_create_child(array_lifetime_ref, "");

                lifetime_scope
                    .instance
                    .merge(child_lifetime_ref, init_value);
            }

            let array_lifetime_tree = lifetime_scope
                .instance
                .get_lifetime_tree(array_lifetime_ref);
            array_lifetime_tree.is_alloc_point = true;

            lifetime_scope
                .instance
                .merge(ast_lifetime_ref, array_lifetime_ref);
        }
        PrimaryLeftExpr::NewArrayExpression(new_array_expression) => {
            let array_lifetime_ref = lifetime_scope
                .instance
                .create_entity_lifetime_tree(EntityID::from(new_array_expression));
            let child_lifetime_ref = lifetime_scope
                .instance
                .get_or_create_child(array_lifetime_ref, "");

            for value_expression in new_array_expression.value_expressions.iter() {
                let value_expression = match value_expression {
                    Ok(expression) => expression,
                    Err(_) => continue,
                };

                let mut lifetime_scope = LifetimeScope::new(lifetime_scope.instance, allocator);

                let expression_lifetime_ref = lifetime_scope
                    .instance
                    .create_entity_lifetime_tree(EntityID::from(*value_expression));

                collect_lifetime_expression(
                    value_expression,
                    true,
                    false,
                    Some(expression_lifetime_ref),
                    return_value_tree_ref,
                    closure_scope_stack,
                    &mut VariableType::NotVariable,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    type_inference_result,
                    &mut lifetime_scope,
                    stack_lifetime_scope,
                    lifetime_source_map,
                    allocator,
                    context,
                );

                let expression_lifetime_tree = lifetime_scope
                    .instance
                    .get_lifetime_tree(expression_lifetime_ref);
                expression_lifetime_tree
                    .alloc_point_ref
                    .insert(array_lifetime_ref);

                lifetime_scope
                    .instance
                    .merge(child_lifetime_ref, expression_lifetime_ref);

                lifetime_scope.collect();
            }

            let array_lifetime_tree = lifetime_scope
                .instance
                .get_lifetime_tree(array_lifetime_ref);
            array_lifetime_tree.is_alloc_point = true;

            lifetime_scope
                .instance
                .merge(ast_lifetime_ref, array_lifetime_ref);
        }
        PrimaryLeftExpr::NewExpression(new_expression) => {
            let object_lifetime_ref = lifetime_scope
                .instance
                .create_entity_lifetime_tree(EntityID::from(new_expression));

            if let Ok(field_assigns) = &new_expression.field_assigns {
                for field_assign in field_assigns.iter() {
                    let expression = match field_assign.expression {
                        Ok(expression) => expression,
                        Err(_) => continue,
                    };

                    let expression_lifetime_ref = lifetime_scope
                        .instance
                        .create_entity_lifetime_tree(EntityID::from(expression));

                    {
                        let mut lifetime_scope =
                            LifetimeScope::new(lifetime_scope.instance, allocator);

                        collect_lifetime_expression(
                            expression,
                            true,
                            false,
                            Some(expression_lifetime_ref),
                            return_value_tree_ref,
                            closure_scope_stack,
                            &mut VariableType::NotVariable,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            module_element_type_maps,
                            type_inference_result,
                            &mut lifetime_scope,
                            stack_lifetime_scope,
                            lifetime_source_map,
                            allocator,
                            context,
                        );

                        let expression_lifetime_tree = lifetime_scope
                            .instance
                            .get_lifetime_tree(expression_lifetime_ref);
                        expression_lifetime_tree
                            .alloc_point_ref
                            .insert(object_lifetime_ref);

                        let child_lifetime_ref = lifetime_scope
                            .instance
                            .get_or_create_child(object_lifetime_ref, field_assign.name.value);
                        lifetime_scope
                            .instance
                            .merge(child_lifetime_ref, expression_lifetime_ref);

                        lifetime_scope.collect();
                    }
                }
            }

            let object_lifetime_tree = lifetime_scope
                .instance
                .get_lifetime_tree(object_lifetime_ref);
            object_lifetime_tree.is_alloc_point = true;

            lifetime_scope
                .instance
                .merge(ast_lifetime_ref, object_lifetime_ref);
        }
        PrimaryLeftExpr::IfExpression(if_expression) => {
            let first_condition = [if_expression.if_statement.condition.as_ref().ok()];
            let first_block = [if_expression.if_statement.block.value.as_ref()];

            let chain_conditions = if_expression.chain.iter().map(|chain| {
                chain
                    .else_if_or_else
                    .value
                    .as_ref()
                    .map(|chain| match chain {
                        Either::Left(if_statement) => if_statement.condition.as_ref().ok(),
                        Either::Right(_) => None,
                    })
                    .unwrap_or(None)
            });

            let chain_blocks = if_expression.chain.iter().map(|chain| {
                chain
                    .else_if_or_else
                    .value
                    .as_ref()
                    .map(|chain| match chain {
                        Either::Left(if_statement) => if_statement.block.value.as_ref(),
                        Either::Right(block) => Some(block),
                    })
                    .unwrap_or(None)
            });

            let conditions_iter = first_condition.iter().cloned().chain(chain_conditions);
            let blocks_iter = first_block.iter().cloned().chain(chain_blocks);

            let if_expression_lifetime_ref = lifetime_scope
                .instance
                .create_entity_lifetime_tree(EntityID::from(if_expression));

            for (condition, block) in conditions_iter.zip(blocks_iter) {
                if let Some(condition) = condition {
                    let mut lifetime_scope = LifetimeScope::new(lifetime_scope.instance, allocator);

                    collect_lifetime_expression(
                        *condition,
                        true,
                        false,
                        None,
                        return_value_tree_ref,
                        closure_scope_stack,
                        &mut VariableType::NotVariable,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        type_inference_result,
                        &mut lifetime_scope,
                        stack_lifetime_scope,
                        lifetime_source_map,
                        allocator,
                        context,
                    );

                    lifetime_scope.collect();
                }

                if let Some(block) = block {
                    let mut lifetime_scope = LifetimeScope::new(lifetime_scope.instance, allocator);

                    if force_be_expression {
                        let program_lifetime_ref = lifetime_scope
                            .instance
                            .create_entity_lifetime_tree(EntityID::from(block.program));

                        collect_lifetime_program(
                            block.program,
                            force_be_expression,
                            Some(program_lifetime_ref),
                            return_value_tree_ref,
                            closure_scope_stack,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            module_element_type_maps,
                            type_inference_result,
                            &mut lifetime_scope,
                            stack_lifetime_scope,
                            lifetime_source_map,
                            allocator,
                            context,
                        );

                        lifetime_scope
                            .instance
                            .merge(if_expression_lifetime_ref, program_lifetime_ref);
                    } else {
                        collect_lifetime_program(
                            block.program,
                            force_be_expression,
                            None,
                            return_value_tree_ref,
                            closure_scope_stack,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            module_element_type_maps,
                            type_inference_result,
                            &mut lifetime_scope,
                            stack_lifetime_scope,
                            lifetime_source_map,
                            allocator,
                            context,
                        );
                    }

                    lifetime_scope.collect();
                }
            }

            lifetime_scope
                .instance
                .merge(ast_lifetime_ref, if_expression_lifetime_ref);
        }
        PrimaryLeftExpr::LoopExpression(loop_expression) => todo!(),
    };

    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_lifetime_mapping_operator(
            mapping_operator,
            ast_lifetime_ref,
            return_value_tree_ref,
            closure_scope_stack,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            type_inference_result,
            lifetime_scope,
            stack_lifetime_scope,
            lifetime_source_map,
            allocator,
            context,
        )
    };

    ast_lifetime_ref
}

fn collect_lifetime_mapping_operator<'allocator>(
    ast: &'allocator MappingOperator<'_, 'allocator>,
    prev_lifetime_tree_ref: LifetimeTreeRef,
    return_value_tree_ref: LifetimeTreeRef,
    closure_scope_stack: &mut Vec<ClosureScope>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_source_map: &mut FxHashMap<EntityID, LifetimeSource>,
    allocator: &'allocator Bump,
    context: &TranspileModuleContext,
) {
    match &ast.value {
        MappingOperatorKind::ResultElvisBlock(recovered)
        | MappingOperatorKind::NullElvisBlock(recovered) => {
            if let Some(block) = &recovered.value {
                let mut lifetime_scope = LifetimeScope::new(lifetime_scope.instance, allocator);

                let block_lifetime_ref = lifetime_scope
                    .instance
                    .create_entity_lifetime_tree(EntityID::from(block.program));

                collect_lifetime_program(
                    block.program,
                    true,
                    Some(block_lifetime_ref),
                    return_value_tree_ref,
                    closure_scope_stack,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    type_inference_result,
                    &mut lifetime_scope,
                    stack_lifetime_scope,
                    lifetime_source_map,
                    allocator,
                    context,
                );

                lifetime_scope
                    .instance
                    .merge(prev_lifetime_tree_ref, block_lifetime_ref);

                lifetime_scope.collect();
            }
        }
        _ => {}
    }
}

fn collect_lifetime_simple_primary<'allocator>(
    ast: &'allocator SimplePrimary<'_, 'allocator>,
    return_value_tree_ref: LifetimeTreeRef,
    closure_scope_stack: &mut Vec<ClosureScope>,
    variable_type: &mut VariableType,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_source_map: &mut FxHashMap<EntityID, LifetimeSource>,
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
            match expressions.len() {
                1 => {
                    let expression = expressions[0];

                    let mut lifetime_scope = LifetimeScope::new(lifetime_scope.instance, allocator);

                    let expression_lifetime_ref = lifetime_scope
                        .instance
                        .create_entity_lifetime_tree(EntityID::from(expression));

                    collect_lifetime_expression(
                        expression,
                        true,
                        false,
                        Some(expression_lifetime_ref),
                        return_value_tree_ref,
                        closure_scope_stack,
                        &mut VariableType::NotVariable,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        type_inference_result,
                        &mut lifetime_scope,
                        stack_lifetime_scope,
                        lifetime_source_map,
                        allocator,
                        context,
                    );

                    lifetime_scope
                        .instance
                        .merge(ast_lifetime_ref, expression_lifetime_ref);

                    lifetime_scope.collect();
                }
                0 => { /* no lifetime(unit type) */ }
                _ => {
                    let tuple_lifetime_ref = lifetime_scope.instance.create_lifetime_tree();

                    for (index, expression) in expressions.iter().enumerate() {
                        let mut lifetime_scope =
                            LifetimeScope::new(lifetime_scope.instance, allocator);

                        let expression_lifetime_ref = lifetime_scope
                            .instance
                            .create_entity_lifetime_tree(EntityID::from(*expression));

                        collect_lifetime_expression(
                            *expression,
                            true,
                            false,
                            Some(expression_lifetime_ref),
                            return_value_tree_ref,
                            closure_scope_stack,
                            &mut VariableType::NotVariable,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            module_element_type_maps,
                            type_inference_result,
                            &mut lifetime_scope,
                            stack_lifetime_scope,
                            lifetime_source_map,
                            allocator,
                            context,
                        );

                        let index_str = index.to_string();

                        let expression_lifetime_tree = lifetime_scope
                            .instance
                            .get_lifetime_tree(expression_lifetime_ref);
                        expression_lifetime_tree
                            .alloc_point_ref
                            .insert(tuple_lifetime_ref);

                        let tuple_child_lifetime_ref = lifetime_scope
                            .instance
                            .get_or_create_child(tuple_lifetime_ref, index_str.as_str());
                        lifetime_scope
                            .instance
                            .merge(tuple_child_lifetime_ref, expression_lifetime_ref);

                        lifetime_scope.collect();
                    }

                    let tuple_lifetime_tree = lifetime_scope
                        .instance
                        .get_lifetime_tree(tuple_lifetime_ref);
                    tuple_lifetime_tree.is_alloc_point = true;

                    lifetime_scope
                        .instance
                        .merge(ast_lifetime_ref, tuple_lifetime_ref);
                }
            }
        }
        SimplePrimary::Identifier(literal) => {
            // local variable
            if let Some(resolved) = name_resolved_map.get(&EntityID::from(literal)) {
                if resolved.define_info.is_static_element
                    || resolved.define_info.define_kind == DefineKind::Import
                {
                    // static element
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
                    println!("{} : static", literal.value);

                    *variable_type = VariableType::StaticOrCapturedVariable;
                } else {
                    // add as captured variable
                    let closure_count = resolved
                        .separators
                        .iter()
                        .filter(|separator| separator.value == EnvironmentSeparatorKind::Closure)
                        .count();

                    if closure_count != 0 {
                        // beyond closure
                        let index = closure_scope_stack
                            .len()
                            .checked_sub(closure_count)
                            .unwrap_or(0);

                        let closure_scope = closure_scope_stack.get_mut(index).unwrap();
                        closure_scope
                            .captured
                            .push((resolved.define_info.entity_id, literal.value.to_string()));

                        let captured_ref = lifetime_scope.instance.get_or_create_child(
                            lifetime_scope.instance.this_argument_lifetime_ref,
                            literal.value,
                        );
                        let borrowed_ref = lifetime_scope
                            .instance
                            .create_entity_lifetime_tree(EntityID::from(literal));
                        let borrowed_tree = lifetime_scope.instance.get_lifetime_tree(borrowed_ref);
                        borrowed_tree.borrow_ref.insert(captured_ref);

                        lifetime_scope
                            .instance
                            .merge(ast_lifetime_ref, borrowed_ref);

                        *variable_type = VariableType::StaticOrCapturedVariable;
                    } else {
                        // NOT beyond closure
                        let kind = resolved.define_info.define_kind;

                        if kind == DefineKind::Variable || kind == DefineKind::FunctionArgument {
                            let variable_entity_id = resolved.define_info.entity_id;
                            let variable_define_lifetime_ref = lifetime_scope
                                .instance
                                .get_entity_lifetime_tree_ref(variable_entity_id);

                            let variable_borrow_lifetime_ref = lifetime_scope
                                .instance
                                .create_entity_lifetime_tree(EntityID::from(literal));
                            let variable_borrow_lifetime_tree = lifetime_scope
                                .instance
                                .get_lifetime_tree(variable_borrow_lifetime_ref);
                            variable_borrow_lifetime_tree
                                .borrow_ref
                                .insert(variable_define_lifetime_ref);

                            lifetime_scope
                                .instance
                                .merge(ast_lifetime_ref, variable_borrow_lifetime_ref);

                            *variable_type = VariableType::LocalVariable;
                        }
                    }
                }
            }
        }
        SimplePrimary::ThisKeyword(literal) => {
            let this_lifetime_ref = lifetime_scope.instance.this_argument_lifetime_ref;

            let borrowed_this_lifetime_ref = lifetime_scope
                .instance
                .create_entity_lifetime_tree(EntityID::from(literal));
            let borrowed_this_lifetime_tree = lifetime_scope
                .instance
                .get_lifetime_tree(borrowed_this_lifetime_ref);
            borrowed_this_lifetime_tree
                .borrow_ref
                .insert(this_lifetime_ref);

            lifetime_scope
                .instance
                .merge(ast_lifetime_ref, borrowed_this_lifetime_ref);
        }
        _ => {}
    }

    ast_lifetime_ref
}

fn collect_lifetime_function_call<'allocator>(
    ast: &'allocator FunctionCall<'_, 'allocator>,
    call_target_entity_id: EntityID,
    this_lifetime_ref: Option<LifetimeTreeRef>,
    return_value_tree_ref: LifetimeTreeRef,
    closure_scope_stack: &mut Vec<ClosureScope>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_scope: &mut LifetimeScope,
    stack_lifetime_scope: &mut StackLifetimeScope,
    lifetime_source_map: &mut FxHashMap<EntityID, LifetimeSource>,
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

        if function_info.define_info.is_closure {
            let closure_lifetime_ref = lifetime_scope
                .instance
                .get_entity_lifetime_tree_ref(call_target_entity_id);

            arguments.push(closure_lifetime_ref);
        }

        if function_info.is_extension {
            if let Some(this_lifetime_ref) = this_lifetime_ref {
                arguments.push(this_lifetime_ref);
            }
        }

        if let Ok(arg_exprs) = &ast.arg_exprs {
            for argument in arg_exprs.iter() {
                let argument_lifetime_ref = lifetime_scope
                    .instance
                    .create_entity_lifetime_tree(EntityID::from(*argument));

                {
                    let mut lifetime_scope = LifetimeScope::new(lifetime_scope.instance, allocator);

                    collect_lifetime_expression(
                        argument,
                        true,
                        false,
                        Some(argument_lifetime_ref),
                        return_value_tree_ref,
                        closure_scope_stack,
                        &mut VariableType::NotVariable,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        type_inference_result,
                        &mut lifetime_scope,
                        stack_lifetime_scope,
                        lifetime_source_map,
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

    let return_value_lifetime_tree = lifetime_scope.instance.get_lifetime_tree(return_value);
    return_value_lifetime_tree.is_alloc_point = true;
    return_value_lifetime_tree.contains_function_return_value = true;

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

    let return_value_tree = lifetime_scope.instance.get_lifetime_tree(return_value_ref);
    return_value_tree.contains_function_return_value = true;
    return_value_tree.is_alloc_point = true;

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
