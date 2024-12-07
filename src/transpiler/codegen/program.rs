use std::ops::Range;

use bumpalo::{collections::String, format, Bump};
use catla_parser::parser::{
    AddOrSubExpression, AddOrSubOp, AddOrSubOpKind, AndExpression, CompareExpression, CompareOp, CompareOpKind, Expression, ExpressionEnum, Factor, MulOrDivExpression, MulOrDivOp, MulOrDivOpKind, OrExpression, Primary, PrimaryRight, Program, Spanned, StatementAST
};

use crate::transpiler::{
    component::EntityID,
    context::TranspileModuleContext,
    semantics::types::{
        import_module_collector::get_module_name_from_primary,
        type_inference::{ImplicitConvertKind, TypeInferenceResultContainer},
        type_info::Type,
    },
};

use super::{CodeBuilder, StackAllocCodeBuilder};

fn create_temp_var<'allocator>(
    code_builder: &mut CodeBuilder<'allocator>,
    span: Range<usize>,
) -> String<'allocator> {
    let temp_var_name = format!(
        in code_builder.allocator,
        "temp_{}_{}",
        span.start,
        span.end
    );

    code_builder.add(format!(in code_builder.allocator, "let {};", &temp_var_name));

    temp_var_name
}

fn build_drop<'allocator>(
    drop_target: &str,
    drop_type: &Type,
    code_builder: &mut CodeBuilder<'allocator>,
) {
}

pub fn codegen_program<'allocator>(
    ast: Program,
    result_bind_var_name: Option<&str>,
    type_inference_result: &TypeInferenceResultContainer,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    user_type_field_builder: Option<&mut CodeBuilder<'allocator>>,
    top_stack_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    current_tree_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    code_builder: &mut CodeBuilder<'allocator>,
    allocator: &'allocator Bump,
    context: &TranspileModuleContext,
) {
    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            Err(_) => continue,
        };

        let mut top_stack_alloc_code_builder = code_builder.fork();
        let mut top_stack_alloc_builder =
            StackAllocCodeBuilder::new(&mut top_stack_alloc_code_builder);

        match statement {
            StatementAST::Assignment(assignment) => {
                let temp_var_name = create_temp_var(code_builder, assignment.span.clone());

                codegen_expression(
                    assignment.right_expr.as_ref().unwrap(),
                    Some(temp_var_name.as_str()),
                    false,
                    type_inference_result,
                    import_element_map,
                    name_resolved_map,
                    &mut top_stack_alloc_builder,
                    current_tree_alloc_builder,
                    code_builder,
                    allocator,
                    context,
                );

                codegen_expression(
                    assignment.left_expr,
                    Some(temp_var_name.as_str()),
                    true,
                    type_inference_result,
                    import_element_map,
                    name_resolved_map,
                    &mut top_stack_alloc_builder,
                    current_tree_alloc_builder,
                    code_builder,
                    allocator,
                    context,
                );
            }
            StatementAST::Exchange(exchange) => {}
            StatementAST::Import(import) => {}
            StatementAST::StatementAttributes(_) => { /* do nothing */ }
            StatementAST::VariableDefine(variable_define) => todo!(),
            StatementAST::FunctionDefine(function_define) => todo!(),
            StatementAST::UserTypeDefine(user_type_define) => todo!(),
            StatementAST::TypeDefine(type_define) => todo!(),
            StatementAST::Implements(implements) => todo!(),
            StatementAST::DropStatement(drop_statement) => todo!(),
            StatementAST::Expression(_) => todo!(),
        }

        code_builder.pull(top_stack_alloc_code_builder);
    }
}

fn codegen_expression<'allocator>(
    ast: Expression,
    result_bind_var_name: Option<&str>,
    as_assign_left: bool,
    type_inference_result: &TypeInferenceResultContainer,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    top_stack_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    current_tree_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    code_builder: &mut CodeBuilder<'allocator>,
    allocator: &'allocator Bump,
    context: &TranspileModuleContext,
) {
    let parent_result_bind_var_name = result_bind_var_name;

    let expr_temp_var_name = create_temp_var(code_builder, ast.get_span());
    let result_bind_var_name = if parent_result_bind_var_name.is_some() {
        Some(expr_temp_var_name.as_str())
    } else {
        None
    };

    match ast {
        ExpressionEnum::OrExpression(or_expression) => {
            codegen_or_expression(
                or_expression,
                result_bind_var_name,
                as_assign_left,
                type_inference_result,
                import_element_map,
                name_resolved_map,
                top_stack_alloc_builder,
                current_tree_alloc_builder,
                code_builder,
                allocator,
                context,
            );
        }
        ExpressionEnum::ReturnExpression(return_expression) => {
            if let Some(result_bind_var_name) = result_bind_var_name {
                code_builder
                    .add(format!(in allocator, "{} = unreachable!();", result_bind_var_name));
            }

            let temp_var_name = create_temp_var(code_builder, return_expression.span.clone());

            if let Some(expression) = return_expression.expression {
                codegen_expression(
                    expression,
                    Some(temp_var_name.as_str()),
                    as_assign_left,
                    type_inference_result,
                    import_element_map,
                    name_resolved_map,
                    top_stack_alloc_builder,
                    current_tree_alloc_builder,
                    code_builder,
                    allocator,
                    context,
                );
            } else {
                code_builder.add(format!(in allocator, "{} = ()", &temp_var_name));
            }

            code_builder.add(format!(in allocator, "return {};", &temp_var_name));
        }
        ExpressionEnum::Closure(closure) => {}
    }

    // implicit convert
    if let Some(result_bind_var_name) = result_bind_var_name {
        if let Some(convert) = type_inference_result
            .implicit_convert_map
            .get(&EntityID::from(ast))
        {
            let code = match convert {
                ImplicitConvertKind::Some => {
                    format!(
                        in allocator,
                        "{} = Some({})",
                        parent_result_bind_var_name.unwrap(),
                        result_bind_var_name
                    )
                }
                ImplicitConvertKind::Ok => {
                    format!(
                        in allocator,
                        "{} = Ok({})",
                        parent_result_bind_var_name.unwrap(),
                        result_bind_var_name
                    )
                }
                ImplicitConvertKind::Error => {
                    format!(
                        in allocator,
                        "{} = Err({})",
                        parent_result_bind_var_name.unwrap(),
                        result_bind_var_name
                    )
                }
            };
            code_builder.add(code);
        } else {
            let code = format!(
                in allocator,
                "{} = {}",
                parent_result_bind_var_name.unwrap(),
                result_bind_var_name
            );
            code_builder.add(code);
        }
    }
}

macro_rules! codegen_for_op2 {
    (
        $function_name:ident,
        $ast_type:ident,
        $next_layer_function_name:ident,
        $operator_span_provider:expr,
        $method_name_provider:expr
    ) => {
        fn $function_name<'allocator>(
            ast: &$ast_type,
            result_bind_var_name: Option<&str>,
            as_assign_left: bool,
            type_inference_result: &TypeInferenceResultContainer,
            import_element_map: &FxHashMap<EntityID, Spanned<String>>,
            name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
            top_stack_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
            current_tree_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
            code_builder: &mut CodeBuilder<'allocator>,
            allocator: &'allocator Bump,
            context: &TranspileModuleContext,
        ) {
            if ast.right_exprs.is_empty() {
                $next_layer_function_name(
                    &ast.left_expr,
                    result_bind_var_name,
                    as_assign_left,
                    type_inference_result,
                    import_element_map,
                    name_resolved_map,
                    top_stack_alloc_builder,
                    current_tree_alloc_builder,
                    code_builder,
                    allocator,
                    context,
                );
            } else {
                let left_expr_temp_var_name = create_temp_var(code_builder, ast.left_expr.span.clone());

                $next_layer_function_name(
                    &ast.left_expr,
                    Some(left_expr_temp_var_name.as_str()),
                    as_assign_left,
                    type_inference_result,
                    import_element_map,
                    name_resolved_map,
                    top_stack_alloc_builder,
                    current_tree_alloc_builder,
                    code_builder,
                    allocator,
                    context,
                );

                let mut last_result_bind_var_name = left_expr_temp_var_name;
                for (operator, right_expr) in ast.right_exprs.iter() {
                    let right_expr = right_expr.as_ref().unwrap();

                    let operator_result_var_name = create_temp_var(code_builder, $operator_span_provider(operator));
                    let right_expr_temp_var_name = create_temp_var(code_builder, right_expr.span.clone());

                    $next_layer_function_name(
                        right_expr,
                        Some(right_expr_temp_var_name.as_str()),
                        as_assign_left,
                        type_inference_result,
                        import_element_map,
                        name_resolved_map,
                        top_stack_alloc_builder,
                        current_tree_alloc_builder,
                        code_builder,
                        allocator,
                        context,
                    );

                    if let Some(convert) = type_inference_result
                        .implicit_convert_map
                        .get(&EntityID::from(right_expr))
                    {
                        let convert_name = match convert {
                            ImplicitConvertKind::Some => "Some",
                            ImplicitConvertKind::Ok => "Ok",
                            ImplicitConvertKind::Error => "Err",
                        };
                        code_builder.add(format!(
                            in allocator,
                            "{} = {}.{}({}({}));",
                            &operator_result_var_name,
                            &last_result_bind_var_name,
                            $method_name_provider(operator),
                            convert_name,
                            &right_expr_temp_var_name
                        ));
                    } else {
                        code_builder.add(format!(
                            in allocator,
                            "{} = {}.{}({});",
                            &operator_result_var_name,
                            &last_result_bind_var_name,
                            $method_name_provider(operator),
                            &right_expr_temp_var_name
                        ));
                    }

                    last_result_bind_var_name = operator_result_var_name;
                }

                if let Some(result_bind_var_name) = result_bind_var_name {
                    code_builder.add(
                        format!(in allocator, "{} = {}", result_bind_var_name, &last_result_bind_var_name),
                    );
                } else {
                    code_builder.add(format!(in allocator, "{}.drop();", &last_result_bind_var_name));
                }
            }
        }
    };
}

codegen_for_op2!(
    codegen_or_expression,
    OrExpression,
    codegen_and_expression,
    |operator: &Range<usize>| { operator.clone() },
    |_| { "or" }
);

codegen_for_op2!(
    codegen_and_expression,
    AndExpression,
    codegen_compare_expression,
    |operator: &Range<usize>| { operator.clone() },
    |_| { "and" }
);

codegen_for_op2!(
    codegen_compare_expression,
    CompareExpression,
    codegen_add_or_sub_expression,
    |operator: &CompareOp| { operator.span.clone() },
    |operator: &CompareOp| {
        match operator.value {
            CompareOpKind::GreaterThan => "greater_than",
            CompareOpKind::GreaterOrEqual => "greater_or_equals",
            CompareOpKind::LessThan => "less_than",
            CompareOpKind::LessOrEqual => "less_or_equals",
            CompareOpKind::Equal => "equals",
            CompareOpKind::NotEqual => "not_equals",
        }
    }
);

codegen_for_op2!(
    codegen_add_or_sub_expression,
    AddOrSubExpression,
    codegen_mul_or_div_expression,
    |operator: &AddOrSubOp| { operator.span.clone() },
    |operator: &AddOrSubOp| {
        match operator.value {
            AddOrSubOpKind::Add => "add",
            AddOrSubOpKind::Sub => "sub",
        }
    }
);

codegen_for_op2!(
    codegen_mul_or_div_expression,
    MulOrDivExpression,
    codegen_factor,
    |operator: &MulOrDivOp| { operator.span.clone() },
    |operator: &MulOrDivOp| {
        match operator.value {
            MulOrDivOpKind::Mul => "mul",
            MulOrDivOpKind::Div => "div",
        }
    }
);

fn codegen_factor<'allocator>(
    ast: &Factor,
    result_bind_var_name: Option<&str>,
    as_assign_left: bool,
    type_inference_result: &TypeInferenceResultContainer,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    top_stack_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    current_tree_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    code_builder: &mut CodeBuilder<'allocator>,
    allocator: &'allocator Bump,
    context: &TranspileModuleContext,
) {
    // TODO : impl negative interface
    if ast.negative_keyword_span.is_some() {
    } else {
        codegen_primary(
            ast.primary.as_ref().unwrap(),
            result_bind_var_name,
            as_assign_left,
            type_inference_result,
            import_element_map,
            name_resolved_map,
            top_stack_alloc_builder,
            current_tree_alloc_builder,
            code_builder,
            allocator,
            context,
        );
    }
}

fn codegen_primary<'allocator>(
    ast: &Primary,
    result_bind_var_name: Option<&str>,
    as_assign_left: bool,
    type_inference_result: &TypeInferenceResultContainer,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    top_stack_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    current_tree_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    code_builder: &mut CodeBuilder<'allocator>,
    allocator: &'allocator Bump,
    context: &TranspileModuleContext,
) {
    let module_name_result =
        get_module_name_from_primary(ast, name_resolved_map, import_element_map, context);

    let mut current_primary_index = 0;

    if let Some((module_name, primary_index)) = module_name_result {
        let right_primary = &ast.chain[primary_index];

        current_primary_index = primary_index + 1;
    } else {
    }
}

fn codegen_primary_left<'allocator>(
    ast: &PrimaryRight,
    result_bind_var_name: Option<&str>,
    as_assign_left: bool,
    type_inference_result: &TypeInferenceResultContainer,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    top_stack_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    current_tree_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    code_builder: &mut CodeBuilder<'allocator>,
    allocator: &'allocator Bump,
    context: &TranspileModuleContext,
) {
}
