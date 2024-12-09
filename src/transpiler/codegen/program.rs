use std::ops::Range;

use allocator_api2::vec::Vec;
use bumpalo::{collections::String, format, Bump};
use catla_parser::parser::{
    AddOrSubExpression, AddOrSubOp, AddOrSubOpKind, AndExpression, CompareExpression, CompareOp,
    CompareOpKind, Expression, ExpressionEnum, Factor, FunctionCall, MulOrDivExpression,
    MulOrDivOp, MulOrDivOpKind, OrExpression, Primary, PrimaryLeft, PrimaryLeftExpr, PrimaryRight,
    Program, SimplePrimary, Spanned, StatementAST, TranspilerTag,
};
use either::Either;
use fxhash::{FxHashMap, FxHashSet};
use regex::Regex;

use crate::transpiler::{
    component::EntityID,
    context::TranspileModuleContext,
    name_resolver::{DefineKind, FoundDefineInfo},
    optimizer::lifetime_analyzer::{LifetimeAnalyzeResult, LifetimeAnalyzeResults},
    semantics::types::{
        import_module_collector::get_module_name_from_primary,
        type_inference::{ImplicitConvertKind, TypeInferenceResultContainer},
        type_info::Type,
    },
    TranspileError,
};

use super::{
    custom::rust_codegen::{rust_codegen_function, rust_codegen_user_type},
    CodeBuilder, StackAllocCodeBuilder,
};

fn create_temp_var<'allocator>(
    code_builder: &mut CodeBuilder<'allocator>,
    span: Range<usize>,
    entity_id: EntityID,
) -> String<'allocator> {
    create_temp_var_with_name(code_builder, span, entity_id, "")
}

fn create_temp_var_with_name<'allocator>(
    code_builder: &mut CodeBuilder<'allocator>,
    span: Range<usize>,
    entity_id: EntityID,
    name: &str,
) -> String<'allocator> {
    let temp_var_name = format!(
        in code_builder.allocator,
        "temp_{}_{}_{}_{}",
        span.start,
        span.end,
        entity_id.ptr,
        name,
    );

    code_builder.add(format!(in code_builder.allocator, "let {};", &temp_var_name));

    temp_var_name
}

fn build_drop<'allocator>(
    drop_target: &str,
    ty: &Type,
    lifetime_analyze_result: LifetimeAnalyzeResult,
    code_builder: &mut CodeBuilder<'allocator>,
) {
    if let Type::Function {
        function_info,
        generics: _,
    } = ty
    {
        if !function_info.define_info.is_closure {
            return;
        }
    }

    if !lifetime_analyze_result.contains_static && !lifetime_analyze_result.is_alloc_point {
        return;
    }

    let drop_func_name = if lifetime_analyze_result.contains_static {
        "drop_ref"
    } else {
        "drop_as_unique"
    };

    let code = format!(
        in code_builder.allocator,
        "{}.{}();",
        drop_target,
        drop_func_name,
    );
    code_builder.add(code);
}

pub const RUST_CODEGEN_TAG: &str = "rust_codegen";

fn contains_rust_codegen_tag(tags: &Vec<&TranspilerTag, &Bump>) -> bool {
    for tag in tags.iter() {
        if let Ok(literal) = &tag.literal {
            if literal.value == RUST_CODEGEN_TAG {
                return true;
            }
        }
    }
    false
}

pub(crate) fn add_auto_import<'allocator>(
    code_builder: &mut CodeBuilder<'allocator>,
    allocator: &'allocator Bump,
    context: &TranspileModuleContext,
) {
    let mut module_element_map = FxHashMap::default();

    for module_name in context.context.auto_import.auto_import_modules.iter() {
        module_element_map.insert(module_name.clone(), FxHashSet::default());
    }

    for (element_name, module_name) in context.context.auto_import.auto_import_elements.iter() {
        let set = module_element_map
            .entry(module_name.clone())
            .or_insert_with(|| FxHashSet::default());
        set.insert(element_name.clone());
    }

    let regex = Regex::new(r"^std::").unwrap();

    for (module_name, element_names) in module_element_map {
        let replace = if context.module_name.starts_with("std::") {
            "crate::"
        } else {
            "catla_std::"
        };

        let module_name = regex.replace(module_name.as_str(), replace).to_string();

        if element_names.is_empty() {
            let code = format!(
                in allocator,
                "use {};",
                module_name,
            );
            code_builder.add(code);
        } else {
            let code = format!(
                in allocator,
                "use {}::{{{}}};",
                module_name,
                element_names.into_iter().collect::<Vec<_>>().join(", "),
            );
            code_builder.add(code);
        }
    }

    code_builder.add(String::from_str_in(
        "use catla_transpile_std::memory::CatlaRefObject;",
        allocator,
    ));
}

pub(crate) fn codegen_program<'allocator>(
    ast: Program,
    result_bind_var_name: Option<&str>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_analyze_results: &LifetimeAnalyzeResults,
    import_element_map: &FxHashMap<EntityID, Spanned<std::string::String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    user_type_field_builder: Option<&mut CodeBuilder<'allocator>>,
    top_stack_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    current_tree_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    mut code_builder: &mut CodeBuilder<'allocator>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    context: &TranspileModuleContext,
) {
    let mut transpiler_tags = Vec::new_in(allocator);

    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            Err(_) => continue,
        };

        match statement {
            StatementAST::Assignment(assignment) => {
                let temp_var_name = create_temp_var(
                    code_builder,
                    assignment.span.clone(),
                    EntityID::from(assignment),
                );

                codegen_expression(
                    assignment.right_expr.as_ref().unwrap(),
                    Some(temp_var_name.as_str()),
                    false,
                    type_inference_result,
                    lifetime_analyze_results,
                    import_element_map,
                    name_resolved_map,
                    top_stack_alloc_builder,
                    current_tree_alloc_builder,
                    code_builder,
                    allocator,
                    errors,
                    context,
                );

                codegen_expression(
                    assignment.left_expr,
                    Some(temp_var_name.as_str()),
                    true,
                    type_inference_result,
                    lifetime_analyze_results,
                    import_element_map,
                    name_resolved_map,
                    top_stack_alloc_builder,
                    current_tree_alloc_builder,
                    code_builder,
                    allocator,
                    errors,
                    context,
                );
            }
            StatementAST::Exchange(exchange) => {}
            StatementAST::Import(import) => {}
            StatementAST::StatementAttributes(_) => { /* do nothing */ }
            StatementAST::VariableDefine(variable_define) => {}
            StatementAST::FunctionDefine(function_define) => {
                if contains_rust_codegen_tag(&transpiler_tags) {
                    rust_codegen_function(
                        function_define,
                        &transpiler_tags,
                        result_bind_var_name,
                        type_inference_result,
                        lifetime_analyze_results,
                        import_element_map,
                        name_resolved_map,
                        top_stack_alloc_builder,
                        current_tree_alloc_builder,
                        code_builder,
                        allocator,
                        errors,
                        context,
                    );
                } else {
                    // TODO : add arguments, return type, etc...
                    let code = format!(
                        in allocator,
                        "pub fn {}() {{",
                        function_define.name.as_ref().unwrap().value,
                    );
                    code_builder.add(code);

                    if let Either::Right(block) =
                        function_define.block_or_semicolon.value.as_ref().unwrap()
                    {
                        let parent_code_builder = &mut code_builder;
                        let mut code_builder = parent_code_builder.fork();

                        code_builder.push_indent();

                        let mut stack_alloc_code_builder = code_builder.fork();
                        let mut top_stack_alloc_builder =
                            StackAllocCodeBuilder::new(&mut stack_alloc_code_builder);

                        let mut current_tree_alloc_code_builder = code_builder.fork();
                        let mut current_tree_alloc_builder =
                            StackAllocCodeBuilder::new(&mut current_tree_alloc_code_builder);

                        codegen_program(
                            block.program,
                            None,
                            type_inference_result,
                            lifetime_analyze_results,
                            import_element_map,
                            name_resolved_map,
                            None,
                            &mut top_stack_alloc_builder,
                            &mut current_tree_alloc_builder,
                            &mut code_builder,
                            allocator,
                            errors,
                            context,
                        );

                        code_builder.pull(current_tree_alloc_code_builder);
                        code_builder.pull(stack_alloc_code_builder);

                        parent_code_builder.pull(code_builder);
                    }

                    code_builder.add(String::from_str_in("}", allocator));
                }
            }
            StatementAST::UserTypeDefine(user_type_define) => {
                if contains_rust_codegen_tag(&transpiler_tags) {
                    rust_codegen_user_type(
                        user_type_define,
                        &transpiler_tags,
                        result_bind_var_name,
                        type_inference_result,
                        lifetime_analyze_results,
                        import_element_map,
                        name_resolved_map,
                        top_stack_alloc_builder,
                        current_tree_alloc_builder,
                        code_builder,
                        allocator,
                        errors,
                        context,
                    );
                }
            }
            StatementAST::TypeDefine(type_define) => {}
            StatementAST::Implements(implements) => {}
            StatementAST::DropStatement(drop_statement) => {}
            StatementAST::Expression(expression) => {
                let mut current_tree_alloc_code_builder = code_builder.fork();
                let mut current_tree_alloc_builder =
                    StackAllocCodeBuilder::new(&mut current_tree_alloc_code_builder);

                codegen_expression(
                    expression,
                    None,
                    false,
                    type_inference_result,
                    lifetime_analyze_results,
                    import_element_map,
                    name_resolved_map,
                    top_stack_alloc_builder,
                    &mut current_tree_alloc_builder,
                    code_builder,
                    allocator,
                    errors,
                    context,
                );

                code_builder.pull(current_tree_alloc_code_builder);
            }
            StatementAST::TranspilerTag(transpiler_tag) => {
                transpiler_tags.push(transpiler_tag);
            }
        }

        if let StatementAST::TranspilerTag(_) = statement {
        } else {
            transpiler_tags.clear();
        }
    }
}

fn codegen_expression<'allocator>(
    ast: Expression,
    result_bind_var_name: Option<&str>,
    as_assign_left: bool,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_analyze_results: &LifetimeAnalyzeResults,
    import_element_map: &FxHashMap<EntityID, Spanned<std::string::String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    top_stack_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    current_tree_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    code_builder: &mut CodeBuilder<'allocator>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    context: &TranspileModuleContext,
) {
    let parent_result_bind_var_name = result_bind_var_name;

    let expr_temp_var_name = create_temp_var_with_name(
        code_builder,
        ast.get_span(),
        EntityID::from(ast),
        "for_convert",
    );
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
                lifetime_analyze_results,
                import_element_map,
                name_resolved_map,
                top_stack_alloc_builder,
                current_tree_alloc_builder,
                code_builder,
                allocator,
                errors,
                context,
            );
        }
        ExpressionEnum::ReturnExpression(return_expression) => {
            if let Some(result_bind_var_name) = result_bind_var_name {
                code_builder
                    .add(format!(in allocator, "{} = unreachable!();", result_bind_var_name));
            }

            let temp_var_name = create_temp_var(
                code_builder,
                return_expression.span.clone(),
                EntityID::from(return_expression),
            );

            if let Some(expression) = return_expression.expression {
                codegen_expression(
                    expression,
                    Some(temp_var_name.as_str()),
                    as_assign_left,
                    type_inference_result,
                    lifetime_analyze_results,
                    import_element_map,
                    name_resolved_map,
                    top_stack_alloc_builder,
                    current_tree_alloc_builder,
                    code_builder,
                    allocator,
                    errors,
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
            lifetime_analyze_results: &LifetimeAnalyzeResults,
            import_element_map: &FxHashMap<EntityID, Spanned<std::string::String>>,
            name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
            top_stack_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
            current_tree_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
            code_builder: &mut CodeBuilder<'allocator>,
            allocator: &'allocator Bump,
            errors: &mut Vec<TranspileError>,
            context: &TranspileModuleContext,
        ) {
            if ast.right_exprs.is_empty() {
                $next_layer_function_name(
                    &ast.left_expr,
                    result_bind_var_name,
                    as_assign_left,
                    type_inference_result,
                    lifetime_analyze_results,
                    import_element_map,
                    name_resolved_map,
                    top_stack_alloc_builder,
                    current_tree_alloc_builder,
                    code_builder,
                    allocator,
                    errors,
                    context,
                );
            } else {
                let left_expr_temp_var_name = create_temp_var(code_builder, ast.left_expr.span.clone(), EntityID::from(&ast.left_expr));

                $next_layer_function_name(
                    &ast.left_expr,
                    Some(left_expr_temp_var_name.as_str()),
                    as_assign_left,
                    type_inference_result,
                    lifetime_analyze_results,
                    import_element_map,
                    name_resolved_map,
                    top_stack_alloc_builder,
                    current_tree_alloc_builder,
                    code_builder,
                    allocator,
                    errors,
                    context,
                );

                let mut last_result_bind_var_name = left_expr_temp_var_name;
                for (operator, right_expr) in ast.right_exprs.iter() {
                    let right_expr = right_expr.as_ref().unwrap();

                    let operator_result_var_name = create_temp_var_with_name(code_builder, $operator_span_provider(operator), EntityID::from(ast), "operator");
                    let right_expr_temp_var_name = create_temp_var(code_builder, right_expr.span.clone(), EntityID::from(right_expr));

                    $next_layer_function_name(
                        right_expr,
                        Some(right_expr_temp_var_name.as_str()),
                        as_assign_left,
                        type_inference_result,
                        lifetime_analyze_results,
                        import_element_map,
                        name_resolved_map,
                        top_stack_alloc_builder,
                        current_tree_alloc_builder,
                        code_builder,
                        allocator,
                        errors,
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
    lifetime_analyze_results: &LifetimeAnalyzeResults,
    import_element_map: &FxHashMap<EntityID, Spanned<std::string::String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    top_stack_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    current_tree_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    code_builder: &mut CodeBuilder<'allocator>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
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
            lifetime_analyze_results,
            import_element_map,
            name_resolved_map,
            top_stack_alloc_builder,
            current_tree_alloc_builder,
            code_builder,
            allocator,
            errors,
            context,
        );
    }
}

fn codegen_primary<'allocator>(
    ast: &Primary,
    result_bind_var_name: Option<&str>,
    as_assign_left: bool,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_analyze_results: &LifetimeAnalyzeResults,
    import_element_map: &FxHashMap<EntityID, Spanned<std::string::String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    top_stack_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    current_tree_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    code_builder: &mut CodeBuilder<'allocator>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    context: &TranspileModuleContext,
) {
    let module_name_result =
        get_module_name_from_primary(ast, name_resolved_map, import_element_map, context);

    let mut current_primary_index = 0;

    let mut last_result_temp_var = String::new_in(allocator);
    let mut last_entity_id = EntityID::dummy();

    if let Some((module_name, primary_index)) = module_name_result {
        let right_primary = &ast.chain[primary_index];

        if let Some((literal, _, function_call)) = &right_primary.second_expr {
            if let Some(function_call) = function_call {
                let is_closure = type_inference_result
                    .get_entity_type(EntityID::from(literal))
                    .is_closure();

                let literal_value_temp = if is_closure {
                    let literal_value_temp = create_temp_var(
                        code_builder,
                        literal.span.clone(),
                        EntityID::from(literal),
                    );

                    let code = format!(
                        in allocator,
                        "{} = {}::{}.get();",
                        &literal_value_temp,
                        &module_name,
                        literal.value,
                    );
                    code_builder.add(code);

                    literal_value_temp
                } else {
                    format!(in allocator, "{}::{}", &module_name, literal.value)
                };

                let (argument_results, argument_drop_info) = codegen_function_call_arguments(
                    function_call,
                    type_inference_result,
                    lifetime_analyze_results,
                    import_element_map,
                    name_resolved_map,
                    top_stack_alloc_builder,
                    current_tree_alloc_builder,
                    code_builder,
                    allocator,
                    errors,
                    context,
                );

                let result_temp = create_temp_var(
                    code_builder,
                    function_call.span.clone(),
                    EntityID::from(function_call),
                );

                let mut arguments = String::new_in(allocator);
                for argument_result in argument_results.iter() {
                    arguments += argument_result.as_str();
                    arguments += ", ";
                }

                let code = format!(
                    in allocator,
                    "{} = {}({});",
                    &result_temp,
                    literal_value_temp,
                    arguments,
                );
                code_builder.add(code);

                for (argument_expr_result, (ty, lifetime_analyze_result)) in
                    argument_results.iter().zip(argument_drop_info.iter()).rev()
                {
                    build_drop(
                        argument_expr_result.as_str(),
                        *ty,
                        lifetime_analyze_result.clone(),
                        code_builder,
                    );
                }

                if is_closure {
                    build_drop(
                        literal_value_temp.as_str(),
                        type_inference_result.get_entity_type(EntityID::from(literal)),
                        lifetime_analyze_results.get_result(EntityID::from(literal)),
                        code_builder,
                    );
                }

                last_result_temp_var = result_temp;
                last_entity_id = EntityID::from(function_call);
            } else {
                let result_temp =
                    create_temp_var(code_builder, literal.span.clone(), EntityID::from(literal));

                let is_static_var = if let Type::Function {
                    function_info,
                    generics: _,
                } =
                    type_inference_result.get_entity_type(EntityID::from(literal))
                {
                    if function_info.define_info.is_closure {
                        true
                    } else {
                        false
                    }
                } else {
                    true
                };

                if is_static_var {
                    let code = format!(
                        in allocator,
                        "{} = {}::{}.get();",
                        &result_temp,
                        &module_name,
                        literal.value,
                    );
                    code_builder.add(code);
                } else {
                    let code = format!(
                        in allocator,
                        "{} = {}::{};",
                        &result_temp,
                        &module_name,
                        literal.value,
                    );
                    code_builder.add(code);
                }

                last_result_temp_var = result_temp;
                last_entity_id = EntityID::from(literal);
            }
        }

        // TODO : mapping operator

        current_primary_index = primary_index + 1;
    } else {
        let result_temp = create_temp_var(
            code_builder,
            ast.left.span.clone(),
            EntityID::from(&ast.left),
        );

        codegen_primary_left(
            &ast.left,
            result_temp.as_str(),
            false,
            type_inference_result,
            lifetime_analyze_results,
            import_element_map,
            name_resolved_map,
            top_stack_alloc_builder,
            current_tree_alloc_builder,
            code_builder,
            allocator,
            errors,
            context,
        );

        last_result_temp_var = result_temp;
        last_entity_id = EntityID::from(&ast.left);
    }

    loop {
        if current_primary_index >= ast.chain.len() {
            break;
        }

        let primary_right = &ast.chain[current_primary_index];

        if let Some((literal, _, function_call)) = &primary_right.second_expr {
            if let Some(function_call) = function_call {
                let is_closure = type_inference_result
                    .get_entity_type(EntityID::from(literal))
                    .is_closure();

                let literal_value_temp = if is_closure {
                    let literal_value_temp = create_temp_var(
                        code_builder,
                        literal.span.clone(),
                        EntityID::from(literal),
                    );

                    let contains_static = lifetime_analyze_results
                        .get_result(EntityID::from(literal))
                        .contains_static;

                    let getter_name = if contains_static { "get" } else { "borrow" };

                    let code = format!(
                        in allocator,
                        "{} = {}.{}_{}();",
                        &literal_value_temp,
                        &last_result_temp_var,
                        getter_name,
                        literal.value,
                    );
                    code_builder.add(code);

                    literal_value_temp
                } else {
                    format!(
                        in allocator,
                        "{}.{}",
                        &last_result_temp_var,
                        literal.value,
                    )
                };

                let (argument_results, argument_drop_info) = codegen_function_call_arguments(
                    function_call,
                    type_inference_result,
                    lifetime_analyze_results,
                    import_element_map,
                    name_resolved_map,
                    top_stack_alloc_builder,
                    current_tree_alloc_builder,
                    code_builder,
                    allocator,
                    errors,
                    context,
                );

                let result_temp = create_temp_var(
                    code_builder,
                    function_call.span.clone(),
                    EntityID::from(function_call),
                );

                let mut arguments = String::new_in(allocator);
                for argument_result in argument_results.iter() {
                    arguments += argument_result.as_str();
                    arguments += ", ";
                }

                let code = format!(
                    in allocator,
                    "{} = {}({});",
                    &result_temp,
                    literal_value_temp,
                    arguments,
                );
                code_builder.add(code);

                for (argument_expr_result, (ty, lifetime_analyze_result)) in
                    argument_results.iter().zip(argument_drop_info.iter()).rev()
                {
                    build_drop(
                        argument_expr_result.as_str(),
                        *ty,
                        lifetime_analyze_result.clone(),
                        code_builder,
                    );
                }

                if is_closure {
                    build_drop(
                        literal_value_temp.as_str(),
                        type_inference_result.get_entity_type(EntityID::from(literal)),
                        lifetime_analyze_results.get_result(EntityID::from(literal)),
                        code_builder,
                    );
                }

                build_drop(
                    &last_result_temp_var,
                    type_inference_result.get_entity_type(last_entity_id),
                    lifetime_analyze_results.get_result(last_entity_id),
                    code_builder,
                );

                last_result_temp_var = result_temp;
                last_entity_id = EntityID::from(function_call);
            } else {
                let literal_value_temp =
                    create_temp_var(code_builder, literal.span.clone(), EntityID::from(literal));

                let contains_static = lifetime_analyze_results
                    .get_result(EntityID::from(literal))
                    .contains_static;

                let getter_name = if contains_static { "get" } else { "borrow" };

                let code = format!(
                    in allocator,
                    "{} = {}.{}_{}();",
                    &literal_value_temp,
                    &last_result_temp_var,
                    getter_name,
                    literal.value,
                );
                code_builder.add(code);

                build_drop(
                    &last_result_temp_var,
                    type_inference_result.get_entity_type(last_entity_id),
                    lifetime_analyze_results.get_result(last_entity_id),
                    code_builder,
                );

                last_result_temp_var = literal_value_temp;
                last_entity_id = EntityID::from(literal);
            }
        }

        current_primary_index += 1;
    }

    if let Some(result_bind_var_name) = result_bind_var_name {
        let code = format!(
            in allocator,
            "{} = {};",
            result_bind_var_name,
            last_result_temp_var,
        );
        code_builder.add(code);
    } else {
        if !as_assign_left {
            build_drop(
                &last_result_temp_var,
                type_inference_result.get_entity_type(last_entity_id),
                lifetime_analyze_results.get_result(last_entity_id),
                code_builder,
            );
        }
    }
}

fn codegen_function_call_arguments<'allocator, 'ty>(
    ast: &FunctionCall,
    type_inference_result: &'ty TypeInferenceResultContainer,
    lifetime_analyze_results: &LifetimeAnalyzeResults,
    import_element_map: &FxHashMap<EntityID, Spanned<std::string::String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    top_stack_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    current_tree_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    code_builder: &mut CodeBuilder<'allocator>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    context: &TranspileModuleContext,
) -> (
    Vec<String<'allocator>, &'allocator Bump>,
    Vec<(&'ty Type, LifetimeAnalyzeResult), &'allocator Bump>,
) {
    let mut argument_results = Vec::new_in(allocator);
    let mut argument_drop_info = Vec::new_in(allocator);

    if let Ok(arguments) = &ast.arg_exprs {
        for argument_expr in arguments.iter() {
            let expr_result_bind_var = create_temp_var(
                code_builder,
                argument_expr.get_span(),
                EntityID::from(*argument_expr),
            );

            codegen_expression(
                *argument_expr,
                Some(expr_result_bind_var.as_str()),
                false,
                type_inference_result,
                lifetime_analyze_results,
                import_element_map,
                name_resolved_map,
                top_stack_alloc_builder,
                current_tree_alloc_builder,
                code_builder,
                allocator,
                errors,
                context,
            );

            argument_results.push(expr_result_bind_var);

            let ty = type_inference_result.get_entity_type(EntityID::from(*argument_expr));
            let lifetime_analyze_result =
                lifetime_analyze_results.get_result(EntityID::from(*argument_expr));

            argument_drop_info.push((ty, lifetime_analyze_result));
        }
    }

    (argument_results, argument_drop_info)
}

fn codegen_primary_left<'allocator>(
    ast: &PrimaryLeft,
    result_bind_var_name: &str,
    as_assign_left: bool,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_analyze_results: &LifetimeAnalyzeResults,
    import_element_map: &FxHashMap<EntityID, Spanned<std::string::String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    top_stack_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    current_tree_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    code_builder: &mut CodeBuilder<'allocator>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    context: &TranspileModuleContext,
) {
    match &ast.first_expr {
        PrimaryLeftExpr::Simple((simple_primary, _, function_call)) => {
            let simple_primary_result_temp = create_temp_var(
                code_builder,
                simple_primary.get_span(),
                EntityID::from(simple_primary),
            );

            match simple_primary {
                SimplePrimary::Expressions {
                    expressions,
                    error_tokens: _,
                    span: _,
                } => {
                    let mut expression_results = Vec::new_in(allocator);

                    for expression in expressions.iter() {
                        let expression_result_temp = create_temp_var(
                            code_builder,
                            expression.get_span(),
                            EntityID::from(*expression),
                        );

                        codegen_expression(
                            *expression,
                            Some(expression_result_temp.as_str()),
                            false,
                            type_inference_result,
                            lifetime_analyze_results,
                            import_element_map,
                            name_resolved_map,
                            top_stack_alloc_builder,
                            current_tree_alloc_builder,
                            code_builder,
                            allocator,
                            errors,
                            context,
                        );

                        expression_results.push(expression_result_temp);
                    }

                    let code = format!(
                        in allocator,
                        "{} = ({});",
                        &simple_primary_result_temp,
                        expression_results.join(", "),
                    );
                    code_builder.add(code);
                }
                SimplePrimary::Identifier(literal) => {
                    if let Some(resolved) = name_resolved_map.get(&EntityID::from(literal)) {
                        let is_static_element = resolved.define_info.is_static_element
                            || resolved.define_info.define_kind == DefineKind::Import;

                        let has_ref_count = if let Type::Function {
                            function_info,
                            generics: _,
                        } =
                            type_inference_result.get_entity_type(EntityID::from(&ast.first_expr))
                        {
                            if function_info.define_info.is_closure {
                                true
                            } else {
                                false
                            }
                        } else {
                            is_static_element
                        };

                        if has_ref_count {
                            let code = format!(
                                in allocator,
                                "{} = {}.get();",
                                &simple_primary_result_temp,
                                literal.value,
                            );
                            code_builder.add(code);
                        } else {
                            let code = format!(
                                in allocator,
                                "{} = {};",
                                &simple_primary_result_temp,
                                literal.value,
                            );
                            code_builder.add(code);
                        }
                    } else {
                        if let Some(module_name) = context
                            .context
                            .auto_import
                            .auto_import_elements
                            .get(literal.value)
                        {
                            let code = format!(
                                in allocator,
                                "{} = {}::{};",
                                &simple_primary_result_temp,
                                module_name,
                                literal.value,
                            );
                            code_builder.add(code);
                        } else {
                            let code = format!(
                                in allocator,
                                "{} = {};",
                                &simple_primary_result_temp,
                                literal.value,
                            );
                            code_builder.add(code);
                        }
                    }
                }
                SimplePrimary::StringLiteral(literal) => {
                    const STRING_TYPE: &str = "catla_transpile_std::rust_codegen::string::String";
                    const LAZY_LOCK_TYPE: &str = "std::sync::LazyLock";

                    let code = format!(
                        in allocator,
                        "static STRING_{}_{}: {}<&'static CatlaRefObject<{}>> = {}::new(|| {{ let str = {}::from_str({}); str.to_mutex(); str }});",
                        literal.span.start,
                        literal.span.end,
                        LAZY_LOCK_TYPE,
                        STRING_TYPE,
                        LAZY_LOCK_TYPE,
                        STRING_TYPE,
                        literal.value,
                    );
                    code_builder.add(code);

                    let code = format!(
                        in allocator,
                        "let str = &*STRING_{}_{}; str.clone_ref(); {} = str;",
                        literal.span.start,
                        literal.span.end,
                        &simple_primary_result_temp,
                    );
                    code_builder.add(code);
                }
                SimplePrimary::NullKeyword(_) => {
                    let code = format!(
                        in allocator,
                        "{} = None;",
                        &simple_primary_result_temp,
                    );
                    code_builder.add(code);
                }
                SimplePrimary::TrueKeyword(_) => {
                    let code = format!(
                        in allocator,
                        "{} = true;",
                        &simple_primary_result_temp,
                    );
                    code_builder.add(code);
                }
                SimplePrimary::FalseKeyword(_) => {
                    let code = format!(
                        in allocator,
                        "{} = false;",
                        &simple_primary_result_temp,
                    );
                    code_builder.add(code);
                }
                SimplePrimary::ThisKeyword(_) => {
                    let code = format!(
                        in allocator,
                        "{} = self;",
                        &simple_primary_result_temp,
                    );
                    code_builder.add(code);
                }
                SimplePrimary::LargeThisKeyword(_) => unreachable!(),
            }

            if let Some(function_call) = function_call {
                let (argument_results, argument_drop_info) = codegen_function_call_arguments(
                    function_call,
                    type_inference_result,
                    lifetime_analyze_results,
                    import_element_map,
                    name_resolved_map,
                    top_stack_alloc_builder,
                    current_tree_alloc_builder,
                    code_builder,
                    allocator,
                    errors,
                    context,
                );

                let result_temp = create_temp_var(
                    code_builder,
                    function_call.span.clone(),
                    EntityID::from(function_call),
                );

                let mut arguments = String::new_in(allocator);
                for argument_result in argument_results.iter() {
                    arguments += argument_result.as_str();
                    arguments += ", ";
                }

                let code = format!(
                    in allocator,
                    "{} = {}({});",
                    &result_temp,
                    &simple_primary_result_temp,
                    arguments,
                );
                code_builder.add(code);

                for (argument_expr_result, (ty, lifetime_analyze_result)) in
                    argument_results.iter().zip(argument_drop_info.iter()).rev()
                {
                    build_drop(
                        argument_expr_result.as_str(),
                        *ty,
                        lifetime_analyze_result.clone(),
                        code_builder,
                    );
                }

                if type_inference_result
                    .get_entity_type(EntityID::from(simple_primary))
                    .is_closure()
                {
                    build_drop(
                        simple_primary_result_temp.as_str(),
                        type_inference_result.get_entity_type(EntityID::from(simple_primary)),
                        lifetime_analyze_results.get_result(EntityID::from(simple_primary)),
                        code_builder,
                    );
                }

                let code = format!(
                    in allocator,
                    "{} = {};",
                    result_bind_var_name,
                    &result_temp
                );
                code_builder.add(code);
            } else {
                let code = format!(
                    in allocator,
                    "{} = {};",
                    result_bind_var_name,
                    &simple_primary_result_temp
                );
                code_builder.add(code);
            }
        }
        PrimaryLeftExpr::NewArrayInitExpression(new_array_init_expression) => {}
        PrimaryLeftExpr::NewArrayExpression(new_array_expression) => {}
        PrimaryLeftExpr::NewExpression(new_expression) => {}
        PrimaryLeftExpr::IfExpression(if_expression) => {}
        PrimaryLeftExpr::LoopExpression(loop_expression) => {}
    }
}
