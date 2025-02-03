use std::sync::Arc;

use catla_parser::parser::{
    AddOrSubExpression, AndExpression, CompareExpression, Expression, ExpressionEnum, Factor,
    MappingOperator, MulOrDivExpression, OrExpression, Primary, PrimaryLeft, PrimaryLeftExpr,
    PrimaryRight, Program, StatementAST,
};
use either::Either;

use crate::transpiler::{
    component::EntityID,
    context::TranspileModuleContext,
    semantics::types::{type_inference::TypeInferenceResultContainer, type_info::Type},
};

use super::ModuleFunctionCallInfo;

pub fn collect_function_call(
    ast: Program,
    type_inference_result: &TypeInferenceResultContainer,
    context: &TranspileModuleContext,
) {
    let mut module_function_call_info = ModuleFunctionCallInfo::new();

    collect_function_call_program(
        ast,
        type_inference_result,
        &mut Vec::new(),
        &mut module_function_call_info,
        context,
    );

    context
        .context
        .function_recursive_info
        .register(context.module_name.clone(), module_function_call_info);
}

fn collect_function_call_program(
    ast: Program,
    type_inference_result: &TypeInferenceResultContainer,
    function_calls: &mut Vec<(Arc<String>, EntityID)>,
    module_function_call_info: &mut ModuleFunctionCallInfo,
    context: &TranspileModuleContext,
) {
    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            Err(_) => continue,
        };

        match statement {
            StatementAST::Assignment(assignment) => {
                collect_function_call_expression(
                    assignment.left_expr,
                    type_inference_result,
                    function_calls,
                    module_function_call_info,
                    context,
                );

                if let Ok(right_expr) = assignment.right_expr {
                    collect_function_call_expression(
                        right_expr,
                        type_inference_result,
                        function_calls,
                        module_function_call_info,
                        context,
                    );
                }
            }
            StatementAST::Exchange(exchange) => {
                collect_function_call_expression(
                    exchange.left_expr,
                    type_inference_result,
                    function_calls,
                    module_function_call_info,
                    context,
                );

                if let Ok(right_expr) = exchange.right_expr {
                    collect_function_call_expression(
                        right_expr,
                        type_inference_result,
                        function_calls,
                        module_function_call_info,
                        context,
                    );
                }
            }
            StatementAST::Import(_) => {}
            StatementAST::StatementAttributes(_) => todo!(),
            StatementAST::VariableDefine(variable_define) => {
                if let Some(expression) = &variable_define.expression {
                    if let Ok(expression) = expression {
                        collect_function_call_expression(
                            *expression,
                            type_inference_result,
                            function_calls,
                            module_function_call_info,
                            context,
                        );
                    }
                }
            }
            StatementAST::FunctionDefine(function_define) => {
                let mut function_calls = Vec::new();

                if let Some(block) = &function_define.block_or_semicolon.value {
                    if let Either::Right(block) = block {
                        collect_function_call_program(
                            block.program,
                            type_inference_result,
                            &mut function_calls,
                            module_function_call_info,
                            context,
                        );
                    }
                }

                module_function_call_info.register(EntityID::from(function_define), function_calls);
            }
            StatementAST::UserTypeDefine(user_type_define) => {
                if let Some(block) = &user_type_define.block.value {
                    collect_function_call_program(
                        block.program,
                        type_inference_result,
                        function_calls,
                        module_function_call_info,
                        context,
                    );
                }
            }
            StatementAST::TypeDefine(_) => {}
            StatementAST::Implements(implements) => {
                if let Some(block) = &implements.block.value {
                    collect_function_call_program(
                        block.program,
                        type_inference_result,
                        function_calls,
                        module_function_call_info,
                        context,
                    );
                }
            }
            StatementAST::DropStatement(drop_statement) => {
                if let Ok(expression) = drop_statement.expression {
                    collect_function_call_expression(
                        expression,
                        type_inference_result,
                        function_calls,
                        module_function_call_info,
                        context,
                    );
                }
            }
            StatementAST::Expression(expression) => {
                collect_function_call_expression(
                    *expression,
                    type_inference_result,
                    function_calls,
                    module_function_call_info,
                    context,
                );
            }
            StatementAST::TranspilerTag(_) => {}
        }
    }
}

fn collect_function_call_expression(
    ast: Expression,
    type_inference_result: &TypeInferenceResultContainer,
    function_calls: &mut Vec<(Arc<String>, EntityID)>,
    module_function_call_info: &mut ModuleFunctionCallInfo,
    context: &TranspileModuleContext,
) {
    match ast {
        ExpressionEnum::OrExpression(or_expression) => {
            collect_function_call_or_expression(
                or_expression,
                type_inference_result,
                function_calls,
                module_function_call_info,
                context,
            );
        }
        ExpressionEnum::ReturnExpression(return_expression) => {
            if let Some(expression) = return_expression.expression {
                collect_function_call_expression(
                    expression,
                    type_inference_result,
                    function_calls,
                    module_function_call_info,
                    context,
                );
            }
        }
        ExpressionEnum::Closure(closure) => {
            let mut function_calls = Vec::new();

            if let Some(expression_or_block) = &closure.expression_or_block.value {
                match expression_or_block {
                    Either::Left(expression) => {
                        collect_function_call_expression(
                            *expression,
                            type_inference_result,
                            &mut function_calls,
                            module_function_call_info,
                            context,
                        );
                    }
                    Either::Right(block) => {
                        collect_function_call_program(
                            block.program,
                            type_inference_result,
                            &mut function_calls,
                            module_function_call_info,
                            context,
                        );
                    }
                }
            }

            module_function_call_info.register(EntityID::from(closure), function_calls);
        }
    }
}

macro_rules! collect_function_call_for_op2 {
    ($function_name:ident, $ast_type:ident, $next_layer_function_name:ident) => {
        fn $function_name(
            ast: &$ast_type,
            type_inference_result: &TypeInferenceResultContainer,
            function_calls: &mut Vec<(Arc<String>, EntityID)>,
            module_function_call_info: &mut ModuleFunctionCallInfo,
            context: &TranspileModuleContext,
        ) {
            $next_layer_function_name(
                &ast.left_expr,
                type_inference_result,
                function_calls,
                module_function_call_info,
                context,
            );

            for (_, right_expr) in ast.right_exprs.iter() {
                if let Ok(right_expr) = right_expr {
                    let operator_function_type = type_inference_result
                        .operator_function_type_map
                        .get(&EntityID::from(right_expr))
                        .unwrap();

                    if let Type::Function {
                        function_info,
                        generics: _,
                    } = operator_function_type
                    {
                        function_calls.push((
                            function_info.define_info.module_name.clone(),
                            function_info.define_info.entity_id,
                        ));
                    }

                    $next_layer_function_name(
                        right_expr,
                        type_inference_result,
                        function_calls,
                        module_function_call_info,
                        context,
                    );
                }
            }
        }
    };
}

collect_function_call_for_op2!(
    collect_function_call_or_expression,
    OrExpression,
    collect_function_call_and_expression
);

collect_function_call_for_op2!(
    collect_function_call_and_expression,
    AndExpression,
    collect_function_call_compare_expression
);

collect_function_call_for_op2!(
    collect_function_call_compare_expression,
    CompareExpression,
    collect_function_call_add_or_sub_expression
);

collect_function_call_for_op2!(
    collect_function_call_add_or_sub_expression,
    AddOrSubExpression,
    collect_function_call_mul_or_div_expression
);

collect_function_call_for_op2!(
    collect_function_call_mul_or_div_expression,
    MulOrDivExpression,
    collect_function_call_factor
);

fn collect_function_call_factor(
    ast: &Factor,
    type_inference_result: &TypeInferenceResultContainer,
    function_calls: &mut Vec<(Arc<String>, EntityID)>,
    module_function_call_info: &mut ModuleFunctionCallInfo,
    context: &TranspileModuleContext,
) {
    let Ok(primary) = &ast.primary else { return };

    if ast.negative_keyword_span.is_some() {
        let operator_function_type = type_inference_result
            .operator_function_type_map
            .get(&EntityID::from(primary))
            .unwrap();

        if let Type::Function {
            function_info,
            generics: _,
        } = operator_function_type
        {
            function_calls.push((
                function_info.define_info.module_name.clone(),
                function_info.define_info.entity_id,
            ));
        }
    }

    collect_function_call_primary(
        primary,
        type_inference_result,
        function_calls,
        module_function_call_info,
        context,
    );
}

fn collect_function_call_primary(
    ast: &Primary,
    type_inference_result: &TypeInferenceResultContainer,
    function_calls: &mut Vec<(Arc<String>, EntityID)>,
    module_function_call_info: &mut ModuleFunctionCallInfo,
    context: &TranspileModuleContext,
) {
    collect_function_call_primary_left(
        &ast.left,
        type_inference_result,
        function_calls,
        module_function_call_info,
        context,
    );

    for right_primary in ast.chain.iter() {
        collect_function_call_primary_right(
            right_primary,
            type_inference_result,
            function_calls,
            module_function_call_info,
            context,
        );
    }
}

fn collect_function_call_primary_left(
    ast: &PrimaryLeft,
    type_inference_result: &TypeInferenceResultContainer,
    function_calls: &mut Vec<(Arc<String>, EntityID)>,
    module_function_call_info: &mut ModuleFunctionCallInfo,
    context: &TranspileModuleContext,
) {
    match &ast.first_expr {
        PrimaryLeftExpr::Simple((simple_primary, _, function_call)) => {
            if let Some(function_call) = function_call {
                let simple_primary_type =
                    type_inference_result.get_entity_type(EntityID::from(simple_primary));

                if let Type::Function {
                    function_info,
                    generics: _,
                } = simple_primary_type
                {
                    function_calls.push((
                        function_info.define_info.module_name.clone(),
                        function_info.define_info.entity_id,
                    ));
                }

                if let Ok(args) = &function_call.arg_exprs {
                    for expression in args.iter() {
                        collect_function_call_expression(
                            *expression,
                            type_inference_result,
                            function_calls,
                            module_function_call_info,
                            context,
                        );
                    }
                }
            }
        }
        PrimaryLeftExpr::NewArrayInitExpression(new_array_init_expression) => todo!(),
        PrimaryLeftExpr::NewArrayExpression(new_array_expression) => todo!(),
        PrimaryLeftExpr::NewExpression(new_expression) => todo!(),
        PrimaryLeftExpr::IfExpression(if_expression) => todo!(),
        PrimaryLeftExpr::LoopExpression(loop_expression) => todo!(),
    }
}

fn collect_function_call_primary_right(
    ast: &PrimaryRight,
    type_inference_result: &TypeInferenceResultContainer,
    function_calls: &mut Vec<(Arc<String>, EntityID)>,
    module_function_call_info: &mut ModuleFunctionCallInfo,
    context: &TranspileModuleContext,
) {
}

fn collect_function_call_mapping_operator(
    ast: &MappingOperator,
    type_inference_result: &TypeInferenceResultContainer,
    function_calls: &mut Vec<(Arc<String>, EntityID)>,
    module_function_call_info: &mut ModuleFunctionCallInfo,
    context: &TranspileModuleContext,
) {
}
