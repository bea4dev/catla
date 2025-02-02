use std::sync::Arc;

use catla_parser::parser::{Expression, ExpressionEnum, OrExpression, Program, StatementAST};
use either::Either;

use crate::transpiler::{
    component::EntityID, context::TranspileModuleContext,
    semantics::types::type_inference::TypeInferenceResultContainer,
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
        ExpressionEnum::OrExpression(or_expression) => todo!(),
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

fn collect_function_call_or_expression(
    ast: &OrExpression,
    type_inference_result: &TypeInferenceResultContainer,
    function_calls: &mut Vec<(Arc<String>, EntityID)>,
    module_function_call_info: &mut ModuleFunctionCallInfo,
    context: &TranspileModuleContext,
) {
}
