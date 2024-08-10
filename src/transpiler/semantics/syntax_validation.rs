use std::ops::Range;

use ariadne::Color;
use catla_parser::parser::{AddOrSubExpression, AndExpression, CompareExpression, StatementAttributes, EQNEExpression, Expression, ExpressionEnum, Factor, FunctionCall, MappingOperatorKind, MulOrDivExpression, OrExpression, Primary, PrimaryLeft, PrimaryLeftExpr, PrimaryRight, Program, SimplePrimary, StatementAST, StatementAttributeKind};
use either::Either::{Left, Right};

use crate::transpiler::{advice::{create_space_indent, get_column, Advice}, context::TranspileModuleContext, error::SimpleError, TranspileError, TranspileWarning};


const ERROR_INVALID_ASSIGNMENT_FORMAT: usize = 0025;
const ERROR_STATEMENT_IN_DATA_STRUCT_DEFINE_ENVIRONMENT: usize = 0026;


pub(crate) fn validate_syntax_program(
    ast: Program,
    context: &TranspileModuleContext,
    user_type_environment_span: Option<Range<usize>>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>
) {
    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            _ => continue
        };
        
        if let Some(data_struct_span) = &user_type_environment_span {
            let (is_valid, span) = match statement {
                StatementAST::Assignment(assignment) => (false, assignment.span.clone()),
                StatementAST::Exchange(exchange) => (false, exchange.span.clone()),
                StatementAST::Import(import) => (true, import.span.clone()),
                StatementAST::StatementAttributes(attributes) => {
                    let start = attributes.first().unwrap().span.start;
                    let end = attributes.last().unwrap().span.end;
                    (false, start..end)
                },
                StatementAST::VariableDefine(variable_define) => (true, variable_define.span.clone()),
                StatementAST::FunctionDefine(function_define) => (true, function_define.span.clone()),
                StatementAST::UserTypeDefine(data_struct_define) => (true, data_struct_define.span.clone()),
                StatementAST::TypeDefine(type_define) => (true, type_define.span.clone()),
                StatementAST::Implements(implements) => (true, implements.span.clone()),
                StatementAST::DropStatement(drop_statement) => (false, drop_statement.span.clone()),
                StatementAST::Expression(expression) => (false, get_expression_span(expression)),
            };

            if !is_valid {
                let labels = vec![
                    ((context.module_name.clone(), span.clone()), Color::Red),
                    ((context.module_name.clone(), data_struct_span.clone()), Color::Yellow)
                ];
                let mut error = SimpleError::new(
                    ERROR_STATEMENT_IN_DATA_STRUCT_DEFINE_ENVIRONMENT,
                    span.clone(),
                    vec![],
                    labels
                );
                
                let remove_advice = Advice::Remove { span: span.clone() };
                let data_struct_define_column = get_column(context.source_code.code.as_str(), data_struct_span.start);
                let indent = create_space_indent(data_struct_define_column + 4);
                let function_add = format!("\n{}function name_here() {{ {} }}", indent, &context.source_code.code[span.clone()]);
                let replace_with_function_advice = Advice::Add {
                    add: function_add,
                    position: span.end,
                    message_override: Some("advice.replace_with_function_label")
                };
                error.add_advice(context.module_name.clone(), remove_advice);
                error.add_advice(context.module_name.clone(), replace_with_function_advice);

                errors.push(error);
                continue;
            }
        }

        match statement {
            StatementAST::Assignment(assignment) => {
                if !is_valid_format_for_assignment(assignment.left_expr) {
                    let span = get_expression_span(assignment.left_expr);
                    errors.push(SimpleError::new(
                        ERROR_INVALID_ASSIGNMENT_FORMAT,
                        span.clone(),
                        vec![],
                        vec![((context.module_name.clone(), span), Color::Red)]
                    ));
                }
                validate_syntax_expression(assignment.left_expr, context, errors, warnings);
                if let Ok(right_expr) = assignment.right_expr {
                    validate_syntax_expression(right_expr, context,errors, warnings);
                }
            },
            StatementAST::Exchange(exchange) => {
                if !is_valid_format_for_assignment(exchange.left_expr) {
                    let span = get_expression_span(exchange.left_expr);
                    errors.push(SimpleError::new(
                        ERROR_INVALID_ASSIGNMENT_FORMAT,
                        span.clone(),
                        vec![],
                        vec![((context.module_name.clone(), span), Color::Red)]
                    ));
                }
                validate_syntax_expression(exchange.left_expr, context,errors, warnings);
                if let Ok(right_expr) = exchange.right_expr {
                    if !is_valid_format_for_assignment(right_expr) {
                        let span = get_expression_span(right_expr);
                        errors.push(SimpleError::new(
                            ERROR_INVALID_ASSIGNMENT_FORMAT,
                            span.clone(),
                            vec![],
                            vec![((context.module_name.clone(), span), Color::Red)]
                        ));
                    }
                    validate_syntax_expression(right_expr, context,errors, warnings);
                }
            },
            StatementAST::VariableDefine(variable_define) => {
                if let Some(expression) = &variable_define.expression {
                    if let Ok(expression) = expression {
                        validate_syntax_expression(&expression, context,errors, warnings);
                    }
                }
            },
            StatementAST::FunctionDefine(function_define) => {
                if user_type_environment_span.is_some()
                    && !function_define.attributes.contains(StatementAttributeKind::Static)
                    && function_define.args.this_mutability.is_none() {
                    
                    let mut error = TranspileError::new(SimpleError::new(
                        0055,
                        function_define.args.span.clone(),
                        vec![],
                        vec![((context.module_name.clone(), function_define.args.span.clone()), Color::Red)]
                    ));
                    error.add_advice(
                        context.module_name.clone(),
                        Advice::Add {
                            add: "var this".to_string(),
                            position: function_define.args.span.start + 1,
                            message_override: None
                        }
                    );
                    errors.push(error);
                }

                if let Some(block) = &function_define.block.value {
                    validate_syntax_program(
                        block.program,
                        context,
                        None,
                        errors,
                        warnings
                    );
                }
            },
            StatementAST::UserTypeDefine(data_struct_define) => {
                if let Some(block) = &data_struct_define.block.value {
                    validate_syntax_program(
                        block.program,
                        context,
                        Some(data_struct_define.span.clone()),
                        errors,
                        warnings
                    );
                }
            },
            StatementAST::Implements(implements) => {
                if let Some(block) = &implements.block.value {
                    validate_syntax_program(
                        block.program,
                        context,
                        Some(implements.span.clone()),
                        errors,
                        warnings
                    );
                }
            },
            StatementAST::DropStatement(drop_statement) => {
                if let Ok(expression) = &drop_statement.expression {
                    validate_syntax_expression(&expression, context,errors, warnings);
                }
            },
            StatementAST::Expression(expression) => {
                validate_syntax_expression(&expression, context,errors, warnings);
            },
            _ => {}
        }
    }
}

fn validate_syntax_expression(
    ast: Expression,
    context: &TranspileModuleContext,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>
) {
    match ast {
        ExpressionEnum::OrExpression(or_expression) => {
            validate_syntax_or_expression(or_expression, context,errors, warnings);
        },
        ExpressionEnum::ReturnExpression(return_expression) => {
            if let Some(expression) = &return_expression.expression {
                validate_syntax_expression(&expression, context,errors, warnings);
            }
        },
        ExpressionEnum::Closure(closure) => {
            if let Some(expression_or_block) = &closure.expression_or_block.value {
                match expression_or_block {
                    Left(expression) => {
                        validate_syntax_expression(&expression, context,errors, warnings);
                    },
                    Right(block) => {
                        validate_syntax_program(
                            block.program,
                            context,
                            None,
                            errors,
                            warnings
                        );
                    }
                }
            }
        },
    }
}

fn validate_syntax_or_expression(
    ast: &OrExpression,
    context: &TranspileModuleContext,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>
) {
    validate_syntax_and_expression(&ast.left_expr, context,errors, warnings);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(and_expression) = &right_expr.1 {
            validate_syntax_and_expression(and_expression, context,errors, warnings);
        }
    }
}

fn validate_syntax_and_expression(
    ast: &AndExpression,
    context: &TranspileModuleContext,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>
) {
    validate_syntax_eqne_expression(&ast.left_expr, context,errors, warnings);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(eqne_expression) = &right_expr.1 {
            validate_syntax_eqne_expression(eqne_expression, context,errors, warnings);
        }
    }
}

fn validate_syntax_eqne_expression(
    ast: &EQNEExpression,
    context: &TranspileModuleContext,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>
) {
    validate_syntax_compare_expression(&ast.left_expr, context,errors, warnings);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(compare_expression) = &right_expr.1 {
            validate_syntax_compare_expression(compare_expression, context,errors, warnings);
        }
    }
}

fn validate_syntax_compare_expression(
    ast: &CompareExpression,
    context: &TranspileModuleContext,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>
) {
    validate_syntax_add_or_sub_expression(&ast.left_expr, context,errors, warnings);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(add_or_sub_expression) = &right_expr.1 {
            validate_syntax_add_or_sub_expression(add_or_sub_expression, context,errors, warnings);
        }
    }
}

fn validate_syntax_add_or_sub_expression(
    ast: &AddOrSubExpression,
    context: &TranspileModuleContext,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>
) {
    validate_syntax_mul_or_div_expression(&ast.left_expr, context,errors, warnings);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(mul_or_div_expression) = &right_expr.1 {
            validate_syntax_mul_or_div_expression(mul_or_div_expression, context,errors, warnings);
        }
    }
}

fn validate_syntax_mul_or_div_expression(
    ast: &MulOrDivExpression,
    context: &TranspileModuleContext,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>
) {
    validate_syntax_factor(&ast.left_expr, context,errors, warnings);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(factor) = &right_expr.1 {
            validate_syntax_factor(factor, context,errors, warnings);
        }
    }
}

fn validate_syntax_factor(
    ast: &Factor,
    context: &TranspileModuleContext,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>
) {
    if let Ok(primary) = &ast.primary {
        validate_syntax_primary(primary, context,errors, warnings);
    }
}

fn validate_syntax_primary(
    ast: &Primary,
    context: &TranspileModuleContext,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>
) {
    validate_syntax_primary_left(&ast.left, context,errors, warnings);
    for primary_right in ast.chain.iter() {
        validate_syntax_primary_right(primary_right, context,errors, warnings);
    }
}

fn validate_syntax_primary_left(
    ast: &PrimaryLeft,
    context: &TranspileModuleContext,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>
) {
    match &ast.first_expr {
        PrimaryLeftExpr::Simple(simple) => {
            match &simple.0 {
                SimplePrimary::Expression { expression, error_tokens: _, span: _ } => {
                    if let Ok(expression) = expression {
                        validate_syntax_expression(&expression, context,errors, warnings);
                    }
                },
                _ => {}
            }

            if let Some(function_call) = &simple.2 {
                validate_syntax_function_call(function_call, context,errors, warnings);
            }
        },
        PrimaryLeftExpr::NewExpression(new_expression) => {
            if let Ok(field_assigns) = &new_expression.field_assigns {
                for field_assign in field_assigns.iter() {
                    if let Ok(expression) = &field_assign.expression {
                        validate_syntax_expression(&expression, context, errors, warnings);
                    }
                }
            }
        },
        PrimaryLeftExpr::NewArrayInitExpression(new_array_init_expression) => {
            if let Ok(init_expression) = new_array_init_expression.init_expression {
                if !is_valid_format_for_array_init_expr(init_expression) {
                    let mut error = SimpleError::new(
                        0066,
                        init_expression.get_span(),
                        vec![],
                        vec![((context.module_name.clone(), init_expression.get_span()), Color::Red)]
                    );
                    error.add_advice(
                        context.module_name.clone(),
                        Advice::Add {
                            add: "i =>".to_string(),
                            position: init_expression.get_span().start,
                            message_override: Some("advice.replace_with_closure")
                        }
                    );
                    errors.push(error);
                }

                validate_syntax_expression(init_expression, context, errors, warnings);
            }
            if let Ok(length_expression) = new_array_init_expression.length_expression {
                validate_syntax_expression(length_expression, context, errors, warnings);
            }
        },
        PrimaryLeftExpr::NewArrayExpression(new_array_expression) => {
            for value_expression in new_array_expression.value_expressions.iter() {
                if let Ok(value_expression) = value_expression {
                    validate_syntax_expression(*value_expression, context, errors, warnings);
                }
            }
        },
        PrimaryLeftExpr::IfExpression(if_expression) => {
            let if_statement = &if_expression.if_statement;
            if let Ok(condition) = &if_statement.condition {
                validate_syntax_expression(condition, context,errors, warnings);
            }
            if let Some(block) = &if_statement.block.value {
                validate_syntax_program(block.program, context, None, errors, warnings);
            }
        },
        PrimaryLeftExpr::LoopExpression(loop_expression) => {
            if let Ok(block) = &loop_expression.block {
                validate_syntax_program(block.program, context, None, errors, warnings);
            }
        },
    }
    
    if let Some(mapping_operator) = &ast.mapping_operator {
        validate_syntax_mapping_operator(&mapping_operator.value, context,errors, warnings);
    }
}

fn is_valid_format_for_array_init_expr(
    ast: Expression
) -> bool {
    let or_expression = match ast {
        ExpressionEnum::OrExpression(or_expression) => or_expression,
        ExpressionEnum::ReturnExpression(_) => return true,
        ExpressionEnum::Closure(_) => return true
    };

    if !or_expression.right_exprs.is_empty() {
        return false;
    }

    let and_expression = &or_expression.left_expr;
    if !and_expression.right_exprs.is_empty() {
        return false;
    }

    let eq_expression = &and_expression.left_expr;
    if !eq_expression.right_exprs.is_empty() {
        return false;
    }

    let compare_expression = &eq_expression.left_expr;
    if !compare_expression.right_exprs.is_empty() {
        return false;
    }

    let add_expression = &compare_expression.left_expr;
    if !add_expression.right_exprs.is_empty() {
        return false;
    }

    let mul_expression = &add_expression.left_expr;
    if !mul_expression.right_exprs.is_empty() {
        return false;
    }

    let factor = &mul_expression.left_expr;
    if let Ok(primary) = &factor.primary {
        if !primary.chain.is_empty() {
            return false;
        }
        
        let left = &primary.left;
        if left.mapping_operator.is_some() {
            return false;
        }

        return if let PrimaryLeftExpr::Simple((simple_primary, generics, function_call)) = &left.first_expr {
            if generics.is_some() {
                return false;
            }
            if function_call.is_some() {
                return false;
            }
            
            match simple_primary {
                SimplePrimary::Expression { expression: _, error_tokens: _, span: _ } => false,
                _ => true
            }
        } else {
            false
        }
    }

    true
}

fn validate_syntax_primary_right(
    ast: &PrimaryRight,
    context: &TranspileModuleContext,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>
) {
    if let Some(second_expr) = &ast.second_expr {
        if let Some(function_call) = &second_expr.2 {
            validate_syntax_function_call(function_call, context,errors, warnings);
        }
    }
    if let Some(mapping_operator) = &ast.mapping_operator {
        validate_syntax_mapping_operator(&mapping_operator.value, context,errors, warnings);
    }
}

fn validate_syntax_mapping_operator(
    ast: &MappingOperatorKind,
    context: &TranspileModuleContext,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>
) {
    let block = match ast {
        MappingOperatorKind::NullElvisBlock(block) => block,
        MappingOperatorKind::ResultElvisBlock(block) => block,
        _ => return
    };
    if let Some(block) = &block.value {
        validate_syntax_program(block.program, context,None, errors, warnings);
    }
}

fn validate_syntax_function_call(
    ast: &FunctionCall,
    context: &TranspileModuleContext,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>
) {
    if let Ok(arg_exprs) = &ast.arg_exprs {
        for arg_expr in arg_exprs.iter() {
            validate_syntax_expression(&arg_expr, context,errors, warnings);
        }
    }
}


fn get_expression_span(expression: Expression) -> Range<usize> {
    match expression {
        ExpressionEnum::OrExpression(or_expression) => or_expression.span.clone(),
        ExpressionEnum::ReturnExpression(return_expression) => return_expression.span.clone(),
        ExpressionEnum::Closure(closure) => closure.span.clone(),
    }
}

fn is_valid_format_for_assignment(expression: Expression) -> bool {
    if let ExpressionEnum::OrExpression(or_expression) = expression {
        let and_expression = &or_expression.left_expr;
        let eqne_expression = &and_expression.left_expr;
        let compare_expression = &eqne_expression.left_expr;
        let add_or_sub_expression = &compare_expression.left_expr;
        let mul_or_div_expression = &add_or_sub_expression.left_expr;
        let factor = &mul_or_div_expression.left_expr;
        or_expression.right_exprs.is_empty()
            && and_expression.right_exprs.is_empty()
            && eqne_expression.right_exprs.is_empty()
            && compare_expression.right_exprs.is_empty()
            && add_or_sub_expression.right_exprs.is_empty()
            && mul_or_div_expression.right_exprs.is_empty()
            && factor.negative_keyword_span.is_none()
    } else {
        false
    }
}