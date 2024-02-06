use std::ops::Range;

use ariadne::{Color, Label, Report, ReportKind, Source};
use catla_parser::parser::{statement, Expression, ExpressionEnum, Program, StatementAST};

use super::{advice::AdviceReport, error::{ErrorMessageKey, ErrorMessageType, TranspileReport}, TranspileError, TranspileWarning};



pub(crate) fn validate_syntax_program(
    ast: Program,
    is_data_struct_environment: bool,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>
) {
    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            _ => continue
        };

        match statement {
            StatementAST::Assignment(assignment) => {
                if !is_valid_format_for_assignment(assignment.left_expr) {
                    errors.push(InvalidAssignTargetFormat::new(get_expression_span(assignment.left_expr)));
                }
                validate_syntax_expression(assignment.left_expr, errors, warnings);
                if let Ok(right_expr) = assignment.right_expr {
                    validate_syntax_expression(right_expr, errors, warnings);
                }
            },
            StatementAST::Exchange(exchange) => {
                if !is_valid_format_for_assignment(exchange.left_expr) {
                    errors.push(InvalidAssignTargetFormat::new(get_expression_span(exchange.left_expr)));
                }
                validate_syntax_expression(exchange.left_expr, errors, warnings);
                if let Ok(right_expr) = exchange.right_expr {
                    if !is_valid_format_for_assignment(right_expr) {
                        errors.push(InvalidAssignTargetFormat::new(get_expression_span(right_expr)));
                    }
                    validate_syntax_expression(right_expr, errors, warnings);
                }
            },
            StatementAST::VariableDefine(variable_define) => {
                if let Some(expression) = &variable_define.expression {
                    if let Ok(expression) = expression {
                        validate_syntax_expression(&expression, errors, warnings);
                    }
                }
            },
            StatementAST::FunctionDefine(function_define) => {
                if let Some(block) = &function_define.block.value {
                    validate_syntax_program(block.program, false, errors, warnings);
                }
            },
            StatementAST::DataStructDefine(data_struct_define) => {
                
            },
            StatementAST::DropStatement(_) => todo!(),
            StatementAST::Expression(_) => todo!(),
            _ => {}
        }
    }
}

fn validate_syntax_expression(
    ast: Expression,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>
) {
    
}



fn get_expression_span(expression: Expression) -> Range<usize> {
    return match expression {
        ExpressionEnum::OrExpression(or_expression) => or_expression.span.clone(),
        ExpressionEnum::ReturnExpression(return_expression) => return_expression.span.clone(),
        ExpressionEnum::Closure(closure) => closure.span.clone(),
    }
}

fn is_valid_format_for_assignment(expression: Expression) -> bool {
    return if let ExpressionEnum::OrExpression(or_expression) = expression {
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
    };
}


pub(crate) struct InvalidAssignTargetFormat {
    span: Range<usize>,
    advice_report: AdviceReport
}

impl InvalidAssignTargetFormat {
    pub(crate) fn new(span: Range<usize>) -> TranspileError {
        return TranspileError::new(Self { span, advice_report: AdviceReport::new() });
    }
}

impl TranspileReport for InvalidAssignTargetFormat {
    fn print(&self, context: &super::context::TranspileModuleContext) {
        let module_name = &context.module_name;
        let text = &context.context.localized_text;
        let error_code = 0023;
        let key = ErrorMessageKey::new(error_code);

        Report::build(ReportKind::Error, module_name, self.span.start)
            .with_code(error_code)
            .with_message(key.get_massage(text, ErrorMessageType::Message))
            .with_label(
                Label::new((module_name, self.span.clone()))
                    .with_color(Color::Red)
                    .with_message(key.get_massage(text, ErrorMessageType::Label(0)))
            )
            .with_note(key.get_massage(text, ErrorMessageType::Note))
            .finish()
            .print((module_name, Source::from(context.source_code.code.as_str())))
            .unwrap();

        self.advice_report.print(context, self.span.start);
    }

    fn add_advice(&mut self, module_name: std::prelude::v1::String, advice: super::advice::Advice) {
        self.advice_report.add_advice(module_name, advice);
    }
}
