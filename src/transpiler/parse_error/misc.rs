use std::ops::Range;

use ariadne::{Color, Label, Report, ReportKind, Source};
use catla_parser::parser::{Spanned, ASTParseError};

use crate::transpiler::{advice::Advice, context::TranspileModuleContext, error::{TranspileReport, ErrorMessageKey, ErrorMessageType}, TranspileError};


pub(crate) struct UnexpectedTokens {
    pub span: Range<usize>,
    pub error_code: usize,
    pub expected: Expected
}

impl TranspileReport for UnexpectedTokens {
    fn print(&self, context: &TranspileModuleContext) {
        let text = &context.context.localized_text;
        let module_name = &context.module_name;

        let key = ErrorMessageKey::new(self.error_code);

        let label_massage = key.get_massage(text, ErrorMessageType::Label(0))
            + text.get_text(self.expected.get_expected_key()).as_str();
        
        let mut builder = Report::build(ReportKind::Error, module_name, self.span.start)
            .with_code(self.error_code)
            .with_message(key.get_massage(text, ErrorMessageType::Message))
            .with_label(
                Label::new((module_name, self.span.clone()))
                    .with_message(label_massage)
                    .with_color(Color::Red)
            );
        
        if let Some(note) = key.get_massage_optional(text, ErrorMessageType::Note) {
            builder.set_note(note);
        }
        if let Some(help) = key.get_massage_optional(text, ErrorMessageType::Help) {
            builder.set_help(help);
        }
        builder.finish()
            .print((module_name, Source::from(context.source_code.code.as_str())))
            .unwrap();
    }
}


pub(crate) struct UnexpectedEOF {
    error_code: usize,
    expected: Expected
}

impl TranspileReport for UnexpectedEOF {
    fn print(&self, context: &TranspileModuleContext) {
        let text = &context.context.localized_text;
        let module_name = &context.module_name;

        let key = ErrorMessageKey::new(self.error_code);

        let code_length = context.source_code.code.len();

        let label_message = key.get_massage(text, ErrorMessageType::Label(0))
            + text.get_text(self.expected.get_expected_key()).as_str();

        let mut builder = Report::build(ReportKind::Error, module_name, code_length)
            .with_code(self.error_code)
            .with_message(key.get_massage(text, ErrorMessageType::Message))
            .with_label(
                Label::new((module_name, code_length..code_length))
                    .with_message(label_message)
                    .with_color(Color::Red)
            );
        
        if let Some(note) = key.get_massage_optional(text, ErrorMessageType::Note) {
            builder.set_note(note);
        }
        if let Some(help) = key.get_massage_optional(text, ErrorMessageType::Help) {
            builder.set_help(help);
        }
        builder.finish()
            .print((module_name, Source::from(context.source_code.code.as_str())))
            .unwrap();
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum Expected {
    Unnecessary,
    Statement,
    Expression,
    ImportElements,
    ParenthesisLeft,
    ParenthesisRight,
    BraceLeft,
    BraceRight,
    VariableName,
    FunctionName,
    FunctionArgument,
    Block,
    DataStructName,
    SuperTypeInfo,
    ClosureArgument,
    VerticalBarRight,
    FatArrow,
    FatArrowAndBlock,
    ExpressionOrBlock,
    Generics,
    ArgumentExpression,
    IfStatementOrBlock,
    TypeInfo,
    GreaterThan,
    FieldAssign
}

impl Expected {
    pub(crate) fn get_expected_key(&self) -> String {
        return format!("expected.{}", match self {
            Expected::Unnecessary => "unnecessary",
            Expected::Statement => "statement",
            Expected::Expression => "expression",
            Expected::ImportElements => "import_elements",
            Expected::ParenthesisLeft => "parenthesis_left",
            Expected::ParenthesisRight => "parenthesis_right",
            Expected::BraceLeft => "brace_left",
            Expected::BraceRight => "brace_right",
            Expected::VariableName => "variable_name",
            Expected::FunctionName => "function_name",
            Expected::FunctionArgument => "function_argument",
            Expected::Block => "block",
            Expected::DataStructName => "data_struct_name",
            Expected::SuperTypeInfo => "super_type_info",
            Expected::ClosureArgument => "closure_argument",
            Expected::VerticalBarRight => "vertical_bar_right",
            Expected::FatArrow => "fat_arrow",
            Expected::FatArrowAndBlock => "fat_arrow_and_block",
            Expected::ExpressionOrBlock => "expression_or_block",
            Expected::Generics => "generics",
            Expected::ArgumentExpression => "argument_expression",
            Expected::IfStatementOrBlock => "if_statement_or_block",
            Expected::TypeInfo => "type_info",
            Expected::GreaterThan => "greater_than",
            Expected::FieldAssign => "field_assign"
        });
    }
}

pub(crate) fn unexpected_token_error(ast_errors: &Vec<&ASTParseError>, expected: Expected, error_code: usize, context: &TranspileModuleContext) -> Vec<TranspileError> {
    let mut transpile_errors = Vec::new();
    let mut unexpected_tokens = Vec::new();

    let mut has_eof_error = false;
    for error in ast_errors {
        match error {
            ASTParseError::UnexpectedToken(token) => unexpected_tokens.extend(token),
            _ => {
                has_eof_error = true;
            }
        }
    }

    let advice_add = match expected {
        Expected::Unnecessary => None,
        Expected::Statement => None,
        Expected::Expression => None,
        Expected::ImportElements => None,
        Expected::ParenthesisLeft => Some("("),
        Expected::ParenthesisRight => Some(")"),
        Expected::BraceLeft => Some("{"),
        Expected::BraceRight => Some("}"),
        Expected::VariableName => Some("name_here"),
        Expected::FunctionName => Some("name_here"),
        Expected::FunctionArgument => None,
        Expected::Block => Some("{ /* do something */ }"),
        Expected::DataStructName => Some("NameHere"),
        Expected::SuperTypeInfo => None,
        Expected::ClosureArgument => None,
        Expected::VerticalBarRight => Some("|"),
        Expected::FatArrow => None,
        Expected::FatArrowAndBlock => Some("=> { /* do something */ }"),
        Expected::ExpressionOrBlock => Some("{ /* do something */ }"),
        Expected::Generics => None,
        Expected::ArgumentExpression => None,
        Expected::IfStatementOrBlock => Some("{ /* do something */ }"),
        Expected::TypeInfo => None,
        Expected::GreaterThan => Some(">"),
        Expected::FieldAssign => None
    };

    if !unexpected_tokens.is_empty() {
        let tokens: Vec<Spanned<String>> = unexpected_tokens.iter().map(|token| { Spanned::new(token.text.to_string(), token.span.clone()) }).collect();
        let span_start = tokens.first().unwrap().span.start;
        let span_end = tokens.last().unwrap().span.end;

        let mut error = TranspileError::new(UnexpectedTokens { span: span_start..span_end, error_code, expected });

        if expected == Expected::Unnecessary {
            error.add_advice(context.module_name.clone(), Advice::Remove { span: span_start..span_end })
        } else {
            if let Some(add) = advice_add {
                let advice = Advice::Add { add: add.to_string(), position: span_start, message_override: None };
                error.add_advice(context.module_name.clone(), advice)
            }
        };

        transpile_errors.push(error);
    }

    if has_eof_error {
        let mut error = TranspileError::new(UnexpectedEOF { error_code, expected });

        if let Some(add) = advice_add {
            let advice = Advice::Add { add: add.to_string(), position: context.source_code.code.len(), message_override: None };
            error.add_advice(context.module_name.clone(), advice);
        }

        transpile_errors.push(error);
    }

    return transpile_errors;
}