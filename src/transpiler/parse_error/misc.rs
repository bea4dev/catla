use ariadne::{Report, ReportKind, Label, Color, Source};
use catla_parser::parser::{Spanned, ASTParseError};

use crate::transpiler::{error::{TranspileReport, ErrorMessageKey, ErrorMessageType}, context::TranspileModuleContext, TranspileError};


pub(crate) struct UnexpectedTokens {
    tokens: Vec<Spanned<String>>,
    expected: Expected,
}

impl TranspileReport for UnexpectedTokens {
    fn print(&self, context: &TranspileModuleContext) {
        let text = &context.context.localized_text;
        let module_name = &context.module_name;
        
        let mut span_start = self.tokens.first().unwrap().span.start;
        let mut span_end = 0;
        for token in self.tokens.iter() {
            span_start = span_start.min(token.span.start);
            span_end = span_end.max(token.span.end);
        }

        let key = ErrorMessageKey::new(0002);

        let label_massage = key.get_massage(text, ErrorMessageType::Label(0))
            + text.get_text(self.expected.get_expected_key()).as_str();
        
        Report::build(ReportKind::Error, module_name, span_start)
            .with_code(2)
            .with_message(key.get_massage(text, ErrorMessageType::Message))
            .with_label(
                Label::new((module_name, span_start..span_end))
                    .with_message(label_massage)
                    .with_color(Color::Red)
            )
            .with_note(key.get_massage(text, ErrorMessageType::Note))
            .finish()
            .print((module_name, Source::from(context.source_code.code.as_str())))
            .unwrap()
    }
}

pub(crate) struct UnexpectedEOF {
    expected: Expected
}

impl TranspileReport for UnexpectedEOF {
    fn print(&self, context: &TranspileModuleContext) {
        let text = &context.context.localized_text;
        let module_name = &context.module_name;

        let key = ErrorMessageKey::new(0003);

        let code_length = context.source_code.code.len();

        let label_message = key.get_massage(text, ErrorMessageType::Label(0))
            + text.get_text(self.expected.get_expected_key()).as_str();

        Report::build(ReportKind::Error, module_name, code_length)
            .with_code(3)
            .with_message(key.get_massage(text, ErrorMessageType::Message))
            .with_label(
                Label::new((module_name, (code_length - 1)..code_length))
                    .with_message(label_message)
                    .with_color(Color::Red)
            )
            .with_note(key.get_massage(text, ErrorMessageType::Note))
            .finish()
            .print((module_name, Source::from(context.source_code.code.as_str())))
            .unwrap()
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
    BraceRight,
    VariableName,
    FunctionName,
    FunctionArgument,
    Block
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
            Expected::BraceRight => "brace_right",
            Expected::VariableName => "variable_name",
            Expected::FunctionName => "function_name",
            Expected::FunctionArgument => "function_argument",
            Expected::Block => "block"
        });
    }
}

pub(crate) fn unexpected_token_error_2_or_error_3(ast_errors: &Vec<&ASTParseError>, expected: Expected) -> Vec<TranspileError> {
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

    if !unexpected_tokens.is_empty() {
        let tokens = unexpected_tokens.iter().map(|token| { Spanned::new(token.text.to_string(), token.span.clone()) }).collect();
        transpile_errors.push(TranspileError::new(UnexpectedTokens { tokens, expected }));
    }

    if has_eof_error {
        transpile_errors.push(TranspileError::new(UnexpectedEOF { expected }));
    }

    return transpile_errors;
}