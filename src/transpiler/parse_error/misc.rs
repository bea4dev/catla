use std::{collections::{BTreeMap, HashMap}, ops::Range};

use ariadne::{Report, ReportKind, Label, Color, Source};
use catla_parser::parser::{Spanned, ASTParseError};
use indexmap::IndexMap;

use crate::transpiler::{error::{TranspileReport, ErrorMessageKey, ErrorMessageType}, context::TranspileModuleContext, TranspileError};


pub(crate) struct UnexpectedTokens {
    pub span: Range<usize>,
    pub error_code: usize,
    pub expected: Expected,
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
            .unwrap()
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
                Label::new((module_name, (code_length - 1)..code_length))
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
    Block,
    DataStructName,
    ExtendsType,
    ImplementsType,
    ClosureArgument,
    VerticalBarRight,
    FatArrow,
    ExpressionOrBlock,
    Generics,
    ArgumentExpression,
    FunctionCall,
    IfStatementOrBlock,
    TypeInfo
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
            Expected::Block => "block",
            Expected::DataStructName => "data_struct_name",
            Expected::ExtendsType => "extends_type",
            Expected::ImplementsType => "implements_type",
            Expected::ClosureArgument => "closure_argument",
            Expected::VerticalBarRight => "vertical_bar_right",
            Expected::FatArrow => "fat_arrow",
            Expected::ExpressionOrBlock => "expression_or_block",
            Expected::Generics => "generics",
            Expected::ArgumentExpression => "argument_expression",
            Expected::FunctionCall => "function_call",
            Expected::IfStatementOrBlock => "if_statement_or_block",
            Expected::TypeInfo => "type_info"
        });
    }
}

pub(crate) fn unexpected_token_error(ast_errors: &Vec<&ASTParseError>, expected: Expected, error_code: usize) -> Vec<TranspileError> {
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
        let tokens: Vec<Spanned<String>> = unexpected_tokens.iter().map(|token| { Spanned::new(token.text.to_string(), token.span.clone()) }).collect();
        let span_start = tokens.first().unwrap().span.start;
        let span_end = tokens.last().unwrap().span.end;

        transpile_errors.push(TranspileError::new(UnexpectedTokens { span: span_start..span_end, error_code, expected }));
    }

    if has_eof_error {
        transpile_errors.push(TranspileError::new(UnexpectedEOF { error_code, expected }));
    }

    return transpile_errors;
}



pub(crate) struct AdviceReport {
    elements: IndexMap<String, Vec<Advice>>
}

impl AdviceReport {

    pub(crate) fn add_advice(&mut self, module_name: String, advice: Advice) {
        let elements = self.elements.entry(module_name).or_insert_with(|| vec![]);
        elements.push(advice);
    }
    
    pub(crate) fn print(&self, context: &TranspileModuleContext, offset: usize) {
        let text = &context.context.localized_text;

        let builder = Report::build(ReportKind::Advice, &context.module_name, offset)
            .with_message(text.get_text("advice.message"));

        for (module_name, elements) in self.elements.iter() {
            let module_context = context.context.get_module_context(module_name).unwrap();
            let original_source = &module_context.source_code.code;
            let mut add_elements = BTreeMap::new();

            for advice in elements.iter() {
                if let Advice::Add { add, position } = advice {
                    let position = *position;

                    let is_prev_space = match original_source.get((position - 1)..position) {
                        Some(prev) => prev == " ",
                        None => false
                    };
                    let is_current_space = match original_source.get(position..(position + 1)) {
                        Some(current) => current == " ",
                        None => false
                    };
                    let prev_space = if is_prev_space { "" } else { " " };
                    let next_space = if is_current_space { "" } else { " " };

                    let add = format!("{}{}{}", prev_space, add, next_space);

                    add_elements.insert(position, Advice::Add { add, position });
                }
            }

            let mut advice_span = usize::MAX..0;
            for element in elements.iter() {
                let span = match element {
                    Advice::Add { add: _, position } => *position..*position,
                    Advice::Remove { span } => span.clone()
                };
                advice_span.start = advice_span.start.min(span.start);
                advice_span.end = advice_span.end.max(span.end);
            }

            const CLEARANCE: usize = 50;
            advice_span.start = advice_span.start.checked_sub(CLEARANCE).unwrap_or(0);
            loop {
                if original_source.is_char_boundary(advice_span.start) {
                    break;
                }
                advice_span.start -= 1;
            }

            advice_span.end = original_source.len().min(advice_span.end + CLEARANCE);
            loop {
                if original_source.is_char_boundary(advice_span.end) {
                    break;
                }
                advice_span.end += 1;
            }

            let span_source = &original_source[advice_span.clone()];
            
            let mut index = 0;
            let mut new_source = String::new();
            for (&position, advice) in add_elements.iter() {
                
            }
        }
    }

}


pub(crate) enum Advice {
    Add { add: String, position: usize },
    Remove { span: Range<usize> }
}