use std::{collections::BTreeMap, ops::Range};

use ariadne::{sources, Color, Label, Report, ReportKind, Source};
use catla_parser::parser::{Spanned, ASTParseError};
use indexmap::IndexMap;

use crate::transpiler::{error::{TranspileReport, ErrorMessageKey, ErrorMessageType}, context::TranspileModuleContext, TranspileError};


pub(crate) struct UnexpectedTokens {
    pub span: Range<usize>,
    pub error_code: usize,
    pub expected: Expected,
    pub advice_report: AdviceReport
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

        self.advice_report.print(context, self.span.start);
    }

    fn add_advice(&mut self, module_name: String, advice: Advice) {
        self.advice_report.add_advice(module_name, advice);
    }
}

pub(crate) struct UnexpectedEOF {
    error_code: usize,
    expected: Expected,
    advice_report: AdviceReport
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

        self.advice_report.print(context, code_length);
    }

    fn add_advice(&mut self, module_name: String, advice: Advice) {
        self.advice_report.add_advice(module_name, advice);
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
    FatArrowAndBlock,
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
            Expected::FatArrowAndBlock => "fat_arrow_and_block",
            Expected::ExpressionOrBlock => "expression_or_block",
            Expected::Generics => "generics",
            Expected::ArgumentExpression => "argument_expression",
            Expected::FunctionCall => "function_call",
            Expected::IfStatementOrBlock => "if_statement_or_block",
            Expected::TypeInfo => "type_info"
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
        Expected::BraceRight => Some("}"),
        Expected::VariableName => Some("name_here"),
        Expected::FunctionName => Some("name_here"),
        Expected::FunctionArgument => None,
        Expected::Block => Some("{ /* do something */ }"),
        Expected::DataStructName => Some("NameHere"),
        Expected::ExtendsType => None,
        Expected::ImplementsType => None,
        Expected::ClosureArgument => None,
        Expected::VerticalBarRight => Some("|"),
        Expected::FatArrow => None,
        Expected::FatArrowAndBlock => Some("=> { /* do something */ }"),
        Expected::ExpressionOrBlock => Some("{ /* do something */ }"),
        Expected::Generics => None,
        Expected::ArgumentExpression => None,
        Expected::FunctionCall => None,
        Expected::IfStatementOrBlock => Some("{ /* do something */ }"),
        Expected::TypeInfo => None,
    };

    if !unexpected_tokens.is_empty() {
        let tokens: Vec<Spanned<String>> = unexpected_tokens.iter().map(|token| { Spanned::new(token.text.to_string(), token.span.clone()) }).collect();
        let span_start = tokens.first().unwrap().span.start;
        let span_end = tokens.last().unwrap().span.end;

        let mut error = TranspileError::new(UnexpectedTokens { span: span_start..span_end, error_code, expected, advice_report: AdviceReport::new() });

        if expected == Expected::Unnecessary {
            error.add_advice(context.module_name.clone(), Advice::Remove { span: span_start..span_end })
        } else {
            if let Some(add) = advice_add {
                error.add_advice(context.module_name.clone(), Advice::Add { add: add.to_string(), position: span_start })
            }
        };

        transpile_errors.push(error);
    }

    if has_eof_error {
        let mut error = TranspileError::new(UnexpectedEOF { error_code, expected, advice_report: AdviceReport::new() });

        if let Some(add) = advice_add {
            error.add_advice(context.module_name.clone(), Advice::Add { add: add.to_string(), position: context.source_code.code.len() });
        }

        transpile_errors.push(error);
    }

    return transpile_errors;
}



pub(crate) struct AdviceReport {
    elements: IndexMap<String, Vec<Advice>>
}

impl AdviceReport {

    pub(crate) fn new() -> AdviceReport {
        return Self { elements: IndexMap::new() }
    }

    pub(crate) fn add_advice(&mut self, module_name: String, advice: Advice) {
        let elements = self.elements.entry(module_name).or_insert_with(|| vec![]);
        elements.push(advice);
    }
    
    pub(crate) fn print(&self, context: &TranspileModuleContext, offset: usize) {
        if self.elements.is_empty() {
            return;
        }

        let text = &context.context.localized_text;

        let advice_color = Color::RGB(52, 226, 226);

        let mut builder = Report::build(ReportKind::Custom("  > Advice", advice_color), &context.module_name, offset)
            .with_message(text.get_text("advice.message"));

        let mut source_list = Vec::new();

        for (module_name, elements) in self.elements.iter() {
            let module_context = context.context.get_module_context(module_name).unwrap();
            let original_source = &module_context.source_code.code;
            let mut add_elements = BTreeMap::new();

            for advice in elements.iter() {
                match advice {
                    Advice::Add { add, position } => {
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
                    },
                    Advice::Remove { span } => {
                        add_elements.insert(span.start, Advice::Remove { span: span.clone() });
                    }
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
            
            let mut span_source_position = 0;
            let mut advice_source_position = 0;
            let mut new_source = String::new();
            for (&position, advice) in add_elements.iter() {
                let advice_position = position - advice_span.start;
                let prev_source = &span_source[span_source_position..advice_position];
                new_source += prev_source;

                advice_source_position += advice_position - span_source_position;

                match advice {
                    Advice::Add { add, position: _ } => {
                        let advice_span_end = advice_source_position + add.len();
                        builder.add_label(
                            Label::new((module_name.clone(), advice_source_position..advice_span_end))
                                .with_message(text.get_text("advice.add_label"))
                                .with_color(advice_color)
                        );
                        advice_source_position = advice_span_end;
                        new_source += add.as_str();
                    },
                    Advice::Remove { span } => {
                        builder.add_label(
                            Label::new((module_name.clone(), advice_source_position..(advice_source_position + (span.len()))))
                                .with_message(text.get_text("advice.remove_label"))
                                .with_color(Color::RGB(252, 233, 79))
                        )
                    },
                }

                span_source_position = advice_position;
            }
            new_source += &span_source[span_source_position..span_source.len()];

            source_list.push((module_context.module_name.clone(), new_source));
        }
        
        builder.set_note(text.get_text("advice.note"));

        builder.finish().print(sources(source_list)).unwrap();
    }

}


pub enum Advice {
    Add { add: String, position: usize },
    Remove { span: Range<usize> }
}