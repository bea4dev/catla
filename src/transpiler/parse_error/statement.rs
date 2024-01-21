use std::ops::Range;

use ariadne::{Report, ReportKind, Label, Source, Color};
use catla_parser::{lexer::Token, parser::{Spanned, StatementAttribute}};

use crate::transpiler::{error::{TranspileReport, ErrorMessageKey, ErrorMessageType}, context::TranspileModuleContext, TranspileError};


pub(crate) struct NotSeparatedStatement {
    token: Spanned<String>
}

impl TranspileReport for NotSeparatedStatement {
    fn print(&self, context: &TranspileModuleContext) {
        let text = &context.context.localized_text;
        let key = ErrorMessageKey::new(0001);

        Report::build(ReportKind::Error, context.module_name.as_str(), self.token.span.start)
            .with_code(1)
            .with_message(key.get_massage(text, ErrorMessageType::Message))
            .with_label(
                Label::new((context.module_name.as_str(), self.token.span.clone()))
                    .with_message(key.get_massage(text, ErrorMessageType::Label(0)))
                    .with_color(Color::Red)
            )
            .with_note(key.get_massage(text, ErrorMessageType::Note))
            .finish()
            .print((context.module_name.as_str(), Source::from(context.source_code.code.as_str())))
            .unwrap()
    }
}

pub(crate) fn not_separated_statement_error_1(token: &Token) -> TranspileError {
    return TranspileError::new(NotSeparatedStatement { token: Spanned::new(token.text.to_string(), token.span.clone()) });
}



pub(crate) struct StatementAttributesWithoutDefine {
    attributes_span: Range<usize>
}

impl TranspileReport for StatementAttributesWithoutDefine {
    fn print(&self, context: &TranspileModuleContext) {
        let text = &context.context.localized_text;
        let key = ErrorMessageKey::new(0004);

        Report::build(ReportKind::Error, context.module_name.as_str(), self.attributes_span.start)
            .with_code(4)
            .with_message(key.get_massage(text, ErrorMessageType::Message))
            .with_label(
                Label::new((context.module_name.as_str(), self.attributes_span.clone()))
                    .with_message(key.get_massage(text, ErrorMessageType::Label(0)))
                    .with_color(Color::Red)
            )
            .with_note(key.get_massage(text, ErrorMessageType::Note))
            .finish()
            .print((context.module_name.as_str(), Source::from(context.source_code.code.as_str())))
            .unwrap()
    }
}

pub(crate) fn statement_attributes_without_define(attributes: &bumpalo::collections::Vec<StatementAttribute>) -> TranspileError {
    let span_start = attributes.first().unwrap().span.start;
    let span_end = attributes.last().unwrap().span.end;
    return TranspileError::new(StatementAttributesWithoutDefine { attributes_span: span_start..span_end });
}