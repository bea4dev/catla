use std::ops::Range;

use allocator_api2::vec::Vec;
use ariadne::{Color, Label, Report, ReportKind, Source};
use bumpalo::Bump;
use catla_parser::{
    lexer::Token,
    parser::{Spanned, StatementAttribute},
};

use crate::transpiler::{
    advice::Advice,
    context::TranspileModuleContext,
    error::{ErrorMessageKey, ErrorMessageType, TranspileReport},
    TranspileError,
};

pub(crate) struct NotSeparatedStatement {
    token: Spanned<String>,
}

impl TranspileReport for NotSeparatedStatement {
    fn print(&self, context: &TranspileModuleContext) {
        let text = &context.context.localized_text;
        let key = ErrorMessageKey::new(0001);

        Report::build(
            ReportKind::Error,
            context.module_name.as_str(),
            self.token.span.start,
        )
        .with_code(1)
        .with_message(key.get_massage(text, ErrorMessageType::Message))
        .with_label(
            Label::new((context.module_name.as_str(), self.token.span.clone()))
                .with_message(key.get_massage(text, ErrorMessageType::Label(0)))
                .with_color(Color::Red),
        )
        .with_note(key.get_massage(text, ErrorMessageType::Note))
        .finish()
        .print((
            context.module_name.as_str(),
            Source::from(context.source_code.code.as_str()),
        ))
        .unwrap();
    }
}

pub(crate) fn not_separated_statement_error_1(
    token: &Token,
    context: &TranspileModuleContext,
) -> TranspileError {
    let mut error = TranspileError::new(NotSeparatedStatement {
        token: Spanned::new(token.text.to_string(), token.span.clone()),
    });
    let advice = Advice::Add {
        add: ";".to_string(),
        position: token.span.start,
        message_override: None,
    };
    error.add_advice(context.module_name.clone(), advice);
    return error;
}

pub(crate) struct StatementAttributesWithoutDefine {
    attributes_span: Range<usize>,
}

impl TranspileReport for StatementAttributesWithoutDefine {
    fn print(&self, context: &TranspileModuleContext) {
        let text = &context.context.localized_text;
        let key = ErrorMessageKey::new(0004);

        Report::build(
            ReportKind::Error,
            context.module_name.as_str(),
            self.attributes_span.start,
        )
        .with_code(4)
        .with_message(key.get_massage(text, ErrorMessageType::Message))
        .with_label(
            Label::new((context.module_name.as_str(), self.attributes_span.clone()))
                .with_message(key.get_massage(text, ErrorMessageType::Label(0)))
                .with_color(Color::Red),
        )
        .with_note(key.get_massage(text, ErrorMessageType::Note))
        .finish()
        .print((
            context.module_name.as_str(),
            Source::from(context.source_code.code.as_str()),
        ))
        .unwrap()
    }
}

pub(crate) fn statement_attributes_without_define(
    attributes: &Vec<StatementAttribute, &Bump>,
) -> TranspileError {
    let span_start = attributes.first().unwrap().span.start;
    let span_end = attributes.last().unwrap().span.end;
    TranspileError::new(StatementAttributesWithoutDefine {
        attributes_span: span_start..span_end,
    })
}

