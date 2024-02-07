use std::ops::Range;

use ariadne::{Report, ReportKind, Label, Source, Color};
use catla_parser::{lexer::Token, parser::{Spanned, StatementAttribute}};

use crate::transpiler::{advice::{Advice, AdviceReport}, context::TranspileModuleContext, error::{TranspileReport, ErrorMessageKey, ErrorMessageType}, TranspileError};


pub(crate) struct NotSeparatedStatement {
    token: Spanned<String>,
    advice_report: AdviceReport
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
            .unwrap();
        
        self.advice_report.print(context, self.token.span.start);
    }

    fn add_advice(&mut self, module_name: String, advice: Advice) {
        self.advice_report.add_advice(module_name, advice);
    }
}

pub(crate) fn not_separated_statement_error_1(token: &Token, context: &TranspileModuleContext) -> TranspileError {
    let mut error = TranspileError::new(NotSeparatedStatement {
        token: Spanned::new(token.text.to_string(), token.span.clone()),
        advice_report: AdviceReport::new()
    });
    let advice = Advice::Add { add: ";".to_string(), position: token.span.start, message_override: None };
    error.add_advice(context.module_name.clone(), advice);
    return error;
}



pub(crate) struct StatementAttributesWithoutDefine {
    attributes_span: Range<usize>,
    advice_report: AdviceReport
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

    fn add_advice(&mut self, module_name: String, advice: Advice) {
        self.advice_report.add_advice(module_name, advice);
    }
}

pub(crate) fn statement_attributes_without_define(attributes: &bumpalo::collections::Vec<StatementAttribute>) -> TranspileError {
    let span_start = attributes.first().unwrap().span.start;
    let span_end = attributes.last().unwrap().span.end;
    return TranspileError::new(StatementAttributesWithoutDefine {
        attributes_span: span_start..span_end,
        advice_report: AdviceReport::new()
    });
}