use std::ops::Range;

use ariadne::{Color, Label, Report, ReportKind, Source};

use crate::localize::localizer::LocalizedText;

use super::{advice::{Advice, AdviceReport}, context::TranspileModuleContext, TranspileError};


pub trait TranspileReport {

    fn print(&self, context: &TranspileModuleContext);

    fn add_advice(&mut self, module_name: String, advice: Advice);

}


pub(crate) struct ErrorMessageKey {
    error_code: usize
}

impl ErrorMessageKey {
    
    pub(crate) fn new(error_code: usize) -> ErrorMessageKey {
        Self { error_code }
    }

    pub(crate) fn get_massage(&self, localized_text: &LocalizedText, ty: ErrorMessageType) -> String {
        localized_text.get_text(format!("error.{:>04}.{}", self.error_code, ty.get()))
    }

    pub(crate) fn get_massage_optional(&self, localized_text: &LocalizedText, ty: ErrorMessageType) -> Option<String> {
        localized_text.get_text_optional(format!("error.{:>04}.{}", self.error_code, ty.get()))
    }

}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum ErrorMessageType {
    Message,
    Label(usize),
    Note,
    Help
}

impl ErrorMessageType {
    
    fn get(&self) -> String {
        match self {
            ErrorMessageType::Message => "message".to_string(),
            ErrorMessageType::Label(number) => format!("label_{}", number),
            ErrorMessageType::Note => "note".to_string(),
            ErrorMessageType::Help => "help".to_string()
        }
    }
    
}


pub(crate) struct SimpleError {
    error_code: usize,
    message_span: Range<usize>,
    message_replace: Vec<String>,
    labels: Vec<(Range<usize>, Color)>,
    advice_report: AdviceReport
}

impl SimpleError {
    pub(crate) fn new(
        error_code: usize,
        message_span: Range<usize>,
        message_replace: Vec<String>,
        labels: Vec<(Range<usize>, Color)>
    ) -> TranspileError {
        TranspileError::new(SimpleError { error_code, message_span, message_replace, labels, advice_report: AdviceReport::new() })
    }
}

impl TranspileReport for SimpleError {
    fn print(&self, context: &TranspileModuleContext) {
        let module_name = &context.module_name;
        let text = &context.context.localized_text;
        let key = ErrorMessageKey::new(self.error_code);

        let mut message = key.get_massage(text, ErrorMessageType::Message);
        for i in 0..self.message_replace.len() {
            let target = format!("%{}", i);
            message = message.replace(&target, &self.message_replace[i]);
        }

        let mut builder = Report::build(ReportKind::Error, module_name, self.message_span.start)
            .with_code(self.error_code)
            .with_message(message);

        let mut index = 0;
        for label in self.labels.iter() {
            let mut message = key.get_massage(text, ErrorMessageType::Label(index));

            for i in 0..self.message_replace.len() {
                let target = format!("%{}", i);
                message = message.replace(&target, &self.message_replace[i]);
            }

            builder.add_label(
                Label::new((module_name, label.0.clone()))
                    .with_color(label.1.clone())
                    .with_message(message)
            );
            index += 1;
        }

        if let Some(note) = key.get_massage_optional(text, ErrorMessageType::Note) {
            builder.set_note(note);
        }

        if let Some(help) = key.get_massage_optional(text, ErrorMessageType::Help) {
            builder.set_help(help);
        }

        builder.finish().print((module_name, Source::from(context.source_code.code.as_str()))).unwrap();

        self.advice_report.print(context, self.message_span.start);
    }

    fn add_advice(&mut self, module_name: String, advice: Advice) {
        self.advice_report.add_advice(module_name, advice);
    }
}