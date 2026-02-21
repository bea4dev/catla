use std::{ops::Range, sync::Arc};

use allocator_api2::vec::Vec;
use ariadne::{sources, Color, Fmt, Label, Report, ReportKind};

use crate::localize::localizer::LocalizedText;

use super::{context::TranspileModuleContext, TranspileError};


pub trait TranspileReport {

    fn print(&self, context: &TranspileModuleContext);

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
    message_replace: Vec<(String, Color)>,
    labels: Vec<((Arc<String>, Range<usize>), Color)>
}

impl SimpleError {
    pub(crate) fn new(
        error_code: usize,
        message_span: Range<usize>,
        message_replace: Vec<(String, Color)>,
        labels: Vec<((Arc<String>, Range<usize>), Color)>
    ) -> TranspileError {
        TranspileError::new(SimpleError { error_code, message_span, message_replace, labels })
    }
}

impl TranspileReport for SimpleError {
    fn print(&self, context: &TranspileModuleContext) {
        let module_name = &context.module_name;
        let text = &context.context.localized_text;
        let key = ErrorMessageKey::new(self.error_code);

        fn replace_message(mut message: String, message_replace: &Vec<(String, Color)>) -> String {
            for i in 0..message_replace.len() {
                let target = format!("%{}", i);
                let (replace_message, color) = &message_replace[i];
                message = message.replace(&target, replace_message.clone().fg(*color).to_string().as_str());
            }
            message
        }

        let mut message = key.get_massage(text, ErrorMessageType::Message);
        message = replace_message(message, &self.message_replace);

        let mut builder = Report::build(ReportKind::Error, module_name.as_ref(), self.message_span.start)
            .with_code(self.error_code)
            .with_message(message);

        let mut index = 0;
        for ((module_name, span), color) in self.labels.iter() {
            let mut message = key.get_massage(text, ErrorMessageType::Label(index));
            message = replace_message(message, &self.message_replace);

            builder.add_label(
                Label::new((module_name.as_ref().clone(), span.clone()))
                    .with_color(*color)
                    .with_message(message)
            );
            index += 1;
        }

        if let Some(note) = key.get_massage_optional(text, ErrorMessageType::Note) {
            let message = replace_message(note, &self.message_replace);
            builder.set_note(message);
        }

        if let Some(help) = key.get_massage_optional(text, ErrorMessageType::Help) {
            let message = replace_message(help, &self.message_replace);
            builder.set_help(message);
        }
        
        let mut module_names = self.labels.iter()
            .map(|((module_name, _), _)| { module_name.as_ref().clone() })
            .collect::<Vec<_>>();
        module_names.push(module_name.as_ref().clone());
        
        let context = &context.context;
        let source_code_vec = module_names.iter()
            .map(|module_name| { context.get_module_context(module_name).unwrap().source_code.clone() })
            .collect::<Vec<_>>();
        
        let sources_vec = module_names.into_iter().zip(source_code_vec.iter())
            .map(|(module_name, source_code)| { (module_name, source_code.code.as_str()) })
            .collect::<Vec<_>>();

        builder.finish().print(sources(sources_vec)).unwrap();
    }
}
