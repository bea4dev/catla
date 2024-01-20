use ariadne::Report;

use crate::localize::localizer::LocalizedText;

use super::context::TranspileModuleContext;


pub trait TranspileReport {

    fn print(&self, context: &TranspileModuleContext);

}


pub(crate) struct ErrorMessageKey {
    error_code: usize
}

impl ErrorMessageKey {
    
    pub(crate) fn new(error_code: usize) -> ErrorMessageKey {
        return Self { error_code };
    }

    pub(crate) fn get_massage(&self, localized_text: &LocalizedText, ty: ErrorMessageType) -> String {
        return localized_text.get_text(format!("error.{:>04}.{}", self.error_code, ty.get()))
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
        return match self {
            ErrorMessageType::Message => "message".to_string(),
            ErrorMessageType::Label(number) => format!("label_{}", number),
            ErrorMessageType::Note => "note".to_string(),
            ErrorMessageType::Help => "help".to_string()
        }
    }
    
}