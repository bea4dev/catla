use std::sync::Arc;

use crate::localize::localizer::LocalizedText;

use super::SourceCode;


pub struct TranspileContext {
    pub settings: TranspileSettings,
    pub(crate) localized_text: LocalizedText
}

impl TranspileContext {
    
    pub fn new(settings: TranspileSettings) -> Arc<TranspileContext> {
        let localized_text = LocalizedText::new(&settings.lang);
        return Arc::new(Self {
            settings,
            localized_text
        });
    }

}


pub struct TranspileSettings {
    pub lang: String
}


pub struct TranspileModuleContext {
    pub source_code: SourceCode,
    pub module_name: String,
    pub context: Arc<TranspileContext>
}
