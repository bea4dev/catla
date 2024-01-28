use std::sync::{Arc, Mutex};

use hashbrown::HashMap;

use crate::localize::localizer::LocalizedText;

use super::SourceCode;


pub struct TranspileContext {
    pub settings: TranspileSettings,
    pub(crate) localized_text: LocalizedText,
    pub module_context_map: Mutex<HashMap<String, Arc<TranspileModuleContext>>>
}

impl TranspileContext {
    
    pub(crate) fn new(settings: TranspileSettings) -> Arc<TranspileContext> {
        let localized_text = LocalizedText::new(&settings.lang);
        return Arc::new(Self {
            settings,
            localized_text,
            module_context_map: Mutex::new(HashMap::new())
        });
    }

    pub(crate) fn register_module_context(&self, module_name: String, module_context: Arc<TranspileModuleContext>) {
        self.module_context_map.lock().unwrap().insert(module_name, module_context);
    }

    pub(crate) fn get_module_context(&self, module_name: &String) -> Option<Arc<TranspileModuleContext>> {
        return self.module_context_map.lock().unwrap().get(module_name).cloned();
    }

}


pub struct TranspileSettings {
    pub lang: String
}


pub struct TranspileModuleContext {
    pub source_code: Arc<SourceCode>,
    pub module_name: String,
    pub context: Arc<TranspileContext>
}
