use std::{collections::HashMap, sync::{Arc, Mutex}};

use manual_future::ManualFuture;
use tokio::runtime::{Builder, Runtime};

use crate::localize::localizer::LocalizedText;

use super::{future::SharedManualFuture, semantics::types::type_info::Type, SourceCode, TranspileError, TranspileWarning};


pub struct TranspileContext {
    pub settings: TranspileSettings,
    pub(crate) localized_text: LocalizedText,
    pub module_context_map: Mutex<HashMap<String, Arc<TranspileModuleContext>>>,
    error_and_warnings: Mutex<HashMap<String, (Vec<TranspileError>, Vec<TranspileWarning>)>>,
    pub(crate) future_runtime: Runtime
}

impl TranspileContext {
    
    pub fn new(settings: TranspileSettings) -> Arc<TranspileContext> {
        let localized_text = LocalizedText::new(&settings.lang);
        let future_runtime = Builder::new_multi_thread()
            .worker_threads(settings.num_threads)
            .enable_all()
            .build()
            .unwrap();
        return Arc::new(Self {
            settings,
            localized_text,
            module_context_map: Mutex::new(HashMap::new()),
            error_and_warnings: Mutex::new(HashMap::new()),
            future_runtime
        });
    }

    pub(crate) fn register_module_context(&self, module_name: String, module_context: Arc<TranspileModuleContext>) {
        self.module_context_map.lock().unwrap().insert(module_name, module_context);
    }

    pub fn get_module_context(&self, module_name: &String) -> Option<Arc<TranspileModuleContext>> {
        return self.module_context_map.lock().unwrap().get(module_name).cloned();
    }

    pub fn add_error_and_warning(&self, module_name: String, errors: Vec<TranspileError>, warnings: Vec<TranspileWarning>) {
        let mut map = self.error_and_warnings.lock().unwrap();
        let error_or_warnings = map.entry(module_name).or_insert_with(|| { (Vec::new(), Vec::new()) });
        error_or_warnings.0.extend(errors);
        error_or_warnings.1.extend(warnings);
    }

    pub fn print_report(&self) {
        let report_map = self.error_and_warnings.lock().unwrap();
        
        for entry in report_map.iter() {
            let module_context = self.get_module_context(entry.0).unwrap();

            for error in &entry.1.0 {
                error.0.print(&module_context);
                print!("\n");
            }

            for warning in &entry.1.1 {
                warning.0.print(&module_context);
                print!("\n");
            }
        }
    }

}


pub struct TranspileSettings {
    pub lang: String,
    pub num_threads: usize
}


pub struct TranspileModuleContext {
    pub source_code: Arc<SourceCode>,
    pub module_name: String,
    pub context: Arc<TranspileContext>,
    pub user_type_future: SharedManualFuture<HashMap<String, Type>>
}
