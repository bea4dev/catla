use std::{collections::{HashMap, HashSet}, sync::{Arc, Mutex}};

use tokio::runtime::{Builder, Runtime};

use crate::localize::localizer::LocalizedText;

use super::{future::SharedManualFuture, resource::SourceCodeProvider, semantics::types::type_info::Type, SourceCode, TranspileError, TranspileWarning};


pub struct TranspileContext {
    pub settings: TranspileSettings,
    pub(crate) localized_text: LocalizedText,
    pub source_code_provider: Box<dyn SourceCodeProvider>,
    pub module_context_map: Mutex<HashMap<String, Arc<TranspileModuleContext>>>,
    error_and_warnings: Mutex<HashMap<String, (Vec<TranspileError>, Vec<TranspileWarning>)>>,
    pub transpiling_modules: Mutex<HashSet<String>>,
    pub(crate) future_runtime: Runtime,
}

impl TranspileContext {
    
    pub fn new(settings: TranspileSettings, source_code_provider: impl SourceCodeProvider + 'static) -> Arc<TranspileContext> {
        let localized_text = LocalizedText::new(&settings.lang);
        let future_runtime = Builder::new_multi_thread()
            .worker_threads(settings.num_threads)
            .enable_all()
            .build()
            .unwrap();
        return Arc::new(Self {
            settings,
            localized_text,
            source_code_provider: Box::new(source_code_provider),
            module_context_map: Mutex::new(HashMap::new()),
            error_and_warnings: Mutex::new(HashMap::new()),
            transpiling_modules: Mutex::new(HashSet::new()),
            future_runtime
        });
    }

    pub(crate) fn register_module_context(&self, module_name: String, module_context: Arc<TranspileModuleContext>) {
        let mut module_context_map = self.module_context_map.lock().unwrap();
        module_context_map.insert(module_name, module_context);
    }

    pub fn get_module_context(&self, module_name: &String) -> Option<Arc<TranspileModuleContext>> {
        return self.module_context_map.lock().unwrap().get(module_name).cloned();
    }

    pub fn try_mark_as_transpiling(&self, module_name: &String) -> bool {
        let mut transpiling_modules = self.transpiling_modules.lock().unwrap();
        if transpiling_modules.contains(module_name) {
            return false;
        }
        transpiling_modules.insert(module_name.clone());
        return true;
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
