use std::{collections::HashMap, sync::{Arc, Mutex}};

use fxhash::FxHashMap;
use tokio::runtime::{Builder, Runtime};

use crate::{localize::localizer::LocalizedText, transpiler::error::TranspileReport};

use super::{future::SharedManualFuture, resource::SourceCodeProvider, semantics::types::type_info::{ImplementsInfoSet, Type}, SourceCode, TranspileError, TranspileWarning};


pub struct TranspileContext {
    pub settings: TranspileSettings,
    pub(crate) localized_text: LocalizedText,
    pub source_code_provider: Box<dyn SourceCodeProvider + Send + Sync>,
    pub module_context_map: Mutex<HashMap<String, Arc<TranspileModuleContext>>>,
    error_and_warnings: Mutex<HashMap<String, (Vec<TranspileError>, Vec<TranspileWarning>)>>,
    pub(crate) future_runtime: Runtime,
}

impl TranspileContext {
    
    pub fn new(
        settings: TranspileSettings,
        source_code_provider: impl SourceCodeProvider + Send + Sync + 'static
    ) -> Arc<TranspileContext> {

        let localized_text = LocalizedText::new(&settings.lang);
        let future_runtime = Builder::new_multi_thread()
            .worker_threads(settings.num_threads)
            .enable_all()
            .build()
            .unwrap();
        
        Arc::new(Self {
            settings,
            localized_text,
            source_code_provider: Box::new(source_code_provider),
            module_context_map: Mutex::new(HashMap::new()),
            error_and_warnings: Mutex::new(HashMap::new()),
            future_runtime
        })
    }

    pub fn get_module_context(&self, module_name: &String) -> Option<Arc<TranspileModuleContext>> {
        self.module_context_map.lock().unwrap().get(module_name).cloned()
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
                error.print(&module_context);
                print!("\n");
            }

            for warning in &entry.1.1 {
                warning.print(&module_context);
                print!("\n");
            }
        }
    }

    pub(crate) fn try_create_module_context(context: &Arc<TranspileContext>, module_name: &String) -> Option<Result<Arc<TranspileModuleContext>, String>> {
        let mut module_context_map = context.module_context_map.lock().unwrap();
    
        if module_context_map.contains_key(module_name) {
            return None;
        }
    
        let source_code = context.source_code_provider.get_source_code(&module_name).unwrap();

        let source_code = match source_code.as_ref() {
            Ok(source_code) => source_code.clone(),
            Err(error) => return Some(Err(error.to_string()))
        };
    
        let module_context = Arc::new(TranspileModuleContext {
            source_code,
            module_name: Arc::new(module_name.clone()),
            context: context.clone(),
            user_type_future: SharedManualFuture::new(),
            module_element_type_future: SharedManualFuture::new(),
            module_type_implements_infos: SharedManualFuture::new()
        });
        module_context_map.insert(module_name.clone(), module_context.clone());
        
        Some(Ok(module_context))
    }

}


pub struct TranspileSettings {
    pub lang: String,
    pub num_threads: usize
}


pub struct TranspileModuleContext {
    pub source_code: Arc<SourceCode>,
    pub module_name: Arc<String>,
    pub context: Arc<TranspileContext>,
    pub user_type_future: SharedManualFuture<FxHashMap<String, Type>>,
    pub module_element_type_future: SharedManualFuture<FxHashMap<String, Type>>,
    pub module_type_implements_infos: SharedManualFuture<ImplementsInfoSet>
}
