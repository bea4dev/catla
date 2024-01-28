use std::{path::Path, sync::Arc};

use bumpalo::Bump;
use catla_parser::parser::parse_source;

use self::{context::{TranspileModuleContext, TranspileContext}, error::TranspileReport, component::{EntityIDMapper, NameEnvironment}, parse_error::{collect_parse_error_program, misc::Advice}};

pub mod component;
pub mod name_resolver;
pub mod error;
pub mod context;
pub mod parse_error;



pub struct TranspileResult {
    pub module_context: Arc<TranspileModuleContext>,
    pub errors: Vec<TranspileError>,
    pub warnings: Vec<TranspileWarning>
}


pub struct TranspileError(pub Box<dyn TranspileReport>);
pub struct TranspileWarning(pub Box<dyn TranspileReport>);

impl TranspileError {

    pub(crate) fn new<T: TranspileReport + 'static>(report: T) -> TranspileError {
        return Self(Box::new(report))
    }

    pub(crate) fn add_advice(&mut self, module_name: String, advice: Advice) {
        self.0.add_advice(module_name, advice);
    }

}

impl TranspileWarning {

    pub(crate) fn new<T: TranspileReport + 'static>(report: T) -> TranspileWarning {
        return Self(Box::new(report))
    }

    pub(crate) fn add_advice(&mut self, module_name: String, advice: Advice) {
        self.0.add_advice(module_name, advice);
    }

}


pub struct SourceCode {
    pub code: String,
    pub module_name: String,
    pub path: Option<Box<Path>>
}


pub fn transpile(source_code: SourceCode, context: Arc<TranspileContext>) -> TranspileResult {

    return transpile_module(Arc::new(source_code), context);

}


fn transpile_module(
    source_code: Arc<SourceCode>,
    context: Arc<TranspileContext>,
) -> TranspileResult {
    let allocator = Bump::new();

    let module_name = source_code.module_name.clone();
    let module_context = Arc::new(
        TranspileModuleContext { source_code, module_name: module_name.clone(), context: context.clone() }
    );
    let source = module_context.source_code.code.as_str();

    context.register_module_context(module_name, module_context.clone());

    let ast = parse_source(source, &allocator);

    let mut errors = Vec::new();
    let mut warnings = Vec::new();

    collect_parse_error_program(ast, &mut errors, &mut warnings, &module_context);

    let mut id_mapper = EntityIDMapper::new(&allocator);
    
    let mut name_environment = NameEnvironment::new(None, &allocator);

    return TranspileResult { module_context, errors, warnings }
}