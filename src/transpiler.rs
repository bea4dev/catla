use std::{path::Path, sync::Arc};

use bumpalo::Bump;
use catla_parser::parser::parse_source;

use crate::localize::localizer::LocalizedText;

use self::{context::{TranspileSettings, TranspileModuleContext, TranspileContext}, error::TranspileReport, component::{ComponentContainer, EntityIDMapper, NameEnvironment}, parse_error::collect_parse_error_program};

pub mod component;
pub mod name_resolver;
pub mod error;
pub mod context;
pub mod parse_error;



pub struct TranspileResult {
    pub module_context: TranspileModuleContext,
    pub errors: Vec<TranspileError>,
    pub warnings: Vec<TranspileWarning>
}


pub struct TranspileError(pub Box<dyn TranspileReport>);
pub struct TranspileWarning(pub Box<dyn TranspileReport>);

impl TranspileError {
    pub fn new<T: TranspileReport + 'static>(report: T) -> TranspileError {
        return Self(Box::new(report))
    }
}

impl TranspileWarning {
    pub fn new<T: TranspileReport + 'static>(report: T) -> TranspileWarning {
        return Self(Box::new(report))
    }
}


#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    let module_context = TranspileModuleContext { source_code, module_name, context };
    let source = module_context.source_code.code.as_str();

    let ast = parse_source(source, &allocator);

    let mut errors = Vec::new();
    let mut warnings = Vec::new();

    collect_parse_error_program(ast, &mut errors, &mut warnings);

    let mut id_mapper = EntityIDMapper::new(&allocator);
    
    let mut name_environment = NameEnvironment::new(None, &allocator);

    return TranspileResult { module_context, errors, warnings }
}