use std::{collections::HashMap, path::Path, sync::Arc};

use bumpalo::Bump;
use catla_parser::parser::parse_source;

use crate::transpiler::future::SharedManualFuture;

use self::{advice::Advice, component::ComponentContainer, context::{TranspileContext, TranspileModuleContext}, error::TranspileReport, name_resolver::name_resolve_program, parse_error::collect_parse_error_program, resource::SourceCodeProvider, semantics::{syntax_validation::validate_syntax_program, types::type_define_collector::collect_user_type_program}};

pub mod component;
pub mod name_resolver;
pub mod error;
pub mod context;
pub mod parse_error;
pub mod advice;
pub mod semantics;
pub mod future;
pub mod resource;



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
}


pub fn transpile(entry_module_name: String, context: Arc<TranspileContext>) {

    context.clone().future_runtime.block_on(async move {
        transpile_module(entry_module_name, context).await;
    });

}


async fn transpile_module(
    module_name: String,
    context: Arc<TranspileContext>,
) {
    if !context.try_mark_as_transpiling(&module_name) {
        return;
    }

    let code = context.source_code_provider.get_source_code(&module_name).unwrap();
    let source_code = SourceCode { code, module_name: module_name.clone() };

    let module_context = Arc::new(
        TranspileModuleContext {
            source_code: Arc::new(source_code),
            module_name: module_name.clone(),
            context: context.clone(),
            user_type_future: SharedManualFuture::new()
        }
    );
    let source = module_context.source_code.code.as_str();

    context.register_module_context(module_name.clone(), module_context.clone());

    let allocator = Bump::new();

    let ast = parse_source(source, &allocator);

    let mut errors = Vec::new();
    let mut warnings = Vec::new();

    collect_parse_error_program(ast, &mut errors, &mut warnings, &module_context);
    
    let mut name_environments = ComponentContainer::new(&allocator);

    name_resolve_program(ast, None, &mut name_environments, &mut errors, &mut warnings, &allocator);

    validate_syntax_program(ast, &module_context, None, &mut errors, &mut warnings);

    let mut user_type_map = HashMap::new();
    collect_user_type_program(ast, &mut user_type_map, &module_context);

    module_context.user_type_future.complete(user_type_map).await;

    dbg!(module_context.user_type_future.get().await);

    drop(name_environments);

    context.add_error_and_warning(module_name, errors, warnings);
}