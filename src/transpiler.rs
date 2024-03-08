use std::sync::Arc;

use async_recursion::async_recursion;
use bumpalo::Bump;
use catla_parser::parser::parse_source;
use fxhash::FxHashMap;

use crate::transpiler::semantics::types::{import_module_collector::collect_import_module_program, user_type_element_collector::collect_module_element_types_program};

use self::{advice::Advice, component::ComponentContainer, context::{try_create_module_context, TranspileContext, TranspileModuleContext}, error::TranspileReport, name_resolver::name_resolve_program, parse_error::collect_parse_error_program, semantics::{syntax_validation::validate_syntax_program, types::type_define_collector::collect_user_type_program}};

pub mod component;
pub mod name_resolver;
pub mod error;
pub mod context;
pub mod parse_error;
pub mod advice;
pub mod semantics;
pub mod future;
pub mod resource;


pub struct TranspileError(pub Box<dyn TranspileReport + Send>);
pub struct TranspileWarning(pub Box<dyn TranspileReport + Send>);

impl TranspileError {

    pub(crate) fn new<T: TranspileReport + Send + 'static>(report: T) -> TranspileError {
        Self(Box::new(report))
    }

    pub(crate) fn add_advice(&mut self, module_name: String, advice: Advice) {
        self.0.add_advice(module_name, advice);
    }

}

impl TranspileWarning {

    pub(crate) fn new<T: TranspileReport + Send + 'static>(report: T) -> TranspileWarning {
        Self(Box::new(report))
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
    let module_context = try_create_module_context(&context, &entry_module_name).unwrap();

    context.future_runtime.block_on(async move {
        transpile_module(entry_module_name, module_context).await;
    });
}


#[async_recursion]
async fn transpile_module(
    module_name: String,
    module_context: Arc<TranspileModuleContext>,
) {
    let source = module_context.source_code.code.as_str();

    let allocator = Bump::new();

    // This 'ast' must not be modified after this.(Of course, interior mutability is also not good.)
    // Because, 'ProgramAST' is implemented 'Send' and 'Sync' by 'unsafe impl'.
    // No one can guarantee your memory safety.
    let ast = parse_source(source, &allocator);

    let mut errors = Vec::new();
    let mut warnings = Vec::new();

    collect_parse_error_program(ast, &mut errors, &mut warnings, &module_context);
    
    let mut name_resolved_map = FxHashMap::default();
    {
        // This is not implemented 'Send'.
        // So, this cannot live across 'await'.
        let mut name_environments = ComponentContainer::new(&allocator);
        name_resolve_program(
            ast,
            None,
            &mut name_environments,
            &mut name_resolved_map,
            &mut errors,
            &mut warnings,
            &allocator
        );
    }

    let mut import_element_map = FxHashMap::default();
    collect_import_module_program(ast, &mut import_element_map, &name_resolved_map, &mut errors, &mut warnings, &module_context);

    // start transpiling imported modules
    let context = &module_context.context;
    for module_name in import_element_map.values() {
        let module_context = try_create_module_context(&module_context.context, module_name);

        if let Some(module_context) = module_context {
            let module_name = module_name.clone();

            context.future_runtime.spawn(async move {
                transpile_module(module_name, module_context).await
            });
        }
    }

    validate_syntax_program(ast, &module_context, None, &mut errors, &mut warnings);

    let mut user_type_map = FxHashMap::default();
    collect_user_type_program(ast, &mut user_type_map, &module_context);

    let user_type_map = Arc::new(user_type_map);
    module_context.user_type_future.complete(user_type_map.clone()).await;

    let mut module_user_type_map = FxHashMap::default();
    for module_name in import_element_map.values() {
        let module_context = context.get_module_context(module_name).unwrap();
        let user_type_map = module_context.user_type_future.get().await;

        module_user_type_map.insert(module_name.clone(), user_type_map);
    }

    let mut module_element_type_map = FxHashMap::default();
    let mut generics_map = FxHashMap::default();
    collect_module_element_types_program(
        ast,
        &user_type_map,
        &import_element_map,
        &name_resolved_map,
        &module_user_type_map,
        &mut module_element_type_map,
        &mut generics_map,
        &mut errors,
        &mut warnings,
        None,
        &module_context
    );

    let module_element_type_map = Arc::new(module_element_type_map);
    dbg!(&module_element_type_map);
    module_context.module_element_type_future.complete(module_element_type_map).await;

    let mut module_element_type_maps = FxHashMap::default();
    for module_name in import_element_map.values() {
        let module_context = context.get_module_context(module_name).unwrap();
        let module_element_type_map = module_context.module_element_type_future.get().await;

        module_element_type_maps.insert(module_name.clone(), module_element_type_map);
    }

    module_context.context.add_error_and_warning(module_name, errors, warnings);
}