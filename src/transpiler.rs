use std::sync::Arc;

use allocator_api2::vec;
use allocator_api2::vec::Vec;
use ariadne::Color;
use async_recursion::async_recursion;
use bumpalo::Bump;
use catla_parser::parser::{parse_source, Spanned};
use either::Either;
use error::SimpleError;
use fxhash::FxHashMap;
use optimizer::optimize;
use semantics::types::{
    type_inference::{infer_type_program, TypeInferenceResultContainer},
    type_info::{collect_duplicated_implementation_error, ScopeThisType, Type, WithDefineInfo},
};

use crate::transpiler::semantics::types::{
    import_module_collector::collect_import_module_program, type_inference::TypeEnvironment,
    user_type_element_collector::collect_module_element_types_program,
};

use self::{
    advice::{Advice, AdviceReport},
    component::ComponentContainer,
    context::{TranspileContext, TranspileModuleContext},
    error::TranspileReport,
    name_resolver::name_resolve_program,
    parse_error::collect_parse_error_program,
    semantics::{
        syntax_validation::validate_syntax_program,
        types::{type_define_collector::collect_user_type_program, type_info::ImplementsInfoSet},
    },
};

pub mod advice;
pub mod component;
pub mod context;
pub mod error;
pub mod future;
pub mod name_resolver;
pub mod optimizer;
pub mod parse_error;
pub mod resource;
pub mod semantics;

pub struct TranspileError(Box<dyn TranspileReport + Send>, AdviceReport);
pub struct TranspileWarning(Box<dyn TranspileReport + Send>, AdviceReport);

impl TranspileError {
    pub(crate) fn new<T: TranspileReport + Send + 'static>(report: T) -> TranspileError {
        Self(Box::new(report), AdviceReport::new())
    }

    pub(crate) fn add_advice(&mut self, module_name: Arc<String>, advice: Advice) {
        self.1.add_advice(module_name, advice);
    }
}

impl TranspileReport for TranspileError {
    fn print(&self, context: &TranspileModuleContext) {
        self.0.print(context);
        self.1.print(context);
    }
}

impl TranspileWarning {
    pub(crate) fn new<T: TranspileReport + Send + 'static>(report: T) -> TranspileWarning {
        Self(Box::new(report), AdviceReport::new())
    }

    pub(crate) fn add_advice(&mut self, module_name: Arc<String>, advice: Advice) {
        self.1.add_advice(module_name, advice);
    }
}

impl TranspileReport for TranspileWarning {
    fn print(&self, context: &TranspileModuleContext) {
        self.0.print(context);
        self.1.print(context);
    }
}

pub struct SourceCode {
    pub code: String,
    pub module_name: String,
}

const TRANSPILE_PHASE_ANALYZE_SEMANTICS: usize = 0;
const TRANSPILE_PHASE_LIFETIME_EVAL: usize = 1;
const TRANSPILE_PHASE_CODE_GEN: usize = 2;

pub fn transpile(entry_module_name: String, context: Arc<TranspileContext>) -> Result<(), String> {
    let module_context =
        TranspileContext::try_create_module_context(&context, &entry_module_name).unwrap()?;

    context.transpile_phase_future.mark_as_started();

    let transpile_context = context.clone();

    context.future_runtime.block_on(async move {
        transpile_context.future_runtime.spawn(async {
            transpile_module(entry_module_name, module_context).await;
        });

        // await until all of semantic analyze task is finished
        transpile_context
            .transpile_phase_future
            .phase(TRANSPILE_PHASE_ANALYZE_SEMANTICS)
            .future()
            .await;

        // run lifetime evaluator
        transpile_context
            .transpile_phase_future
            .phase(TRANSPILE_PHASE_LIFETIME_EVAL)
            .force_finish()
            .await;

        // await until all of code gen task is finished
        transpile_context
            .transpile_phase_future
            .phase(TRANSPILE_PHASE_CODE_GEN)
            .future()
            .await;
    });

    Ok(())
}

#[async_recursion]
async fn transpile_module(module_name: String, module_context: Arc<TranspileModuleContext>) {
    let source = module_context.source_code.code.as_str();

    let allocator = Bump::new();

    // This 'ast' must not be modified after this.(Of course, interior mutability is also not good.)
    // Because, 'ProgramAST' is implemented 'Send' and 'Sync' by 'unsafe impl'.
    // No one can guarantee your memory safety, unless you follow these rules.
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
            false,
            &mut name_environments,
            &mut name_resolved_map,
            &mut errors,
            &mut warnings,
            &allocator,
        );
    }

    let mut import_element_map = FxHashMap::default();
    collect_import_module_program(
        ast,
        &mut import_element_map,
        &name_resolved_map,
        &mut errors,
        &mut warnings,
        &module_context,
    );

    let context = &module_context.context;

    let import_modules = import_element_map
        .values()
        .cloned()
        .chain(
            context
                .auto_import
                .auto_import_modules
                .iter()
                .filter(|&name| name != &module_name)
                .map(|name| Spanned::new(name.clone(), 0..0)),
        )
        .collect::<Vec<_>>();

    // start transpile imported modules
    for module_name in import_modules.iter() {
        let new_module_context = TranspileContext::try_create_module_context(
            &module_context.context,
            &module_name.value,
        );

        if let Some(new_module_context) = new_module_context {
            let module_context = match new_module_context {
                Ok(module_context) => module_context,
                Err(error) => {
                    let error = SimpleError::new(
                        0070,
                        module_name.span.clone(),
                        vec![
                            (
                                context
                                    .source_code_provider
                                    .get_source_code_path(&module_name.value),
                                Color::Yellow,
                            ),
                            (error, Color::Red),
                        ],
                        vec![(
                            (module_context.module_name.clone(), module_name.span.clone()),
                            Color::Red,
                        )],
                    );
                    errors.push(error);
                    continue;
                }
            };
            let module_name = module_name.value.clone();

            context.transpile_phase_future.mark_as_started();

            context
                .future_runtime
                .spawn(async move { transpile_module(module_name, module_context).await });
        }
    }

    validate_syntax_program(
        ast,
        &module_context,
        None,
        false,
        &mut errors,
        &mut warnings,
    );

    let mut user_type_map = FxHashMap::default();
    collect_user_type_program(ast, &mut user_type_map, &module_context);

    let user_type_map = Arc::new(user_type_map);
    module_context
        .user_type_future
        .complete(user_type_map.clone())
        .await;

    let mut module_user_type_map = FxHashMap::default();
    for module_name in import_modules.iter() {
        let module_context = context.get_module_context(&module_name.value).unwrap();
        let user_type_map = module_context.user_type_future.get().await;

        module_user_type_map.insert(module_name.value.clone(), user_type_map);
    }

    let mut module_element_type_map = FxHashMap::default();
    let mut generics_map = FxHashMap::default();
    let mut module_entity_type_map = FxHashMap::default();
    let mut implements_infos = ImplementsInfoSet::new();
    collect_module_element_types_program(
        ast,
        &user_type_map,
        &import_element_map,
        &name_resolved_map,
        &module_user_type_map,
        &mut module_element_type_map,
        &mut generics_map,
        &mut module_entity_type_map,
        &mut implements_infos,
        &ScopeThisType::new(Type::Unknown),
        &mut errors,
        &mut warnings,
        None,
        &module_context,
    );

    let module_type_info = Arc::new(module_element_type_map);
    let implements_infos = Arc::new(implements_infos);
    //dbg!(&module_element_type_map);
    module_context
        .module_element_type_future
        .complete(module_type_info.clone())
        .await;
    module_context
        .module_type_implements_infos
        .complete(implements_infos.clone())
        .await;

    let mut module_element_type_maps = FxHashMap::default();
    let mut merged_implements_infos = ImplementsInfoSet::new();

    for module_name in import_modules.iter() {
        let module_context = context.get_module_context(&module_name.value).unwrap();
        let module_element_type_map = module_context.module_element_type_future.get().await;
        let module_type_implements_infos = module_context.module_type_implements_infos.get().await;

        module_element_type_maps.insert(module_name.value.clone(), module_element_type_map);
        merged_implements_infos.merge(&module_type_implements_infos);
    }

    collect_duplicated_implementation_error(
        &implements_infos,
        &merged_implements_infos,
        &mut errors,
    );

    merged_implements_infos.merge(&implements_infos);

    if module_context.context.settings.is_transpiler_debug
        && module_context.module_name.as_str() == "test::test"
    {
        std::thread::sleep(std::time::Duration::from_millis(1000));
    }

    let mut type_inference_results = TypeInferenceResultContainer::default();
    {
        // This is not implemented 'Send'.
        let mut type_environment = TypeEnvironment::new_with_return_type(
            Either::Right(WithDefineInfo {
                value: Type::Unit,
                module_name: module_context.module_name.clone(),
                span: ast.span.clone(),
            }),
            &allocator,
        );

        infer_type_program(
            ast,
            &user_type_map,
            &import_element_map,
            &name_resolved_map,
            &module_user_type_map,
            &module_type_info,
            &module_element_type_maps,
            &generics_map,
            &module_entity_type_map,
            &merged_implements_infos,
            &ScopeThisType::new(Type::Unknown),
            &None,
            false,
            false,
            &Vec::new(),
            false,
            &mut type_environment,
            &mut type_inference_results,
            &allocator,
            &mut errors,
            &mut warnings,
            &module_context,
        );

        merged_implements_infos.validate_super_type(&mut errors, &module_context, &allocator);

        type_environment.collect_info(
            &mut type_inference_results,
            &merged_implements_infos,
            &mut errors,
            &mut warnings,
            &module_context,
            &allocator,
        );
    }

    let optimize_result = optimize(
        ast,
        &import_element_map,
        &name_resolved_map,
        &module_user_type_map,
        &module_type_info,
        &module_element_type_maps,
        &type_inference_results,
        &allocator,
        &module_context,
    );

    module_context
        .context
        .transpile_phase_future
        .phase(TRANSPILE_PHASE_ANALYZE_SEMANTICS)
        .mark_as_finished()
        .await;

    // await until the lifetime evaluator is finished
    module_context
        .context
        .transpile_phase_future
        .phase(TRANSPILE_PHASE_LIFETIME_EVAL)
        .future()
        .await;

    module_context
        .context
        .add_error_and_warning(module_name, errors, warnings);

    // TODO : Code gen

    module_context
        .context
        .transpile_phase_future
        .phase(TRANSPILE_PHASE_CODE_GEN)
        .mark_as_finished()
        .await;
}
