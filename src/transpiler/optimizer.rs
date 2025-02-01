use std::sync::Arc;

use bumpalo::Bump;
use catla_parser::parser::{Program, Spanned};
use fxhash::FxHashMap;
use lifetime_analyzer::collect_lifetime;
use move_optimization::{collect_last_users, LastUsers};

use super::{
    component::EntityID,
    context::TranspileModuleContext,
    name_resolver::FoundDefineInfo,
    semantics::types::{type_inference::TypeInferenceResultContainer, type_info::Type},
};

pub mod lifetime_analyzer;
pub mod move_optimization;
pub mod function_equals;
pub mod function_recursive;

pub struct OptimizeResultContainer {
    pub last_users: LastUsers,
}

pub fn optimize<'allocator>(
    ast: Program<'_, 'allocator>,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    type_inference_result: &TypeInferenceResultContainer,
    allocator: &'allocator Bump,
    context: &TranspileModuleContext,
) -> OptimizeResultContainer {
    let optimization_settings = &context.context.settings.optimization;

    if optimization_settings.lifetime_analyzer {
        let lifetime_source_map = collect_lifetime(
            ast,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            type_inference_result,
            allocator,
            context,
        );

        context
            .context
            .lifetime_evaluator
            .add_sources(context.module_name.clone(), lifetime_source_map);
    }

    let last_users = if optimization_settings.move_optimizer {
        collect_last_users(ast, name_resolved_map)
    } else {
        LastUsers::new()
    };

    OptimizeResultContainer { last_users }
}
