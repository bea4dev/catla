use std::sync::LazyLock;

use allocator_api2::vec::Vec;
use bumpalo::Bump;
use catla_parser::parser::{FunctionDefine, Spanned, TranspilerTag, UserTypeDefine};
use fxhash::FxHashMap;
use print::PrintFunctionCodeGenerator;
use string::StringCodeGenerator;

use crate::transpiler::{
    codegen::{CodeBuilder, StackAllocCodeBuilder},
    component::EntityID,
    context::TranspileModuleContext,
    name_resolver::FoundDefineInfo,
    optimizer::lifetime_analyzer::LifetimeAnalyzeResults,
    semantics::types::type_inference::TypeInferenceResultContainer,
    TranspileError,
};

use super::CustomCodeGeneratorHolder;

pub mod print;
pub mod string;

static RUST_CODEGEN_FUNCTIONS: LazyLock<Box<[CustomCodeGeneratorHolder<FunctionDefine>]>> =
    LazyLock::new(|| {
        Box::new([CustomCodeGeneratorHolder::new(
            PrintFunctionCodeGenerator {},
        )])
    });
static RUST_CODEGEN_USER_TYPES: LazyLock<Box<[CustomCodeGeneratorHolder<UserTypeDefine>]>> =
    LazyLock::new(|| Box::new([CustomCodeGeneratorHolder::new(StringCodeGenerator {})]));

pub(crate) fn rust_codegen_function<'allocator>(
    ast: &FunctionDefine,
    tags: &Vec<&TranspilerTag, &'allocator Bump>,
    result_bind_var_name: Option<&str>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_analyze_results: &LifetimeAnalyzeResults,
    import_element_map: &FxHashMap<EntityID, Spanned<std::string::String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    top_stack_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    current_tree_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    code_builder: &mut CodeBuilder<'allocator>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    context: &TranspileModuleContext,
) {
    for generator in RUST_CODEGEN_FUNCTIONS.iter() {
        generator.codegen(
            ast,
            tags,
            result_bind_var_name,
            type_inference_result,
            lifetime_analyze_results,
            import_element_map,
            name_resolved_map,
            top_stack_alloc_builder,
            current_tree_alloc_builder,
            code_builder,
            allocator,
            errors,
            context,
        );
    }
}

pub(crate) fn rust_codegen_user_type<'allocator>(
    ast: &UserTypeDefine,
    tags: &Vec<&TranspilerTag, &'allocator Bump>,
    result_bind_var_name: Option<&str>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_analyze_results: &LifetimeAnalyzeResults,
    import_element_map: &FxHashMap<EntityID, Spanned<std::string::String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    top_stack_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    current_tree_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    code_builder: &mut CodeBuilder<'allocator>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    context: &TranspileModuleContext,
) {
    for generator in RUST_CODEGEN_USER_TYPES.iter() {
        generator.codegen(
            ast,
            tags,
            result_bind_var_name,
            type_inference_result,
            lifetime_analyze_results,
            import_element_map,
            name_resolved_map,
            top_stack_alloc_builder,
            current_tree_alloc_builder,
            code_builder,
            allocator,
            errors,
            context,
        );
    }
}
