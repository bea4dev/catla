pub mod rust_codegen;

use std::mem::transmute;

use allocator_api2::vec::Vec;
use bumpalo::Bump;
use catla_parser::parser::{Spanned, TranspilerTag, AST};
use fxhash::FxHashMap;

use crate::transpiler::{
    component::EntityID, context::TranspileModuleContext, name_resolver::FoundDefineInfo,
    optimizer::lifetime_analyzer::LifetimeAnalyzeResults,
    semantics::types::type_inference::TypeInferenceResultContainer, TranspileError,
};

use super::{CodeBuilder, StackAllocCodeBuilder};

pub trait CustomCodeGenerator<T: AST>: Sync + Send {
    fn codegen<'allocator>(
        &self,
        ast: &T,
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
    );
}

pub struct CustomCodeGeneratorHolder<T: AST> {
    genarator: Box<dyn CustomCodeGenerator<T> + Send + Sync>,
}

impl<T: AST> CustomCodeGeneratorHolder<T> {
    pub fn new<G: CustomCodeGenerator<T> + 'static + Sync + Send>(generator: G) -> Self {
        Self {
            genarator: Box::new(generator) as _,
        }
    }

    pub fn codegen<'allocator, A: AST>(
        &self,
        ast: &A,
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
        if typeid::of::<T>() != typeid::of::<A>() {
            return;
        }

        let ast = unsafe { transmute::<&A, &T>(ast) };

        self.genarator.codegen(
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
