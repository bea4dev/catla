use allocator_api2::vec;
use allocator_api2::vec::Vec;
use ariadne::Color;
use bumpalo::{collections::String, Bump};
use catla_parser::parser::{Spanned, TranspilerTag, UserTypeDefine};
use fxhash::FxHashMap;

use crate::transpiler::{
    codegen::{
        custom::CustomCodeGenerator, program::RUST_CODEGEN_TAG, CodeBuilder, StackAllocCodeBuilder,
    },
    component::EntityID,
    context::TranspileModuleContext,
    name_resolver::FoundDefineInfo,
    optimizer::lifetime_analyzer::LifetimeAnalyzeResults,
    semantics::types::type_inference::TypeInferenceResultContainer,
    SimpleError, TranspileError,
};

pub struct StringCodeGenerator {}

impl CustomCodeGenerator<UserTypeDefine<'_, '_>> for StringCodeGenerator {
    fn codegen<'allocator>(
        &self,
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
        let name = ast.name.as_ref().unwrap().value;

        match name {
            "String" => {
                code_builder.add_line(String::from_str_in(
                    "pub type String = catla_transpile_std::rust_codegen::string::String;",
                    allocator,
                ));
            }
            _ => {
                let mut rust_codegen_tag = None;
                for tag in tags.iter() {
                    if tag.literal.as_ref().unwrap().value == RUST_CODEGEN_TAG {
                        rust_codegen_tag = Some(tag);
                    }
                }

                let error_span = rust_codegen_tag.unwrap().span.clone();

                let error = TranspileError::new(SimpleError::new(
                    0082,
                    error_span.clone(),
                    vec![(RUST_CODEGEN_TAG.to_string(), Color::Red)],
                    vec![((context.module_name.clone(), error_span), Color::Red)],
                ));
                errors.push(error);
            }
        }
    }
}
