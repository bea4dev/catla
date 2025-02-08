use std::ops::Deref;

use ariadne::{Report, ReportKind, Source};
use catla_parser::parser::Program;

use crate::transpiler::context::TranspileModuleContext;

use super::{
    function_recursive::debug::print_recursive_functions,
    lifetime_analyzer::debug::print_lifetime_debug_info,
    variable_users::{
        debug::print_variable_user_info, variable_user_collector::VariableUserInfo,
    },
};

pub fn print_ast_optimizaion_info(
    ast: Program,
    variable_user_info: &Option<VariableUserInfo>,
    context: &TranspileModuleContext,
) {
    let mut builder = Report::build(
        ReportKind::Custom("Optimization Debug", ariadne::Color::Cyan),
        context.module_name.deref().clone(),
        0,
    );

    let optimize_settings = &context.context.settings.optimization;

    let mut has_element = false;

    if optimize_settings.is_required_function_recursive_info() {
        print_recursive_functions(ast, &mut builder, &mut has_element, context);
    }
    if optimize_settings.analyze_lifetime {
        print_lifetime_debug_info(ast, &mut builder, &mut has_element, context);
    }
    if optimize_settings.is_required_variable_move_info() {
        print_variable_user_info(
            ast,
            variable_user_info.as_ref().unwrap(),
            &mut builder,
            &mut has_element,
            context,
        );
    }

    if !has_element {
        return;
    }

    let _lock = context.context.debug_print_lock.lock().unwrap();

    builder
        .finish()
        .print((
            context.module_name.deref().clone(),
            Source::from(context.source_code.code.as_str()),
        ))
        .unwrap();
}
