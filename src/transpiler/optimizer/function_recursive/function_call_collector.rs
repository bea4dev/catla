use std::sync::Arc;

use catla_parser::parser::{Program, StatementAST};

use crate::transpiler::{
    component::EntityID, context::TranspileModuleContext,
    semantics::types::type_inference::TypeInferenceResultContainer,
};

pub fn collect_function_call(
    ast: Program,
    type_inference_result: &TypeInferenceResultContainer,
    context: &TranspileModuleContext,
) {
    collect_function_call_program(ast, type_inference_result, &mut Vec::new(), context);
}

fn collect_function_call_program(
    ast: Program,
    type_inference_result: &TypeInferenceResultContainer,
    function_calls: &mut Vec<(Arc<String>, EntityID)>,
    context: &TranspileModuleContext,
) {
    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            Err(_) => continue,
        };

        match statement {
            StatementAST::Assignment(assignment) => todo!(),
            StatementAST::Exchange(exchange) => todo!(),
            StatementAST::Import(import) => todo!(),
            StatementAST::StatementAttributes(vec) => todo!(),
            StatementAST::VariableDefine(variable_define) => todo!(),
            StatementAST::FunctionDefine(function_define) => todo!(),
            StatementAST::UserTypeDefine(user_type_define) => todo!(),
            StatementAST::TypeDefine(type_define) => todo!(),
            StatementAST::Implements(implements) => todo!(),
            StatementAST::DropStatement(drop_statement) => todo!(),
            StatementAST::Expression(_) => todo!(),
            StatementAST::TranspilerTag(transpiler_tag) => todo!(),
        }
    }
}
