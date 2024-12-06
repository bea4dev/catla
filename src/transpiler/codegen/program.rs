use bumpalo::Bump;
use catla_parser::parser::{Program, StatementAST};

use crate::transpiler::{
    context::TranspileModuleContext, semantics::types::type_inference::TypeInferenceResultContainer,
};

use super::{CodeBuilder, StackAllocCodeBuilder};

pub fn codegen_program<'allocator>(
    ast: Program,
    type_inference_result: &TypeInferenceResultContainer,
    top_stack_alloc_builder: Option<&mut StackAllocCodeBuilder<'allocator>>,
    current_tree_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    code_builder: &mut CodeBuilder<'allocator>,
    allocator: &'allocator Bump,
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
        }
    }
}
