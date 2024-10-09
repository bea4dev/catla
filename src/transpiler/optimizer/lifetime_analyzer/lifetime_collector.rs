use bumpalo::Bump;
use catla_parser::parser::{Program, Spanned, StatementAST};
use fxhash::FxHashMap;

use crate::transpiler::{component::EntityID, context::TranspileModuleContext, name_resolver::FoundDefineInfo};

use super::{LifetimeInstance, LifetimeScope};



pub fn collect_lifetime_program<'allocator>(
    ast: Program<'_, 'allocator>,
    force_be_expression: bool,
    import_element_map: &FxHashMap<EntityID, Spanned<String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    lifetime_scope: &mut LifetimeScope,
    lifetime_instance_map: &mut FxHashMap<EntityID, LifetimeInstance>,
    allocator: &Bump,
    context: &TranspileModuleContext
) {
    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            Err(_) => continue
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
