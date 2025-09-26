use catla_name_resolver::ResolvedInfo;
use catla_parser::ast::{Define, EntityID, Expression, Program, Statement, TypeInfo, TypeInfoBase};
use hashbrown::HashMap;

use crate::types::{GlobalUserTypeID, GlobalUserTypeSet, Type};

pub fn collect_module_element_type_for_program(
    ast: &Program,
    parent: Option<Type>,
    module_element_entity_type_map: &mut HashMap<EntityID, Type>,
    module_element_name_type_map: &mut HashMap<String, Type>,
    entity_user_type_map: &HashMap<EntityID, GlobalUserTypeID>,
    name_user_type_map: &HashMap<String, HashMap<String, GlobalUserTypeID>>,
    user_type_set: &GlobalUserTypeSet,
) {
    for statement in ast.statements.iter() {
        match &statement.statement {
            Statement::Assignment(assignment) => {
                collect_module_element_type_for_expression(
                    &assignment.left,
                    module_element_entity_type_map,
                    module_element_name_type_map,
                    entity_user_type_map,
                    name_user_type_map,
                    user_type_set,
                );
                if let Ok(right) = &assignment.right {
                    collect_module_element_type_for_expression(
                        right,
                        module_element_entity_type_map,
                        module_element_name_type_map,
                        entity_user_type_map,
                        name_user_type_map,
                        user_type_set,
                    );
                }
            }
            Statement::Swap(swap_statement) => {
                collect_module_element_type_for_expression(
                    &swap_statement.left,
                    module_element_entity_type_map,
                    module_element_name_type_map,
                    entity_user_type_map,
                    name_user_type_map,
                    user_type_set,
                );
                if let Ok(right) = &swap_statement.right {
                    collect_module_element_type_for_expression(
                        right,
                        module_element_entity_type_map,
                        module_element_name_type_map,
                        entity_user_type_map,
                        name_user_type_map,
                        user_type_set,
                    );
                }
            }
            Statement::Import(_) => {}
            Statement::DefineWithAttribute(define_with_attribute) => {
                if let Ok(define) = &define_with_attribute.define {
                    match define {
                        Define::Function(function_define) => {
                            
                        },
                        Define::UserType(user_type_define) => todo!(),
                        Define::Variable(variable_define) => todo!(),
                        Define::TypeAlias(type_alias) => todo!(),
                    }
                }
            }
            Statement::Drop(drop_statement) => todo!(),
            Statement::Expression(expression) => todo!(),
            Statement::Implements(implements) => todo!(),
        }
    }
}

pub(crate) fn get_type(
    ast: &TypeInfo,
    this_type: Option<Type>,
    entity_user_type_map: &HashMap<EntityID, GlobalUserTypeID>,
    name_user_type_map: &HashMap<String, HashMap<String, GlobalUserTypeID>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
) {
    let mut base_type = match &ast.base {
        TypeInfoBase::Array(array_type_info) => todo!(),
        TypeInfoBase::Base(base_type_info) => todo!(),
        TypeInfoBase::Tuple(tuple_type_info) => todo!(),
        TypeInfoBase::This(spanned) => todo!(),
    };
}

fn collect_module_element_type_for_expression(
    ast: &Expression,
    module_element_entity_type_map: &mut HashMap<EntityID, Type>,
    module_element_name_type_map: &mut HashMap<String, Type>,
    entity_user_type_map: &HashMap<EntityID, GlobalUserTypeID>,
    name_user_type_map: &HashMap<String, HashMap<String, GlobalUserTypeID>>,
    user_type_set: &GlobalUserTypeSet,
) {
}

