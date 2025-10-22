use catla_parser::ast::{
    AddOrSubExpression, AndExpression, Define, ElseChain, EntityID, EqualsExpression, Expression,
    ExpressionOrBlock, Factor, FunctionCall, LessOrGreaterExpression, MappingOperator,
    MulOrDivExpression, OrExpression, Primary, PrimaryLeft, PrimaryLeftExpr, PrimaryRight, Program,
    SimplePrimary, Statement,
};
use catla_util::module_path::ModulePath;
use hashbrown::HashMap;

use crate::types::{GlobalUserTypeID, GlobalUserTypeSet, UserTypeInfo};

pub fn collect_user_type_for_program(
    ast: &Program,
    module_entity_type_map: &mut HashMap<EntityID, GlobalUserTypeID>,
    module_name_type_map: &mut HashMap<String, GlobalUserTypeID>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
) {
    for statement in ast.statements.iter() {
        match &statement.statement {
            Statement::Assignment(assignment) => {
                collect_user_type_for_expression(
                    &assignment.left,
                    module_entity_type_map,
                    module_name_type_map,
                    user_type_set,
                    module_path,
                );
                if let Ok(right) = &assignment.right {
                    collect_user_type_for_expression(
                        right,
                        module_entity_type_map,
                        module_name_type_map,
                        user_type_set,
                        module_path,
                    );
                }
            }
            Statement::Swap(swap_statement) => {
                collect_user_type_for_expression(
                    &swap_statement.left,
                    module_entity_type_map,
                    module_name_type_map,
                    user_type_set,
                    module_path,
                );
                if let Ok(right) = &swap_statement.right {
                    collect_user_type_for_expression(
                        right,
                        module_entity_type_map,
                        module_name_type_map,
                        user_type_set,
                        module_path,
                    );
                }
            }
            Statement::Import(_) => {}
            Statement::DefineWithAttribute(define_with_attribute) => {
                if let Ok(define) = &define_with_attribute.define {
                    match define {
                        Define::Function(function_define) => {
                            if let Some(block) = &function_define.block {
                                collect_user_type_for_program(
                                    block.program,
                                    module_entity_type_map,
                                    module_name_type_map,
                                    user_type_set,
                                    module_path,
                                );
                            }
                        }
                        Define::UserType(user_type_define) => {
                            if let Ok(name) = &user_type_define.name {
                                let info = UserTypeInfo {
                                    module_path: module_path.clone(),
                                    name: name.clone().map(|str| str.to_string()),
                                    is_alias: false,
                                    element_types: HashMap::new(),
                                    generics: Vec::new(),
                                    where_clause: Vec::new(),
                                    span: user_type_define.span.clone(),
                                };
                                let id = user_type_set.new_user_type(info);

                                module_entity_type_map.insert(user_type_define.into(), id);
                                module_name_type_map.insert(name.value.to_string(), id);
                            }
                        }
                        Define::Variable(variable_define) => {
                            if let Some(expression) = &variable_define.expression {
                                collect_user_type_for_expression(
                                    expression,
                                    module_entity_type_map,
                                    module_name_type_map,
                                    user_type_set,
                                    module_path,
                                );
                            }
                        }
                        Define::TypeAlias(type_alias) => {
                            if let Ok(name) = &type_alias.name {
                                let info = UserTypeInfo {
                                    module_path: module_path.clone(),
                                    name: name.clone().map(|str| str.to_string()),
                                    is_alias: true,
                                    element_types: HashMap::new(),
                                    generics: Vec::new(),
                                    where_clause: Vec::new(),
                                    span: type_alias.span.clone(),
                                };
                                let id = user_type_set.new_user_type(info);

                                module_entity_type_map.insert(type_alias.into(), id);
                                module_name_type_map.insert(name.value.to_string(), id);
                            }
                        }
                    }
                }
            }
            Statement::Drop(drop_statement) => {
                if let Ok(expression) = &drop_statement.expression {
                    collect_user_type_for_expression(
                        expression,
                        module_entity_type_map,
                        module_name_type_map,
                        user_type_set,
                        module_path,
                    );
                }
            }
            Statement::Expression(expression) => {
                collect_user_type_for_expression(
                    expression,
                    module_entity_type_map,
                    module_name_type_map,
                    user_type_set,
                    module_path,
                );
            }
            Statement::Implements(implements) => {
                if let Ok(block) = &implements.block {
                    collect_user_type_for_program(
                        block.program,
                        module_entity_type_map,
                        module_name_type_map,
                        user_type_set,
                        module_path,
                    );
                }
            }
        }
    }
}

fn collect_user_type_for_expression(
    ast: &Expression,
    module_entity_type_map: &mut HashMap<EntityID, GlobalUserTypeID>,
    module_name_type_map: &mut HashMap<String, GlobalUserTypeID>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
) {
    match ast {
        Expression::Return(return_expression) => {
            if let Some(expression) = &return_expression.expression {
                collect_user_type_for_expression(
                    expression,
                    module_entity_type_map,
                    module_name_type_map,
                    user_type_set,
                    module_path,
                );
            }
        }
        Expression::Closure(closure) => {
            if let Ok(expression_or_block) = &closure.expression_or_block {
                match expression_or_block {
                    ExpressionOrBlock::Expression(expression) => {
                        collect_user_type_for_expression(
                            expression,
                            module_entity_type_map,
                            module_name_type_map,
                            user_type_set,
                            module_path,
                        );
                    }
                    ExpressionOrBlock::Block(block) => {
                        collect_user_type_for_program(
                            block.program,
                            module_entity_type_map,
                            module_name_type_map,
                            user_type_set,
                            module_path,
                        );
                    }
                }
            }
        }
        Expression::Or(or_expression) => {
            collect_user_type_for_or_expression(
                *or_expression,
                module_entity_type_map,
                module_name_type_map,
                user_type_set,
                module_path,
            );
        }
    }
}

macro_rules! collect_user_type_for_2op {
    ($name:ident, $ty:ty, $next:ident) => {
        fn $name(
            ast: &$ty,
            module_entity_type_map: &mut HashMap<EntityID, GlobalUserTypeID>,
            module_name_type_map: &mut HashMap<String, GlobalUserTypeID>,
            user_type_set: &GlobalUserTypeSet,
            module_path: &ModulePath,
        ) {
            $next(
                &ast.left,
                module_entity_type_map,
                module_name_type_map,
                user_type_set,
                module_path,
            );
            for chain in ast.chain.iter() {
                $next(
                    chain,
                    module_entity_type_map,
                    module_name_type_map,
                    user_type_set,
                    module_path,
                );
            }
        }
    };
    (op, $name:ident, $ty:ty, $next:ident) => {
        fn $name(
            ast: &$ty,
            module_entity_type_map: &mut HashMap<EntityID, GlobalUserTypeID>,
            module_name_type_map: &mut HashMap<String, GlobalUserTypeID>,
            user_type_set: &GlobalUserTypeSet,
            module_path: &ModulePath,
        ) {
            $next(
                &ast.left,
                module_entity_type_map,
                module_name_type_map,
                user_type_set,
                module_path,
            );
            for chain in ast.chain.iter() {
                $next(
                    &chain.1,
                    module_entity_type_map,
                    module_name_type_map,
                    user_type_set,
                    module_path,
                );
            }
        }
    };
}

collect_user_type_for_2op!(
    collect_user_type_for_or_expression,
    OrExpression,
    collect_user_type_for_and_expression
);

collect_user_type_for_2op!(
    collect_user_type_for_and_expression,
    AndExpression,
    collect_user_type_for_equals_expression
);

collect_user_type_for_2op!(
    op,
    collect_user_type_for_equals_expression,
    EqualsExpression,
    collect_user_type_for_less_or_greater_expression
);

collect_user_type_for_2op!(
    op,
    collect_user_type_for_less_or_greater_expression,
    LessOrGreaterExpression,
    collect_user_type_for_add_or_sub_expression
);

collect_user_type_for_2op!(
    op,
    collect_user_type_for_add_or_sub_expression,
    AddOrSubExpression,
    collect_user_type_for_mul_or_div_expression
);

collect_user_type_for_2op!(
    op,
    collect_user_type_for_mul_or_div_expression,
    MulOrDivExpression,
    collect_user_type_for_factor
);

fn collect_user_type_for_factor(
    ast: &Factor,
    module_entity_type_map: &mut HashMap<EntityID, GlobalUserTypeID>,
    module_name_type_map: &mut HashMap<String, GlobalUserTypeID>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
) {
    if let Ok(primary) = &ast.primary {
        collect_user_type_for_primary(
            primary,
            module_entity_type_map,
            module_name_type_map,
            user_type_set,
            module_path,
        );
    }
}

fn collect_user_type_for_primary(
    ast: &Primary,
    module_entity_type_map: &mut HashMap<EntityID, GlobalUserTypeID>,
    module_name_type_map: &mut HashMap<String, GlobalUserTypeID>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
) {
    collect_user_type_for_primary_left(
        &ast.left,
        module_entity_type_map,
        module_name_type_map,
        user_type_set,
        module_path,
    );
    for chain in ast.chain.iter() {
        collect_user_type_for_primary_right(
            chain,
            module_entity_type_map,
            module_name_type_map,
            user_type_set,
            module_path,
        );
    }
}

fn collect_user_type_for_primary_left(
    ast: &PrimaryLeft,
    module_entity_type_map: &mut HashMap<EntityID, GlobalUserTypeID>,
    module_name_type_map: &mut HashMap<String, GlobalUserTypeID>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
) {
    match &ast.first {
        PrimaryLeftExpr::Simple {
            left,
            generics: _,
            function_call,
            span: _,
        } => {
            match left {
                SimplePrimary::Tuple {
                    expressions,
                    span: _,
                } => {
                    for expression in expressions.iter() {
                        collect_user_type_for_expression(
                            expression,
                            module_entity_type_map,
                            module_name_type_map,
                            user_type_set,
                            module_path,
                        );
                    }
                }
                SimplePrimary::Literal(_) => {}
                SimplePrimary::StringLiteral(_) => {}
                SimplePrimary::NumericLiteral(_) => {}
                SimplePrimary::Null(_) => {}
                SimplePrimary::True(_) => {}
                SimplePrimary::False(_) => {}
                SimplePrimary::This(_) => {}
                SimplePrimary::LargeThis(_) => {}
            }
            if let Some(function_call) = function_call {
                collect_user_type_for_function_call(
                    function_call,
                    module_entity_type_map,
                    module_name_type_map,
                    user_type_set,
                    module_path,
                );
            }
        }
        PrimaryLeftExpr::NewObject { new_object } => {
            for field_assign in new_object.field_assign.elements.iter() {
                collect_user_type_for_expression(
                    &field_assign.expression,
                    module_entity_type_map,
                    module_name_type_map,
                    user_type_set,
                    module_path,
                );
            }
        }
        PrimaryLeftExpr::NewArray { new_array } => {
            for expression in new_array.elements.iter() {
                collect_user_type_for_expression(
                    expression,
                    module_entity_type_map,
                    module_name_type_map,
                    user_type_set,
                    module_path,
                );
            }
        }
        PrimaryLeftExpr::NewArrayInit { new_array_init } => {
            if let Ok(init_expression) = &new_array_init.init_expression {
                collect_user_type_for_expression(
                    init_expression,
                    module_entity_type_map,
                    module_name_type_map,
                    user_type_set,
                    module_path,
                );
            }
            if let Ok(length_expression) = &new_array_init.length_expression {
                collect_user_type_for_expression(
                    length_expression,
                    module_entity_type_map,
                    module_name_type_map,
                    user_type_set,
                    module_path,
                );
            }
        }
        PrimaryLeftExpr::If { if_expression } => {
            if let Ok(condition) = &if_expression.first.condition {
                collect_user_type_for_expression(
                    condition,
                    module_entity_type_map,
                    module_name_type_map,
                    user_type_set,
                    module_path,
                );
            }
            if let Ok(block) = &if_expression.first.block {
                collect_user_type_for_program(
                    block.program,
                    module_entity_type_map,
                    module_name_type_map,
                    user_type_set,
                    module_path,
                );
            }

            for chain in if_expression.chain.iter() {
                match chain {
                    ElseChain::ElseIf { if_statement } => {
                        if let Ok(condition) = &if_statement.condition {
                            collect_user_type_for_expression(
                                condition,
                                module_entity_type_map,
                                module_name_type_map,
                                user_type_set,
                                module_path,
                            );
                        }
                        if let Ok(block) = &if_statement.block {
                            collect_user_type_for_program(
                                block.program,
                                module_entity_type_map,
                                module_name_type_map,
                                user_type_set,
                                module_path,
                            );
                        }
                    }
                    ElseChain::Else { block } => {
                        collect_user_type_for_program(
                            block.program,
                            module_entity_type_map,
                            module_name_type_map,
                            user_type_set,
                            module_path,
                        );
                    }
                }
            }
        }
        PrimaryLeftExpr::Loop { loop_expression } => {
            if let Ok(block) = &loop_expression.block {
                collect_user_type_for_program(
                    block.program,
                    module_entity_type_map,
                    module_name_type_map,
                    user_type_set,
                    module_path,
                );
            }
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_user_type_for_mapping_operator(
            mapping_operator,
            module_entity_type_map,
            module_name_type_map,
            user_type_set,
            module_path,
        );
    }
}

fn collect_user_type_for_primary_right(
    ast: &PrimaryRight,
    module_entity_type_map: &mut HashMap<EntityID, GlobalUserTypeID>,
    module_name_type_map: &mut HashMap<String, GlobalUserTypeID>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
) {
    if let Some(second) = &ast.second {
        if let Some(function_call) = &second.function_call {
            collect_user_type_for_function_call(
                function_call,
                module_entity_type_map,
                module_name_type_map,
                user_type_set,
                module_path,
            );
        }
    }
    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_user_type_for_mapping_operator(
            mapping_operator,
            module_entity_type_map,
            module_name_type_map,
            user_type_set,
            module_path,
        );
    }
}

fn collect_user_type_for_mapping_operator(
    ast: &MappingOperator,
    module_entity_type_map: &mut HashMap<EntityID, GlobalUserTypeID>,
    module_name_type_map: &mut HashMap<String, GlobalUserTypeID>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
) {
    let block = match ast {
        MappingOperator::NullElvis { block, span: _ } => block,
        MappingOperator::ResultElvis { block, span: _ } => block,
        _ => return,
    };

    if let Ok(block) = block {
        collect_user_type_for_program(
            block.program,
            module_entity_type_map,
            module_name_type_map,
            user_type_set,
            module_path,
        );
    }
}

fn collect_user_type_for_function_call(
    ast: &FunctionCall,
    module_entity_type_map: &mut HashMap<EntityID, GlobalUserTypeID>,
    module_name_type_map: &mut HashMap<String, GlobalUserTypeID>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
) {
    for expression in ast.arguments.iter() {
        collect_user_type_for_expression(
            expression,
            module_entity_type_map,
            module_name_type_map,
            user_type_set,
            module_path,
        );
    }
}
