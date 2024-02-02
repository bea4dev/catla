use std::{cell::RefCell, ops::Range};

use ariadne::{Color, Label, Report, ReportKind, Source};
use bumpalo::{collections::String, Bump};
use catla_parser::{grammar::number_literal_regex, parser::{AddOrSubExpression, AndExpression, Block, CompareExpression, EQNEExpression, Expression, ExpressionEnum, Factor, FunctionCall, Generics, Literal, MappingOperatorKind, MulOrDivExpression, Primary, PrimaryLeft, PrimaryLeftExpr, PrimaryRight, Program, SimplePrimary, StatementAST, TypeInfo, TypeTag}};
use either::Either::{Left, Right};
use hashbrown::{hash_map::DefaultHashBuilder, HashMap};

use super::{advice::AdviceReport, component::{ComponentContainer, EntityID}, error::{ErrorMessageKey, ErrorMessageType, TranspileReport}, TranspileError, TranspileWarning};


#[derive(Debug, Clone)]
pub(crate) struct DefineWithName<'allocator> {
    entity_id: EntityID<'allocator>,
    span: Range<usize>,
    define_kind: DefineKind
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum DefineKind {
    Import,
    Function,
    Variable,
    FunctionArgument,
    ClosureArgument,
    DataStruct,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum EnvironmentSeparatorKind {
    Function,
    DataStruct,
    Closure
}

pub(crate) struct NameEnvironment<'allocator> {
    name_define_info_map: RefCell<HashMap<String<'allocator>, DefineWithName<'allocator>, DefaultHashBuilder, &'allocator Bump>>,
    resolved_map: RefCell<HashMap<EntityID<'allocator>, FoundDefineInfo<'allocator>, DefaultHashBuilder, &'allocator Bump>>,
    parent: Option<EntityID<'allocator>>,
    separator: Option<EnvironmentSeparatorKind>,
    span : Range<usize>
}

impl<'allocator> NameEnvironment<'allocator> {
    
    pub(crate) fn new(parent: Option<EntityID<'allocator>>, separator: Option<EnvironmentSeparatorKind>, span: Range<usize>, allocator: &'allocator Bump) -> NameEnvironment<'allocator> {
        return Self {
            name_define_info_map: RefCell::new(HashMap::new_in(allocator)),
            parent,
            resolved_map: RefCell::new(HashMap::new_in(allocator)),
            separator,
            span
        };
    }

    pub(crate) fn get_name_define_info(
        &self,
        name: &str,
        environments: &ComponentContainer<NameEnvironment<'allocator>>
    ) -> Option<FoundDefineInfo<'allocator>> {
        
        return match self.name_define_info_map.borrow().get(name) {
            Some(define_info) => Some(FoundDefineInfo { define_info: define_info.clone(), separators: Vec::new() }),
            _ => {
                match self.parent {
                    Some(parent) => {
                        let mut separators = Vec::new();
                        let mut current_entity_id = parent;
                        loop {
                            let name_environment = &environments[current_entity_id];
                            if let Some(separator) = name_environment.separator {
                                separators.push(separator);
                            }

                            match name_environment.name_define_info_map.borrow().get(name) {
                                Some(define_info) => return Some(FoundDefineInfo { define_info: define_info.clone(), separators }),
                                _ => {
                                    let parent = match name_environment.parent {
                                        Some(parent) => parent,
                                        _ => return None,
                                    };
                                    current_entity_id = parent;
                                }
                            }
                        }
                    },
                    _ => return None
                }
            }
        };
    }

    pub(crate) fn set_name_define_info(&self, name: String<'allocator>, define_info: DefineWithName<'allocator>) {
        self.name_define_info_map.borrow_mut().insert(name, define_info);
    }

    pub(crate) fn mark_as_resolved(&self, entity_id: EntityID<'allocator>, define_info: FoundDefineInfo<'allocator>) {
        self.resolved_map.borrow_mut().insert(entity_id, define_info);
    }

}

pub(crate) struct FoundDefineInfo<'allocator> {
    define_info: DefineWithName<'allocator>,
    separators: Vec<EnvironmentSeparatorKind>
}

impl FoundDefineInfo<'_> {
    
    pub(crate) fn has_separator(&self, kind: &[EnvironmentSeparatorKind]) -> bool {
        for separator in self.separators.iter() {
            if kind.contains(separator) {
                return true;
            }
        }
        return false;
    }

}



pub(crate) fn name_resolve_program<'allocator>(
    ast: Program<'allocator, '_>,
    parent_env_id: Option<EntityID<'allocator>>,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    let current_environment_id = EntityID::from(ast);
    let environment_span = match parent_env_id {
        Some(entity_id) => name_environments[entity_id].span.clone(),
        None => ast.span.clone(),
    };
    name_environments.insert(current_environment_id, NameEnvironment::new(parent_env_id, None, environment_span.clone(), allocator));
    let name_environment = &name_environments[current_environment_id];

    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            _ => continue
        };

        match statement {
            StatementAST::Import(import) => {
                let name_environment = &name_environments[current_environment_id];
                let entity_id = EntityID::from(import);
                for element in import.elements.elements.iter() {
                    let define_info = DefineWithName { entity_id, span: element.span.clone(), define_kind: DefineKind::Import };
                    if let Some(already_exists) = name_environment.get_name_define_info(element.value, &name_environments) {
                        errors.push(NameAlreadyExists::new(define_info.span.clone(), already_exists.define_info.span.clone()));
                    } else {
                        let name = String::from_str_in(element.value, allocator);
                        name_environment.set_name_define_info(name, define_info);
                    }
                }
            },
            StatementAST::FunctionDefine(function_define) => {
                if let Ok(function_name) = &function_define.name {
                    if let Left(name) = function_name {
                        let entity_id = EntityID::from(function_define);
                        let define_info = DefineWithName { entity_id, span: name.span.clone(), define_kind: DefineKind::Function };

                        if let Some(already_exists) = name_environment.get_name_define_info(name.value, name_environments) {
                            errors.push(NameAlreadyExists::new(define_info.span.clone(), already_exists.define_info.span.clone()));
                        } else {
                            let name = String::from_str_in(name.value, allocator);
                            name_environment.set_name_define_info(name, define_info);
                        }
                    }
                }
            },
            StatementAST::DataStructDefine(data_struct_define) => {
                if let Ok(data_struct_name) = &data_struct_define.name {
                    let entity_id = EntityID::from(data_struct_define);
                    let define_info = DefineWithName { entity_id, span: data_struct_name.span.clone(), define_kind: DefineKind::DataStruct };

                    if let Some(already_exists) = name_environment.get_name_define_info(data_struct_name.value, name_environments) {
                        errors.push(NameAlreadyExists::new(define_info.span.clone(), already_exists.define_info.span.clone()));
                    } else {
                        let name = String::from_str_in(data_struct_name.value, allocator);
                        name_environment.set_name_define_info(name, define_info);
                    }
                }
            },
            _ => {}
        }
    }

    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            _ => continue
        };

        match statement {
            StatementAST::Assignment(assignment) => {
                if !is_valid_format_for_assignment(assignment.left_expr) {
                    errors.push(InvalidAssignTargetFormat::new(get_expression_span(assignment.left_expr)));
                }
                name_resolve_expression(assignment.left_expr, current_environment_id, name_environments, errors, warnings, allocator);
                
                if let Ok(right_expr) = assignment.right_expr {
                    name_resolve_expression(right_expr, current_environment_id, name_environments, errors, warnings, allocator);
                }
            },
            StatementAST::Exchange(exchange) => {
                if !is_valid_format_for_assignment(exchange.left_expr) {
                    errors.push(InvalidAssignTargetFormat::new(get_expression_span(exchange.left_expr)));
                }
                name_resolve_expression(exchange.left_expr, current_environment_id, name_environments, errors, warnings, allocator);
                
                if let Ok(right_expr) = exchange.right_expr {
                    if !is_valid_format_for_assignment(right_expr) {
                        errors.push(InvalidAssignTargetFormat::new(get_expression_span(right_expr)));
                    }
                    name_resolve_expression(right_expr, current_environment_id, name_environments, errors, warnings, allocator);
                }
            },
            StatementAST::VariableDefine(variable_define) => {
                if let Some(expression) = &variable_define.expression {
                    if let Ok(expression) = expression {
                        name_resolve_expression(&expression, current_environment_id, name_environments, errors, warnings, allocator);
                    }
                }

                if let Ok(variable_name) = &variable_define.name {
                    let entity_id = EntityID::from(variable_define);
                    let define_info = DefineWithName { entity_id, span: variable_name.span.clone(), define_kind: DefineKind::Variable };
                    let name = String::from_str_in(variable_name.value, allocator);
                    name_environments[current_environment_id].set_name_define_info(name, define_info);
                }
            },
            StatementAST::FunctionDefine(function_define) => {
                let name_environment = NameEnvironment::new(Some(current_environment_id), Some(EnvironmentSeparatorKind::Function), environment_span.clone(), allocator);

                // TODO - generics

                for argument in function_define.args.arguments.iter() {
                    let entity_id = EntityID::from(argument);
                    let define_info = DefineWithName { entity_id, span: argument.span.clone(), define_kind: DefineKind::FunctionArgument };
                    let name = String::from_str_in(argument.name.value, allocator);
                    name_environment.set_name_define_info(name, define_info);
                }

                let entity_id = EntityID::from(function_define);
                name_environments.insert(entity_id, name_environment);

                if let Some(block) = &function_define.block.value {
                    name_resolve_block(block, entity_id, name_environments, errors, warnings, allocator);
                }
            },
            //StatementAST::DataStructDefine(_) => todo!(),
            //StatementAST::DropStatement(_) => todo!(),
            StatementAST::Expression(expression) => {
                name_resolve_expression(&expression, current_environment_id, name_environments, errors, warnings, allocator);
            },
            _ => {}
        }
    }
}

fn is_valid_format_for_assignment(expression: Expression) -> bool {
    return if let ExpressionEnum::OrExpression(or_expression) = expression {
        let and_expression = &or_expression.left_expr;
        let eqne_expression = &and_expression.left_expr;
        let compare_expression = &eqne_expression.left_expr;
        let add_or_sub_expression = &compare_expression.left_expr;
        let mul_or_div_expression = &add_or_sub_expression.left_expr;
        let factor = &mul_or_div_expression.left_expr;
        or_expression.right_exprs.is_empty()
            && and_expression.right_exprs.is_empty()
            && eqne_expression.right_exprs.is_empty()
            && compare_expression.right_exprs.is_empty()
            && add_or_sub_expression.right_exprs.is_empty()
            && mul_or_div_expression.right_exprs.is_empty()
            && factor.negative_keyword_span.is_none()
    } else {
        false
    };
}

fn get_expression_span(expression: Expression) -> Range<usize> {
    return match expression {
        ExpressionEnum::OrExpression(or_expression) => or_expression.span.clone(),
        ExpressionEnum::ReturnExpression(return_expression) => return_expression.span.clone(),
        ExpressionEnum::Closure(closure) => closure.span.clone(),
    }
}


fn name_resolve_expression<'allocator>(
    ast: Expression<'allocator, '_>,
    environment_id: EntityID<'allocator>,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    match ast {
        ExpressionEnum::OrExpression(or_expression) => {
            name_resolve_and_expression(&or_expression.left_expr, environment_id, name_environments, errors, warnings, allocator);
            for right_expr in or_expression.right_exprs.iter() {
                if let Ok(and_expression) = &right_expr.1 {
                    name_resolve_and_expression(and_expression, environment_id, name_environments, errors, warnings, allocator);
                }
            }
        },
        ExpressionEnum::ReturnExpression(return_expression) => {
            if let Some(expression) = &return_expression.expression {
                name_resolve_expression(&expression, environment_id, name_environments, errors, warnings, allocator);
            }
        },
        ExpressionEnum::Closure(closure) => {
            let environment_span = name_environments[environment_id].span.clone();
            let name_environment = NameEnvironment::new(Some(environment_id), Some(EnvironmentSeparatorKind::Closure), environment_span, allocator);
            
            match &closure.arguments.arguments {
                Left(argument) => {
                    let entity_id = EntityID::from(argument);
                    let name = String::from_str_in(argument.value, allocator);
                    let define_info = DefineWithName { entity_id, span: argument.span.clone(), define_kind: DefineKind::ClosureArgument };
                    name_environment.set_name_define_info(name, define_info);
                },
                Right(arguments) => {
                    for argument in arguments.iter() {
                        match argument {
                            Left(argument) => {
                                let entity_id = EntityID::from(argument);
                                let name = String::from_str_in(argument.name.value, allocator);
                                let define_info = DefineWithName { entity_id, span: argument.name.span.clone(), define_kind: DefineKind::ClosureArgument };
                                name_environment.set_name_define_info(name, define_info);
                            },
                            Right(argument) => {
                                let entity_id = EntityID::from(argument);
                                let name = String::from_str_in(argument.value, allocator);
                                let define_info = DefineWithName { entity_id, span: argument.span.clone(), define_kind: DefineKind::ClosureArgument };
                                name_environment.set_name_define_info(name, define_info);
                            }
                        }
                    }
                }
            }

            let entity_id = EntityID::from(closure);
            name_environments.insert(entity_id, name_environment);

            if let Some(expression_or_block) = &closure.expression_or_block.value {
                match expression_or_block {
                    Left(expression) => {
                        name_resolve_expression(&expression, entity_id, name_environments, errors, warnings, allocator);
                    },
                    Right(block) => {
                        name_resolve_block(block, entity_id, name_environments, errors, warnings, allocator);
                    }
                }
            }
        }
    }
}

fn name_resolve_block<'allocator>(
    ast: &'allocator Block<'allocator, '_>,
    environment_id: EntityID<'allocator>,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    let name_environment = NameEnvironment::new(Some(environment_id), None, ast.span.clone(), allocator);
    let entity_id = EntityID::from(ast);
    name_environments.insert(entity_id, name_environment);

    name_resolve_program(ast.program, Some(entity_id), name_environments, errors, warnings, allocator);
}

fn name_resolve_and_expression<'allocator>(
    ast: &'allocator AndExpression<'allocator, '_>,
    environment_id: EntityID<'allocator>,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    name_resolve_eqne_expression(&ast.left_expr, environment_id, name_environments, errors, warnings, allocator);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(eqne_expression) = &right_expr.1 {
            name_resolve_eqne_expression(eqne_expression, environment_id, name_environments, errors, warnings, allocator);
        }
    }
}

fn name_resolve_eqne_expression<'allocator>(
    ast: &'allocator EQNEExpression<'allocator, '_>,
    environment_id: EntityID<'allocator>,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    name_resolve_compare_expression(&ast.left_expr, environment_id, name_environments, errors, warnings, allocator);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(compare_expression) = &right_expr.1 {
            name_resolve_compare_expression(compare_expression, environment_id, name_environments, errors, warnings, allocator);
        }
    }
}

fn name_resolve_compare_expression<'allocator>(
    ast: &'allocator CompareExpression<'allocator, '_>,
    environment_id: EntityID<'allocator>,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    name_resolve_add_or_sub_expression(&ast.left_expr, environment_id, name_environments, errors, warnings, allocator);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(add_or_sub_expression) = &right_expr.1 {
            name_resolve_add_or_sub_expression(add_or_sub_expression, environment_id, name_environments, errors, warnings, allocator);
        }
    }
}

fn name_resolve_add_or_sub_expression<'allocator>(
    ast: &'allocator AddOrSubExpression<'allocator, '_>,
    environment_id: EntityID<'allocator>,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    name_resolve_mul_or_div_expression(&ast.left_expr, environment_id, name_environments, errors, warnings, allocator);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(add_or_sub_expression) = &right_expr.1 {
            name_resolve_mul_or_div_expression(add_or_sub_expression, environment_id, name_environments, errors, warnings, allocator);
        }
    }
}

fn name_resolve_mul_or_div_expression<'allocator>(
    ast: &'allocator MulOrDivExpression<'allocator, '_>,
    environment_id: EntityID<'allocator>,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    name_resolve_factor(&ast.left_expr, environment_id, name_environments, errors, warnings, allocator);
    for right_expr in ast.right_exprs.iter() {
        if let Ok(add_or_sub_expression) = &right_expr.1 {
            name_resolve_factor(add_or_sub_expression, environment_id, name_environments, errors, warnings, allocator);
        }
    }
}

fn name_resolve_factor<'allocator>(
    ast: &'allocator Factor<'allocator, '_>,
    environment_id: EntityID<'allocator>,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    if let Ok(primary) = &ast.primary {
        name_resolve_primary(primary, environment_id, name_environments, errors, warnings, allocator);
    }
}

fn name_resolve_primary<'allocator>(
    ast: &'allocator Primary<'allocator, '_>,
    environment_id: EntityID<'allocator>,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    name_resolve_primary_left(&ast.left, environment_id, name_environments, errors, warnings, allocator);
    for primary_right in ast.chain.iter() {
        name_resolve_primary_right(primary_right, environment_id, name_environments, errors, warnings, allocator);
    }
}

fn name_resolve_primary_left<'allocator>(
    ast: &'allocator PrimaryLeft<'allocator, '_>,
    environment_id: EntityID<'allocator>,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    match &ast.first_expr {
        PrimaryLeftExpr::Simple(simple) => {
            match &simple.0 {
                SimplePrimary::Expression { expression, error_tokens: _ } => {
                    if let Ok(expression) = expression {
                        name_resolve_expression(&expression, environment_id, name_environments, errors, warnings, allocator);
                    }
                },
                SimplePrimary::Identifier(literal) => {
                    name_resolve_literal(literal, environment_id, name_environments, errors);
                },
                _ => {}
            }
            if let Some(function_call) = &simple.1 {
                name_resolve_function_call(function_call, environment_id, name_environments, errors, warnings, allocator);
            }
        },
        PrimaryLeftExpr::NewExpression(new_expression) => {
            if let Some(first_literal) = new_expression.path.first() {
                name_resolve_literal(first_literal, environment_id, name_environments, errors);
            }
        },
        PrimaryLeftExpr::IfExpression(if_expression) => {
            let if_statement = &if_expression.if_statement;
            let environment_span = match &if_statement.block.value {
                Some(block) => block.span.clone(),
                _ => if_statement.span.clone(),
            };
            let name_environment = NameEnvironment::new(Some(environment_id), None, environment_span, allocator);
            let entity_id = EntityID::from(if_statement);
            name_environments.insert(entity_id, name_environment);

            if let Ok(condition) = &if_statement.condition {
                name_resolve_expression(&condition, entity_id, name_environments, errors, warnings, allocator);
            }
            if let Some(block) = &if_statement.block.value {
                name_resolve_block(block, entity_id, name_environments, errors, warnings, allocator);
            }

            for else_or_if_statement in if_expression.chain.iter() {
                if let Some(else_or_if_statement) = &else_or_if_statement.else_if_or_else.value {
                    match else_or_if_statement {
                        Left(if_statement) => {
                            let environment_span = match &if_statement.block.value {
                                Some(block) => block.span.clone(),
                                _ => if_statement.span.clone(),
                            };
                            let name_environment = NameEnvironment::new(Some(environment_id), None, environment_span, allocator);
                            let entity_id = EntityID::from(if_statement);
                            name_environments.insert(entity_id, name_environment);

                            if let Ok(condition) = &if_statement.condition {
                                name_resolve_expression(&condition, entity_id, name_environments, errors, warnings, allocator);
                            }
                            if let Some(block) = &if_statement.block.value {
                                name_resolve_block(block, entity_id, name_environments, errors, warnings, allocator);
                            }
                        },
                        Right(block) => {
                            name_resolve_block(block, environment_id, name_environments, errors, warnings, allocator);
                        }
                    }
                }
            }
        },
        PrimaryLeftExpr::LoopExpression(loop_expression) => {
            if let Ok(block) = &loop_expression.block {
                name_resolve_block(block, environment_id, name_environments, errors, warnings, allocator);
            }
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        name_resolve_mapping_operator(&mapping_operator.value, environment_id, name_environments, errors, warnings, allocator);
    }
}

fn name_resolve_mapping_operator<'allocator>(
    ast: &'allocator MappingOperatorKind<'allocator, '_>,
    environment_id: EntityID<'allocator>,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    let block = match ast {
        MappingOperatorKind::NullElvisBlock(block) => block,
        MappingOperatorKind::ResultElvisBlock(block) => block,
        _ => return
    };
    if let Some(block) = &block.value {
        name_resolve_block(block, environment_id, name_environments, errors, warnings, allocator);
    }
}

fn name_resolve_primary_right<'allocator>(
    ast: &'allocator PrimaryRight<'allocator, '_>,
    environment_id: EntityID<'allocator>,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    if let Some(second_expr) = &ast.second_expr {
        if let Some(function_call) = &second_expr.1 {
            name_resolve_function_call(function_call, environment_id, name_environments, errors, warnings, allocator);
        }
    }
    if let Some(mapping_operator) = &ast.mapping_operator {
        name_resolve_mapping_operator(&mapping_operator.value, environment_id, name_environments, errors, warnings, allocator);
    }
}

fn name_resolve_function_call<'allocator>(
    ast: &'allocator FunctionCall<'allocator, '_>,
    environment_id: EntityID<'allocator>,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    if let Some(generics) = &ast.generics {
        if let Ok(generics) = generics {
            name_resolve_generics(generics, environment_id, name_environments, errors, warnings, allocator);
        }
    }
    if let Ok(arg_exprs) = &ast.arg_exprs {
        for expression in arg_exprs.iter() {
            name_resolve_expression(&expression, environment_id, name_environments, errors, warnings, allocator);
        }
    }
}


fn name_resolve_generics<'allocator>(
    ast: &'allocator Generics<'allocator, '_>,
    environment_id: EntityID<'allocator>,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {

}

fn name_resolve_type_tag<'allocator>(
    ast: &'allocator TypeTag<'allocator, '_>,
    environment_id: EntityID<'allocator>,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {

}

fn name_resolve_type_info<'allocator>(
    ast: &'allocator TypeInfo<'allocator, '_>,
    environment_id: EntityID<'allocator>,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {

}

fn name_resolve_literal<'allocator>(
    literal: &'allocator Literal<'allocator>,
    environment_id: EntityID<'allocator>,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    errors: &mut Vec<TranspileError>,
) {
    if number_literal_regex(literal.value) {
        // is numeric literal
        return;
    }

    let name_environment = &name_environments[environment_id];
    match name_environment.get_name_define_info(literal.value, &name_environments) {
        Some(define_info) => {
            name_environment.mark_as_resolved(EntityID::from(literal), define_info);
        },
        _ => {
            errors.push(UndefinedIdentifier::new(literal.span.clone()));
        }
    }
}



pub(crate) struct NameAlreadyExists {
    define_span: Range<usize>,
    already_exists_span: Range<usize>,
    advice_report: AdviceReport
}

impl NameAlreadyExists {
    pub(crate) fn new(define_span: Range<usize>, already_exists_span: Range<usize>) -> TranspileError {
        return TranspileError::new(Self {
            define_span,
            already_exists_span,
            advice_report: AdviceReport::new()
        });
    }
}

impl TranspileReport for NameAlreadyExists {
    fn print(&self, context: &super::context::TranspileModuleContext) {
        let module_name = &context.module_name;

        let text = &context.context.localized_text;
        let error_code = 0022;
        let key = ErrorMessageKey::new(error_code);
        
        Report::build(ReportKind::Error, module_name, self.define_span.start)
            .with_code(error_code)
            .with_message(key.get_massage(text, ErrorMessageType::Message))
            .with_label(
                Label::new((module_name, self.define_span.clone()))
                    .with_color(Color::Red)
                    .with_message(key.get_massage(text, ErrorMessageType::Label(0)))
            )
            .with_label(
                Label::new((module_name, self.already_exists_span.clone()))
                    .with_color(Color::Yellow)
                    .with_message(key.get_massage(text, ErrorMessageType::Label(1)))
            )
            .with_note(key.get_massage(text, ErrorMessageType::Note))
            .finish()
            .print((module_name, Source::from(context.source_code.code.as_str())))
            .unwrap();

        self.advice_report.print(context, self.define_span.start);
    }

    fn add_advice(&mut self, module_name: std::prelude::v1::String, advice: super::advice::Advice) {
        self.advice_report.add_advice(module_name, advice);
    }
}


pub(crate) struct InvalidAssignTargetFormat {
    span: Range<usize>,
    advice_report: AdviceReport
}

impl InvalidAssignTargetFormat {
    pub(crate) fn new(span: Range<usize>) -> TranspileError {
        return TranspileError::new(Self { span, advice_report: AdviceReport::new() });
    }
}

impl TranspileReport for InvalidAssignTargetFormat {
    fn print(&self, context: &super::context::TranspileModuleContext) {
        let module_name = &context.module_name;
        let text = &context.context.localized_text;
        let error_code = 0023;
        let key = ErrorMessageKey::new(error_code);

        Report::build(ReportKind::Error, module_name, self.span.start)
            .with_code(error_code)
            .with_message(key.get_massage(text, ErrorMessageType::Message))
            .with_label(
                Label::new((module_name, self.span.clone()))
                    .with_color(Color::Red)
                    .with_message(key.get_massage(text, ErrorMessageType::Label(0)))
            )
            .with_note(key.get_massage(text, ErrorMessageType::Note))
            .finish()
            .print((module_name, Source::from(context.source_code.code.as_str())))
            .unwrap();

        self.advice_report.print(context, self.span.start);
    }

    fn add_advice(&mut self, module_name: std::prelude::v1::String, advice: super::advice::Advice) {
        self.advice_report.add_advice(module_name, advice);
    }
}


pub(crate) struct UndefinedIdentifier {
    literal_span: Range<usize>,
    advice_report: AdviceReport
}

impl UndefinedIdentifier {
    pub(crate) fn new(literal_span: Range<usize>) -> TranspileError {
        return TranspileError::new(UndefinedIdentifier { literal_span, advice_report: AdviceReport::new() });
    }
}

impl TranspileReport for UndefinedIdentifier {
    fn print(&self, context: &super::context::TranspileModuleContext) {
        let module_name = &context.module_name;
        let text = &context.context.localized_text;
        let error_code = 0024;
        let key = ErrorMessageKey::new(error_code);

        Report::build(ReportKind::Error, module_name, self.literal_span.start)
            .with_code(error_code)
            .with_message(key.get_massage(text, ErrorMessageType::Message))
            .with_label(
                Label::new((module_name, self.literal_span.clone()))
                    .with_color(Color::Red)
                    .with_message(key.get_massage(text, ErrorMessageType::Label(0)))
            )
            .with_note(key.get_massage(text, ErrorMessageType::Note))
            .finish()
            .print((module_name, Source::from(context.source_code.code.as_str())))
            .unwrap();

        self.advice_report.print(context, self.literal_span.start);
    }

    fn add_advice(&mut self, module_name: std::prelude::v1::String, advice: super::advice::Advice) {
        self.advice_report.add_advice(module_name, advice);
    }
}