use std::{cell::RefCell, ops::Range};

use ariadne::{Color, Label, Report, ReportKind, Source};
use bumpalo::{collections::String, Bump};
use catla_parser::parser::{expression, Expression, ExpressionEnum, Generics, PrimaryLeftExpr, Program, SimplePrimary, Spanned, StatementAST, TypeInfo, TypeTag};
use either::Either::Left;
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
    DataStruct,
}

pub(crate) enum EnvironmentSeparatorKind {
    None,
    Function,
    DataStruct,
    Closure
}

pub(crate) struct NameEnvironment<'allocator> {
    map: RefCell<HashMap<String<'allocator>, DefineWithName<'allocator>, DefaultHashBuilder, &'allocator Bump>>,
    parent: Option<EntityID<'allocator>>,
    separator: EnvironmentSeparatorKind
}

impl<'allocator> NameEnvironment<'allocator> {
    
    pub(crate) fn new(parent: Option<EntityID<'allocator>>, separator: EnvironmentSeparatorKind, allocator: &'allocator Bump) -> NameEnvironment<'allocator> {
        return Self {
            map: RefCell::new(HashMap::new_in(allocator)),
            parent,
            separator
        };
    }

    pub(crate) fn get_name_define_info(&self, name: &str, environments: &ComponentContainer<NameEnvironment<'allocator>>) -> Option<DefineWithName<'allocator>> {
        return match self.map.borrow().get(name) {
            Some(entity_id) => Some(entity_id.clone()),
            _ => {
                let parent = match self.parent {
                    Some(parent) => &environments[parent],
                    _ => return None
                };
                parent.get_name_define_info(name, environments)
            }
        };
    }

    pub(crate) fn set_name_define_info(&self, name: String<'allocator>, define_info: DefineWithName<'allocator>) {
        self.map.borrow_mut().insert(name, define_info);
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
    name_environments.insert(current_environment_id, NameEnvironment::new(parent_env_id, EnvironmentSeparatorKind::None, allocator));
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
                        errors.push(NameAlreadyExists::new(define_info.span.clone(), already_exists.span.clone()));
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
                            errors.push(NameAlreadyExists::new(define_info.span.clone(), already_exists.span.clone()));
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
                        errors.push(NameAlreadyExists::new(define_info.span.clone(), already_exists.span.clone()));
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
            },
            //StatementAST::FunctionDefine(function_define) => {
                
            //},
            //StatementAST::DataStructDefine(_) => todo!(),
            //StatementAST::DropStatement(_) => todo!(),
            StatementAST::Expression(expression) => {
                name_resolve_expression(&expression, current_environment_id, name_environments, errors, warnings, allocator);
            },
            _ => {}
        }


        let name_environment = &name_environments[current_environment_id];
        match statement {
            StatementAST::VariableDefine(variable_define) => {
                if let Ok(variable_name) = &variable_define.name {
                    let entity_id = EntityID::from(variable_define);
                    let define_info = DefineWithName { entity_id, span: variable_name.span.clone(), define_kind: DefineKind::Variable };
                    let name = String::from_str_in(variable_name.value, allocator);
                    name_environment.set_name_define_info(name, define_info);
                }
            },
            _ => continue
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
    environment_id: EntityID,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {

}

fn name_resolve_generics<'allocator>(
    ast: &Generics<'allocator, '_>,
    environment_id: EntityID,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {

}

fn name_resolve_type_tag<'allocator>(
    ast: &TypeTag<'allocator, '_>,
    environment_id: EntityID,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {

}

fn name_resolve_type_info<'allocator>(
    ast: &TypeInfo<'allocator, '_>,
    environment_id: EntityID,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {

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