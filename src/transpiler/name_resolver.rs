use std::{cell::RefCell, ops::Range};

use ariadne::{Color, Label, Report, ReportKind, Source};
use bumpalo::{collections::String, Bump};
use catla_parser::{grammar::number_literal_regex, parser::{AddOrSubExpression, AndExpression, ArrayTypeInfo, BaseTypeInfo, Block, CompareExpression, EQNEExpression, Expression, ExpressionEnum, Factor, FunctionCall, Generics, GenericsDefine, Literal, MappingOperatorKind, MulOrDivExpression, Primary, PrimaryLeft, PrimaryLeftExpr, PrimaryRight, PrimarySeparatorKind, Program, SimplePrimary, Spanned, StatementAST, TypeAttributeEnum, TypeInfo, TypeTag, WhereClause}};
use either::Either::{Left, Right};
use fxhash::FxHashMap;
use hashbrown::{hash_map::DefaultHashBuilder, HashMap};

use crate::localize::localizer::LocalizedText;

use super::{component::{ComponentContainer, EntityID}, error::{ErrorMessageKey, ErrorMessageType, TranspileReport}, semantics::types::type_info::PRIMITIVE_TYPE_NAMES, TranspileError, TranspileWarning};


#[derive(Debug, Clone)]
pub(crate) struct DefineWithName {
    pub entity_id: EntityID,
    pub span: Range<usize>,
    pub define_kind: DefineKind
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum DefineKind {
    Import,
    Function,
    Variable,
    FunctionArgument,
    ClosureArgument,
    UserType,
    Generics
}

impl DefineKind {
    pub(crate) fn get_name(&self, text: &LocalizedText) -> std::string::String {
        let name = match self {
            DefineKind::Import => "import",
            DefineKind::Function => "function",
            DefineKind::Variable => "variable",
            DefineKind::FunctionArgument => "function_argument",
            DefineKind::ClosureArgument => "closure_argument",
            DefineKind::UserType => "user_type",
            DefineKind::Generics => "generics",
        };
        text.get_text(format!("name_resolved_kind.{}", name))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum EnvironmentSeparatorKind {
    Function,
    UserTypeDefine,
    Closure
}

#[derive(Debug)]
pub(crate) struct NameEnvironment<'allocator> {
    name_define_info_map: RefCell<HashMap<String<'allocator>, DefineWithName, DefaultHashBuilder, &'allocator Bump>>,
    parent: Option<EntityID>,
    separator: Option<EnvironmentSeparatorKind>,
    span : Range<usize>
}

impl<'allocator> NameEnvironment<'allocator> {
    
    pub(crate) fn new(
        parent: Option<EntityID>,
        separator: Option<EnvironmentSeparatorKind>,
        span: Range<usize>,
        allocator: &'allocator Bump
    ) -> NameEnvironment<'allocator> {
        Self {
            name_define_info_map: RefCell::new(HashMap::new_in(allocator)),
            parent,
            separator,
            span
        }
    }

    pub(crate) fn get_name_define_info(
        &self,
        name: &str,
        environments: &ComponentContainer<NameEnvironment<'allocator>>
    ) -> Option<FoundDefineInfo> {
        
        match self.name_define_info_map.borrow().get(name) {
            Some(define_info) => Some(FoundDefineInfo { define_info: define_info.clone(), separators: Vec::new() }),
            _ => {
                match self.parent {
                    Some(parent) => {
                        let mut separators = Vec::new();
                        let mut current_entity_id = parent;
                        loop {
                            let name_environment = &environments[current_entity_id];

                            match name_environment.name_define_info_map.borrow().get(name) {
                                Some(define_info) => return Some(FoundDefineInfo { define_info: define_info.clone(), separators }),
                                _ => {
                                    if let Some(separator) = name_environment.separator {
                                        separators.push(Spanned::new(separator, name_environment.span.clone()));
                                    }

                                    let parent = match name_environment.parent {
                                        Some(parent) => parent,
                                        _ => return None,
                                    };
                                    current_entity_id = parent;
                                }
                            }
                        }
                    },
                    _ => None
                }
            }
        }
    }

    pub(crate) fn set_name_define_info(&self, name: String<'allocator>, define_info: DefineWithName) {
        self.name_define_info_map.borrow_mut().insert(name, define_info);
    }

}

pub(crate) struct FoundDefineInfo {
    pub define_info: DefineWithName,
    pub separators: Vec<Spanned<EnvironmentSeparatorKind>>
}

impl FoundDefineInfo {
    
    pub(crate) fn has_separator(&self, kind: &[EnvironmentSeparatorKind]) -> bool {
        for separator in self.separators.iter() {
            if kind.contains(&separator.value) {
                return true;
            }
        }
        false
    }

}



pub(crate) fn name_resolve_program<'allocator>(
    ast: Program<'allocator, '_>,
    parent_env_id: Option<EntityID>,
    is_user_type_scope: bool,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    resolved_map: &mut FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    let current_environment_id = EntityID::from(ast);
    let environment_span = match parent_env_id {
        Some(entity_id) => name_environments[entity_id].span.clone(),
        None => ast.span.clone(),
    };
    name_environments.insert(
        current_environment_id,
        NameEnvironment::new(parent_env_id, None, environment_span.clone(), allocator)
    );
    let name_environment = &name_environments[current_environment_id];

    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            _ => continue
        };

        match statement {
            StatementAST::Import(import) => {
                for element in import.elements.elements.iter() {
                    let entity_id = EntityID::from(element);
                    let define_info = DefineWithName {
                        entity_id,
                        span: element.span.clone(),
                        define_kind: DefineKind::Import
                    };
                    
                    if let Some(already_exists) = name_environment.get_name_define_info(element.value, &name_environments) {
                        errors.push(NameAlreadyExists::new(
                            define_info.span.clone(),
                            already_exists.define_info.span.clone()
                        ));
                    } else {
                        let name = String::from_str_in(element.value, allocator);
                        name_environment.set_name_define_info(name, define_info);
                    }
                }
            },
            StatementAST::FunctionDefine(function_define) => {
                if let Ok(name) = &function_define.name {
                    let entity_id = EntityID::from(function_define);
                    let define_info = DefineWithName {
                        entity_id,
                        span: name.span.clone(),
                        define_kind: DefineKind::Function
                    };

                    if let Some(already_exists) = name_environment.get_name_define_info(name.value, name_environments) {
                        if !is_user_type_scope {
                            errors.push(NameAlreadyExists::new(
                                define_info.span.clone(),
                                already_exists.define_info.span.clone()
                            ));
                        }
                    } else {
                        let name = String::from_str_in(name.value, allocator);
                        name_environment.set_name_define_info(name, define_info);
                    }
                }
            },
            StatementAST::UserTypeDefine(data_struct_define) => {
                if let Ok(data_struct_name) = &data_struct_define.name {
                    let entity_id = EntityID::from(data_struct_define);
                    let define_info = DefineWithName {
                        entity_id,
                        span: data_struct_name.span.clone(),
                        define_kind: DefineKind::UserType
                    };

                    if let Some(already_exists) = name_environment.get_name_define_info(data_struct_name.value, name_environments) {
                        errors.push(NameAlreadyExists::new(
                            define_info.span.clone(),
                            already_exists.define_info.span.clone()
                        ));
                    } else {
                        let name = String::from_str_in(data_struct_name.value, allocator);
                        name_environment.set_name_define_info(name, define_info);
                    }
                }
            },
            StatementAST::TypeDefine(type_define) => {
                if let Ok(name) = &type_define.name {
                    let entity_id = EntityID::from(type_define);
                    let define_info = DefineWithName {
                        entity_id,
                        span: name.span.clone(),
                        define_kind: DefineKind::UserType
                    };

                    if let Some(already_exists) = name_environment.get_name_define_info(name.value, name_environments) {
                        errors.push(NameAlreadyExists::new(
                            define_info.span.clone(),
                            already_exists.define_info.span.clone()
                        ));
                    } else {
                        let name = String::from_str_in(name.value, allocator);
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
                name_resolve_expression(
                    assignment.left_expr,
                    current_environment_id,
                    name_environments,
                    resolved_map,
                    errors,
                    warnings,
                    allocator
                );
                
                if let Ok(right_expr) = assignment.right_expr {
                    name_resolve_expression(
                        right_expr,
                        current_environment_id,
                        name_environments,
                        resolved_map,
                        errors,
                        warnings,
                        allocator
                    );
                }
            },
            StatementAST::Exchange(exchange) => {
                name_resolve_expression(
                    exchange.left_expr,
                    current_environment_id,
                    name_environments,
                    resolved_map,
                    errors,
                    warnings,
                    allocator
                );
                
                if let Ok(right_expr) = exchange.right_expr {
                    name_resolve_expression(
                        right_expr,
                        current_environment_id,
                        name_environments,
                        resolved_map,
                        errors,
                        warnings,
                        allocator
                    );
                }
            },
            StatementAST::VariableDefine(variable_define) => {
                if let Some(expression) = &variable_define.expression {
                    if let Ok(expression) = expression {
                        name_resolve_expression(
                            &expression,
                            current_environment_id,
                            name_environments,
                            resolved_map,
                            errors,
                            warnings,
                            allocator
                        );
                    }
                }

                if let Some(type_tag) = &variable_define.type_tag {
                    name_resolve_type_tag(
                        type_tag,
                        current_environment_id,
                        name_environments,
                        resolved_map,
                        errors,
                        warnings,
                        allocator
                    );
                }

                if let Ok(variable_name) = &variable_define.name {
                    let entity_id = EntityID::from(variable_define);
                    let define_info = DefineWithName {
                        entity_id,
                        span: variable_name.span.clone(),
                        define_kind: DefineKind::Variable
                    };
                    let name = String::from_str_in(variable_name.value, allocator);
                    name_environments[current_environment_id].set_name_define_info(name, define_info);
                }
            },
            StatementAST::FunctionDefine(function_define) => {
                let environment_span = match &function_define.block.value {
                    Some(block) => block.span.clone(),
                    _ => function_define.span.clone()
                };
                let name_environment = NameEnvironment::new(
                    Some(current_environment_id),
                    Some(EnvironmentSeparatorKind::Function),
                    environment_span,
                    allocator
                );

                if let Some(generics_define) = &function_define.generics_define {
                    name_resolve_generics_define(
                        generics_define,
                        current_environment_id,
                        name_environments,
                        resolved_map,
                        errors,
                        warnings,
                        allocator
                    );
                }

                if let Some(type_tag) = &function_define.type_tag {
                    name_resolve_type_tag(
                        type_tag,
                        current_environment_id,
                        name_environments,
                        resolved_map,
                        errors,
                        warnings,
                        allocator
                    );
                }

                for argument in function_define.args.arguments.iter() {
                    name_resolve_type_tag(
                        &argument.type_tag,
                        current_environment_id,
                        name_environments,
                        resolved_map,
                        errors,
                        warnings,
                        allocator
                    );

                    let entity_id = EntityID::from(argument);
                    let define_info = DefineWithName {
                        entity_id,
                        span: argument.span.clone(),
                        define_kind: DefineKind::FunctionArgument
                    };
                    let name = String::from_str_in(argument.name.value, allocator);
                    name_environment.set_name_define_info(name, define_info);
                }

                if let Some(where_clause) = &function_define.where_clause {
                    name_resolve_where_clause(
                        where_clause,
                        current_environment_id,
                        name_environments,
                        resolved_map,
                        errors,
                        warnings,
                        allocator
                    );
                }

                let entity_id = EntityID::from(function_define);
                name_environments.insert(entity_id, name_environment);

                if let Some(block) = &function_define.block.value {
                    name_resolve_block(
                        block,
                        entity_id,
                        false,
                        name_environments,
                        resolved_map,
                        errors,
                        warnings,
                        allocator
                    );
                }
            },
            StatementAST::UserTypeDefine(data_struct_define) => {
                let name_environment = NameEnvironment::new(
                    Some(current_environment_id),
                    Some(EnvironmentSeparatorKind::UserTypeDefine),
                    data_struct_define.span.clone(),
                    allocator
                );
                let entity_id = EntityID::from(data_struct_define);
                name_environments.insert(entity_id, name_environment);

                if let Some(generics_define) = &data_struct_define.generics_define {
                    name_resolve_generics_define(
                        generics_define,
                        entity_id,
                        name_environments,
                        resolved_map,
                        errors,
                        warnings,
                        allocator
                    );
                }

                if let Some(super_type_info) = &data_struct_define.super_type_info {
                    for type_info in super_type_info.type_infos.iter() {
                        name_resolve_type_info(
                            type_info,
                            entity_id,
                            name_environments,
                            resolved_map,
                            errors,
                            warnings,
                            allocator
                        );
                    }
                }

                if let Some(where_clause) = &data_struct_define.where_clause {
                    name_resolve_where_clause(
                        where_clause,
                        entity_id,
                        name_environments,
                        resolved_map,
                        errors,
                        warnings,
                        allocator
                    );
                }

                if let Some(block) = &data_struct_define.block.value {
                    name_resolve_block(
                        block,
                        entity_id,
                        true,
                        name_environments,
                        resolved_map,
                        errors,
                        warnings,
                        allocator
                    );
                }
            },
            StatementAST::TypeDefine(type_define) => {
                let name_environment = NameEnvironment::new(
                    Some(current_environment_id),
                    Some(EnvironmentSeparatorKind::UserTypeDefine),
                    type_define.span.clone(),
                    allocator
                );
                let entity_id = EntityID::from(type_define);
                name_environments.insert(entity_id, name_environment);

                if let Some(generics_define) = &type_define.generics_define {
                    name_resolve_generics_define(
                        generics_define,
                        entity_id,
                        name_environments,
                        resolved_map,
                        errors,
                        warnings,
                        allocator
                    );
                }

                if let Ok(type_info) = &type_define.type_info {
                    name_resolve_type_info(
                        type_info,
                        entity_id,
                        name_environments,
                        resolved_map,
                        errors,
                        warnings,
                        allocator
                    );
                }
            },
            StatementAST::Implements(implements) => {
                let name_environment = NameEnvironment::new(
                    Some(current_environment_id),
                    Some(EnvironmentSeparatorKind::UserTypeDefine),
                    implements.span.clone(),
                    allocator
                );
                let entity_id = EntityID::from(implements);
                name_environments.insert(entity_id, name_environment);

                if let Some(generics_define) = &implements.generics_define {
                    name_resolve_generics_define(
                        generics_define,
                        entity_id,
                        name_environments,
                        resolved_map,
                        errors,
                        warnings,
                        allocator
                    );
                }
                if let Ok(type_info) = &implements.interface {
                    name_resolve_type_info(
                        type_info,
                        entity_id,
                        name_environments,
                        resolved_map,
                        errors,
                        warnings,
                        allocator
                    );
                }
                if let Ok(type_info) = &implements.target_user_type {
                    name_resolve_type_info(
                        type_info,
                        entity_id,
                        name_environments,
                        resolved_map,
                        errors,
                        warnings,
                        allocator
                    );
                }
                if let Some(where_clause) = &implements.where_clause {
                    name_resolve_where_clause(
                        where_clause,
                        entity_id,
                        name_environments,
                        resolved_map,
                        errors,
                        warnings,
                        allocator
                    );
                }

                if let Some(block) = &implements.block.value {
                    name_resolve_block(
                        block,
                        entity_id,
                        true,
                        name_environments,
                        resolved_map,
                        errors,
                        warnings,
                        allocator
                    );
                }
            },
            StatementAST::DropStatement(drop_statement) => {
                if let Ok(expression) = &drop_statement.expression {
                    name_resolve_expression(
                        &expression,
                        current_environment_id,
                        name_environments,
                        resolved_map,
                        errors,
                        warnings,
                        allocator
                    );
                }
            },
            StatementAST::Expression(expression) => {
                name_resolve_expression(
                    &expression,
                    current_environment_id,
                    name_environments,
                    resolved_map,
                    errors,
                    warnings,
                    allocator
                );
            },
            _ => {}
        }
    }
}

fn name_resolve_generics_define<'allocator>(
    ast: &'allocator GenericsDefine<'allocator, '_>,
    environment_id: EntityID,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    resolved_map: &mut FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    for element in ast.elements.iter() {
        let entity_id = EntityID::from(element);
        let define_info = DefineWithName {
            entity_id,
            span: element.span.clone(),
            define_kind: DefineKind::Generics
        };
        name_environments[environment_id].set_name_define_info(
            String::from_str_in(element.name.value, allocator),
            define_info
        );

        for bound_type_info in element.bounds.iter() {
            name_resolve_type_info(
                bound_type_info,
                environment_id,
                name_environments,
                resolved_map,
                errors,
                warnings,
                allocator
            );
        }
    }
}

fn name_resolve_where_clause<'allocator>(
    ast: &'allocator WhereClause<'allocator, '_>,
    environment_id: EntityID,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    resolved_map: &mut FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    for element in ast.elements.iter() {
        name_resolve_type_info(
            &element.target_type,
            environment_id,
            name_environments,
            resolved_map,
            errors,
            warnings,
            allocator
        );
        for bound in element.bounds.iter() {
            name_resolve_type_info(
                bound,
                environment_id,
                name_environments,
                resolved_map,
                errors,
                warnings,
                allocator
            );
        }
    }
}

fn name_resolve_expression<'allocator>(
    ast: Expression<'allocator, '_>,
    environment_id: EntityID,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    resolved_map: &mut FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    match ast {
        ExpressionEnum::OrExpression(or_expression) => {
            name_resolve_and_expression(
                &or_expression.left_expr,
                environment_id,
                name_environments,
                resolved_map,
                errors,
                warnings,
                allocator
            );
            for right_expr in or_expression.right_exprs.iter() {
                if let Ok(and_expression) = &right_expr.1 {
                    name_resolve_and_expression(
                        and_expression,
                        environment_id,
                        name_environments,
                        resolved_map,
                        errors,
                        warnings,
                        allocator
                    );
                }
            }
        },
        ExpressionEnum::ReturnExpression(return_expression) => {
            if let Some(expression) = &return_expression.expression {
                name_resolve_expression(
                    &expression,
                    environment_id,
                    name_environments,
                    resolved_map,
                    errors,
                    warnings,
                    allocator
                );
            }
        },
        ExpressionEnum::Closure(closure) => {
            let environment_span = name_environments[environment_id].span.clone();
            let name_environment = NameEnvironment::new(
                Some(environment_id),
                Some(EnvironmentSeparatorKind::Closure),
                environment_span,
                allocator
            );
            
            match &closure.arguments.arguments {
                Left(argument) => {
                    let entity_id = EntityID::from(argument);
                    let name = String::from_str_in(argument.value, allocator);
                    let define_info = DefineWithName {
                        entity_id,
                        span: argument.span.clone(),
                        define_kind: DefineKind::ClosureArgument
                    };
                    name_environment.set_name_define_info(name, define_info);
                },
                Right(arguments) => {
                    for argument in arguments.iter() {
                        match argument {
                            Left(argument) => {
                                let entity_id = EntityID::from(argument);
                                let name = String::from_str_in(argument.name.value, allocator);
                                let define_info = DefineWithName {
                                    entity_id,
                                    span: argument.name.span.clone(),
                                    define_kind: DefineKind::ClosureArgument
                                };
                                name_environment.set_name_define_info(name, define_info);
                            },
                            Right(argument) => {
                                let entity_id = EntityID::from(argument);
                                let name = String::from_str_in(argument.value, allocator);
                                let define_info = DefineWithName {
                                    entity_id,
                                    span: argument.span.clone(),
                                    define_kind: DefineKind::ClosureArgument
                                };
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
                        name_resolve_expression(
                            &expression,
                            entity_id,
                            name_environments,
                            resolved_map,
                            errors,
                            warnings,
                            allocator
                        );
                    },
                    Right(block) => {
                        name_resolve_block(
                            block,
                            entity_id,
                            false,
                            name_environments,
                            resolved_map,
                            errors,
                            warnings,
                            allocator
                        );
                    }
                }
            }
        }
    }
}

fn name_resolve_block<'allocator>(
    ast: &'allocator Block<'allocator, '_>,
    environment_id: EntityID,
    is_user_type_scope: bool,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    resolved_map: &mut FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    let name_environment = NameEnvironment::new(
        Some(environment_id),
        None,
        ast.span.clone(),
        allocator
    );
    let entity_id = EntityID::from(ast);
    name_environments.insert(entity_id, name_environment);

    name_resolve_program(
        ast.program,
        Some(entity_id),
        is_user_type_scope,
        name_environments,
        resolved_map,
        errors,
        warnings,
        allocator
    );
}

fn name_resolve_and_expression<'allocator>(
    ast: &'allocator AndExpression<'allocator, '_>,
    environment_id: EntityID,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    resolved_map: &mut FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    name_resolve_eqne_expression(
        &ast.left_expr,
        environment_id,
        name_environments,
        resolved_map,
        errors,
        warnings,
        allocator
    );
    for right_expr in ast.right_exprs.iter() {
        if let Ok(eqne_expression) = &right_expr.1 {
            name_resolve_eqne_expression(
                eqne_expression,
                environment_id,
                name_environments,
                resolved_map,
                errors,
                warnings,
                allocator
            );
        }
    }
}

fn name_resolve_eqne_expression<'allocator>(
    ast: &'allocator EQNEExpression<'allocator, '_>,
    environment_id: EntityID,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    resolved_map: &mut FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    name_resolve_compare_expression(
        &ast.left_expr,
        environment_id,
        name_environments,
        resolved_map,
        errors,
        warnings,
        allocator
    );
    for right_expr in ast.right_exprs.iter() {
        if let Ok(compare_expression) = &right_expr.1 {
            name_resolve_compare_expression(
                compare_expression,
                environment_id,
                name_environments,
                resolved_map,
                errors,
                warnings,
                allocator
            );
        }
    }
}

fn name_resolve_compare_expression<'allocator>(
    ast: &'allocator CompareExpression<'allocator, '_>,
    environment_id: EntityID,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    resolved_map: &mut FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    name_resolve_add_or_sub_expression(
        &ast.left_expr,
        environment_id,
        name_environments,
        resolved_map,
        errors,
        warnings,
        allocator
    );
    for right_expr in ast.right_exprs.iter() {
        if let Ok(add_or_sub_expression) = &right_expr.1 {
            name_resolve_add_or_sub_expression(
                add_or_sub_expression,
                environment_id,
                name_environments,
                resolved_map,
                errors,
                warnings,
                allocator
            );
        }
    }
}

fn name_resolve_add_or_sub_expression<'allocator>(
    ast: &'allocator AddOrSubExpression<'allocator, '_>,
    environment_id: EntityID,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    resolved_map: &mut FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    name_resolve_mul_or_div_expression(
        &ast.left_expr,
        environment_id,
        name_environments,
        resolved_map,
        errors,
        warnings,
        allocator
    );
    for right_expr in ast.right_exprs.iter() {
        if let Ok(add_or_sub_expression) = &right_expr.1 {
            name_resolve_mul_or_div_expression(
                add_or_sub_expression,
                environment_id,
                name_environments,
                resolved_map,
                errors,
                warnings,
                allocator
            );
        }
    }
}

fn name_resolve_mul_or_div_expression<'allocator>(
    ast: &'allocator MulOrDivExpression<'allocator, '_>,
    environment_id: EntityID,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    resolved_map: &mut FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    name_resolve_factor(
        &ast.left_expr,
        environment_id,
        name_environments,
        resolved_map,
        errors,
        warnings,
        allocator
    );
    for right_expr in ast.right_exprs.iter() {
        if let Ok(add_or_sub_expression) = &right_expr.1 {
            name_resolve_factor(
                add_or_sub_expression,
                environment_id,
                name_environments,
                resolved_map,
                errors,
                warnings,
                allocator
            );
        }
    }
}

fn name_resolve_factor<'allocator>(
    ast: &'allocator Factor<'allocator, '_>,
    environment_id: EntityID,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    resolved_map: &mut FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    if let Ok(primary) = &ast.primary {
        name_resolve_primary(
            primary,
            environment_id,
            name_environments,
            resolved_map,
            errors,
            warnings,
            allocator
        );
    }
}

fn name_resolve_primary<'allocator>(
    ast: &'allocator Primary<'allocator, '_>,
    environment_id: EntityID,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    resolved_map: &mut FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    name_resolve_primary_left(
        &ast.left,
        environment_id,
        name_environments,
        resolved_map,
        errors,
        warnings,
        allocator
    );
    
    let mut has_double_colon = false;
    for primary_right in ast.chain.iter() {
        if primary_right.separator.value == PrimarySeparatorKind::DoubleColon {
            has_double_colon = true;
        }
        name_resolve_primary_right(
            primary_right,
            environment_id,
            name_environments,
            resolved_map,
            errors,
            warnings,
            allocator
        );
    }

    if let PrimaryLeftExpr::Simple(simple) = &ast.left.first_expr {
        let collect_error = !(ast.left.mapping_operator.is_none() && has_double_colon);
        
        if let SimplePrimary::Identifier(literal) = &simple.0 {
            name_resolve_literal(
                literal,
                collect_error,
                environment_id,
                name_environments,
                resolved_map,
                errors
            );
        }
    }
}

fn name_resolve_primary_left<'allocator>(
    ast: &'allocator PrimaryLeft<'allocator, '_>,
    environment_id: EntityID,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    resolved_map: &mut FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    match &ast.first_expr {
        PrimaryLeftExpr::Simple(simple) => {
            match &simple.0 {
                SimplePrimary::Expression { expression, error_tokens: _, span: _ } => {
                    if let Ok(expression) = expression {
                        name_resolve_expression(
                            &expression,
                            environment_id,
                            name_environments,
                            resolved_map,
                            errors,
                            warnings,
                            allocator
                        );
                    }
                },
                _ => {}
            }
            if let Some(generics) = &simple.1 {
                if let Ok(generics) = generics {
                    name_resolve_generics(
                        generics,
                        environment_id,
                        name_environments,
                        resolved_map,
                        errors,
                        warnings,
                        allocator
                    );
                }
            }
            if let Some(function_call) = &simple.2 {
                name_resolve_function_call(
                    function_call,
                    environment_id,
                    name_environments,
                    resolved_map,
                    errors,
                    warnings,
                    allocator
                );
            }
        },
        PrimaryLeftExpr::NewArrayInitExpression(new_array_init_expression) => {
            if let Ok(init_expression) = new_array_init_expression.init_expression {
                name_resolve_expression(
                    init_expression,
                    environment_id,
                    name_environments,
                    resolved_map,
                    errors,
                    warnings,
                    allocator
                );
            }
            if let Ok(length_expression) = new_array_init_expression.length_expression {
                name_resolve_expression(
                    length_expression,
                    environment_id,
                    name_environments,
                    resolved_map,
                    errors,
                    warnings,
                    allocator
                );
            }
        },
        PrimaryLeftExpr::NewArrayExpression(new_array_expression) => {
            for value_expression in new_array_expression.value_expressions.iter() {
                if let Ok(value_expression) = value_expression {
                    name_resolve_expression(
                        value_expression,
                        environment_id,
                        name_environments,
                        resolved_map,
                        errors,
                        warnings,
                        allocator
                    );
                }
            }
        },
        PrimaryLeftExpr::NewExpression(new_expression) => {
            if let Some(first_literal) = new_expression.path.first() {
                let collect_error = new_expression.path.len() == 1;
                name_resolve_literal(
                    first_literal,
                    collect_error,
                    environment_id,
                    name_environments,
                    resolved_map,
                    errors
                );
            }

            if let Ok(field_assigns) = &new_expression.field_assigns {
                for field_assign in field_assigns.iter() {
                    if let Ok(expression) = &field_assign.expression {
                        name_resolve_expression(
                            &expression,
                            environment_id,
                            name_environments,
                            resolved_map,
                            errors,
                            warnings,
                            allocator
                        );
                    }
                }
            }
        },
        PrimaryLeftExpr::IfExpression(if_expression) => {
            let if_statement = &if_expression.if_statement;
            let environment_span = match &if_statement.block.value {
                Some(block) => block.span.clone(),
                _ => if_statement.span.clone(),
            };
            let name_environment = NameEnvironment::new(
                Some(environment_id),
                None,
                environment_span,
                allocator
            );
            let entity_id = EntityID::from(if_statement);
            name_environments.insert(entity_id, name_environment);

            if let Ok(condition) = &if_statement.condition {
                name_resolve_expression(
                    &condition,
                    entity_id,
                    name_environments,
                    resolved_map,
                    errors,
                    warnings,
                    allocator
                );
            }
            if let Some(block) = &if_statement.block.value {
                name_resolve_block(
                    block,
                    entity_id,
                    false,
                    name_environments,
                    resolved_map,
                    errors,
                    warnings,
                    allocator
                );
            }

            for else_or_if_statement in if_expression.chain.iter() {
                if let Some(else_or_if_statement) = &else_or_if_statement.else_if_or_else.value {
                    match else_or_if_statement {
                        Left(if_statement) => {
                            let environment_span = match &if_statement.block.value {
                                Some(block) => block.span.clone(),
                                _ => if_statement.span.clone(),
                            };
                            let name_environment = NameEnvironment::new(
                                Some(environment_id),
                                None,
                                environment_span,
                                allocator
                            );
                            let entity_id = EntityID::from(if_statement);
                            name_environments.insert(entity_id, name_environment);

                            if let Ok(condition) = &if_statement.condition {
                                name_resolve_expression(
                                    &condition,
                                    entity_id,
                                    name_environments,
                                    resolved_map,
                                    errors,
                                    warnings,
                                    allocator
                                );
                            }
                            if let Some(block) = &if_statement.block.value {
                                name_resolve_block(
                                    block,
                                    entity_id,
                                    false,
                                    name_environments,
                                    resolved_map,
                                    errors,
                                    warnings,
                                    allocator
                                );
                            }
                        },
                        Right(block) => {
                            name_resolve_block(
                                block,
                                environment_id,
                                false,
                                name_environments,
                                resolved_map,
                                errors,
                                warnings,
                                allocator
                            );
                        }
                    }
                }
            }
        },
        PrimaryLeftExpr::LoopExpression(loop_expression) => {
            if let Ok(block) = &loop_expression.block {
                name_resolve_block(
                    block,
                    environment_id,
                    false,
                    name_environments,
                    resolved_map,
                    errors,
                    warnings,
                    allocator
                );
            }
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        name_resolve_mapping_operator(
            &mapping_operator.value,
            environment_id,
            name_environments,
            resolved_map,
            errors,
            warnings,
            allocator
        );
    }
}

fn name_resolve_mapping_operator<'allocator>(
    ast: &'allocator MappingOperatorKind<'allocator, '_>,
    environment_id: EntityID,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    resolved_map: &mut FxHashMap<EntityID, FoundDefineInfo>,
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
        name_resolve_block(
            block,
            environment_id,
            false,
            name_environments,
            resolved_map,
            errors,
            warnings,
            allocator
        );
    }
}

fn name_resolve_primary_right<'allocator>(
    ast: &'allocator PrimaryRight<'allocator, '_>,
    environment_id: EntityID,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    resolved_map: &mut FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    if let Some(second_expr) = &ast.second_expr {
        if let Some(generics) = &second_expr.1 {
            if let Ok(generics) = generics {
                name_resolve_generics(
                    generics,
                    environment_id,
                    name_environments,
                    resolved_map,
                    errors,
                    warnings,
                    allocator
                );
            }
        }
        if let Some(function_call) = &second_expr.2 {
            name_resolve_function_call(
                function_call,
                environment_id,
                name_environments,
                resolved_map,
                errors,
                warnings,
                allocator
            );
        }
    }
    if let Some(mapping_operator) = &ast.mapping_operator {
        name_resolve_mapping_operator(
            &mapping_operator.value,
            environment_id,
            name_environments,
            resolved_map,
            errors,
            warnings,
            allocator
        );
    }
}

fn name_resolve_function_call<'allocator>(
    ast: &'allocator FunctionCall<'allocator, '_>,
    environment_id: EntityID,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    resolved_map: &mut FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    if let Ok(arg_exprs) = &ast.arg_exprs {
        for expression in arg_exprs.iter() {
            name_resolve_expression(
                &expression,
                environment_id,
                name_environments,
                resolved_map,
                errors,
                warnings,
                allocator
            );
        }
    }
}


fn name_resolve_generics<'allocator>(
    ast: &'allocator Generics<'allocator, '_>,
    environment_id: EntityID,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    resolved_map: &mut FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    for type_info in ast.elements.iter() {
        name_resolve_type_info(
            type_info,
            environment_id,
            name_environments,
            resolved_map,
            errors,
            warnings,
            allocator
        );
    }
}

fn name_resolve_type_tag<'allocator>(
    ast: &'allocator TypeTag<'allocator, '_>,
    environment_id: EntityID,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    resolved_map: &mut FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    if let Ok(type_info) = &ast.type_info {
        name_resolve_type_info(
            type_info,
            environment_id,
            name_environments,
            resolved_map,
            errors,
            warnings,
            allocator
        );
    }
}

fn name_resolve_type_info<'allocator>(
    ast: &'allocator TypeInfo<'allocator, '_>,
    environment_id: EntityID,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    resolved_map: &mut FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    match ast {
        TypeInfo::BaseType(base_type_info) => {
            name_resolve_base_type_info(
                base_type_info,
                environment_id,
                name_environments,
                resolved_map,
                errors,
                warnings,
                allocator
            );
        },
        TypeInfo::ArrayType(array_type_info) => {
            name_resolve_array_type_info(
                array_type_info,
                environment_id,
                name_environments,
                resolved_map,
                errors,
                warnings,
                allocator
            );
        }
    }
}

fn name_resolve_array_type_info<'allocator>(
    ast: &'allocator ArrayTypeInfo<'allocator, '_>,
    environment_id: EntityID,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    resolved_map: &mut FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    if let Ok(type_info) = ast.type_info {
        name_resolve_type_info(
            type_info,
            environment_id,
            name_environments,
            resolved_map,
            errors,
            warnings,
            allocator
        );
    }
}

fn name_resolve_base_type_info<'allocator>(
    ast: &'allocator BaseTypeInfo<'allocator, '_>,
    environment_id: EntityID,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    resolved_map: &mut FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    allocator: &'allocator Bump
) {
    if ast.path.len() >= 1 {
        let collect_error = ast.path.len() == 1 && !PRIMITIVE_TYPE_NAMES.contains(&ast.path[0].value);
        name_resolve_literal(
            &ast.path[0],
            collect_error,
            environment_id,
            name_environments,
            resolved_map,
            errors
        );
    }

    if let Some(generics) = &ast.generics {
        name_resolve_generics(
            generics,
            environment_id,
            name_environments,
            resolved_map,
            errors,
            warnings,
            allocator
        );
    }

    for attribute in ast.type_attributes.iter() {
        if let TypeAttributeEnum::Result(error_type_generics) = &attribute.value {
            if let Some(generics) = error_type_generics {
                name_resolve_generics(
                    generics,
                    environment_id,
                    name_environments,
                    resolved_map,
                    errors,
                    warnings,
                    allocator
                );
            }
        }
    }
}

fn name_resolve_literal<'allocator>(
    literal: &'allocator Literal<'allocator>,
    collect_error: bool,
    environment_id: EntityID,
    name_environments: &mut ComponentContainer<'allocator, NameEnvironment<'allocator>>,
    resolved_map: &mut FxHashMap<EntityID, FoundDefineInfo>,
    errors: &mut Vec<TranspileError>,
) {
    if number_literal_regex(literal.value) {
        // is numeric literal
        return;
    }

    let name_environment = &name_environments[environment_id];
    match name_environment.get_name_define_info(literal.value, &name_environments) {
        Some(define_info) => {
            resolved_map.insert(EntityID::from(literal), define_info);
        },
        _ => {
            if collect_error {
                errors.push(UndefinedIdentifier::new(literal.span.clone()));
            }
        }
    }
}



pub(crate) struct NameAlreadyExists {
    define_span: Range<usize>,
    already_exists_span: Range<usize>,
}

impl NameAlreadyExists {
    pub(crate) fn new(define_span: Range<usize>, already_exists_span: Range<usize>) -> TranspileError {
        return TranspileError::new(Self {
            define_span,
            already_exists_span
        });
    }
}

impl TranspileReport for NameAlreadyExists {
    fn print(&self, context: &super::context::TranspileModuleContext) {
        let module_name = &context.module_name;

        let text = &context.context.localized_text;
        let error_code = 0023;
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
    }
}



pub(crate) struct UndefinedIdentifier {
    literal_span: Range<usize>
}

impl UndefinedIdentifier {
    pub(crate) fn new(literal_span: Range<usize>) -> TranspileError {
        return TranspileError::new(UndefinedIdentifier { literal_span });
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
    }
}