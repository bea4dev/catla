use std::ops::Range;

use catla_parser::ast::{
    AddOrSubExpression, AndExpression, ArrayTypeInfo, BaseTypeInfo, ClosureArgumentsOrLiteral,
    Define, ElementsOrWildCard, ElseChain, EntityID, EqualsExpression, Expression,
    ExpressionOrBlock, Factor, FunctionArgument, FunctionArgumentOrVariableBinding, GenericsDefine,
    GenericsInfo, LessOrGreaterExpression, Literal, MappingOperator, MulOrDivExpression,
    OrExpression, Primary, PrimaryLeft, PrimaryLeftExpr, PrimaryRight, Program, SimplePrimary,
    Statement, StatementAttribute, TupleTypeInfo, TypeInfo, TypeInfoBase, VariableBinding,
    WhereClause,
};
use hashbrown::HashMap;

use crate::{
    DefineInfo, DefineKind, EnvironmentSeparatorKind, NameEnvironmentID, NameEnvironmentSet,
    NameResolveError, NameResolveErrorKind, ResolvedInfo,
};

pub(crate) fn resolve_name_for_program<'input, 'name_env_alloc>(
    ast: &Program<'input, '_>,
    environment: NameEnvironmentID,
    components: &mut NameEnvironmentSet<'input, 'name_env_alloc>,
    resolved_map: &mut HashMap<EntityID, ResolvedInfo>,
    all_crates_and_auto_import: &std::vec::Vec<String>,
    wild_card_imports: &HashMap<EntityID, std::vec::Vec<String>>,
    module_element_names: &mut HashMap<String, Range<usize>>,
    is_user_type_or_impl_scope: bool,
    errors: &mut std::vec::Vec<NameResolveError>,
) {
    for statement in ast.statements.iter() {
        match &statement.statement {
            Statement::Import(import_statement) => match &import_statement.elements_or_wild_card {
                Some(elements_or_wild_card) => match elements_or_wild_card {
                    ElementsOrWildCard::Elements(elements) => {
                        for element in elements.iter() {
                            let define = DefineInfo {
                                entity_id: element.into(),
                                span: element.span.clone(),
                                kind: DefineKind::Import,
                            };
                            components.define(environment, element.value.into(), define);
                        }
                    }
                    ElementsOrWildCard::WildCard(literal) => {
                        let imports = wild_card_imports.get(&EntityID::from(literal)).unwrap();

                        for element in imports.iter() {
                            let define = DefineInfo {
                                entity_id: literal.into(),
                                span: literal.span.clone(),
                                kind: DefineKind::Import,
                            };
                            components.define(environment, element.clone().into(), define);
                        }
                    }
                },
                None => {
                    if let Some(last) = import_statement.path.last() {
                        let define = DefineInfo {
                            entity_id: last.into(),
                            span: last.span.clone(),
                            kind: DefineKind::Import,
                        };
                        components.define(environment, last.value.into(), define);
                    }
                }
            },
            Statement::DefineWithAttribute(define_with_attribute) => {
                if let Ok(define) = &define_with_attribute.define {
                    fn collect_module_element_name(
                        name: &Literal,
                        module_element_names: &mut HashMap<String, Range<usize>>,
                        errors: &mut std::vec::Vec<NameResolveError>,
                    ) {
                        match module_element_names.get(name.value) {
                            Some(exists) => {
                                let error = NameResolveError {
                                    name: name.value.to_string(),
                                    kind: NameResolveErrorKind::DuplicatedName {
                                        exists: exists.clone(),
                                    },
                                    span: name.span.clone(),
                                };
                                errors.push(error);
                            }
                            None => {
                                module_element_names
                                    .insert(name.value.to_string(), name.span.clone());
                            }
                        }
                    }

                    match define {
                        Define::Function(function_define) => {
                            if let Ok(name) = &function_define.name {
                                let define = DefineInfo {
                                    entity_id: function_define.into(),
                                    span: name.span.clone(),
                                    kind: DefineKind::Function,
                                };
                                components.define(environment, name.value.into(), define);

                                if !is_user_type_or_impl_scope {
                                    collect_module_element_name(name, module_element_names, errors);
                                }
                            }
                        }
                        Define::UserType(user_type_define) => {
                            if let Ok(name) = &user_type_define.name {
                                let define = DefineInfo {
                                    entity_id: user_type_define.into(),
                                    span: name.span.clone(),
                                    kind: DefineKind::UserType,
                                };
                                components.define(environment, name.value.into(), define);

                                collect_module_element_name(name, module_element_names, errors);
                            }
                        }
                        Define::Variable(variable_define) => {
                            if define_with_attribute
                                .attribute
                                .iter()
                                .any(|attribute| attribute.value == StatementAttribute::Static)
                            {
                                if let Ok(binding) = &variable_define.binding {
                                    resolve_name_for_variable_binding(
                                        binding,
                                        environment,
                                        components,
                                        module_element_names,
                                        errors,
                                    );

                                    if let VariableBinding::Literal(name) = binding {
                                        collect_module_element_name(
                                            name,
                                            module_element_names,
                                            errors,
                                        );
                                    }
                                }
                            }
                        }
                        Define::TypeAlias(type_alias) => {
                            if let Ok(name) = &type_alias.name {
                                let define = DefineInfo {
                                    entity_id: type_alias.into(),
                                    span: name.span.clone(),
                                    kind: DefineKind::UserType,
                                };
                                components.define(environment, name.value.into(), define);

                                collect_module_element_name(name, module_element_names, errors);
                            }
                        }
                    }
                }
            }
            _ => {}
        }
    }

    for statement in ast.statements.iter() {
        match &statement.statement {
            Statement::Assignment(assignment) => {
                resolve_name_for_expression(
                    &assignment.left,
                    environment,
                    components,
                    resolved_map,
                    all_crates_and_auto_import,
                    wild_card_imports,
                    module_element_names,
                    errors,
                );

                if let Ok(right) = &assignment.right {
                    resolve_name_for_expression(
                        right,
                        environment,
                        components,
                        resolved_map,
                        all_crates_and_auto_import,
                        wild_card_imports,
                        module_element_names,
                        errors,
                    );
                }
            }
            Statement::Swap(swap_statement) => {
                resolve_name_for_expression(
                    &swap_statement.left,
                    environment,
                    components,
                    resolved_map,
                    all_crates_and_auto_import,
                    wild_card_imports,
                    module_element_names,
                    errors,
                );

                if let Ok(right) = &swap_statement.right {
                    resolve_name_for_expression(
                        right,
                        environment,
                        components,
                        resolved_map,
                        all_crates_and_auto_import,
                        wild_card_imports,
                        module_element_names,
                        errors,
                    );
                }
            }
            Statement::Import(_) => {}
            Statement::DefineWithAttribute(define_with_attribute) => {
                if let Ok(define) = &define_with_attribute.define {
                    match define {
                        Define::Function(function_define) => {
                            let environment = components.new_environment(
                                Some(environment),
                                EnvironmentSeparatorKind::Function,
                                function_define.span.clone(),
                            );

                            if let Some(generics) = &function_define.generics {
                                resolve_name_for_generics_define(
                                    generics,
                                    environment,
                                    components,
                                    resolved_map,
                                    all_crates_and_auto_import,
                                    wild_card_imports,
                                    module_element_names,
                                    errors,
                                );
                            }

                            if let Ok(arguments) = &function_define.arguments {
                                for argument in arguments.arguments.iter() {
                                    resolve_name_for_function_argument(
                                        argument,
                                        environment,
                                        components,
                                        resolved_map,
                                        all_crates_and_auto_import,
                                        wild_card_imports,
                                        module_element_names,
                                        errors,
                                    );
                                }
                            }

                            if let Some(return_type) = &function_define.return_type {
                                if let Ok(return_type) = &return_type.type_info {
                                    resolve_name_for_type_info(
                                        return_type,
                                        environment,
                                        components,
                                        resolved_map,
                                        all_crates_and_auto_import,
                                        wild_card_imports,
                                        module_element_names,
                                        errors,
                                    );
                                }
                            }

                            if let Some(where_clause) = &function_define.where_clause {
                                resolve_name_for_where_clause(
                                    where_clause,
                                    environment,
                                    components,
                                    resolved_map,
                                    all_crates_and_auto_import,
                                    wild_card_imports,
                                    module_element_names,
                                    errors,
                                );
                            }

                            if let Some(block) = &function_define.block {
                                resolve_name_for_program(
                                    block.program,
                                    environment,
                                    components,
                                    resolved_map,
                                    all_crates_and_auto_import,
                                    wild_card_imports,
                                    module_element_names,
                                    false,
                                    errors,
                                );
                            }
                        }
                        Define::UserType(user_type_define) => {
                            let environment = components.new_environment(
                                Some(environment),
                                EnvironmentSeparatorKind::UserTypeDefine,
                                user_type_define.span.clone(),
                            );

                            if let Some(generics) = &user_type_define.generics {
                                resolve_name_for_generics_define(
                                    generics,
                                    environment,
                                    components,
                                    resolved_map,
                                    all_crates_and_auto_import,
                                    wild_card_imports,
                                    module_element_names,
                                    errors,
                                );
                            }

                            if let Some(super_type_info) = &user_type_define.super_type {
                                for type_info in super_type_info.types.iter() {
                                    resolve_name_for_type_info(
                                        type_info,
                                        environment,
                                        components,
                                        resolved_map,
                                        all_crates_and_auto_import,
                                        wild_card_imports,
                                        module_element_names,
                                        errors,
                                    );
                                }
                            }

                            if let Some(where_clause) = &user_type_define.where_clause {
                                resolve_name_for_where_clause(
                                    where_clause,
                                    environment,
                                    components,
                                    resolved_map,
                                    all_crates_and_auto_import,
                                    wild_card_imports,
                                    module_element_names,
                                    errors,
                                );
                            }

                            if let Ok(block) = &user_type_define.block {
                                resolve_name_for_program(
                                    block.program,
                                    environment,
                                    components,
                                    resolved_map,
                                    all_crates_and_auto_import,
                                    wild_card_imports,
                                    module_element_names,
                                    true,
                                    errors,
                                );
                            }
                        }
                        Define::Variable(variable_define) => {
                            if let Some(expression) = &variable_define.expression {
                                resolve_name_for_expression(
                                    expression,
                                    environment,
                                    components,
                                    resolved_map,
                                    all_crates_and_auto_import,
                                    wild_card_imports,
                                    module_element_names,
                                    errors,
                                );
                            }

                            if !define_with_attribute
                                .attribute
                                .iter()
                                .any(|attribute| attribute.value == StatementAttribute::Static)
                            {
                                if let Ok(binding) = &variable_define.binding {
                                    resolve_name_for_variable_binding(
                                        binding,
                                        environment,
                                        components,
                                        module_element_names,
                                        errors,
                                    );
                                }
                            }

                            if let Some(type_tag) = &variable_define.type_tag {
                                if let Ok(type_info) = &type_tag.type_info {
                                    resolve_name_for_type_info(
                                        type_info,
                                        environment,
                                        components,
                                        resolved_map,
                                        all_crates_and_auto_import,
                                        wild_card_imports,
                                        module_element_names,
                                        errors,
                                    );
                                }
                            }
                        }
                        Define::TypeAlias(type_alias) => {
                            if let Some(generics) = &type_alias.generics {
                                resolve_name_for_generics_define(
                                    generics,
                                    environment,
                                    components,
                                    resolved_map,
                                    all_crates_and_auto_import,
                                    wild_card_imports,
                                    module_element_names,
                                    errors,
                                );
                            }

                            if let Ok(type_info) = &type_alias.alias_type {
                                resolve_name_for_type_info(
                                    type_info,
                                    environment,
                                    components,
                                    resolved_map,
                                    all_crates_and_auto_import,
                                    wild_card_imports,
                                    module_element_names,
                                    errors,
                                );
                            }
                        }
                    }
                }
            }
            Statement::Drop(drop_statement) => {
                if let Ok(expression) = &drop_statement.expression {
                    resolve_name_for_expression(
                        expression,
                        environment,
                        components,
                        resolved_map,
                        all_crates_and_auto_import,
                        wild_card_imports,
                        module_element_names,
                        errors,
                    );
                }
            }
            Statement::Expression(expression) => {
                resolve_name_for_expression(
                    expression,
                    environment,
                    components,
                    resolved_map,
                    all_crates_and_auto_import,
                    wild_card_imports,
                    module_element_names,
                    errors,
                );
            }
            Statement::Implements(implements) => {
                let environment = components.new_environment(
                    Some(environment),
                    EnvironmentSeparatorKind::UserTypeDefine,
                    implements.span.clone(),
                );

                if let Some(generics) = &implements.generics {
                    resolve_name_for_generics_define(
                        generics,
                        environment,
                        components,
                        resolved_map,
                        all_crates_and_auto_import,
                        wild_card_imports,
                        module_element_names,
                        errors,
                    );
                }

                if let Ok(interface) = &implements.interface {
                    resolve_name_for_type_info(
                        interface,
                        environment,
                        components,
                        resolved_map,
                        all_crates_and_auto_import,
                        wild_card_imports,
                        module_element_names,
                        errors,
                    );
                }

                if let Ok(target) = &implements.concrete {
                    resolve_name_for_type_info(
                        target,
                        environment,
                        components,
                        resolved_map,
                        all_crates_and_auto_import,
                        wild_card_imports,
                        module_element_names,
                        errors,
                    );
                }

                if let Some(where_clause) = &implements.where_clause {
                    resolve_name_for_where_clause(
                        where_clause,
                        environment,
                        components,
                        resolved_map,
                        all_crates_and_auto_import,
                        wild_card_imports,
                        module_element_names,
                        errors,
                    );
                }

                if let Ok(block) = &implements.block {
                    resolve_name_for_program(
                        block.program,
                        environment,
                        components,
                        resolved_map,
                        all_crates_and_auto_import,
                        wild_card_imports,
                        module_element_names,
                        true,
                        errors,
                    );
                }
            }
        }
    }
}

fn resolve_name_for_variable_binding<'input, 'name_env_alloc>(
    ast: &VariableBinding<'input, '_>,
    environment: NameEnvironmentID,
    components: &mut NameEnvironmentSet<'input, 'name_env_alloc>,
    module_element_names: &mut HashMap<String, Range<usize>>,
    errors: &mut std::vec::Vec<NameResolveError>,
) {
    match ast {
        VariableBinding::Literal(literal) => {
            let define = DefineInfo {
                entity_id: literal.into(),
                span: literal.span.clone(),
                kind: DefineKind::Variable,
            };
            components.define(environment, literal.value.into(), define);
        }
        VariableBinding::Binding { bindings, span: _ } => {
            for binding in bindings.iter() {
                resolve_name_for_variable_binding(
                    binding,
                    environment,
                    components,
                    module_element_names,
                    errors,
                );
            }
        }
    }
}

fn resolve_name_for_function_argument<'input, 'name_env_alloc>(
    ast: &FunctionArgument<'input, '_>,
    environment: NameEnvironmentID,
    components: &mut NameEnvironmentSet<'input, 'name_env_alloc>,
    resolved_map: &mut HashMap<EntityID, ResolvedInfo>,
    all_crates_and_auto_import: &std::vec::Vec<String>,
    wild_card_imports: &HashMap<EntityID, std::vec::Vec<String>>,
    module_element_names: &mut HashMap<String, Range<usize>>,
    errors: &mut std::vec::Vec<NameResolveError>,
) {
    resolve_name_for_variable_binding(
        &ast.binding,
        environment,
        components,
        module_element_names,
        errors,
    );

    if let Ok(type_info) = &ast.type_tag.type_info {
        resolve_name_for_type_info(
            type_info,
            environment,
            components,
            resolved_map,
            all_crates_and_auto_import,
            wild_card_imports,
            module_element_names,
            errors,
        );
    }
}

fn resolve_name_for_generics_define<'input, 'name_env_alloc>(
    ast: &GenericsDefine<'input, '_>,
    environment: NameEnvironmentID,
    components: &mut NameEnvironmentSet<'input, 'name_env_alloc>,
    resolved_map: &mut HashMap<EntityID, ResolvedInfo>,
    all_crates_and_auto_import: &std::vec::Vec<String>,
    wild_card_imports: &HashMap<EntityID, std::vec::Vec<String>>,
    module_element_names: &mut HashMap<String, Range<usize>>,
    errors: &mut std::vec::Vec<NameResolveError>,
) {
    for element in ast.elements.iter() {
        let define = DefineInfo {
            entity_id: element.into(),
            span: element.name.span.clone(),
            kind: DefineKind::Generics,
        };
        components.define(environment, element.name.value.into(), define);

        for bound in element.bounds.iter() {
            resolve_name_for_type_info(
                bound,
                environment,
                components,
                resolved_map,
                all_crates_and_auto_import,
                wild_card_imports,
                module_element_names,
                errors,
            );
        }
    }
}

fn resolve_name_for_where_clause<'input, 'name_env_alloc>(
    ast: &WhereClause<'input, '_>,
    environment: NameEnvironmentID,
    components: &mut NameEnvironmentSet<'input, 'name_env_alloc>,
    resolved_map: &mut HashMap<EntityID, ResolvedInfo>,
    all_crates_and_auto_import: &std::vec::Vec<String>,
    wild_card_imports: &HashMap<EntityID, std::vec::Vec<String>>,
    module_element_names: &mut HashMap<String, Range<usize>>,
    errors: &mut std::vec::Vec<NameResolveError>,
) {
    for element in ast.elements.iter() {
        resolve_name_for_type_info(
            &element.target_type,
            environment,
            components,
            resolved_map,
            all_crates_and_auto_import,
            wild_card_imports,
            module_element_names,
            errors,
        );

        for bound in element.bounds.iter() {
            resolve_name_for_type_info(
                bound,
                environment,
                components,
                resolved_map,
                all_crates_and_auto_import,
                wild_card_imports,
                module_element_names,
                errors,
            );
        }
    }
}

fn resolve_name_for_type_info<'input, 'name_env_alloc>(
    ast: &TypeInfo<'input, '_>,
    environment: NameEnvironmentID,
    components: &mut NameEnvironmentSet<'input, 'name_env_alloc>,
    resolved_map: &mut HashMap<EntityID, ResolvedInfo>,
    all_crates_and_auto_import: &std::vec::Vec<String>,
    wild_card_imports: &HashMap<EntityID, std::vec::Vec<String>>,
    module_element_names: &mut HashMap<String, Range<usize>>,
    errors: &mut std::vec::Vec<NameResolveError>,
) {
    match &ast.base {
        TypeInfoBase::Array(array_type_info) => {
            resolve_name_for_array_type_info(
                array_type_info,
                environment,
                components,
                resolved_map,
                all_crates_and_auto_import,
                wild_card_imports,
                module_element_names,
                errors,
            );
        }
        TypeInfoBase::Base(base_type_info) => {
            resolve_name_for_base_type_info(
                base_type_info,
                environment,
                components,
                resolved_map,
                all_crates_and_auto_import,
                wild_card_imports,
                module_element_names,
                errors,
            );
        }
        TypeInfoBase::Tuple(tuple_type_info) => {
            resolve_name_for_tuple_type_info(
                tuple_type_info,
                environment,
                components,
                resolved_map,
                all_crates_and_auto_import,
                wild_card_imports,
                module_element_names,
                errors,
            );
        }
        TypeInfoBase::This(literal) => {
            components.resolve(
                environment,
                literal.value,
                literal.into(),
                literal.span.clone(),
                all_crates_and_auto_import,
                resolved_map,
                errors,
            );
        }
    }
}

fn resolve_name_for_base_type_info<'input, 'name_env_alloc>(
    ast: &BaseTypeInfo<'input, '_>,
    environment: NameEnvironmentID,
    components: &mut NameEnvironmentSet<'input, 'name_env_alloc>,
    resolved_map: &mut HashMap<EntityID, ResolvedInfo>,
    all_crates_and_auto_import: &std::vec::Vec<String>,
    wild_card_imports: &HashMap<EntityID, std::vec::Vec<String>>,
    module_element_names: &mut HashMap<String, Range<usize>>,
    errors: &mut std::vec::Vec<NameResolveError>,
) {
    if let Some(first) = ast.path.first() {
        if let "int" | "int8" | "int16" | "int32" | "int64" | "uint" | "uint8" | "uint16"
        | "uint32" | "uint64" | "float" | "float32" | "float64" | "bool" | "unit" = first.value
        {
        } else {
            components.resolve(
                environment,
                first.value,
                first.into(),
                first.span.clone(),
                all_crates_and_auto_import,
                resolved_map,
                errors,
            );
        }
    }

    if let Some(generics) = &ast.generics {
        resolve_name_for_generics_info(
            generics,
            environment,
            components,
            resolved_map,
            all_crates_and_auto_import,
            wild_card_imports,
            module_element_names,
            errors,
        );
    }
}

fn resolve_name_for_array_type_info<'input, 'name_env_alloc>(
    ast: &ArrayTypeInfo<'input, '_>,
    environment: NameEnvironmentID,
    components: &mut NameEnvironmentSet<'input, 'name_env_alloc>,
    resolved_map: &mut HashMap<EntityID, ResolvedInfo>,
    all_crates_and_auto_import: &std::vec::Vec<String>,
    wild_card_imports: &HashMap<EntityID, std::vec::Vec<String>>,
    module_element_names: &mut HashMap<String, Range<usize>>,
    errors: &mut std::vec::Vec<NameResolveError>,
) {
    if let Ok(base_type) = &ast.base_type {
        resolve_name_for_type_info(
            base_type,
            environment,
            components,
            resolved_map,
            all_crates_and_auto_import,
            wild_card_imports,
            module_element_names,
            errors,
        );
    }
}

fn resolve_name_for_tuple_type_info<'input, 'name_env_alloc>(
    ast: &TupleTypeInfo<'input, '_>,
    environment: NameEnvironmentID,
    components: &mut NameEnvironmentSet<'input, 'name_env_alloc>,
    resolved_map: &mut HashMap<EntityID, ResolvedInfo>,
    all_crates_and_auto_import: &std::vec::Vec<String>,
    wild_card_imports: &HashMap<EntityID, std::vec::Vec<String>>,
    module_element_names: &mut HashMap<String, Range<usize>>,
    errors: &mut std::vec::Vec<NameResolveError>,
) {
    for element in ast.types.iter() {
        resolve_name_for_type_info(
            element,
            environment,
            components,
            resolved_map,
            all_crates_and_auto_import,
            wild_card_imports,
            module_element_names,
            errors,
        );
    }
}

fn resolve_name_for_generics_info<'input, 'name_env_alloc>(
    ast: &GenericsInfo<'input, '_>,
    environment: NameEnvironmentID,
    components: &mut NameEnvironmentSet<'input, 'name_env_alloc>,
    resolved_map: &mut HashMap<EntityID, ResolvedInfo>,
    all_crates_and_auto_import: &std::vec::Vec<String>,
    wild_card_imports: &HashMap<EntityID, std::vec::Vec<String>>,
    module_element_names: &mut HashMap<String, Range<usize>>,
    errors: &mut std::vec::Vec<NameResolveError>,
) {
    for element in ast.types.iter() {
        resolve_name_for_type_info(
            element,
            environment,
            components,
            resolved_map,
            all_crates_and_auto_import,
            wild_card_imports,
            module_element_names,
            errors,
        );
    }
}

fn resolve_name_for_expression<'input, 'name_env_alloc>(
    ast: &Expression<'input, '_>,
    environment: NameEnvironmentID,
    components: &mut NameEnvironmentSet<'input, 'name_env_alloc>,
    resolved_map: &mut HashMap<EntityID, ResolvedInfo>,
    all_crates_and_auto_import: &std::vec::Vec<String>,
    wild_card_imports: &HashMap<EntityID, std::vec::Vec<String>>,
    module_element_names: &mut HashMap<String, Range<usize>>,
    errors: &mut std::vec::Vec<NameResolveError>,
) {
    match ast {
        Expression::Return(return_expression) => {
            if let Some(expression) = &return_expression.expression {
                resolve_name_for_expression(
                    expression,
                    environment,
                    components,
                    resolved_map,
                    all_crates_and_auto_import,
                    wild_card_imports,
                    module_element_names,
                    errors,
                );
            }
        }
        Expression::Closure(closure) => {
            let environment = components.new_environment(
                Some(environment),
                EnvironmentSeparatorKind::Closure,
                closure.span.clone(),
            );

            match &closure.arguments {
                ClosureArgumentsOrLiteral::ClosureArguments(closure_arguments) => {
                    for argument in closure_arguments.arguments.iter() {
                        match argument {
                            FunctionArgumentOrVariableBinding::FunctionArgument(
                                function_argument,
                            ) => {
                                resolve_name_for_function_argument(
                                    function_argument,
                                    environment,
                                    components,
                                    resolved_map,
                                    all_crates_and_auto_import,
                                    wild_card_imports,
                                    module_element_names,
                                    errors,
                                );
                            }
                            FunctionArgumentOrVariableBinding::VariableBinding(
                                variable_binding,
                            ) => {
                                resolve_name_for_variable_binding(
                                    variable_binding,
                                    environment,
                                    components,
                                    module_element_names,
                                    errors,
                                );
                            }
                        }
                    }
                }
                ClosureArgumentsOrLiteral::Literal(literal) => {
                    let define = DefineInfo {
                        entity_id: literal.into(),
                        span: literal.span.clone(),
                        kind: DefineKind::Variable,
                    };
                    components.define(environment, literal.value.into(), define);
                }
            }

            if let Ok(expression_or_block) = &closure.expression_or_block {
                match expression_or_block {
                    ExpressionOrBlock::Expression(expression) => {
                        resolve_name_for_expression(
                            expression,
                            environment,
                            components,
                            resolved_map,
                            all_crates_and_auto_import,
                            wild_card_imports,
                            module_element_names,
                            errors,
                        );
                    }
                    ExpressionOrBlock::Block(block) => {
                        resolve_name_for_program(
                            block.program,
                            environment,
                            components,
                            resolved_map,
                            all_crates_and_auto_import,
                            wild_card_imports,
                            module_element_names,
                            false,
                            errors,
                        );
                    }
                }
            }
        }
        Expression::Or(or_expression) => {
            resolve_name_for_or_expression(
                *or_expression,
                environment,
                components,
                resolved_map,
                all_crates_and_auto_import,
                wild_card_imports,
                module_element_names,
                errors,
            );
        }
    }
}

macro_rules! resolve_name_2op {
    ($name:ident, $ty:ty, $next:ident) => {
        fn $name<'input, 'name_env_alloc>(
            ast: &$ty,
            environment: NameEnvironmentID,
            components: &mut NameEnvironmentSet<'input, 'name_env_alloc>,
            resolved_map: &mut HashMap<EntityID, ResolvedInfo>,
            all_crates_and_auto_import: &std::vec::Vec<String>,
            wild_card_imports: &HashMap<EntityID, std::vec::Vec<String>>,
            module_element_names: &mut HashMap<String, Range<usize>>,
            errors: &mut std::vec::Vec<NameResolveError>,
        ) {
            $next(
                &ast.left,
                environment,
                components,
                resolved_map,
                all_crates_and_auto_import,
                wild_card_imports,
                module_element_names,
                errors,
            );

            for chain in ast.chain.iter() {
                $next(
                    chain,
                    environment,
                    components,
                    resolved_map,
                    all_crates_and_auto_import,
                    wild_card_imports,
                    module_element_names,
                    errors,
                );
            }
        }
    };
    (op, $name:ident, $ty:ty, $next:ident) => {
        fn $name<'input, 'name_env_alloc>(
            ast: &$ty,
            environment: NameEnvironmentID,
            components: &mut NameEnvironmentSet<'input, 'name_env_alloc>,
            resolved_map: &mut HashMap<EntityID, ResolvedInfo>,
            all_crates_and_auto_import: &std::vec::Vec<String>,
            wild_card_imports: &HashMap<EntityID, std::vec::Vec<String>>,
            module_element_names: &mut HashMap<String, Range<usize>>,
            errors: &mut std::vec::Vec<NameResolveError>,
        ) {
            $next(
                &ast.left,
                environment,
                components,
                resolved_map,
                all_crates_and_auto_import,
                wild_card_imports,
                module_element_names,
                errors,
            );

            for chain in ast.chain.iter() {
                $next(
                    &chain.1,
                    environment,
                    components,
                    resolved_map,
                    all_crates_and_auto_import,
                    wild_card_imports,
                    module_element_names,
                    errors,
                );
            }
        }
    };
}

resolve_name_2op!(
    resolve_name_for_or_expression,
    OrExpression<'input, '_>,
    resolve_name_for_and_expression
);
resolve_name_2op!(
    resolve_name_for_and_expression,
    AndExpression<'input, '_>,
    resolve_name_for_equals_expression
);
resolve_name_2op!(
    op,
    resolve_name_for_equals_expression,
    EqualsExpression<'input, '_>,
    resolve_name_for_less_or_greater_expression
);
resolve_name_2op!(
    op,
    resolve_name_for_less_or_greater_expression,
    LessOrGreaterExpression<'input, '_>,
    resolve_name_for_add_or_sub_expression
);
resolve_name_2op!(
    op,
    resolve_name_for_add_or_sub_expression,
    AddOrSubExpression<'input, '_>,
    resolve_name_for_mul_or_div_expression
);
resolve_name_2op!(
    op,
    resolve_name_for_mul_or_div_expression,
    MulOrDivExpression<'input, '_>,
    resolve_name_for_factor
);

fn resolve_name_for_factor<'input, 'name_env_alloc>(
    ast: &Factor<'input, '_>,
    environment: NameEnvironmentID,
    components: &mut NameEnvironmentSet<'input, 'name_env_alloc>,
    resolved_map: &mut HashMap<EntityID, ResolvedInfo>,
    all_crates_and_auto_import: &std::vec::Vec<String>,
    wild_card_imports: &HashMap<EntityID, std::vec::Vec<String>>,
    module_element_names: &mut HashMap<String, Range<usize>>,
    errors: &mut std::vec::Vec<NameResolveError>,
) {
    if let Ok(primary) = &ast.primary {
        resolve_name_for_primary(
            primary,
            environment,
            components,
            resolved_map,
            all_crates_and_auto_import,
            wild_card_imports,
            module_element_names,
            errors,
        );
    }
}

fn resolve_name_for_primary<'input, 'name_env_alloc>(
    ast: &Primary<'input, '_>,
    environment: NameEnvironmentID,
    components: &mut NameEnvironmentSet<'input, 'name_env_alloc>,
    resolved_map: &mut HashMap<EntityID, ResolvedInfo>,
    all_crates_and_auto_import: &std::vec::Vec<String>,
    wild_card_imports: &HashMap<EntityID, std::vec::Vec<String>>,
    module_element_names: &mut HashMap<String, Range<usize>>,
    errors: &mut std::vec::Vec<NameResolveError>,
) {
    resolve_name_for_primary_left(
        &ast.left,
        environment,
        components,
        resolved_map,
        all_crates_and_auto_import,
        wild_card_imports,
        module_element_names,
        errors,
    );

    for chain in ast.chain.iter() {
        resolve_name_for_primary_right(
            chain,
            environment,
            components,
            resolved_map,
            all_crates_and_auto_import,
            wild_card_imports,
            module_element_names,
            errors,
        );
    }
}

fn resolve_name_for_primary_left<'input, 'name_env_alloc>(
    ast: &PrimaryLeft<'input, '_>,
    environment: NameEnvironmentID,
    components: &mut NameEnvironmentSet<'input, 'name_env_alloc>,
    resolved_map: &mut HashMap<EntityID, ResolvedInfo>,
    all_crates_and_auto_import: &std::vec::Vec<String>,
    wild_card_imports: &HashMap<EntityID, std::vec::Vec<String>>,
    module_element_names: &mut HashMap<String, Range<usize>>,
    errors: &mut std::vec::Vec<NameResolveError>,
) {
    match &ast.first {
        PrimaryLeftExpr::Simple {
            left,
            generics,
            function_call,
            span: _,
        } => {
            match left {
                SimplePrimary::Tuple {
                    expressions,
                    span: _,
                } => {
                    for expression in expressions.iter() {
                        resolve_name_for_expression(
                            expression,
                            environment,
                            components,
                            resolved_map,
                            all_crates_and_auto_import,
                            wild_card_imports,
                            module_element_names,
                            errors,
                        );
                    }
                }
                SimplePrimary::Literal(literal) => {
                    components.resolve(
                        environment,
                        literal.value,
                        literal.into(),
                        literal.span.clone(),
                        all_crates_and_auto_import,
                        resolved_map,
                        errors,
                    );
                }
                SimplePrimary::StringLiteral(_) => {}
                SimplePrimary::NumericLiteral(_) => {}
                SimplePrimary::Null(_) => {}
                SimplePrimary::True(_) => {}
                SimplePrimary::False(_) => {}
                SimplePrimary::This(span) => {}
                SimplePrimary::LargeThis(span) => {}
            }

            if let Some(generics) = &generics {
                resolve_name_for_generics_info(
                    generics,
                    environment,
                    components,
                    resolved_map,
                    all_crates_and_auto_import,
                    wild_card_imports,
                    module_element_names,
                    errors,
                );
            }

            if let Some(function_call) = function_call {
                for expression in function_call.arguments.iter() {
                    resolve_name_for_expression(
                        expression,
                        environment,
                        components,
                        resolved_map,
                        all_crates_and_auto_import,
                        wild_card_imports,
                        module_element_names,
                        errors,
                    );
                }
            }
        }
        PrimaryLeftExpr::NewObject { new_object } => {
            if let Some(first) = new_object.path.first() {
                components.resolve(
                    environment,
                    first.value,
                    first.into(),
                    first.span.clone(),
                    all_crates_and_auto_import,
                    resolved_map,
                    errors,
                );
            }

            for field_assign in new_object.field_assign.elements.iter() {
                resolve_name_for_expression(
                    &field_assign.expression,
                    environment,
                    components,
                    resolved_map,
                    all_crates_and_auto_import,
                    wild_card_imports,
                    module_element_names,
                    errors,
                );
            }
        }
        PrimaryLeftExpr::NewArray { new_array } => {
            for expression in new_array.elements.iter() {
                resolve_name_for_expression(
                    expression,
                    environment,
                    components,
                    resolved_map,
                    all_crates_and_auto_import,
                    wild_card_imports,
                    module_element_names,
                    errors,
                );
            }
        }
        PrimaryLeftExpr::NewArrayInit { new_array_init } => {
            if let Ok(expression) = &new_array_init.init_expression {
                resolve_name_for_expression(
                    expression,
                    environment,
                    components,
                    resolved_map,
                    all_crates_and_auto_import,
                    wild_card_imports,
                    module_element_names,
                    errors,
                );
            }

            if let Ok(expression) = &new_array_init.length_expression {
                resolve_name_for_expression(
                    expression,
                    environment,
                    components,
                    resolved_map,
                    all_crates_and_auto_import,
                    wild_card_imports,
                    module_element_names,
                    errors,
                );
            }
        }
        PrimaryLeftExpr::If { if_expression } => {
            if let Ok(expression) = &if_expression.first.condition {
                resolve_name_for_expression(
                    expression,
                    environment,
                    components,
                    resolved_map,
                    all_crates_and_auto_import,
                    wild_card_imports,
                    module_element_names,
                    errors,
                );
            }
            if let Ok(block) = &if_expression.first.block {
                resolve_name_for_program(
                    block.program,
                    environment,
                    components,
                    resolved_map,
                    all_crates_and_auto_import,
                    wild_card_imports,
                    module_element_names,
                    false,
                    errors,
                );
            }

            for chain in if_expression.chain.iter() {
                match chain {
                    ElseChain::ElseIf { if_statement } => {
                        if let Ok(expression) = &if_statement.condition {
                            resolve_name_for_expression(
                                expression,
                                environment,
                                components,
                                resolved_map,
                                all_crates_and_auto_import,
                                wild_card_imports,
                                module_element_names,
                                errors,
                            );
                        }
                        if let Ok(block) = &if_statement.block {
                            resolve_name_for_program(
                                block.program,
                                environment,
                                components,
                                resolved_map,
                                all_crates_and_auto_import,
                                wild_card_imports,
                                module_element_names,
                                false,
                                errors,
                            );
                        }
                    }
                    ElseChain::Else { block } => {
                        resolve_name_for_program(
                            block.program,
                            environment,
                            components,
                            resolved_map,
                            all_crates_and_auto_import,
                            wild_card_imports,
                            module_element_names,
                            false,
                            errors,
                        );
                    }
                }
            }
        }
        PrimaryLeftExpr::Loop { loop_expression } => {
            if let Ok(block) = &loop_expression.block {
                resolve_name_for_program(
                    block.program,
                    environment,
                    components,
                    resolved_map,
                    all_crates_and_auto_import,
                    wild_card_imports,
                    module_element_names,
                    false,
                    errors,
                );
            }
        }
    }
}

fn resolve_name_for_mapping_operator<'input, 'name_env_alloc>(
    ast: &MappingOperator<'input, '_>,
    environment: NameEnvironmentID,
    components: &mut NameEnvironmentSet<'input, 'name_env_alloc>,
    resolved_map: &mut HashMap<EntityID, ResolvedInfo>,
    all_crates_and_auto_import: &std::vec::Vec<String>,
    wild_card_imports: &HashMap<EntityID, std::vec::Vec<String>>,
    module_element_names: &mut HashMap<String, Range<usize>>,
    errors: &mut std::vec::Vec<NameResolveError>,
) {
    let block = match ast {
        MappingOperator::NullElvis { block, span: _ } => block,
        MappingOperator::ResultElvis { block, span: _ } => block,
        _ => return,
    };

    if let Ok(block) = block {
        resolve_name_for_program(
            block.program,
            environment,
            components,
            resolved_map,
            all_crates_and_auto_import,
            wild_card_imports,
            module_element_names,
            false,
            errors,
        );
    }
}

fn resolve_name_for_primary_right<'input, 'name_env_alloc>(
    ast: &PrimaryRight<'input, '_>,
    environment: NameEnvironmentID,
    components: &mut NameEnvironmentSet<'input, 'name_env_alloc>,
    resolved_map: &mut HashMap<EntityID, ResolvedInfo>,
    all_crates_and_auto_import: &std::vec::Vec<String>,
    wild_card_imports: &HashMap<EntityID, std::vec::Vec<String>>,
    module_element_names: &mut HashMap<String, Range<usize>>,
    errors: &mut std::vec::Vec<NameResolveError>,
) {
    if let Some(second) = &ast.second {
        if let Some(generics) = &second.generics {
            resolve_name_for_generics_info(
                generics,
                environment,
                components,
                resolved_map,
                all_crates_and_auto_import,
                wild_card_imports,
                module_element_names,
                errors,
            );
        }
        if let Some(function_call) = &second.function_call {
            for expression in function_call.arguments.iter() {
                resolve_name_for_expression(
                    expression,
                    environment,
                    components,
                    resolved_map,
                    all_crates_and_auto_import,
                    wild_card_imports,
                    module_element_names,
                    errors,
                );
            }
        }
    }
    if let Some(mapping_operator) = &ast.mapping_operator {
        resolve_name_for_mapping_operator(
            mapping_operator,
            environment,
            components,
            resolved_map,
            all_crates_and_auto_import,
            wild_card_imports,
            module_element_names,
            errors,
        );
    }
}
