use catla_parser::ast::{
    AddOrSub, AddOrSubExpression, AndExpression, Define, ElementsOrWildCard, EntityID,
    EqualsExpression, Expression, Factor, FunctionDefine, GenericsDefine, ImportStatement, LetVar,
    LessOrGreaterExpression, MulOrDivExpression, OrExpression, Primary, PrimaryLeftExpr,
    PrimarySeparator, Program, SimplePrimary, Spanned, Statement, StatementAttribute,
    StatementWithTagAndDocs, TypeAttribute, TypeInfo, TypeInfoBase, UserTypeDefine, UserTypeKind,
    VariableBinding, VariableDefine, WhereClause,
};
use catla_name_resolver::{DefineKind, ResolvedInfo};
use catla_optimization::lifetime::{AllocationKind, LifetimeAnalyzeResults};
use catla_type::types::{GlobalUserTypeSet, Type};
use hashbrown::{HashMap, HashSet};

use crate::CodeBuilderScope;

pub(crate) fn codegen_for_program(
    ast: &Program,
    in_impl_scope: bool,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    user_type_set: &GlobalUserTypeSet,
    module_static_variables: &HashMap<String, bool>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
    stack_slot_counter: &mut usize,
) {
    for statement in ast.statements.iter() {
        match &statement.statement {
            Statement::Assignment(assignment) => {
                let Ok(right) = &assignment.right else {
                    continue;
                };

                let mut stack_slots = HashMap::new();
                let stack_entities =
                    collect_stack_alloc_new_object_entities(right, lifetime_analyze_results);
                for entity in stack_entities.into_iter() {
                    let slot_name = format!("__catla_stack_slot_{}", *stack_slot_counter);
                    *stack_slot_counter += 1;

                    builder.push_line(
                        format!(
                            "let mut {} = std::mem::ManuallyDrop::new(std::mem::MaybeUninit::uninit());",
                            slot_name
                        )
                        .as_str(),
                    );

                    stack_slots.insert(entity, slot_name);
                }

                let left_code = codegen_assignment_left_expression(
                    &assignment.left,
                    type_infer_results,
                    user_type_set,
                    module_static_variables,
                    current_crate_name,
                )
                .unwrap_or_else(|| {
                    codegen_expression(
                        &assignment.left,
                        type_infer_results,
                        lifetime_analyze_results,
                        user_type_set,
                        module_static_variables,
                        name_resolved_map,
                        module_entity_type_map,
                        builder,
                        current_crate_name,
                        &HashMap::new(),
                        false,
                        ClassValueUsage::Normal,
                    )
                });
                let right_code = codegen_expression(
                    right,
                    type_infer_results,
                    lifetime_analyze_results,
                    user_type_set,
                    module_static_variables,
                    name_resolved_map,
                    module_entity_type_map,
                    builder,
                    current_crate_name,
                    &stack_slots,
                    false,
                    ClassValueUsage::Normal,
                );

                builder.push_line(format!("{} = {};", left_code, right_code).as_str());
            }
            Statement::Swap(swap_statement) => todo!(),
            Statement::Import(import_statement) => {
                builder.push_line(codegen_import(import_statement, current_crate_name).as_str());
            }
            Statement::DefineWithAttribute(define_with_attribute) => {
                if let Ok(define) = &define_with_attribute.define {
                    match define {
                        Define::Function(function_define) => {
                            codegen_function_define(
                                statement,
                                function_define,
                                !in_impl_scope,
                                false,
                                type_infer_results,
                                lifetime_analyze_results,
                                user_type_set,
                                module_static_variables,
                                name_resolved_map,
                                module_entity_type_map,
                                builder,
                                current_crate_name,
                                stack_slot_counter,
                            );
                        }
                        Define::UserType(user_type_define) => {
                            codegen_user_type_define(
                                in_impl_scope,
                                user_type_define,
                                type_infer_results,
                                lifetime_analyze_results,
                                user_type_set,
                                module_static_variables,
                                name_resolved_map,
                                module_entity_type_map,
                                builder,
                                current_crate_name,
                                stack_slot_counter,
                            );
                        }
                        Define::Variable(variable_define) => {
                            let is_static = define_with_attribute
                                .attribute
                                .iter()
                                .any(|attribute| attribute.value == StatementAttribute::Static);

                            codegen_variable_define(
                                variable_define,
                                is_static,
                                type_infer_results,
                                lifetime_analyze_results,
                                user_type_set,
                                module_static_variables,
                                name_resolved_map,
                                module_entity_type_map,
                                builder,
                                current_crate_name,
                                stack_slot_counter,
                            );
                        }
                        Define::TypeAlias(type_alias) => todo!(),
                    }
                }
            }
            Statement::Drop(drop_statement) => todo!(),
            Statement::Expression(expression) => {
                builder.push_line(
                    format!(
                        "{};",
                        codegen_expression(
                            expression,
                            &type_infer_results,
                            lifetime_analyze_results,
                            user_type_set,
                            module_static_variables,
                            name_resolved_map,
                            module_entity_type_map,
                            builder,
                            current_crate_name,
                            &HashMap::new(),
                            false,
                            ClassValueUsage::Normal,
                        )
                        .as_str()
                    )
                    .as_str(),
                );
            }
            Statement::Implements(implements) => {
                builder.push_line(
                    format!(
                        "impl{} {} for {} {} {{",
                        implements
                            .generics
                            .as_ref()
                            .map(|generics| {
                                codegen_for_generics_define(generics, current_crate_name)
                            })
                            .unwrap_or_default(),
                        implements
                            .interface
                            .as_ref()
                            .map(|ty| {
                                codegen_for_type(
                                    ty,
                                    type_infer_results,
                                    user_type_set,
                                    current_crate_name,
                                )
                            })
                            .unwrap_or_default(),
                        implements
                            .concrete
                            .as_ref()
                            .map(|ty| {
                                codegen_for_type(
                                    ty,
                                    type_infer_results,
                                    user_type_set,
                                    current_crate_name,
                                )
                            })
                            .unwrap_or_default(),
                        implements
                            .where_clause
                            .as_ref()
                            .map(|where_clause| codegen_for_where_clause(
                                where_clause,
                                type_infer_results,
                                user_type_set,
                                current_crate_name
                            ))
                            .unwrap_or_default(),
                    )
                    .as_str(),
                );

                {
                    let scope = builder.scope();

                    if let Ok(block) = &implements.block {
                        codegen_for_program(
                            block.program,
                            true,
                            type_infer_results,
                            lifetime_analyze_results,
                            user_type_set,
                            module_static_variables,
                            name_resolved_map,
                            module_entity_type_map,
                            &scope,
                            current_crate_name,
                            stack_slot_counter,
                        );
                    }
                }

                builder.push_line("}");
            }
        }
    }
}

pub(crate) fn collect_module_static_variables(ast: &Program) -> HashMap<String, bool> {
    let mut static_variables = HashMap::new();

    for statement in ast.statements.iter() {
        let Statement::DefineWithAttribute(define_with_attribute) = &statement.statement else {
            continue;
        };

        let is_static = define_with_attribute
            .attribute
            .iter()
            .any(|attribute| attribute.value == StatementAttribute::Static);
        if !is_static {
            continue;
        }

        let Ok(Define::Variable(variable_define)) = &define_with_attribute.define else {
            continue;
        };
        let Ok(VariableBinding::Literal(literal)) = &variable_define.binding else {
            continue;
        };

        static_variables.insert(literal.value.to_string(), variable_define.let_var.value == LetVar::Var);
    }

    static_variables
}

fn resolve_variable_type_code(
    variable_define: &VariableDefine,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    user_type_set: &GlobalUserTypeSet,
    current_crate_name: &str,
) -> Option<String> {
    if let Some(type_tag) = variable_define.type_tag.as_ref() {
        if let Ok(ty) = type_tag.type_info.as_ref() {
            return Some(codegen_for_type(
                ty,
                type_infer_results,
                user_type_set,
                current_crate_name,
            ));
        }
    }

    type_infer_results
        .get(&EntityID::from(variable_define))
        .or_else(|| {
            variable_define
                .binding
                .as_ref()
                .ok()
                .and_then(|binding| match binding {
                    VariableBinding::Literal(literal) => type_infer_results.get(&EntityID::from(literal)),
                    VariableBinding::Binding { bindings: _, span: _ } => None,
                })
        })
        .map(|ty| codegen_for_inferred_type(&ty.value, user_type_set, current_crate_name))
}

fn codegen_variable_define(
    variable_define: &VariableDefine,
    is_static: bool,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    user_type_set: &GlobalUserTypeSet,
    module_static_variables: &HashMap<String, bool>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
    stack_slot_counter: &mut usize,
) {
    let Ok(binding) = variable_define.binding.as_ref() else {
        return;
    };
    let binding_code = codegen_for_variable_binding(binding);

    if is_static {
        let type_code = resolve_variable_type_code(
            variable_define,
            type_infer_results,
            user_type_set,
            current_crate_name,
        )
        .unwrap_or_else(|| "()".to_string());

        let expression_code = variable_define
            .expression
            .as_ref()
            .map(|expression| {
                codegen_expression(
                    expression,
                    type_infer_results,
                    lifetime_analyze_results,
                    user_type_set,
                    module_static_variables,
                    name_resolved_map,
                    module_entity_type_map,
                    builder,
                    current_crate_name,
                    &HashMap::new(),
                    true,
                    ClassValueUsage::Normal,
                )
            })
            .unwrap_or_else(|| "todo!()".to_string());

        if variable_define.let_var.value == LetVar::Var {
            builder.push_line(
                format!(
                    "static {}: std::sync::LazyLock<std::sync::RwLock<{}>> = std::sync::LazyLock::new(|| std::sync::RwLock::new({}));",
                    binding_code, type_code, expression_code
                )
                .as_str(),
            );
        } else {
            builder.push_line(
                format!(
                    "static {}: std::sync::LazyLock<{}> = std::sync::LazyLock::new(|| {});",
                    binding_code, type_code, expression_code
                )
                .as_str(),
            );
        }

        return;
    }

    let mut stack_slots = HashMap::new();
    if let Some(expression) = variable_define.expression.as_ref() {
        let stack_entities = collect_stack_alloc_new_object_entities(expression, lifetime_analyze_results);
        for entity in stack_entities.into_iter() {
            let slot_name = format!("__catla_stack_slot_{}", *stack_slot_counter);
            *stack_slot_counter += 1;

            builder.push_line(
                format!(
                    "let mut {} = std::mem::ManuallyDrop::new(std::mem::MaybeUninit::uninit());",
                    slot_name
                )
                .as_str(),
            );

            stack_slots.insert(entity, slot_name);
        }
    }

    let type_code = variable_define
        .type_tag
        .as_ref()
        .map(|type_tag| type_tag.type_info.as_ref().ok())
        .flatten()
        .map(|ty| {
            format!(
                ": {}",
                codegen_for_type(ty, type_infer_results, user_type_set, current_crate_name)
            )
        })
        .unwrap_or_default();
    let expression_code = variable_define
        .expression
        .as_ref()
        .map(|expression| {
            format!(
                " = {}",
                codegen_expression(
                    expression,
                    type_infer_results,
                    lifetime_analyze_results,
                    user_type_set,
                    module_static_variables,
                    name_resolved_map,
                    module_entity_type_map,
                    builder,
                    current_crate_name,
                    &stack_slots,
                    false,
                    ClassValueUsage::Normal,
                )
            )
        })
        .unwrap_or_default();

    let let_keyword = if variable_define.let_var.value == LetVar::Var {
        "let mut"
    } else {
        "let"
    };
    builder.push_line(
        format!(
            "{} {}{}{};",
            let_keyword, binding_code, type_code, expression_code
        )
        .as_str(),
    );
}

fn codegen_for_interface_program(
    ast: &Program,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    user_type_set: &GlobalUserTypeSet,
    module_static_variables: &HashMap<String, bool>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
    stack_slot_counter: &mut usize,
) {
    for statement in ast.statements.iter() {
        if let Statement::DefineWithAttribute(define_with_attribute) = &statement.statement {
            if let Ok(Define::Function(function_define)) = &define_with_attribute.define {
                codegen_function_define(
                    statement,
                    function_define,
                    false,
                    true,
                    type_infer_results,
                    lifetime_analyze_results,
                    user_type_set,
                    module_static_variables,
                    name_resolved_map,
                    module_entity_type_map,
                    builder,
                    current_crate_name,
                    stack_slot_counter,
                );
            }
        }
    }
}

fn codegen_user_type_define(
    in_impl_scope: bool,
    user_type_define: &UserTypeDefine,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    user_type_set: &GlobalUserTypeSet,
    module_static_variables: &HashMap<String, bool>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
    stack_slot_counter: &mut usize,
) {
    let visibility = if in_impl_scope { "" } else { "pub " };
    let name = user_type_define
        .name
        .as_ref()
        .map(|name| name.value)
        .unwrap_or_default();
    let generics = user_type_define
        .generics
        .as_ref()
        .map(|generics| codegen_for_generics_define(generics, current_crate_name))
        .unwrap_or_default();
    let generics_arguments = user_type_define
        .generics
        .as_ref()
        .map(codegen_for_generics_arguments)
        .unwrap_or_default();
    let super_types = user_type_define
        .super_type
        .as_ref()
        .map(|super_types| {
            if super_types.types.is_empty() {
                String::new()
            } else {
                format!(
                    ": {}",
                    super_types
                        .types
                        .iter()
                        .map(|ty| {
                            codegen_for_type(
                                ty,
                                type_infer_results,
                                user_type_set,
                                current_crate_name,
                            )
                        })
                        .collect::<Vec<_>>()
                        .join(" + ")
                )
            }
        })
        .unwrap_or_default();
    let where_clause = user_type_define
        .where_clause
        .as_ref()
        .map(|where_clause| {
            format!(
                " {}",
                codegen_for_where_clause(
                    where_clause,
                    type_infer_results,
                    user_type_set,
                    current_crate_name,
                )
            )
        })
        .unwrap_or_default();

    match user_type_define.kind.value {
        UserTypeKind::Interface => {
            builder.push_line(
                format!(
                    "{}trait {}{}{}{} {{",
                    visibility, name, generics, super_types, where_clause
                )
                .as_str(),
            );

            {
                let scope = builder.scope();

                if let Ok(block) = &user_type_define.block {
                    codegen_for_interface_program(
                        block.program,
                        type_infer_results,
                        lifetime_analyze_results,
                        user_type_set,
                        module_static_variables,
                        name_resolved_map,
                        module_entity_type_map,
                        &scope,
                        current_crate_name,
                        stack_slot_counter,
                    );
                }
            }

            builder.push_line("}");
        }
        UserTypeKind::Class | UserTypeKind::Struct => {
            if user_type_define.kind.value == UserTypeKind::Struct {
                builder.push_line("#[derive(Debug, Clone, Copy)]");
            } else {
                builder.push_line("#[derive(Debug)]");
            }

            builder.push_line(
                format!("{}struct {}{}{} {{", visibility, name, generics, where_clause).as_str(),
            );
            {
                let scope = builder.scope();
                for (field_name, field_type) in collect_user_type_fields(
                    user_type_define,
                    type_infer_results,
                    user_type_set,
                    current_crate_name,
                )
                .into_iter()
                {
                    scope.push_line(format!("pub {}: {},", field_name, field_type).as_str());
                }
            }
            builder.push_line("}");

            builder.push_line(
                format!(
                    "impl{} catla_std::drop::CatlaDrop for {}{}{} {{",
                    generics, name, generics_arguments, where_clause
                )
                .as_str(),
            );
            {
                let scope = builder.scope();
                scope.push_line("fn drop(&self) {}");
            }
            builder.push_line("}");

            builder.push_line(
                format!(
                    "impl{} {}{}{} {{",
                    generics, name, generics_arguments, where_clause
                )
                .as_str(),
            );
            {
                let scope = builder.scope();
                if let Ok(block) = &user_type_define.block {
                    codegen_for_user_type_impl_program(
                        block.program,
                        type_infer_results,
                        lifetime_analyze_results,
                        user_type_set,
                        module_static_variables,
                        name_resolved_map,
                        module_entity_type_map,
                        &scope,
                        current_crate_name,
                        stack_slot_counter,
                    );
                }
            }
            builder.push_line("}");
        }
    }
}

fn codegen_for_user_type_impl_program(
    ast: &Program,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    user_type_set: &GlobalUserTypeSet,
    module_static_variables: &HashMap<String, bool>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
    stack_slot_counter: &mut usize,
) {
    for statement in ast.statements.iter() {
        if let Statement::DefineWithAttribute(define_with_attribute) = &statement.statement {
            if let Ok(Define::Function(function_define)) = &define_with_attribute.define {
                codegen_function_define(
                    statement,
                    function_define,
                    false,
                    false,
                    type_infer_results,
                    lifetime_analyze_results,
                    user_type_set,
                    module_static_variables,
                    name_resolved_map,
                    module_entity_type_map,
                    builder,
                    current_crate_name,
                    stack_slot_counter,
                );
            }
        }
    }
}

fn collect_user_type_fields(
    user_type_define: &UserTypeDefine,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    user_type_set: &GlobalUserTypeSet,
    current_crate_name: &str,
) -> Vec<(String, String)> {
    let mut fields = Vec::new();

    let Ok(block) = &user_type_define.block else {
        return fields;
    };

    for statement in block.program.statements.iter() {
        let Statement::DefineWithAttribute(define_with_attribute) = &statement.statement else {
            continue;
        };
        let Ok(Define::Variable(variable_define)) = &define_with_attribute.define else {
            continue;
        };

        let field_name = match variable_define.binding.as_ref() {
            Ok(VariableBinding::Literal(literal)) => literal.value.to_string(),
            _ => continue,
        };

        let field_inferred_type = type_infer_results
            .get(&EntityID::from(variable_define))
            .or_else(|| {
                variable_define
                    .binding
                    .as_ref()
                    .ok()
                    .and_then(|binding| match binding {
                        VariableBinding::Literal(literal) => {
                            type_infer_results.get(&EntityID::from(literal))
                        }
                        VariableBinding::Binding { bindings: _, span: _ } => None,
                    })
            });

        let field_type = field_inferred_type
            .map(|ty| codegen_for_inferred_type(&ty.value, user_type_set, current_crate_name))
            .or_else(|| {
                variable_define
                    .type_tag
                    .as_ref()
                    .and_then(|type_tag| type_tag.type_info.as_ref().ok())
                    .map(|type_info| {
                        codegen_for_type(
                            type_info,
                            type_infer_results,
                            user_type_set,
                            current_crate_name,
                        )
                    })
            })
            .unwrap_or_else(|| "()".to_string());

        fields.push((field_name, field_type));
    }

    fields
}

fn codegen_function_define(
    statement: &StatementWithTagAndDocs,
    function_define: &FunctionDefine,
    public: bool,
    in_trait_scope: bool,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    user_type_set: &GlobalUserTypeSet,
    module_static_variables: &HashMap<String, bool>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
    stack_slot_counter: &mut usize,
) {
    let visibility = if public { "pub " } else { "" };
    let signature = codegen_for_function_signature(
        function_define,
        type_infer_results,
        user_type_set,
        current_crate_name,
    );
    let has_body = statement
        .compiler_tags
        .iter()
        .any(|tag| tag.literal.value == "compiler_support_unreachable")
        || function_define.block.is_some();

    if in_trait_scope && !has_body {
        builder.push_line(format!("{}{};", visibility, signature).as_str());
        return;
    }

    if !has_body {
        builder.push_line(format!("{}{};", visibility, signature).as_str());
        return;
    }

    builder.push_line(format!("{}{} {{", visibility, signature).as_str());

    {
        let scope = builder.scope();

        if statement
            .compiler_tags
            .iter()
            .any(|tag| tag.literal.value == "compiler_support_unreachable")
        {
            scope.push_line("unreachable!()");
        } else if let Some(block) = &function_define.block {
                codegen_for_program(
                    block.program,
                    false,
                    type_infer_results,
                    lifetime_analyze_results,
                    user_type_set,
                    module_static_variables,
                    name_resolved_map,
                    module_entity_type_map,
                    &scope,
                    current_crate_name,
                    stack_slot_counter,
                );
            }
        }

    builder.push_line("}");
}

fn codegen_for_function_signature(
    ast: &FunctionDefine,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    user_type_set: &GlobalUserTypeSet,
    current_crate_name: &str,
) -> String {
    let name = ast.name.as_ref().map(|name| name.value).unwrap_or_default();
    let generics = ast
        .generics
        .as_ref()
        .map(|generics| codegen_for_generics_define(generics, current_crate_name))
        .unwrap_or_default();
    let arguments = codegen_for_function_arguments(
        ast,
        type_infer_results,
        user_type_set,
        current_crate_name,
    );
    let return_type = ast
        .return_type
        .as_ref()
        .map(|return_type| {
            return_type
                .type_info
                .as_ref()
                .map(|ty| {
                    format!(
                        " -> {}",
                        codegen_for_type(ty, type_infer_results, user_type_set, current_crate_name)
                    )
                })
                .unwrap_or_default()
        })
        .unwrap_or_default();
    let where_clause = ast
        .where_clause
        .as_ref()
        .map(|where_clause| {
            format!(
                " {}",
                codegen_for_where_clause(
                    where_clause,
                    type_infer_results,
                    user_type_set,
                    current_crate_name,
                )
            )
        })
        .unwrap_or_default();

    format!(
        "fn {}{}{}{}{}",
        name, generics, arguments, return_type, where_clause
    )
}

fn codegen_for_function_arguments(
    ast: &FunctionDefine,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    user_type_set: &GlobalUserTypeSet,
    current_crate_name: &str,
) -> String {
    let mut arguments = String::new();
    arguments += "(";

    if let Ok(function_arguments) = &ast.arguments {
        let mut argument_elements = Vec::new();

        if function_arguments.this_mutability.is_some() {
            argument_elements.push("&self".to_string());
        }

        argument_elements.extend(function_arguments.arguments.iter().map(|argument| {
            format!(
                "{}: {}",
                codegen_for_variable_binding(&argument.binding),
                argument
                    .type_tag
                    .type_info
                    .as_ref()
                    .map(|ty| {
                        codegen_for_type(ty, type_infer_results, user_type_set, current_crate_name)
                    })
                    .unwrap_or_default()
            )
        }));

        arguments += argument_elements.join(", ").as_str();
    }

    arguments += ")";
    arguments
}

fn codegen_for_variable_binding(ast: &VariableBinding) -> String {
    match ast {
        VariableBinding::Literal(literal) => literal.value.to_string(),
        VariableBinding::Binding { bindings, span: _ } => {
            let mut code = String::new();
            code += "(";

            code += bindings
                .iter()
                .map(|binding| codegen_for_variable_binding(binding))
                .collect::<Vec<_>>()
                .join(", ")
                .as_str();

            code += ")";

            code
        }
    }
}

fn codegen_for_generics_define(ast: &GenericsDefine, current_crate_name: &str) -> String {
    let mut code = String::new();
    code += "<";

    code += ast
        .elements
        .iter()
        .map(|element| {
            if element.bounds.is_empty() {
                element.name.value.to_string()
            } else {
                format!(
                    "{}: {}",
                    element.name.value,
                    element
                        .bounds
                        .iter()
                        .map(|bound| codegen_for_type_without_class_wrapper(bound, current_crate_name))
                        .collect::<Vec<_>>()
                        .join(" + ")
                )
            }
        })
        .collect::<Vec<_>>()
        .join(", ")
        .as_str();

    code += ">";

    code
}

fn codegen_for_generics_arguments(ast: &GenericsDefine) -> String {
    format!(
        "<{}>",
        ast.elements
            .iter()
            .map(|element| element.name.value)
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn codegen_for_where_clause(
    ast: &WhereClause,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    user_type_set: &GlobalUserTypeSet,
    current_crate_name: &str,
) -> String {
    format!(
        "where {}",
        ast.elements
            .iter()
            .map(|element| {
                format!(
                    "{}: {}",
                    codegen_for_type(
                        &element.target_type,
                        type_infer_results,
                        user_type_set,
                        current_crate_name
                    ),
                    element
                        .bounds
                        .iter()
                        .map(|bound| {
                            codegen_for_type(
                                bound,
                                type_infer_results,
                                user_type_set,
                                current_crate_name,
                            )
                        })
                        .collect::<Vec<_>>()
                        .join(" + ")
                )
            })
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn codegen_for_type(
    ast: &TypeInfo,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    user_type_set: &GlobalUserTypeSet,
    current_crate_name: &str,
) -> String {
    let mut code = codegen_for_type_without_class_wrapper(ast, current_crate_name);

    for attribute in ast.attributes.iter() {
        match attribute {
            TypeAttribute::Optional { span: _ } => todo!(),
            TypeAttribute::Result {
                generics: _,
                span: _,
            } => todo!(),
        }
    }

    let inferred_is_class = type_infer_results
        .get(&EntityID::from(ast))
        .map(|ty| is_class_type(&ty.value, user_type_set))
        .unwrap_or(false);
    let declared_is_class = is_class_type_info(ast, user_type_set);

    if inferred_is_class || declared_is_class {
        code = format!("catla_std::object::CatlaObjectRef<{}>", code);
    }

    code
}

fn codegen_for_type_without_class_wrapper(ast: &TypeInfo, current_crate_name: &str) -> String {
    let mut code = String::new();

    match &ast.base {
        TypeInfoBase::Array(_array_type_info) => todo!(),
        TypeInfoBase::Base(base_type_info) => {
            if base_type_info.path.len() == 1 {
                if let Some(mapped) = map_catla_builtin_type(base_type_info.path[0].value) {
                    code += mapped;
                } else {
                    code += base_type_info.path[0].value;
                }
            } else {
                code += base_type_info
                    .path
                    .iter()
                    .enumerate()
                    .map(|(index, path)| {
                        if index == 0 {
                            map_module_root(path.value, current_crate_name)
                        } else {
                            path.value.to_string()
                        }
                    })
                    .collect::<Vec<_>>()
                    .join("::")
                    .as_str();
            }
            if let Some(generics) = &base_type_info.generics {
                code += "<";
                code += generics
                    .types
                    .iter()
                    .map(|generic| codegen_for_type_without_class_wrapper(generic, current_crate_name))
                    .collect::<Vec<_>>()
                    .join(", ")
                    .as_str();
                code += ">";
            }
        }
        TypeInfoBase::Tuple(_tuple_type_info) => todo!(),
        TypeInfoBase::This(_) => code += "Self",
    }

    code
}

fn codegen_for_inferred_type(
    ty: &Type,
    user_type_set: &GlobalUserTypeSet,
    _current_crate_name: &str,
) -> String {
    match ty {
        Type::Int8 => "i8".to_string(),
        Type::Int16 => "i16".to_string(),
        Type::Int32 => "i32".to_string(),
        Type::Int64 => "i64".to_string(),
        Type::Uint8 => "u8".to_string(),
        Type::Uint16 => "u16".to_string(),
        Type::Uint32 => "u32".to_string(),
        Type::Uint64 => "u64".to_string(),
        Type::Float32 => "f32".to_string(),
        Type::Float64 => "f64".to_string(),
        Type::Bool => "bool".to_string(),
        Type::Unit => "()".to_string(),
        Type::UserType {
            user_type_info,
            generics: _,
        } => {
            let user_type_info = user_type_set.get(*user_type_info);
            let user_type_info = user_type_info.read().unwrap();

            let type_name = user_type_info.name.value.clone();

            if user_type_info
                .kind
                .as_ref()
                .map(|kind| kind.value == UserTypeKind::Class)
                .unwrap_or(false)
            {
                format!("catla_std::object::CatlaObjectRef<{}>", type_name)
            } else {
                type_name
            }
        }
        Type::Array(_) | Type::Tuple(_) => "()".to_string(),
        Type::IntegerLiteral => "i64".to_string(),
        Type::FloatLiteral => "f64".to_string(),
        _ => "()".to_string(),
    }
}

fn is_class_type(ty: &Type, user_type_set: &GlobalUserTypeSet) -> bool {
    let Type::UserType {
        user_type_info,
        generics: _,
    } = ty
    else {
        return false;
    };

    let user_type_info = user_type_set.get(*user_type_info);
    let user_type_info = user_type_info.read().unwrap();
    user_type_info
        .kind
        .as_ref()
        .map(|kind| kind.value == UserTypeKind::Class)
        .unwrap_or(false)
}

fn is_class_type_info(type_info: &TypeInfo, user_type_set: &GlobalUserTypeSet) -> bool {
    let TypeInfoBase::Base(base_type_info) = &type_info.base else {
        return false;
    };

    let Some(type_name) = base_type_info.path.last().map(|path| path.value) else {
        return false;
    };

    user_type_set.all_infos().into_iter().any(|info| {
        let info = info.read().unwrap();
        info.name.value == type_name
            && info
                .kind
                .as_ref()
                .map(|kind| kind.value == UserTypeKind::Class)
                .unwrap_or(false)
    })
}

fn resolve_callee_function_type_for_literal<'a>(
    literal: &Spanned<&str>,
    type_infer_results: &'a HashMap<EntityID, Spanned<Type>>,
    name_resolved_map: &'a HashMap<EntityID, ResolvedInfo>,
    module_entity_type_map: &'a HashMap<EntityID, Type>,
) -> Option<&'a Type> {
    if let Some(ty) = type_infer_results.get(&EntityID::from(literal)) {
        if matches!(ty.value, Type::Function { .. }) {
            return Some(&ty.value);
        }
    }

    let resolved = name_resolved_map.get(&EntityID::from(literal))?;
    if resolved.define.kind != DefineKind::Function {
        return None;
    }

    let function_type = module_entity_type_map.get(&resolved.define.entity_id)?;
    if matches!(function_type, Type::Function { .. }) {
        Some(function_type)
    } else {
        None
    }
}

fn function_argument_usage(
    function_type: Option<&Type>,
    argument_index: usize,
    user_type_set: &GlobalUserTypeSet,
) -> ClassValueUsage {
    let Some(Type::Function { function_info, .. }) = function_type else {
        return ClassValueUsage::Normal;
    };

    function_info
        .arguments
        .get(argument_index)
        .map(|argument| {
            if is_class_type(&argument.value, user_type_set) {
                ClassValueUsage::FunctionArgument
            } else {
                ClassValueUsage::Normal
            }
        })
        .unwrap_or(ClassValueUsage::Normal)
}

fn map_catla_builtin_type(name: &str) -> Option<&'static str> {
    match name {
        "int8" => Some("i8"),
        "int16" => Some("i16"),
        "int32" => Some("i32"),
        "int64" => Some("i64"),
        "uint8" => Some("u8"),
        "uint16" => Some("u16"),
        "uint32" => Some("u32"),
        "uint64" => Some("u64"),
        "float32" => Some("f32"),
        "float64" => Some("f64"),
        _ => None,
    }
}

fn map_module_root(module_root: &str, current_crate_name: &str) -> String {
    if module_root == current_crate_name {
        "crate".to_string()
    } else if module_root == "std" {
        "catla_std".to_string()
    } else {
        module_root.to_string()
    }
}

fn codegen_import(ast: &ImportStatement, current_crate_name: &str) -> String {
    let mut code = String::new();
    code += "use ";

    let mut path_temp = Vec::new();
    for (index, path) in ast.path.iter().enumerate() {
        if index == 0 {
            path_temp.push(map_module_root(path.value, current_crate_name));
        } else {
            path_temp.push(path.value.to_string());
        }
    }
    code += path_temp.join("::").as_str();

    if let Some(element) = &ast.elements_or_wild_card {
        code += "{";

        match element {
            ElementsOrWildCard::Elements(literals) => {
                code += literals
                    .iter()
                    .map(|literal| literal.value)
                    .collect::<Vec<_>>()
                    .join(", ")
                    .as_str();
            }
            ElementsOrWildCard::WildCard(_) => {
                code += "*";
            }
        }

        code += "}";
    }

    code += ";";

    code
}

fn collect_stack_alloc_new_object_entities(
    expression: &Expression,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
) -> Vec<EntityID> {
    let mut entities = Vec::new();
    let mut seen = HashSet::new();
    collect_stack_alloc_from_expression(
        expression,
        lifetime_analyze_results,
        &mut entities,
        &mut seen,
    );
    entities
}

fn collect_stack_alloc_from_expression(
    expression: &Expression,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    out_entities: &mut Vec<EntityID>,
    seen: &mut HashSet<EntityID>,
) {
    match expression {
        Expression::Return(return_expression) => {
            if let Some(inner) = return_expression.expression.as_ref() {
                collect_stack_alloc_from_expression(inner, lifetime_analyze_results, out_entities, seen);
            }
        }
        Expression::Closure(closure) => {
            if let Ok(expression_or_block) = &closure.expression_or_block {
                match expression_or_block {
                    catla_parser::ast::ExpressionOrBlock::Expression(expression) => {
                        collect_stack_alloc_from_expression(
                            expression,
                            lifetime_analyze_results,
                            out_entities,
                            seen,
                        );
                    }
                    catla_parser::ast::ExpressionOrBlock::Block(block) => {
                        for statement in block.program.statements.iter() {
                            if let Statement::DefineWithAttribute(define_with_attribute) = &statement.statement {
                                if let Ok(Define::Variable(variable_define)) = &define_with_attribute.define {
                                    if let Some(expression) = variable_define.expression.as_ref() {
                                        collect_stack_alloc_from_expression(
                                            expression,
                                            lifetime_analyze_results,
                                            out_entities,
                                            seen,
                                        );
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
        Expression::Or(or_expression) => {
            collect_stack_alloc_from_or_expression(
                or_expression,
                lifetime_analyze_results,
                out_entities,
                seen,
            );
        }
    }
}

fn collect_stack_alloc_from_or_expression(
    expression: &OrExpression,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    out_entities: &mut Vec<EntityID>,
    seen: &mut HashSet<EntityID>,
) {
    collect_stack_alloc_from_and_expression(
        &expression.left,
        lifetime_analyze_results,
        out_entities,
        seen,
    );
    for chain in expression.chain.iter() {
        collect_stack_alloc_from_and_expression(chain, lifetime_analyze_results, out_entities, seen);
    }
}

fn collect_stack_alloc_from_and_expression(
    expression: &AndExpression,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    out_entities: &mut Vec<EntityID>,
    seen: &mut HashSet<EntityID>,
) {
    collect_stack_alloc_from_equals_expression(
        &expression.left,
        lifetime_analyze_results,
        out_entities,
        seen,
    );
    for chain in expression.chain.iter() {
        collect_stack_alloc_from_equals_expression(chain, lifetime_analyze_results, out_entities, seen);
    }
}

fn collect_stack_alloc_from_equals_expression(
    expression: &EqualsExpression,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    out_entities: &mut Vec<EntityID>,
    seen: &mut HashSet<EntityID>,
) {
    collect_stack_alloc_from_less_or_greater_expression(
        &expression.left,
        lifetime_analyze_results,
        out_entities,
        seen,
    );
    for (_, chain) in expression.chain.iter() {
        collect_stack_alloc_from_less_or_greater_expression(
            chain,
            lifetime_analyze_results,
            out_entities,
            seen,
        );
    }
}

fn collect_stack_alloc_from_less_or_greater_expression(
    expression: &LessOrGreaterExpression,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    out_entities: &mut Vec<EntityID>,
    seen: &mut HashSet<EntityID>,
) {
    collect_stack_alloc_from_add_or_sub_expression(
        &expression.left,
        lifetime_analyze_results,
        out_entities,
        seen,
    );
    for (_, chain) in expression.chain.iter() {
        collect_stack_alloc_from_add_or_sub_expression(
            chain,
            lifetime_analyze_results,
            out_entities,
            seen,
        );
    }
}

fn collect_stack_alloc_from_add_or_sub_expression(
    expression: &AddOrSubExpression,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    out_entities: &mut Vec<EntityID>,
    seen: &mut HashSet<EntityID>,
) {
    collect_stack_alloc_from_mul_or_div_expression(
        &expression.left,
        lifetime_analyze_results,
        out_entities,
        seen,
    );
    for (_, chain) in expression.chain.iter() {
        collect_stack_alloc_from_mul_or_div_expression(
            chain,
            lifetime_analyze_results,
            out_entities,
            seen,
        );
    }
}

fn collect_stack_alloc_from_mul_or_div_expression(
    expression: &MulOrDivExpression,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    out_entities: &mut Vec<EntityID>,
    seen: &mut HashSet<EntityID>,
) {
    collect_stack_alloc_from_factor(
        &expression.left,
        lifetime_analyze_results,
        out_entities,
        seen,
    );
    for (_, chain) in expression.chain.iter() {
        collect_stack_alloc_from_factor(chain, lifetime_analyze_results, out_entities, seen);
    }
}

fn collect_stack_alloc_from_factor(
    factor: &Factor,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    out_entities: &mut Vec<EntityID>,
    seen: &mut HashSet<EntityID>,
) {
    let Ok(primary) = &factor.primary else {
        return;
    };
    collect_stack_alloc_from_primary(primary, lifetime_analyze_results, out_entities, seen);
}

fn collect_stack_alloc_from_primary(
    primary: &Primary,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    out_entities: &mut Vec<EntityID>,
    seen: &mut HashSet<EntityID>,
) {
    match &primary.left.first {
        PrimaryLeftExpr::Simple {
            left: _,
            generics: _,
            function_call,
            span: _,
        } => {
            if let Some(function_call) = function_call {
                for argument in function_call.arguments.iter() {
                    collect_stack_alloc_from_expression(
                        argument,
                        lifetime_analyze_results,
                        out_entities,
                        seen,
                    );
                }
            }
        }
        PrimaryLeftExpr::NewObject { new_object } => {
            let entity_id = EntityID::from(new_object);
            if lifetime_analyze_results
                .and_then(|results| results.object_result(entity_id))
                .map(|result| result.allocation == AllocationKind::Stack)
                .unwrap_or(false)
                && seen.insert(entity_id)
            {
                out_entities.push(entity_id);
            }

            for field_assign in new_object.field_assign.elements.iter() {
                collect_stack_alloc_from_expression(
                    &field_assign.expression,
                    lifetime_analyze_results,
                    out_entities,
                    seen,
                );
            }
        }
        PrimaryLeftExpr::NewArray { new_array } => {
            for expression in new_array.elements.iter() {
                collect_stack_alloc_from_expression(
                    expression,
                    lifetime_analyze_results,
                    out_entities,
                    seen,
                );
            }
        }
        PrimaryLeftExpr::NewArrayInit { new_array_init } => {
            if let Ok(init_expression) = &new_array_init.init_expression {
                collect_stack_alloc_from_expression(
                    init_expression,
                    lifetime_analyze_results,
                    out_entities,
                    seen,
                );
            }
            if let Ok(length_expression) = &new_array_init.length_expression {
                collect_stack_alloc_from_expression(
                    length_expression,
                    lifetime_analyze_results,
                    out_entities,
                    seen,
                );
            }
        }
        PrimaryLeftExpr::If { if_expression } => {
            if let Ok(condition) = &if_expression.first.condition {
                collect_stack_alloc_from_expression(
                    condition,
                    lifetime_analyze_results,
                    out_entities,
                    seen,
                );
            }
        }
        PrimaryLeftExpr::Loop { loop_expression: _ } => {}
    }

    for chain in primary.chain.iter() {
        let Some(second) = &chain.second else {
            continue;
        };
        let Some(function_call) = &second.function_call else {
            continue;
        };

        for argument in function_call.arguments.iter() {
            collect_stack_alloc_from_expression(argument, lifetime_analyze_results, out_entities, seen);
        }
    }
}

fn codegen_assignment_left_expression(
    expression: &Expression,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    user_type_set: &GlobalUserTypeSet,
    module_static_variables: &HashMap<String, bool>,
    current_crate_name: &str,
) -> Option<String> {
    let primary = extract_assignment_primary(expression)?;
    codegen_assignment_left_primary(
        primary,
        type_infer_results,
        user_type_set,
        module_static_variables,
        current_crate_name,
    )
}

fn extract_assignment_primary<'a, 'input, 'allocator>(
    expression: &'a Expression<'input, 'allocator>,
) -> Option<&'a Primary<'input, 'allocator>> {
    let Expression::Or(or_expression) = expression else {
        return None;
    };
    if !or_expression.chain.is_empty() {
        return None;
    }

    let and_expression = &or_expression.left;
    if !and_expression.chain.is_empty() {
        return None;
    }

    let equals_expression = &and_expression.left;
    if !equals_expression.chain.is_empty() {
        return None;
    }

    let less_or_greater_expression = &equals_expression.left;
    if !less_or_greater_expression.chain.is_empty() {
        return None;
    }

    let add_or_sub_expression = &less_or_greater_expression.left;
    if !add_or_sub_expression.chain.is_empty() {
        return None;
    }

    let mul_or_div_expression = &add_or_sub_expression.left;
    if !mul_or_div_expression.chain.is_empty() {
        return None;
    }

    let factor = &mul_or_div_expression.left;
    if factor.minus.is_some() {
        return None;
    }

    factor.primary.as_ref().ok()
}

fn codegen_assignment_left_primary(
    primary: &Primary,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    user_type_set: &GlobalUserTypeSet,
    module_static_variables: &HashMap<String, bool>,
    current_crate_name: &str,
) -> Option<String> {
    let PrimaryLeftExpr::Simple {
        left,
        generics: _,
        function_call,
        span: _,
    } = &primary.left.first
    else {
        return None;
    };

    if function_call.is_some() || primary.left.mapping_operator.is_some() {
        return None;
    }

    let SimplePrimary::Literal(literal) = left else {
        return None;
    };

    let mut code = String::new();

    let starts_with_module_path = primary
        .chain
        .first()
        .map(|right| right.separator.value == PrimarySeparator::DoubleColon)
        .unwrap_or(false);

    let is_mutable_static_root = !starts_with_module_path
        && module_static_variables
            .get(literal.value)
            .copied()
            .unwrap_or(false);

    if is_mutable_static_root {
        code += format!("(*{}.write().unwrap())", literal.value).as_str();
    } else if starts_with_module_path {
        code += map_module_root(literal.value, current_crate_name).as_str();
    } else {
        code += literal.value;
    }

    let insert_value_after_root = !starts_with_module_path
        && primary
            .chain
            .first()
            .map(|right| right.separator.value == PrimarySeparator::Dot)
            .unwrap_or(false);

    if insert_value_after_root {
        code += ".value()";
    }

    for (index, primary_right) in primary.chain.iter().enumerate() {
        let separator = match primary_right.separator.value {
            PrimarySeparator::Dot => ".",
            PrimarySeparator::DoubleColon => "::",
        };
        code += separator;

        let Some(second) = &primary_right.second else {
            continue;
        };
        if second.function_call.is_some() || primary_right.mapping_operator.is_some() {
            return None;
        }

        code += second.literal.value;

        let next_is_dot = primary
            .chain
            .get(index + 1)
            .map(|next| next.separator.value == PrimarySeparator::Dot)
            .unwrap_or(false);
        if next_is_dot
            && type_infer_results
                .get(&EntityID::from(&second.literal))
                .map(|ty| is_class_type(&ty.value, user_type_set))
                .unwrap_or(false)
        {
            code += ".value()";
        }
    }

    Some(code)
}

fn codegen_expression(
    ast: &Expression,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    user_type_set: &GlobalUserTypeSet,
    module_static_variables: &HashMap<String, bool>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
    stack_slots: &HashMap<EntityID, String>,
    in_static_initializer: bool,
    class_value_usage: ClassValueUsage,
) -> String {
    match ast {
        Expression::Return(return_expression) => todo!(),
        Expression::Closure(closure) => todo!(),
        Expression::Or(or_expression) => codegen_or_expression(
            *or_expression,
            type_infer_results,
            lifetime_analyze_results,
            user_type_set,
            module_static_variables,
            name_resolved_map,
            module_entity_type_map,
            builder,
            current_crate_name,
            stack_slots,
            in_static_initializer,
            class_value_usage,
        ),
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum ClassValueUsage {
    Normal,
    FunctionArgument,
}

fn codegen_or_expression(
    ast: &OrExpression,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    user_type_set: &GlobalUserTypeSet,
    module_static_variables: &HashMap<String, bool>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
    stack_slots: &HashMap<EntityID, String>,
    in_static_initializer: bool,
    class_value_usage: ClassValueUsage,
) -> String {
    let left_result = codegen_and_expression(
        &ast.left,
        type_infer_results,
        lifetime_analyze_results,
        user_type_set,
        module_static_variables,
        name_resolved_map,
        module_entity_type_map,
        builder,
        current_crate_name,
        stack_slots,
        in_static_initializer,
        class_value_usage,
    );

    match ast.chain.len() {
        0 => left_result,
        _ => todo!(),
    }
}

fn codegen_and_expression(
    ast: &AndExpression,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    user_type_set: &GlobalUserTypeSet,
    module_static_variables: &HashMap<String, bool>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
    stack_slots: &HashMap<EntityID, String>,
    in_static_initializer: bool,
    class_value_usage: ClassValueUsage,
) -> String {
    let left_result = codegen_equals_expression(
        &ast.left,
        type_infer_results,
        lifetime_analyze_results,
        user_type_set,
        module_static_variables,
        name_resolved_map,
        module_entity_type_map,
        builder,
        current_crate_name,
        stack_slots,
        in_static_initializer,
        class_value_usage,
    );

    match ast.chain.len() {
        0 => left_result,
        _ => todo!(),
    }
}

fn codegen_equals_expression(
    ast: &EqualsExpression,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    user_type_set: &GlobalUserTypeSet,
    module_static_variables: &HashMap<String, bool>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
    stack_slots: &HashMap<EntityID, String>,
    in_static_initializer: bool,
    class_value_usage: ClassValueUsage,
) -> String {
    let left_result = codegen_less_or_greater_expression(
        &ast.left,
        type_infer_results,
        lifetime_analyze_results,
        user_type_set,
        module_static_variables,
        name_resolved_map,
        module_entity_type_map,
        builder,
        current_crate_name,
        stack_slots,
        in_static_initializer,
        class_value_usage,
    );

    match ast.chain.len() {
        0 => left_result,
        _ => todo!(),
    }
}

fn codegen_less_or_greater_expression(
    ast: &LessOrGreaterExpression,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    user_type_set: &GlobalUserTypeSet,
    module_static_variables: &HashMap<String, bool>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
    stack_slots: &HashMap<EntityID, String>,
    in_static_initializer: bool,
    class_value_usage: ClassValueUsage,
) -> String {
    let left_result = codegen_add_or_sub_expression(
        &ast.left,
        type_infer_results,
        lifetime_analyze_results,
        user_type_set,
        module_static_variables,
        name_resolved_map,
        module_entity_type_map,
        builder,
        current_crate_name,
        stack_slots,
        in_static_initializer,
        class_value_usage,
    );

    match ast.chain.len() {
        0 => left_result,
        _ => todo!(),
    }
}

fn codegen_add_or_sub_expression(
    ast: &AddOrSubExpression,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    user_type_set: &GlobalUserTypeSet,
    module_static_variables: &HashMap<String, bool>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
    stack_slots: &HashMap<EntityID, String>,
    in_static_initializer: bool,
    class_value_usage: ClassValueUsage,
) -> String {
    let left_result = codegen_mul_or_div_expression(
        &ast.left,
        type_infer_results,
        lifetime_analyze_results,
        user_type_set,
        module_static_variables,
        name_resolved_map,
        module_entity_type_map,
        builder,
        current_crate_name,
        stack_slots,
        in_static_initializer,
        class_value_usage,
    );

    match ast.chain.len() {
        0 => left_result,
        _ => {
            let left_type = type_infer_results.get(&EntityID::from(&ast.left)).unwrap();

            let mut result = String::new();
            result += left_result.as_str();

            if left_type.value.is_integer() || left_type.value.is_float() {
                for (op, right) in ast.chain.iter() {
                    let op = match op.value {
                        AddOrSub::Add => " + ",
                        AddOrSub::Sub => " - ",
                    };

                    result += op;

                    result += codegen_mul_or_div_expression(
                        right,
                        type_infer_results,
                        lifetime_analyze_results,
                        user_type_set,
                        module_static_variables,
                        name_resolved_map,
                        module_entity_type_map,
                        builder,
                        current_crate_name,
                        stack_slots,
                        in_static_initializer,
                        class_value_usage,
                    )
                    .as_str();
                }
            }

            result
        }
    }
}

fn codegen_mul_or_div_expression(
    ast: &MulOrDivExpression,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    user_type_set: &GlobalUserTypeSet,
    module_static_variables: &HashMap<String, bool>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
    stack_slots: &HashMap<EntityID, String>,
    in_static_initializer: bool,
    class_value_usage: ClassValueUsage,
) -> String {
    let left_result = codegen_for_factor(
        &ast.left,
        type_infer_results,
        lifetime_analyze_results,
        user_type_set,
        module_static_variables,
        name_resolved_map,
        module_entity_type_map,
        builder,
        current_crate_name,
        stack_slots,
        in_static_initializer,
        class_value_usage,
    );

    match ast.chain.len() {
        0 => left_result,
        _ => todo!(),
    }
}

fn codegen_for_factor(
    ast: &Factor,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    user_type_set: &GlobalUserTypeSet,
    module_static_variables: &HashMap<String, bool>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
    stack_slots: &HashMap<EntityID, String>,
    in_static_initializer: bool,
    class_value_usage: ClassValueUsage,
) -> String {
    if let Ok(primary) = &ast.primary {
        return codegen_for_primary(
            primary,
            type_infer_results,
            lifetime_analyze_results,
            user_type_set,
            module_static_variables,
            name_resolved_map,
            module_entity_type_map,
            builder,
            current_crate_name,
            stack_slots,
            in_static_initializer,
            class_value_usage,
        );
    }
    String::new()
}

fn codegen_for_primary(
    ast: &Primary,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    user_type_set: &GlobalUserTypeSet,
    module_static_variables: &HashMap<String, bool>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
    stack_slots: &HashMap<EntityID, String>,
    in_static_initializer: bool,
    class_value_usage: ClassValueUsage,
) -> String {
    match &ast.left.first {
        PrimaryLeftExpr::Simple {
            left,
            generics,
            function_call,
            span,
        } => {
            let mut code = String::new();
            let starts_with_module_path = ast
                .chain
                .first()
                .map(|right| right.separator.value == PrimarySeparator::DoubleColon)
                .unwrap_or(false);

            match left {
                SimplePrimary::Tuple { expressions, span } => todo!(),
                SimplePrimary::Literal(literal) => {
                    let is_static_root = !starts_with_module_path
                        && module_static_variables.contains_key(literal.value);

                    if is_static_root && function_call.is_none() {
                        let is_mutable_static = module_static_variables
                            .get(literal.value)
                            .copied()
                            .unwrap_or(false);
                        if is_mutable_static {
                            code += format!("(*{}.read().unwrap())", literal.value).as_str();
                        } else {
                            code += format!("(*{})", literal.value).as_str();
                        }
                    } else if starts_with_module_path {
                        code += map_module_root(literal.value, current_crate_name).as_str();
                    } else {
                        code += literal.value;
                    }
                }
                SimplePrimary::StringLiteral(literal) => {
                    code += format!(
                        "catla_std::string::String::from_static_str({})",
                        literal.value
                    )
                    .as_str()
                }
                SimplePrimary::NumericLiteral(literal) => code += literal.value,
                SimplePrimary::Null(range) => todo!(),
                SimplePrimary::True(range) => todo!(),
                SimplePrimary::False(range) => todo!(),
                SimplePrimary::This(range) => todo!(),
                SimplePrimary::LargeThis(range) => todo!(),
            }

            if let Some(function_call) = function_call {
                let left_function_type = match left {
                    SimplePrimary::Literal(literal) => resolve_callee_function_type_for_literal(
                        literal,
                        type_infer_results,
                        name_resolved_map,
                        module_entity_type_map,
                    ),
                    _ => None,
                };

                code += format!(
                    "({})",
                    function_call
                        .arguments
                        .iter()
                        .enumerate()
                        .map(|(argument_index, expression)| codegen_expression(
                            expression,
                            type_infer_results,
                            lifetime_analyze_results,
                            user_type_set,
                            module_static_variables,
                            name_resolved_map,
                            module_entity_type_map,
                            builder,
                            current_crate_name,
                            stack_slots,
                            in_static_initializer,
                            function_argument_usage(
                                left_function_type,
                                argument_index,
                                user_type_set,
                            ),
                        ))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
                .as_str();
            }

            for primary_right in ast.chain.iter() {
                let separator = match primary_right.separator.value {
                    PrimarySeparator::Dot => ".",
                    PrimarySeparator::DoubleColon => "::",
                };
                code += separator;

                if let Some(second) = &primary_right.second {
                    code += second.literal.value;

                    if let Some(function_call) = &second.function_call {
                        let second_function_type = resolve_callee_function_type_for_literal(
                            &second.literal,
                            type_infer_results,
                            name_resolved_map,
                            module_entity_type_map,
                        );

                        code += format!(
                            "({})",
                            function_call
                                .arguments
                                .iter()
                                .enumerate()
                                .map(|(argument_index, expression)| codegen_expression(
                                    expression,
                                    type_infer_results,
                                    lifetime_analyze_results,
                                    user_type_set,
                                    module_static_variables,
                                    name_resolved_map,
                                    module_entity_type_map,
                                    builder,
                                    current_crate_name,
                                    stack_slots,
                                    in_static_initializer,
                                    function_argument_usage(
                                        second_function_type,
                                        argument_index,
                                        user_type_set,
                                    ),
                                ))
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                        .as_str();
                    }
                }
            }

            let has_any_function_call = function_call.is_some()
                || ast.chain.iter().any(|primary_right| {
                    primary_right
                        .second
                        .as_ref()
                        .and_then(|second| second.function_call.as_ref())
                        .is_some()
                });
            let should_wrap_class_value = matches!(left, SimplePrimary::Literal(_))
                && !starts_with_module_path
                && !has_any_function_call;

            if should_wrap_class_value {
                code = match class_value_usage {
                    ClassValueUsage::Normal => format!("({}).clone()", code),
                    ClassValueUsage::FunctionArgument => format!("({}).borrow()", code),
                };
            }

            code
        }
        PrimaryLeftExpr::NewObject { new_object } => {
            let path = new_object
                .path
                .iter()
                .enumerate()
                .map(|(index, literal)| {
                    if index == 0 {
                        map_module_root(literal.value, current_crate_name)
                    } else {
                        literal.value.to_string()
                    }
                })
                .collect::<Vec<_>>()
                .join("::");

            let fields = new_object
                .field_assign
                .elements
                .iter()
                .map(|field_assign| {
                    format!(
                        "{}: {}",
                        field_assign.field.value,
                        codegen_expression(
                            &field_assign.expression,
                            type_infer_results,
                            lifetime_analyze_results,
                            user_type_set,
                            module_static_variables,
                            name_resolved_map,
                            module_entity_type_map,
                            builder,
                            current_crate_name,
                            stack_slots,
                            in_static_initializer,
                            ClassValueUsage::Normal,
                        )
                    )
                })
                .collect::<Vec<_>>();

            let value = if fields.is_empty() {
                format!("{} {{}}", path)
            } else {
                format!("{} {{ {} }}", path, fields.join(", "))
            };

            let is_class = type_infer_results
                .get(&EntityID::from(new_object))
                .map(|ty| {
                    if is_class_type(&ty.value, user_type_set) {
                        true
                    } else if let Type::UserType { user_type_info, .. } = &ty.value {
                        let info = user_type_set.get(*user_type_info);
                        let info = info.read().unwrap();
                        info.kind
                            .as_ref()
                            .map(|kind| kind.value != UserTypeKind::Struct)
                            .unwrap_or(true)
                    } else {
                        true
                    }
                })
                .unwrap_or(true);

            let object_result = lifetime_analyze_results
                .and_then(|results| results.object_result(EntityID::from(new_object)));
            let requires_drop = object_result.map(|result| result.requires_drop).unwrap_or(true);
            let mutex_flag = if in_static_initializer { "true" } else { "false" };

            if is_class {
                if in_static_initializer
                {
                    format!(
                        "catla_std::object::CatlaObjectRef::heap({}, {}, {})",
                        value, mutex_flag, requires_drop
                    )
                } else if object_result
                    .map(|result| result.allocation == AllocationKind::Stack)
                    .unwrap_or(false)
                {
                    if let Some(slot_name) = stack_slots.get(&EntityID::from(new_object)) {
                        format!(
                            "catla_std::object::CatlaObjectRef::stack({}, {}, &mut {})",
                            value, requires_drop, slot_name
                        )
                    } else {
                        format!(
                            "catla_std::object::CatlaObjectRef::heap({}, {}, {})",
                            value, mutex_flag, requires_drop
                        )
                    }
                } else {
                    format!(
                        "catla_std::object::CatlaObjectRef::heap({}, {}, {})",
                        value, mutex_flag, requires_drop
                    )
                }
            } else {
                value
            }
        }
        PrimaryLeftExpr::NewArray { new_array } => todo!(),
        PrimaryLeftExpr::NewArrayInit { new_array_init } => todo!(),
        PrimaryLeftExpr::If { if_expression } => todo!(),
        PrimaryLeftExpr::Loop { loop_expression } => todo!(),
    }
}
