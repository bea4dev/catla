use catla_name_resolver::{DefineKind, ResolvedInfo};
use catla_optimization::lifetime::{AllocationKind, LifetimeAnalyzeResults};
use catla_parser::ast::{
    AddOrSub, AddOrSubExpression, AndExpression, Define, DefineWithAttribute, ElementsOrWildCard,
    EntityID, EqualsExpression, Expression, Factor, FunctionDefine, GenericsDefine,
    ImportStatement, LessOrGreaterExpression, LetVar, MulOrDivExpression, OrExpression, Primary,
    PrimaryLeftExpr, PrimarySeparator, Program, SimplePrimary, Spanned, Statement,
    StatementAttribute, StatementWithTagAndDocs, TypeAlias, TypeAttribute, TypeInfo, TypeInfoBase,
    UserTypeDefine, UserTypeKind, VariableBinding, VariableDefine, WhereClause,
};
use catla_type::types::{GlobalUserTypeSet, Type};
use hashbrown::{HashMap, HashSet};

use crate::{CodeBuilder, CodeBuilderScope};

const RETURN_PLACE_ARG_NAME: &str = "__catla_return_place";
const RETURN_PLACE_SLOT_MARKER: &str = "__catla_return_place_marker";

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
    return_place_arg: Option<&'static str>,
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
                    if return_place_arg.is_some()
                        && lifetime_analyze_results
                            .map(|results| results.is_return_origin(entity))
                            .unwrap_or(false)
                    {
                        stack_slots.insert(entity, RETURN_PLACE_SLOT_MARKER.to_string());
                        continue;
                    }

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
                    name_resolved_map,
                    module_entity_type_map,
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
                        return_place_arg,
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
                    return_place_arg,
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
                                ast,
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
                                return_place_arg,
                            );
                        }
                        Define::TypeAlias(type_alias) => {
                            codegen_type_alias_define(
                                in_impl_scope,
                                define_with_attribute,
                                type_alias,
                                type_infer_results,
                                user_type_set,
                                builder,
                                current_crate_name,
                            );
                        }
                    }
                }
            }
            Statement::Drop(drop_statement) => todo!(),
            Statement::Expression(expression) => {
                let mut stack_slots = HashMap::new();
                let stack_entities =
                    collect_stack_alloc_new_object_entities(expression, lifetime_analyze_results);
                for entity in stack_entities.into_iter() {
                    if return_place_arg.is_some()
                        && lifetime_analyze_results
                            .map(|results| results.is_return_origin(entity))
                            .unwrap_or(false)
                    {
                        stack_slots.insert(entity, RETURN_PLACE_SLOT_MARKER.to_string());
                        continue;
                    }

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
                            &stack_slots,
                            false,
                            ClassValueUsage::Normal,
                            return_place_arg,
                        )
                        .as_str()
                    )
                    .as_str(),
                );
            }
            Statement::Implements(implements) => {
                if is_drop_implements_statement(implements) {
                    continue;
                }

                builder.push_line(
                    format!(
                        "impl{} {} for {} {} {{",
                        implements
                            .generics
                            .as_ref()
                            .map(|generics| {
                                codegen_for_generics_define_with_static_bound(
                                    generics,
                                    current_crate_name,
                                )
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
                                codegen_for_implements_concrete_type(
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
                            None,
                        );
                    }
                }

                builder.push_line("}");
            }
        }
    }
}

fn codegen_for_implements_concrete_type(
    ast: &TypeInfo,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    user_type_set: &GlobalUserTypeSet,
    current_crate_name: &str,
) -> String {
    if is_class_type_info_or_inferred(ast, type_infer_results, user_type_set) {
        codegen_for_object_type_without_class_wrapper(
            ast,
            type_infer_results,
            user_type_set,
            current_crate_name,
        )
        .unwrap_or_else(|| codegen_for_type_without_class_wrapper(ast, current_crate_name))
    } else {
        codegen_for_type(ast, type_infer_results, user_type_set, current_crate_name)
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

        static_variables.insert(
            literal.value.to_string(),
            variable_define.let_var.value == LetVar::Var,
        );
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
                    VariableBinding::Literal(literal) => {
                        type_infer_results.get(&EntityID::from(literal))
                    }
                    VariableBinding::Binding {
                        bindings: _,
                        span: _,
                    } => None,
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
    return_place_arg: Option<&'static str>,
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
                    None,
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
        let stack_entities =
            collect_stack_alloc_new_object_entities(expression, lifetime_analyze_results);
        for entity in stack_entities.into_iter() {
            if return_place_arg.is_some()
                && lifetime_analyze_results
                    .map(|results| results.is_return_origin(entity))
                    .unwrap_or(false)
            {
                stack_slots.insert(entity, RETURN_PLACE_SLOT_MARKER.to_string());
                continue;
            }

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
                    return_place_arg,
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

fn codegen_interface_ref_forward_impl(
    user_type_define: &UserTypeDefine,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    user_type_set: &GlobalUserTypeSet,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
) {
    if current_crate_name == "std" || current_crate_name == "catla_std" {
        return;
    }

    let object_module_path = if current_crate_name == "catla_std" || current_crate_name == "std" {
        "crate::object"
    } else {
        "catla_std::object"
    };
    let name = user_type_define
        .name
        .as_ref()
        .map(|name| name.value)
        .unwrap_or_default();
    let trait_generics_define = user_type_define
        .generics
        .as_ref()
        .map(|generics| codegen_for_generics_define(generics, current_crate_name))
        .unwrap_or_default();
    let trait_generics_arguments = user_type_define
        .generics
        .as_ref()
        .map(codegen_for_generics_arguments)
        .unwrap_or_default();

    let impl_generics = if trait_generics_define.is_empty() {
        format!("<T: {}::CatlaObject>", object_module_path)
    } else {
        let trait_generics_inner = &trait_generics_define[1..trait_generics_define.len() - 1];
        format!(
            "<T: {}::CatlaObject, {}>",
            object_module_path, trait_generics_inner
        )
    };

    let mut where_elements = vec![format!("T: {}{}", name, trait_generics_arguments)];
    if let Some(where_clause) = user_type_define.where_clause.as_ref() {
        where_elements.push(
            codegen_for_where_clause(
                where_clause,
                type_infer_results,
                user_type_set,
                current_crate_name,
            )
            .trim_start_matches("where ")
            .to_string(),
        );
    }
    let where_clause = if where_elements.is_empty() {
        String::new()
    } else {
        format!(" where {}", where_elements.join(", "))
    };

    builder.push_line(
        format!(
            "impl{} {}{} for {}::CatlaObjectRef<T>{} {{",
            impl_generics, name, trait_generics_arguments, object_module_path, where_clause
        )
        .as_str(),
    );

    {
        let scope = builder.scope();

        if let Ok(block) = &user_type_define.block {
            for statement in block.program.statements.iter() {
                let Statement::DefineWithAttribute(define_with_attribute) = &statement.statement
                else {
                    continue;
                };
                let Ok(Define::Function(function_define)) = &define_with_attribute.define else {
                    continue;
                };

                let signature = codegen_for_function_signature(
                    function_define,
                    type_infer_results,
                    module_entity_type_map,
                    user_type_set,
                    current_crate_name,
                );
                let has_return_place = function_return_object_type_for_function_define(
                    function_define,
                    type_infer_results,
                    module_entity_type_map,
                    user_type_set,
                    current_crate_name,
                )
                .is_some();
                scope.push_line(format!("{} {{", signature).as_str());
                {
                    let function_scope = scope.scope();

                    let Some(function_name) =
                        function_define.name.as_ref().ok().map(|name| name.value)
                    else {
                        function_scope.push_line("todo!()");
                        scope.push_line("}");
                        continue;
                    };

                    let arguments = function_define
                        .arguments
                        .as_ref()
                        .ok()
                        .map(|function_arguments| {
                            function_arguments
                                .arguments
                                .iter()
                                .map(|argument| codegen_for_variable_binding(&argument.binding))
                                .collect::<Vec<_>>()
                        })
                        .unwrap_or_default();
                    let has_this = function_define
                        .arguments
                        .as_ref()
                        .ok()
                        .map(|function_arguments| function_arguments.this_mutability.is_some())
                        .unwrap_or(false);

                    let call_expression = if has_this {
                        let mut call_arguments = vec!["std::ops::Deref::deref(self)".to_string()];
                        call_arguments.extend(arguments);
                        if has_return_place {
                            call_arguments.push(RETURN_PLACE_ARG_NAME.to_string());
                        }
                        format!(
                            "<T as {}{}>::{}({})",
                            name,
                            trait_generics_arguments,
                            function_name,
                            call_arguments.join(", ")
                        )
                    } else {
                        let mut call_arguments = arguments;
                        if has_return_place {
                            call_arguments.push(RETURN_PLACE_ARG_NAME.to_string());
                        }
                        format!(
                            "<T as {}{}>::{}({})",
                            name,
                            trait_generics_arguments,
                            function_name,
                            call_arguments.join(", ")
                        )
                    };

                    function_scope.push_line(call_expression.as_str());
                }
                scope.push_line("}");
            }
        }
    }

    builder.push_line("}");
}

fn codegen_user_type_define(
    in_impl_scope: bool,
    source_program: &Program,
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

            codegen_interface_ref_forward_impl(
                user_type_define,
                type_infer_results,
                module_entity_type_map,
                user_type_set,
                builder,
                current_crate_name,
            );
        }
        UserTypeKind::Class | UserTypeKind::Struct => {
            if user_type_define.kind.value == UserTypeKind::Struct {
                builder.push_line("#[derive(Debug, Clone, Copy)]");
            } else {
                builder.push_line("#[derive(Debug)]");
            }

            builder.push_line(
                format!(
                    "{}struct {}{}{} {{",
                    visibility, name, generics, where_clause
                )
                .as_str(),
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

            if user_type_define.kind.value == UserTypeKind::Class {
                builder.push_line(
                    format!(
                        "{}type {}{} = {}{};",
                        visibility,
                        class_value_type_alias_name(name),
                        generics,
                        name,
                        generics_arguments
                    )
                    .as_str(),
                );
            }

            if user_type_define.kind.value == UserTypeKind::Class {
                let object_struct_name = class_object_struct_name(name);
                let drop_impl_function = find_drop_function_for_class(source_program, name);

                builder.push_line("#[derive(Debug)]");
                builder.push_line(
                    format!(
                        "pub struct {}{}{} {{",
                        object_struct_name, generics, where_clause
                    )
                    .as_str(),
                );
                {
                    let scope = builder.scope();
                    scope.push_line(
                        format!(
                            "pub value: std::cell::UnsafeCell<{}{}>,",
                            name, generics_arguments
                        )
                        .as_str(),
                    );
                    scope.push_line("pub count_and_flags: std::cell::UnsafeCell<usize>,");
                }
                builder.push_line("}");

                builder.push_line(
                    format!(
                        "unsafe impl{} Sync for {}{}{} {{}}",
                        generics, object_struct_name, generics_arguments, where_clause
                    )
                    .as_str(),
                );
                builder.push_line(
                    format!(
                        "unsafe impl{} Send for {}{}{} {{}}",
                        generics, object_struct_name, generics_arguments, where_clause
                    )
                    .as_str(),
                );

                builder.push_line(
                    format!(
                        "impl{} {}{}{} {{",
                        generics, object_struct_name, generics_arguments, where_clause
                    )
                    .as_str(),
                );
                {
                    let scope = builder.scope();
                    scope.push_line(
                        format!(
                            "pub fn value(&self) -> &mut {}{} {{",
                            name, generics_arguments
                        )
                        .as_str(),
                    );
                    {
                        let value_scope = scope.scope();
                        value_scope.push_line("unsafe { &mut *self.value.get() }");
                    }
                    scope.push_line("}");
                }
                builder.push_line("}");

                builder.push_line(
                    format!(
                        "impl{} std::ops::Deref for {}{}{} {{",
                        generics, object_struct_name, generics_arguments, where_clause
                    )
                    .as_str(),
                );
                {
                    let scope = builder.scope();
                    scope.push_line(
                        format!("type Target = {}{};", name, generics_arguments).as_str(),
                    );
                    scope.push_line("fn deref(&self) -> &Self::Target {");
                    {
                        let deref_scope = scope.scope();
                        deref_scope.push_line("unsafe { &*self.value.get() }");
                    }
                    scope.push_line("}");
                }
                builder.push_line("}");

                builder.push_line(
                    format!(
                        "impl{} std::ops::DerefMut for {}{}{} {{",
                        generics, object_struct_name, generics_arguments, where_clause
                    )
                    .as_str(),
                );
                {
                    let scope = builder.scope();
                    scope.push_line("fn deref_mut(&mut self) -> &mut Self::Target {");
                    {
                        let deref_scope = scope.scope();
                        deref_scope.push_line("unsafe { &mut *self.value.get() }");
                    }
                    scope.push_line("}");
                }
                builder.push_line("}");

                builder.push_line(
                    format!(
                        "impl{} catla_std::dispose::Drop for {}{}{} {{",
                        generics, object_struct_name, generics_arguments, where_clause
                    )
                    .as_str(),
                );
                {
                    let scope = builder.scope();
                    scope.push_line("fn drop(&self) {");
                    {
                        let drop_scope = scope.scope();
                        drop_scope.push_line("let this = self;");

                        if let Some(drop_impl_function) = drop_impl_function {
                            if let Some(block) = &drop_impl_function.block {
                                codegen_for_program(
                                    block.program,
                                    false,
                                    type_infer_results,
                                    lifetime_analyze_results,
                                    user_type_set,
                                    module_static_variables,
                                    name_resolved_map,
                                    module_entity_type_map,
                                    &drop_scope,
                                    current_crate_name,
                                    stack_slot_counter,
                                    None,
                                );
                            }
                        }
                    }
                    scope.push_line("}");
                }
                builder.push_line("}");

                let catla_object_impl_generics = user_type_define
                    .generics
                    .as_ref()
                    .map(|generics| {
                        codegen_for_generics_define_with_static_bound(generics, current_crate_name)
                    })
                    .unwrap_or_default();
                builder.push_line(
                    format!(
                        "impl{} catla_std::object::CatlaObject for {}{}{} {{",
                        catla_object_impl_generics,
                        object_struct_name,
                        generics_arguments,
                        where_clause
                    )
                    .as_str(),
                );
                {
                    let scope = builder.scope();
                    scope.push_line(
                        format!("type Value = {}{};", name, generics_arguments).as_str(),
                    );

                    scope.push_line(
                        "fn new(value: Self::Value, mutex: bool, heap: bool, drop: bool) -> Self {",
                    );
                    {
                        let new_scope = scope.scope();
                        new_scope.push_line("Self {");
                        {
                            let init_scope = new_scope.scope();
                            init_scope.push_line("value: std::cell::UnsafeCell::new(value),");
                            init_scope.push_line(
                                "count_and_flags: std::cell::UnsafeCell::new(catla_std::object::init_count_and_flags(mutex, heap, drop)),",
                            );
                        }
                        new_scope.push_line("}");
                    }
                    scope.push_line("}");

                    scope.push_line("fn count_and_flags_ptr(&self) -> *mut usize {");
                    {
                        let ptr_scope = scope.scope();
                        ptr_scope.push_line("self.count_and_flags.get()");
                    }
                    scope.push_line("}");

                    scope.push_line("fn value_ptr(&self) -> *mut Self::Value {");
                    {
                        let ptr_scope = scope.scope();
                        ptr_scope.push_line("self.value.get()");
                    }
                    scope.push_line("}");
                }
                builder.push_line("}");
            }

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

            if user_type_define.kind.value == UserTypeKind::Class {
                let object_struct_name = class_object_struct_name(name);
                builder.push_line(
                    format!(
                        "impl{} {}{}{} {{",
                        generics, object_struct_name, generics_arguments, where_clause
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

fn is_drop_interface_type_info(interface: &TypeInfo) -> bool {
    let TypeInfoBase::Base(base_type_info) = &interface.base else {
        return false;
    };

    base_type_info
        .path
        .last()
        .map(|segment| segment.value == "Drop")
        .unwrap_or(false)
}

fn concrete_class_name_from_implements<'a, 'input, 'allocator>(
    implements: &'a catla_parser::ast::Implements<'input, 'allocator>,
) -> Option<&'a str> {
    let concrete = implements.concrete.as_ref().ok()?;
    let TypeInfoBase::Base(base_type_info) = &concrete.base else {
        return None;
    };

    base_type_info.path.last().map(|segment| segment.value)
}

fn is_drop_implements_statement(implements: &catla_parser::ast::Implements) -> bool {
    let Some(interface) = implements.interface.as_ref().ok() else {
        return false;
    };

    is_drop_interface_type_info(interface)
}

fn find_drop_function_for_class<'ast, 'input, 'allocator>(
    program: &'ast Program<'input, 'allocator>,
    class_name: &str,
) -> Option<&'ast FunctionDefine<'input, 'allocator>> {
    for statement in program.statements.iter() {
        let Statement::Implements(implements) = &statement.statement else {
            continue;
        };

        if !is_drop_implements_statement(implements) {
            continue;
        }

        let Some(concrete_class_name) = concrete_class_name_from_implements(implements) else {
            continue;
        };

        if concrete_class_name != class_name {
            continue;
        }

        let Ok(block) = &implements.block else {
            continue;
        };

        for statement in block.program.statements.iter() {
            let Statement::DefineWithAttribute(define_with_attribute) = &statement.statement else {
                continue;
            };
            let Ok(Define::Function(function_define)) = &define_with_attribute.define else {
                continue;
            };

            let Some(function_name) = function_define.name.as_ref().ok() else {
                continue;
            };
            if function_name.value == "drop" {
                return Some(function_define);
            }
        }
    }

    None
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
                        VariableBinding::Binding {
                            bindings: _,
                            span: _,
                        } => None,
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

fn codegen_type_alias_define(
    in_impl_scope: bool,
    define_with_attribute: &DefineWithAttribute,
    type_alias: &TypeAlias,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    user_type_set: &GlobalUserTypeSet,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
) {
    let public = !in_impl_scope
        && !define_with_attribute
            .attribute
            .iter()
            .any(|attribute| attribute.value == StatementAttribute::Private);
    let visibility = if public { "pub " } else { "" };

    let name = type_alias
        .name
        .as_ref()
        .map(|name| name.value)
        .unwrap_or_default();
    let generics = type_alias
        .generics
        .as_ref()
        .map(|generics| codegen_for_generics_define(generics, current_crate_name))
        .unwrap_or_default();
    let alias_type = type_alias
        .alias_type
        .as_ref()
        .map(|ty| codegen_for_type(ty, type_infer_results, user_type_set, current_crate_name))
        .unwrap_or_else(|_| "()".to_string());

    builder
        .push_line(format!("{}type {}{} = {};", visibility, name, generics, alias_type).as_str());

    if let Ok(alias_type_info) = &type_alias.alias_type {
        if is_class_type_info_or_inferred(alias_type_info, type_infer_results, user_type_set) {
            let value_alias_type = codegen_for_type_without_class_wrapper_nested(
                alias_type_info,
                type_infer_results,
                user_type_set,
                current_crate_name,
            );
            builder.push_line(
                format!(
                    "{}type {}{} = {};",
                    visibility,
                    class_value_type_alias_name(name),
                    generics,
                    value_alias_type
                )
                .as_str(),
            );

            if let Some(object_alias_type) = codegen_for_object_type_without_class_wrapper(
                alias_type_info,
                type_infer_results,
                user_type_set,
                current_crate_name,
            ) {
                builder.push_line(
                    format!(
                        "{}type {}{} = {};",
                        visibility,
                        class_object_struct_name(name),
                        generics,
                        object_alias_type
                    )
                    .as_str(),
                );
            }
        }
    }
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
        module_entity_type_map,
        user_type_set,
        current_crate_name,
    );
    let has_return_place = function_return_object_type_for_function_define(
        function_define,
        type_infer_results,
        module_entity_type_map,
        user_type_set,
        current_crate_name,
    )
    .is_some();
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
                if has_return_place {
                    Some(RETURN_PLACE_ARG_NAME)
                } else {
                    None
                },
            );
        }
    }

    builder.push_line("}");
}

fn codegen_for_function_signature(
    ast: &FunctionDefine,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    user_type_set: &GlobalUserTypeSet,
    current_crate_name: &str,
) -> String {
    let name = ast.name.as_ref().map(|name| name.value).unwrap_or_default();
    let generics = ast
        .generics
        .as_ref()
        .map(|generics| codegen_for_generics_define(generics, current_crate_name))
        .unwrap_or_default();
    let return_object_type = function_return_object_type_for_function_define(
        ast,
        type_infer_results,
        module_entity_type_map,
        user_type_set,
        current_crate_name,
    );
    let arguments = codegen_for_function_arguments(
        ast,
        type_infer_results,
        user_type_set,
        current_crate_name,
        return_object_type.as_deref(),
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
    return_object_type: Option<&str>,
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

    if let Some(return_object_type) = return_object_type {
        if arguments.len() > 1 {
            arguments += ", ";
        }
        arguments += format!(
            "mut {}: catla_std::object::ReturnPlace<'_, {}>",
            RETURN_PLACE_ARG_NAME, return_object_type
        )
        .as_str();
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
                        .map(|bound| codegen_for_type_without_class_wrapper(
                            bound,
                            current_crate_name
                        ))
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

fn codegen_for_generics_define_with_static_bound(
    ast: &GenericsDefine,
    current_crate_name: &str,
) -> String {
    let mut code = String::new();
    code += "<";

    code += ast
        .elements
        .iter()
        .map(|element| {
            if element.bounds.is_empty() {
                format!("{}: 'static", element.name.value)
            } else {
                format!(
                    "{}: {} + 'static",
                    element.name.value,
                    element
                        .bounds
                        .iter()
                        .map(|bound| codegen_for_type_without_class_wrapper(
                            bound,
                            current_crate_name
                        ))
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
    let mut code = codegen_for_type_without_class_wrapper_nested(
        ast,
        type_infer_results,
        user_type_set,
        current_crate_name,
    );

    for attribute in ast.attributes.iter() {
        match attribute {
            TypeAttribute::Optional { span: _ } => todo!(),
            TypeAttribute::Result {
                generics: _,
                span: _,
            } => todo!(),
        }
    }

    if is_class_type_info_or_inferred(ast, type_infer_results, user_type_set) {
        let object_code = codegen_for_object_type_without_class_wrapper(
            ast,
            type_infer_results,
            user_type_set,
            current_crate_name,
        )
        .unwrap_or(code);
        code = format!("catla_std::object::CatlaObjectRef<{}>", object_code);
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
                    .map(|generic| {
                        codegen_for_type_without_class_wrapper(generic, current_crate_name)
                    })
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

fn codegen_for_type_without_class_wrapper_nested(
    ast: &TypeInfo,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    user_type_set: &GlobalUserTypeSet,
    current_crate_name: &str,
) -> String {
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
                    .map(|generic| {
                        codegen_for_type(
                            generic,
                            type_infer_results,
                            user_type_set,
                            current_crate_name,
                        )
                    })
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

fn class_object_struct_name(class_name: &str) -> String {
    format!("__CatlaObject_{}", class_name)
}

fn class_value_type_alias_name(class_name: &str) -> String {
    format!("__CatlaValue_{}", class_name)
}

fn codegen_for_object_type_without_class_wrapper(
    ast: &TypeInfo,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    user_type_set: &GlobalUserTypeSet,
    current_crate_name: &str,
) -> Option<String> {
    let TypeInfoBase::Base(base_type_info) = &ast.base else {
        return None;
    };

    let mut path = if base_type_info.path.len() == 1 {
        vec![base_type_info.path[0].value.to_string()]
    } else {
        base_type_info
            .path
            .iter()
            .enumerate()
            .map(|(index, segment)| {
                if index == 0 {
                    map_module_root(segment.value, current_crate_name)
                } else {
                    segment.value.to_string()
                }
            })
            .collect::<Vec<_>>()
    };

    let Some(last) = path.last_mut() else {
        return None;
    };
    *last = class_object_struct_name(last);

    let mut code = path.join("::");
    if let Some(generics) = &base_type_info.generics {
        code += "<";
        code += generics
            .types
            .iter()
            .map(|generic| {
                codegen_for_type(
                    generic,
                    type_infer_results,
                    user_type_set,
                    current_crate_name,
                )
            })
            .collect::<Vec<_>>()
            .join(", ")
            .as_str();
        code += ">";
    }

    Some(code)
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
                format!(
                    "catla_std::object::CatlaObjectRef<{}>",
                    class_object_struct_name(type_name.as_str())
                )
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

fn is_class_type_info_or_inferred(
    type_info: &TypeInfo,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    user_type_set: &GlobalUserTypeSet,
) -> bool {
    if type_infer_results
        .get(&EntityID::from(type_info))
        .map(|ty| is_class_type(&ty.value, user_type_set))
        .unwrap_or(false)
    {
        return true;
    }

    is_class_type_info(type_info, user_type_set)
}

fn is_class_literal(
    literal: &Spanned<&str>,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    user_type_set: &GlobalUserTypeSet,
) -> bool {
    if let Some(ty) = type_infer_results.get(&EntityID::from(literal)) {
        return is_class_type(&ty.value, user_type_set);
    }

    let Some(resolved) = name_resolved_map.get(&EntityID::from(literal)) else {
        return false;
    };
    if resolved.define.kind != DefineKind::UserType {
        return false;
    }

    let Some(ty) = module_entity_type_map.get(&resolved.define.entity_id) else {
        return false;
    };

    is_class_type(ty, user_type_set)
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

fn function_return_object_type_from_type(
    function_type: &Type,
    user_type_set: &GlobalUserTypeSet,
    current_crate_name: &str,
) -> Option<String> {
    let Type::Function { function_info, .. } = function_type else {
        return None;
    };

    let Type::UserType {
        user_type_info,
        generics,
    } = &function_info.return_type.value
    else {
        return None;
    };
    let user_type_info = user_type_set.get(*user_type_info);
    let user_type_info = user_type_info.read().unwrap();
    if user_type_info
        .kind
        .as_ref()
        .map(|kind| kind.value != UserTypeKind::Class)
        .unwrap_or(true)
    {
        return None;
    }

    let mut code = class_object_struct_name(user_type_info.name.value.as_str());
    if !generics.is_empty() {
        code += "<";
        code += generics
            .iter()
            .map(|generic| codegen_for_inferred_type(generic, user_type_set, current_crate_name))
            .collect::<Vec<_>>()
            .join(", ")
            .as_str();
        code += ">";
    }
    Some(code)
}

fn function_return_object_type_for_function_define(
    ast: &FunctionDefine,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    user_type_set: &GlobalUserTypeSet,
    current_crate_name: &str,
) -> Option<String> {
    if let Some(return_type) = ast.return_type.as_ref() {
        if let Ok(type_info) = return_type.type_info.as_ref() {
            if is_class_type_info_or_inferred(type_info, type_infer_results, user_type_set) {
                return codegen_for_object_type_without_class_wrapper(
                    type_info,
                    type_infer_results,
                    user_type_set,
                    current_crate_name,
                );
            }
        }
    }

    let function_entity = EntityID::from(ast);
    let function_type = module_entity_type_map.get(&function_entity)?;
    function_return_object_type_from_type(function_type, user_type_set, current_crate_name)
}

fn function_returns_class(function_type: Option<&Type>, user_type_set: &GlobalUserTypeSet) -> bool {
    let Some(Type::Function { function_info, .. }) = function_type else {
        return false;
    };

    is_class_type(&function_info.return_type.value, user_type_set)
}

fn codegen_return_place_argument(
    call_entity: EntityID,
    stack_slots: &HashMap<EntityID, String>,
    in_static_initializer: bool,
    return_place_arg: Option<&'static str>,
) -> String {
    if in_static_initializer {
        return "catla_std::object::ReturnPlace::Heap".to_string();
    }

    if let Some(slot_name) = stack_slots.get(&call_entity) {
        if slot_name == RETURN_PLACE_SLOT_MARKER {
            if let Some(return_place_arg) = return_place_arg {
                return format!("{}.reborrow()", return_place_arg);
            }
        } else {
            return format!("catla_std::object::ReturnPlace::Stack(&mut {})", slot_name);
        }
    }

    "catla_std::object::ReturnPlace::Heap".to_string()
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

fn collect_stack_alloc_from_program(
    program: &Program,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    out_entities: &mut Vec<EntityID>,
    seen: &mut HashSet<EntityID>,
) {
    for statement in program.statements.iter() {
        match &statement.statement {
            Statement::Assignment(assignment) => {
                collect_stack_alloc_from_expression(
                    &assignment.left,
                    lifetime_analyze_results,
                    out_entities,
                    seen,
                );
                if let Ok(right) = &assignment.right {
                    collect_stack_alloc_from_expression(
                        right,
                        lifetime_analyze_results,
                        out_entities,
                        seen,
                    );
                }
            }
            Statement::Swap(swap_statement) => {
                collect_stack_alloc_from_expression(
                    &swap_statement.left,
                    lifetime_analyze_results,
                    out_entities,
                    seen,
                );
                if let Ok(right) = &swap_statement.right {
                    collect_stack_alloc_from_expression(
                        right,
                        lifetime_analyze_results,
                        out_entities,
                        seen,
                    );
                }
            }
            Statement::DefineWithAttribute(define_with_attribute) => {
                if let Ok(define) = &define_with_attribute.define {
                    match define {
                        Define::Variable(variable_define) => {
                            if let Some(expression) = variable_define.expression.as_ref() {
                                collect_stack_alloc_from_expression(
                                    expression,
                                    lifetime_analyze_results,
                                    out_entities,
                                    seen,
                                );
                            }
                        }
                        Define::UserType(user_type_define) => {
                            if let Ok(block) = &user_type_define.block {
                                collect_stack_alloc_from_program(
                                    block.program,
                                    lifetime_analyze_results,
                                    out_entities,
                                    seen,
                                );
                            }
                        }
                        Define::Function(_) | Define::TypeAlias(_) => {}
                    }
                }
            }
            Statement::Drop(drop_statement) => {
                if let Ok(expression) = &drop_statement.expression {
                    collect_stack_alloc_from_expression(
                        expression,
                        lifetime_analyze_results,
                        out_entities,
                        seen,
                    );
                }
            }
            Statement::Expression(expression) => {
                collect_stack_alloc_from_expression(
                    expression,
                    lifetime_analyze_results,
                    out_entities,
                    seen,
                );
            }
            Statement::Implements(implements) => {
                if let Ok(block) = &implements.block {
                    collect_stack_alloc_from_program(
                        block.program,
                        lifetime_analyze_results,
                        out_entities,
                        seen,
                    );
                }
            }
            Statement::Import(_) => {}
        }
    }
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
                collect_stack_alloc_from_expression(
                    inner,
                    lifetime_analyze_results,
                    out_entities,
                    seen,
                );
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
                        collect_stack_alloc_from_program(
                            block.program,
                            lifetime_analyze_results,
                            out_entities,
                            seen,
                        );
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
        collect_stack_alloc_from_and_expression(
            chain,
            lifetime_analyze_results,
            out_entities,
            seen,
        );
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
        collect_stack_alloc_from_equals_expression(
            chain,
            lifetime_analyze_results,
            out_entities,
            seen,
        );
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

                let entity_id = EntityID::from(function_call);
                if lifetime_analyze_results
                    .and_then(|results| results.object_result(entity_id))
                    .map(|result| result.allocation == AllocationKind::Stack)
                    .unwrap_or(false)
                    && seen.insert(entity_id)
                {
                    out_entities.push(entity_id);
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
            if let Ok(block) = &if_expression.first.block {
                collect_stack_alloc_from_program(
                    block.program,
                    lifetime_analyze_results,
                    out_entities,
                    seen,
                );
            }
            for chain in if_expression.chain.iter() {
                match chain {
                    catla_parser::ast::ElseChain::ElseIf { if_statement } => {
                        if let Ok(condition) = &if_statement.condition {
                            collect_stack_alloc_from_expression(
                                condition,
                                lifetime_analyze_results,
                                out_entities,
                                seen,
                            );
                        }
                        if let Ok(block) = &if_statement.block {
                            collect_stack_alloc_from_program(
                                block.program,
                                lifetime_analyze_results,
                                out_entities,
                                seen,
                            );
                        }
                    }
                    catla_parser::ast::ElseChain::Else { block } => {
                        collect_stack_alloc_from_program(
                            block.program,
                            lifetime_analyze_results,
                            out_entities,
                            seen,
                        );
                    }
                }
            }
        }
        PrimaryLeftExpr::Loop { loop_expression } => {
            if let Ok(block) = &loop_expression.block {
                collect_stack_alloc_from_program(
                    block.program,
                    lifetime_analyze_results,
                    out_entities,
                    seen,
                );
            }
        }
    }

    for chain in primary.chain.iter() {
        let Some(second) = &chain.second else {
            continue;
        };
        let Some(function_call) = &second.function_call else {
            continue;
        };

        for argument in function_call.arguments.iter() {
            collect_stack_alloc_from_expression(
                argument,
                lifetime_analyze_results,
                out_entities,
                seen,
            );
        }

        let entity_id = EntityID::from(function_call);
        if lifetime_analyze_results
            .and_then(|results| results.object_result(entity_id))
            .map(|result| result.allocation == AllocationKind::Stack)
            .unwrap_or(false)
            && seen.insert(entity_id)
        {
            out_entities.push(entity_id);
        }
    }
}

fn codegen_assignment_left_expression(
    expression: &Expression,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    user_type_set: &GlobalUserTypeSet,
    module_static_variables: &HashMap<String, bool>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    current_crate_name: &str,
) -> Option<String> {
    let primary = extract_assignment_primary(expression)?;
    codegen_assignment_left_primary(
        primary,
        type_infer_results,
        user_type_set,
        module_static_variables,
        name_resolved_map,
        module_entity_type_map,
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
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    module_entity_type_map: &HashMap<EntityID, Type>,
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
        if is_class_literal(
            literal,
            type_infer_results,
            name_resolved_map,
            module_entity_type_map,
            user_type_set,
        ) {
            code += class_object_struct_name(literal.value).as_str();
        } else {
            code += map_module_root(literal.value, current_crate_name).as_str();
        }
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

        if primary_right.separator.value == PrimarySeparator::DoubleColon
            && is_class_literal(
                &second.literal,
                type_infer_results,
                name_resolved_map,
                module_entity_type_map,
                user_type_set,
            )
        {
            code += class_object_struct_name(second.literal.value).as_str();
        } else {
            code += second.literal.value;
        }

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

fn codegen_program_as_expression(
    program: &Program,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    user_type_set: &GlobalUserTypeSet,
    module_static_variables: &HashMap<String, bool>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    current_crate_name: &str,
    stack_slots: &HashMap<EntityID, String>,
    in_static_initializer: bool,
    class_value_usage: ClassValueUsage,
    return_place_arg: Option<&'static str>,
) -> String {
    let temp_builder = CodeBuilder::new();
    {
        let scope = temp_builder.scope();
        let mut stack_slot_counter = program
            .span
            .start
            .saturating_add(program.span.end)
            .saturating_mul(1024);

        match program.statements.split_last() {
            Some((last_statement, prefix_statements))
                if matches!(last_statement.statement, Statement::Expression(_)) =>
            {
                if !prefix_statements.is_empty() {
                    let prefix_program = Program {
                        statements: prefix_statements,
                        span: program.span.clone(),
                    };
                    codegen_for_program(
                        &prefix_program,
                        false,
                        type_infer_results,
                        lifetime_analyze_results,
                        user_type_set,
                        module_static_variables,
                        name_resolved_map,
                        module_entity_type_map,
                        &scope,
                        current_crate_name,
                        &mut stack_slot_counter,
                        return_place_arg,
                    );
                }

                let Statement::Expression(expression) = &last_statement.statement else {
                    unreachable!();
                };

                let expression_code = codegen_expression(
                    expression,
                    type_infer_results,
                    lifetime_analyze_results,
                    user_type_set,
                    module_static_variables,
                    name_resolved_map,
                    module_entity_type_map,
                    &scope,
                    current_crate_name,
                    stack_slots,
                    in_static_initializer,
                    class_value_usage,
                    return_place_arg,
                );
                scope.push_line(expression_code.as_str());
            }
            _ => {
                codegen_for_program(
                    program,
                    false,
                    type_infer_results,
                    lifetime_analyze_results,
                    user_type_set,
                    module_static_variables,
                    name_resolved_map,
                    module_entity_type_map,
                    &scope,
                    current_crate_name,
                    &mut stack_slot_counter,
                    return_place_arg,
                );
                scope.push_line("()");
            }
        }
    }

    let inner_code = temp_builder.dump();
    format!("{{\n{}}}", inner_code)
}

fn codegen_block_as_expression(
    block: &catla_parser::ast::Block,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    lifetime_analyze_results: Option<&LifetimeAnalyzeResults>,
    user_type_set: &GlobalUserTypeSet,
    module_static_variables: &HashMap<String, bool>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    module_entity_type_map: &HashMap<EntityID, Type>,
    current_crate_name: &str,
    stack_slots: &HashMap<EntityID, String>,
    in_static_initializer: bool,
    class_value_usage: ClassValueUsage,
    return_place_arg: Option<&'static str>,
) -> String {
    codegen_program_as_expression(
        block.program,
        type_infer_results,
        lifetime_analyze_results,
        user_type_set,
        module_static_variables,
        name_resolved_map,
        module_entity_type_map,
        current_crate_name,
        stack_slots,
        in_static_initializer,
        class_value_usage,
        return_place_arg,
    )
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
    return_place_arg: Option<&'static str>,
) -> String {
    match ast {
        Expression::Return(return_expression) => match &return_expression.expression {
            Some(expression) => {
                let expression_code = codegen_expression(
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
                    class_value_usage,
                    return_place_arg,
                );

                format!("return {}", expression_code)
            }
            None => "return".to_string(),
        },
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
            return_place_arg,
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
    return_place_arg: Option<&'static str>,
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
        return_place_arg,
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
    return_place_arg: Option<&'static str>,
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
        return_place_arg,
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
    return_place_arg: Option<&'static str>,
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
        return_place_arg,
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
    return_place_arg: Option<&'static str>,
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
        return_place_arg,
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
    return_place_arg: Option<&'static str>,
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
        return_place_arg,
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
                        return_place_arg,
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
    return_place_arg: Option<&'static str>,
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
        return_place_arg,
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
    return_place_arg: Option<&'static str>,
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
            return_place_arg,
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
    return_place_arg: Option<&'static str>,
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
                        if is_class_literal(
                            literal,
                            type_infer_results,
                            name_resolved_map,
                            module_entity_type_map,
                            user_type_set,
                        ) {
                            code += class_object_struct_name(literal.value).as_str();
                        } else {
                            code += map_module_root(literal.value, current_crate_name).as_str();
                        }
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
                SimplePrimary::True(_range) => code += "true",
                SimplePrimary::False(_range) => code += "false",
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
                let mut argument_codes = function_call
                    .arguments
                    .iter()
                    .enumerate()
                    .map(|(argument_index, expression)| {
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
                            stack_slots,
                            in_static_initializer,
                            function_argument_usage(
                                left_function_type,
                                argument_index,
                                user_type_set,
                            ),
                            return_place_arg,
                        )
                    })
                    .collect::<Vec<_>>();
                if function_returns_class(left_function_type, user_type_set)
                    || type_infer_results
                        .get(&EntityID::from(function_call))
                        .map(|ty| is_class_type(&ty.value, user_type_set))
                        .unwrap_or(false)
                {
                    argument_codes.push(codegen_return_place_argument(
                        EntityID::from(function_call),
                        stack_slots,
                        in_static_initializer,
                        return_place_arg,
                    ));
                }
                code += format!("({})", argument_codes.join(", ")).as_str();
            }

            for primary_right in ast.chain.iter() {
                let separator = match primary_right.separator.value {
                    PrimarySeparator::Dot => ".",
                    PrimarySeparator::DoubleColon => "::",
                };
                code += separator;

                if let Some(second) = &primary_right.second {
                    if primary_right.separator.value == PrimarySeparator::DoubleColon
                        && is_class_literal(
                            &second.literal,
                            type_infer_results,
                            name_resolved_map,
                            module_entity_type_map,
                            user_type_set,
                        )
                    {
                        code += class_object_struct_name(second.literal.value).as_str();
                    } else {
                        code += second.literal.value;
                    }

                    if let Some(function_call) = &second.function_call {
                        let second_function_type = resolve_callee_function_type_for_literal(
                            &second.literal,
                            type_infer_results,
                            name_resolved_map,
                            module_entity_type_map,
                        );

                        let mut argument_codes = function_call
                            .arguments
                            .iter()
                            .enumerate()
                            .map(|(argument_index, expression)| {
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
                                    stack_slots,
                                    in_static_initializer,
                                    function_argument_usage(
                                        second_function_type,
                                        argument_index,
                                        user_type_set,
                                    ),
                                    return_place_arg,
                                )
                            })
                            .collect::<Vec<_>>();
                        if function_returns_class(second_function_type, user_type_set)
                            || type_infer_results
                                .get(&EntityID::from(function_call))
                                .map(|ty| is_class_type(&ty.value, user_type_set))
                                .unwrap_or(false)
                        {
                            argument_codes.push(codegen_return_place_argument(
                                EntityID::from(function_call),
                                stack_slots,
                                in_static_initializer,
                                return_place_arg,
                            ));
                        }
                        code += format!("({})", argument_codes.join(", ")).as_str();
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
            let object_path = new_object
                .path
                .iter()
                .enumerate()
                .map(|(index, literal)| {
                    if index == 0 && new_object.path.len() > 1 {
                        map_module_root(literal.value, current_crate_name)
                    } else if index + 1 == new_object.path.len() {
                        class_object_struct_name(literal.value)
                    } else {
                        literal.value.to_string()
                    }
                })
                .collect::<Vec<_>>()
                .join("::");
            let class_value_path = new_object
                .path
                .iter()
                .enumerate()
                .map(|(index, literal)| {
                    if index == 0 && new_object.path.len() > 1 {
                        map_module_root(literal.value, current_crate_name)
                    } else if index + 1 == new_object.path.len() {
                        class_value_type_alias_name(literal.value)
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
                            return_place_arg,
                        )
                    )
                })
                .collect::<Vec<_>>();

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
            let value = if is_class {
                if fields.is_empty() {
                    format!("{} {{}}", class_value_path)
                } else {
                    format!("{} {{ {} }}", class_value_path, fields.join(", "))
                }
            } else if fields.is_empty() {
                format!("{} {{}}", path)
            } else {
                format!("{} {{ {} }}", path, fields.join(", "))
            };

            let object_result = lifetime_analyze_results
                .and_then(|results| results.object_result(EntityID::from(new_object)));
            let requires_drop = object_result
                .map(|result| result.requires_drop)
                .unwrap_or(true);
            let mutex_flag = if in_static_initializer {
                "true"
            } else {
                "false"
            };

            if is_class {
                if in_static_initializer {
                    format!(
                        "catla_std::object::CatlaObjectRef::<{}>::heap({}, {}, {})",
                        object_path, value, mutex_flag, requires_drop
                    )
                } else if object_result
                    .map(|result| result.allocation == AllocationKind::Stack)
                    .unwrap_or(false)
                {
                    if let Some(slot_name) = stack_slots.get(&EntityID::from(new_object)) {
                        if slot_name == RETURN_PLACE_SLOT_MARKER {
                            if let Some(return_place_arg) = return_place_arg {
                                format!(
                                    "catla_std::object::CatlaObjectRef::<{}>::from_return_place({}, {}, {}, {}.reborrow())",
                                    object_path, value, mutex_flag, requires_drop, return_place_arg
                                )
                            } else {
                                format!(
                                    "catla_std::object::CatlaObjectRef::<{}>::heap({}, {}, {})",
                                    object_path, value, mutex_flag, requires_drop
                                )
                            }
                        } else {
                            format!(
                                "catla_std::object::CatlaObjectRef::<{}>::stack({}, {}, &mut {})",
                                object_path, value, requires_drop, slot_name
                            )
                        }
                    } else {
                        format!(
                            "catla_std::object::CatlaObjectRef::<{}>::heap({}, {}, {})",
                            object_path, value, mutex_flag, requires_drop
                        )
                    }
                } else {
                    format!(
                        "catla_std::object::CatlaObjectRef::<{}>::heap({}, {}, {})",
                        object_path, value, mutex_flag, requires_drop
                    )
                }
            } else {
                value
            }
        }
        PrimaryLeftExpr::NewArray { new_array } => todo!(),
        PrimaryLeftExpr::NewArrayInit { new_array_init } => todo!(),
        PrimaryLeftExpr::If { if_expression } => {
            let mut code = String::new();

            let first_condition_code = if_expression
                .first
                .condition
                .as_ref()
                .map(|condition| {
                    codegen_expression(
                        condition,
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
                        return_place_arg,
                    )
                })
                .unwrap_or_else(|_| "false".to_string());

            let first_block_code = if_expression
                .first
                .block
                .as_ref()
                .map(|block| {
                        codegen_block_as_expression(
                            block,
                        type_infer_results,
                        lifetime_analyze_results,
                        user_type_set,
                        module_static_variables,
                        name_resolved_map,
                        module_entity_type_map,
                        current_crate_name,
                            stack_slots,
                            in_static_initializer,
                            class_value_usage,
                            return_place_arg,
                        )
                    })
                .unwrap_or_else(|_| "{\n()\n}".to_string());

            code += format!("if {} {}", first_condition_code, first_block_code).as_str();

            let mut has_else = false;
            for chain in if_expression.chain.iter() {
                match chain {
                    catla_parser::ast::ElseChain::ElseIf { if_statement } => {
                        let condition_code = if_statement
                            .condition
                            .as_ref()
                            .map(|condition| {
                                codegen_expression(
                                    condition,
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
                                    return_place_arg,
                                )
                            })
                            .unwrap_or_else(|_| "false".to_string());
                        let block_code = if_statement
                            .block
                            .as_ref()
                            .map(|block| {
                                codegen_block_as_expression(
                                    block,
                                    type_infer_results,
                                    lifetime_analyze_results,
                                    user_type_set,
                                    module_static_variables,
                                    name_resolved_map,
                                    module_entity_type_map,
                                    current_crate_name,
                                    stack_slots,
                                    in_static_initializer,
                                    class_value_usage,
                                    return_place_arg,
                                )
                            })
                            .unwrap_or_else(|_| "{\n()\n}".to_string());

                        code += format!(" else if {} {}", condition_code, block_code).as_str();
                    }
                    catla_parser::ast::ElseChain::Else { block } => {
                        let block_code = codegen_block_as_expression(
                            block,
                            type_infer_results,
                            lifetime_analyze_results,
                            user_type_set,
                            module_static_variables,
                            name_resolved_map,
                            module_entity_type_map,
                            current_crate_name,
                            stack_slots,
                            in_static_initializer,
                            class_value_usage,
                            return_place_arg,
                        );
                        code += format!(" else {}", block_code).as_str();
                        has_else = true;
                    }
                }
            }

            if !has_else {
                code += " else {\n()\n}";
            }

            code
        }
        PrimaryLeftExpr::Loop { loop_expression } => todo!(),
    }
}
