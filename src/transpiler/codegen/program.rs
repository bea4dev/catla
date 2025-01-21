use std::ops::Range;

use allocator_api2::vec::Vec;
use bumpalo::{collections::String, format, Bump};
use catla_parser::parser::{
    AddOrSubExpression, AddOrSubOp, AddOrSubOpKind, AndExpression, CompareExpression, CompareOp,
    CompareOpKind, Expression, ExpressionEnum, Factor, FunctionCall, MulOrDivExpression,
    MulOrDivOp, MulOrDivOpKind, OrExpression, Primary, PrimaryLeft, PrimaryLeftExpr, Program,
    SimplePrimary, Spanned, StatementAST, TranspilerTag, UserTypeKindEnum, VariableBinding,
};
use either::Either;
use fxhash::{FxHashMap, FxHashSet};
use regex::Regex;

use crate::transpiler::{
    component::EntityID,
    context::TranspileModuleContext,
    name_resolver::{DefineKind, FoundDefineInfo},
    optimizer::lifetime_analyzer::{LifetimeAnalyzeResult, LifetimeAnalyzeResults},
    semantics::types::{
        import_module_collector::get_module_name_from_primary,
        type_inference::{ImplicitConvertKind, TypeInferenceResultContainer},
        type_info::Type,
    },
    TranspileError,
};

use super::{
    custom::rust_codegen::{rust_codegen_function, rust_codegen_user_type},
    CodeBuilder, StackAllocCodeBuilder,
};

fn create_temp_var<'allocator>(
    code_builder: &mut CodeBuilder<'allocator>,
    span: Range<usize>,
    entity_id: EntityID,
) -> String<'allocator> {
    create_temp_var_with_name(code_builder, span, entity_id, "")
}

fn create_temp_var_with_name<'allocator>(
    code_builder: &mut CodeBuilder<'allocator>,
    span: Range<usize>,
    entity_id: EntityID,
    name: &str,
) -> String<'allocator> {
    let temp_var_name = format!(
        in code_builder.allocator,
        "temp_{}_{}_{}_{}",
        span.start,
        span.end,
        entity_id.ptr,
        name,
    );

    code_builder.add_line(format!(in code_builder.allocator, "let {};", &temp_var_name));

    temp_var_name
}

fn build_drop_holder<'allocator>(
    holder: &str,
    ty: &Type,
    lifetime_analyze_result: LifetimeAnalyzeResult,
    code_builder: &mut CodeBuilder<'allocator>,
) {
    if let Type::Function {
        function_info,
        generics: _,
    } = ty
    {
        if !function_info.define_info.is_closure {
            return;
        }
    }

    let drop_func_name = if lifetime_analyze_result.contains_static {
        "drop_with_free"
    } else {
        "drop_without_free"
    };

    let code = format!(
        in code_builder.allocator,
        "{}.{}();",
        holder,
        drop_func_name,
    );
    code_builder.add_line(code);
}

pub const RUST_CODEGEN_TAG: &str = "rust_codegen";

fn contains_rust_codegen_tag(tags: &Vec<&TranspilerTag, &Bump>) -> bool {
    for tag in tags.iter() {
        if let Ok(literal) = &tag.literal {
            if literal.value == RUST_CODEGEN_TAG {
                return true;
            }
        }
    }
    false
}

pub(crate) fn add_auto_import<'allocator>(
    ast: Program,
    code_builder: &mut CodeBuilder<'allocator>,
    allocator: &'allocator Bump,
    context: &TranspileModuleContext,
) {
    let mut user_define_names = FxHashSet::default();

    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            Err(_) => continue,
        };

        match statement {
            StatementAST::Import(import) => {
                user_define_names
                    .extend(import.elements.elements.iter().map(|element| element.value));
            }
            StatementAST::VariableDefine(variable_define) => {
                fn collect_name<'a>(
                    binding: &'a VariableBinding,
                    user_define_names: &mut FxHashSet<&'a str>,
                ) {
                    match &binding.binding {
                        Either::Left(literal) => {
                            user_define_names.insert(literal.value);
                        }
                        Either::Right(bindings) => {
                            for binding in bindings.iter() {
                                collect_name(binding, user_define_names);
                            }
                        }
                    }
                }

                if let Ok(binding) = &variable_define.binding {
                    collect_name(binding, &mut user_define_names);
                }
            }
            StatementAST::FunctionDefine(function_define) => {
                if let Ok(name) = &function_define.name {
                    user_define_names.insert(name.value);
                }
            }
            StatementAST::UserTypeDefine(user_type_define) => {
                if let Ok(name) = &user_type_define.name {
                    user_define_names.insert(name.value);
                }
            }
            StatementAST::TypeDefine(type_define) => {
                if let Ok(name) = &type_define.name {
                    user_define_names.insert(name.value);
                }
            }
            _ => {}
        }
    }

    let mut module_element_map = FxHashMap::default();

    for module_name in context.context.auto_import.auto_import_modules.iter() {
        module_element_map.insert(module_name.clone(), FxHashSet::default());
    }

    for (element_name, module_name) in context.context.auto_import.auto_import_elements.iter() {
        let set = module_element_map
            .entry(module_name.clone())
            .or_insert_with(|| FxHashSet::default());
        set.insert(element_name.clone());
    }

    let regex = Regex::new(r"^std::").unwrap();

    for (module_name, element_names) in module_element_map {
        let replace = if context.module_name.starts_with("std::") {
            "crate::"
        } else {
            "catla_std::"
        };

        let module_name = regex.replace(module_name.as_str(), replace).to_string();

        if element_names.is_empty() {
            let module_name_last = module_name.split("::").last().unwrap();

            if !user_define_names.contains(module_name_last) {
                let code = format!(
                    in allocator,
                    "use {};",
                    module_name,
                );
                code_builder.add_line(code);
            }
        } else {
            let code = format!(
                in allocator,
                "use {}::{{{}}};",
                module_name,
                element_names
                    .into_iter()
                    .filter(|name| !user_define_names.contains(name.as_str()))
                    .collect::<Vec<_>>()
                    .join(", "),
            );
            code_builder.add_line(code);
        }
    }

    code_builder.add_line(String::from_str_in(
        "use catla_transpile_std::memory::{ CatlaRefObject, CatlaRefManagement };",
        allocator,
    ));
    code_builder.add_line(String::from_str_in(
        "use catla_transpile_std::holder::{ CatlaObjectHolder, CatlaObjectDummyHolder };",
        allocator,
    ));
    code_builder.add_line(String::from_str_in(
        "use catla_transpile_std::holder::Hold;",
        allocator,
    ));
}

pub(crate) fn codegen_program<'allocator, 'input: 'allocator, 'type_map: 'allocator>(
    ast: Program<'input, '_>,
    result_bind_var_name: Option<&str>,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_analyze_results: &LifetimeAnalyzeResults,
    import_element_map: &FxHashMap<EntityID, Spanned<std::string::String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_entity_type_map: &'type_map FxHashMap<EntityID, Type>,
    mut user_type_impl_builder: Option<&mut CodeBuilder<'allocator>>,
    top_stack_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    current_tree_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    mut code_builder: &mut CodeBuilder<'allocator>,
    is_interface_scope: bool,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    context: &TranspileModuleContext,
) {
    let mut transpiler_tags = Vec::new_in(allocator);

    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            Err(_) => continue,
        };

        match statement {
            StatementAST::Assignment(assignment) => {
                let temp_var_name = create_temp_var(
                    code_builder,
                    assignment.span.clone(),
                    EntityID::from(assignment),
                );

                codegen_expression(
                    assignment.right_expr.as_ref().unwrap(),
                    Some(temp_var_name.as_str()),
                    false,
                    type_inference_result,
                    lifetime_analyze_results,
                    import_element_map,
                    name_resolved_map,
                    module_entity_type_map,
                    top_stack_alloc_builder,
                    current_tree_alloc_builder,
                    code_builder,
                    allocator,
                    errors,
                    context,
                );

                codegen_expression(
                    assignment.left_expr,
                    Some(temp_var_name.as_str()),
                    true,
                    type_inference_result,
                    lifetime_analyze_results,
                    import_element_map,
                    name_resolved_map,
                    module_entity_type_map,
                    top_stack_alloc_builder,
                    current_tree_alloc_builder,
                    code_builder,
                    allocator,
                    errors,
                    context,
                );
            }
            StatementAST::Exchange(exchange) => {}
            StatementAST::Import(import) => {}
            StatementAST::StatementAttributes(_) => { /* do nothing */ }
            StatementAST::VariableDefine(variable_define) => {
                if let Some(user_type_impl_builder) = &mut user_type_impl_builder {
                    let name = variable_define
                        .binding
                        .as_ref()
                        .unwrap()
                        .binding
                        .as_ref()
                        .left()
                        .unwrap()
                        .value;

                    let name = String::from_str_in(name, allocator).into_bump_str();

                    code_builder.add_line_str(name);
                    code_builder.add_str(": ");

                    let mut type_name_builder = CodeBuilder::new(allocator);
                    codegen_type(
                        type_inference_result.get_entity_type(EntityID::from(
                            variable_define.type_tag.as_ref().unwrap(),
                        )),
                        &mut type_name_builder,
                        allocator,
                    );
                    let type_name = type_name_builder.to_string_without_indent().into_bump_str();

                    code_builder.add_str("std::cell::Cell<");
                    code_builder.add_str(type_name);
                    code_builder.add_str(">");
                    code_builder.add_str(",\n");

                    user_type_impl_builder.add_line_str("pub fn __get_");
                    user_type_impl_builder.add_str(name);
                    user_type_impl_builder.add_str("(&self) -> ");
                    user_type_impl_builder.add_str(type_name);
                    user_type_impl_builder
                        .add_str("{ if self.is_mutex() { self.lock(); let value = self.");
                    user_type_impl_builder.add_line_str(name);
                    user_type_impl_builder
                        .add_str(".get(); value.clone_ref_mutex(); self.unlock(); value }");
                    user_type_impl_builder.add_str(" else { let value = self.");
                    user_type_impl_builder.add_str(name);
                    user_type_impl_builder.add_str(".get(); value.clone_ref(); value } }\n");

                    user_type_impl_builder.add_line_str("pub fn __get_non_static_");
                    user_type_impl_builder.add_str(name);
                    user_type_impl_builder.add_str("(&self) -> ");
                    user_type_impl_builder.add_str(type_name);
                    user_type_impl_builder.add_str(" { let value = self.");
                    user_type_impl_builder.add_str(name);
                    user_type_impl_builder.add_str(".get(); value.clone_ref(); value }\n");

                    user_type_impl_builder.add_line_str("pub fn __get_non_rc");
                    user_type_impl_builder.add_str(name);
                    user_type_impl_builder.add_str("(&self) -> ");
                    user_type_impl_builder.add_str(type_name);
                    user_type_impl_builder.add_str(" { self.");
                    user_type_impl_builder.add_str(name);
                    user_type_impl_builder.add_str(".get()}\n");

                    user_type_impl_builder.add_line_str("pub fn __set_");
                    user_type_impl_builder.add_str(name);
                    user_type_impl_builder.add_str("(&self, new: ");
                    user_type_impl_builder.add_str(type_name);
                    user_type_impl_builder.add_str(") ");
                    user_type_impl_builder
                        .add_str("{ if self.is_mutex() { self.lock(); let value = self.");
                    user_type_impl_builder.add_str(name);
                    user_type_impl_builder.add_str(".get(); value.drop_ref_mutex(); self.");
                    user_type_impl_builder.add_str(name);
                    user_type_impl_builder.add_str(".set(new); self.unlock(); } else { ");
                    user_type_impl_builder.add_str("let value = self.");
                    user_type_impl_builder.add_str(name);
                    user_type_impl_builder.add_str(".get(); value.drop_ref(); self.");
                    user_type_impl_builder.add_str(name);
                    user_type_impl_builder.add_str(".set(new); } }\n");

                    user_type_impl_builder.add_line_str("pub fn __set_non_static");
                    user_type_impl_builder.add_str(name);
                    user_type_impl_builder.add_str("(&self, new: ");
                    user_type_impl_builder.add_str(type_name);
                    user_type_impl_builder.add_str(") { ");
                    user_type_impl_builder.add_str("let value = self.");
                    user_type_impl_builder.add_str(name);
                    user_type_impl_builder.add_str(".get(); value.drop_ref(); self.");
                    user_type_impl_builder.add_str(name);
                    user_type_impl_builder.add_str(".set(new); }\n");

                    user_type_impl_builder.add_line_str("pub fn __set_non_rc");
                    user_type_impl_builder.add_str(name);
                    user_type_impl_builder.add_str("(&self, new: ");
                    user_type_impl_builder.add_str(type_name);
                    user_type_impl_builder.add_str(") { self.");
                    user_type_impl_builder.add_str(name);
                    user_type_impl_builder.add_str(".set(new); }\n");
                } else {
                    code_builder.add_line_str("let ");

                    let mut var_binding_builder = CodeBuilder::new(allocator);
                    codegen_variable_binding(
                        variable_define.binding.as_ref().unwrap(),
                        &mut var_binding_builder,
                        allocator,
                    );
                    let variable_binding = var_binding_builder
                        .to_string_without_indent()
                        .into_bump_str();

                    code_builder.add_line_str(variable_binding);

                    if let Some(type_tag) = &variable_define.type_tag {
                        code_builder.add_line_str(": ");

                        codegen_type(
                            type_inference_result.get_entity_type(EntityID::from(type_tag)),
                            code_builder,
                            allocator,
                        );
                    }

                    code_builder.add_line_str(";");

                    if let Some(expression) = &variable_define.expression {
                        if let Ok(expression) = &expression {
                            codegen_expression(
                                *expression,
                                Some(variable_binding),
                                false,
                                type_inference_result,
                                lifetime_analyze_results,
                                import_element_map,
                                name_resolved_map,
                                module_entity_type_map,
                                top_stack_alloc_builder,
                                current_tree_alloc_builder,
                                code_builder,
                                allocator,
                                errors,
                                context,
                            );
                        }
                    }
                }
            }
            StatementAST::FunctionDefine(function_define) => {
                if contains_rust_codegen_tag(&transpiler_tags) {
                    rust_codegen_function(
                        function_define,
                        &transpiler_tags,
                        result_bind_var_name,
                        type_inference_result,
                        lifetime_analyze_results,
                        import_element_map,
                        name_resolved_map,
                        top_stack_alloc_builder,
                        current_tree_alloc_builder,
                        code_builder,
                        allocator,
                        errors,
                        context,
                    );
                } else {
                    code_builder.add_line_str("");

                    if !is_interface_scope {
                        code_builder.add_str("pub ");
                    }

                    code_builder.add_str("fn ");
                    code_builder.add_str(function_define.name.as_ref().unwrap().value);

                    let function_type = module_entity_type_map
                        .get(&EntityID::from(function_define))
                        .unwrap();

                    if let Type::Function {
                        function_info,
                        generics: _,
                    } = function_type
                    {
                        if !function_info.generics_define.is_empty() {
                            code_builder.add_str("<");
                            for generic in function_info.generics_define.iter() {
                                code_builder.add_str(generic.name.as_str());
                            }
                            code_builder.add_str(">");
                        }

                        code_builder.add_str("(");

                        let mut argument_type_iterator = function_info.argument_types.iter();
                        if function_info.is_extension {
                            argument_type_iterator.next();
                        }

                        for (argument, argument_type) in function_define
                            .args
                            .arguments
                            .iter()
                            .zip(argument_type_iterator)
                        {
                            codegen_variable_binding(&argument.binding, code_builder, allocator);

                            code_builder.add_str(": ");

                            codegen_type(argument_type, code_builder, allocator);

                            code_builder.add_str(", ");
                        }

                        code_builder.add_str(")");

                        code_builder.add_str(" -> ");
                        codegen_type(&function_info.return_type.value, code_builder, allocator);

                        code_builder.add_str("{");
                    }

                    if let Either::Right(block) =
                        function_define.block_or_semicolon.value.as_ref().unwrap()
                    {
                        let parent_code_builder = &mut code_builder;
                        let mut code_builder = parent_code_builder.fork();

                        code_builder.push_indent();

                        let mut stack_alloc_code_builder = code_builder.fork();
                        let mut top_stack_alloc_builder =
                            StackAllocCodeBuilder::new(&mut stack_alloc_code_builder);

                        let mut current_tree_alloc_code_builder = code_builder.fork();
                        let mut current_tree_alloc_builder =
                            StackAllocCodeBuilder::new(&mut current_tree_alloc_code_builder);

                        codegen_program(
                            block.program,
                            None,
                            type_inference_result,
                            lifetime_analyze_results,
                            import_element_map,
                            name_resolved_map,
                            module_entity_type_map,
                            None,
                            &mut top_stack_alloc_builder,
                            &mut current_tree_alloc_builder,
                            &mut code_builder,
                            false,
                            allocator,
                            errors,
                            context,
                        );

                        code_builder.pull(current_tree_alloc_code_builder);
                        code_builder.pull(stack_alloc_code_builder);

                        parent_code_builder.pull(code_builder);
                    }

                    code_builder.add_line(String::from_str_in("}", allocator));
                }
            }
            StatementAST::UserTypeDefine(user_type_define) => {
                if contains_rust_codegen_tag(&transpiler_tags) {
                    rust_codegen_user_type(
                        user_type_define,
                        &transpiler_tags,
                        result_bind_var_name,
                        type_inference_result,
                        lifetime_analyze_results,
                        import_element_map,
                        name_resolved_map,
                        top_stack_alloc_builder,
                        current_tree_alloc_builder,
                        code_builder,
                        allocator,
                        errors,
                        context,
                    );
                } else {
                    let mut generics = Vec::new_in(allocator);

                    if let Some(generic_define) = &user_type_define.generics_define {
                        for element in generic_define.elements.iter() {
                            generics.push(
                                String::from_str_in(element.name.value, allocator).into_bump_str(),
                            );
                            generics.push(", ");
                        }
                    }

                    let mut user_type_impl_builder = None;

                    if user_type_define.kind.value != UserTypeKindEnum::Interface {
                        code_builder.add_line_str("impl<");

                        for generic in generics.iter() {
                            code_builder.add_str(*generic);
                        }

                        if user_type_define.kind.value == UserTypeKindEnum::Class {
                            code_builder.add_str("> CatlaRefObject<");
                        } else {
                            code_builder.add_str("> ");
                        }

                        code_builder.add_str(
                            String::from_str_in(
                                user_type_define.name.as_ref().unwrap().value,
                                allocator,
                            )
                            .into_bump_str(),
                        );

                        code_builder.add_str("<");

                        for generic in generics.iter() {
                            code_builder.add_str(*generic);
                        }

                        if user_type_define.kind.value == UserTypeKindEnum::Class {
                            code_builder.add_str(">> {\n");
                        } else {
                            code_builder.add_str("> {\n");
                        }

                        user_type_impl_builder = {
                            let mut builder = code_builder.fork();
                            builder.push_indent();
                            Some(builder)
                        };

                        code_builder.add_line_str("}");
                    }

                    code_builder.add_line_str("pub ");

                    let type_kind = match user_type_define.kind.value {
                        UserTypeKindEnum::Class => "struct",
                        UserTypeKindEnum::Struct => "struct",
                        UserTypeKindEnum::Interface => "trait",
                    };

                    code_builder.add_str(type_kind);
                    code_builder.add_str(" ");

                    code_builder.add_str(
                        String::from_str_in(
                            user_type_define.name.as_ref().unwrap().value,
                            allocator,
                        )
                        .into_bump_str(),
                    );

                    code_builder.add_str("<");

                    for generic in generics.iter() {
                        code_builder.add_str(*generic);
                    }

                    code_builder.add_str("> {");

                    let mut user_type_impl_builder = match user_type_impl_builder {
                        Some(builder) => builder,
                        None => {
                            let mut builder = code_builder.fork();
                            builder.push_indent();
                            builder
                        }
                    };

                    let mut child_code_builder = code_builder.fork();
                    child_code_builder.push_indent();

                    codegen_program(
                        user_type_define.block.value.as_ref().unwrap().program,
                        None,
                        type_inference_result,
                        lifetime_analyze_results,
                        import_element_map,
                        name_resolved_map,
                        module_entity_type_map,
                        Some(&mut user_type_impl_builder),
                        top_stack_alloc_builder,
                        current_tree_alloc_builder,
                        &mut child_code_builder,
                        user_type_define.kind.value == UserTypeKindEnum::Interface,
                        allocator,
                        errors,
                        context,
                    );

                    code_builder.add_line_str("}\n");

                    code_builder.pull(user_type_impl_builder);
                    code_builder.pull(child_code_builder);
                }
            }
            StatementAST::TypeDefine(type_define) => {}
            StatementAST::Implements(implements) => {}
            StatementAST::DropStatement(drop_statement) => {}
            StatementAST::Expression(expression) => {
                let mut current_tree_alloc_code_builder = code_builder.fork();
                let mut current_tree_alloc_builder =
                    StackAllocCodeBuilder::new(&mut current_tree_alloc_code_builder);

                codegen_expression(
                    expression,
                    None,
                    false,
                    type_inference_result,
                    lifetime_analyze_results,
                    import_element_map,
                    name_resolved_map,
                    module_entity_type_map,
                    top_stack_alloc_builder,
                    &mut current_tree_alloc_builder,
                    code_builder,
                    allocator,
                    errors,
                    context,
                );

                code_builder.pull(current_tree_alloc_code_builder);
            }
            StatementAST::TranspilerTag(transpiler_tag) => {
                transpiler_tags.push(transpiler_tag);
            }
        }

        if let StatementAST::TranspilerTag(_) = statement {
        } else {
            transpiler_tags.clear();
        }
    }
}

fn codegen_variable_binding<'allocator, 'input: 'allocator, 'type_map: 'allocator>(
    ast: &VariableBinding<'input, '_>,
    code_builder: &mut CodeBuilder<'allocator>,
    allocator: &'allocator Bump,
) {
    match &ast.binding {
        Either::Left(literal) => {
            code_builder.add_str(literal.value);
        }
        Either::Right(bindings) => {
            code_builder.add_str("(");

            for binding in bindings.iter() {
                codegen_variable_binding(binding, code_builder, allocator);

                code_builder.add_str(", ");
            }

            code_builder.add_str(")");
        }
    }
}

fn codegen_type<'allocator>(
    ty: &Type,
    code_builder: &mut CodeBuilder<'allocator>,
    allocator: &'allocator Bump,
) {
    let primitive_type_name = match ty {
        Type::Int8 => "i8",
        Type::Int16 => "i16",
        Type::Int32 => "i32",
        Type::Int64 => "i64",
        Type::Uint8 => "u8",
        Type::Uint16 => "u16",
        Type::Uint32 => "u32",
        Type::Uint64 => "u64",
        Type::Float32 => "f32",
        Type::Float64 => "f64",
        Type::Bool => "bool",
        Type::Unit => "()",
        Type::NumericLiteral(_) => unreachable!(),
        Type::UserType {
            user_type_info,
            generics,
            generics_span: _,
        } => {
            let name = format!(
                in allocator,
                "{}::{}",
                &user_type_info.module_name,
                user_type_info.name.value,
            );
            code_builder.add_str(name.into_bump_str());

            code_builder.add_str("<");

            for generic in generics.iter() {
                codegen_type(generic, code_builder, allocator);

                code_builder.add_str(", ");
            }

            code_builder.add_str(">");

            return;
        }
        Type::Function {
            function_info: _,
            generics: _,
        } => todo!(),
        Type::Generic(generic) => {
            code_builder.add_str(String::from_str_in(&generic.name, allocator).into_bump_str());
            return;
        }
        Type::LocalGeneric(_) => unreachable!(),
        Type::Array(_) => todo!(),
        Type::Tuple(types) => {
            code_builder.add_str("(");

            for ty in types.iter() {
                codegen_type(ty, code_builder, allocator);

                code_builder.add_str(", ");
            }

            code_builder.add_str(")");

            return;
        }
        Type::Option(base_type) => {
            code_builder.add_str("Option<");

            codegen_type(&base_type, code_builder, allocator);

            code_builder.add_str(">");

            return;
        }
        Type::Result { value, error } => {
            code_builder.add_str("Result<");

            codegen_type(&value, code_builder, allocator);

            code_builder.add_str(", ");

            codegen_type(&error, code_builder, allocator);

            code_builder.add_str(">");

            return;
        }
        Type::This => "Self",
        Type::Unreachable => "!",
        Type::Unknown => unreachable!(),
    };

    code_builder.add_line_str(primitive_type_name);
}

fn codegen_expression<'allocator, 'input: 'allocator, 'type_map: 'allocator>(
    ast: Expression<'input, '_>,
    result_bind_var_name: Option<&str>,
    as_assign_left: bool,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_analyze_results: &LifetimeAnalyzeResults,
    import_element_map: &FxHashMap<EntityID, Spanned<std::string::String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_entity_type_map: &'type_map FxHashMap<EntityID, Type>,
    top_stack_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    current_tree_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    code_builder: &mut CodeBuilder<'allocator>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    context: &TranspileModuleContext,
) {
    let parent_result_bind_var_name = result_bind_var_name;

    let result_bind_var_name = if parent_result_bind_var_name.is_some() {
        let expr_temp_var_name = create_temp_var_with_name(
            code_builder,
            ast.get_span(),
            EntityID::from(ast),
            "for_convert",
        );

        Some(expr_temp_var_name)
    } else {
        None
    };

    match ast {
        ExpressionEnum::OrExpression(or_expression) => {
            codegen_or_expression(
                or_expression,
                result_bind_var_name.as_ref().map(|str| str.as_str()),
                as_assign_left,
                type_inference_result,
                lifetime_analyze_results,
                import_element_map,
                name_resolved_map,
                module_entity_type_map,
                top_stack_alloc_builder,
                current_tree_alloc_builder,
                code_builder,
                allocator,
                errors,
                context,
            );
        }
        ExpressionEnum::ReturnExpression(return_expression) => {
            if let Some(result_bind_var_name) = &result_bind_var_name {
                code_builder
                    .add_line(format!(in allocator, "{} = unreachable!();", result_bind_var_name));
            }

            let temp_var_name = create_temp_var(
                code_builder,
                return_expression.span.clone(),
                EntityID::from(return_expression),
            );

            if let Some(expression) = return_expression.expression {
                codegen_expression(
                    expression,
                    Some(temp_var_name.as_str()),
                    as_assign_left,
                    type_inference_result,
                    lifetime_analyze_results,
                    import_element_map,
                    name_resolved_map,
                    module_entity_type_map,
                    top_stack_alloc_builder,
                    current_tree_alloc_builder,
                    code_builder,
                    allocator,
                    errors,
                    context,
                );
            } else {
                code_builder.add_line(format!(in allocator, "{} = ();", &temp_var_name));
            }

            code_builder.add_line(format!(in allocator, "return {};", &temp_var_name));
        }
        ExpressionEnum::Closure(closure) => {}
    }

    // implicit convert
    if let Some(result_bind_var_name) = &result_bind_var_name {
        if let Some(convert) = type_inference_result
            .implicit_convert_map
            .get(&EntityID::from(ast))
        {
            let code = match convert {
                ImplicitConvertKind::Some => {
                    format!(
                        in allocator,
                        "{} = Some({})",
                        parent_result_bind_var_name.unwrap(),
                        result_bind_var_name
                    )
                }
                ImplicitConvertKind::Ok => {
                    format!(
                        in allocator,
                        "{} = Ok({})",
                        parent_result_bind_var_name.unwrap(),
                        result_bind_var_name
                    )
                }
                ImplicitConvertKind::Error => {
                    format!(
                        in allocator,
                        "{} = Err({})",
                        parent_result_bind_var_name.unwrap(),
                        result_bind_var_name
                    )
                }
            };
            code_builder.add_line(code);
        } else {
            let code = format!(
                in allocator,
                "{} = {};",
                parent_result_bind_var_name.unwrap(),
                result_bind_var_name
            );
            code_builder.add_line(code);
        }
    }
}

macro_rules! codegen_for_op2 {
    (
        $function_name:ident,
        $ast_type:ident,
        $next_layer_function_name:ident,
        $operator_span_provider:expr,
        $method_name_provider:expr
    ) => {
        fn $function_name<'allocator, 'input: 'allocator, 'type_map: 'allocator>(
            ast: &$ast_type<'input, '_>,
            result_bind_var_name: Option<&str>,
            as_assign_left: bool,
            type_inference_result: &TypeInferenceResultContainer,
            lifetime_analyze_results: &LifetimeAnalyzeResults,
            import_element_map: &FxHashMap<EntityID, Spanned<std::string::String>>,
            name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
            module_entity_type_map: &'type_map FxHashMap<EntityID, Type>,
            top_stack_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
            current_tree_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
            code_builder: &mut CodeBuilder<'allocator>,
            allocator: &'allocator Bump,
            errors: &mut Vec<TranspileError>,
            context: &TranspileModuleContext,
        ) {
            if ast.right_exprs.is_empty() {
                $next_layer_function_name(
                    &ast.left_expr,
                    result_bind_var_name,
                    as_assign_left,
                    type_inference_result,
                    lifetime_analyze_results,
                    import_element_map,
                    name_resolved_map,
                    module_entity_type_map,
                    top_stack_alloc_builder,
                    current_tree_alloc_builder,
                    code_builder,
                    allocator,
                    errors,
                    context,
                );
            } else {
                let left_expr_temp_var_name = create_temp_var(code_builder, ast.left_expr.span.clone(), EntityID::from(&ast.left_expr));

                $next_layer_function_name(
                    &ast.left_expr,
                    Some(left_expr_temp_var_name.as_str()),
                    as_assign_left,
                    type_inference_result,
                    lifetime_analyze_results,
                    import_element_map,
                    name_resolved_map,
                    module_entity_type_map,
                    top_stack_alloc_builder,
                    current_tree_alloc_builder,
                    code_builder,
                    allocator,
                    errors,
                    context,
                );

                let mut last_result_bind_var_name = left_expr_temp_var_name;
                for (operator, right_expr) in ast.right_exprs.iter() {
                    let right_expr = right_expr.as_ref().unwrap();

                    let operator_result_var_name = create_temp_var_with_name(code_builder, $operator_span_provider(operator), EntityID::from(ast), "operator");
                    let right_expr_temp_var_name = create_temp_var(code_builder, right_expr.span.clone(), EntityID::from(right_expr));

                    $next_layer_function_name(
                        right_expr,
                        Some(right_expr_temp_var_name.as_str()),
                        as_assign_left,
                        type_inference_result,
                        lifetime_analyze_results,
                        import_element_map,
                        name_resolved_map,
                        module_entity_type_map,
                        top_stack_alloc_builder,
                        current_tree_alloc_builder,
                        code_builder,
                        allocator,
                        errors,
                        context,
                    );

                    if let Some(convert) = type_inference_result
                        .implicit_convert_map
                        .get(&EntityID::from(right_expr))
                    {
                        let convert_name = match convert {
                            ImplicitConvertKind::Some => "Some",
                            ImplicitConvertKind::Ok => "Ok",
                            ImplicitConvertKind::Error => "Err",
                        };
                        code_builder.add_line(format!(
                            in allocator,
                            "{} = {}.{}({}({}));",
                            &operator_result_var_name,
                            &last_result_bind_var_name,
                            $method_name_provider(operator),
                            convert_name,
                            &right_expr_temp_var_name
                        ));
                    } else {
                        code_builder.add_line(format!(
                            in allocator,
                            "{} = {}.{}({});",
                            &operator_result_var_name,
                            &last_result_bind_var_name,
                            $method_name_provider(operator),
                            &right_expr_temp_var_name
                        ));
                    }

                    last_result_bind_var_name = operator_result_var_name;
                }

                if let Some(result_bind_var_name) = result_bind_var_name {
                    code_builder.add_line(
                        format!(in allocator, "{} = {}", result_bind_var_name, &last_result_bind_var_name),
                    );
                } else {
                    code_builder.add_line(format!(in allocator, "{}.drop();", &last_result_bind_var_name));
                }
            }
        }
    };
}

codegen_for_op2!(
    codegen_or_expression,
    OrExpression,
    codegen_and_expression,
    |operator: &Range<usize>| { operator.clone() },
    |_| { "or" }
);

codegen_for_op2!(
    codegen_and_expression,
    AndExpression,
    codegen_compare_expression,
    |operator: &Range<usize>| { operator.clone() },
    |_| { "and" }
);

codegen_for_op2!(
    codegen_compare_expression,
    CompareExpression,
    codegen_add_or_sub_expression,
    |operator: &CompareOp| { operator.span.clone() },
    |operator: &CompareOp| {
        match operator.value {
            CompareOpKind::GreaterThan => "greater_than",
            CompareOpKind::GreaterOrEqual => "greater_or_equals",
            CompareOpKind::LessThan => "less_than",
            CompareOpKind::LessOrEqual => "less_or_equals",
            CompareOpKind::Equal => "equals",
            CompareOpKind::NotEqual => "not_equals",
        }
    }
);

codegen_for_op2!(
    codegen_add_or_sub_expression,
    AddOrSubExpression,
    codegen_mul_or_div_expression,
    |operator: &AddOrSubOp| { operator.span.clone() },
    |operator: &AddOrSubOp| {
        match operator.value {
            AddOrSubOpKind::Add => "add",
            AddOrSubOpKind::Sub => "sub",
        }
    }
);

codegen_for_op2!(
    codegen_mul_or_div_expression,
    MulOrDivExpression,
    codegen_factor,
    |operator: &MulOrDivOp| { operator.span.clone() },
    |operator: &MulOrDivOp| {
        match operator.value {
            MulOrDivOpKind::Mul => "mul",
            MulOrDivOpKind::Div => "div",
        }
    }
);

fn codegen_factor<'allocator, 'input: 'allocator, 'type_map: 'allocator>(
    ast: &Factor<'input, '_>,
    result_bind_var_name: Option<&str>,
    as_assign_left: bool,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_analyze_results: &LifetimeAnalyzeResults,
    import_element_map: &FxHashMap<EntityID, Spanned<std::string::String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_entity_type_map: &'type_map FxHashMap<EntityID, Type>,
    top_stack_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    current_tree_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    code_builder: &mut CodeBuilder<'allocator>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    context: &TranspileModuleContext,
) {
    // TODO : impl negative interface
    if ast.negative_keyword_span.is_some() {
    } else {
        codegen_primary(
            ast.primary.as_ref().unwrap(),
            result_bind_var_name,
            as_assign_left,
            type_inference_result,
            lifetime_analyze_results,
            import_element_map,
            name_resolved_map,
            module_entity_type_map,
            top_stack_alloc_builder,
            current_tree_alloc_builder,
            code_builder,
            allocator,
            errors,
            context,
        );
    }
}

fn codegen_primary<'allocator, 'input: 'allocator, 'type_map: 'allocator>(
    ast: &Primary<'input, '_>,
    result_bind_var_name: Option<&str>,
    as_assign_left: bool,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_analyze_results: &LifetimeAnalyzeResults,
    import_element_map: &FxHashMap<EntityID, Spanned<std::string::String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_entity_type_map: &'type_map FxHashMap<EntityID, Type>,
    top_stack_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    current_tree_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    code_builder: &mut CodeBuilder<'allocator>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    context: &TranspileModuleContext,
) {
    let module_name_result =
        get_module_name_from_primary(ast, name_resolved_map, import_element_map, context);

    let mut current_primary_index = 0;

    let mut last_result_temp_var = String::new_in(allocator);
    let mut last_entity_id = EntityID::dummy();

    if let Some((module_name, primary_index)) = module_name_result {
        let right_primary = &ast.chain[primary_index];

        if let Some((literal, _, function_call)) = &right_primary.second_expr {
            if let Some(function_call) = function_call {
                let is_closure = type_inference_result
                    .get_entity_type(EntityID::from(literal))
                    .is_closure();

                let literal_value_temp = if is_closure {
                    let literal_value_temp = create_temp_var(
                        code_builder,
                        literal.span.clone(),
                        EntityID::from(literal),
                    );

                    let code = format!(
                        in allocator,
                        "{} = {}::{}.get();",
                        &literal_value_temp,
                        &module_name,
                        literal.value,
                    );
                    code_builder.add_line(code);

                    literal_value_temp
                } else {
                    format!(in allocator, "{}::{}", &module_name, literal.value)
                };

                let (argument_results, argument_drop_info) = codegen_function_call_arguments(
                    function_call,
                    type_inference_result,
                    lifetime_analyze_results,
                    import_element_map,
                    name_resolved_map,
                    module_entity_type_map,
                    top_stack_alloc_builder,
                    current_tree_alloc_builder,
                    code_builder,
                    allocator,
                    errors,
                    context,
                );

                let result_temp = create_temp_var(
                    code_builder,
                    function_call.span.clone(),
                    EntityID::from(function_call),
                );

                let mut arguments = String::new_in(allocator);
                for argument_result in argument_results.iter() {
                    arguments += argument_result.as_str();
                    arguments += ".borrow(), ";
                }

                let code = format!(
                    in allocator,
                    "{} = {}({});",
                    &result_temp,
                    literal_value_temp,
                    arguments,
                );
                code_builder.add_line(code);

                for (argument_expr_result, (ty, lifetime_analyze_result)) in
                    argument_results.iter().zip(argument_drop_info.iter()).rev()
                {
                    build_drop_holder(
                        argument_expr_result.as_str(),
                        *ty,
                        lifetime_analyze_result.clone(),
                        code_builder,
                    );
                }

                if is_closure {
                    build_drop_holder(
                        literal_value_temp.as_str(),
                        type_inference_result.get_entity_type(EntityID::from(literal)),
                        lifetime_analyze_results.get_result(EntityID::from(literal)),
                        code_builder,
                    );
                }

                last_result_temp_var = result_temp;
                last_entity_id = EntityID::from(function_call);
            } else {
                let result_temp =
                    create_temp_var(code_builder, literal.span.clone(), EntityID::from(literal));

                let is_static_var = if let Type::Function {
                    function_info,
                    generics: _,
                } =
                    type_inference_result.get_entity_type(EntityID::from(literal))
                {
                    if function_info.define_info.is_closure {
                        true
                    } else {
                        false
                    }
                } else {
                    true
                };

                if is_static_var {
                    let code = format!(
                        in allocator,
                        "{} = {}::{}.get();",
                        &result_temp,
                        &module_name,
                        literal.value,
                    );
                    code_builder.add_line(code);
                } else {
                    let code = format!(
                        in allocator,
                        "{} = {}::{};",
                        &result_temp,
                        &module_name,
                        literal.value,
                    );
                    code_builder.add_line(code);
                }

                last_result_temp_var = result_temp;
                last_entity_id = EntityID::from(literal);
            }
        }

        // TODO : mapping operator

        current_primary_index = primary_index + 1;
    } else {
        let result_temp = create_temp_var(
            code_builder,
            ast.left.span.clone(),
            EntityID::from(&ast.left),
        );

        codegen_primary_left(
            &ast.left,
            result_temp.as_str(),
            false,
            type_inference_result,
            lifetime_analyze_results,
            import_element_map,
            name_resolved_map,
            module_entity_type_map,
            top_stack_alloc_builder,
            current_tree_alloc_builder,
            code_builder,
            allocator,
            errors,
            context,
        );

        last_result_temp_var = result_temp;
        last_entity_id = EntityID::from(&ast.left);
    }

    loop {
        if current_primary_index >= ast.chain.len() {
            break;
        }

        let primary_right = &ast.chain[current_primary_index];

        if let Some((literal, _, function_call)) = &primary_right.second_expr {
            if let Some(function_call) = function_call {
                let is_closure = type_inference_result
                    .get_entity_type(EntityID::from(literal))
                    .is_closure();

                let literal_value_temp = if is_closure {
                    let literal_value_temp = create_temp_var(
                        code_builder,
                        literal.span.clone(),
                        EntityID::from(literal),
                    );

                    let contains_static = lifetime_analyze_results
                        .get_result(EntityID::from(literal))
                        .contains_static;

                    let getter_name = if contains_static {
                        "__get"
                    } else {
                        "__get_non_static"
                    };

                    let code = format!(
                        in allocator,
                        "{} = {}.{}_{}().hold();",
                        &literal_value_temp,
                        &last_result_temp_var,
                        getter_name,
                        literal.value,
                    );
                    code_builder.add_line(code);

                    literal_value_temp
                } else {
                    format!(
                        in allocator,
                        "{}.{}",
                        &last_result_temp_var,
                        literal.value,
                    )
                };

                let (argument_results, argument_drop_info) = codegen_function_call_arguments(
                    function_call,
                    type_inference_result,
                    lifetime_analyze_results,
                    import_element_map,
                    name_resolved_map,
                    module_entity_type_map,
                    top_stack_alloc_builder,
                    current_tree_alloc_builder,
                    code_builder,
                    allocator,
                    errors,
                    context,
                );

                let result_temp = create_temp_var(
                    code_builder,
                    function_call.span.clone(),
                    EntityID::from(function_call),
                );

                let mut arguments = String::new_in(allocator);
                for argument_result in argument_results.iter() {
                    arguments += argument_result.as_str();
                    arguments += ".borrow(), ";
                }

                let code = format!(
                    in allocator,
                    "{} = {}({});",
                    &result_temp,
                    literal_value_temp,
                    arguments,
                );
                code_builder.add_line(code);

                for (argument_expr_result, (ty, lifetime_analyze_result)) in
                    argument_results.iter().zip(argument_drop_info.iter()).rev()
                {
                    build_drop_holder(
                        argument_expr_result.as_str(),
                        *ty,
                        lifetime_analyze_result.clone(),
                        code_builder,
                    );
                }

                if is_closure {
                    build_drop_holder(
                        literal_value_temp.as_str(),
                        type_inference_result.get_entity_type(EntityID::from(literal)),
                        lifetime_analyze_results.get_result(EntityID::from(literal)),
                        code_builder,
                    );
                }

                build_drop_holder(
                    &last_result_temp_var,
                    type_inference_result.get_entity_type(last_entity_id),
                    lifetime_analyze_results.get_result(last_entity_id),
                    code_builder,
                );

                last_result_temp_var = result_temp;
                last_entity_id = EntityID::from(function_call);
            } else {
                let literal_value_temp =
                    create_temp_var(code_builder, literal.span.clone(), EntityID::from(literal));

                let contains_static = lifetime_analyze_results
                    .get_result(EntityID::from(literal))
                    .contains_static;

                let getter_name = if contains_static {
                    "__get"
                } else {
                    "__get_non_static"
                };

                let code = format!(
                    in allocator,
                    "{} = {}.{}_{}().hold();",
                    &literal_value_temp,
                    &last_result_temp_var,
                    getter_name,
                    literal.value,
                );
                code_builder.add_line(code);

                build_drop_holder(
                    &last_result_temp_var,
                    type_inference_result.get_entity_type(last_entity_id),
                    lifetime_analyze_results.get_result(last_entity_id),
                    code_builder,
                );

                last_result_temp_var = literal_value_temp;
                last_entity_id = EntityID::from(literal);
            }
        }

        current_primary_index += 1;
    }

    if let Some(result_bind_var_name) = result_bind_var_name {
        let code = format!(
            in allocator,
            "{} = {};",
            result_bind_var_name,
            last_result_temp_var,
        );
        code_builder.add_line(code);
    } else {
        if !as_assign_left {
            build_drop_holder(
                &last_result_temp_var,
                type_inference_result.get_entity_type(last_entity_id),
                lifetime_analyze_results.get_result(last_entity_id),
                code_builder,
            );
        }
    }
}

fn codegen_function_call_arguments<'allocator, 'input: 'allocator, 'type_map: 'allocator, 'ty>(
    ast: &FunctionCall<'input, '_>,
    type_inference_result: &'ty TypeInferenceResultContainer,
    lifetime_analyze_results: &LifetimeAnalyzeResults,
    import_element_map: &FxHashMap<EntityID, Spanned<std::string::String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_entity_type_map: &'type_map FxHashMap<EntityID, Type>,
    top_stack_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    current_tree_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    code_builder: &mut CodeBuilder<'allocator>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    context: &TranspileModuleContext,
) -> (
    Vec<String<'allocator>, &'allocator Bump>,
    Vec<(&'ty Type, LifetimeAnalyzeResult), &'allocator Bump>,
) {
    let mut argument_results = Vec::new_in(allocator);
    let mut argument_drop_info = Vec::new_in(allocator);

    if let Ok(arguments) = &ast.arg_exprs {
        for argument_expr in arguments.iter() {
            let expr_result_bind_var = create_temp_var(
                code_builder,
                argument_expr.get_span(),
                EntityID::from(*argument_expr),
            );

            codegen_expression(
                *argument_expr,
                Some(expr_result_bind_var.as_str()),
                false,
                type_inference_result,
                lifetime_analyze_results,
                import_element_map,
                name_resolved_map,
                module_entity_type_map,
                top_stack_alloc_builder,
                current_tree_alloc_builder,
                code_builder,
                allocator,
                errors,
                context,
            );

            argument_results.push(expr_result_bind_var);

            let ty = type_inference_result.get_entity_type(EntityID::from(*argument_expr));
            let lifetime_analyze_result =
                lifetime_analyze_results.get_result(EntityID::from(*argument_expr));

            argument_drop_info.push((ty, lifetime_analyze_result));
        }
    }

    (argument_results, argument_drop_info)
}

fn codegen_primary_left<'allocator, 'input: 'allocator, 'type_map: 'allocator>(
    ast: &PrimaryLeft<'input, '_>,
    result_bind_var_name: &str,
    as_assign_left: bool,
    type_inference_result: &TypeInferenceResultContainer,
    lifetime_analyze_results: &LifetimeAnalyzeResults,
    import_element_map: &FxHashMap<EntityID, Spanned<std::string::String>>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_entity_type_map: &'type_map FxHashMap<EntityID, Type>,
    top_stack_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    current_tree_alloc_builder: &mut StackAllocCodeBuilder<'allocator>,
    code_builder: &mut CodeBuilder<'allocator>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    context: &TranspileModuleContext,
) {
    match &ast.first_expr {
        PrimaryLeftExpr::Simple((simple_primary, _, function_call)) => {
            let simple_primary_result_temp = create_temp_var(
                code_builder,
                simple_primary.get_span(),
                EntityID::from(simple_primary),
            );

            match simple_primary {
                SimplePrimary::Expressions {
                    expressions,
                    error_tokens: _,
                    span: _,
                } => {
                    let mut expression_results = Vec::new_in(allocator);

                    for expression in expressions.iter() {
                        let expression_result_temp = create_temp_var(
                            code_builder,
                            expression.get_span(),
                            EntityID::from(*expression),
                        );

                        codegen_expression(
                            *expression,
                            Some(expression_result_temp.as_str()),
                            false,
                            type_inference_result,
                            lifetime_analyze_results,
                            import_element_map,
                            name_resolved_map,
                            module_entity_type_map,
                            top_stack_alloc_builder,
                            current_tree_alloc_builder,
                            code_builder,
                            allocator,
                            errors,
                            context,
                        );

                        expression_results.push(expression_result_temp);
                    }

                    let code = format!(
                        in allocator,
                        "{} = ({});",
                        &simple_primary_result_temp,
                        expression_results.join(", "),
                    );
                    code_builder.add_line(code);
                }
                SimplePrimary::Identifier(literal) => {
                    if let Some(resolved) = name_resolved_map.get(&EntityID::from(literal)) {
                        let is_static_element = resolved.define_info.is_static_element
                            || resolved.define_info.define_kind == DefineKind::Import;

                        let has_ref_count = if let Type::Function {
                            function_info,
                            generics: _,
                        } =
                            type_inference_result.get_entity_type(EntityID::from(&ast.first_expr))
                        {
                            if function_info.define_info.is_closure {
                                true && is_static_element
                            } else {
                                false
                            }
                        } else {
                            is_static_element
                        };

                        if has_ref_count {
                            let code = format!(
                                in allocator,
                                "{} = {}.get().hold();",
                                &simple_primary_result_temp,
                                literal.value,
                            );
                            code_builder.add_line(code);
                        } else {
                            let contains_static = lifetime_analyze_results
                                .get_result(EntityID::from(literal))
                                .contains_static;

                            let clone_function_name = if contains_static {
                                "clone"
                            } else {
                                "clone_non_mutex"
                            };

                            let code = format!(
                                in allocator,
                                "{} = {}.{}();",
                                &simple_primary_result_temp,
                                literal.value,
                                clone_function_name,
                            );
                            code_builder.add_line(code);
                        }
                    } else {
                        if let Some(module_name) = context
                            .context
                            .auto_import
                            .auto_import_elements
                            .get(literal.value)
                        {
                            let module_name = if module_name.starts_with("std::") {
                                module_name.replace("std::", "catla_std::")
                            } else {
                                module_name.clone()
                            };

                            let code = format!(
                                in allocator,
                                "{} = {}::{};",
                                &simple_primary_result_temp,
                                module_name,
                                literal.value,
                            );
                            code_builder.add_line(code);
                        } else {
                            unreachable!("unresolved literal!");
                        }
                    }
                }
                SimplePrimary::StringLiteral(literal) => {
                    const STRING_TYPE: &str = "catla_transpile_std::rust_codegen::string::String";
                    const LAZY_LOCK_TYPE: &str = "std::sync::LazyLock";

                    let code = format!(
                        in allocator,
                        "static STRING_{}_{}: {}<&'static CatlaRefObject<{}>> = {}::new(|| {{ let str = {}::from_str({}); str.to_mutex(); str }});",
                        literal.span.start,
                        literal.span.end,
                        LAZY_LOCK_TYPE,
                        STRING_TYPE,
                        LAZY_LOCK_TYPE,
                        STRING_TYPE,
                        literal.value,
                    );
                    code_builder.add_line(code);

                    let code = format!(
                        in allocator,
                        "let str = &*STRING_{}_{}; str.clone_ref(); {} = str.hold();",
                        literal.span.start,
                        literal.span.end,
                        &simple_primary_result_temp,
                    );
                    code_builder.add_line(code);
                }
                SimplePrimary::NullKeyword(_) => {
                    let code = format!(
                        in allocator,
                        "{} = None;",
                        &simple_primary_result_temp,
                    );
                    code_builder.add_line(code);
                }
                SimplePrimary::TrueKeyword(_) => {
                    let code = format!(
                        in allocator,
                        "{} = true;",
                        &simple_primary_result_temp,
                    );
                    code_builder.add_line(code);
                }
                SimplePrimary::FalseKeyword(_) => {
                    let code = format!(
                        in allocator,
                        "{} = false;",
                        &simple_primary_result_temp,
                    );
                    code_builder.add_line(code);
                }
                SimplePrimary::ThisKeyword(_) => {
                    let code = format!(
                        in allocator,
                        "{} = self;",
                        &simple_primary_result_temp,
                    );
                    code_builder.add_line(code);
                }
                SimplePrimary::LargeThisKeyword(_) => unreachable!(),
            }

            if let Some(function_call) = function_call {
                let (argument_results, argument_drop_info) = codegen_function_call_arguments(
                    function_call,
                    type_inference_result,
                    lifetime_analyze_results,
                    import_element_map,
                    name_resolved_map,
                    module_entity_type_map,
                    top_stack_alloc_builder,
                    current_tree_alloc_builder,
                    code_builder,
                    allocator,
                    errors,
                    context,
                );

                let result_temp = create_temp_var(
                    code_builder,
                    function_call.span.clone(),
                    EntityID::from(function_call),
                );

                let mut arguments = String::new_in(allocator);
                for argument_result in argument_results.iter() {
                    arguments += argument_result.as_str();
                    arguments += ".borrow(), ";
                }

                let code = format!(
                    in allocator,
                    "{} = {}({}).hold();",
                    &result_temp,
                    &simple_primary_result_temp,
                    arguments,
                );
                code_builder.add_line(code);

                for (argument_expr_result, (ty, lifetime_analyze_result)) in
                    argument_results.iter().zip(argument_drop_info.iter()).rev()
                {
                    build_drop_holder(
                        argument_expr_result.as_str(),
                        *ty,
                        lifetime_analyze_result.clone(),
                        code_builder,
                    );
                }

                if type_inference_result
                    .get_entity_type(EntityID::from(simple_primary))
                    .is_closure()
                {
                    build_drop_holder(
                        simple_primary_result_temp.as_str(),
                        type_inference_result.get_entity_type(EntityID::from(simple_primary)),
                        lifetime_analyze_results.get_result(EntityID::from(simple_primary)),
                        code_builder,
                    );
                }

                let code = format!(
                    in allocator,
                    "{} = {};",
                    result_bind_var_name,
                    &result_temp
                );
                code_builder.add_line(code);
            } else {
                let code = format!(
                    in allocator,
                    "{} = {};",
                    result_bind_var_name,
                    &simple_primary_result_temp
                );
                code_builder.add_line(code);
            }
        }
        PrimaryLeftExpr::NewArrayInitExpression(new_array_init_expression) => {}
        PrimaryLeftExpr::NewArrayExpression(new_array_expression) => {}
        PrimaryLeftExpr::NewExpression(new_expression) => {
            if let Ok(assignment) = &new_expression.field_assigns {
                let mut assigns = Vec::new_in(allocator);

                for assignment in assignment.iter() {
                    let temp_var_name = create_temp_var_with_name(
                        code_builder,
                        assignment.name.span.clone(),
                        EntityID::from(assignment),
                        assignment.name.value,
                    );

                    codegen_expression(
                        assignment.expression.as_ref().unwrap(),
                        Some(temp_var_name.as_str()),
                        false,
                        type_inference_result,
                        lifetime_analyze_results,
                        import_element_map,
                        name_resolved_map,
                        module_entity_type_map,
                        top_stack_alloc_builder,
                        current_tree_alloc_builder,
                        code_builder,
                        allocator,
                        errors,
                        context,
                    );

                    assigns.push((assignment.name.value, temp_var_name));
                }

                let new_temp_var = create_temp_var_with_name(
                    code_builder,
                    new_expression.span.clone(),
                    EntityID::from(new_expression),
                    "new",
                );

                let mut code = String::new_in(allocator);

                code += new_temp_var.as_str();
                code += " = ";

                code += new_expression
                    .path
                    .iter()
                    .map(|path| String::from_str_in(path.value, allocator))
                    .collect::<Vec<_>>()
                    .join("::")
                    .as_str();

                code += " { ";

                for (name, result) in assigns {
                    code += format!(
                        in allocator,
                        "{}: std::cell::UnsafeCell::new({}),",
                        name,
                        result,
                    )
                    .as_str();
                }

                code += " };";

                code_builder.add_line(code);
            }
        }
        PrimaryLeftExpr::IfExpression(if_expression) => {}
        PrimaryLeftExpr::LoopExpression(loop_expression) => {}
    }
}
