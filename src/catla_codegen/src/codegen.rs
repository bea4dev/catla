use catla_parser::ast::{
    AddOrSub, AddOrSubExpression, AndExpression, Define, ElementsOrWildCard, EntityID,
    EqualsExpression, Expression, Factor, FunctionDefine, GenericsDefine, ImportStatement,
    LessOrGreaterExpression, MulOrDivExpression, OrExpression, Primary, PrimaryLeftExpr,
    PrimarySeparator, Program, SimplePrimary, Spanned, Statement, StatementWithTagAndDocs,
    TypeAttribute, TypeInfo, TypeInfoBase, UserTypeKind, VariableBinding, WhereClause,
};
use catla_type::types::Type;
use hashbrown::HashMap;

use crate::CodeBuilderScope;

pub(crate) fn codegen_for_program(
    ast: &Program,
    in_impl_scope: bool,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
) {
    for statement in ast.statements.iter() {
        match &statement.statement {
            Statement::Assignment(assignment) => todo!(),
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
                                builder,
                                current_crate_name,
                            );
                        }
                        Define::UserType(user_type_define) => {
                            if user_type_define.kind.value != UserTypeKind::Interface {
                                continue;
                            }

                            let visibility = if in_impl_scope { "" } else { "pub " };
                            let name = user_type_define
                                .name
                                .as_ref()
                                .map(|name| name.value)
                                .unwrap_or_default();
                            let generics = user_type_define
                                .generics
                                .as_ref()
                                .map(|generics| {
                                    codegen_for_generics_define(generics, current_crate_name)
                                })
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
                                                .map(|ty| codegen_for_type(ty, current_crate_name))
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
                                        codegen_for_where_clause(where_clause, current_crate_name)
                                    )
                                })
                                .unwrap_or_default();

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
                                        &scope,
                                        current_crate_name,
                                    );
                                }
                            }

                            builder.push_line("}");
                        }
                        Define::Variable(variable_define) => {
                            builder.push_line(
                                format!(
                                    "let {}{}{};",
                                    variable_define
                                        .binding
                                        .as_ref()
                                        .map(|binding| codegen_for_variable_binding(binding))
                                        .unwrap_or_default(),
                                    variable_define
                                        .type_tag
                                        .as_ref()
                                        .map(|type_tag| type_tag.type_info.as_ref().ok())
                                        .flatten()
                                        .map(|ty| format!(
                                            ": {}",
                                            codegen_for_type(ty, current_crate_name)
                                        ))
                                        .unwrap_or_default(),
                                    variable_define
                                        .expression
                                        .as_ref()
                                        .map(|expression| format!(
                                            " = {}",
                                            codegen_expression(
                                                expression,
                                                type_infer_results,
                                                builder,
                                                current_crate_name
                                            )
                                        ))
                                        .unwrap_or_default()
                                )
                                .as_str(),
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
                            builder,
                            current_crate_name
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
                            .map(|ty| codegen_for_type(ty, current_crate_name))
                            .unwrap_or_default(),
                        implements
                            .concrete
                            .as_ref()
                            .map(|ty| codegen_for_type(ty, current_crate_name))
                            .unwrap_or_default(),
                        implements
                            .where_clause
                            .as_ref()
                            .map(|where_clause| {
                                codegen_for_where_clause(where_clause, current_crate_name)
                            })
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
                            &scope,
                            current_crate_name,
                        );
                    }
                }

                builder.push_line("}");
            }
        }
    }
}

fn codegen_for_interface_program(
    ast: &Program,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
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
                    builder,
                    current_crate_name,
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
    builder: &CodeBuilderScope,
    current_crate_name: &str,
) {
    let visibility = if public { "pub " } else { "" };
    let signature = codegen_for_function_signature(function_define, current_crate_name);
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
                &scope,
                current_crate_name,
            );
        }
    }

    builder.push_line("}");
}

fn codegen_for_function_signature(ast: &FunctionDefine, current_crate_name: &str) -> String {
    let name = ast.name.as_ref().map(|name| name.value).unwrap_or_default();
    let generics = ast
        .generics
        .as_ref()
        .map(|generics| codegen_for_generics_define(generics, current_crate_name))
        .unwrap_or_default();
    let arguments = codegen_for_function_arguments(ast, current_crate_name);
    let return_type = ast
        .return_type
        .as_ref()
        .map(|return_type| {
            return_type
                .type_info
                .as_ref()
                .map(|ty| format!(" -> {}", codegen_for_type(ty, current_crate_name)))
                .unwrap_or_default()
        })
        .unwrap_or_default();
    let where_clause = ast
        .where_clause
        .as_ref()
        .map(|where_clause| {
            format!(
                " {}",
                codegen_for_where_clause(where_clause, current_crate_name)
            )
        })
        .unwrap_or_default();

    format!(
        "fn {}{}{}{}{}",
        name, generics, arguments, return_type, where_clause
    )
}

fn codegen_for_function_arguments(ast: &FunctionDefine, current_crate_name: &str) -> String {
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
                    .map(|ty| codegen_for_type(ty, current_crate_name))
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
                        .map(|bound| codegen_for_type(bound, current_crate_name))
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

fn codegen_for_where_clause(ast: &WhereClause, current_crate_name: &str) -> String {
    format!(
        "where {}",
        ast.elements
            .iter()
            .map(|element| {
                format!(
                    "{}: {}",
                    codegen_for_type(&element.target_type, current_crate_name),
                    element
                        .bounds
                        .iter()
                        .map(|bound| codegen_for_type(bound, current_crate_name))
                        .collect::<Vec<_>>()
                        .join(" + ")
                )
            })
            .collect::<Vec<_>>()
            .join(", ")
    )
}

fn codegen_for_type(ast: &TypeInfo, current_crate_name: &str) -> String {
    let mut code = String::new();

    match &ast.base {
        TypeInfoBase::Array(array_type_info) => todo!(),
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
                    .map(|generic| codegen_for_type(generic, current_crate_name))
                    .collect::<Vec<_>>()
                    .join(", ")
                    .as_str();
                code += ">";
            }
        }
        TypeInfoBase::Tuple(tuple_type_info) => todo!(),
        TypeInfoBase::This(_) => code += "Self",
    }

    for attribute in ast.attributes.iter() {
        match attribute {
            TypeAttribute::Optional { span } => todo!(),
            TypeAttribute::Result { generics, span } => todo!(),
        }
    }

    code
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

fn codegen_expression(
    ast: &Expression,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
) -> String {
    match ast {
        Expression::Return(return_expression) => todo!(),
        Expression::Closure(closure) => todo!(),
        Expression::Or(or_expression) => codegen_or_expression(
            *or_expression,
            type_infer_results,
            builder,
            current_crate_name,
        ),
    }
}

fn codegen_or_expression(
    ast: &OrExpression,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
) -> String {
    let left_result =
        codegen_and_expression(&ast.left, type_infer_results, builder, current_crate_name);

    match ast.chain.len() {
        0 => left_result,
        _ => todo!(),
    }
}

fn codegen_and_expression(
    ast: &AndExpression,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
) -> String {
    let left_result =
        codegen_equals_expression(&ast.left, type_infer_results, builder, current_crate_name);

    match ast.chain.len() {
        0 => left_result,
        _ => todo!(),
    }
}

fn codegen_equals_expression(
    ast: &EqualsExpression,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
) -> String {
    let left_result = codegen_less_or_greater_expression(
        &ast.left,
        type_infer_results,
        builder,
        current_crate_name,
    );

    match ast.chain.len() {
        0 => left_result,
        _ => todo!(),
    }
}

fn codegen_less_or_greater_expression(
    ast: &LessOrGreaterExpression,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
) -> String {
    let left_result =
        codegen_add_or_sub_expression(&ast.left, type_infer_results, builder, current_crate_name);

    match ast.chain.len() {
        0 => left_result,
        _ => todo!(),
    }
}

fn codegen_add_or_sub_expression(
    ast: &AddOrSubExpression,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
) -> String {
    let left_result =
        codegen_mul_or_div_expression(&ast.left, type_infer_results, builder, current_crate_name);

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
                        builder,
                        current_crate_name,
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
    builder: &CodeBuilderScope,
    current_crate_name: &str,
) -> String {
    let left_result =
        codegen_for_factor(&ast.left, type_infer_results, builder, current_crate_name);

    match ast.chain.len() {
        0 => left_result,
        _ => todo!(),
    }
}

fn codegen_for_factor(
    ast: &Factor,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
) -> String {
    if let Ok(primary) = &ast.primary {
        return codegen_for_primary(primary, type_infer_results, builder, current_crate_name);
    }
    String::new()
}

fn codegen_for_primary(
    ast: &Primary,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    builder: &CodeBuilderScope,
    current_crate_name: &str,
) -> String {
    match &ast.left.first {
        PrimaryLeftExpr::Simple {
            left,
            generics,
            function_call,
            span,
        } => {
            let mut code = String::new();

            match left {
                SimplePrimary::Tuple { expressions, span } => todo!(),
                SimplePrimary::Literal(literal) => {
                    if ast
                        .chain
                        .first()
                        .map(|right| right.separator.value == PrimarySeparator::DoubleColon)
                        .unwrap_or(false)
                    {
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
                code += format!(
                    "({})",
                    function_call
                        .arguments
                        .iter()
                        .map(|expression| codegen_expression(
                            expression,
                            type_infer_results,
                            builder,
                            current_crate_name
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
                        code += format!(
                            "({})",
                            function_call
                                .arguments
                                .iter()
                                .map(|expression| codegen_expression(
                                    expression,
                                    type_infer_results,
                                    builder,
                                    current_crate_name
                                ))
                                .collect::<Vec<_>>()
                                .join(", ")
                        )
                        .as_str();
                    }
                }
            }

            code
        }
        PrimaryLeftExpr::NewObject { new_object } => todo!(),
        PrimaryLeftExpr::NewArray { new_array } => todo!(),
        PrimaryLeftExpr::NewArrayInit { new_array_init } => todo!(),
        PrimaryLeftExpr::If { if_expression } => todo!(),
        PrimaryLeftExpr::Loop { loop_expression } => todo!(),
    }
}
