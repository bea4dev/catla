use catla_parser::ast::{
    AddOrSubExpression, AndExpression, Define, ElementsOrWildCard, EntityID, EqualsExpression,
    Expression, Factor, GenericsDefine, ImportStatement, LessOrGreaterExpression,
    MulOrDivExpression, OrExpression, Primary, PrimaryLeftExpr, Program, SimplePrimary, Spanned,
    Statement, TypeAttribute, TypeInfo, TypeInfoBase, VariableBinding, WhereClause,
};
use catla_type::types::Type;
use hashbrown::HashMap;

use crate::CodeBuilderScope;

pub(crate) fn codegen_for_program(
    ast: &Program,
    in_impl_scope: bool,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    builder: &CodeBuilderScope,
) {
    for statement in ast.statements.iter() {
        match &statement.statement {
            Statement::Assignment(assignment) => todo!(),
            Statement::Swap(swap_statement) => todo!(),
            Statement::Import(import_statement) => {
                builder.push_line(codegen_import(import_statement).as_str());
            }
            Statement::DefineWithAttribute(define_with_attribute) => {
                if let Ok(define) = &define_with_attribute.define {
                    match define {
                        Define::Function(function_define) => {
                            let public = match in_impl_scope {
                                true => "",
                                false => "pub ",
                            };

                            let mut arguments_str = String::new();
                            arguments_str += "(";

                            if let Ok(arguments) = &function_define.arguments {
                                if let Some(_) = &arguments.this_mutability {
                                    arguments_str += "&self, ";
                                }

                                arguments_str += arguments
                                    .arguments
                                    .iter()
                                    .map(|argument| {
                                        format!(
                                            "{}: {}",
                                            codegen_for_variable_binding(&argument.binding),
                                            argument
                                                .type_tag
                                                .type_info
                                                .as_ref()
                                                .map(|ty| codegen_for_type(ty))
                                                .unwrap_or_default()
                                        )
                                    })
                                    .collect::<Vec<_>>()
                                    .join(", ")
                                    .as_str();
                            }

                            arguments_str += ")";

                            builder.push_line(
                                format!(
                                    "{}fn {}{} {{",
                                    public,
                                    function_define
                                        .name
                                        .as_ref()
                                        .map(|name| name.value)
                                        .unwrap_or_default(),
                                    arguments_str,
                                )
                                .as_str(),
                            );

                            {
                                let scope = builder.scope();

                                if statement
                                    .compiler_tags
                                    .iter()
                                    .any(|tag| tag.literal.value == "compiler_support_unreachable")
                                {
                                    scope.push_line("unreachable!()");
                                } else {
                                    if let Some(block) = &function_define.block {
                                        codegen_for_program(
                                            block.program,
                                            false,
                                            type_infer_results,
                                            &scope,
                                        );
                                    }
                                }
                            }

                            builder.push_line("}");
                        }
                        Define::UserType(user_type_define) => {}
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
                                        .map(|ty| format!(": {}", codegen_for_type(ty)))
                                        .unwrap_or_default(),
                                    variable_define
                                        .expression
                                        .as_ref()
                                        .map(|expression| format!(
                                            " = {}",
                                            codegen_expression(
                                                expression,
                                                type_infer_results,
                                                builder
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
                        codegen_expression(expression, &type_infer_results, builder).as_str()
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
                            .map(|generics| codegen_for_generics_define(generics))
                            .unwrap_or_default(),
                        implements
                            .interface
                            .as_ref()
                            .map(|ty| codegen_for_type(ty))
                            .unwrap_or_default(),
                        implements
                            .concrete
                            .as_ref()
                            .map(|ty| codegen_for_type(ty))
                            .unwrap_or_default(),
                        implements
                            .where_clause
                            .as_ref()
                            .map(|where_clause| codegen_for_where_clause(where_clause))
                            .unwrap_or_default(),
                    )
                    .as_str(),
                );

                {
                    let scope = builder.scope();

                    if let Ok(block) = &implements.block {
                        codegen_for_program(block.program, true, type_infer_results, &scope);
                    }
                }

                builder.push_line("}");
            }
        }
    }
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

fn codegen_for_generics_define(ast: &GenericsDefine) -> String {
    let mut code = String::new();
    code += "<";

    code += ast
        .elements
        .iter()
        .map(|element| {
            format!(
                "{}: {}",
                element.name.value,
                element
                    .bounds
                    .iter()
                    .map(|bound| codegen_for_type(bound))
                    .collect::<Vec<_>>()
                    .join(" + ")
            )
        })
        .collect::<Vec<_>>()
        .join(", ")
        .as_str();

    code += ">";

    code
}

fn codegen_for_where_clause(ast: &WhereClause) -> String {
    let mut code = String::new();
    code += "where ";

    for element in ast.elements.iter() {
        code += codegen_for_type(&element.target_type).as_str();
        code += ": ";

        code += element
            .bounds
            .iter()
            .map(|bound| codegen_for_type(bound))
            .collect::<Vec<_>>()
            .join(" + ")
            .as_str();
    }

    code
}

fn codegen_for_type(ast: &TypeInfo) -> String {
    let mut code = String::new();

    match &ast.base {
        TypeInfoBase::Array(array_type_info) => todo!(),
        TypeInfoBase::Base(base_type_info) => {
            code += base_type_info
                .path
                .iter()
                .enumerate()
                .map(|(index, path)| {
                    if index == 0 && path.value == "std" {
                        "catla_std".to_string()
                    } else {
                        path.value.to_string()
                    }
                })
                .collect::<Vec<_>>()
                .join("::")
                .as_str();
            if let Some(generics) = &base_type_info.generics {
                code += "<";
                code += generics
                    .types
                    .iter()
                    .map(|generic| codegen_for_type(generic))
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

fn codegen_import(ast: &ImportStatement) -> String {
    let mut code = String::new();
    code += "use ";

    let mut path_temp = Vec::new();
    for (index, path) in ast.path.iter().enumerate() {
        if index == 0 && path.value == "std" {
            path_temp.push("catla_std");
        } else {
            path_temp.push(path.value);
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
) -> String {
    match ast {
        Expression::Return(return_expression) => todo!(),
        Expression::Closure(closure) => todo!(),
        Expression::Or(or_expression) => {
            codegen_or_expression(*or_expression, type_infer_results, builder)
        }
    }
}

fn codegen_or_expression(
    ast: &OrExpression,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    builder: &CodeBuilderScope,
) -> String {
    let left_result = codegen_and_expression(&ast.left, type_infer_results, builder);

    match ast.chain.len() {
        0 => left_result,
        _ => todo!(),
    }
}

fn codegen_and_expression(
    ast: &AndExpression,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    builder: &CodeBuilderScope,
) -> String {
    let left_result = codegen_equals_expression(&ast.left, type_infer_results, builder);

    match ast.chain.len() {
        0 => left_result,
        _ => todo!(),
    }
}

fn codegen_equals_expression(
    ast: &EqualsExpression,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    builder: &CodeBuilderScope,
) -> String {
    let left_result = codegen_less_or_greater_expression(&ast.left, type_infer_results, builder);

    match ast.chain.len() {
        0 => left_result,
        _ => todo!(),
    }
}

fn codegen_less_or_greater_expression(
    ast: &LessOrGreaterExpression,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    builder: &CodeBuilderScope,
) -> String {
    let left_result = codegen_add_or_sub_expression(&ast.left, type_infer_results, builder);

    match ast.chain.len() {
        0 => left_result,
        _ => todo!(),
    }
}

fn codegen_add_or_sub_expression(
    ast: &AddOrSubExpression,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    builder: &CodeBuilderScope,
) -> String {
    let left_result = codegen_mul_or_div_expression(&ast.left, type_infer_results, builder);

    match ast.chain.len() {
        0 => left_result,
        _ => {
            let left_type = type_infer_results.get(&EntityID::from(&ast.left)).unwrap();

            if left_type.value.is_integer() || left_type.value.is_float() {
                
            }
        },
    }
}

fn codegen_mul_or_div_expression(
    ast: &MulOrDivExpression,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    builder: &CodeBuilderScope,
) -> String {
    let left_result = codegen_for_factor(&ast.left, type_infer_results, builder);

    match ast.chain.len() {
        0 => left_result,
        _ => todo!(),
    }
}

fn codegen_for_factor(
    ast: &Factor,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    builder: &CodeBuilderScope,
) -> String {
    if let Ok(primary) = &ast.primary {
        return codegen_for_primary(primary, type_infer_results, builder);
    }
    String::new()
}

fn codegen_for_primary(
    ast: &Primary,
    type_infer_results: &HashMap<EntityID, Spanned<Type>>,
    builder: &CodeBuilderScope,
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
                SimplePrimary::Literal(literal) => code += literal.value,
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
                            builder
                        ))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
                .as_str();
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
