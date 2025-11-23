use catla_parser::ast::{
    AddOrSubExpression, AndExpression, Define, ElementsOrWildCard, EqualsExpression, Expression,
    Factor, ImportStatement, LessOrGreaterExpression, MulOrDivExpression, OrExpression, Primary,
    PrimaryLeftExpr, Program, SimplePrimary, Statement, TypeAttribute, TypeInfo, TypeInfoBase,
    VariableBinding,
};

use crate::CodeBuilderScope;

pub(crate) fn codegen_for_program(ast: &Program, in_impl_scope: bool, builder: &CodeBuilderScope) {
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
                                true => "pub ",
                                false => "",
                            };

                            let mut arguments_str = String::new();
                            arguments_str += "(";

                            if let Ok(arguments) = &function_define.arguments {
                                arguments_str += arguments
                                    .arguments
                                    .iter()
                                    .map(|argument| {
                                        format!(
                                            "{}: {}",
                                            codegen_for_variable_binding(&argument.binding),
                                            codegen_for_type(
                                                argument.type_tag.type_info.as_ref().unwrap()
                                            )
                                        )
                                    })
                                    .collect::<Vec<_>>()
                                    .join(", ")
                                    .as_str();
                            }

                            arguments_str += ")";

                            builder.push_line(
                                format!(
                                    "{}fn {}",
                                    public,
                                    function_define
                                        .name
                                        .as_ref()
                                        .map(|name| name.value)
                                        .unwrap_or_default()
                                )
                                .as_str(),
                            );
                        }
                        Define::UserType(user_type_define) => {}
                        Define::Variable(variable_define) => todo!(),
                        Define::TypeAlias(type_alias) => todo!(),
                    }
                }
            }
            Statement::Drop(drop_statement) => todo!(),
            Statement::Expression(expression) => {
                builder.push_raw(codegen_expression(expression, &builder).as_str());
                builder.push_line(";");
            }
            Statement::Implements(implements) => todo!(),
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

fn codegen_for_type(ast: &TypeInfo) -> String {
    let mut code = String::new();

    match &ast.base {
        TypeInfoBase::Array(array_type_info) => todo!(),
        TypeInfoBase::Base(base_type_info) => {
            code += base_type_info
                .path
                .iter()
                .map(|path| path.value.to_string())
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
        TypeInfoBase::This(spanned) => todo!(),
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

    code
}

fn codegen_expression(ast: &Expression, builder: &CodeBuilderScope) -> String {
    match ast {
        Expression::Return(return_expression) => todo!(),
        Expression::Closure(closure) => todo!(),
        Expression::Or(or_expression) => todo!(),
    }
}

fn codegen_or_expression(ast: &OrExpression, builder: &CodeBuilderScope) -> String {
    let left_result = codegen_and_expression(&ast.left, builder);

    match ast.chain.len() {
        0 => left_result,
        _ => todo!(),
    }
}

fn codegen_and_expression(ast: &AndExpression, builder: &CodeBuilderScope) -> String {
    let left_result = codegen_equals_expression(&ast.left, builder);

    match ast.chain.len() {
        0 => left_result,
        _ => todo!(),
    }
}

fn codegen_equals_expression(ast: &EqualsExpression, builder: &CodeBuilderScope) -> String {
    let left_result = codegen_less_or_greater_expression(&ast.left, builder);

    match ast.chain.len() {
        0 => left_result,
        _ => todo!(),
    }
}

fn codegen_less_or_greater_expression(
    ast: &LessOrGreaterExpression,
    builder: &CodeBuilderScope,
) -> String {
    let left_result = codegen_add_or_sub_expression(&ast.left, builder);

    match ast.chain.len() {
        0 => left_result,
        _ => todo!(),
    }
}

fn codegen_add_or_sub_expression(ast: &AddOrSubExpression, builder: &CodeBuilderScope) -> String {
    let left_result = codegen_mul_or_div_expression(&ast.left, builder);

    match ast.chain.len() {
        0 => left_result,
        _ => todo!(),
    }
}

fn codegen_mul_or_div_expression(ast: &MulOrDivExpression, builder: &CodeBuilderScope) -> String {
    let left_result = codegen_for_factor(&ast.left, builder);

    match ast.chain.len() {
        0 => left_result,
        _ => todo!(),
    }
}

fn codegen_for_factor(ast: &Factor, builder: &CodeBuilderScope) -> String {
    if let Ok(primary) = &ast.primary {
        return codegen_for_primary(primary, builder);
    }
    String::new()
}

fn codegen_for_primary(ast: &Primary, builder: &CodeBuilderScope) -> String {
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
                SimplePrimary::NumericLiteral(spanned) => todo!(),
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
                        .map(|expression| codegen_expression(expression, builder))
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
