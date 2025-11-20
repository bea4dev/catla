use catla_parser::ast::{
    AddOrSubExpression, AndExpression, BaseTypeInfo, ClosureArgumentsOrLiteral, Define,
    ElementsOrWildCard, ElseChain, EqualsExpression, Expression, Factor,
    FunctionArgumentOrVariableBinding, FunctionCall, GenericsInfo, ImportStatement,
    LessOrGreaterExpression, MappingOperator, MulOrDivExpression, OrExpression, Primary,
    PrimaryLeft, PrimaryLeftExpr, PrimaryRight, PrimarySeparator, Program, SimplePrimary,
    Statement, TypeAttribute, TypeInfo, TypeInfoBase, WhereClause,
};

use crate::resource::{PackageResource, PackageResourceSet};

pub fn collect_import(ast: &Program, package_resource_set: &PackageResourceSet) -> Vec<String> {
    let mut modules = Vec::new();

    collect_import_for_program(ast, &mut modules, package_resource_set);

    modules
}

fn collect_import_for_program(
    ast: &Program,
    modules: &mut Vec<String>,
    package_resource_set: &PackageResourceSet,
) {
    for statement in ast.statements.iter() {
        match &statement.statement {
            Statement::Assignment(assignment) => {
                collect_import_for_expression(&assignment.left, modules, package_resource_set);
                if let Ok(right) = &assignment.right {
                    collect_import_for_expression(right, modules, package_resource_set);
                }
            }
            Statement::Swap(swap_statement) => {
                collect_import_for_expression(&swap_statement.left, modules, package_resource_set);
                if let Ok(right) = &swap_statement.right {
                    collect_import_for_expression(right, modules, package_resource_set);
                }
            }
            Statement::Import(import_statement) => {
                collect_import_for_import_statement(
                    import_statement,
                    modules,
                    package_resource_set,
                );
            }
            Statement::DefineWithAttribute(define_with_attribute) => {
                if let Ok(define) = &define_with_attribute.define {
                    match define {
                        Define::Function(function_define) => {
                            if let Ok(arguments) = &function_define.arguments {
                                for argument in arguments.arguments.iter() {
                                    if let Ok(type_info) = &argument.type_tag.type_info {
                                        collect_import_for_type_info(
                                            type_info,
                                            modules,
                                            package_resource_set,
                                        );
                                    }
                                }
                            }

                            if let Some(return_type) = &function_define.return_type {
                                if let Ok(type_info) = &return_type.type_info {
                                    collect_import_for_type_info(
                                        type_info,
                                        modules,
                                        package_resource_set,
                                    );
                                }
                            }

                            if let Some(where_clause) = &function_define.where_clause {
                                collect_import_for_where_clause(
                                    where_clause,
                                    modules,
                                    package_resource_set,
                                );
                            }

                            if let Some(block) = &function_define.block {
                                collect_import_for_program(
                                    block.program,
                                    modules,
                                    package_resource_set,
                                );
                            }
                        }
                        Define::UserType(user_type_define) => {
                            if let Some(super_types) = &user_type_define.super_type {
                                for ty in super_types.types.iter() {
                                    collect_import_for_type_info(ty, modules, package_resource_set);
                                }
                            }

                            if let Some(where_clause) = &user_type_define.where_clause {
                                collect_import_for_where_clause(
                                    where_clause,
                                    modules,
                                    package_resource_set,
                                );
                            }

                            if let Ok(block) = &user_type_define.block {
                                collect_import_for_program(
                                    block.program,
                                    modules,
                                    package_resource_set,
                                );
                            }
                        }
                        Define::Variable(variable_define) => {
                            if let Some(type_tag) = &variable_define.type_tag {
                                if let Ok(type_info) = &type_tag.type_info {
                                    collect_import_for_type_info(
                                        type_info,
                                        modules,
                                        package_resource_set,
                                    );
                                }
                            }

                            if let Some(expression) = &variable_define.expression {
                                collect_import_for_expression(
                                    expression,
                                    modules,
                                    package_resource_set,
                                );
                            }
                        }
                        Define::TypeAlias(type_alias) => {
                            if let Ok(type_info) = &type_alias.alias_type {
                                collect_import_for_type_info(
                                    type_info,
                                    modules,
                                    package_resource_set,
                                );
                            }
                        }
                    }
                }
            }
            Statement::Drop(drop_statement) => {
                if let Ok(expression) = &drop_statement.expression {
                    collect_import_for_expression(expression, modules, package_resource_set);
                }
            }
            Statement::Expression(expression) => {
                collect_import_for_expression(expression, modules, package_resource_set)
            }
            Statement::Implements(implements) => {
                if let Ok(interface) = &implements.interface {
                    collect_import_for_type_info(interface, modules, package_resource_set);
                }
                if let Ok(concrete) = &implements.concrete {
                    collect_import_for_type_info(concrete, modules, package_resource_set);
                }
                if let Some(where_clause) = &implements.where_clause {
                    collect_import_for_where_clause(where_clause, modules, package_resource_set);
                }
                if let Ok(block) = &implements.block {
                    collect_import_for_program(block.program, modules, package_resource_set);
                }
            }
        }
    }
}

fn collect_import_for_where_clause(
    ast: &WhereClause,
    modules: &mut Vec<String>,
    package_resource_set: &PackageResourceSet,
) {
    for element in ast.elements.iter() {
        collect_import_for_type_info(&element.target_type, modules, package_resource_set);

        for bound in element.bounds.iter() {
            collect_import_for_type_info(bound, modules, package_resource_set);
        }
    }
}

fn collect_import_for_import_statement(
    ast: &ImportStatement,
    modules: &mut Vec<String>,
    package_resource_set: &PackageResourceSet,
) {
    if ast.path.is_empty() {
        return;
    }

    let path = ast
        .path
        .iter()
        .map(|path| path.value.to_string())
        .collect::<Vec<_>>();
    let path_string = path.join("::");

    match package_resource_set.get(&path_string) {
        Some(resource) => {
            modules.push(path_string.clone());

            match resource {
                PackageResource::Package => match &ast.elements_or_wild_card {
                    Some(element_or_wild_card) => match element_or_wild_card {
                        ElementsOrWildCard::Elements(elements) => {
                            for element in elements.iter() {
                                let mut module_name = path_string.clone();
                                module_name += "::";
                                module_name += element.value;

                                if let None = package_resource_set.get(&module_name) {
                                    break;
                                }

                                modules.push(module_name);
                            }
                        }
                        ElementsOrWildCard::WildCard(_) => {}
                    },
                    None => {}
                },
                PackageResource::Module { source_code: _ } => match &ast.elements_or_wild_card {
                    Some(element_or_wild_card) => match element_or_wild_card {
                        ElementsOrWildCard::Elements(elements) => {
                            for element in elements.iter() {
                                let mut module_name = path_string.clone();
                                module_name += "::";
                                module_name += element.value;

                                if let None = package_resource_set.get(&module_name) {
                                    break;
                                }

                                modules.push(module_name);
                            }
                        }
                        ElementsOrWildCard::WildCard(_) => {}
                    },
                    None => {}
                },
            }
        }
        None => {
            let path = ast.path[..ast.path.len() - 1]
                .iter()
                .map(|path| path.value.to_string())
                .collect::<Vec<_>>();
            let path_string = path.join("::");

            if let Some(_) = package_resource_set.get(&path_string) {
                modules.push(path_string);
            }
        }
    }
}

fn collect_import_for_expression(
    ast: &Expression,
    modules: &mut Vec<String>,
    package_resource_set: &PackageResourceSet,
) {
    match ast {
        Expression::Return(return_expression) => {
            if let Some(expression) = &return_expression.expression {
                collect_import_for_expression(expression, modules, package_resource_set);
            }
        }
        Expression::Closure(closure) => {
            if let ClosureArgumentsOrLiteral::ClosureArguments(closure_arguments) =
                &closure.arguments
            {
                for argument in closure_arguments.arguments.iter() {
                    if let FunctionArgumentOrVariableBinding::FunctionArgument(function_argument) =
                        argument
                    {
                        if let Ok(type_info) = &function_argument.type_tag.type_info {
                            collect_import_for_type_info(type_info, modules, package_resource_set);
                        }
                    }
                }
            }
        }
        Expression::Or(or_expression) => {
            collect_import_for_or_expression(*or_expression, modules, package_resource_set);
        }
    }
}

fn collect_import_for_type_info(
    ast: &TypeInfo,
    modules: &mut Vec<String>,
    package_resource_set: &PackageResourceSet,
) {
    match &ast.base {
        TypeInfoBase::Array(array_type_info) => {
            if let Ok(base_type) = &array_type_info.base_type {
                collect_import_for_type_info(*base_type, modules, package_resource_set);
            }
        }
        TypeInfoBase::Base(base_type_info) => {
            collect_import_for_base_type_info(base_type_info, modules, package_resource_set);
        }
        TypeInfoBase::Tuple(tuple_type_info) => {
            for type_info in tuple_type_info.types.iter() {
                collect_import_for_type_info(type_info, modules, package_resource_set);
            }
        }
        TypeInfoBase::This(_) => {}
    }

    for attribute in ast.attributes.iter() {
        if let TypeAttribute::Result { generics, span: _ } = attribute {
            if let Some(generics) = generics {
                collect_import_for_generics(generics, modules, package_resource_set);
            }
        }
    }
}

fn collect_import_for_generics(
    ast: &GenericsInfo,
    modules: &mut Vec<String>,
    package_resource_set: &PackageResourceSet,
) {
    for type_info in ast.types.iter() {
        collect_import_for_type_info(type_info, modules, package_resource_set);
    }
}

fn collect_import_for_base_type_info(
    ast: &BaseTypeInfo,
    modules: &mut Vec<String>,
    package_resource_set: &PackageResourceSet,
) {
    match ast.path.len() {
        0 => return,
        1 => return,
        _ => {
            let module_name = ast.path[..ast.path.len() - 1]
                .iter()
                .map(|path| path.value.to_string())
                .collect::<Vec<_>>()
                .join("::");

            match package_resource_set.get(&module_name) {
                Some(_) => modules.push(module_name),
                None => {}
            }
        }
    }
}

macro_rules! collect_import_for_2op {
    ($name:ident, $ast:ty, $next:ident) => {
        fn $name(ast: &$ast, modules: &mut Vec<String>, package_resource_set: &PackageResourceSet) {
            $next(&ast.left, modules, package_resource_set);
            for chain in ast.chain.iter() {
                $next(chain, modules, package_resource_set);
            }
        }
    };
    (op, $name:ident, $ast:ty, $next:ident) => {
        fn $name(ast: &$ast, modules: &mut Vec<String>, package_resource_set: &PackageResourceSet) {
            $next(&ast.left, modules, package_resource_set);
            for chain in ast.chain.iter() {
                $next(&chain.1, modules, package_resource_set);
            }
        }
    };
}

collect_import_for_2op!(
    collect_import_for_or_expression,
    OrExpression,
    collect_import_for_and_expression
);
collect_import_for_2op!(
    collect_import_for_and_expression,
    AndExpression,
    collect_import_for_equals_expression
);
collect_import_for_2op!(
    op,
    collect_import_for_equals_expression,
    EqualsExpression,
    collect_import_for_less_or_greater_expression
);
collect_import_for_2op!(
    op,
    collect_import_for_less_or_greater_expression,
    LessOrGreaterExpression,
    collect_import_for_add_or_sub_expression
);
collect_import_for_2op!(
    op,
    collect_import_for_add_or_sub_expression,
    AddOrSubExpression,
    collect_import_for_mul_or_div_expression
);
collect_import_for_2op!(
    op,
    collect_import_for_mul_or_div_expression,
    MulOrDivExpression,
    collect_import_for_factor
);

fn collect_import_for_factor(
    ast: &Factor,
    modules: &mut Vec<String>,
    package_resource_set: &PackageResourceSet,
) {
    if let Ok(primary) = &ast.primary {
        collect_import_for_primary(primary, modules, package_resource_set);
    }
}

fn collect_import_for_primary(
    ast: &Primary,
    modules: &mut Vec<String>,
    package_resource_set: &PackageResourceSet,
) {
    if let PrimaryLeftExpr::Simple {
        left,
        generics: _,
        function_call: _,
        span: _,
    } = &ast.left.first
    {
        if let SimplePrimary::Literal(literal) = left {
            let mut module_name = literal.value.to_string();

            if let Some(_) = package_resource_set.get(&module_name) {
                modules.push(module_name.clone());

                for chain in ast.chain.iter() {
                    if chain.separator.value != PrimarySeparator::DoubleColon {
                        break;
                    }

                    match &chain.second {
                        Some(expr) => {
                            module_name += "::";
                            module_name += expr.literal.value;

                            match package_resource_set.get(&module_name) {
                                Some(_) => modules.push(module_name.clone()),
                                None => break,
                            }
                        }
                        None => break,
                    }
                }
            }
        }
    }

    collect_import_for_primary_left(&ast.left, modules, package_resource_set);
    for chain in ast.chain.iter() {
        collect_import_for_primary_right(chain, modules, package_resource_set);
    }
}

fn collect_import_for_primary_left(
    ast: &PrimaryLeft,
    modules: &mut Vec<String>,
    package_resource_set: &PackageResourceSet,
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
                        collect_import_for_expression(expression, modules, package_resource_set);
                    }
                }
                _ => {}
            }

            if let Some(function_call) = function_call {
                collect_import_for_function_call(function_call, modules, package_resource_set);
            }

            if let Some(generics_info) = generics {
                collect_import_for_generics(generics_info, modules, package_resource_set);
            }
        }
        PrimaryLeftExpr::NewObject { new_object } => match new_object.path.len() {
            0 => {}
            1 => {}
            _ => {
                let module_name = new_object.path[..new_object.path.len() - 1]
                    .iter()
                    .map(|path| path.value.to_string())
                    .collect::<Vec<_>>()
                    .join("::");

                match package_resource_set.get(&module_name) {
                    Some(_) => {
                        modules.push(module_name);
                    }
                    None => {}
                }
            }
        },
        PrimaryLeftExpr::NewArray { new_array } => {
            for expression in new_array.elements.iter() {
                collect_import_for_expression(expression, modules, package_resource_set);
            }
        }
        PrimaryLeftExpr::NewArrayInit { new_array_init } => {
            if let Ok(init_expression) = &new_array_init.init_expression {
                collect_import_for_expression(init_expression, modules, package_resource_set);
            }
            if let Ok(length_expression) = &new_array_init.length_expression {
                collect_import_for_expression(length_expression, modules, package_resource_set);
            }
        }
        PrimaryLeftExpr::If { if_expression } => {
            if let Ok(condition) = &if_expression.first.condition {
                collect_import_for_expression(condition, modules, package_resource_set);
            }
            if let Ok(block) = &if_expression.first.block {
                collect_import_for_program(block.program, modules, package_resource_set);
            }

            for chain in if_expression.chain.iter() {
                match chain {
                    ElseChain::ElseIf { if_statement } => {
                        if let Ok(condition) = &if_statement.condition {
                            collect_import_for_expression(condition, modules, package_resource_set);
                        }
                        if let Ok(block) = &if_statement.block {
                            collect_import_for_program(
                                block.program,
                                modules,
                                package_resource_set,
                            );
                        }
                    }
                    ElseChain::Else { block } => {
                        collect_import_for_program(block.program, modules, package_resource_set);
                    }
                }
            }
        }
        PrimaryLeftExpr::Loop { loop_expression } => {
            if let Ok(block) = &loop_expression.block {
                collect_import_for_program(block.program, modules, package_resource_set);
            }
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_import_for_mapping_operator(mapping_operator, modules, package_resource_set);
    }
}

fn collect_import_for_primary_right(
    ast: &PrimaryRight,
    modules: &mut Vec<String>,
    package_resource_set: &PackageResourceSet,
) {
    if let Some(second) = &ast.second {
        if let Some(function_call) = &second.function_call {
            collect_import_for_function_call(function_call, modules, package_resource_set);
        }
        if let Some(generics) = &second.generics {
            collect_import_for_generics(generics, modules, package_resource_set);
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_import_for_mapping_operator(mapping_operator, modules, package_resource_set);
    }
}

fn collect_import_for_function_call(
    ast: &FunctionCall,
    modules: &mut Vec<String>,
    package_resource_set: &PackageResourceSet,
) {
    for argument in ast.arguments.iter() {
        collect_import_for_expression(argument, modules, package_resource_set);
    }
}

fn collect_import_for_mapping_operator(
    ast: &MappingOperator,
    modules: &mut Vec<String>,
    package_resource_set: &PackageResourceSet,
) {
    let block = match ast {
        MappingOperator::NullElvis { block, span: _ } => block,
        MappingOperator::ResultElvis { block, span: _ } => block,
        _ => return,
    };

    let Ok(block) = block else {
        return;
    };

    collect_import_for_program(block.program, modules, package_resource_set);
}
