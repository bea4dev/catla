use std::sync::Arc;

use catla_parser::ast::{
    AddOrSubExpression, AndExpression, BaseTypeInfo, ClosureArgumentsOrLiteral, Define,
    ElementsOrWildCard, ElseChain, EntityID, EqualsExpression, Expression, Factor,
    FunctionArgumentOrVariableBinding, FunctionCall, GenericsInfo, ImportStatement,
    LessOrGreaterExpression, MappingOperator, MulOrDivExpression, OrExpression, Primary,
    PrimaryLeft, PrimaryLeftExpr, PrimaryRight, Program, SimplePrimary, Statement, TypeAttribute,
    TypeInfo, TypeInfoBase, WhereClause,
};
use catla_util::module_path::ModulePath;
use hashbrown::HashMap;

use crate::{
    ImportElement,
    error::{ImportError, ImportErrorKind},
    resource::{PackageResource, PackageResourceSet},
};

pub fn collect_import_element(
    ast: &Program,
    package_resource_set: &PackageResourceSet,
    module_element_name_map: &HashMap<String, Arc<Vec<String>>>,
    module_path: &ModulePath,
) -> (HashMap<EntityID, Vec<ImportElement>>, Vec<ImportError>) {
    let mut import_elements = HashMap::new();
    let mut errors = Vec::new();

    collect_import_element_for_program(
        ast,
        &mut &mut import_elements,
        package_resource_set,
        module_element_name_map,
        &mut errors,
        module_path,
    );

    (import_elements, errors)
}

fn collect_import_element_for_program(
    ast: &Program,
    import_elements: &mut HashMap<EntityID, Vec<ImportElement>>,
    package_resource_set: &PackageResourceSet,
    module_element_name_map: &HashMap<String, Arc<Vec<String>>>,
    errors: &mut Vec<ImportError>,
    module_path: &ModulePath,
) {
    for statement in ast.statements.iter() {
        match &statement.statement {
            Statement::Assignment(assignment) => {
                collect_import_element_for_expression(
                    &assignment.left,
                    import_elements,
                    package_resource_set,
                    module_element_name_map,
                    errors,
                    module_path,
                );
                if let Ok(right) = &assignment.right {
                    collect_import_element_for_expression(
                        right,
                        import_elements,
                        package_resource_set,
                        module_element_name_map,
                        errors,
                        module_path,
                    );
                }
            }
            Statement::Swap(swap_statement) => {
                collect_import_element_for_expression(
                    &swap_statement.left,
                    import_elements,
                    package_resource_set,
                    module_element_name_map,
                    errors,
                    module_path,
                );
                if let Ok(right) = &swap_statement.right {
                    collect_import_element_for_expression(
                        right,
                        import_elements,
                        package_resource_set,
                        module_element_name_map,
                        errors,
                        module_path,
                    );
                }
            }
            Statement::Import(import_statement) => {
                collect_import_element_for_import_statement(
                    import_statement,
                    import_elements,
                    package_resource_set,
                    module_element_name_map,
                    errors,
                    module_path,
                );
            }
            Statement::DefineWithAttribute(define_with_attribute) => {
                if let Ok(define) = &define_with_attribute.define {
                    match define {
                        Define::Function(function_define) => {
                            if let Ok(arguments) = &function_define.arguments {
                                for argument in arguments.arguments.iter() {
                                    if let Ok(type_info) = &argument.type_tag.type_info {
                                        collect_import_element_for_type_info(
                                            type_info,
                                            import_elements,
                                            package_resource_set,
                                            module_element_name_map,
                                            errors,
                                            module_path,
                                        );
                                    }
                                }
                            }

                            if let Some(return_type) = &function_define.return_type {
                                if let Ok(type_info) = &return_type.type_info {
                                    collect_import_element_for_type_info(
                                        type_info,
                                        import_elements,
                                        package_resource_set,
                                        module_element_name_map,
                                        errors,
                                        module_path,
                                    );
                                }
                            }

                            if let Some(where_clause) = &function_define.where_clause {
                                collect_import_element_for_where_clause(
                                    where_clause,
                                    import_elements,
                                    package_resource_set,
                                    module_element_name_map,
                                    errors,
                                    module_path,
                                );
                            }

                            if let Some(block) = &function_define.block {
                                collect_import_element_for_program(
                                    block.program,
                                    import_elements,
                                    package_resource_set,
                                    module_element_name_map,
                                    errors,
                                    module_path,
                                );
                            }
                        }
                        Define::UserType(user_type_define) => {
                            if let Some(super_types) = &user_type_define.super_type {
                                for ty in super_types.types.iter() {
                                    collect_import_element_for_type_info(
                                        ty,
                                        import_elements,
                                        package_resource_set,
                                        module_element_name_map,
                                        errors,
                                        module_path,
                                    );
                                }
                            }

                            if let Some(where_clause) = &user_type_define.where_clause {
                                collect_import_element_for_where_clause(
                                    where_clause,
                                    import_elements,
                                    package_resource_set,
                                    module_element_name_map,
                                    errors,
                                    module_path,
                                );
                            }

                            if let Ok(block) = &user_type_define.block {
                                collect_import_element_for_program(
                                    block.program,
                                    import_elements,
                                    package_resource_set,
                                    module_element_name_map,
                                    errors,
                                    module_path,
                                );
                            }
                        }
                        Define::Variable(variable_define) => {
                            if let Some(type_tag) = &variable_define.type_tag {
                                if let Ok(type_info) = &type_tag.type_info {
                                    collect_import_element_for_type_info(
                                        type_info,
                                        import_elements,
                                        package_resource_set,
                                        module_element_name_map,
                                        errors,
                                        module_path,
                                    );
                                }
                            }

                            if let Some(expression) = &variable_define.expression {
                                collect_import_element_for_expression(
                                    expression,
                                    import_elements,
                                    package_resource_set,
                                    module_element_name_map,
                                    errors,
                                    module_path,
                                );
                            }
                        }
                        Define::TypeAlias(type_alias) => {
                            if let Ok(type_info) = &type_alias.alias_type {
                                collect_import_element_for_type_info(
                                    type_info,
                                    import_elements,
                                    package_resource_set,
                                    module_element_name_map,
                                    errors,
                                    module_path,
                                );
                            }
                        }
                    }
                }
            }
            Statement::Drop(drop_statement) => {
                if let Ok(expression) = &drop_statement.expression {
                    collect_import_element_for_expression(
                        expression,
                        import_elements,
                        package_resource_set,
                        module_element_name_map,
                        errors,
                        module_path,
                    );
                }
            }
            Statement::Expression(expression) => collect_import_element_for_expression(
                expression,
                import_elements,
                package_resource_set,
                module_element_name_map,
                errors,
                module_path,
            ),
            Statement::Implements(implements) => {
                if let Ok(interface) = &implements.interface {
                    collect_import_element_for_type_info(
                        interface,
                        import_elements,
                        package_resource_set,
                        module_element_name_map,
                        errors,
                        module_path,
                    );
                }
                if let Ok(concrete) = &implements.concrete {
                    collect_import_element_for_type_info(
                        concrete,
                        import_elements,
                        package_resource_set,
                        module_element_name_map,
                        errors,
                        module_path,
                    );
                }
                if let Some(where_clause) = &implements.where_clause {
                    collect_import_element_for_where_clause(
                        where_clause,
                        import_elements,
                        package_resource_set,
                        module_element_name_map,
                        errors,
                        module_path,
                    );
                }
                if let Ok(block) = &implements.block {
                    collect_import_element_for_program(
                        block.program,
                        import_elements,
                        package_resource_set,
                        module_element_name_map,
                        errors,
                        module_path,
                    );
                }
            }
        }
    }
}

fn collect_import_element_for_where_clause(
    ast: &WhereClause,
    import_elements: &mut HashMap<EntityID, Vec<ImportElement>>,
    package_resource_set: &PackageResourceSet,
    module_element_name_map: &HashMap<String, Arc<Vec<String>>>,
    errors: &mut Vec<ImportError>,
    module_path: &ModulePath,
) {
    for element in ast.elements.iter() {
        collect_import_element_for_type_info(
            &element.target_type,
            import_elements,
            package_resource_set,
            module_element_name_map,
            errors,
            module_path,
        );

        for bound in element.bounds.iter() {
            collect_import_element_for_type_info(
                bound,
                import_elements,
                package_resource_set,
                module_element_name_map,
                errors,
                module_path,
            );
        }
    }
}

fn collect_import_element_for_import_statement(
    ast: &ImportStatement,
    import_elements: &mut HashMap<EntityID, Vec<ImportElement>>,
    package_resource_set: &PackageResourceSet,
    module_element_name_map: &HashMap<String, Arc<Vec<String>>>,
    errors: &mut Vec<ImportError>,
    module_path: &ModulePath,
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
        Some(resource) => match resource {
            PackageResource::Package => match &ast.elements_or_wild_card {
                Some(element_or_wild_card) => match element_or_wild_card {
                    ElementsOrWildCard::Elements(elements) => {
                        for element in elements.iter() {
                            let mut module_name = path_string.clone();
                            module_name += "::";
                            module_name += element.value;

                            if let None = package_resource_set.get(&module_name) {
                                let error = ImportError {
                                    kind: ImportErrorKind::ModuleNotFound { module_name },
                                    span: element.span.clone(),
                                    module_path: module_path.clone(),
                                };
                                errors.push(error);
                                continue;
                            }

                            let mut element_path = path.clone();
                            element_path.push(element.value.to_string());
                            import_elements.insert(
                                EntityID::from(element),
                                vec![ImportElement::ModuleAlias { path: element_path }],
                            );
                        }
                    }
                    ElementsOrWildCard::WildCard(wild_card) => {
                        let error = ImportError {
                            kind: ImportErrorKind::CannotImportModuleWithWildCard,
                            span: wild_card.span.clone(),
                            module_path: module_path.clone(),
                        };
                        errors.push(error);
                    }
                },
                None => {
                    if let Some(last) = ast.path.last() {
                        import_elements.insert(
                            EntityID::from(last),
                            vec![ImportElement::ModuleAlias { path }],
                        );
                    }
                }
            },
            PackageResource::Module { source_code: _ } => match &ast.elements_or_wild_card {
                Some(element_or_wild_card) => match element_or_wild_card {
                    ElementsOrWildCard::Elements(elements) => {
                        for element in elements.iter() {
                            if let Some(module_elements) = module_element_name_map.get(&path_string)
                            {
                                let mut as_module_path_string = path_string.clone();
                                as_module_path_string += "::";
                                as_module_path_string += element.value;

                                let mut as_module_path = path.clone();
                                as_module_path.push(element.value.to_string());

                                if module_elements
                                    .iter()
                                    .any(|element_name| element_name == element.value)
                                {
                                    import_elements.insert(
                                        EntityID::from(element),
                                        vec![ImportElement::ModuleElement {
                                            path: path.clone(),
                                            element: element.value.to_string(),
                                        }],
                                    );
                                } else if let Some(_) =
                                    package_resource_set.get(&as_module_path_string)
                                {
                                    import_elements.insert(
                                        EntityID::from(element),
                                        vec![ImportElement::ModuleAlias {
                                            path: as_module_path,
                                        }],
                                    );
                                } else {
                                    let error = ImportError {
                                        kind: ImportErrorKind::NoElementFound {
                                            element_name: element.value.to_string(),
                                        },
                                        span: element.span.clone(),
                                        module_path: module_path.clone(),
                                    };
                                    errors.push(error);
                                    import_elements.insert(
                                        EntityID::from(element),
                                        vec![ImportElement::Unknown],
                                    );
                                }
                            }
                        }
                    }
                    ElementsOrWildCard::WildCard(wild_card) => {
                        if let Some(module_elements) = module_element_name_map.get(&path_string) {
                            import_elements.insert(
                                EntityID::from(wild_card),
                                module_elements
                                    .iter()
                                    .map(|element| ImportElement::ModuleElement {
                                        path: path.clone(),
                                        element: element.clone(),
                                    })
                                    .collect(),
                            );
                        }
                    }
                },
                None => {
                    if let Some(last) = ast.path.last() {
                        import_elements.insert(
                            EntityID::from(last),
                            vec![ImportElement::ModuleAlias { path }],
                        );
                    }
                }
            },
        },
        None => {
            let path = ast.path[..ast.path.len() - 1]
                .iter()
                .map(|path| path.value.to_string())
                .collect::<Vec<_>>();
            let path_string = path.join("::");

            if let Some(PackageResource::Module { source_code: _ }) =
                package_resource_set.get(&path_string)
            {
                if module_element_name_map
                    .get(&path_string)
                    .unwrap()
                    .iter()
                    .any(|element| element.as_str() == ast.path.last().unwrap().value)
                {
                    import_elements.insert(
                        EntityID::from(ast.path.last().unwrap()),
                        vec![ImportElement::ModuleElement {
                            path,
                            element: ast.path.last().unwrap().value.to_string(),
                        }],
                    );
                } else {
                    let error = ImportError {
                        kind: ImportErrorKind::NoElementFound {
                            element_name: ast.path.last().unwrap().value.to_string(),
                        },
                        span: ast.path.last().unwrap().span.clone(),
                        module_path: module_path.clone(),
                    };
                    errors.push(error);
                    import_elements.insert(
                        EntityID::from(ast.path.last().unwrap()),
                        vec![ImportElement::Unknown],
                    );
                }
            } else {
                let error = ImportError {
                    kind: ImportErrorKind::ModuleNotFound {
                        module_name: path_string,
                    },
                    span: ast.span.clone(),
                    module_path: module_path.clone(),
                };
                errors.push(error);
            }
        }
    }
}

fn collect_import_element_for_expression(
    ast: &Expression,
    import_elements: &mut HashMap<EntityID, Vec<ImportElement>>,
    package_resource_set: &PackageResourceSet,
    module_element_name_map: &HashMap<String, Arc<Vec<String>>>,
    errors: &mut Vec<ImportError>,
    module_path: &ModulePath,
) {
    match ast {
        Expression::Return(return_expression) => {
            if let Some(expression) = &return_expression.expression {
                collect_import_element_for_expression(
                    expression,
                    import_elements,
                    package_resource_set,
                    module_element_name_map,
                    errors,
                    module_path,
                );
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
                            collect_import_element_for_type_info(
                                type_info,
                                import_elements,
                                package_resource_set,
                                module_element_name_map,
                                errors,
                                module_path,
                            );
                        }
                    }
                }
            }
        }
        Expression::Or(or_expression) => {
            collect_import_element_for_or_expression(
                *or_expression,
                import_elements,
                package_resource_set,
                module_element_name_map,
                errors,
                module_path,
            );
        }
    }
}

fn collect_import_element_for_type_info(
    ast: &TypeInfo,
    import_elements: &mut HashMap<EntityID, Vec<ImportElement>>,
    package_resource_set: &PackageResourceSet,
    module_element_name_map: &HashMap<String, Arc<Vec<String>>>,
    errors: &mut Vec<ImportError>,
    module_path: &ModulePath,
) {
    match &ast.base {
        TypeInfoBase::Array(array_type_info) => {
            if let Ok(base_type) = &array_type_info.base_type {
                collect_import_element_for_type_info(
                    *base_type,
                    import_elements,
                    package_resource_set,
                    module_element_name_map,
                    errors,
                    module_path,
                );
            }
        }
        TypeInfoBase::Base(base_type_info) => {
            collect_import_element_for_base_type_info(
                base_type_info,
                package_resource_set,
                errors,
                module_path,
            );
        }
        TypeInfoBase::Tuple(tuple_type_info) => {
            for type_info in tuple_type_info.types.iter() {
                collect_import_element_for_type_info(
                    type_info,
                    import_elements,
                    package_resource_set,
                    module_element_name_map,
                    errors,
                    module_path,
                );
            }
        }
        TypeInfoBase::This(_) => {}
    }

    for attribute in ast.attributes.iter() {
        if let TypeAttribute::Result { generics, span: _ } = attribute {
            if let Some(generics) = generics {
                collect_import_element_for_generics(
                    generics,
                    import_elements,
                    package_resource_set,
                    module_element_name_map,
                    errors,
                    module_path,
                );
            }
        }
    }
}

fn collect_import_element_for_generics(
    ast: &GenericsInfo,
    import_elements: &mut HashMap<EntityID, Vec<ImportElement>>,
    package_resource_set: &PackageResourceSet,
    module_element_name_map: &HashMap<String, Arc<Vec<String>>>,
    errors: &mut Vec<ImportError>,
    module_path: &ModulePath,
) {
    for type_info in ast.types.iter() {
        collect_import_element_for_type_info(
            type_info,
            import_elements,
            package_resource_set,
            module_element_name_map,
            errors,
            module_path,
        );
    }
}

fn collect_import_element_for_base_type_info(
    ast: &BaseTypeInfo,
    package_resource_set: &PackageResourceSet,
    errors: &mut Vec<ImportError>,
    module_path: &ModulePath,
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
                Some(_) => {}
                None => {
                    let span_start = ast.path.first().unwrap().span.start;
                    let span_end = ast.path[ast.path.len() - 1].span.end;

                    let error = ImportError {
                        kind: ImportErrorKind::ModuleNotFound { module_name },
                        span: span_start..span_end,
                        module_path: module_path.clone(),
                    };
                    errors.push(error);
                }
            }
        }
    }
}

macro_rules! collect_import_element_for_2op {
    ($name:ident, $ast:ty, $next:ident) => {
        fn $name(
            ast: &$ast,

            import_elements: &mut HashMap<EntityID, Vec<ImportElement>>,
            package_resource_set: &PackageResourceSet,
            module_element_name_map: &HashMap<String, Arc<Vec<String>>>,
            errors: &mut Vec<ImportError>,
            module_path: &ModulePath,
        ) {
            $next(
                &ast.left,
                import_elements,
                package_resource_set,
                module_element_name_map,
                errors,
                module_path,
            );
            for chain in ast.chain.iter() {
                $next(
                    chain,
                    import_elements,
                    package_resource_set,
                    module_element_name_map,
                    errors,
                    module_path,
                );
            }
        }
    };
    (op, $name:ident, $ast:ty, $next:ident) => {
        fn $name(
            ast: &$ast,

            import_elements: &mut HashMap<EntityID, Vec<ImportElement>>,
            package_resource_set: &PackageResourceSet,
            module_element_name_map: &HashMap<String, Arc<Vec<String>>>,
            errors: &mut Vec<ImportError>,
            module_path: &ModulePath,
        ) {
            $next(
                &ast.left,
                import_elements,
                package_resource_set,
                module_element_name_map,
                errors,
                module_path,
            );
            for chain in ast.chain.iter() {
                $next(
                    &chain.1,
                    import_elements,
                    package_resource_set,
                    module_element_name_map,
                    errors,
                    module_path,
                );
            }
        }
    };
}

collect_import_element_for_2op!(
    collect_import_element_for_or_expression,
    OrExpression,
    collect_import_element_for_and_expression
);
collect_import_element_for_2op!(
    collect_import_element_for_and_expression,
    AndExpression,
    collect_import_element_for_equals_expression
);
collect_import_element_for_2op!(
    op,
    collect_import_element_for_equals_expression,
    EqualsExpression,
    collect_import_element_for_less_or_greater_expression
);
collect_import_element_for_2op!(
    op,
    collect_import_element_for_less_or_greater_expression,
    LessOrGreaterExpression,
    collect_import_element_for_add_or_sub_expression
);
collect_import_element_for_2op!(
    op,
    collect_import_element_for_add_or_sub_expression,
    AddOrSubExpression,
    collect_import_element_for_mul_or_div_expression
);
collect_import_element_for_2op!(
    op,
    collect_import_element_for_mul_or_div_expression,
    MulOrDivExpression,
    collect_import_element_for_factor
);

fn collect_import_element_for_factor(
    ast: &Factor,
    import_elements: &mut HashMap<EntityID, Vec<ImportElement>>,
    package_resource_set: &PackageResourceSet,
    module_element_name_map: &HashMap<String, Arc<Vec<String>>>,
    errors: &mut Vec<ImportError>,
    module_path: &ModulePath,
) {
    if let Ok(primary) = &ast.primary {
        collect_import_element_for_primary(
            primary,
            import_elements,
            package_resource_set,
            module_element_name_map,
            errors,
            module_path,
        );
    }
}

fn collect_import_element_for_primary(
    ast: &Primary,
    import_elements: &mut HashMap<EntityID, Vec<ImportElement>>,
    package_resource_set: &PackageResourceSet,
    module_element_name_map: &HashMap<String, Arc<Vec<String>>>,
    errors: &mut Vec<ImportError>,
    module_path: &ModulePath,
) {
    collect_import_element_for_primary_left(
        &ast.left,
        import_elements,
        package_resource_set,
        module_element_name_map,
        errors,
        module_path,
    );
    for chain in ast.chain.iter() {
        collect_import_element_for_primary_right(
            chain,
            import_elements,
            package_resource_set,
            module_element_name_map,
            errors,
            module_path,
        );
    }
}

fn collect_import_element_for_primary_left(
    ast: &PrimaryLeft,
    import_elements: &mut HashMap<EntityID, Vec<ImportElement>>,
    package_resource_set: &PackageResourceSet,
    module_element_name_map: &HashMap<String, Arc<Vec<String>>>,
    errors: &mut Vec<ImportError>,
    module_path: &ModulePath,
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
                        collect_import_element_for_expression(
                            expression,
                            import_elements,
                            package_resource_set,
                            module_element_name_map,
                            errors,
                            module_path,
                        );
                    }
                }
                _ => {}
            }

            if let Some(function_call) = function_call {
                collect_import_element_for_function_call(
                    function_call,
                    import_elements,
                    package_resource_set,
                    module_element_name_map,
                    errors,
                    module_path,
                );
            }

            if let Some(generics_info) = generics {
                collect_import_element_for_generics(
                    generics_info,
                    import_elements,
                    package_resource_set,
                    module_element_name_map,
                    errors,
                    module_path,
                );
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
                    Some(_) => {}
                    None => {
                        let span_start = new_object.path.first().unwrap().span.start;
                        let span_end = new_object.path.last().unwrap().span.end;

                        let error = ImportError {
                            kind: ImportErrorKind::ModuleNotFound { module_name },
                            span: span_start..span_end,
                            module_path: module_path.clone(),
                        };
                        errors.push(error);
                    }
                }
            }
        },
        PrimaryLeftExpr::NewArray { new_array } => {
            for expression in new_array.elements.iter() {
                collect_import_element_for_expression(
                    expression,
                    import_elements,
                    package_resource_set,
                    module_element_name_map,
                    errors,
                    module_path,
                );
            }
        }
        PrimaryLeftExpr::NewArrayInit { new_array_init } => {
            if let Ok(init_expression) = &new_array_init.init_expression {
                collect_import_element_for_expression(
                    init_expression,
                    import_elements,
                    package_resource_set,
                    module_element_name_map,
                    errors,
                    module_path,
                );
            }
            if let Ok(length_expression) = &new_array_init.length_expression {
                collect_import_element_for_expression(
                    length_expression,
                    import_elements,
                    package_resource_set,
                    module_element_name_map,
                    errors,
                    module_path,
                );
            }
        }
        PrimaryLeftExpr::If { if_expression } => {
            if let Ok(condition) = &if_expression.first.condition {
                collect_import_element_for_expression(
                    condition,
                    import_elements,
                    package_resource_set,
                    module_element_name_map,
                    errors,
                    module_path,
                );
            }
            if let Ok(block) = &if_expression.first.block {
                collect_import_element_for_program(
                    block.program,
                    import_elements,
                    package_resource_set,
                    module_element_name_map,
                    errors,
                    module_path,
                );
            }

            for chain in if_expression.chain.iter() {
                match chain {
                    ElseChain::ElseIf { if_statement } => {
                        if let Ok(condition) = &if_statement.condition {
                            collect_import_element_for_expression(
                                condition,
                                import_elements,
                                package_resource_set,
                                module_element_name_map,
                                errors,
                                module_path,
                            );
                        }
                        if let Ok(block) = &if_statement.block {
                            collect_import_element_for_program(
                                block.program,
                                import_elements,
                                package_resource_set,
                                module_element_name_map,
                                errors,
                                module_path,
                            );
                        }
                    }
                    ElseChain::Else { block } => {
                        collect_import_element_for_program(
                            block.program,
                            import_elements,
                            package_resource_set,
                            module_element_name_map,
                            errors,
                            module_path,
                        );
                    }
                }
            }
        }
        PrimaryLeftExpr::Loop { loop_expression } => {
            if let Ok(block) = &loop_expression.block {
                collect_import_element_for_program(
                    block.program,
                    import_elements,
                    package_resource_set,
                    module_element_name_map,
                    errors,
                    module_path,
                );
            }
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_import_element_for_mapping_operator(
            mapping_operator,
            import_elements,
            package_resource_set,
            module_element_name_map,
            errors,
            module_path,
        );
    }
}

fn collect_import_element_for_primary_right(
    ast: &PrimaryRight,
    import_elements: &mut HashMap<EntityID, Vec<ImportElement>>,
    package_resource_set: &PackageResourceSet,
    module_element_name_map: &HashMap<String, Arc<Vec<String>>>,
    errors: &mut Vec<ImportError>,
    module_path: &ModulePath,
) {
    if let Some(second) = &ast.second {
        if let Some(function_call) = &second.function_call {
            collect_import_element_for_function_call(
                function_call,
                import_elements,
                package_resource_set,
                module_element_name_map,
                errors,
                module_path,
            );
        }
        if let Some(generics) = &second.generics {
            collect_import_element_for_generics(
                generics,
                import_elements,
                package_resource_set,
                module_element_name_map,
                errors,
                module_path,
            );
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_import_element_for_mapping_operator(
            mapping_operator,
            import_elements,
            package_resource_set,
            module_element_name_map,
            errors,
            module_path,
        );
    }
}

fn collect_import_element_for_function_call(
    ast: &FunctionCall,
    import_elements: &mut HashMap<EntityID, Vec<ImportElement>>,
    package_resource_set: &PackageResourceSet,
    module_element_name_map: &HashMap<String, Arc<Vec<String>>>,
    errors: &mut Vec<ImportError>,
    module_path: &ModulePath,
) {
    for argument in ast.arguments.iter() {
        collect_import_element_for_expression(
            argument,
            import_elements,
            package_resource_set,
            module_element_name_map,
            errors,
            module_path,
        );
    }
}

fn collect_import_element_for_mapping_operator(
    ast: &MappingOperator,
    import_elements: &mut HashMap<EntityID, Vec<ImportElement>>,
    package_resource_set: &PackageResourceSet,
    module_element_name_map: &HashMap<String, Arc<Vec<String>>>,
    errors: &mut Vec<ImportError>,
    module_path: &ModulePath,
) {
    let block = match ast {
        MappingOperator::NullElvis { block, span: _ } => block,
        MappingOperator::ResultElvis { block, span: _ } => block,
        _ => return,
    };

    let Ok(block) = block else {
        return;
    };

    collect_import_element_for_program(
        block.program,
        import_elements,
        package_resource_set,
        module_element_name_map,
        errors,
        module_path,
    );
}
