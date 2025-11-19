use std::sync::{Arc, RwLock};

use catla_import::ImportElement;
use catla_name_resolver::{DefineKind, ResolvedInfo};
use catla_parser::ast::{
    AddOrSubExpression, AndExpression, BaseTypeInfo, Define, ElseChain, EntityID, EqualsExpression,
    Expression, ExpressionOrBlock, Factor, FunctionDefine, GenericsDefine, LessOrGreaterExpression,
    MappingOperator, MulOrDivExpression, OrExpression, Primary, PrimaryLeft, PrimaryLeftExpr,
    PrimaryRight, Program, SimplePrimary, Spanned, Statement, StatementAttribute, TypeInfo,
    TypeInfoBase, VariableBinding, WhereClause, WithSpan,
};
use catla_util::module_path::{ModulePath, Moduled, ToModuled};
use hashbrown::HashMap;

use crate::{
    error::{TypeError, TypeErrorKind},
    type_infer::TypeEnvironment,
    types::{
        FunctionTypeInfo, GenericType, GlobalUserTypeSet, ImplementsCheckResult, ImplementsInfo,
        ImplementsInfoSet, Type, WhereClauseInfo,
    },
};

pub fn collect_module_element_type_for_program(
    ast: &Program,
    this_type: &Option<Type>,
    element_type_holder: &mut Option<&mut HashMap<String, Spanned<Type>>>,
    generics: &mut HashMap<EntityID, Arc<GenericType>>,
    module_element_entity_type_map: &mut HashMap<EntityID, Type>,
    module_element_name_type_map: &mut HashMap<String, Type>,
    implements_infos: &mut ImplementsInfoSet,
    import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
    entity_user_type_map: &HashMap<EntityID, Type>,
    moduled_name_user_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    errors: &mut Vec<TypeError>,
) {
    for statement in ast.statements.iter() {
        match &statement.statement {
            Statement::Assignment(assignment) => {
                collect_module_element_type_for_expression(
                    &assignment.left,
                    generics,
                    module_element_entity_type_map,
                    module_element_name_type_map,
                    implements_infos,
                    import_map,
                    entity_user_type_map,
                    moduled_name_user_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
                    errors,
                );
                if let Ok(right) = &assignment.right {
                    collect_module_element_type_for_expression(
                        right,
                        generics,
                        module_element_entity_type_map,
                        module_element_name_type_map,
                        implements_infos,
                        import_map,
                        entity_user_type_map,
                        moduled_name_user_type_map,
                        name_resolved_map,
                        user_type_set,
                        module_path,
                        errors,
                    );
                }
            }
            Statement::Swap(swap_statement) => {
                collect_module_element_type_for_expression(
                    &swap_statement.left,
                    generics,
                    module_element_entity_type_map,
                    module_element_name_type_map,
                    implements_infos,
                    import_map,
                    entity_user_type_map,
                    moduled_name_user_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
                    errors,
                );
                if let Ok(right) = &swap_statement.right {
                    collect_module_element_type_for_expression(
                        right,
                        generics,
                        module_element_entity_type_map,
                        module_element_name_type_map,
                        implements_infos,
                        import_map,
                        entity_user_type_map,
                        moduled_name_user_type_map,
                        name_resolved_map,
                        user_type_set,
                        module_path,
                        errors,
                    );
                }
            }
            Statement::Import(_) => {}
            Statement::DefineWithAttribute(define_with_attribute) => {
                if let Ok(define) = &define_with_attribute.define {
                    match define {
                        Define::Function(function_define) => {
                            if let Some(generics_define) = &function_define.generics {
                                collect_generics_define(
                                    generics_define,
                                    this_type,
                                    generics,
                                    import_map,
                                    entity_user_type_map,
                                    moduled_name_user_type_map,
                                    name_resolved_map,
                                    user_type_set,
                                    module_path,
                                    errors,
                                );
                            }

                            let function_type = get_function_type(
                                function_define,
                                this_type,
                                generics,
                                import_map,
                                entity_user_type_map,
                                moduled_name_user_type_map,
                                name_resolved_map,
                                user_type_set,
                                module_path,
                                errors,
                            );
                            let function_type = Arc::new(function_type);

                            match element_type_holder {
                                Some(element_type_holder) => {
                                    if let Ok(name) = &function_define.name {
                                        element_type_holder.insert(
                                            name.value.to_string(),
                                            Spanned::new(
                                                Type::Function {
                                                    function_info: function_type,
                                                    generics: Arc::new(Vec::new()),
                                                },
                                                name.span.clone(),
                                            ),
                                        );
                                    }
                                }
                                None => {
                                    if let Ok(name) = &function_define.name {
                                        module_element_name_type_map.insert(
                                            name.value.to_string(),
                                            Type::Function {
                                                function_info: function_type.clone(),
                                                generics: Arc::new(Vec::new()),
                                            },
                                        );
                                        module_element_entity_type_map.insert(
                                            function_define.into(),
                                            Type::Function {
                                                function_info: function_type,
                                                generics: Arc::new(Vec::new()),
                                            },
                                        );
                                    }
                                }
                            }

                            if let Some(block) = &function_define.block {
                                collect_module_element_type_for_program(
                                    block.program,
                                    this_type,
                                    element_type_holder,
                                    generics,
                                    module_element_entity_type_map,
                                    module_element_name_type_map,
                                    implements_infos,
                                    import_map,
                                    entity_user_type_map,
                                    moduled_name_user_type_map,
                                    name_resolved_map,
                                    user_type_set,
                                    module_path,
                                    errors,
                                );
                            }
                        }
                        Define::UserType(user_type_define) => {
                            let user_type = entity_user_type_map
                                .get(&EntityID::from(user_type_define))
                                .unwrap()
                                .clone();

                            let Type::UserType {
                                user_type_info: user_type_id,
                                generics: _,
                            } = user_type
                            else {
                                unreachable!()
                            };

                            let this_type = Some(user_type.clone());

                            module_element_entity_type_map
                                .insert(user_type_define.into(), user_type.clone());

                            if let Ok(name) = &user_type_define.name {
                                module_element_name_type_map
                                    .insert(name.value.to_string(), user_type.clone());
                            }

                            if let Some(generics_define) = &user_type_define.generics {
                                let generics = collect_generics_define(
                                    generics_define,
                                    &this_type,
                                    generics,
                                    import_map,
                                    entity_user_type_map,
                                    moduled_name_user_type_map,
                                    name_resolved_map,
                                    user_type_set,
                                    module_path,
                                    errors,
                                );

                                let user_type_info = user_type_set.get(user_type_id);
                                let mut user_type_info = user_type_info.write().unwrap();
                                user_type_info.generics.extend(generics);
                            }

                            if let Some(where_clause) = &user_type_define.where_clause {
                                let where_clause_info = collect_where_clause(
                                    where_clause,
                                    &this_type,
                                    generics,
                                    import_map,
                                    entity_user_type_map,
                                    moduled_name_user_type_map,
                                    name_resolved_map,
                                    user_type_set,
                                    module_path,
                                    errors,
                                );

                                let user_type_info = user_type_set.get(user_type_id);
                                let mut user_type_info = user_type_info.write().unwrap();
                                user_type_info.where_clause.extend(where_clause_info);
                            }

                            if let Some(super_type_info) = &user_type_define.super_type {
                                for super_type_info in super_type_info.types.iter() {
                                    let super_type = get_type(
                                        super_type_info,
                                        &this_type,
                                        generics,
                                        import_map,
                                        entity_user_type_map,
                                        moduled_name_user_type_map,
                                        name_resolved_map,
                                        user_type_set,
                                        module_path,
                                        errors,
                                        &mut None,
                                    );

                                    implements_infos.register_implements_info(ImplementsInfo {
                                        generics_define: std::vec::Vec::new(),
                                        interface: super_type
                                            .with_span(super_type_info.span.clone()),
                                        concrete: user_type
                                            .clone()
                                            .with_span(user_type_define.span.clone()),
                                        where_clause: std::vec::Vec::new(),
                                        element_type: HashMap::new(),
                                        module_path: module_path.clone(),
                                        is_instant: true,
                                    });
                                }
                            }

                            if let Ok(block) = &user_type_define.block {
                                let mut element_type_holder = HashMap::new();

                                collect_module_element_type_for_program(
                                    block.program,
                                    &this_type,
                                    &mut Some(&mut element_type_holder),
                                    generics,
                                    module_element_entity_type_map,
                                    module_element_name_type_map,
                                    implements_infos,
                                    import_map,
                                    entity_user_type_map,
                                    moduled_name_user_type_map,
                                    name_resolved_map,
                                    user_type_set,
                                    module_path,
                                    errors,
                                );

                                let user_type_info = user_type_set.get(user_type_id);
                                let mut user_type_info = user_type_info.write().unwrap();
                                user_type_info.element_types.extend(element_type_holder);
                            }
                        }
                        Define::Variable(variable_define) => {
                            let type_info = match &variable_define.type_tag {
                                Some(type_tag) => match &type_tag.type_info {
                                    Ok(type_info) => get_type(
                                        type_info,
                                        this_type,
                                        generics,
                                        import_map,
                                        entity_user_type_map,
                                        moduled_name_user_type_map,
                                        name_resolved_map,
                                        user_type_set,
                                        module_path,
                                        errors,
                                        &mut None,
                                    ),
                                    Err(_) => Type::Unknown,
                                },
                                None => {
                                    if define_with_attribute.attribute.iter().any(|attribute| {
                                        attribute.value == StatementAttribute::Static
                                    }) {
                                        let error = TypeError {
                                            kind: TypeErrorKind::MissingStaticVariableType,
                                            span: variable_define.span.clone(),
                                            module_path: module_path.clone(),
                                        };
                                        errors.push(error);
                                    }

                                    Type::Unknown
                                }
                            };

                            if define_with_attribute
                                .attribute
                                .iter()
                                .any(|attribute| attribute.value == StatementAttribute::Static)
                            {
                                if let Ok(binding) = &variable_define.binding {
                                    if let VariableBinding::Literal(literal) = binding {
                                        module_element_name_type_map
                                            .insert(literal.value.to_string(), type_info.clone());
                                        module_element_entity_type_map
                                            .insert(literal.into(), type_info);
                                    }
                                }
                            } else if let Some(element_type_holder) = element_type_holder {
                                if let Ok(binding) = &variable_define.binding {
                                    if let VariableBinding::Literal(literal) = binding {
                                        element_type_holder.insert(
                                            literal.value.to_string(),
                                            Spanned::new(type_info, literal.span.clone()),
                                        );
                                    }
                                }
                            }
                        }
                        Define::TypeAlias(type_alias) => {
                            if let Some(generics_define) = &type_alias.generics {
                                collect_generics_define(
                                    generics_define,
                                    this_type,
                                    generics,
                                    import_map,
                                    entity_user_type_map,
                                    moduled_name_user_type_map,
                                    name_resolved_map,
                                    user_type_set,
                                    module_path,
                                    errors,
                                );
                            }

                            let user_type = entity_user_type_map
                                .get(&EntityID::from(type_alias))
                                .unwrap()
                                .clone();

                            let Type::UserType {
                                user_type_info,
                                generics: _,
                            } = user_type
                            else {
                                unreachable!()
                            };

                            let user_type_info = user_type_set.get(user_type_info);
                            let mut user_type_info = user_type_info.write().unwrap();

                            let alias_type = type_alias
                                .alias_type
                                .as_ref()
                                .map(|type_info| {
                                    get_type(
                                        type_info,
                                        this_type,
                                        generics,
                                        import_map,
                                        entity_user_type_map,
                                        moduled_name_user_type_map,
                                        name_resolved_map,
                                        user_type_set,
                                        module_path,
                                        errors,
                                        &mut None,
                                    )
                                })
                                .unwrap_or(Type::Unknown);

                            user_type_info.element_types.insert(
                                String::new(),
                                Spanned::new(alias_type, type_alias.span.clone()),
                            );
                        }
                    }
                }
            }
            Statement::Drop(drop_statement) => {
                if let Ok(expression) = &drop_statement.expression {
                    collect_module_element_type_for_expression(
                        expression,
                        generics,
                        module_element_entity_type_map,
                        module_element_name_type_map,
                        implements_infos,
                        import_map,
                        entity_user_type_map,
                        moduled_name_user_type_map,
                        name_resolved_map,
                        user_type_set,
                        module_path,
                        errors,
                    );
                }
            }
            Statement::Expression(expression) => {
                collect_module_element_type_for_expression(
                    expression,
                    generics,
                    module_element_entity_type_map,
                    module_element_name_type_map,
                    implements_infos,
                    import_map,
                    entity_user_type_map,
                    moduled_name_user_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
                    errors,
                );
            }
            Statement::Implements(implements) => {
                let generics_define = implements
                    .generics
                    .as_ref()
                    .map(|generics_define| {
                        collect_generics_define(
                            generics_define,
                            this_type,
                            generics,
                            import_map,
                            entity_user_type_map,
                            moduled_name_user_type_map,
                            name_resolved_map,
                            user_type_set,
                            module_path,
                            errors,
                        )
                    })
                    .unwrap_or(Vec::new());

                let interface = implements
                    .interface
                    .as_ref()
                    .map(|interface| {
                        Spanned::new(
                            get_type(
                                interface,
                                this_type,
                                generics,
                                import_map,
                                entity_user_type_map,
                                moduled_name_user_type_map,
                                name_resolved_map,
                                user_type_set,
                                module_path,
                                errors,
                                &mut None,
                            ),
                            interface.span.clone(),
                        )
                    })
                    .unwrap_or(Spanned::new(Type::Unknown, implements.span.clone()));

                let concrete = implements
                    .concrete
                    .as_ref()
                    .map(|concrete_type| {
                        Spanned::new(
                            get_type(
                                concrete_type,
                                this_type,
                                generics,
                                import_map,
                                entity_user_type_map,
                                moduled_name_user_type_map,
                                name_resolved_map,
                                user_type_set,
                                module_path,
                                errors,
                                &mut None,
                            ),
                            concrete_type.span.clone(),
                        )
                    })
                    .unwrap_or(Spanned::new(Type::Unknown, implements.span.clone()));

                let where_clause = implements
                    .where_clause
                    .as_ref()
                    .map(|where_clause| {
                        collect_where_clause(
                            where_clause,
                            this_type,
                            generics,
                            import_map,
                            entity_user_type_map,
                            moduled_name_user_type_map,
                            name_resolved_map,
                            user_type_set,
                            module_path,
                            errors,
                        )
                    })
                    .unwrap_or(Vec::new());

                let mut element_type = HashMap::new();

                if let Ok(block) = &implements.block {
                    collect_module_element_type_for_program(
                        block.program,
                        this_type,
                        &mut Some(&mut element_type),
                        generics,
                        module_element_entity_type_map,
                        module_element_name_type_map,
                        implements_infos,
                        import_map,
                        entity_user_type_map,
                        moduled_name_user_type_map,
                        name_resolved_map,
                        user_type_set,
                        module_path,
                        errors,
                    );
                }

                implements_infos.register_implements_info(ImplementsInfo {
                    generics_define,
                    interface,
                    concrete,
                    where_clause,
                    element_type,
                    module_path: module_path.clone(),
                    is_instant: false,
                });
            }
        }
    }
}

fn get_function_type(
    ast: &FunctionDefine,
    this_type: &Option<Type>,
    generics: &mut HashMap<EntityID, Arc<GenericType>>,
    import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
    entity_user_type_map: &HashMap<EntityID, Type>,
    moduled_name_user_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    errors: &mut Vec<TypeError>,
) -> FunctionTypeInfo {
    let generic_types = ast
        .generics
        .as_ref()
        .map(|generic_define| {
            collect_generics_define(
                generic_define,
                this_type,
                generics,
                import_map,
                entity_user_type_map,
                moduled_name_user_type_map,
                name_resolved_map,
                user_type_set,
                module_path,
                errors,
            )
        })
        .unwrap_or(Vec::new());

    let name = match &ast.name {
        Ok(name) => name.clone().map(|name| name.to_string()),
        Err(_) => Spanned::new(String::new(), ast.span.clone()),
    };

    let arguments = ast
        .arguments
        .as_ref()
        .map(|arguments| {
            arguments
                .arguments
                .iter()
                .map(|argument| {
                    argument
                        .type_tag
                        .type_info
                        .as_ref()
                        .map(|type_info| {
                            get_type(
                                type_info,
                                this_type,
                                generics,
                                import_map,
                                entity_user_type_map,
                                moduled_name_user_type_map,
                                name_resolved_map,
                                user_type_set,
                                module_path,
                                errors,
                                &mut None,
                            )
                            .with_span(type_info.span.clone())
                        })
                        .unwrap_or(Type::Unknown.with_span(argument.span.clone()))
                })
                .collect()
        })
        .unwrap_or(Vec::new());

    let return_type = ast
        .return_type
        .as_ref()
        .map(|type_tag| {
            type_tag
                .type_info
                .as_ref()
                .map(|type_info| {
                    get_type(
                        type_info,
                        this_type,
                        generics,
                        import_map,
                        entity_user_type_map,
                        moduled_name_user_type_map,
                        name_resolved_map,
                        user_type_set,
                        module_path,
                        errors,
                        &mut None,
                    )
                    .with_span(type_info.span.clone())
                })
                .unwrap_or(Type::Unknown.with_span(type_tag.span.clone()))
        })
        .unwrap_or(Type::Unit.with_span(ast.span.clone()));

    let where_clause = match &ast.where_clause {
        Some(where_clause) => collect_where_clause(
            where_clause,
            this_type,
            generics,
            import_map,
            entity_user_type_map,
            moduled_name_user_type_map,
            name_resolved_map,
            user_type_set,
            module_path,
            errors,
        ),
        None => Vec::new(),
    };

    FunctionTypeInfo {
        module_path: module_path.clone(),
        name: Some(name),
        generics: generic_types,
        arguments,
        return_type,
        where_clause,
        span: ast.span.clone(),
    }
}

fn collect_generics_define(
    ast: &GenericsDefine,
    this_type: &Option<Type>,
    generics: &mut HashMap<EntityID, Arc<GenericType>>,
    import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
    entity_user_type_map: &HashMap<EntityID, Type>,
    moduled_name_user_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    errors: &mut Vec<TypeError>,
) -> Vec<Arc<GenericType>> {
    let mut generic_types = Vec::new();

    for element in ast.elements.iter() {
        let generic_type = Arc::new(GenericType {
            module_path: module_path.clone(),
            entity_id: element.into(),
            name: element.name.clone().map(|name| name.to_string()),
            bounds: RwLock::new(Vec::new()),
        });

        generics.insert(element.into(), generic_type.clone());

        let mut bounds = Vec::new();
        for bound in element.bounds.iter() {
            bounds.push(
                get_type(
                    bound,
                    this_type,
                    generics,
                    import_map,
                    entity_user_type_map,
                    moduled_name_user_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
                    errors,
                    &mut None,
                )
                .with_span(bound.span.clone()),
            );
        }
        generic_type.bounds.write().unwrap().extend(bounds);

        generic_types.push(generic_type);
    }

    generic_types
}

pub(crate) fn get_type(
    ast: &TypeInfo,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
    entity_user_type_map: &HashMap<EntityID, Type>,
    moduled_name_user_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    errors: &mut Vec<TypeError>,
    type_environments: &mut Option<(&ImplementsInfoSet, &mut TypeEnvironment)>,
) -> Type {
    let base_type = match &ast.base {
        TypeInfoBase::Array(array_type_info) => {
            let base_type = array_type_info
                .base_type
                .as_ref()
                .map(|base_type| {
                    get_type(
                        *base_type,
                        this_type,
                        generics,
                        import_map,
                        entity_user_type_map,
                        moduled_name_user_type_map,
                        name_resolved_map,
                        user_type_set,
                        module_path,
                        errors,
                        type_environments,
                    )
                })
                .unwrap_or(Type::Unknown);

            Type::Array(Arc::new(base_type))
        }
        TypeInfoBase::Base(base_type_info) => get_base_type(
            base_type_info,
            this_type,
            generics,
            import_map,
            entity_user_type_map,
            moduled_name_user_type_map,
            name_resolved_map,
            user_type_set,
            module_path,
            errors,
            type_environments,
        ),
        TypeInfoBase::Tuple(tuple_type_info) => Type::Tuple(Arc::new(
            tuple_type_info
                .types
                .iter()
                .map(|type_info| {
                    get_type(
                        type_info,
                        this_type,
                        generics,
                        import_map,
                        entity_user_type_map,
                        moduled_name_user_type_map,
                        name_resolved_map,
                        user_type_set,
                        module_path,
                        errors,
                        type_environments,
                    )
                })
                .collect(),
        )),
        TypeInfoBase::This(literal) => match this_type {
            Some(this_type) => this_type.clone(),
            None => {
                let error = TypeError {
                    kind: TypeErrorKind::UnknownThis,
                    span: literal.span.clone(),
                    module_path: module_path.clone(),
                };
                errors.push(error);

                Type::Unknown
            }
        },
    };

    base_type
}

fn get_base_type(
    ast: &BaseTypeInfo,
    this_type: &Option<Type>,
    generics: &HashMap<EntityID, Arc<GenericType>>,
    import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
    entity_user_type_map: &HashMap<EntityID, Type>,
    moduled_name_user_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    errors: &mut Vec<TypeError>,
    type_environments: &mut Option<(&ImplementsInfoSet, &mut TypeEnvironment)>,
) -> Type {
    let base_type = match ast.path.first() {
        Some(first) => match name_resolved_map.get(&EntityID::from(first)) {
            Some(resolved) => match resolved.define.kind {
                DefineKind::Import => match ast.path.len() {
                    1 => match import_map
                        .get(&resolved.define.entity_id)
                        .unwrap()
                        .first()
                        .unwrap_or(&ImportElement::Unknown)
                    {
                        ImportElement::ModuleAlias { path: _ } => {
                            let error = TypeError {
                                kind: TypeErrorKind::MissingModuleElementType,
                                span: ast.path.first().unwrap().span.clone(),
                                module_path: module_path.clone(),
                            };
                            errors.push(error);

                            Type::Unknown
                        }
                        ImportElement::ModuleElement { path, element: _ } => {
                            let module_name = path.iter().cloned().collect::<Vec<_>>().join("::");
                            match moduled_name_user_type_map
                                .get(&module_name)
                                .unwrap()
                                .get(first.value)
                            {
                                Some(user_type) => user_type.clone(),
                                None => {
                                    let error = TypeError {
                                        kind: TypeErrorKind::NotFoundModuleElementType,
                                        span: first.span.clone(),
                                        module_path: module_path.clone(),
                                    };
                                    errors.push(error);

                                    Type::Unknown
                                }
                            }
                        }
                        ImportElement::Unknown => Type::Unknown,
                    },
                    _ => {
                        let module_path_chain = ast.path[1..ast.path.len() - 1]
                            .iter()
                            .map(|literal| literal.value.to_string());

                        match import_map
                            .get(&resolved.define.entity_id)
                            .unwrap()
                            .first()
                            .unwrap_or(&ImportElement::Unknown)
                        {
                            ImportElement::ModuleAlias { path } => {
                                let path = path
                                    .iter()
                                    .cloned()
                                    .chain(module_path_chain)
                                    .collect::<Vec<_>>()
                                    .join("::");

                                match moduled_name_user_type_map.get(&path) {
                                    Some(module_element_type_map) => {
                                        match module_element_type_map
                                            .get(ast.path.last().unwrap().value)
                                        {
                                            Some(user_type) => user_type.clone(),
                                            None => {
                                                let error = TypeError {
                                                    kind: TypeErrorKind::NotFoundModuleElementType,
                                                    span: ast.path.last().unwrap().span.clone(),
                                                    module_path: module_path.clone(),
                                                };
                                                errors.push(error);

                                                Type::Unknown
                                            }
                                        }
                                    }
                                    None => Type::Unknown,
                                }
                            }
                            ImportElement::ModuleElement {
                                path: _,
                                element: _,
                            } => {
                                let error = TypeError {
                                    kind: TypeErrorKind::UnknownModuleElementType,
                                    span: ast.path.last().unwrap().span.clone(),
                                    module_path: module_path.clone(),
                                };
                                errors.push(error);

                                Type::Unknown
                            }
                            ImportElement::Unknown => Type::Unknown,
                        }
                    }
                },
                DefineKind::Function => Type::Unknown,
                DefineKind::Variable => Type::Unknown,
                DefineKind::UserType => match ast.path.len() {
                    1 => entity_user_type_map
                        .get(&resolved.define.entity_id)
                        .unwrap()
                        .clone(),
                    _ => Type::Unknown,
                },
                DefineKind::Generics => match ast.path.len() {
                    1 => Type::Generic(generics.get(&resolved.define.entity_id).unwrap().clone()),
                    _ => Type::Unknown,
                },
            },
            None => match ast.path.len() {
                1 => match first.value {
                    "int" => Type::Int32,
                    "int8" => Type::Int8,
                    "int16" => Type::Int16,
                    "int32" => Type::Int32,
                    "int64" => Type::Int64,
                    "uint" => Type::Uint32,
                    "uint8" => Type::Uint8,
                    "uint16" => Type::Uint16,
                    "uint32" => Type::Uint32,
                    "uint64" => Type::Uint64,
                    "float" => Type::Float32,
                    "float32" => Type::Float32,
                    "float64" => Type::Float64,
                    "bool" => Type::Bool,
                    "unit" => Type::Unit,
                    _ => {
                        let error = TypeError {
                            kind: TypeErrorKind::MissingModuleElementType,
                            span: ast.path.first().unwrap().span.clone(),
                            module_path: module_path.clone(),
                        };
                        errors.push(error);

                        Type::Unknown
                    }
                },
                _ => {
                    let module_name = ast.path[..ast.path.len() - 1]
                        .iter()
                        .map(|literal| literal.value.to_string())
                        .collect::<Vec<_>>()
                        .join("::");

                    match moduled_name_user_type_map.get(&module_name) {
                        Some(module_element_map) => {
                            match module_element_map.get(ast.path.last().unwrap().value) {
                                Some(user_type) => user_type.clone(),
                                None => {
                                    let error = TypeError {
                                        kind: TypeErrorKind::NotFoundModuleElementType,
                                        span: ast.path.last().unwrap().span.clone(),
                                        module_path: module_path.clone(),
                                    };
                                    errors.push(error);

                                    Type::Unknown
                                }
                            }
                        }
                        None => Type::Unknown,
                    }
                }
            },
        },
        None => Type::Unknown,
    };

    match &ast.generics {
        Some(generics_info_ast) => match base_type {
            Type::UserType {
                user_type_info,
                generics: _,
            } => {
                let new_generics_types = generics_info_ast
                    .types
                    .iter()
                    .map(|type_info| {
                        get_type(
                            type_info,
                            this_type,
                            generics,
                            import_map,
                            entity_user_type_map,
                            moduled_name_user_type_map,
                            name_resolved_map,
                            user_type_set,
                            module_path,
                            errors,
                            &mut None,
                        )
                    })
                    .collect::<Vec<_>>();

                if let Some((implements_infos, type_environment)) = type_environments {
                    let user_type_info = user_type_set.get(user_type_info);
                    let user_type_info = user_type_info.read().unwrap();

                    if user_type_info.generics.len() != new_generics_types.len() {
                        let error = TypeError {
                            kind: TypeErrorKind::InvalidGenericsCount {
                                expected: user_type_info.generics.len(),
                                found: new_generics_types.len(),
                                defined: Moduled::new(
                                    (),
                                    user_type_info.module_path.clone(),
                                    user_type_info.span.clone(),
                                ),
                            },
                            span: generics_info_ast.span.clone(),
                            module_path: module_path.clone(),
                        };
                        errors.push(error);
                    }

                    let mut bounds = Vec::new();
                    for generic in user_type_info.generics.iter() {
                        bounds.push(WhereClauseInfo {
                            target: Spanned::new(
                                Type::Generic(generic.clone()),
                                generic.name.span.clone(),
                            ),
                            bounds: generic.bounds.read().unwrap().clone(),
                        });
                    }
                    bounds.extend(user_type_info.where_clause.iter().cloned());

                    let old_generics = &user_type_info.generics;
                    let new_generics = &new_generics_types;

                    for where_bound in bounds.iter() {
                        let target = where_bound
                            .target
                            .value
                            .replace_generics(old_generics, new_generics);

                        for bound in where_bound.bounds.iter() {
                            let replaced_bound =
                                bound.value.replace_generics(old_generics, new_generics);

                            match implements_infos.is_implements(
                                target.clone().moduled(
                                    user_type_info.module_path.clone(),
                                    where_bound.target.span.clone(),
                                ),
                                replaced_bound.moduled(
                                    user_type_info.module_path.clone(),
                                    bound.span.clone(),
                                ),
                                None,
                                user_type_set,
                                type_environment,
                            ) {
                                ImplementsCheckResult::NoImplementsFound => {
                                    let error = TypeError {
                                        kind: TypeErrorKind::NotSatisfied {
                                            target: target
                                                .to_display_string(
                                                    user_type_set,
                                                    Some(type_environment),
                                                )
                                                .moduled(
                                                    module_path.clone(),
                                                    generics_info_ast.span.clone(),
                                                ),
                                            origin_target: where_bound
                                                .target
                                                .value
                                                .to_display_string(
                                                    user_type_set,
                                                    Some(type_environment),
                                                )
                                                .moduled(
                                                    user_type_info.module_path.clone(),
                                                    where_bound.target.span.clone(),
                                                ),
                                            origin_bound: bound
                                                .value
                                                .to_display_string(
                                                    user_type_set,
                                                    Some(type_environment),
                                                )
                                                .moduled(
                                                    user_type_info.module_path.clone(),
                                                    bound.span.clone(),
                                                ),
                                        },
                                        span: generics_info_ast.span.clone(),
                                        module_path: module_path.clone(),
                                    };
                                    errors.push(error);
                                }
                                ImplementsCheckResult::Conflicts { conflicts } => {
                                    let error = TypeError {
                                        kind: TypeErrorKind::ConflictedImplementInTypeInfer {
                                            conflicts: conflicts
                                                .into_iter()
                                                .map(|ty| {
                                                    ty.map(|ty| {
                                                        ty.to_display_string(
                                                            user_type_set,
                                                            Some(type_environment),
                                                        )
                                                    })
                                                })
                                                .collect(),
                                        },
                                        span: generics_info_ast.span.clone(),
                                        module_path: module_path.clone(),
                                    };
                                    errors.push(error);
                                }
                                ImplementsCheckResult::Success {
                                    new_concrete,
                                    new_interface: _,
                                    new_generics: _,
                                } => {
                                    if !type_environment.test_unify_type(
                                        target.clone().moduled(
                                            module_path.clone(),
                                            generics_info_ast.span.clone(),
                                        ),
                                        new_concrete,
                                        user_type_set,
                                    ) {
                                        unreachable!("generic bounds check unify must not fail!");
                                    }
                                }
                            }
                        }
                    }
                }

                Type::UserType {
                    user_type_info,
                    generics: Arc::new(new_generics_types),
                }
            }
            _ => base_type,
        },
        None => base_type,
    }
}

fn collect_where_clause(
    ast: &WhereClause,
    this_type: &Option<Type>,
    generics: &mut HashMap<EntityID, Arc<GenericType>>,
    import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
    entity_user_type_map: &HashMap<EntityID, Type>,
    moduled_name_user_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    errors: &mut Vec<TypeError>,
) -> Vec<WhereClauseInfo> {
    ast.elements
        .iter()
        .map(|element| {
            let target = Spanned::new(
                get_type(
                    &element.target_type,
                    this_type,
                    generics,
                    import_map,
                    entity_user_type_map,
                    moduled_name_user_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
                    errors,
                    &mut None,
                ),
                element.target_type.span.clone(),
            );

            let bounds = element
                .bounds
                .iter()
                .map(|bound| {
                    Spanned::new(
                        get_type(
                            bound,
                            this_type,
                            generics,
                            import_map,
                            entity_user_type_map,
                            moduled_name_user_type_map,
                            name_resolved_map,
                            user_type_set,
                            module_path,
                            errors,
                            &mut None,
                        ),
                        bound.span.clone(),
                    )
                })
                .collect();

            WhereClauseInfo { target, bounds }
        })
        .collect()
}

fn collect_module_element_type_for_expression(
    ast: &Expression,
    generics: &mut HashMap<EntityID, Arc<GenericType>>,
    module_element_entity_type_map: &mut HashMap<EntityID, Type>,
    module_element_name_type_map: &mut HashMap<String, Type>,
    implements_infos: &mut ImplementsInfoSet,
    import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
    entity_user_type_map: &HashMap<EntityID, Type>,
    moduled_name_user_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    errors: &mut Vec<TypeError>,
) {
    match ast {
        Expression::Return(return_expression) => {
            if let Some(expression) = &return_expression.expression {
                collect_module_element_type_for_expression(
                    expression,
                    generics,
                    module_element_entity_type_map,
                    module_element_name_type_map,
                    implements_infos,
                    import_map,
                    entity_user_type_map,
                    moduled_name_user_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
                    errors,
                );
            }
        }
        Expression::Closure(closure) => {
            if let Ok(expression_or_block) = &closure.expression_or_block {
                match expression_or_block {
                    ExpressionOrBlock::Expression(expression) => {
                        collect_module_element_type_for_expression(
                            expression,
                            generics,
                            module_element_entity_type_map,
                            module_element_name_type_map,
                            implements_infos,
                            import_map,
                            entity_user_type_map,
                            moduled_name_user_type_map,
                            name_resolved_map,
                            user_type_set,
                            module_path,
                            errors,
                        );
                    }
                    ExpressionOrBlock::Block(block) => {
                        collect_module_element_type_for_program(
                            block.program,
                            &mut None,
                            &mut None,
                            generics,
                            module_element_entity_type_map,
                            module_element_name_type_map,
                            implements_infos,
                            import_map,
                            entity_user_type_map,
                            moduled_name_user_type_map,
                            name_resolved_map,
                            user_type_set,
                            module_path,
                            errors,
                        );
                    }
                }
            }
        }
        Expression::Or(or_expression) => {
            collect_module_element_type_for_or_expression(
                *or_expression,
                generics,
                module_element_entity_type_map,
                module_element_name_type_map,
                implements_infos,
                import_map,
                entity_user_type_map,
                moduled_name_user_type_map,
                name_resolved_map,
                user_type_set,
                module_path,
                errors,
            );
        }
    }
}

macro_rules! collect_module_element_type_for_2op {
    ($name:ident, $ty:ty, $next:ident) => {
        fn $name(
            ast: &$ty,
            generics: &mut HashMap<EntityID, Arc<GenericType>>,
            module_element_entity_type_map: &mut HashMap<EntityID, Type>,
            module_element_name_type_map: &mut HashMap<String, Type>,
            implements_infos: &mut ImplementsInfoSet,
            import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
            entity_user_type_map: &HashMap<EntityID, Type>,
            moduled_name_user_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
            name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
            user_type_set: &GlobalUserTypeSet,
            module_path: &ModulePath,
            errors: &mut Vec<TypeError>,
        ) {
            $next(
                &ast.left,
                generics,
                module_element_entity_type_map,
                module_element_name_type_map,
                implements_infos,
                import_map,
                entity_user_type_map,
                moduled_name_user_type_map,
                name_resolved_map,
                user_type_set,
                module_path,
                errors,
            );
            for chain in ast.chain.iter() {
                $next(
                    chain,
                    generics,
                    module_element_entity_type_map,
                    module_element_name_type_map,
                    implements_infos,
                    import_map,
                    entity_user_type_map,
                    moduled_name_user_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
                    errors,
                );
            }
        }
    };
    (op, $name:ident, $ty:ty, $next:ident) => {
        fn $name(
            ast: &$ty,
            generics: &mut HashMap<EntityID, Arc<GenericType>>,
            module_element_entity_type_map: &mut HashMap<EntityID, Type>,
            module_element_name_type_map: &mut HashMap<String, Type>,
            implements_infos: &mut ImplementsInfoSet,
            import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
            entity_user_type_map: &HashMap<EntityID, Type>,
            moduled_name_user_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
            name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
            user_type_set: &GlobalUserTypeSet,
            module_path: &ModulePath,
            errors: &mut Vec<TypeError>,
        ) {
            $next(
                &ast.left,
                generics,
                module_element_entity_type_map,
                module_element_name_type_map,
                implements_infos,
                import_map,
                entity_user_type_map,
                moduled_name_user_type_map,
                name_resolved_map,
                user_type_set,
                module_path,
                errors,
            );
            for chain in ast.chain.iter() {
                $next(
                    &chain.1,
                    generics,
                    module_element_entity_type_map,
                    module_element_name_type_map,
                    implements_infos,
                    import_map,
                    entity_user_type_map,
                    moduled_name_user_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
                    errors,
                );
            }
        }
    };
}

collect_module_element_type_for_2op!(
    collect_module_element_type_for_or_expression,
    OrExpression,
    collect_module_element_type_for_and_expression
);

collect_module_element_type_for_2op!(
    collect_module_element_type_for_and_expression,
    AndExpression,
    collect_module_element_type_for_equals_expression
);

collect_module_element_type_for_2op!(
    op,
    collect_module_element_type_for_equals_expression,
    EqualsExpression,
    collect_module_element_type_for_less_or_greater_expression
);

collect_module_element_type_for_2op!(
    op,
    collect_module_element_type_for_less_or_greater_expression,
    LessOrGreaterExpression,
    collect_module_element_type_for_add_or_sub_expression
);

collect_module_element_type_for_2op!(
    op,
    collect_module_element_type_for_add_or_sub_expression,
    AddOrSubExpression,
    collect_module_element_type_for_mul_or_div_expression
);

collect_module_element_type_for_2op!(
    op,
    collect_module_element_type_for_mul_or_div_expression,
    MulOrDivExpression,
    collect_module_element_type_for_factor
);

fn collect_module_element_type_for_factor(
    ast: &Factor,
    generics: &mut HashMap<EntityID, Arc<GenericType>>,
    module_element_entity_type_map: &mut HashMap<EntityID, Type>,
    module_element_name_type_map: &mut HashMap<String, Type>,
    implements_infos: &mut ImplementsInfoSet,
    import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
    entity_user_type_map: &HashMap<EntityID, Type>,
    moduled_name_user_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    errors: &mut Vec<TypeError>,
) {
    if let Ok(primary) = &ast.primary {
        collect_module_element_type_for_primary(
            primary,
            generics,
            module_element_entity_type_map,
            module_element_name_type_map,
            implements_infos,
            import_map,
            entity_user_type_map,
            moduled_name_user_type_map,
            name_resolved_map,
            user_type_set,
            module_path,
            errors,
        );
    }
}

fn collect_module_element_type_for_primary(
    ast: &Primary,
    generics: &mut HashMap<EntityID, Arc<GenericType>>,
    module_element_entity_type_map: &mut HashMap<EntityID, Type>,
    module_element_name_type_map: &mut HashMap<String, Type>,
    implements_infos: &mut ImplementsInfoSet,
    import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
    entity_user_type_map: &HashMap<EntityID, Type>,
    moduled_name_user_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    errors: &mut Vec<TypeError>,
) {
    collect_module_element_type_for_primary_left(
        &ast.left,
        generics,
        module_element_entity_type_map,
        module_element_name_type_map,
        implements_infos,
        import_map,
        entity_user_type_map,
        moduled_name_user_type_map,
        name_resolved_map,
        user_type_set,
        module_path,
        errors,
    );
    for chain in ast.chain.iter() {
        collect_module_element_type_for_primary_right(
            chain,
            generics,
            module_element_entity_type_map,
            module_element_name_type_map,
            implements_infos,
            import_map,
            entity_user_type_map,
            moduled_name_user_type_map,
            name_resolved_map,
            user_type_set,
            module_path,
            errors,
        );
    }
}

fn collect_module_element_type_for_primary_left(
    ast: &PrimaryLeft,
    generics: &mut HashMap<EntityID, Arc<GenericType>>,
    module_element_entity_type_map: &mut HashMap<EntityID, Type>,
    module_element_name_type_map: &mut HashMap<String, Type>,
    implements_infos: &mut ImplementsInfoSet,
    import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
    entity_user_type_map: &HashMap<EntityID, Type>,
    moduled_name_user_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    errors: &mut Vec<TypeError>,
) {
    match &ast.first {
        PrimaryLeftExpr::Simple {
            left,
            generics: _,
            function_call,
            span: _,
        } => {
            match left {
                SimplePrimary::Tuple {
                    expressions,
                    span: _,
                } => {
                    for expression in expressions.iter() {
                        collect_module_element_type_for_expression(
                            expression,
                            generics,
                            module_element_entity_type_map,
                            module_element_name_type_map,
                            implements_infos,
                            import_map,
                            entity_user_type_map,
                            moduled_name_user_type_map,
                            name_resolved_map,
                            user_type_set,
                            module_path,
                            errors,
                        );
                    }
                }
                SimplePrimary::Literal(_) => {}
                SimplePrimary::StringLiteral(_) => {}
                SimplePrimary::NumericLiteral(_) => {}
                SimplePrimary::Null(_) => {}
                SimplePrimary::True(_) => {}
                SimplePrimary::False(_) => {}
                SimplePrimary::This(_) => {}
                SimplePrimary::LargeThis(_) => {}
            }

            if let Some(function_call) = function_call {
                for argument in function_call.arguments.iter() {
                    collect_module_element_type_for_expression(
                        argument,
                        generics,
                        module_element_entity_type_map,
                        module_element_name_type_map,
                        implements_infos,
                        import_map,
                        entity_user_type_map,
                        moduled_name_user_type_map,
                        name_resolved_map,
                        user_type_set,
                        module_path,
                        errors,
                    );
                }
            }
        }
        PrimaryLeftExpr::NewObject { new_object } => {
            for field_assign in new_object.field_assign.elements.iter() {
                collect_module_element_type_for_expression(
                    &field_assign.expression,
                    generics,
                    module_element_entity_type_map,
                    module_element_name_type_map,
                    implements_infos,
                    import_map,
                    entity_user_type_map,
                    moduled_name_user_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
                    errors,
                );
            }
        }
        PrimaryLeftExpr::NewArray { new_array } => {
            for expression in new_array.elements.iter() {
                collect_module_element_type_for_expression(
                    expression,
                    generics,
                    module_element_entity_type_map,
                    module_element_name_type_map,
                    implements_infos,
                    import_map,
                    entity_user_type_map,
                    moduled_name_user_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
                    errors,
                );
            }
        }
        PrimaryLeftExpr::NewArrayInit { new_array_init } => {
            if let Ok(expression) = &new_array_init.init_expression {
                collect_module_element_type_for_expression(
                    expression,
                    generics,
                    module_element_entity_type_map,
                    module_element_name_type_map,
                    implements_infos,
                    import_map,
                    entity_user_type_map,
                    moduled_name_user_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
                    errors,
                );
            }
            if let Ok(expression) = &new_array_init.length_expression {
                collect_module_element_type_for_expression(
                    expression,
                    generics,
                    module_element_entity_type_map,
                    module_element_name_type_map,
                    implements_infos,
                    import_map,
                    entity_user_type_map,
                    moduled_name_user_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
                    errors,
                );
            }
        }
        PrimaryLeftExpr::If { if_expression } => {
            if let Ok(condition) = &if_expression.first.condition {
                collect_module_element_type_for_expression(
                    condition,
                    generics,
                    module_element_entity_type_map,
                    module_element_name_type_map,
                    implements_infos,
                    import_map,
                    entity_user_type_map,
                    moduled_name_user_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
                    errors,
                );
            }
            if let Ok(block) = &if_expression.first.block {
                collect_module_element_type_for_program(
                    block.program,
                    &mut None,
                    &mut None,
                    generics,
                    module_element_entity_type_map,
                    module_element_name_type_map,
                    implements_infos,
                    import_map,
                    entity_user_type_map,
                    moduled_name_user_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
                    errors,
                );
            }

            for chain in if_expression.chain.iter() {
                match chain {
                    ElseChain::ElseIf { if_statement } => {
                        if let Ok(condition) = &if_statement.condition {
                            collect_module_element_type_for_expression(
                                condition,
                                generics,
                                module_element_entity_type_map,
                                module_element_name_type_map,
                                implements_infos,
                                import_map,
                                entity_user_type_map,
                                moduled_name_user_type_map,
                                name_resolved_map,
                                user_type_set,
                                module_path,
                                errors,
                            );
                            if let Ok(block) = &if_statement.block {
                                collect_module_element_type_for_program(
                                    block.program,
                                    &mut None,
                                    &mut None,
                                    generics,
                                    module_element_entity_type_map,
                                    module_element_name_type_map,
                                    implements_infos,
                                    import_map,
                                    entity_user_type_map,
                                    moduled_name_user_type_map,
                                    name_resolved_map,
                                    user_type_set,
                                    module_path,
                                    errors,
                                );
                            }
                        }
                    }
                    ElseChain::Else { block } => {
                        collect_module_element_type_for_program(
                            block.program,
                            &mut None,
                            &mut None,
                            generics,
                            module_element_entity_type_map,
                            module_element_name_type_map,
                            implements_infos,
                            import_map,
                            entity_user_type_map,
                            moduled_name_user_type_map,
                            name_resolved_map,
                            user_type_set,
                            module_path,
                            errors,
                        );
                    }
                }
            }
        }
        PrimaryLeftExpr::Loop { loop_expression } => {
            if let Ok(block) = &loop_expression.block {
                collect_module_element_type_for_program(
                    block.program,
                    &mut None,
                    &mut None,
                    generics,
                    module_element_entity_type_map,
                    module_element_name_type_map,
                    implements_infos,
                    import_map,
                    entity_user_type_map,
                    moduled_name_user_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
                    errors,
                );
            }
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_module_element_type_for_mapping_operator(
            mapping_operator,
            generics,
            module_element_entity_type_map,
            module_element_name_type_map,
            implements_infos,
            import_map,
            entity_user_type_map,
            moduled_name_user_type_map,
            name_resolved_map,
            user_type_set,
            module_path,
            errors,
        );
    }
}

fn collect_module_element_type_for_primary_right(
    ast: &PrimaryRight,
    generics: &mut HashMap<EntityID, Arc<GenericType>>,
    module_element_entity_type_map: &mut HashMap<EntityID, Type>,
    module_element_name_type_map: &mut HashMap<String, Type>,
    implements_infos: &mut ImplementsInfoSet,
    import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
    entity_user_type_map: &HashMap<EntityID, Type>,
    moduled_name_user_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    errors: &mut Vec<TypeError>,
) {
    if let Some(second) = &ast.second {
        if let Some(function_call) = &second.function_call {
            for argument in function_call.arguments.iter() {
                collect_module_element_type_for_expression(
                    argument,
                    generics,
                    module_element_entity_type_map,
                    module_element_name_type_map,
                    implements_infos,
                    import_map,
                    entity_user_type_map,
                    moduled_name_user_type_map,
                    name_resolved_map,
                    user_type_set,
                    module_path,
                    errors,
                );
            }
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        collect_module_element_type_for_mapping_operator(
            mapping_operator,
            generics,
            module_element_entity_type_map,
            module_element_name_type_map,
            implements_infos,
            import_map,
            entity_user_type_map,
            moduled_name_user_type_map,
            name_resolved_map,
            user_type_set,
            module_path,
            errors,
        );
    }
}

fn collect_module_element_type_for_mapping_operator(
    ast: &MappingOperator,
    generics: &mut HashMap<EntityID, Arc<GenericType>>,
    module_element_entity_type_map: &mut HashMap<EntityID, Type>,
    module_element_name_type_map: &mut HashMap<String, Type>,
    implements_infos: &mut ImplementsInfoSet,
    import_map: &HashMap<EntityID, std::vec::Vec<ImportElement>>,
    entity_user_type_map: &HashMap<EntityID, Type>,
    moduled_name_user_type_map: &HashMap<String, Arc<HashMap<String, Type>>>,
    name_resolved_map: &HashMap<EntityID, ResolvedInfo>,
    user_type_set: &GlobalUserTypeSet,
    module_path: &ModulePath,
    errors: &mut Vec<TypeError>,
) {
    let block = match ast {
        MappingOperator::NullElvis { block, span: _ } => block,
        MappingOperator::ResultElvis { block, span: _ } => block,
        _ => return,
    };

    if let Ok(block) = block {
        collect_module_element_type_for_program(
            block.program,
            &mut None,
            &mut None,
            generics,
            module_element_entity_type_map,
            module_element_name_type_map,
            implements_infos,
            import_map,
            entity_user_type_map,
            moduled_name_user_type_map,
            name_resolved_map,
            user_type_set,
            module_path,
            errors,
        );
    }
}
