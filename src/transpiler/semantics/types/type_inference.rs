use std::{borrow::Borrow, mem, ops::{DerefMut, Range}, sync::{Arc, Mutex}};

use ariadne::Color;
use bumpalo::Bump;
use catla_parser::parser::{AddOrSubExpression, AndExpression, CompareExpression, EQNEExpression, Expression, ExpressionEnum, Factor, FunctionCall, FunctionDefine, GenericsDefine, MappingOperator, MappingOperatorKind, MemoryManageAttributeKind, MulOrDivExpression, Primary, PrimaryLeft, PrimaryLeftExpr, PrimaryRight, Program, SimplePrimary, Spanned, StatementAST, StatementAttributeKind, TypeAttributeEnum, TypeInfo};
use either::Either;
use fxhash::FxHashMap;
use hashbrown::{hash_map::DefaultHashBuilder, HashMap};

use crate::transpiler::{component::EntityID, context::TranspileModuleContext, error::{SimpleError, TranspileReport}, name_resolver::{DefineKind, FoundDefineInfo}, TranspileError, TranspileWarning};

use super::{type_info::{FunctionType, GenericType, LocalGenericID, Type}, user_type_element_collector::get_type};


pub(crate) struct TypeEnvironment<'allocator> {
    entity_type_map: HashMap<EntityID, Either<EntityID, Spanned<Type>>, DefaultHashBuilder, &'allocator Bump>,
    generic_type_map: HashMap<LocalGenericID, Either<LocalGenericID, Spanned<Type>>, DefaultHashBuilder, &'allocator Bump>,
    return_type: Either<EntityID, Spanned<Type>>,
    current_generics_id: usize
}

impl<'allocator> TypeEnvironment<'allocator> {
    
    pub fn new(allocator: &'allocator Bump) -> TypeEnvironment<'allocator> {
        Self::new_with_return_type(
            Either::Right(Spanned::new(Type::Unit, 0..0)),
            allocator
        )
    }

    pub fn new_with_return_type(return_type: Either<EntityID, Spanned<Type>>, allocator: &'allocator Bump) -> TypeEnvironment<'allocator> {
        Self {
            entity_type_map: HashMap::new_in(allocator),
            generic_type_map: HashMap::new_in(allocator),
            return_type,
            current_generics_id: 0,
        }
    }

    pub fn new_local_generic_id(&mut self, type_span: Range<usize>) -> LocalGenericID {
        self.current_generics_id += 1;
        let generic_id = LocalGenericID(self.current_generics_id);
        
        self.generic_type_map.insert(
            generic_id,
            Either::Right(Spanned::new(Type::Unknown, type_span))
        );
        
        generic_id
    }

    pub fn set_entity_type(&mut self, entity_id: EntityID, ty: Spanned<Type>) {
        let entity_id_or_type = self.entity_type_map.get(&entity_id).unwrap();
        let next_entity_id_or_type = match &entity_id_or_type {
            Either::Left(entity_id) => Either::Left(*entity_id),
            Either::Right(ty) => Either::Right(ty.clone()),
        };

        match next_entity_id_or_type {
            Either::Left(entity_id) => self.set_entity_type(entity_id, ty),
            Either::Right(old_type) => {
                if let Type::Unknown = &old_type.value {
                    self.entity_type_map.insert(entity_id, Either::Right(ty));
                }
            }
        }
    }

    pub fn set_generic_type(&mut self, generic_id: LocalGenericID, ty: Spanned<Type>) {
        let generic_id_or_type = self.generic_type_map.get(&generic_id).unwrap();
        let next_generic_id_or_type = match &generic_id_or_type {
            Either::Left(generic_id) => Either::Left(*generic_id),
            Either::Right(ty) => Either::Right(ty.clone()),
        };

        match next_generic_id_or_type {
            Either::Left(generic_id) => self.set_generic_type(generic_id, ty),
            Either::Right(old_type) => {
                if let Type::Unknown = &old_type.value {
                    self.generic_type_map.insert(generic_id, Either::Right(ty));
                }
            }
        }
    }
    
    pub fn set_entity_id_equals(&mut self, first_entity_id: EntityID, second_entity_id: EntityID) {
        self.entity_type_map.insert(second_entity_id, Either::Left(first_entity_id));
    }

    pub fn set_generic_id_equals(&mut self, first_generic_id: LocalGenericID, second_generic_id: LocalGenericID) {
        self.generic_type_map.insert(second_generic_id, Either::Left(first_generic_id));
    }

    pub fn unify(
        &mut self,
        first_entity_id: Spanned<EntityID>,
        second_entity_id: Spanned<EntityID>
    ) -> Result<(), TypeMismatchError> {
        
        let mut first_resolved = self.resolve_entity_type(first_entity_id.value);
        let mut second_resolved = self.resolve_entity_type(second_entity_id.value);
        first_resolved.span = first_entity_id.span.clone();
        second_resolved.span = second_entity_id.span.clone();

        self.unify_type(
            &first_resolved.value,
            &first_resolved.span,
            &second_resolved.value,
            &second_resolved.span
        )
    }

    pub fn unify_with_return_type(
        &mut self,
        return_expr_entity_id: Spanned<EntityID>
    ) -> Result<(), TypeMismatchError> {

        let return_type = match &self.return_type {
            Either::Left(entity_id) => self.resolve_entity_type(*entity_id),
            Either::Right(ty) => ty.clone(),
        };
        let mut return_expr_resolved = self.resolve_entity_type(return_expr_entity_id.value);
        return_expr_resolved.span = return_expr_entity_id.span.clone();

        self.unify_type(
            &return_type.value,
            &return_type.span,
            &return_expr_resolved.value,
            &return_expr_resolved.span
        )
    }

    fn unify_type(
        &mut self,
        first_type: &Type,
        first_span: &Range<usize>,
        second_type: &Type,
        second_span: &Range<usize>
    ) -> Result<(), TypeMismatchError> {

        let result = self.unify_type_recursive(
            first_type,
            first_span,
            second_type,
            second_span
        );

        result.map_err(|mut err| {
            let first = Spanned::new(first_type.clone(), first_span.clone());
            let second = Spanned::new(second_type.clone(), second_span.clone());

            let remove = (first.clone(), second.clone());
            err.retain(|element| { element != &remove });

            TypeMismatchError {
                type_0: first,
                type_1: second,
                generics: err
            }
        })
    }


    fn unify_type_recursive(
        &mut self,
        first_type: &Type,
        first_span: &Range<usize>,
        second_type: &Type,
        second_span: &Range<usize>
    ) -> Result<(), Vec<(Spanned<Type>, Spanned<Type>)>> {

        let eq = match first_type {
            Type::DataStruct { data_struct_info: first_info, generics: first_generics } => {
                if let Type::DataStruct { data_struct_info: second_info, generics: second_generics } = second_type {
                    if first_info == second_info {
                        self.unify_generics(first_generics, first_span, second_generics, second_span)?;
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            },
            Type::Function { function_info: first_function_info, generics: first_generics } => {
                if let Type::Function { function_info: second_function_info, generics: second_generics } = second_type {
                    if first_function_info == second_function_info {
                        self.unify_generics(first_generics, first_span, second_generics, second_span)?;
                        true
                    } else {
                        false
                    }
                } else {
                    false
                }
            },
            Type::LocalGeneric(first_generic_id) => {
                if let Type::LocalGeneric(second_generic_id) = second_type {
                    let first = self.resolve_generic_type(*first_generic_id);
                    let second = self.resolve_generic_type(*second_generic_id);

                    if &first.1.value == &Type::Unknown && &second.1.value == &Type::Unknown {
                        self.generic_type_map.insert(second.0, Either::Left(first.0));
                    } else if &first.1.value == &Type::Unknown && &second.1.value != &Type::Unknown {
                        self.generic_type_map.insert(first.0.clone(), Either::Right(second.1));
                        self.generic_type_map.insert(second.0, Either::Left(first.0));
                    } else if &first.1.value != &Type::Unknown && &second.1.value == &Type::Unknown {
                        self.generic_type_map.insert(second.0, Either::Left(first.0));
                    } else {
                        return self.unify_type_recursive(
                            &first.1.value,
                            &first.1.span,
                            &second.1.value,
                            &second.1.span
                        );
                    }

                    true
                } else {
                    false
                }
            },
            Type::Option(first_type) => {
                if let Type::Option(second_type) = second_type {
                    self.unify_type_recursive(first_type, first_span, second_type, second_span)?;
                    true
                } else {
                    false
                }
            },
            Type::Result { value: first_value_type, error: first_error_type } => {
                if let Type::Result { value: second_value_type, error: second_error_type } = second_type {
                    let value_type_result = self.unify_type_recursive(
                        first_value_type,
                        first_span,
                        second_value_type,
                        second_span
                    );
                    let error_type_result = self.unify_type_recursive(
                        first_error_type,
                        first_span,
                        second_error_type,
                        second_span
                    );

                    let mut errors = Vec::new();
                    if let Err(error) = value_type_result {
                        errors.extend(error);
                    }
                    if let Err(error) = error_type_result {
                        errors.extend(error);
                    }

                    if !errors.is_empty() {
                        return Err(errors);
                    }

                    true
                } else {
                    false
                }
            },
            _ => first_type == second_type
        };

        if eq {
            Ok(())
        } else {
            Err(vec![(
                Spanned::new(first_type.clone(), first_span.clone()),
                Spanned::new(second_type.clone(), second_span.clone())
            )])
        }
    }

    fn unify_generics(
        &mut self,
        first_generics: &Vec<Type>,
        first_span: &Range<usize>,
        second_generics: &Vec<Type>,
        second_span: &Range<usize>
    ) -> Result<(), Vec<(Spanned<Type>, Spanned<Type>)>> {

        let mut errors = Vec::new();

        for i in 0..first_generics.len() {
            let first_generic_type = &first_generics[i];
            let second_generics_type = &second_generics[i];

            let result = self.unify_type_recursive(
                first_generic_type,
                first_span,
                second_generics_type,
                second_span
            );
            
            if let Err(error) = result {
                errors.extend(error);
            }
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }

    pub fn resolve_entity_type(&mut self, entity_id: EntityID) -> Spanned<Type> {
        let mut current_id = entity_id;
        loop {
            let entity_id_or_type = self.entity_type_map.get(&current_id).unwrap();
            match &entity_id_or_type {
                Either::Left(entity_id) => current_id = *entity_id,
                Either::Right(ty) => return ty.clone()
            }
        }
    }

    pub fn resolve_generic_type(&mut self, generic_id: LocalGenericID) -> (LocalGenericID, Spanned<Type>) {
        let mut current_id = generic_id;
        loop {
            let generic_id_or_type = self.generic_type_map.get(&current_id).unwrap();
            match &generic_id_or_type {
                Either::Left(entity_id) => current_id = *entity_id,
                Either::Right(ty) => {
                    return (current_id, ty.clone());
                }
            }
        }
    }

}


pub(crate) fn type_inference_program<'allocator>(
    ast: Program,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    type_environment: &mut TypeEnvironment<'allocator>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    for statement in ast.statements.iter() {
        let statement = match statement {
            Ok(statement) => statement,
            _ => continue
        };

        match statement {
            StatementAST::Assignment(assignment) => {
                type_inference_expression(
                    assignment.left_expr,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    false,
                    type_environment,
                    type_map,
                    allocator,
                    errors,
                    warnings,
                    context
                );
                if let Ok(right_expr) = &assignment.right_expr {
                    type_inference_expression(
                        &right_expr,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        generics_map,
                        module_entity_type_map,
                        true,
                        type_environment,
                        type_map,
                        allocator,
                        errors,
                        warnings,
                        context
                    );

                    let result = type_environment.unify(
                        Spanned::new(EntityID::from(*right_expr), right_expr.get_span()),
                        Spanned::new(EntityID::from(assignment.left_expr), assignment.left_expr.get_span())
                    );
                    add_error(result, errors);
                }
            },
            StatementAST::Exchange(exchange) => {
                type_inference_expression(
                    exchange.left_expr,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    false,
                    type_environment,
                    type_map,
                    allocator,
                    errors,
                    warnings,
                    context
                );
                if let Ok(right_expr) = &exchange.right_expr {
                    type_inference_expression(
                        &right_expr,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        generics_map,
                        module_entity_type_map,
                        false,
                        type_environment,
                        type_map,
                        allocator,
                        errors,
                        warnings,
                        context
                    );

                    let result = type_environment.unify(
                        Spanned::new(EntityID::from(*right_expr), right_expr.get_span()),
                        Spanned::new(EntityID::from(exchange.left_expr), exchange.left_expr.get_span())
                    );
                    add_error(result, errors);
                }
            },
            StatementAST::FunctionDefine(function_define) => {
                let function_type = module_entity_type_map.get(&EntityID::from(function_define)).unwrap();

                if let Type::Function { function_info, generics } = function_type {
                    let mut type_environment = TypeEnvironment::new_with_return_type(
                        Either::Right(function_info.return_type.clone()),
                        allocator
                    );

                    for i in 0..function_define.args.arguments.len() {
                        let argument = &function_define.args.arguments[i];
                        let argument_type = &function_info.argument_types[i];

                        type_environment.set_entity_type(
                            EntityID::from(argument),
                            Spanned::new(argument_type.clone(), argument.span.clone())
                        );
                    }

                    if let Some(block) = &function_define.block.value {
                        type_inference_program(
                            block.program,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            module_element_type_maps,
                            generics_map,
                            module_entity_type_map,
                            &mut type_environment,
                            type_map,
                            allocator,
                            errors,
                            warnings,
                            context
                        );
                    }
                }
            },
            StatementAST::DataStructDefine(data_struct_define) => {
                if let Some(block) = &data_struct_define.block.value {
                    type_inference_program(
                        block.program,
                        user_type_map,
                        import_element_map,
                        name_resolved_map, module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        generics_map,
                        module_entity_type_map,
                        &mut TypeEnvironment::new(allocator),
                        type_map,
                        allocator,
                        errors,
                        warnings,
                        context
                    );
                }
            },
            StatementAST::DropStatement(drop_statement) => {
                if let Ok(expression) = drop_statement.expression {
                    type_inference_expression(
                        expression,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        generics_map,
                        module_entity_type_map,
                        false,
                        type_environment,
                        type_map,
                        allocator,
                        errors,
                        warnings,
                        context
                    );
                }
            },
            StatementAST::Expression(expression) => {
                type_inference_expression(
                    &expression,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    false,
                    type_environment,
                    type_map,
                    allocator,
                    errors,
                    warnings,
                    context
                );
            },
            StatementAST::VariableDefine(variable_define) => {
                let tag_type = match module_entity_type_map.get(&EntityID::from(variable_define)) {
                    Some(ty) => ty.clone(),
                    _ => {
                        match &variable_define.type_tag {
                            Some(type_tag) => {
                                match &type_tag.type_info {
                                    Ok(type_tag) => {
                                        get_type(
                                            type_tag,
                                            user_type_map,
                                            import_element_map,
                                            name_resolved_map,
                                            module_user_type_map,
                                            module_element_type_map,
                                            generics_map,
                                            errors,
                                            warnings,
                                            context
                                        )
                                    },
                                    Err(_) => Type::Unknown
                                }
                            },
                            None => Type::Unknown
                        }
                    }
                };

                if &tag_type != &Type::Unknown {
                    let span = variable_define.name.clone()
                        .map(|name| { name.span })
                        .unwrap_or(variable_define.span.clone());

                    type_environment.set_entity_type(
                        EntityID::from(variable_define),
                        Spanned::new(tag_type.clone(), span)
                    );
                }

                if let Some(expression) = &variable_define.expression {
                    if let Ok(expression) = expression {
                        type_inference_expression(
                            &expression,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            module_element_type_maps,
                            generics_map,
                            module_entity_type_map,
                            true,
                            type_environment,
                            type_map,
                            allocator,
                            errors,
                            warnings,
                            context
                        );

                        if &tag_type == &Type::Unknown {
                            type_environment.set_entity_id_equals(
                                EntityID::from(*expression),
                                EntityID::from(variable_define)
                            );
                        } else {
                            let tag_type_span = variable_define.type_tag.as_ref()
                            .map(|type_tag| {
                                type_tag.type_info.as_ref().map(|type_info| { type_info.span.clone() }).ok()
                            }).flatten().unwrap_or(variable_define.span.clone());

                            type_environment.unify(
                                Spanned::new(EntityID::from(*expression), expression.get_span()),
                                Spanned::new(EntityID::from(variable_define), tag_type_span)
                            );
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

fn type_inference_expression<'allocator>(
    ast: Expression,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    mut force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'allocator>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    match ast {
        ExpressionEnum::OrExpression(or_expression) => {
            force_be_expression |= !or_expression.right_exprs.is_empty();

            type_inference_and_expression(
                &or_expression.left_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                module_entity_type_map,
                force_be_expression,
                type_environment,
                type_map,
                allocator,
                errors,
                warnings,
                context
            );

            let mut results = Vec::new();

            for right_expr in or_expression.right_exprs.iter() {
                if let Ok(right_expr) = &right_expr.1 {
                    type_inference_and_expression(
                        right_expr,
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        generics_map,
                        module_entity_type_map,
                        force_be_expression,
                        type_environment,
                        type_map,
                        allocator,
                        errors,
                        warnings,
                        context
                    );

                    let result = type_environment.unify(
                        Spanned::new(EntityID::from(&or_expression.left_expr), or_expression.left_expr.span.clone()),
                        Spanned::new(EntityID::from(right_expr), right_expr.span.clone())
                    );
                    results.push(result);
                }
            }

            add_errors(results, errors);
        },
        ExpressionEnum::ReturnExpression(return_expression) => {
            if let Some(expression) = return_expression.expression {
                type_inference_expression(
                    expression,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    true,
                    type_environment,
                    type_map,
                    allocator,
                    errors,
                    warnings,
                    context
                );

                let result = type_environment.unify_with_return_type(
                    Spanned::new(EntityID::from(expression), expression.get_span())
                );
                add_error(result, errors);
            }
        },
        ExpressionEnum::Closure(closure) => {
            if let Some(expression_or_block) = &closure.expression_or_block.value {
                match expression_or_block {
                    Either::Left(expression) => {
                        type_inference_expression(
                            &expression,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            module_element_type_maps,
                            generics_map,
                            module_entity_type_map,
                            true,
                            type_environment,
                            type_map,
                            allocator,
                            errors,
                            warnings,
                            context
                        );
                    },
                    Either::Right(block) => {
                        type_inference_program(
                            block.program,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            module_element_type_maps,
                            generics_map,
                            module_entity_type_map,
                            type_environment,
                            type_map,
                            allocator,
                            errors,
                            warnings,
                            context
                        );
                    }
                }
            }
        }
    }
}

fn type_inference_and_expression<'allocator>(
    ast: &AndExpression,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'allocator>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    type_inference_eqne_expression(
        &ast.left_expr,
        user_type_map,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
        module_element_type_maps,
        generics_map,
        module_entity_type_map,
        type_environment,
        type_map,
        allocator,
        errors,
        warnings,
        context
    );
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            type_inference_eqne_expression(
                right_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                module_entity_type_map,
                type_environment,
                type_map,
                allocator,
                errors,
                warnings,
                context
            );
        }
    }
}

fn type_inference_eqne_expression<'allocator>(
    ast: &EQNEExpression,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    type_environment: &mut TypeEnvironment<'allocator>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    type_inference_compare_expression(
        &ast.left_expr,
        user_type_map,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
        module_element_type_maps,
        generics_map,
        module_entity_type_map,
        type_environment,
        type_map,
        allocator,
        errors,
        warnings,
        context
    );
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            type_inference_compare_expression(
                right_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                module_entity_type_map,
                type_environment,
                type_map,
                allocator,
                errors,
                warnings,
                context
            );
        }
    }
}

fn type_inference_compare_expression<'allocator>(
    ast: &CompareExpression,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    type_environment: &mut TypeEnvironment<'allocator>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    type_inference_add_or_sub_expression(
        &ast.left_expr,
        user_type_map,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
        module_element_type_maps,
        generics_map,
        module_entity_type_map,
        type_environment,
        type_map,
        allocator,
        errors,
        warnings,
        context
    );
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            type_inference_add_or_sub_expression(
                right_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                module_entity_type_map,
                type_environment,
                type_map,
                allocator,
                errors,
                warnings,
                context
            );
        }
    }
}

fn type_inference_add_or_sub_expression<'allocator>(
    ast: &AddOrSubExpression,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    type_environment: &mut TypeEnvironment<'allocator>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    type_inference_mul_or_div_expression(
        &ast.left_expr,
        user_type_map,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
        module_element_type_maps,
        generics_map,
        module_entity_type_map,
        type_environment,
        type_map,
        allocator,
        errors,
        warnings,
        context
    );
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            type_inference_mul_or_div_expression(
                right_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                module_entity_type_map,
                type_environment,
                type_map,
                allocator,
                errors,
                warnings,
                context
            );
        }
    }
}

fn type_inference_mul_or_div_expression<'allocator>(
    ast: &MulOrDivExpression,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    type_environment: &mut TypeEnvironment<'allocator>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    type_inference_factor(
        &ast.left_expr,
        user_type_map,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
        module_element_type_maps,
        generics_map,
        module_entity_type_map,
        type_environment,
        type_map,
        allocator,
        errors,
        warnings,
        context
    );
    for right_expr in ast.right_exprs.iter() {
        if let Ok(right_expr) = &right_expr.1 {
            type_inference_factor(
                right_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                module_entity_type_map,
                type_environment,
                type_map,
                allocator,
                errors,
                warnings,
                context
            );
        }
    }
}

fn type_inference_factor<'allocator>(
    ast: &Factor,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    type_environment: &mut TypeEnvironment<'allocator>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    if let Ok(primary) = &ast.primary {
        type_inference_primary(
            primary,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            generics_map,
            module_entity_type_map,
            type_environment,
            type_map,
            allocator,
            errors,
            warnings,
            context
        );
    }
}

fn type_inference_primary<'allocator>(
    ast: &Primary,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    type_environment: &mut TypeEnvironment<'allocator>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    type_inference_primary_left(
        &ast.left,
        user_type_map,
        import_element_map,
        name_resolved_map,
        module_user_type_map,
        module_element_type_map,
        module_element_type_maps,
        generics_map,
        module_entity_type_map,
        type_environment,
        type_map,
        allocator,
        errors,
        warnings,
        context
    );
    for primary_right in ast.chain.iter() {
        type_inference_primary_right(
            primary_right,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            generics_map,
            module_entity_type_map,
            type_environment,
            type_map,
            allocator,
            errors,
            warnings,
            context
        );
    }
}

fn type_inference_primary_left<'allocator>(
    ast: &PrimaryLeft,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    type_environment: &mut TypeEnvironment<'allocator>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    match &ast.first_expr {
        PrimaryLeftExpr::Simple(simple) => {
            match &simple.0 {
                SimplePrimary::Expression { expression, error_tokens: _ } => {
                    if let Ok(expression) = expression {
                        type_inference_expression(
                            &expression,
                            user_type_map,
                            import_element_map,
                            name_resolved_map,
                            module_user_type_map,
                            module_element_type_map,
                            module_element_type_maps,
                            generics_map,
                            module_entity_type_map,
                            type_environment,
                            type_map,
                            allocator,
                            errors,
                            warnings,
                            context
                        );
                    }
                },
                _ => {}
            }

            if let Some(function_call) = &simple.1 {
                type_inference_function_call(
                    function_call,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    type_environment,
                    type_map,
                    allocator,
                    errors,
                    warnings,
                    context
                );
            }
        },
        PrimaryLeftExpr::NewExpression(new_expression) => {
            if let Ok(function_call) = &new_expression.function_call {
                type_inference_function_call(
                    function_call,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    type_environment,
                    type_map,
                    allocator,
                    errors,
                    warnings,
                    context
                );
            }
        },
        PrimaryLeftExpr::IfExpression(if_expression) => {
            let first_statement = &if_expression.if_statement;
            if let Ok(condition) = &first_statement.condition {
                type_inference_expression(
                    &condition,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    type_environment,
                    type_map,
                    allocator,
                    errors,
                    warnings,
                    context
                );
            }
            if let Some(block) = &first_statement.block.value {
                type_inference_program(
                    block.program,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    type_environment,
                    type_map,
                    allocator,
                    errors,
                    warnings,
                    None,
                    context
                );
            }

            for else_if_or_block in if_expression.chain.iter() {
                if let Some(else_if_or_block) = &else_if_or_block.else_if_or_else.value {
                    match else_if_or_block {
                        Either::Left(if_statement) => {
                            if let Ok(condition) = &if_statement.condition {
                                type_inference_expression(
                                    &condition,
                                    user_type_map,
                                    import_element_map,
                                    name_resolved_map,
                                    module_user_type_map,
                                    module_element_type_map,
                                    module_element_type_maps,
                                    generics_map,
                                    module_entity_type_map,
                                    type_environment,
                                    type_map,
                                    allocator,
                                    errors,
                                    warnings,
                                    context
                                );
                            }
                            if let Some(block) = &if_statement.block.value {
                                type_inference_program(
                                    block.program,
                                    user_type_map,
                                    import_element_map,
                                    name_resolved_map,
                                    module_user_type_map,
                                    module_element_type_map,
                                    module_element_type_maps,
                                    generics_map,
                                    module_entity_type_map,
                                    type_environment,
                                    type_map,
                                    allocator,
                                    errors,
                                    warnings,
                                    None,
                                    context
                                );
                            }
                        },
                        Either::Right(block) => {
                            type_inference_program(
                                block.program,
                                user_type_map,
                                import_element_map,
                                name_resolved_map,
                                module_user_type_map,
                                module_element_type_map,
                                module_element_type_maps,
                                generics_map,
                                module_entity_type_map,
                                type_environment,
                                type_map,
                                allocator,
                                errors,
                                warnings,
                                None,
                                context
                            );
                        }
                    }
                }
            }
        },
        PrimaryLeftExpr::LoopExpression(loop_expression) => {
            if let Ok(block) = &loop_expression.block {
                type_inference_program(
                    block.program,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    type_environment,
                    type_map,
                    allocator,
                    errors,
                    warnings,
                    None,
                    context
                );
            }
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        type_inference_mapping_operator(
            mapping_operator,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            generics_map,
            module_entity_type_map,
            type_environment,
            type_map,
            allocator,
            errors,
            warnings,
            context
        );
    }
}

fn type_inference_primary_right<'allocator>(
    ast: &PrimaryRight,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    type_environment: &mut TypeEnvironment<'allocator>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    if let Some(second_expr) = &ast.second_expr {
        if let Some(function_call) = &second_expr.1 {
            type_inference_function_call(
                function_call,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                module_entity_type_map,
                type_environment,
                type_map,
                allocator,
                errors,
                warnings,
                context
            );
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        type_inference_mapping_operator(
            mapping_operator,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            generics_map,
            module_entity_type_map,
            type_environment,
            type_map,
            allocator,
            errors,
            warnings,
            context
        );
    }
}

fn type_inference_mapping_operator<'allocator>(
    ast: &MappingOperator,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    type_environment: &mut TypeEnvironment<'allocator>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    let block = match &ast.value {
        MappingOperatorKind::NullElvisBlock(block) => block,
        MappingOperatorKind::ResultElvisBlock(block) => block,
        _ => return
    };

    if let Some(block) = &block.value {
        type_inference_program(
            block.program,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            generics_map,
            module_entity_type_map,
            type_environment,
            type_map,
            allocator,
            errors,
            warnings,
            None,
            context
        );
    }
}

fn type_inference_function_call<'allocator>(
    ast: &FunctionCall,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    type_environment: &mut TypeEnvironment<'allocator>,
    type_map: &mut FxHashMap<EntityID, Type>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    if let Ok(arg_exprs) = &ast.arg_exprs {
        for arg_expr in arg_exprs.iter() {
            type_inference_expression(
                &arg_expr,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                module_entity_type_map,
                type_environment,
                type_map,
                allocator,
                errors,
                warnings,
                context
            );
        }
    }
}


pub(crate) struct TypeMismatchError {
    type_0: Spanned<Type>,
    type_1: Spanned<Type>,
    generics: Vec<(Spanned<Type>, Spanned<Type>)>
}

impl TranspileReport for TypeMismatchError {
    fn print(&self, context: &TranspileModuleContext) {
        todo!()
    }
}


fn add_error(result: Result<(), TypeMismatchError>, errors: &mut Vec<TranspileError>) {
    if let Err(error) = result {
        errors.push(TranspileError::new(error));
    }
}


fn add_errors(results: Vec<Result<(), TypeMismatchError>>, errors: &mut Vec<TranspileError>) {

}