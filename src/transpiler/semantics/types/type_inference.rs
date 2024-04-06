use std::{alloc::Allocator, borrow::Borrow, mem, ops::{DerefMut, Range}, sync::{Arc, Mutex}};

use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind, Source};
use bumpalo::Bump;
use catla_parser::parser::{AddOrSubExpression, AndExpression, CompareExpression, EQNEExpression, Expression, ExpressionEnum, Factor, FunctionCall, FunctionDefine, GenericsDefine, MappingOperator, MappingOperatorKind, MemoryManageAttributeKind, MulOrDivExpression, Primary, PrimaryLeft, PrimaryLeftExpr, PrimaryRight, Program, SimplePrimary, Spanned, StatementAST, StatementAttributeKind, TypeAttributeEnum, TypeInfo};
use either::Either;
use fxhash::FxHashMap;
use hashbrown::{hash_map::DefaultHashBuilder, HashMap};

use crate::transpiler::{component::EntityID, context::TranspileModuleContext, error::{ErrorMessageKey, ErrorMessageType, SimpleError, TranspileReport}, name_resolver::{DefineKind, EnvironmentSeparatorKind, FoundDefineInfo}, TranspileError, TranspileWarning};

use super::{import_module_collector::get_module_name_from_new_expression, type_info::{FunctionType, GenericType, LocalGenericID, Type}, user_type_element_collector::get_type};


pub(crate) struct TypeEnvironment<'allocator> {
    entity_type_map: HashMap<EntityID, Either<EntityID, Spanned<Type>>, DefaultHashBuilder, &'allocator Bump>,
    generic_type_map: HashMap<LocalGenericID, Either<LocalGenericID, Spanned<Type>>, DefaultHashBuilder, &'allocator Bump>,
    return_type: Either<EntityID, Spanned<Type>>,
    lazy_type_reports: Vec<Box<dyn LazyTypeReport>, &'allocator Bump>,
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
            lazy_type_reports: Vec::new_in(allocator),
            current_generics_id: 0
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
        let next_entity_id_or_type = match self.entity_type_map.get(&entity_id){
            Some(value) => value.clone(),
            None => {
                self.entity_type_map.insert(entity_id, Either::Right(ty));
                return;
            }
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
        let next_generic_id_or_type = match self.generic_type_map.get(&generic_id) {
            Some(value) => value.clone(),
            None => {
                self.generic_type_map.insert(generic_id, Either::Right(ty));
                return;
            }
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
        if first_entity_id == second_entity_id {
            return;
        }
        self.entity_type_map.insert(second_entity_id, Either::Left(first_entity_id));
    }

    pub fn set_generic_id_equals(&mut self, first_generic_id: LocalGenericID, second_generic_id: LocalGenericID) {
        if first_generic_id == second_generic_id {
            return;
        }
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

        if let Type::LocalGeneric(first_generic_id) = first_type {
            if second_type != &Type::Unknown {
                let first_resolved_type = self.resolve_generic_type(*first_generic_id);

                return if &first_resolved_type.1.value == &Type::Unknown {
                    self.set_generic_type(first_resolved_type.0, Spanned::new(second_type.clone(), second_span.clone()));
                    Ok(())
                } else {
                    self.unify_type_recursive(
                        &first_resolved_type.1.value,
                        &first_resolved_type.1.span,
                        second_type,
                        second_span
                    )
                };
            }
        }
        if let Type::LocalGeneric(second_generic_id) = second_type {
            if first_type != &Type::Unknown {
                let second_resolved_type = self.resolve_generic_type(*second_generic_id);

                return if &second_resolved_type.1.value == &Type::Unknown {
                    self.set_generic_type(second_resolved_type.0, Spanned::new(first_type.clone(), first_span.clone()));
                    Ok(())
                } else {
                    self.unify_type_recursive(
                        first_type,
                        first_span,
                        &second_resolved_type.1.value,
                        &second_resolved_type.1.span
                    )
                }
            }
        }

        let eq = match first_type {
            Type::UserType { user_type_info: first_info, generics: first_generics } => {
                if let Type::UserType { user_type_info: second_info, generics: second_generics } = second_type {
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
                    self.unify_type_recursive(
                        &first_type,
                        first_span,
                        &second_type,
                        second_span
                    )?;
                    true
                } else {
                    false
                }
            },
            Type::Result { value: first_value_type, error: first_error_type } => {
                if let Type::Result { value: second_value_type, error: second_error_type } = second_type {
                    let value_type_result = self.unify_type_recursive(
                        &first_value_type,
                        first_span,
                        &second_value_type,
                        second_span
                    );
                    let error_type_result = self.unify_type_recursive(
                        &first_error_type,
                        first_span,
                        &second_error_type,
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
            let result = self.unify_type_recursive(
                &first_generics[i],
                first_span,
                &second_generics[i],
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

    pub fn resolve_entity_type(&self, entity_id: EntityID) -> Spanned<Type> {
        let mut current_id = entity_id;
        loop {
            let entity_id_or_type = self.entity_type_map.get(&current_id).unwrap();
            match entity_id_or_type {
                Either::Left(entity_id) => current_id = *entity_id,
                Either::Right(ty) => return ty.clone()
            }
        }
    }

    pub fn resolve_generic_type(&self, generic_id: LocalGenericID) -> (LocalGenericID, Spanned<Type>) {
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
    
    pub fn get_type_display_string(&self, ty: &Type) -> String {
        match ty {
            Type::Int8 => "int8".to_string(),
            Type::Int16 => "int16".to_string(),
            Type::Int32 => "int32".to_string(),
            Type::Int64 => "int64".to_string(),
            Type::Uint8 => "uint8".to_string(),
            Type::Uint16 => "uint16".to_string(),
            Type::Uint32 => "uint32".to_string(),
            Type::Uint64 => "uint64".to_string(),
            Type::Float32 => "float32".to_string(),
            Type::Float64 => "float64".to_string(),
            Type::Bool => "bool".to_string(),
            Type::Unit => "unit".to_string(),
            Type::UserType { user_type_info, generics } => {
                let mut name = user_type_info.name.value.clone();
                self.add_generics_info(&mut name, generics);
                name
            },
            Type::Function { function_info, generics } => {
                let mut name = "function".to_string();

                name += "(";
                for i in 0..generics.len() {
                    let argument = &function_info.argument_types[i];
                    if i != 0 {
                        name += ", ";
                    }
                    name += self.get_type_display_string(argument).as_str();
                }
                name += ")";

                self.add_generics_info(&mut name, generics);
                name
            },
            Type::Generic(generic_type) => generic_type.name.clone(),
            Type::LocalGeneric(generic_id) => {
                self.get_type_display_string(&self.resolve_generic_type(*generic_id).1.value)
            },
            Type::Option(value_type) => format!("{}?", self.get_type_display_string(value_type)),
            Type::Result { value, error } => {
                format!("{}!{}", self.get_type_display_string(value), self.get_type_display_string(error))
            },
            Type::Unknown => "unknown".to_string()
        }
    }

    fn add_generics_info(&self, name: &mut String, generics: &Vec<Type>) {
        if !generics.is_empty() {
            *name += "<";
    
            for i in 0..generics.len() {
                if i != 0 {
                    *name += ", ";
                }
                *name += self.get_type_display_string(&generics[i]).as_str();
            }
    
            *name += ">";
        }
    }

    fn add_lazy_type_error_report<T: 'static + LazyTypeReport>(&mut self, report: T) {
        self.lazy_type_reports.push(Box::new(report));
    }

    fn collect_lazy_type_report(&self, errors: &mut Vec<TranspileError>, warnings: &mut Vec<TranspileWarning>) {
        for report in self.lazy_type_reports.iter() {
            match report.build_report(self) {
                Either::Left(error) => errors.push(error),
                Either::Right(warning) => warnings.push(warning),
            }
        }
    }

}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum ImplicitConvert {
    Some,
    Ok,
    Error
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
    force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'allocator>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvert>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    let mut has_type = false;
    let mut var_type_and_spans = Vec::new_in(allocator);

    for i in 0..ast.statements.len() {
        let statement = match &ast.statements[i] {
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
                    implicit_convert_map,
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
                        implicit_convert_map,
                        allocator,
                        errors,
                        warnings,
                        context
                    );

                    let result = type_environment.unify(
                        Spanned::new(EntityID::from(*right_expr), right_expr.get_span()),
                        Spanned::new(EntityID::from(assignment.left_expr), assignment.left_expr.get_span())
                    );
                    add_error(result, type_environment);
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
                    implicit_convert_map,
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
                        implicit_convert_map,
                        allocator,
                        errors,
                        warnings,
                        context
                    );

                    let result = type_environment.unify(
                        Spanned::new(EntityID::from(*right_expr), right_expr.get_span()),
                        Spanned::new(EntityID::from(exchange.left_expr), exchange.left_expr.get_span())
                    );
                    add_error(result, type_environment);
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
                            false,
                            &mut type_environment,
                            implicit_convert_map,
                            allocator,
                            errors,
                            warnings,
                            context
                        );
                    }
                }
            },
            StatementAST::UserTypeDefine(data_struct_define) => {
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
                        false,
                        &mut TypeEnvironment::new(allocator),
                        implicit_convert_map,
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
                        implicit_convert_map,
                        allocator,
                        errors,
                        warnings,
                        context
                    );
                }
            },
            StatementAST::Expression(expression) => {
                let is_last_statement = i == ast.statements.len() - 1;
                let force_be_expression = force_be_expression && is_last_statement;

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
                    force_be_expression,
                    type_environment,
                    implicit_convert_map,
                    allocator,
                    errors,
                    warnings,
                    context
                );

                if force_be_expression {
                    type_environment.set_entity_id_equals(
                        EntityID::from(*expression),
                        EntityID::from(ast)
                    );

                    has_type = true;
                }
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
                            implicit_convert_map,
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

                            let result = type_environment.unify(
                                Spanned::new(EntityID::from(*expression), expression.get_span()),
                                Spanned::new(EntityID::from(variable_define), tag_type_span)
                            );
                            add_error(result, type_environment);
                        }

                        if let Ok(name) = &variable_define.name {
                            var_type_and_spans.push((
                                name.span.clone(),
                                type_environment.resolve_entity_type(EntityID::from(variable_define)).value
                            ));
                        }
                    }
                }
            }
            _ => {}
        }
    }

    type_environment.collect_lazy_type_report(errors, warnings);

    if !has_type {
        type_environment.set_entity_type(
            EntityID::from(ast),
            Spanned::new(Type::Unit, ast.span.clone())
        );
    }

    let mut builder = Report::build(ReportKind::Custom("Debug", Color::Cyan), &context.module_name, 0);

    for var_type_and_span in var_type_and_spans {
        builder.add_label(
            Label::new((&context.module_name, var_type_and_span.0))
                .with_color(Color::Green)
                .with_message(type_environment.get_type_display_string(&var_type_and_span.1))
        );
    }
    
    builder.finish().print((&context.module_name, Source::from(context.source_code.code.as_str()))).unwrap();
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
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvert>,
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
                implicit_convert_map,
                allocator,
                errors,
                warnings,
                context
            );

            let mut results = Vec::new_in(allocator);
            let mut previous = Spanned::new(EntityID::from(&or_expression.left_expr), or_expression.left_expr.span.clone());

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
                        implicit_convert_map,
                        allocator,
                        errors,
                        warnings,
                        context
                    );

                    let current = Spanned::new(EntityID::from(right_expr), right_expr.span.clone());

                    let result = type_environment.unify(previous.clone(),current.clone());
                    results.push(result);

                    previous = current;
                }
            }

            add_errors(results, type_environment);

            if previous.value != EntityID::from(ast) {
                type_environment.set_entity_id_equals(
                    previous.value,
                    EntityID::from(ast)
                );
            }
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
                    implicit_convert_map,
                    allocator,
                    errors,
                    warnings,
                    context
                );

                let result = type_environment.unify_with_return_type(
                    Spanned::new(EntityID::from(expression), expression.get_span())
                );
                add_error(result, type_environment);
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
                            implicit_convert_map,
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
                            false,
                            type_environment,
                            implicit_convert_map,
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
    mut force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'allocator>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvert>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    force_be_expression |= !ast.right_exprs.is_empty();

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
        force_be_expression,
        type_environment,
        implicit_convert_map,
        allocator,
        errors,
        warnings,
        context
    );

    let mut results = Vec::new_in(allocator);
    let mut previous = Spanned::new(EntityID::from(&ast.left_expr), ast.left_expr.span.clone());

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
                force_be_expression,
                type_environment,
                implicit_convert_map,
                allocator,
                errors,
                warnings,
                context
            );
            
            let current = Spanned::new(EntityID::from(right_expr), right_expr.span.clone());

            let result = type_environment.unify(previous.clone(), current.clone());
            results.push(result);

            previous = current;
        }
    }

    add_errors(results, type_environment);

    if previous.value != EntityID::from(ast) {
        type_environment.set_entity_id_equals(
            previous.value,
            EntityID::from(ast)
        );
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
    mut force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'allocator>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvert>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    force_be_expression |= !ast.right_exprs.is_empty();

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
        force_be_expression,
        type_environment,
        implicit_convert_map,
        allocator,
        errors,
        warnings,
        context
    );

    let mut results = Vec::new_in(allocator);
    let mut previous = Spanned::new(EntityID::from(&ast.left_expr), ast.left_expr.span.clone());

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
                force_be_expression,
                type_environment,
                implicit_convert_map,
                allocator,
                errors,
                warnings,
                context
            );

            let current = Spanned::new(EntityID::from(right_expr), right_expr.span.clone());

            let result = type_environment.unify(previous.clone(), current.clone());
            results.push(result);

            previous = current;
        }
    }

    add_errors(results, type_environment);

    if previous.value != EntityID::from(ast) {
        type_environment.set_entity_id_equals(
            previous.value,
            EntityID::from(ast)
        );
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
    mut force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'allocator>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvert>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    force_be_expression |= !ast.right_exprs.is_empty();

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
        force_be_expression,
        type_environment,
        implicit_convert_map,
        allocator,
        errors,
        warnings,
        context
    );

    let mut results = Vec::new_in(allocator);
    let mut previous = Spanned::new(EntityID::from(&ast.left_expr), ast.left_expr.span.clone());

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
                force_be_expression,
                type_environment,
                implicit_convert_map,
                allocator,
                errors,
                warnings,
                context
            );

            let current = Spanned::new(EntityID::from(right_expr), right_expr.span.clone());

            let result = type_environment.unify(previous.clone(), current.clone());
            results.push(result);

            previous = current;
        }
    }

    add_errors(results, type_environment);

    if previous.value != EntityID::from(ast) {
        type_environment.set_entity_id_equals(
            previous.value,
            EntityID::from(ast)
        );
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
    mut force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'allocator>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvert>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    force_be_expression |= !ast.right_exprs.is_empty();

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
        force_be_expression,
        type_environment,
        implicit_convert_map,
        allocator,
        errors,
        warnings,
        context
    );

    let mut results = Vec::new_in(allocator);
    let mut previous = Spanned::new(EntityID::from(&ast.left_expr), ast.left_expr.span.clone());

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
                force_be_expression,
                type_environment,
                implicit_convert_map,
                allocator,
                errors,
                warnings,
                context
            );

            let current = Spanned::new(EntityID::from(right_expr), right_expr.span.clone());

            let result = type_environment.unify(previous.clone(), current.clone());
            results.push(result);

            previous = current;
        }
    }

    add_errors(results, type_environment);

    if previous.value != EntityID::from(ast) {
        type_environment.set_entity_id_equals(
            previous.value,
            EntityID::from(ast)
        );
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
    mut force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'allocator>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvert>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    force_be_expression |= ast.right_exprs.is_empty();

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
        force_be_expression,
        type_environment,
        implicit_convert_map,
        allocator,
        errors,
        warnings,
        context
    );

    let mut results = Vec::new_in(allocator);
    let mut previous = Spanned::new(EntityID::from(&ast.left_expr), ast.left_expr.span.clone());

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
                force_be_expression,
                type_environment,
                implicit_convert_map,
                allocator,
                errors,
                warnings,
                context
            );

            let current = Spanned::new(EntityID::from(right_expr), right_expr.span.clone());

            let result = type_environment.unify(previous.clone(), current.clone());
            results.push(result);

            previous = current;
        }
    }

    add_errors(results, type_environment);

    if previous.value != EntityID::from(ast) {
        type_environment.set_entity_id_equals(
            previous.value,
            EntityID::from(ast)
        );
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
    force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'allocator>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvert>,
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
            force_be_expression,
            type_environment,
            implicit_convert_map,
            allocator,
            errors,
            warnings,
            context
        );
        // TODO - check bounds

        type_environment.set_entity_id_equals(
            EntityID::from(primary),
            EntityID::from(ast)
        );
    } else {
        type_environment.set_entity_type(
            EntityID::from(ast),
            Spanned::new(Type::Unknown, ast.span.clone())
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
    force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'allocator>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvert>,
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
        force_be_expression,
        type_environment,
        implicit_convert_map,
        allocator,
        errors,
        warnings,
        context
    );

    let mut previous = EntityID::from(&ast.left);
    for primary_right in ast.chain.iter() {
        type_inference_primary_right(
            primary_right,
            previous,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            generics_map,
            module_entity_type_map,
            type_environment,
            implicit_convert_map,
            allocator,
            errors,
            warnings,
            context
        );
        previous = EntityID::from(primary_right);
    }

    type_environment.set_entity_id_equals(
        previous,
        EntityID::from(ast)
    );
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
    force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'allocator>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvert>,
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
                            force_be_expression,
                            type_environment,
                            implicit_convert_map,
                            allocator,
                            errors,
                            warnings,
                            context
                        );
                        type_environment.set_entity_id_equals(
                            EntityID::from(*expression),
                            EntityID::from(&simple.0)
                        );
                    }
                },
                SimplePrimary::Identifier(identifier) => {
                    if let Some(resolved) = name_resolved_map.get(&EntityID::from(identifier)) {
                        let link = match resolved.define_info.define_kind {
                            DefineKind::Import => false,
                            DefineKind::Function => {
                                // TODO - check is method
                                true
                            },
                            DefineKind::Variable | DefineKind::FunctionArgument | DefineKind::ClosureArgument => {
                                if resolved.has_separator(&[EnvironmentSeparatorKind::DataStruct, EnvironmentSeparatorKind::Function]) {
                                    errors.push(OutOfEnvironmentVariable::new(identifier.span.clone(), resolved));
                                    false
                                } else {
                                    true
                                }
                            },
                            DefineKind::UserType => false,
                            DefineKind::Generics => false,
                        };

                        if link {
                            type_environment.set_entity_id_equals(
                                resolved.define_info.entity_id,
                                EntityID::from(&simple.0)
                            );
                        } else {
                            type_environment.set_entity_type(
                                EntityID::from(&simple.0),
                                Spanned::new(Type::Unknown, identifier.span.clone())
                            );
                        }
                    } else {
                        let text = identifier.value;
                        let ty = if text.parse::<i32>().is_ok() {
                            Type::Int32
                        } else if text.parse::<i64>().is_ok() {
                            Type::Int64
                        } else if text.parse::<f32>().is_ok() {
                            Type::Float32
                        } else if text.parse::<f64>().is_ok() {
                            Type::Float64
                        } else {
                            Type::Unknown
                        };

                        type_environment.set_entity_type(
                            EntityID::from(&simple.0),
                            Spanned::new(ty, identifier.span.clone())
                        );
                    }
                },
                SimplePrimary::NullKeyword(null_keyword_span) => {
                    let generic_id = type_environment.new_local_generic_id(null_keyword_span.clone());
                    type_environment.set_entity_type(
                        EntityID::from(&simple.0),
                        Spanned::new(Type::Option(Arc::new(Type::LocalGeneric(generic_id))), null_keyword_span.clone())
                    );
                }
            }

            if let Some(function_call) = &simple.1 {
                type_inference_function_call(
                    function_call,
                    EntityID::from(&simple.0),
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    type_environment,
                    implicit_convert_map,
                    allocator,
                    errors,
                    warnings,
                    context
                );
                
                type_environment.set_entity_id_equals(
                    EntityID::from(function_call),
                    EntityID::from(&ast.first_expr)
                );
            } else {
                type_environment.set_entity_id_equals(
                    EntityID::from(&simple.0),
                    EntityID::from(&ast.first_expr)
                );
            }
        },
        PrimaryLeftExpr::NewExpression(new_expression) => {
            let user_type = if new_expression.path.len() > 1 {
                let module_name = get_module_name_from_new_expression(new_expression, import_element_map, name_resolved_map);
                
                if let Some(user_type_map) = module_user_type_map.get(&module_name) {
                    if let Some(user_type) = user_type_map.get(new_expression.path.last().unwrap().value) {
                        user_type.clone()
                    } else {
                        Type::Unknown
                    }
                } else {
                    Type::Unknown
                }
            } else {
                if let Some(name) = new_expression.path.last() {
                    if let Some(resolved) = name_resolved_map.get(&EntityID::from(name)) {
                        match resolved.define_info.define_kind {
                            DefineKind::Import => {
                                if let Some(module_name) = import_element_map.get(&resolved.define_info.entity_id) {
                                    if let Some(user_type_map) = module_user_type_map.get(module_name) {
                                        if let Some(user_type) = user_type_map.get(name.value) {
                                            user_type.clone()
                                        } else {
                                            Type::Unknown
                                        }
                                    } else {
                                        Type::Unknown
                                    }
                                } else {
                                    Type::Unknown
                                }
                            },
                            DefineKind::UserType => {
                                user_type_map.get(name.value).unwrap().clone()
                            },
                            DefineKind::Generics => {
                                Type::Generic(generics_map.get(&resolved.define_info.entity_id).unwrap().clone())
                            },
                            _ => Type::Unknown
                        }
                    } else {
                        Type::Unknown
                    }
                } else {
                    Type::Unknown
                }
            };

            if let Type::UserType { user_type_info, generics } = user_type {
                let number_of_generics = user_type_info.generics_define.len();
                let mut generics = Vec::with_capacity(number_of_generics);
                for _ in 0..number_of_generics {
                    let local_generic_id = type_environment.new_local_generic_id(new_expression.span.clone());
                    generics.push(Type::LocalGeneric(local_generic_id));
                }
                let user_type = Type::UserType { user_type_info, generics: Arc::new(generics) };

                if let Ok(field_assigns) = &new_expression.field_assigns {
                    for field_assign in field_assigns.iter() {
                        if let Some(element_type) = user_type.get_element_type_with_local_generic(field_assign.name.value) {
                            type_environment.set_entity_type(
                                EntityID::from(field_assign),
                                Spanned::new(element_type, field_assign.name.span.clone())
                            );

                            if let Ok(expression) = &field_assign.expression {
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
                                    force_be_expression,
                                    type_environment,
                                    implicit_convert_map,
                                    allocator,
                                    errors,
                                    warnings,
                                    context
                                );

                                let result = type_environment.unify(
                                    Spanned::new(EntityID::from(field_assign), field_assign.name.span.clone()),
                                    Spanned::new(EntityID::from(*expression), expression.get_span())
                                );
                                add_error(result, type_environment);
                            }
                        } else {
                            let error = NotFoundTypeElementError {
                                user_type: user_type.clone(),
                                name: field_assign.name.clone().map(|name| { name.to_string() }),
                            };
                            type_environment.add_lazy_type_error_report(error);
                        }
                    }
                }

                type_environment.set_entity_type(
                    EntityID::from(&ast.first_expr),
                    Spanned::new(user_type, new_expression.span.clone())
                );
            } else {
                if !new_expression.path.is_empty() {
                    let span_start = new_expression.path.first().unwrap().span.start;
                    let span_end = new_expression.path.last().unwrap().span.end;
                    let span = span_start..span_end;

                    let error = SimpleError::new(
                        0036,
                        span.clone(),
                        vec![],
                        vec![(span, Color::Red)]
                    );
                    errors.push(error);
                }
                
                type_environment.set_entity_type(
                    EntityID::from(&ast.first_expr),
                    Spanned::new(Type::Unknown, new_expression.span.clone())
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
                    force_be_expression,
                    type_environment,
                    implicit_convert_map,
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
                    force_be_expression,
                    type_environment,
                    implicit_convert_map,
                    allocator,
                    errors,
                    warnings,
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
                                    force_be_expression,
                                    type_environment,
                                    implicit_convert_map,
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
                                    force_be_expression,
                                    type_environment,
                                    implicit_convert_map,
                                    allocator,
                                    errors,
                                    warnings,
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
                                force_be_expression,
                                type_environment,
                                implicit_convert_map,
                                allocator,
                                errors,
                                warnings,
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
                    force_be_expression,
                    type_environment,
                    implicit_convert_map,
                    allocator,
                    errors,
                    warnings,
                    context
                );
            }
        }
    }

    if let Some(mapping_operator) = &ast.mapping_operator {
        type_inference_mapping_operator(
            mapping_operator,
            EntityID::from(&ast.first_expr),
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            generics_map,
            module_entity_type_map,
            type_environment,
            implicit_convert_map,
            allocator,
            errors,
            warnings,
            context
        );

        type_environment.set_entity_id_equals(
            EntityID::from(&mapping_operator.value),
            EntityID::from(ast)
        );
    } else {
        type_environment.set_entity_id_equals(
            EntityID::from(&ast.first_expr),
            EntityID::from(ast)
        );
    }
}

fn type_inference_primary_right<'allocator>(
    ast: &PrimaryRight,
    previous_primary: EntityID,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    type_environment: &mut TypeEnvironment<'allocator>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvert>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    let second_expr_id = if let Some(second_expr) = &ast.second_expr {
        let user_type = type_environment.resolve_entity_type(previous_primary);
        let literal_entity_id = EntityID::from(&second_expr.0);
        let element_type = match user_type.value.get_element_type_with_local_generic(second_expr.0.value) {
            Some(element_type) => element_type,
            _ => {
                let error = NotFoundTypeElementError {
                    user_type: user_type.value,
                    name: second_expr.0.clone().map(|name| { name.to_string() })
                };
                type_environment.add_lazy_type_error_report(error);

                Type::Unknown
            }
        };
        type_environment.set_entity_type(
            literal_entity_id,
            Spanned::new(element_type, second_expr.0.span.clone())
        );

        if let Some(function_call) = &second_expr.1 {
            type_inference_function_call(
                function_call,
                literal_entity_id,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                module_entity_type_map,
                type_environment,
                implicit_convert_map,
                allocator,
                errors,
                warnings,
                context
            );

            EntityID::from(function_call)
        } else {
            literal_entity_id
        }
    } else {
        previous_primary
    };

    if let Some(mapping_operator) = &ast.mapping_operator {
        type_inference_mapping_operator(
            mapping_operator,
            second_expr_id,
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            generics_map,
            module_entity_type_map,
            type_environment,
            implicit_convert_map,
            allocator,
            errors,
            warnings,
            context
        );
    }
}

fn type_inference_mapping_operator<'allocator>(
    ast: &MappingOperator,
    previous_entity_id: EntityID,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    type_environment: &mut TypeEnvironment<'allocator>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvert>,
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
            true,
            type_environment,
            implicit_convert_map,
            allocator,
            errors,
            warnings,
            context
        );
    }
}

fn type_inference_function_call<'allocator>(
    ast: &FunctionCall,
    function: EntityID,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    type_environment: &mut TypeEnvironment<'allocator>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvert>,
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
                true,
                type_environment,
                implicit_convert_map,
                allocator,
                errors,
                warnings,
                context
            );
        }
    }
}


pub(crate) trait LazyTypeReport {
    fn build_report(&self, type_environment: &TypeEnvironment) -> Either<TranspileError, TranspileWarning>;
}


pub(crate) struct TypeMismatchError {
    type_0: Spanned<Type>,
    type_1: Spanned<Type>,
    generics: Vec<(Spanned<Type>, Spanned<Type>)>
}

impl LazyTypeReport for TypeMismatchError {
    fn build_report(&self, type_environment: &TypeEnvironment) -> Either<TranspileError, TranspileWarning> {
        let func = |ty| { type_environment.get_type_display_string(&ty) };

        let type_0 = self.type_0.clone().map(func);
        let type_1 = self.type_1.clone().map(func);
        let generics = self.generics.iter()
            .map(|generic| { (generic.0.clone().map(func), generic.1.clone().map(func)) })
            .collect();
        
        let error = TypeMismatchErrorReport {
            type_0,
            type_1,
            generics
        };
        Either::Left(TranspileError::new(error))
    }
}

struct TypeMismatchErrorReport {
    type_0: Spanned<String>,
    type_1: Spanned<String>,
    generics: Vec<(Spanned<String>, Spanned<String>)>
}

impl TranspileReport for TypeMismatchErrorReport {
    fn print(&self, context: &TranspileModuleContext) {
        let module_name = &context.module_name;
        let text = &context.context.localized_text;
        let error_code = 0034;
        let key = ErrorMessageKey::new(error_code);

        let message = key.get_massage(text, ErrorMessageType::Message);

        let mut builder = Report::build(ReportKind::Error, module_name, self.type_0.span.start)
            .with_code(error_code)
            .with_message(message);

        builder.add_label(
            Label::new((module_name, self.type_0.span.clone()))
                .with_color(Color::Red)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(0))
                        .replace("%type", self.type_0.value.clone().fg(Color::Red).to_string().as_str())
                )
        );

        builder.add_label(
            Label::new((module_name, self.type_1.span.clone()))
                .with_color(Color::Red)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(0))
                        .replace("%type", self.type_1.value.clone().fg(Color::Red).to_string().as_str())
                )
        );

        let mut color_generator = ColorGenerator::new();
        let mut generic_ids = String::new();
        for i in 0..self.generics.len() {
            let generic = &self.generics[i];
            let color = color_generator.next();

            let message_0 = key.get_massage(text, ErrorMessageType::Label(1))
                .replace("%generic", format!("'{}", i.to_string()).fg(color).to_string().as_str())
                .replace("%type", generic.0.value.clone().fg(color).to_string().as_str());

            builder.add_label(
                Label::new((module_name, generic.0.span.clone()))
                    .with_color(color)
                    .with_message(message_0)
            );

            let message_1 = key.get_massage(text, ErrorMessageType::Label(1))
                .replace("%generic", format!("'{}", i.to_string()).fg(color).to_string().as_str())
                .replace("%type", generic.1.value.clone().fg(color).to_string().as_str());

            builder.add_label(
                Label::new((module_name, generic.1.span.clone()))
                    .with_color(color)
                    .with_message(message_1)
            );

            if i != 0 {
                generic_ids += " ";
            }
            generic_ids += format!("'{}", i).fg(color).to_string().as_str();
        }

        if !self.generics.is_empty() {
            builder.set_note(key.get_massage(text, ErrorMessageType::Note).replace("%generics", &generic_ids));
        }

        if let Some(help) = key.get_massage_optional(text, ErrorMessageType::Help) {
            builder.set_help(help);
        }

        builder.finish().print((module_name, Source::from(context.source_code.code.as_str()))).unwrap();
    }
}


fn add_error(result: Result<(), TypeMismatchError>, type_environment: &mut TypeEnvironment) {
    if let Err(error) = result {
        type_environment.lazy_type_reports.push(Box::new(error));
    }
}


fn add_errors<A: Allocator>(results: Vec<Result<(), TypeMismatchError>, A>, type_environment: &mut TypeEnvironment) {

}


struct OutOfEnvironmentVariable {
    identifier_span: Range<usize>,
    defined_span: Range<usize>,
    environment_spans: Vec<Range<usize>>
}

impl OutOfEnvironmentVariable {
    
    pub fn new(identifier_span: Range<usize>, resolved: &FoundDefineInfo) -> TranspileError {
        let defined_span = resolved.define_info.span.clone();
        let mut environment_spans = Vec::new();
        for separator in resolved.separators.iter() {
            let across = match separator.value {
                EnvironmentSeparatorKind::Function => false,
                EnvironmentSeparatorKind::DataStruct => false,
                EnvironmentSeparatorKind::Closure => true
            };

            if !across {
                environment_spans.push(separator.span.clone());
            }
        }
        
        TranspileError::new(
            OutOfEnvironmentVariable {
                identifier_span,
                defined_span,
                environment_spans,
            }
        )
    }

}

impl TranspileReport for OutOfEnvironmentVariable {
    fn print(&self, context: &TranspileModuleContext) {
        let module_name = &context.module_name;
        let text = &context.context.localized_text;
        let error_code = 0035;
        let key = ErrorMessageKey::new(error_code);

        let message = key.get_massage(text, ErrorMessageType::Message);

        let mut builder = Report::build(ReportKind::Error, module_name, self.identifier_span.start)
            .with_code(error_code)
            .with_message(message);

        builder.add_label(
            Label::new((module_name, self.identifier_span.clone()))
                .with_color(Color::Red)
                .with_message(key.get_massage(text, ErrorMessageType::Label(0)))
        );

        builder.add_label(
            Label::new((module_name, self.defined_span.clone()))
                .with_color(Color::Yellow)
                .with_message(key.get_massage(text, ErrorMessageType::Label(1)))
        );

        for environment_span in self.environment_spans.iter() {
            builder.add_label(
                Label::new((module_name, environment_span.clone()))
                    .with_color(Color::Yellow)
                    .with_message(key.get_massage(text, ErrorMessageType::Label(2)))
            );
        }

        if let Some(note) = key.get_massage_optional(text, ErrorMessageType::Note) {
            builder.set_note(note);
        }

        if let Some(help) = key.get_massage_optional(text, ErrorMessageType::Help) {
            builder.set_help(help);
        }

        builder.finish().print((module_name, Source::from(context.source_code.code.as_str()))).unwrap();
    }
}



struct NotFoundTypeElementError {
    user_type: Type,
    name: Spanned<String>
}

impl LazyTypeReport for NotFoundTypeElementError {
    fn build_report(&self, type_environment: &TypeEnvironment) -> Either<TranspileError, TranspileWarning> {
        let error = SimpleError::new(
            0037,
            self.name.span.clone(),
            vec![type_environment.get_type_display_string(&self.user_type), self.name.value.to_string()],
            vec![(self.name.span.clone(), Color::Red)]
        );
        Either::Left(error)
    }
}