use std::{alloc::Allocator, ops::{Deref, Range}, sync::Arc};

use ariadne::{sources, Color, ColorGenerator, Fmt, Label, Report, ReportKind, Source};
use bumpalo::Bump;
use catla_parser::parser::{AddOrSubExpression, AndExpression, Block, Closure, CompareExpression, EQNEExpression, Expression, ExpressionEnum, Factor, FunctionCall, Generics, MappingOperator, MappingOperatorKind, MulOrDivExpression, Primary, PrimaryLeft, PrimaryLeftExpr, PrimaryRight, PrimarySeparatorKind, Program, SimplePrimary, Spanned, StatementAST, StatementAttributeKind, StatementAttributes, UserTypeKindEnum};
use either::Either;
use fxhash::{FxHashMap, FxHashSet};
use hashbrown::{hash_map::DefaultHashBuilder, HashMap};
use indexmap::IndexMap;

use crate::transpiler::{advice::Advice, component::EntityID, context::TranspileModuleContext, error::{ErrorMessageKey, ErrorMessageType, SimpleError, TranspileReport}, name_resolver::{DefineKind, EnvironmentSeparatorKind, FoundDefineInfo}, TranspileError, TranspileWarning};

use super::{import_module_collector::{get_module_name_from_new_expression, get_module_name_from_primary}, type_info::{Bound, CollectedImplementation, FreezableMutex, FunctionDefineInfo, FunctionType, GenericType, ImplementsInfo, ImplementsInfoSet, LocalGenericID, OverrideElementsEnvironment, ScopeThisType, Type, WhereBound, WithDefineInfo}, user_type_element_collector::{get_type, parse_primitive_type}};



const IF_CONDITION_TYPE_ERROR: usize = 0038;
const FUNCTION_CALL_TYPE_ERROR: usize = 0039;
const MAPPING_OPERATOR_TYPE_ERROR: usize = 0041;
const BOUNDS_NOT_SATISFIED_ERROR: usize = 0046;
const INVALID_SET_GENERICS_TYPE_ERROR: usize = 0047;
const NUMBER_OF_GENERICS_MISMATCH_ERROR: usize = 0048;
const WHERE_BOUNDS_NOT_SATISFIED_ERROR: usize = 0050;
const DUPLICATED_ELEMENT_ERROR: usize = 0051;
const UNRESOLVED_INTERFACE: usize = 0052;
const INVALID_OVERRIDE_ERROR: usize = 0056;


pub(crate) struct TypeEnvironment<'allocator, 'input> {
    entity_type_map: HashMap<EntityID, Either<EntityID, Spanned<Type>>, DefaultHashBuilder, &'allocator Bump>,
    generic_type_map: HashMap<LocalGenericID, Either<LocalGenericID, Spanned<Type>>, DefaultHashBuilder, &'allocator Bump>,
    generic_bounds_checks: Vec<GenericsBoundCheck, &'allocator Bump>,
    impl_interface_generics_check: Vec<(Range<usize>, WithDefineInfo<Type>), &'allocator Bump>,
    implicit_convert_map: HashMap<EntityID, ImplicitConvertKind, DefaultHashBuilder, &'allocator Bump>,
    return_type: Either<EntityID, Spanned<Type>>,
    closures: Vec<&'allocator Closure<'allocator, 'input>, &'allocator Bump>,
    lazy_type_reports: Vec<Box<dyn LazyTypeReport>, &'allocator Bump>,
    current_generics_id: usize
}

impl<'allocator, 'input> TypeEnvironment<'allocator, 'input> {
    
    pub fn new(allocator: &'allocator Bump) -> TypeEnvironment<'allocator, 'input> {
        Self::new_with_return_type(
            Either::Right(Spanned::new(Type::Unit, 0..0)),
            allocator
        )
    }

    pub fn new_with_return_type(return_type: Either<EntityID, Spanned<Type>>, allocator: &'allocator Bump) -> TypeEnvironment<'allocator, 'input> {
        Self {
            entity_type_map: HashMap::new_in(allocator),
            generic_type_map: HashMap::new_in(allocator),
            generic_bounds_checks: Vec::new_in(allocator),
            impl_interface_generics_check: Vec::new_in(allocator),
            implicit_convert_map: HashMap::new_in(allocator),
            return_type,
            closures: Vec::new_in(allocator),
            lazy_type_reports: Vec::new_in(allocator),
            current_generics_id: 0
        }
    }

    pub fn new_local_generic_id(
        &mut self,
        type_span: Range<usize>
    ) -> LocalGenericID {
        self.current_generics_id += 1;
        let generic_id = LocalGenericID(self.current_generics_id);
        
        self.generic_type_map.insert(
            generic_id,
            Either::Right(Spanned::new(Type::Unknown, type_span.clone()))
        );
        
        generic_id
    }

    pub fn get_user_or_function_type_with_local_generic_id(
        &mut self,
        ty: Spanned<Type>,
        current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
        register_bounds_check: bool
    ) -> Spanned<Type> {
        let generics_define = match &ty.value {
            Type::UserType { user_type_info, generics: _, generics_span: _ } => {
                &user_type_info.generics_define
            },
            Type::Function { function_info, generics: _ } => {
                &function_info.generics_define
            },
            _ => return ty.clone()
        };

        let number_of_generics = generics_define.len();
        let mut generics = Vec::with_capacity(number_of_generics);
        for _ in 0..number_of_generics {
            let generic_id = self.new_local_generic_id( ty.span.clone());
            generics.push(Type::LocalGeneric(generic_id));
        }
        let generics = Arc::new(generics);

        let ty = match &ty.value {
            Type::UserType { user_type_info, generics: _, generics_span: _ } => {
                Spanned::new(Type::UserType { user_type_info: user_type_info.clone(), generics: generics.clone(), generics_span: None }, ty.span)
            },
            Type::Function { function_info, generics: _ } => {
                Spanned::new(Type::Function { function_info: function_info.clone(), generics: generics.clone() }, ty.span)
            },
            _ => unreachable!()
        };
        
        for (generic_define, local_generic) in ty.value.get_generics_define_with_replaced_generic().unwrap().iter().zip(generics.iter()) {
            self.generic_bounds_checks.push(GenericsBoundCheck::Generics {
                type_span: ty.span.clone(),
                ty: Spanned::new(local_generic.clone(), ty.span.clone()),
                generic_define: generic_define.clone(),
                scope_implements_info_set: current_scope_implements_info_set.clone()
            });
        }

        if register_bounds_check {
            let where_bounds = ty.value.get_where_bounds_with_replaced_generic().unwrap();
            for where_bound in where_bounds.iter() {
                self.generic_bounds_checks.push(GenericsBoundCheck::Where {
                    type_span: ty.span.clone(),
                    target_type: where_bound.target_type.value.clone(),
                    bounds: where_bound.bounds.clone(),
                    scope_implements_info_set: current_scope_implements_info_set.clone()
                });
            }
        }

        ty
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
            Either::Right(_) => {
                self.entity_type_map.insert(entity_id, Either::Right(ty));
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
            Either::Right(_) => {
                self.generic_type_map.insert(generic_id, Either::Right(ty));
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
        second_entity_id: Spanned<EntityID>,
        current_scope_this_type: &ScopeThisType
    ) -> Result<(), TypeMismatchError> {
        
        let mut first_resolved = self.resolve_entity_type_with_id(first_entity_id.value);
        let mut second_resolved = self.resolve_entity_type_with_id(second_entity_id.value);
        first_resolved.1.span = first_entity_id.span.clone();
        second_resolved.1.span = second_entity_id.span.clone();

        match (first_resolved.1.value.is_replaceable(), second_resolved.1.value.is_replaceable()) {
            (true, true) => {
                self.entity_type_map.insert(second_resolved.0, Either::Left(first_resolved.0));
            },
            (true, false) => {
                self.entity_type_map.insert(first_resolved.0, Either::Right(second_resolved.1));
                self.entity_type_map.insert(second_resolved.0, Either::Left(first_resolved.0));
            },
            (false, true) => {
                self.entity_type_map.insert(second_resolved.0, Either::Left(first_resolved.0));
            },
            (false, false) => {
                return self.unify_type(
                    &first_resolved.1.value,
                    &first_resolved.1.span,
                    &second_resolved.1.value,
                    &second_resolved.1.span,
                    current_scope_this_type,
                    false
                );
            }
        }
        
        Ok(())
    }

    pub fn unify_with_implicit_convert(
        &mut self,
        first_entity_id: Spanned<EntityID>,
        second_entity_id: Spanned<EntityID>,
        current_scope_this_type: &ScopeThisType,
        first_is_expr: bool
    ) -> Result<(), TypeMismatchError> {

        let mut first_resolved = self.resolve_entity_type_with_id(first_entity_id.value);
        let mut second_resolved = self.resolve_entity_type_with_id(second_entity_id.value);
        first_resolved.1.span = first_entity_id.span.clone();
        second_resolved.1.span = second_entity_id.span.clone();

        match (first_resolved.1.value.is_replaceable(), second_resolved.1.value.is_replaceable()) {
            (true, true) => {
                self.entity_type_map.insert(second_resolved.0, Either::Left(first_resolved.0));
            },
            (true, false) => {
                self.entity_type_map.insert(first_resolved.0, Either::Right(second_resolved.1));
                self.entity_type_map.insert(second_resolved.0, Either::Left(first_resolved.0));
            },
            (false, true) => {
                self.entity_type_map.insert(second_resolved.0, Either::Left(first_resolved.0));
            },
            (false, false) => {
                let result = self.unify_type_with_implicit_convert(
                    &first_resolved.1.value,
                    &first_resolved.1.span,
                    &second_resolved.1.value,
                    &second_resolved.1.span,
                    current_scope_this_type,
                    first_is_expr
                )?;
        
                if let Some(convert_kind) = result {
                    let entity_id = if first_is_expr {
                        first_entity_id.value
                    } else {
                        second_entity_id.value
                    };
                    self.implicit_convert_map.insert(entity_id, convert_kind);
                }
            }
        }
        
        Ok(())
    }

    fn unify_type_with_implicit_convert(
        &mut self,
        first_type: &Type,
        first_span: &Range<usize>,
        second_type: &Type,
        second_span: &Range<usize>,
        current_scope_this_type: &ScopeThisType,
        first_is_expr: bool
    ) -> Result<Option<ImplicitConvertKind>, TypeMismatchError> {
        if first_is_expr {
            if second_type.is_option_or_result() {
                let ty = if let Type::LocalGeneric(generic_id) = first_type {
                    self.resolve_generic_type(*generic_id).1.value
                } else {
                    first_type.clone()
                };

                let mut implicit_convert = None;

                let new_first_type = if let Type::Option(_) = second_type {
                    if let Type::Option(_) = &ty {
                        ty
                    } else {
                        implicit_convert = Some(ImplicitConvertKind::Some);
                        Type::Option(Arc::new(ty))
                    }
                } else if let Type::Result { value, error } = second_type {
                    if let Type::Result { value: _, error: _ } = &ty {
                        ty
                    } else if let Type::Unit = &ty {
                        // TODO - replace with std error type
                        implicit_convert = Some(ImplicitConvertKind::Error);
                        Type::Result { value: value.clone(), error: Arc::new(ty) }
                    } else {
                        implicit_convert = Some(ImplicitConvertKind::Ok);
                        Type::Result { value: Arc::new(ty), error: error.clone() }
                    }
                } else {
                    unreachable!()
                };

                self.unify_type(
                    &new_first_type,
                    first_span,
                    second_type,
                    second_span,
                    current_scope_this_type,
                    false
                )?;
                Ok(implicit_convert)
            } else {
                self.unify_type(
                    first_type,
                    first_span,
                    second_type,
                    second_span,
                    current_scope_this_type,
                    false
                )?;
                Ok(None)
            }
        } else {
            if first_type.is_option_or_result() {
                let ty = if let Type::LocalGeneric(generic_id) = second_type {
                    self.resolve_generic_type(*generic_id).1.value
                } else {
                    second_type.clone()
                };

                let mut implicit_convert = None;

                let new_second_type = if let Type::Option(_) = &first_type {
                    if let Type::Option(_) = &ty {
                        ty
                    } else {
                        implicit_convert = Some(ImplicitConvertKind::Some);
                        Type::Option(Arc::new(ty))
                    }
                } else if let Type::Result { value, error } = &first_type {
                    if let Type::Result { value: _, error: _ } = &ty {
                        ty
                    } else if let Type::Unit = &ty {
                        // TODO - replace with std error type
                        implicit_convert = Some(ImplicitConvertKind::Error);
                        Type::Result { value: value.clone(), error: Arc::new(ty) }
                    } else {
                        implicit_convert = Some(ImplicitConvertKind::Ok);
                        Type::Result { value: Arc::new(ty), error: error.clone() }
                    }
                } else {
                    unreachable!()
                };

                self.unify_type(
                    first_type,
                    first_span,
                    &new_second_type,
                    second_span,
                    current_scope_this_type,
                    false
                )?;
                Ok(implicit_convert)
            } else {
                self.unify_type(
                    first_type,
                    first_span,
                    second_type,
                    second_span,
                    current_scope_this_type,
                    false
                )?;
                Ok(None)
            }
        }
    }

    pub fn unify_with_return_type(
        &mut self,
        return_expr_entity_id: Spanned<EntityID>,
        current_scope_this_type: &ScopeThisType
    ) -> Result<(), TypeMismatchError> {

        let return_type = match &self.return_type {
            Either::Left(entity_id) => self.resolve_entity_type(*entity_id),
            Either::Right(ty) => ty.clone(),
        };
        let mut return_expr_resolved = self.resolve_entity_type(return_expr_entity_id.value);
        return_expr_resolved.span = return_expr_entity_id.span.clone();

        let result = self.unify_type_with_implicit_convert(
            &return_type.value,
            &return_type.span,
            &return_expr_resolved.value,
            &return_expr_resolved.span,
            current_scope_this_type,
            false
        );

        if let Ok(convert_kind) = &result {
            if let Some(convert_kind) = convert_kind {
                self.implicit_convert_map.insert(return_expr_entity_id.value, *convert_kind);
            }
        }

        result?;
        Ok(())
    }

    pub fn unify_type(
        &mut self,
        first_type: &Type,
        first_span: &Range<usize>,
        second_type: &Type,
        second_span: &Range<usize>,
        current_scope_this_type: &ScopeThisType,
        allow_unknown: bool
    ) -> Result<(), TypeMismatchError> {

        let result = self.unify_type_recursive(
            first_type,
            first_span,
            second_type,
            second_span,
            current_scope_this_type,
            allow_unknown
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
        second_span: &Range<usize>,
        current_scope_this_type: &ScopeThisType,
        allow_unknown: bool
    ) -> Result<(), Vec<(Spanned<Type>, Spanned<Type>)>> {

        if allow_unknown {
            if first_type == &Type::Unknown || second_type == &Type::Unknown {
                return Ok(());
            }
        }
        
        if first_type == &Type::Unreachable || second_type == &Type::Unreachable {
            return Ok(());
        }

        if let Type::LocalGeneric(first_generic_id) = first_type {
            let first_resolved_type = self.resolve_generic_type(*first_generic_id);

            if let Type::LocalGeneric(second_generic_id) = second_type {
                let second_resolved_type = self.resolve_generic_type(*second_generic_id);

                match (first_resolved_type.1.value.is_replaceable(), second_resolved_type.1.value.is_replaceable()) {
                    (true, true) => {
                        self.generic_type_map.insert(second_resolved_type.0, Either::Left(first_resolved_type.0));
                    },
                    (true, false) => {
                        self.generic_type_map.insert(first_resolved_type.0, Either::Right(second_resolved_type.1));
                        self.generic_type_map.insert(second_resolved_type.0, Either::Left(first_resolved_type.0));
                    },
                    (false, true) => {
                        self.generic_type_map.insert(second_resolved_type.0, Either::Left(first_resolved_type.0));
                    },
                    (false, false) => {
                        return self.unify_type_recursive(
                            &first_resolved_type.1.value,
                            &first_resolved_type.1.span,
                            &second_resolved_type.1.value,
                            &second_resolved_type.1.span,
                            current_scope_this_type,
                            allow_unknown
                        );
                    }
                }

                return Ok(());
            } else {
                self.set_generic_type(
                    *first_generic_id,
                    Spanned::new(second_type.clone(), second_span.clone())
                );
                return Ok(());
            }
        } else {
            if let Type::LocalGeneric(second_generic_id) = second_type {
                self.set_generic_type(
                    *second_generic_id,
                    Spanned::new(first_type.clone(), first_span.clone())
                );
                return Ok(());
            }
        }


        if &Type::This != &current_scope_this_type.ty {
            if let Type::This = first_type {
                return self.unify_type_recursive(
                    &current_scope_this_type.ty,
                    first_span,
                    second_type,
                    second_span,
                    current_scope_this_type,
                    allow_unknown
                );
            }
            if let Type::This = second_type {
                return self.unify_type_recursive(
                    first_type,
                    first_span,
                    &current_scope_this_type.ty,
                    second_span,
                    current_scope_this_type,
                    allow_unknown
                );
            }
        }

        let eq = match first_type {
            Type::UserType { user_type_info: first_info, generics: first_generics, generics_span: _ } => {
                if let Type::UserType { user_type_info: second_info, generics: second_generics, generics_span: _ } = second_type {
                    if first_info == second_info {
                        self.unify_generics(
                            first_generics,
                            first_span,
                            second_generics,
                            second_span,
                            current_scope_this_type,
                            allow_unknown
                        )?;
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
                    if first_function_info.argument_types.len() != second_function_info.argument_types.len() {
                        false
                    } else {
                        for (first, second) in first_function_info.argument_types.iter().zip(second_function_info.argument_types.iter()) {
                            self.unify_type_recursive(
                                first,
                                first_span,
                                second,
                                second_span,
                                current_scope_this_type,
                                allow_unknown
                            )?;
                        }

                        self.unify_type_recursive(
                            &first_function_info.return_type.value,
                            first_span,
                            &second_function_info.return_type.value,
                            second_span,
                            current_scope_this_type,
                            allow_unknown
                        )?;

                        self.unify_generics(
                            first_generics,
                            first_span,
                            second_generics,
                            second_span,
                            current_scope_this_type,
                            allow_unknown
                        )?;
                        true
                    }
                } else {
                    false
                }
            },
            Type::LocalGeneric(_) => unreachable!(),
            Type::Option(first_type) => {
                if let Type::Option(second_type) = second_type {
                    self.unify_type_recursive(
                        &first_type,
                        first_span,
                        &second_type,
                        second_span,
                        current_scope_this_type,
                        allow_unknown
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
                        second_span,
                        current_scope_this_type,
                        allow_unknown
                    );
                    let error_type_result = self.unify_type_recursive(
                        &first_error_type,
                        first_span,
                        &second_error_type,
                        second_span,
                        current_scope_this_type,
                        allow_unknown
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
        second_span: &Range<usize>,
        current_scope_this_type: &ScopeThisType,
        allow_unknown: bool
    ) -> Result<(), Vec<(Spanned<Type>, Spanned<Type>)>> {

        let mut errors = Vec::new();

        for i in 0..first_generics.len() {
            let result = self.unify_type_recursive(
                &first_generics[i],
                first_span,
                &second_generics[i],
                second_span,
                current_scope_this_type,
                allow_unknown
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

    pub fn resolve_entity_type_with_id(&self, entity_id: EntityID) -> (EntityID, Spanned<Type>) {
        let mut current_id = entity_id;
        loop {
            let entity_id_or_type = self.entity_type_map.get(&current_id).unwrap();
            match entity_id_or_type {
                Either::Left(entity_id) => current_id = *entity_id,
                Either::Right(ty) => return (current_id, ty.clone())
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
            Type::UserType { user_type_info, generics, generics_span: _ } => {
                let mut name = user_type_info.name.value.clone();
                self.add_generics_info(&mut name, generics);
                name
            },
            Type::Function { function_info, generics } => {
                let mut name = "function".to_string();

                self.add_generics_info(&mut name, generics);

                name += "(";
                for i in 0..function_info.argument_types.len() {
                    let argument = &function_info.argument_types[i];
                    if i != 0 {
                        name += ", ";
                    }
                    name += self.get_type_display_string(argument).as_str();
                }
                name += ")";

                if function_info.return_type.value != Type::Unit {
                    name += " -> ";
                    name += self.get_type_display_string(&function_info.return_type.value).as_str();
                }

                name
            },
            Type::Generic(generic_type) => generic_type.name.deref().clone(),
            Type::LocalGeneric(generic_id) => {
                self.get_type_display_string(&self.resolve_generic_type(*generic_id).1.value)
            },
            Type::Option(value_type) => format!("{}?", self.get_type_display_string(value_type)),
            Type::Result { value, error } => {
                format!("{}!<{}>", self.get_type_display_string(value), self.get_type_display_string(error))
            },
            Type::This => "This".to_string(),
            Type::Unreachable => "$unreachable".to_string(),
            Type::Unknown => "unknown".to_string()
        }
    }

    pub fn resolve_type(&self, ty: &Type) -> Type {
        match ty {
            Type::UserType { user_type_info, generics, generics_span: _ } => {
                let mut new_generics = Vec::new();
                for generic in generics.iter() {
                    new_generics.push(self.resolve_type(generic));
                }
                Type::UserType { user_type_info: user_type_info.clone(), generics: Arc::new(new_generics), generics_span: None }
            },
            Type::Function { function_info, generics } => {
                let mut new_generics = Vec::new();
                for generic in generics.iter() {
                    new_generics.push(self.resolve_type(generic));
                }
                Type::Function { function_info: function_info.clone(), generics: Arc::new(new_generics) }
            },
            Type::LocalGeneric(generic_id) => self.resolve_generic_type(*generic_id).1.value,
            Type::Option(value) => Type::Option(Arc::new(self.resolve_type(&value))),
            Type::Result { value, error } => {
                Type::Result {
                    value: Arc::new(self.resolve_type(&value)),
                    error: Arc::new(self.resolve_type(&error))
                }
            },
            _ => ty.clone()
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

    pub(crate) fn collect_info(
        &mut self,
        implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvertKind>,
        global_implements_info_set: &ImplementsInfoSet,
        errors: &mut Vec<TranspileError>,
        warnings: &mut Vec<TranspileWarning>,
        context: &TranspileModuleContext
    ) {
        implicit_convert_map.extend(&self.implicit_convert_map);

        self.type_check_bounds(global_implements_info_set, errors);

        for report in self.lazy_type_reports.iter() {
            match report.build_report(self, context) {
                Either::Left(error) => errors.push(error),
                Either::Right(warning) => warnings.push(warning),
            }
        }

        self.check_impl_interface_generics(errors);
    }

    fn type_check_bounds(&mut self, global_implements_info_set: &ImplementsInfoSet, errors: &mut Vec<TranspileError>) {
        for bounds_check in self.generic_bounds_checks.clone().iter() {
            match bounds_check {
                GenericsBoundCheck::Generics {
                    type_span,
                    ty,
                    generic_define: generics_define,
                    scope_implements_info_set
                } => {
                    let type_resolved = if let Type::LocalGeneric(generic_id) = &ty.value {
                        self.resolve_generic_type(*generic_id).1
                    } else {
                        Spanned::new(self.resolve_type(&ty.value), ty.span.clone())
                    };

                    let result = global_implements_info_set.is_satisfied(
                        &type_resolved.value,
                        &generics_define.bounds.freeze_and_get(),
                        self,
                        scope_implements_info_set,
                        false
                    );
        
                    if let Err(bounds) = result {
                        let contains_unknown = bounds.iter().any(|bound| { bound.ty.contains_unknown() });
                        if type_resolved.value.contains_unknown() || contains_unknown {
                            continue;
                        }
        
                        let type_name = type_resolved.map(|ty| { self.get_type_display_string(&ty) });
                        let bounds_module_name = bounds.first().unwrap().module_name.clone();
                        let not_satisfied_bounds = bounds.into_iter().map(|bound| {
                            Spanned::new(self.get_type_display_string(&bound.ty), bound.span.clone())
                        }).collect();
        
                        let error = TranspileError::new(BoundsNotSatisfiedError {
                            span: type_span.clone(),
                            type_name,
                            bounds_module_name,
                            not_satisfied_bounds
                        });
                        errors.push(error);
                    }
                },
                GenericsBoundCheck::Where {
                    type_span,
                    target_type,
                    bounds,
                    scope_implements_info_set
                } => {
                    let result = global_implements_info_set.is_satisfied(
                        target_type,
                        bounds,
                        self,
                        scope_implements_info_set,
                        false
                    );

                    if let Err(bounds) = result {
                        let bounds_contain_unknown = bounds.iter()
                            .any(|bound| { self.resolve_type(&bound.ty).contains_unknown() });
                        if self.resolve_type(target_type).contains_unknown() || bounds_contain_unknown {
                            continue;
                        }
        
                        let type_name = self.get_type_display_string(target_type);
                        let bounds_module_name = bounds.first().unwrap().module_name.clone();
                        let not_satisfied_bounds = bounds.into_iter().map(|bound| {
                            Spanned::new(self.get_type_display_string(&bound.ty), bound.span.clone())
                        }).collect();
        
                        let error = TranspileError::new(WhereBoundsNotSatisfiedError {
                            type_name: Spanned::new(type_name, type_span.clone()),
                            bounds_module_name,
                            not_satisfied_bounds
                        });
                        errors.push(error);
                    }
                }
            }
        }
    }

    fn check_impl_interface_generics(&self, errors: &mut Vec<TranspileError>) {
        for (reference_span, interface) in self.impl_interface_generics_check.iter() {
            if let Type::UserType { user_type_info: _, generics, generics_span: _ } = self.resolve_type(&interface.value) {
                let mut has_unknown = false;
                for generic in generics.iter() {
                    if generic.contains_unknown() {
                        has_unknown = true;
                        break;
                    }
                }

                if has_unknown {
                    let original_name = self.get_type_display_string(&interface.value.as_original_type());
                    let resolved_name = self.get_type_display_string(&self.resolve_type(&interface.value));

                    let error = UnresolvedInterface {
                        original_name,
                        resolved_name,
                        reference_span: reference_span.clone(),
                        implementation: interface.clone().map(|_| { () })
                    };
                    errors.push(TranspileError::new(error));
                }
            }
        }
    }

    fn add_check_type_info_bounds(
        &mut self,
        ty: Spanned<Type>,
        this_type: &Type,
        check_this_type_bounds: bool,
        current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>
    ) {
        if let Type::UserType { user_type_info, generics, generics_span } = &ty.value {
            if user_type_info.generics_define.len() != generics.len() {
                return;
            }

            let generics_span = generics_span.as_ref().map(|spans| {
                if spans.len() == generics.len() {
                    Some(spans)
                } else {
                    None
                }
            }).flatten();
            
            let mut generics_define = ty.value.get_generics_define_with_replaced_generic().unwrap();
            if ty.value.is_interface() {
                generics_define = Type::replace_generics_define_this_type(&generics_define, this_type);
            }

            for i in 0..generics.len() {
                let generic_define = &generics_define[i];
                let generic = &generics[i];

                let generic_span = generics_span
                    .map(|spans| { spans[i].clone() })
                    .unwrap_or(ty.span.clone());

                self.generic_bounds_checks.push(GenericsBoundCheck::Generics {
                    type_span: ty.span.clone(),
                    ty: Spanned::new(generic.clone(), generic_span.clone()),
                    generic_define: generic_define.clone(),
                    scope_implements_info_set: current_scope_implements_info_set.clone()
                });

                self.add_check_type_info_bounds(
                    Spanned::new(generic.clone(), generic_span),
                    this_type,
                    check_this_type_bounds,
                    current_scope_implements_info_set
                );
            }
            
            let where_bounds = ty.value.get_where_bounds_with_replaced_generic().unwrap();
            let replaced_where_bounds = if ty.value.is_interface() {
                Some(Type::replace_where_bounds_this_type(&where_bounds, this_type))
            } else {
                None
            };

            for i in 0..where_bounds.len() {
                let where_bound = &where_bounds[i];
                let replaced_where_bound = if let Some(replaced_where_bounds) = &replaced_where_bounds {
                    &replaced_where_bounds[i]
                } else {
                    where_bound
                };
                
                if !check_this_type_bounds {
                    if &where_bound.target_type.value == &Type::This {
                        continue;
                    }
                }
                
                self.generic_bounds_checks.push(GenericsBoundCheck::Where {
                    type_span: ty.span.clone(),
                    target_type: replaced_where_bound.target_type.value.clone(),
                    bounds: replaced_where_bound.bounds.clone(),
                    scope_implements_info_set: current_scope_implements_info_set.clone()
                });
            }
        }
    }
    
    fn add_check_type_info_generics_define(
        &mut self,
        generics_define: &Vec<Arc<GenericType>>,
        current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>
    ) {
        for generic_define in generics_define.iter() {
            let target_type = Type::Generic(generic_define.clone());
            
            for bound in generic_define.bounds.freeze_and_get().iter() {
                let bound_type = Spanned::new(bound.ty.clone(), bound.span.clone());
                self.add_check_type_info_bounds(
                    bound_type,
                    &target_type,
                    false,
                    current_scope_implements_info_set
                );
            }
        }
    }

}


#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) enum ImplicitConvertKind {
    Some,
    Ok,
    Error
}

#[derive(Debug, Clone)]
pub(crate) enum GenericsBoundCheck {
    Generics {
        type_span: Range<usize>,
        ty: Spanned<Type>,
        generic_define: Arc<GenericType>,
        scope_implements_info_set: Option<Arc<ImplementsInfoSet>>
    },
    Where {
        type_span: Range<usize>,
        target_type: Type,
        bounds: Vec<Arc<Bound>>,
        scope_implements_info_set: Option<Arc<ImplementsInfoSet>>
    }
}



pub(crate) fn type_inference_program<'allocator, 'input>(
    ast: Program<'allocator, 'input>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    is_interface_scope: bool,
    is_closure_scope: bool,
    implements_interfaces: &Vec<Spanned<Type>>,
    force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'allocator, 'input>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvertKind>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    let mut has_type = false;
    let mut var_entity_id_and_spans = Vec::new_in(allocator);

    let mut override_elements_environment = OverrideElementsEnvironment::new(implements_interfaces);

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
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
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
                        global_implements_info_set,
                        current_scope_this_type,
                        current_scope_implements_info_set,
                        true,
                        type_environment,
                        implicit_convert_map,
                        allocator,
                        errors,
                        warnings,
                        context
                    );

                    let result = type_environment.unify_with_implicit_convert(
                        Spanned::new(EntityID::from(assignment.left_expr), assignment.left_expr.get_span()),
                        Spanned::new(EntityID::from(*right_expr), right_expr.get_span()),
                        current_scope_this_type,
                        false
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
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
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
                        global_implements_info_set,
                        current_scope_this_type,
                        current_scope_implements_info_set,
                        false,
                        type_environment,
                        implicit_convert_map,
                        allocator,
                        errors,
                        warnings,
                        context
                    );

                    let result = type_environment.unify(
                        Spanned::new(EntityID::from(exchange.left_expr), exchange.left_expr.get_span()),
                        Spanned::new(EntityID::from(*right_expr), right_expr.get_span()),
                        current_scope_this_type
                    );
                    add_error(result, type_environment);
                }
            },
            StatementAST::FunctionDefine(function_define) => {
                let function_type = module_entity_type_map.get(&EntityID::from(function_define)).unwrap();

                if let Ok(name) = &function_define.name {
                    if function_define.attributes.contains(StatementAttributeKind::Override) {
                        let override_keyword_span = function_define.attributes.get_span(StatementAttributeKind::Override).unwrap();

                        if implements_interfaces.is_empty() {
                            let mut error = TranspileError::new(SimpleError::new(
                                INVALID_OVERRIDE_ERROR,
                                override_keyword_span.clone(),
                                vec![],
                                vec![((context.module_name.clone(), override_keyword_span.clone()), Color::Red)]
                            ));
                            error.add_advice(
                                context.module_name.clone(),
                                Advice::Remove { span: override_keyword_span }
                            );
                            errors.push(error);
                        } else {
                            if !is_interface_scope {
                                let interface_span_start = implements_interfaces.first().unwrap().span.start;
                                let interface_span_end = implements_interfaces.last().unwrap().span.end;
                                let interface_span = interface_span_start..interface_span_end;
    
                                if let Err(error) = override_elements_environment.check(
                                    name.clone(),
                                    function_type,
                                    &current_scope_this_type.ty,
                                    global_implements_info_set,
                                    type_environment,
                                    context
                                ) {
                                    error.collect(override_keyword_span, interface_span, type_environment, errors, context);
                                }
                            }
                        }
                    }
                }

                if let Type::Function { function_info, generics: _ } = function_type {
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

                    let current_scope_implements_info_set = get_and_check_where_bounds_implements_info(
                        &function_info.where_bounds.freeze_and_get(),
                        current_scope_implements_info_set,
                        &mut type_environment,
                        context
                    );
                    
                    type_environment.add_check_type_info_generics_define(
                        &function_info.generics_define,
                        &current_scope_implements_info_set
                    );

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
                            global_implements_info_set,
                            &current_scope_this_type.nest(),
                            &current_scope_implements_info_set,
                            false,
                            false,
                            &Vec::new(),
                            false,
                            &mut type_environment,
                            implicit_convert_map,
                            allocator,
                            errors,
                            warnings,
                            context
                        );
                    }

                    type_environment.collect_info(implicit_convert_map, global_implements_info_set, errors, warnings, context);
                }
            },
            StatementAST::UserTypeDefine(user_type_define) => {
                let user_type = if let Ok(name) = &user_type_define.name {
                    user_type_map.get(name.value).unwrap().clone()
                } else {
                    Type::Unknown
                };

                let this_type = if user_type_define.kind.value == UserTypeKindEnum::Interface {
                    Type::This
                } else {
                    user_type.clone()
                };

                let mut current_scope_implements_info_set = match &user_type {
                    Type::UserType { user_type_info, generics: _, generics_span: _ } => {
                        get_and_check_where_bounds_implements_info(
                            &user_type_info.where_bounds.freeze_and_get(),
                            &None,
                            type_environment,
                            context
                        )
                    },
                    _ => None
                };
                
                if let Type::UserType { user_type_info, generics: _, generics_span: _ } = &user_type {
                    type_environment.add_check_type_info_generics_define(
                        &user_type_info.generics_define,
                        &current_scope_implements_info_set
                    );
                }

                if user_type_define.kind.value == UserTypeKindEnum::Interface {
                    if let Ok(name) = &user_type_define.name {
                        let mut implements_info_set = current_scope_implements_info_set.as_ref()
                            .map(|implements_info_set| { implements_info_set.deref().clone() })
                            .unwrap_or_default();
                        implements_info_set.insert(
                            EntityID::from(user_type_define),
                            ImplementsInfo {
                                generics: Arc::new(Vec::new()),
                                interface: Spanned::new(user_type.clone(), name.span.clone()),
                                concrete: Spanned::new(Type::This, name.span.clone()),
                                module_name: context.module_name.clone(),
                                where_bounds: Arc::new(Vec::new()),
                                element_types: Arc::new(FxHashMap::default()),
                                is_bounds_info: true
                            }
                        );
                        current_scope_implements_info_set = Some(Arc::new(implements_info_set));
                    }
                }

                let mut implements_interfaces = Vec::new();

                if let Some(super_type_info) = &user_type_define.super_type_info {
                    let mut implements_infos = current_scope_implements_info_set.as_ref()
                        .map(|implements_infos| { implements_infos.implements_infos.clone() })
                        .unwrap_or(IndexMap::new());

                    for type_info in super_type_info.type_infos.iter() {
                        let current_scope_implements_info_set = Some(Arc::new(ImplementsInfoSet {
                            implements_infos: implements_infos.clone(),
                        }));

                        let super_type = module_entity_type_map.get(&EntityID::from(type_info)).unwrap();

                        implements_interfaces.push(Spanned::new(super_type.clone(), type_info.span.clone()));

                        type_environment.add_check_type_info_bounds(
                            Spanned::new(super_type.clone(), type_info.span.clone()),
                            &user_type,
                            true,
                            &current_scope_implements_info_set
                        );

                        if let Ok(name) = &user_type_define.name {
                            let concrete = if user_type_define.kind.value == UserTypeKindEnum::Interface {
                                Type::This
                            } else {
                                user_type_map.get(name.value).unwrap().clone()
                            };
                            implements_infos.insert(EntityID::from(type_info), ImplementsInfo {
                                generics: Arc::new(Vec::new()),
                                interface: Spanned::new(super_type.clone(), type_info.span.clone()),
                                concrete: Spanned::new(concrete, name.span.clone()),
                                module_name: context.module_name.clone(),
                                where_bounds: Arc::new(Vec::new()),
                                element_types: Arc::new(FxHashMap::default()),
                                is_bounds_info: true
                            });
                        }
                    }
                    
                    if user_type_define.kind.value == UserTypeKindEnum::Interface {
                        current_scope_implements_info_set = Some(Arc::new(ImplementsInfoSet { implements_infos }));
                    }
                }

                if let Some(block) = &user_type_define.block.value {
                    let mut type_environment = TypeEnvironment::new(allocator);

                    type_inference_program(
                        block.program,
                        user_type_map,
                        import_element_map,
                        name_resolved_map, module_user_type_map,
                        module_element_type_map,
                        module_element_type_maps,
                        generics_map,
                        module_entity_type_map,
                        global_implements_info_set,
                        &ScopeThisType::new(this_type),
                        &current_scope_implements_info_set,
                        user_type_define.kind.value == UserTypeKindEnum::Interface,
                        false,
                        &implements_interfaces,
                        false,
                        &mut type_environment,
                        implicit_convert_map,
                        allocator,
                        errors,
                        warnings,
                        context
                    );

                    type_environment.collect_info(implicit_convert_map, global_implements_info_set, errors, warnings, context);
                }
            },
            StatementAST::Implements(implements) => {
                let concrete = if let Ok(target_type) = &implements.target_user_type {
                    module_entity_type_map.get(&EntityID::from(target_type)).unwrap().clone()
                } else {
                    Type::Unknown
                };

                let implements_info  = global_implements_info_set.get(EntityID::from(implements));
                let current_scope_implements_info_set = if let Some(implements_info) = implements_info {
                    get_and_check_where_bounds_implements_info(
                        &implements_info.where_bounds,
                        &None,
                        type_environment,
                        context
                    )
                } else {
                    None
                };

                if let Some(implements_info) = implements_info {
                    let mut has_same_module_type = false;
                    if let Type::UserType { user_type_info, generics: _, generics_span: _ } = &implements_info.interface.value {
                        if &context.module_name == &user_type_info.module_name {
                            has_same_module_type = true;
                        }
                    }
                    if let Type::UserType { user_type_info, generics: _, generics_span: _ } = &implements_info.concrete.value {
                        if &context.module_name == &user_type_info.module_name {
                            has_same_module_type = true;
                        }
                    }

                    if !has_same_module_type {
                        let error = SimpleError::new(
                            0062,
                            implements_info.interface.span.clone(),
                            vec![],
                            vec![
                                ((context.module_name.clone(), implements_info.interface.span.clone()), Color::Red),
                                ((context.module_name.clone(), implements_info.concrete.span.clone()), Color::Red)
                            ]
                        );
                        errors.push(TranspileError::new(error));
                    }
                }
                
                if let Type::UserType { user_type_info, generics: _, generics_span: _ } = &concrete {
                    type_environment.add_check_type_info_generics_define(
                        &user_type_info.generics_define,
                        &current_scope_implements_info_set
                    );
                }
                
                if let Ok(target_type) = &implements.target_user_type {
                    type_environment.add_check_type_info_bounds(
                        Spanned::new(concrete.clone(), target_type.span.clone()),
                        &concrete,
                        true,
                        &current_scope_implements_info_set
                    );
                }

                let mut implements_interfaces = Vec::new();
                
                if let Ok(interface_info) = &implements.interface {
                    let interface = module_entity_type_map.get(&EntityID::from(interface_info)).unwrap();

                    implements_interfaces.push(Spanned::new(interface.clone(), interface_info.span.clone()));

                    type_environment.add_check_type_info_bounds(
                        Spanned::new(interface.clone(), interface_info.span.clone()),
                        &concrete,
                        true,
                        &current_scope_implements_info_set
                    );
                }

                if let Some(block) = &implements.block.value {
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
                        global_implements_info_set,
                        &ScopeThisType::new(concrete),
                        &current_scope_implements_info_set,
                        false,
                        false,
                        &implements_interfaces,
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
                        global_implements_info_set,
                        current_scope_this_type,
                        current_scope_implements_info_set,
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
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
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
                                            current_scope_this_type,
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

                if let Some(type_tag) = &variable_define.type_tag {
                    type_environment.add_check_type_info_bounds(
                        Spanned::new(tag_type.clone(), type_tag.span.clone()),
                        &Type::This,
                        false,
                        current_scope_implements_info_set
                    );
                }

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
                            global_implements_info_set,
                            current_scope_this_type,
                            current_scope_implements_info_set,
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

                            let result = type_environment.unify_with_implicit_convert(
                                Spanned::new(EntityID::from(*expression), expression.get_span()),
                                Spanned::new(EntityID::from(variable_define), tag_type_span),
                                current_scope_this_type,
                                true
                            );
                            add_error(result, type_environment);
                        }

                        if let Ok(name) = &variable_define.name {
                            var_entity_id_and_spans.push((name.span.clone(), EntityID::from(variable_define)));
                        }
                    }
                }
            }
            _ => {}
        }
    }
    
    if !is_closure_scope {
        for i in 0..type_environment.closures.len() {
            let closure = type_environment.closures[i];
            
            let closure_type = type_environment.resolve_entity_type(EntityID::from(closure));
            
            if let Type::Function { function_info, generics: _ } = closure_type.value {
                let return_type_temp = type_environment.return_type.clone();
                type_environment.return_type = Either::Right(function_info.return_type.clone());
                
                if let Some(block_or_expr) = &closure.expression_or_block.value {
                    match block_or_expr {
                        Either::Left(expression) => {
                            type_inference_expression(
                                *expression,
                                user_type_map,
                                import_element_map,
                                name_resolved_map,
                                module_user_type_map,
                                module_element_type_map,
                                module_element_type_maps,
                                generics_map,
                                module_entity_type_map,
                                global_implements_info_set,
                                current_scope_this_type,
                                current_scope_implements_info_set,
                                true,
                                type_environment,
                                implicit_convert_map,
                                allocator,
                                errors,
                                warnings,
                                context
                            );
                            
                            let expression_type = type_environment.resolve_entity_type(EntityID::from(*expression));
                            
                            if &expression_type.value != &Type::Unreachable {
                                let result = type_environment.unify_with_return_type(
                                    Spanned::new(EntityID::from(*expression), expression.get_span()),
                                    current_scope_this_type
                                );
                                add_error(result, type_environment);
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
                                global_implements_info_set,
                                current_scope_this_type,
                                current_scope_implements_info_set,
                                false,
                                true,
                                implements_interfaces,
                                true,
                                type_environment,
                                implicit_convert_map,
                                allocator,
                                errors,
                                warnings,
                                context
                            );
                            
                            let program_type = type_environment.resolve_entity_type(EntityID::from(block.program));
                            
                            if &program_type.value != &Type::Unreachable {
                                let result = type_environment.unify_with_return_type(
                                    Spanned::new(EntityID::from(block.program), block.span.clone()),
                                    current_scope_this_type
                                );
                                add_error(result, type_environment);
                            }
                        }
                    }
                }
                
                type_environment.return_type = return_type_temp;
            } else {
                unreachable!()
            }
        }
    }

    if !has_type {
        type_environment.set_entity_type(
            EntityID::from(ast),
            Spanned::new(Type::Unit, ast.span.clone())
        );
    }
    
    if !is_interface_scope {
        override_elements_environment.collect_errors(errors, context);
    }

    let mut builder = Report::build(ReportKind::Custom("Debug", Color::Cyan), &context.module_name, 0);

    for var_type_and_span in var_entity_id_and_spans {
        let ty = type_environment.resolve_entity_type(var_type_and_span.1).value;
        
        builder.add_label(
            Label::new((&context.module_name, var_type_and_span.0))
                .with_color(Color::Green)
                .with_message(type_environment.get_type_display_string(&ty))
        );
    }
    
    builder.finish().print((&context.module_name, Source::from(context.source_code.code.as_str()))).unwrap();
}

fn get_and_check_where_bounds_implements_info(
    where_bounds: &Vec<WhereBound>,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    type_environment: &mut TypeEnvironment,
    context: &TranspileModuleContext
) -> Option<Arc<ImplementsInfoSet>> {
    if where_bounds.is_empty() {
        return current_scope_implements_info_set.clone();
    }

    let mut implements_infos = current_scope_implements_info_set.as_ref()
        .map(|implements_info_set| { implements_info_set.implements_infos.clone() })
        .unwrap_or(IndexMap::new());

    for where_bound in where_bounds.iter() {
        let current_implements_info_set = Some(Arc::new(ImplementsInfoSet {
            implements_infos: implements_infos.clone()
        }));

        type_environment.add_check_type_info_bounds(
            where_bound.target_type.clone(),
            &where_bound.target_type.value,
            true,
            &current_implements_info_set
        );

        for bound in where_bound.bounds.iter() {
            type_environment.add_check_type_info_bounds(
                Spanned::new(bound.ty.clone(), bound.span.clone()),
                &where_bound.target_type.value,
                false,
                &current_implements_info_set
            );

            implements_infos.insert(bound.entity_id, ImplementsInfo {
                generics: Arc::new(Vec::new()),
                interface: Spanned::new(bound.ty.clone(), bound.span.clone()),
                concrete: where_bound.target_type.clone(),
                module_name: context.module_name.clone(),
                where_bounds: Arc::new(Vec::new()),
                element_types: Arc::new(FxHashMap::default()),
                is_bounds_info: true
            });
        }
    }

    Some(Arc::new(ImplementsInfoSet { implements_infos }))
}

fn type_inference_expression<'allocator, 'input>(
    ast: Expression<'allocator, 'input>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    mut force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'allocator, 'input>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvertKind>,
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
                global_implements_info_set,
                current_scope_this_type,
                current_scope_implements_info_set,
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
                        global_implements_info_set,
                        current_scope_this_type,
                        current_scope_implements_info_set,
                        force_be_expression,
                        type_environment,
                        implicit_convert_map,
                        allocator,
                        errors,
                        warnings,
                        context
                    );

                    let current = Spanned::new(EntityID::from(right_expr), right_expr.span.clone());

                    let result = type_environment.unify(
                        previous.clone(),
                        current.clone(),
                        current_scope_this_type
                    );
                    results.push(result);

                    previous = current;
                }
            }

            add_errors(results, type_environment);

            type_environment.set_entity_id_equals(
                previous.value,
                EntityID::from(ast)
            );
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
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    true,
                    type_environment,
                    implicit_convert_map,
                    allocator,
                    errors,
                    warnings,
                    context
                );

                let result = type_environment.unify_with_return_type(
                    Spanned::new(EntityID::from(expression), expression.get_span()),
                    current_scope_this_type
                );
                add_error(result, type_environment);
            }
            
            type_environment.set_entity_type(
                EntityID::from(ast),
                Spanned::new(Type::Unreachable, return_expression.span.clone())
            );
        },
        ExpressionEnum::Closure(closure) => {
            let mut argument_types = Vec::new();
            
            match &closure.arguments.arguments {
                Either::Left(literal) => {
                    let ty = Type::LocalGeneric(type_environment.new_local_generic_id(literal.span.clone()));
                    
                    type_environment.set_entity_type(
                        EntityID::from(literal),
                        Spanned::new(ty.clone(), literal.span.clone())
                    );
                    
                    argument_types.push(ty);
                },
                Either::Right(arguments) => {
                    for argument in arguments.iter() {
                        match argument {
                            Either::Left(argument) => {
                                let ty = if let Ok(type_info) = &argument.type_tag.type_info {
                                    get_type(
                                        type_info,
                                        user_type_map,
                                        import_element_map,
                                        name_resolved_map,
                                        module_user_type_map,
                                        module_element_type_map,
                                        generics_map,
                                        current_scope_this_type,
                                        errors,
                                        warnings,
                                        context
                                    )
                                } else {
                                    Type::LocalGeneric(type_environment.new_local_generic_id(argument.span.clone()))
                                };
                                
                                type_environment.set_entity_type(
                                    EntityID::from(argument),
                                    Spanned::new(ty.clone(), argument.span.clone())
                                );
                                
                                argument_types.push(ty);
                            },
                            Either::Right(literal) => {
                                let ty = Type::LocalGeneric(type_environment.new_local_generic_id(literal.span.clone()));
                                
                                type_environment.set_entity_type(
                                    EntityID::from(literal),
                                    Spanned::new(ty.clone(), literal.span.clone())
                                );
                                
                                argument_types.push(ty);
                            }
                        }
                    }
                }
            }
            
            let return_type = Spanned::new(
                Type::LocalGeneric(type_environment.new_local_generic_id(closure.span.clone())),
                closure.span.clone()
            );
            
            let define_info = FunctionDefineInfo {
                module_name: context.module_name.clone(),
                generics_define_span: None,
                arguments_span: closure.arguments.span.clone(),
                is_closure: true,
                span: closure.span.clone()
            };
            
            let function_info = FunctionType {
                is_extension: false,
                generics_define: Vec::new(),
                argument_types,
                return_type,
                define_info,
                where_bounds: FreezableMutex::new(Vec::new())
            };
            
            let closure_type = Spanned::new(
                Type::Function { function_info: Arc::new(function_info), generics: Arc::new(Vec::new()) },
                closure.span.clone()
            );
            
            type_environment.set_entity_type(EntityID::from(closure), closure_type);
            
            type_environment.set_entity_id_equals(
                EntityID::from(closure),
                EntityID::from(ast)
            );
            
            type_environment.closures.push(closure);
        }
    }
}

fn type_inference_and_expression<'allocator, 'input>(
    ast: &'allocator AndExpression<'allocator, 'input>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    mut force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'allocator, 'input>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvertKind>,
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
        global_implements_info_set,
        current_scope_this_type,
        current_scope_implements_info_set,
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
                global_implements_info_set,
                current_scope_this_type,
                current_scope_implements_info_set,
                force_be_expression,
                type_environment,
                implicit_convert_map,
                allocator,
                errors,
                warnings,
                context
            );
            
            let current = Spanned::new(EntityID::from(right_expr), right_expr.span.clone());

            let result = type_environment.unify(
                previous.clone(),
                current.clone(),
                current_scope_this_type
            );
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

fn type_inference_eqne_expression<'allocator, 'input>(
    ast: &'allocator EQNEExpression<'allocator, 'input>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    mut force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'allocator, 'input>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvertKind>,
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
        global_implements_info_set,
        current_scope_this_type,
        current_scope_implements_info_set,
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
                global_implements_info_set,
                current_scope_this_type,
                current_scope_implements_info_set,
                force_be_expression,
                type_environment,
                implicit_convert_map,
                allocator,
                errors,
                warnings,
                context
            );

            let current = Spanned::new(EntityID::from(right_expr), right_expr.span.clone());

            let result = type_environment.unify(
                previous.clone(),
                current.clone(),
                current_scope_this_type
            );
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

fn type_inference_compare_expression<'allocator, 'input>(
    ast: &'allocator CompareExpression<'allocator, 'input>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    mut force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'allocator, 'input>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvertKind>,
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
        global_implements_info_set,
        current_scope_this_type,
        current_scope_implements_info_set,
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
                global_implements_info_set,
                current_scope_this_type,
                current_scope_implements_info_set,
                force_be_expression,
                type_environment,
                implicit_convert_map,
                allocator,
                errors,
                warnings,
                context
            );

            let current = Spanned::new(EntityID::from(right_expr), right_expr.span.clone());

            let result = type_environment.unify(
                previous.clone(),
                current.clone(),
                current_scope_this_type
            );
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

fn type_inference_add_or_sub_expression<'allocator, 'input>(
    ast: &'allocator AddOrSubExpression<'allocator, 'input>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    mut force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'allocator, 'input>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvertKind>,
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
        global_implements_info_set,
        current_scope_this_type,
        current_scope_implements_info_set,
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
                global_implements_info_set,
                current_scope_this_type,
                current_scope_implements_info_set,
                force_be_expression,
                type_environment,
                implicit_convert_map,
                allocator,
                errors,
                warnings,
                context
            );

            let current = Spanned::new(EntityID::from(right_expr), right_expr.span.clone());

            let result = type_environment.unify(
                previous.clone(),
                current.clone(),
                current_scope_this_type
            );
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

fn type_inference_mul_or_div_expression<'allocator, 'input>(
    ast: &'allocator MulOrDivExpression<'allocator, 'input>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    mut force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'allocator, 'input>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvertKind>,
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
        global_implements_info_set,
        current_scope_this_type,
        current_scope_implements_info_set,
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
                global_implements_info_set,
                current_scope_this_type,
                current_scope_implements_info_set,
                force_be_expression,
                type_environment,
                implicit_convert_map,
                allocator,
                errors,
                warnings,
                context
            );

            let current = Spanned::new(EntityID::from(right_expr), right_expr.span.clone());

            let result = type_environment.unify(
                previous.clone(),
                current.clone(),
                current_scope_this_type
            );
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

fn type_inference_factor<'allocator, 'input>(
    ast: &'allocator Factor<'allocator, 'input>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'allocator, 'input>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvertKind>,
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
            global_implements_info_set,
            current_scope_this_type,
            current_scope_implements_info_set,
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

fn type_inference_primary<'allocator, 'input>(
    ast: &'allocator Primary<'allocator, 'input>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'allocator, 'input>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvertKind>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    let module_name_result = get_module_name_from_primary(
        ast,
        name_resolved_map,
        import_element_map,
        context
    );

    if let Some((module_name, count)) = module_name_result {
        if ast.chain.is_empty() {
            let span = ast.left.span.clone();
            let next_span = span.end..span.end;

            let error = SimpleError::new(
                0043,
                next_span.clone(),
                vec![],
                vec![
                    ((context.module_name.clone(), span), Color::Yellow),
                    ((context.module_name.clone(), next_span.clone()), Color::Red)
                ]
            );
            let mut error = TranspileError::new(error);

            let advice = Advice::Add {
                add: "::element_name_here".to_string(),
                position: next_span.end,
                message_override: Some("error.0043.advice")
            };
            error.add_advice(context.module_name.clone(), advice);

            errors.push(error);

            type_environment.set_entity_type(
                EntityID::from(ast),
                Spanned::new(Type::Unknown, ast.span.clone())
            );

            return;
        }

        let next_primary = &ast.chain[count];

        if next_primary.separator.value != PrimarySeparatorKind::DoubleColon {
            let span = next_primary.separator.span.clone();

            let error = SimpleError::new(
                0042,
                span.clone(),
                vec![],
                vec![
                    ((context.module_name.clone(), next_primary.span.clone()), Color::Yellow),
                    ((context.module_name.clone(), span), Color::Red)
                ]
            );
            errors.push(TranspileError::new(error));

            type_environment.set_entity_type(
                EntityID::from(ast),
                Spanned::new(Type::Unknown, ast.span.clone())
            );

            return;
        }

        if let Some(second_expr) = &next_primary.second_expr {
            let import_user_type_map = module_user_type_map.get(&module_name).unwrap();
            let element_type_map = module_element_type_maps.get(&module_name).unwrap();

            let ty = if let Some(user_type) = import_user_type_map.get(second_expr.0.value) {
                user_type.clone()
            } else if let Some(element_type) = element_type_map.get(second_expr.0.value) {
                element_type.clone()
            } else {
                let span = second_expr.0.span.clone();
                let first_span_start = ast.left.span.start;
                let first_span_end = if count == 0 {
                    ast.left.span.end
                } else {
                    ast.chain[count - 1].span.end
                };

                let error = SimpleError::new(
                    0044,
                    span.clone(),
                    vec![],
                    vec![
                        ((context.module_name.clone(), first_span_start..first_span_end), Color::Yellow),
                        ((context.module_name.clone(), span), Color::Red)
                    ]
                );
                errors.push(error);

                Type::Unknown
            };
            
            let ty = type_environment.get_user_or_function_type_with_local_generic_id(
                Spanned::new(ty, second_expr.0.span.clone()),
                current_scope_implements_info_set,
                true
            );

            type_environment.set_entity_type(
                EntityID::from(&second_expr.0),
                ty
            );

            if let Some(generics) = &second_expr.1 {
                if let Ok(generics) = generics {
                    type_inference_generics(
                        generics,
                        EntityID::from(&second_expr.0),
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        generics_map,
                        current_scope_this_type,
                        current_scope_implements_info_set,
                        type_environment,
                        errors,
                        warnings,
                        context
                    );
                }
            }

            let mut mappings = Vec::new_in(allocator);

            let entity_id = if let Some(function_call) = &second_expr.2 {
                type_inference_function_call(
                    function_call,
                    EntityID::from(&second_expr.0),
                    false,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    type_environment,
                    implicit_convert_map,
                    allocator,
                    errors,
                    warnings,
                    context
                );

                Spanned::new(
                    EntityID::from(function_call),
                    second_expr.0.span.start..function_call.span.end
                )
            } else {
                Spanned::new(
                    EntityID::from(&second_expr.0),
                    second_expr.0.span.clone()
                )
            };

            let mut previous = if let Some(mapping_operator) = &next_primary.mapping_operator {
                type_inference_mapping_operator(
                    mapping_operator,
                    entity_id,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    type_environment,
                    implicit_convert_map,
                    &mut mappings,
                    allocator,
                    errors,
                    warnings,
                    context
                );

                Spanned::new(
                    EntityID::from(&mapping_operator.value),
                    second_expr.0.span.start..mapping_operator.span.end
                )
            } else {
                entity_id
            };

            for i in (count + 1)..ast.chain.len() {
                let primary_right = &ast.chain[i];

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
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    type_environment,
                    implicit_convert_map,
                    &mut mappings,
                    allocator,
                    errors,
                    warnings,
                    context
                );

                previous = Spanned::new(
                    EntityID::from(primary_right),
                    primary_right.span.clone()
                );
            }

            type_environment.set_entity_id_equals(
                previous.value,
                EntityID::from(ast)
            );
        } else {
            let span = next_primary.span.clone();
            let first_span_start = ast.left.span.start;
            let first_span_end = if count == 0 {
                ast.left.span.end
            } else {
                ast.chain[count - 1].span.end
            };

            let error = SimpleError::new(
                0043,
                span.clone(),
                vec![],
                vec![
                    ((context.module_name.clone(), first_span_start..first_span_end), Color::Yellow),
                    ((context.module_name.clone(), span), Color::Red)
                ]
            );
            let mut error = TranspileError::new(error);

            let advice = Advice::Add {
                add: "element_name_here".to_string(),
                position: next_primary.separator.span.end,
                message_override: Some("error.0043.advice")
            };
            error.add_advice(context.module_name.clone(), advice);

            errors.push(error);

            type_environment.set_entity_type(
                EntityID::from(ast),
                Spanned::new(Type::Unknown, ast.span.clone())
            );
        }
    } else {
        let mut mappings = Vec::new_in(allocator);

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
            global_implements_info_set,
            current_scope_this_type,
            current_scope_implements_info_set,
            force_be_expression,
            type_environment,
            implicit_convert_map,
            &mut mappings,
            allocator,
            errors,
            warnings,
            context
        );
    
        let mut previous = Spanned::new(EntityID::from(&ast.left), ast.left.span.clone());
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
                global_implements_info_set,
                current_scope_this_type,
                current_scope_implements_info_set,
                type_environment,
                implicit_convert_map,
                &mut mappings,
                allocator,
                errors,
                warnings,
                context
            );
            previous = Spanned::new(
                EntityID::from(primary_right),
                primary_right.span.clone()
            );
        }

        type_environment.set_entity_id_equals(
            previous.value,
            EntityID::from(ast)
        );
    }
}

enum MappingTypeKind {
    Option,
    Result { error_type: Type }
}

fn type_inference_primary_left<'allocator, 'input>(
    ast: &'allocator PrimaryLeft<'allocator, 'input>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'allocator, 'input>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvertKind>,
    mappings: &mut Vec<MappingTypeKind, &'allocator Bump>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    match &ast.first_expr {
        PrimaryLeftExpr::Simple(simple) => {
            match &simple.0 {
                SimplePrimary::Expression { expression, error_tokens: _, span: _ } => {
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
                            global_implements_info_set,
                            current_scope_this_type,
                            current_scope_implements_info_set,
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
                                if resolved.has_separator(&[EnvironmentSeparatorKind::UserTypeDefine, EnvironmentSeparatorKind::Function]) {
                                    errors.push(OutOfEnvironmentVariable::new(identifier.span.clone(), resolved));
                                    false
                                } else {
                                    true
                                }
                            },
                            DefineKind::UserType => false,
                            DefineKind::Generics => false
                        };

                        match resolved.define_info.define_kind {
                            DefineKind::Variable | DefineKind::FunctionArgument | DefineKind::ClosureArgument => {
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
                            },
                            DefineKind::Generics => {
                                let generic_type = generics_map.get(&resolved.define_info.entity_id).unwrap();
                                type_environment.set_entity_type(
                                    EntityID::from(&simple.0),
                                    Spanned::new(Type::Generic(generic_type.clone()), identifier.span.clone())
                                );
                            },
                            _ => {
                                let ty = if let Some(user_type) = user_type_map.get(identifier.value) {
                                    user_type.clone()
                                } else if let Some(element_type) = module_element_type_map.get(identifier.value) {
                                    element_type.clone()
                                } else {
                                    Type::Unknown
                                };

                                let ty = type_environment.get_user_or_function_type_with_local_generic_id(
                                    Spanned::new(ty, identifier.span.clone()),
                                    current_scope_implements_info_set,
                                    true
                                );

                                type_environment.set_entity_type(
                                    resolved.define_info.entity_id,
                                    ty
                                );

                                type_environment.set_entity_id_equals(
                                    resolved.define_info.entity_id,
                                    EntityID::from(&simple.0)
                                );
                            }
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
                            parse_primitive_type(text, current_scope_this_type)
                        };

                        type_environment.set_entity_type(
                            EntityID::from(&simple.0),
                            Spanned::new(ty, identifier.span.clone())
                        );
                    }
                },
                SimplePrimary::NullKeyword(null_keyword_span) => {
                    let generic_id = type_environment.new_local_generic_id(
                        null_keyword_span.clone()
                    );
                    type_environment.set_entity_type(
                        EntityID::from(&simple.0),
                        Spanned::new(Type::Option(Arc::new(Type::LocalGeneric(generic_id))), null_keyword_span.clone())
                    );
                },
                SimplePrimary::TrueKeyword(keyword_span) => {
                    type_environment.set_entity_type(
                        EntityID::from(&simple.0),
                        Spanned::new(Type::Bool, keyword_span.clone())
                    );
                },
                SimplePrimary::FalseKeyword(keyword_span) => {
                    type_environment.set_entity_type(
                        EntityID::from(&simple.0),
                        Spanned::new(Type::Bool, keyword_span.clone())
                    );
                },
                SimplePrimary::ThisKeyword(literal) => {
                    type_environment.set_entity_type(
                        EntityID::from(&simple.0),
                        Spanned::new(Type::This, literal.span.clone())
                    );
                },
                SimplePrimary::LargeThisKeyword(literal) => {
                    type_environment.set_entity_type(
                        EntityID::from(&simple.0),
                        Spanned::new(Type::This, literal.span.clone())
                    );
                }
            }

            if let Some(generics) = &simple.1 {
                if let Ok(generics) = generics {
                    type_inference_generics(
                        generics,
                        EntityID::from(&simple.0),
                        user_type_map,
                        import_element_map,
                        name_resolved_map,
                        module_user_type_map,
                        module_element_type_map,
                        generics_map,
                        current_scope_this_type,
                        current_scope_implements_info_set,
                        type_environment,
                        errors,
                        warnings,
                        context
                    );
                }
            }

            if let Some(function_call) = &simple.2 {
                type_inference_function_call(
                    function_call,
                    EntityID::from(&simple.0),
                    false,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    module_element_type_maps,
                    generics_map,
                    module_entity_type_map,
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
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
            
            let user_type_span = if new_expression.path.is_empty() {
                new_expression.span.clone()
            } else {
                let span_start = new_expression.path.first().unwrap().span.start;
                let span_end = new_expression.path.last().unwrap().span.end;
                span_start..span_end
            };

            let mut user_type = type_environment.get_user_or_function_type_with_local_generic_id(
                Spanned::new(user_type, user_type_span),
                current_scope_implements_info_set,
                true
            );

            if user_type.value.is_renamed_type() {
                user_type.value = user_type.value.get_element_type_with_replaced_generic("type").unwrap().value;
            }

            if let Type::UserType { user_type_info: _, generics: _, generics_span: _ } = &user_type.value {
                if let Ok(field_assigns) = &new_expression.field_assigns {
                    for field_assign in field_assigns.iter() {
                        if let Some(element_type) = user_type.value.get_element_type_with_replaced_generic(field_assign.name.value) {
                            type_environment.set_entity_type(
                                EntityID::from(field_assign),
                                Spanned::new(element_type.value, field_assign.name.span.clone())
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
                                    global_implements_info_set,
                                    current_scope_this_type,
                                    current_scope_implements_info_set,
                                    true,
                                    type_environment,
                                    implicit_convert_map,
                                    allocator,
                                    errors,
                                    warnings,
                                    context
                                );

                                let result = type_environment.unify_with_implicit_convert(
                                    Spanned::new(EntityID::from(field_assign), field_assign.name.span.clone()),
                                    Spanned::new(EntityID::from(*expression), expression.get_span()),
                                    current_scope_this_type,
                                    false
                                );
                                add_error(result, type_environment);
                            }
                        } else {
                            let error = NotFoundTypeElementError {
                                user_type: user_type.value.clone(),
                                name: field_assign.name.clone().map(|name| { name.to_string() }),
                            };
                            type_environment.add_lazy_type_error_report(error);
                        }
                    }
                }

                type_environment.set_entity_type(
                    EntityID::from(&ast.first_expr),
                    Spanned::new(user_type.value, new_expression.span.clone())
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
                        vec![((context.module_name.clone(), span), Color::Red)]
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
            let mut blocks = Vec::new_in(allocator);
            let mut has_else = false;

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
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    force_be_expression,
                    type_environment,
                    implicit_convert_map,
                    allocator,
                    errors,
                    warnings,
                    context
                );

                let condition_type = type_environment.resolve_entity_type(EntityID::from(*condition));
                
                let result = type_environment.unify_type(
                    &Type::Bool,
                    &condition.get_span(),
                    &condition_type.value,
                    &condition_type.span,
                    current_scope_this_type,
                    false
                );
                if result.is_err() {
                    type_environment.add_lazy_type_error_report(
                        SimpleTypeError { error_code: IF_CONDITION_TYPE_ERROR, ty: condition_type }
                    );
                }
            }
            if let Some(block) = &first_statement.block.value {
                blocks.push(block);
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
                                    global_implements_info_set,
                                    current_scope_this_type,
                                    current_scope_implements_info_set,
                                    force_be_expression,
                                    type_environment,
                                    implicit_convert_map,
                                    allocator,
                                    errors,
                                    warnings,
                                    context
                                );

                                let condition_type = type_environment.resolve_entity_type(EntityID::from(*condition));
                
                                let result = type_environment.unify_type(
                                    &Type::Bool,
                                    &condition.get_span(),
                                    &condition_type.value,
                                    &condition_type.span,
                                    current_scope_this_type,
                                    false
                                );
                                if result.is_err() {
                                    type_environment.add_lazy_type_error_report(
                                        SimpleTypeError { error_code: IF_CONDITION_TYPE_ERROR, ty: condition_type }
                                    );
                                }
                            }
                            if let Some(block) = &if_statement.block.value {
                                blocks.push(block);
                            }
                        },
                        Either::Right(block) => {
                            has_else = true;
                            blocks.push(block);
                        }
                    }
                }
            }

            if force_be_expression && !has_else {
                let error = SimpleError::new(
                    0063,
                    if_expression.span.clone(),
                    vec![],
                    vec![((context.module_name.clone(), if_expression.span.clone()), Color::Red)]
                );
                errors.push(TranspileError::new(error));
            }

            type_inference_blocks(
                blocks,
                Spanned::new(EntityID::from(&ast.first_expr), if_expression.span.clone()),
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                module_entity_type_map,
                global_implements_info_set,
                current_scope_this_type,
                current_scope_implements_info_set,
                force_be_expression,
                type_environment,
                implicit_convert_map,
                allocator,
                errors,
                warnings,
                context
            );
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
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    false,
                    false,
                    &Vec::new(),
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
            Spanned::new(EntityID::from(&ast.first_expr), ast.first_expr.get_span()),
            user_type_map,
            import_element_map,
            name_resolved_map,
            module_user_type_map,
            module_element_type_map,
            module_element_type_maps,
            generics_map,
            module_entity_type_map,
            global_implements_info_set,
            current_scope_this_type,
            current_scope_implements_info_set,
            type_environment,
            implicit_convert_map,
            mappings,
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

fn type_inference_blocks<'allocator, 'input>(
    blocks: Vec<&'allocator Block<'allocator, 'input>, &'allocator Bump>,
    parent_ast_entity_id: Spanned<EntityID>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    force_be_expression: bool,
    type_environment: &mut TypeEnvironment<'allocator, 'input>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvertKind>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    let mut first_type = match blocks.first() {
        Some(block) => {
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
                global_implements_info_set,
                current_scope_this_type,
                current_scope_implements_info_set,
                false,
                false,
                &Vec::new(),
                force_be_expression,
                type_environment,
                implicit_convert_map,
                allocator,
                errors,
                warnings,
                context
            );

            type_environment.resolve_entity_type(EntityID::from(block.program))
        },
        _ => {
            type_environment.set_entity_type(
                parent_ast_entity_id.value,
                Spanned::new(Type::Unknown, parent_ast_entity_id.span)
            );
            return;
        }
    };

    type_environment.set_entity_type(parent_ast_entity_id.value, first_type.clone());

    let mut has_implicit_convert = false;

    for i in 1..blocks.len() {
        let block = blocks[i];
        
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
            global_implements_info_set,
            current_scope_this_type,
            current_scope_implements_info_set,
            false,
            false,
            &Vec::new(),
            force_be_expression,
            type_environment,
            implicit_convert_map,
            allocator,
            errors,
            warnings,
            context
        );

        if !force_be_expression {
            continue;
        }

        let result = type_environment.unify_with_implicit_convert(
            Spanned::new(parent_ast_entity_id.value, first_type.span.clone()),
            Spanned::new(EntityID::from(block.program), block.program.span.clone()),
            current_scope_this_type,
            false
        );

        let second_type = type_environment.resolve_entity_type(EntityID::from(block.program));

        if let Err(error) = result {
            if error.generics.is_empty() && !has_implicit_convert {
                if let Type::Unit = &second_type.value {
                    // TODO - replace with std error type
                    first_type.value = Type::Result {
                        value: Arc::new(first_type.value.clone()),
                        error: Arc::new(Type::Unit)
                    };
                    for block in blocks[0..=i].iter() {
                        implicit_convert_map.insert(EntityID::from(block.program), ImplicitConvertKind::Ok);
                    }
                    has_implicit_convert = true;
                } else if second_type.value.is_option_or_result() {
                    let result = if let Type::Option(value_type) = &second_type.value {
                        type_environment.unify_type(
                            &first_type.value,
                            &first_type.span,
                            &value_type,
                            &second_type.span,
                            current_scope_this_type,
                            false
                        )
                    } else if let Type::Result { value, error: _ } = &second_type.value {
                        type_environment.unify_type(
                            &first_type.value,
                            &first_type.span,
                            &value,
                            &second_type.span,
                            current_scope_this_type,
                            false
                        )
                    } else {
                        unreachable!()
                    };

                    if let Err(_) = result {
                        type_environment.add_lazy_type_error_report(error);
                    } else {
                        first_type.value = if let Type::Option(_) = &second_type.value {
                            Type::Option(Arc::new(first_type.value))
                        } else if let Type::Result { value: _, error } = &second_type.value {
                            Type::Result { value: Arc::new(first_type.value), error: error.clone() }
                        } else {
                            unreachable!()
                        };

                        let convert_kind = if let Type::Option(_) = &first_type.value {
                            ImplicitConvertKind::Some
                        } else {
                            ImplicitConvertKind::Ok
                        };
                        for block in blocks[0..=i].iter() {
                            implicit_convert_map.insert(EntityID::from(block.program), convert_kind);
                        }
                        has_implicit_convert = true;
                    }
                } else {
                    type_environment.add_lazy_type_error_report(error);
                }
            } else {
                type_environment.add_lazy_type_error_report(error);
            }
        }
    }

    type_environment.set_entity_type(parent_ast_entity_id.value, first_type.clone());
}

fn type_inference_primary_right<'allocator, 'input>(
    ast: &PrimaryRight<'allocator, 'input>,
    previous_primary: Spanned<EntityID>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    type_environment: &mut TypeEnvironment<'allocator, 'input>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvertKind>,
    mappings: &mut Vec<MappingTypeKind, &'allocator Bump>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    let second_expr_id = if let Some(second_expr) = &ast.second_expr {
        let user_type = type_environment.resolve_entity_type(previous_primary.value);
        let literal_entity_id = EntityID::from(&second_expr.0);
        
        let element_name = second_expr.0.value;
        let element_type = user_type.value.get_element_type_with_replaced_generic(element_name);

        let mut pre_element_types = Vec::new_in(allocator);
        if let Some(element_type) = element_type {
            pre_element_types.push((element_type, user_type.value.clone(), None));
        }
        
        let mut implementations = Vec::new_in(allocator);
        
        if let Type::Generic(generic_define) = &user_type.value {
            for bound in generic_define.bounds.freeze_and_get().iter() {
                if let Some(element_type) = bound.ty.get_element_type_with_replaced_generic(element_name) {
                    let element_type = element_type
                        .map(|ty| { ty.replace_this_type(&user_type.value, true) });
                    pre_element_types.push((
                        element_type,
                        bound.ty.clone(),
                        None
                    ));
                } else {
                    implementations.extend(
                        global_implements_info_set.collect_satisfied_implementations(
                            &bound.ty,
                            type_environment,
                            current_scope_implements_info_set,
                            allocator
                        ).into_iter().map(|implementation| {
                            let this_type = Type::Generic(generic_define.clone());
                            
                            let implements_info = implementation.implements_info;
                            let implements_info = ImplementsInfo {
                                generics: implements_info.generics,
                                interface: implements_info.interface.map(|ty| { ty.replace_this_type(&this_type, true) }),
                                concrete: implements_info.concrete.map(|ty| { ty.replace_this_type(&this_type, true) }),
                                module_name: implements_info.module_name,
                                where_bounds: Arc::new(Type::replace_where_bounds_this_type(&implements_info.where_bounds, &this_type)),
                                element_types: Arc::new(Default::default()),
                                is_bounds_info: true
                            };
                            
                            CollectedImplementation {
                                implements_info,
                                local_generics: implementation.local_generics
                            }
                        })
                    );
                }
            }
        }
        
        implementations.extend(global_implements_info_set.collect_satisfied_implementations(
            &user_type.value,
            type_environment,
            current_scope_implements_info_set,
            allocator
        ));
        
        for implementation in implementations {
            let element_type = implementation.implements_info.get_element_type(element_name);
            if let Some(element_type) = element_type {
                let ty = Type::get_type_with_replaced_generics(
                    &element_type.value,
                    &implementation.implements_info.generics,
                    &implementation.local_generics
                );

                let interface = implementation.implements_info.interface;

                pre_element_types.push(
                    (
                        WithDefineInfo {
                            value: ty,
                            module_name: element_type.module_name.clone(),
                            span: element_type.span.clone()
                        },
                        interface.value.clone(),
                        Some((
                            WithDefineInfo {
                                value: interface.value,
                                module_name: implementation.implements_info.module_name,
                                span: interface.span
                            },
                            implementation.implements_info.concrete.value,
                            implementation.implements_info.where_bounds
                        ))
                    )
                );
            }
        }

        let mut origin_types = FxHashSet::default();
        let mut element_types = Vec::new_in(allocator);
        'root: for (element_type, origin_type, interface_and_where_bounds) in pre_element_types {
            
            let ty = type_environment.get_user_or_function_type_with_local_generic_id(
                Spanned::new(element_type.value, second_expr.0.span.clone()),
                current_scope_implements_info_set,
                false
            ).value;

            if let Some(where_bounds) = ty.get_where_bounds_with_replaced_generic() {
                for where_bound in where_bounds.iter() {
                    for bound in where_bound.bounds.iter() {
                        if !global_implements_info_set.is_implemented(
                            &where_bound.target_type.value,
                            &bound.ty,
                            type_environment,
                            current_scope_implements_info_set,
                            true
                        ) {
                            continue 'root;
                        }
                    }
                }
            }
            
            if let Type::UserType { user_type_info, generics: _, generics_span: _ } = origin_type {
                let name = (user_type_info.module_name.clone(), user_type_info.name.value.clone());
                if origin_types.contains(&name) {
                    continue;
                }
                origin_types.insert(name);
            }

            element_types.push(
                (
                    WithDefineInfo {
                        value: ty,
                        module_name: element_type.module_name.clone(),
                        span: element_type.span.clone()
                    },
                    interface_and_where_bounds
                )
            );
        }

        let element_type = if element_types.is_empty() {
            let error = NotFoundTypeElementError {
                user_type: user_type.value.clone(),
                name: second_expr.0.clone().map(|name| { name.to_string() })
            };
            type_environment.add_lazy_type_error_report(error);

            Type::Unknown
        } else {
            if element_types.len() > 1 {
                let found_elements = element_types.iter()
                    .map(|element| { element.0.clone().map(|_| {()}) })
                    .collect();

                let error = DuplicatedElement {
                    type_name: type_environment.get_type_display_string(&user_type.value),
                    element: second_expr.0.clone().map(|name| { name.to_string() }),
                    count: element_types.len(),
                    found_elements
                };
                errors.push(TranspileError::new(error));
            } else {
                let where_bounds = element_types.last().unwrap().0.value.get_where_bounds_with_replaced_generic();

                if let Some(where_bounds) = where_bounds {
                    for where_bound in where_bounds.iter() {
                        type_environment.generic_bounds_checks.push(GenericsBoundCheck::Where {
                            type_span: second_expr.0.span.clone(),
                            target_type: where_bound.target_type.value.clone(),
                            bounds: where_bound.bounds.clone(),
                            scope_implements_info_set: current_scope_implements_info_set.clone()
                        });
                    }
                }

                if let Some((interface, concrete, where_bounds)) = &element_types.last().unwrap().1 {
                    type_environment.impl_interface_generics_check.push((second_expr.0.span.clone(), interface.clone()));

                    for where_bound in where_bounds.iter() {
                        type_environment.generic_bounds_checks.push(GenericsBoundCheck::Where {
                            type_span: second_expr.0.span.clone(),
                            target_type: where_bound.target_type.value.clone(),
                            bounds: where_bound.bounds.clone(),
                            scope_implements_info_set: current_scope_implements_info_set.clone()
                        });
                    }
                }
            }
            element_types.last().unwrap().0.value.clone()
        };

        let element_type = Spanned::new(element_type, second_expr.0.span.clone());

        type_environment.set_entity_type(
            literal_entity_id,
            element_type
        );

        if let Some(generics) = &second_expr.1 {
            if let Ok(generics) = generics {
                type_inference_generics(
                    generics,
                    literal_entity_id,
                    user_type_map,
                    import_element_map,
                    name_resolved_map,
                    module_user_type_map,
                    module_element_type_map,
                    generics_map,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    type_environment,
                    errors,
                    warnings,
                    context
                );
            }
        }

        let last_entity_id = if let Some(function_call) = &second_expr.2 {
            type_inference_function_call(
                function_call,
                literal_entity_id,
                ast.separator.value == PrimarySeparatorKind::Dot,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                module_element_type_maps,
                generics_map,
                module_entity_type_map,
                global_implements_info_set,
                current_scope_this_type,
                current_scope_implements_info_set,
                type_environment,
                implicit_convert_map,
                allocator,
                errors,
                warnings,
                context
            );

            let span_start = second_expr.0.span.start;
            let span_end = function_call.span.end;

            Spanned::new(EntityID::from(function_call), span_start..span_end)
        } else {
            Spanned::new(literal_entity_id, second_expr.0.span.clone())
        };

        let mut ty = type_environment.resolve_entity_type(last_entity_id.value);
        for mapping in mappings.iter().rev() {
            ty.value = match mapping {
                MappingTypeKind::Option => Type::Option(Arc::new(ty.value)),
                MappingTypeKind::Result { error_type } => {
                    Type::Result { value: Arc::new(ty.value), error: Arc::new(error_type.clone()) }
                }
            };
        }
        mappings.clear();
        type_environment.set_entity_type(last_entity_id.value, ty);
        
        last_entity_id
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
            global_implements_info_set,
            current_scope_this_type,
            current_scope_implements_info_set,
            type_environment,
            implicit_convert_map,
            mappings,
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
            second_expr_id.value,
            EntityID::from(ast)
        );
    }
}

fn type_inference_mapping_operator<'allocator, 'input>(
    ast: &MappingOperator<'allocator, 'input>,
    previous_entity_id: Spanned<EntityID>,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    type_environment: &mut TypeEnvironment<'allocator, 'input>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvertKind>,
    mappings: &mut Vec<MappingTypeKind, &'allocator Bump>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    let block = match &ast.value {
        MappingOperatorKind::NullElvisBlock(block) => block.value.as_ref(),
        MappingOperatorKind::ResultElvisBlock(block) => block.value.as_ref(),
        _ => None
    };

    let previous_type = type_environment.resolve_entity_type(previous_entity_id.value);
    let previous_type_resolved = if let Type::LocalGeneric(generic_id) = &previous_type.value {
        type_environment.resolve_generic_type(*generic_id).1.value
    } else {
        previous_type.value.clone()
    };

    let check_result = match &ast.value {
        MappingOperatorKind::NullPropagation
         | MappingOperatorKind::NullUnwrap
         | MappingOperatorKind::NullElvisBlock(_) => {
            if let Type::Option(value_type) = &previous_type_resolved {
                Ok(value_type.as_ref().clone())
            } else {
                Err(ExpectedTypeKind::Option)
            }
        },
        MappingOperatorKind::ResultPropagation
         | MappingOperatorKind::ResultUnwrap
         | MappingOperatorKind::ResultElvisBlock(_) => {
            if let Type::Result { value, error: _ } = &previous_type_resolved {
                Ok(value.as_ref().clone())
            } else {
                Err(ExpectedTypeKind::Result)
            }
        }
    };

    if let Type::Option(_) = &previous_type_resolved {
        mappings.push(MappingTypeKind::Option)
    }
    if let Type::Result { value: _, error } = &previous_type_resolved {
        mappings.push(MappingTypeKind::Result { error_type: error.as_ref().clone() })
    }

    if let Err(expected) = &check_result {
        let error = UnexpectedTypeError {
            error_code: MAPPING_OPERATOR_TYPE_ERROR,
            found_type: Spanned::new(previous_type.value.clone(), previous_entity_id.span.clone()),
            expected_kind: Spanned::new(*expected, ast.span.clone())
        };
        type_environment.add_lazy_type_error_report(error);
    }

    if let Some(block) = &block {
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
            global_implements_info_set,
            current_scope_this_type,
            current_scope_implements_info_set,
            false,
            false,
            &Vec::new(),
            true,
            type_environment,
            implicit_convert_map,
            allocator,
            errors,
            warnings,
            context
        );

        if let Ok(value_type) = check_result {
            let span = previous_entity_id.span.start..ast.span.end;
            type_environment.set_entity_type(
                EntityID::from(&ast.value),
                Spanned::new(value_type, span.clone())
            );

            let result = type_environment.unify_with_implicit_convert(
                Spanned::new(EntityID::from(&ast.value), span),
                Spanned::new(EntityID::from(block.program), block.program.span.clone()),
                current_scope_this_type,
                false
            );

            add_error(result, type_environment);
        } else {
            type_environment.set_entity_type(
                EntityID::from(&ast.value),
                Spanned::new(Type::Unknown, ast.span.clone())
            );
        }
    } else {
        let span = previous_entity_id.span.start..ast.span.end;

        if let Ok(value_type) = check_result {
            type_environment.set_entity_type(
                EntityID::from(&ast.value),
                Spanned::new(value_type, span)
            );
        } else {
            type_environment.set_entity_type(
                EntityID::from(&ast.value),
                Spanned::new(previous_type.value, span)
            );
        }
    }
}

fn type_inference_generics<'allocator, 'input>(
    ast: &Generics,
    previous: EntityID,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    type_environment: &mut TypeEnvironment<'allocator, 'input>,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    let ty = type_environment.resolve_entity_type(previous);
    let generics = match &ty.value {
        Type::UserType { user_type_info: _, generics, generics_span: _ } => generics,
        Type::Function { function_info: _, generics } => generics,
        _ => {
            let error = InvalidSetGenericsTypeError {
                ty: ty.clone(),
                set_span: ast.span.clone()
            };
            type_environment.add_lazy_type_error_report(error);
            return;
        }
    };

    if generics.len() != ast.elements.len() {
        let error = NumberOfGenericsMismatchError {
            set_span: ast.span.clone(),
            expected: generics.len(),
            found: ast.elements.len(),
            ty: ty.map(|ty| { ty.as_original_type() })
        };
        type_environment.add_lazy_type_error_report(error);
        return;
    }

    for i in 0..generics.len() {
        let generic = &generics[i];
        if let Type::LocalGeneric(generic_id) = generic {
            let type_info = &ast.elements[i];
            let ty = get_type(
                type_info,
                user_type_map,
                import_element_map,
                name_resolved_map,
                module_user_type_map,
                module_element_type_map,
                generics_map,
                current_scope_this_type,
                errors,
                warnings,
                context
            );

            let ty = Spanned::new(ty.clone(), type_info.span.clone());

            type_environment.set_generic_type(
                *generic_id,
                ty.clone()
            );

            type_environment.add_check_type_info_bounds(
                ty.clone(),
                &ty.value,
                true,
                current_scope_implements_info_set
            );
        }
    }
}

fn type_inference_function_call<'allocator, 'input>(
    ast: &FunctionCall<'allocator, 'input>,
    function: EntityID,
    is_method_call: bool,
    user_type_map: &FxHashMap<String, Type>,
    import_element_map: &FxHashMap<EntityID, String>,
    name_resolved_map: &FxHashMap<EntityID, FoundDefineInfo>,
    module_user_type_map: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    module_element_type_map: &FxHashMap<String, Type>,
    module_element_type_maps: &FxHashMap<String, Arc<FxHashMap<String, Type>>>,
    generics_map: &FxHashMap<EntityID, Arc<GenericType>>,
    module_entity_type_map: &FxHashMap<EntityID, Type>,
    global_implements_info_set: &ImplementsInfoSet,
    current_scope_this_type: &ScopeThisType,
    current_scope_implements_info_set: &Option<Arc<ImplementsInfoSet>>,
    type_environment: &mut TypeEnvironment<'allocator, 'input>,
    implicit_convert_map: &mut FxHashMap<EntityID, ImplicitConvertKind>,
    allocator: &'allocator Bump,
    errors: &mut Vec<TranspileError>,
    warnings: &mut Vec<TranspileWarning>,
    context: &TranspileModuleContext
) {
    let function_type = type_environment.resolve_entity_type(function);

    if let Type::Function { function_info, generics: _ } = &function_type.value {
        let is_method_call = is_method_call && function_info.is_extension;

        let function_type = function_type.value.clone();

        if let Ok(arg_exprs) = &ast.arg_exprs {
            let is_method_call = is_method_call && function_info.is_extension;

            let number_of_args = if is_method_call {
                (function_info.argument_types.len() - 1, arg_exprs.len())
            } else {
                (function_info.argument_types.len(), arg_exprs.len())
            };

            if number_of_args.0 != number_of_args.1 {
                let error = TranspileError::new(ArgumentCountMismatchError {
                    defined_count: number_of_args.0,
                    specified_count: number_of_args.1,
                    defined_module: function_info.define_info.module_name.clone(),
                    defined_span: function_info.define_info.arguments_span.clone(),
                    specified_span: ast.span.clone()
                });
                errors.push(error);
            }
            
            let mut define_arg_index = if is_method_call { 1 } else { 0 };

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
                    global_implements_info_set,
                    current_scope_this_type,
                    current_scope_implements_info_set,
                    true,
                    type_environment,
                    implicit_convert_map,
                    allocator,
                    errors,
                    warnings,
                    context
                );

                if let Some(defined_arg_type) = function_type.get_indexed_element_type_with_replaced_generic(define_arg_index) {
                    type_environment.set_entity_type(
                        EntityID::from(ast),
                        Spanned::new(defined_arg_type, arg_expr.get_span())
                    );

                    let result = type_environment.unify_with_implicit_convert(
                        Spanned::new(EntityID::from(*arg_expr), arg_expr.get_span()),
                        Spanned::new(EntityID::from(ast), arg_expr.get_span()),
                        current_scope_this_type,
                        true
                    );

                    if let Err(error) = result {
                        type_environment.add_lazy_type_error_report(error);
                    }
                }

                define_arg_index += 1;
            }
        } else {
            let number_of_args = if is_method_call {
                function_info.argument_types.len() - 1
            } else {
                function_info.argument_types.len()
            };

            if number_of_args != 0 {
                let error = TranspileError::new(ArgumentCountMismatchError {
                    defined_count: number_of_args,
                    specified_count: 0,
                    defined_module: function_info.define_info.module_name.clone(),
                    defined_span: function_info.define_info.arguments_span.clone(),
                    specified_span: ast.span.clone()
                });
                errors.push(error);
            }
        }

        let return_type = function_type.get_return_type_with_replaced_generic().unwrap();

        type_environment.set_entity_type(
            EntityID::from(ast),
            Spanned::new(return_type, ast.span.clone())
        );
    } else {
        type_environment.add_lazy_type_error_report(
            SimpleTypeError { error_code: FUNCTION_CALL_TYPE_ERROR, ty: function_type }
        );

        type_environment.set_entity_type(
            EntityID::from(ast),
            Spanned::new(Type::Unknown, ast.span.clone())
        );
    }
}


pub(crate) trait LazyTypeReport {
    fn build_report(&self, type_environment: &TypeEnvironment, context: &TranspileModuleContext) -> Either<TranspileError, TranspileWarning>;
}


pub(crate) struct TypeMismatchError {
    type_0: Spanned<Type>,
    type_1: Spanned<Type>,
    generics: Vec<(Spanned<Type>, Spanned<Type>)>
}

impl LazyTypeReport for TypeMismatchError {
    fn build_report(&self, type_environment: &TypeEnvironment, context: &TranspileModuleContext) -> Either<TranspileError, TranspileWarning> {
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

        if &self.type_0.span == &self.type_1.span {
            builder.add_label(
                Label::new((module_name, self.type_0.span.clone()))
                    .with_color(Color::Red)
                    .with_message(
                        key.get_massage(text, ErrorMessageType::Label(2))
                            .replace("%found", self.type_0.value.clone().fg(Color::Red).to_string().as_str())
                            .replace("%expected", self.type_1.value.clone().fg(Color::Yellow).to_string().as_str())
                    )
            );
        } else {
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
        }

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
                EnvironmentSeparatorKind::UserTypeDefine => false,
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
    fn build_report(&self, type_environment: &TypeEnvironment, context: &TranspileModuleContext) -> Either<TranspileError, TranspileWarning> {
        let error = SimpleError::new(
            0037,
            self.name.span.clone(),
            vec![
                (type_environment.get_type_display_string(&self.user_type), Color::Yellow),
                (self.name.value.to_string(), Color::Red)
            ],
            vec![((context.module_name.clone(), self.name.span.clone()), Color::Red)]
        );
        Either::Left(error)
    }
}



struct SimpleTypeError {
    error_code: usize,
    ty: Spanned<Type>
}

impl LazyTypeReport for SimpleTypeError {
    fn build_report(&self, type_environment: &TypeEnvironment, context: &TranspileModuleContext) -> Either<TranspileError, TranspileWarning> {
        let ty = self.ty.clone().map(|ty| { type_environment.get_type_display_string(&ty) });
        
        let error = SimpleError::new(
            self.error_code,
            self.ty.span.clone(),
            vec![(ty.value, Color::Red)],
            vec![((context.module_name.clone(), ty.span), Color::Red)]
        );
        Either::Left(error)
    }
}



struct ArgumentCountMismatchError {
    defined_count: usize,
    specified_count: usize,
    defined_module: Arc<String>,
    defined_span: Range<usize>,
    specified_span: Range<usize>
}

impl TranspileReport for ArgumentCountMismatchError {
    fn print(&self, context: &TranspileModuleContext) {
        let module_name = context.module_name.deref();
        let text = &context.context.localized_text;
        let error_code = 0040;
        let key = ErrorMessageKey::new(error_code);

        let message = key.get_massage(text, ErrorMessageType::Message);

        let mut builder = Report::build(ReportKind::Error, module_name, self.specified_span.start)
            .with_code(error_code)
            .with_message(message);

        builder.add_label(
            Label::new((module_name.clone(), self.specified_span.clone()))
                .with_color(Color::Red)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(0))
                        .replace("%defined", self.defined_count.to_string().fg(Color::Green).to_string().as_str())
                        .replace("%specified", self.specified_count.to_string().fg(Color::Yellow).to_string().as_str())
                )
        );

        builder.add_label(
            Label::new((self.defined_module.deref().clone(), self.defined_span.clone()))
                .with_color(Color::Yellow)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(1))
                        .replace("%defined", self.defined_count.to_string().fg(Color::Green).to_string().as_str())
                        .replace("%specified", self.specified_count.to_string().fg(Color::Yellow).to_string().as_str())
                )
        );

        if let Some(note) = key.get_massage_optional(text, ErrorMessageType::Note) {
            builder.set_note(note);
        }

        if let Some(help) = key.get_massage_optional(text, ErrorMessageType::Help) {
            builder.set_help(help);
        }

        let defined_module_context = context.context.get_module_context(&self.defined_module).unwrap();

        let source_list = vec![
            (module_name.clone(), context.source_code.code.as_str()),
            (self.defined_module.deref().clone(), defined_module_context.source_code.code.as_str())
        ];

        builder.finish().print(sources(source_list)).unwrap();
    }
}



struct UnexpectedTypeError {
    error_code: usize,
    found_type: Spanned<Type>,
    expected_kind: Spanned<ExpectedTypeKind>
}

impl LazyTypeReport for UnexpectedTypeError {
    fn build_report(&self, type_environment: &TypeEnvironment, _: &TranspileModuleContext) -> Either<TranspileError, TranspileWarning> {
        let report = UnexpectedTypeErrorReport {
            error_code: self.error_code,
            found_type: self.found_type.clone().map(|ty| { type_environment.get_type_display_string(&ty) }),
            expected_kind: self.expected_kind.clone()
        };
        Either::Left(TranspileError::new(report))
    }
}

#[derive(Clone, Copy)]
enum ExpectedTypeKind {
    Option,
    Result
}

impl ExpectedTypeKind {
    fn get_key(&self) -> String {
        format!("expected_type.{}", match self {
            ExpectedTypeKind::Option => "option",
            ExpectedTypeKind::Result => "result"
        })
    }
}

struct UnexpectedTypeErrorReport {
    error_code: usize,
    found_type: Spanned<String>,
    expected_kind: Spanned<ExpectedTypeKind>
}

impl TranspileReport for UnexpectedTypeErrorReport {
    fn print(&self, context: &TranspileModuleContext) {
        let module_name = &context.module_name;
        let text = &context.context.localized_text;
        let key = ErrorMessageKey::new(self.error_code);

        let message = key.get_massage(text, ErrorMessageType::Message);

        let mut builder = Report::build(ReportKind::Error, module_name, self.found_type.span.start)
            .with_code(self.error_code)
            .with_message(message);

        let expected_text = text.get_text(self.expected_kind.value.get_key());

        builder.add_label(
            Label::new((module_name, self.found_type.span.clone()))
                .with_color(Color::Red)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(0))
                        .replace("%found", self.found_type.value.clone().fg(Color::Red).to_string().as_str())
                        .replace("%expected", expected_text.as_str().fg(Color::Green).to_string().as_str())
                )
        );

        builder.add_label(
            Label::new((module_name, self.expected_kind.span.clone()))
                .with_color(Color::Yellow)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(1))
                        .replace("%found", self.found_type.value.clone().fg(Color::Red).to_string().as_str())
                        .replace("%expected", expected_text.as_str().fg(Color::Green).to_string().as_str())
                )
        );

        if let Some(note) = key.get_massage_optional(text, ErrorMessageType::Note) {
            builder.set_note(note);
        }

        if let Some(help) = key.get_massage_optional(text, ErrorMessageType::Help) {
            builder.set_help(help);
        }

        builder.finish().print((module_name, Source::from(context.source_code.code.as_str()))).unwrap();
    }
}


struct BoundsNotSatisfiedError {
    span: Range<usize>,
    type_name: Spanned<String>,
    bounds_module_name: Arc<String>,
    not_satisfied_bounds: Vec<Spanned<String>>
}

impl TranspileReport for BoundsNotSatisfiedError {
    fn print(&self, context: &TranspileModuleContext) {
        let module_name = context.module_name.deref();
        let text = &context.context.localized_text;
        let key = ErrorMessageKey::new(BOUNDS_NOT_SATISFIED_ERROR);

        let message = key.get_massage(text, ErrorMessageType::Message);

        let mut builder = Report::build(ReportKind::Error, module_name, self.span.start)
            .with_code(BOUNDS_NOT_SATISFIED_ERROR)
            .with_message(message);

        let type_text = self.type_name.value.clone().fg(Color::Red).to_string();

        let bounds_text = self.not_satisfied_bounds.iter()
            .map(|bound| { bound.value.clone().fg(Color::Yellow).to_string() })
            .collect::<Vec<String>>()
            .join(", ");

        builder.add_label(
            Label::new((module_name.clone(), self.span.clone()))
                .with_color(Color::Red)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(0))
                        .replace("%type", &type_text)
                        .replace("%bounds", &bounds_text)
                )
        );

        builder.add_label(
            Label::new((module_name.clone(), self.type_name.span.clone()))
                .with_color(Color::Yellow)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(1))
                        .replace("%type", &type_text)
                )
        );

        for bound in self.not_satisfied_bounds.iter() {
            let bound_text = bound.value.clone().fg(Color::Yellow).to_string();

            builder.add_label(
                Label::new((self.bounds_module_name.deref().clone(), bound.span.clone()))
                    .with_color(Color::Yellow)
                    .with_message(
                        key.get_massage(text, ErrorMessageType::Label(2))
                            .replace("%bound", &bound_text)
                    )
            );
        }

        if let Some(note) = key.get_massage_optional(text, ErrorMessageType::Note) {
            let note = note.replace("%type", &type_text).replace("%bounds", &bounds_text);
            builder.set_note(note);
        }

        if let Some(help) = key.get_massage_optional(text, ErrorMessageType::Help) {
            let help = help.replace("%type", &type_text).replace("%bounds", &bounds_text);
            builder.set_help(help);
        }


        let bounds_module_context = context.context.get_module_context(&self.bounds_module_name).unwrap();

        let source_list = vec![
            (module_name.clone(), context.source_code.code.as_str()),
            (self.bounds_module_name.deref().clone(), bounds_module_context.source_code.code.as_str())
        ];

        builder.finish().print(sources(source_list)).unwrap();
    }
}

struct WhereBoundsNotSatisfiedError {
    type_name: Spanned<String>,
    bounds_module_name: Arc<String>,
    not_satisfied_bounds: Vec<Spanned<String>>
}

impl TranspileReport for WhereBoundsNotSatisfiedError {
    fn print(&self, context: &TranspileModuleContext) {
        let module_name = context.module_name.deref();
        let text = &context.context.localized_text;
        let key = ErrorMessageKey::new(WHERE_BOUNDS_NOT_SATISFIED_ERROR);

        let message = key.get_massage(text, ErrorMessageType::Message);

        let mut builder = Report::build(ReportKind::Error, module_name, self.type_name.span.start)
            .with_code(WHERE_BOUNDS_NOT_SATISFIED_ERROR)
            .with_message(message);

        let type_text = self.type_name.value.clone().fg(Color::Red).to_string();

        let bounds_text = self.not_satisfied_bounds.iter()
            .map(|bound| { bound.value.clone().fg(Color::Yellow).to_string() })
            .collect::<Vec<String>>()
            .join(" + ");

        builder.add_label(
            Label::new((module_name.clone(), self.type_name.span.clone()))
                .with_color(Color::Red)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(0))
                        .replace("%type", &type_text)
                        .replace("%bounds", &bounds_text)
                )
        );

        for bound in self.not_satisfied_bounds.iter() {
            let bound_text = bound.value.clone().fg(Color::Yellow).to_string();

            builder.add_label(
                Label::new((self.bounds_module_name.deref().clone(), bound.span.clone()))
                    .with_color(Color::Yellow)
                    .with_message(
                        key.get_massage(text, ErrorMessageType::Label(1))
                            .replace("%bound", &bound_text)
                    )
            );
        }

        if let Some(note) = key.get_massage_optional(text, ErrorMessageType::Note) {
            let note = note.replace("%type", &type_text).replace("%bounds", &bounds_text);
            builder.set_note(note);
        }

        if let Some(help) = key.get_massage_optional(text, ErrorMessageType::Help) {
            let help = help.replace("%type", &type_text).replace("%bounds", &bounds_text);
            builder.set_help(help);
        }


        let bounds_module_context = context.context.get_module_context(&self.bounds_module_name).unwrap();

        let source_list = vec![
            (module_name.clone(), context.source_code.code.as_str()),
            (self.bounds_module_name.deref().clone(), bounds_module_context.source_code.code.as_str())
        ];

        builder.finish().print(sources(source_list)).unwrap();
    }
}


struct InvalidSetGenericsTypeError {
    ty: Spanned<Type>,
    set_span: Range<usize>
}

impl LazyTypeReport for InvalidSetGenericsTypeError {
    fn build_report(&self, type_environment: &TypeEnvironment, context: &TranspileModuleContext) -> Either<TranspileError, TranspileWarning> {
        let type_name = type_environment.get_type_display_string(&self.ty.value)
            .fg(Color::Red).to_string();

        Either::Left(TranspileError::new(SimpleError::new(
            INVALID_SET_GENERICS_TYPE_ERROR,
            self.set_span.clone(),
            vec![(type_name, Color::Yellow)],
            vec![
                ((context.module_name.clone(), self.set_span.clone()), Color::Red),
                ((context.module_name.clone(), self.ty.span.clone()), Color::Yellow)
            ]
        )))
    }
}


struct NumberOfGenericsMismatchError {
    set_span: Range<usize>,
    expected: usize,
    found: usize,
    ty: Spanned<Type>
}

impl LazyTypeReport for NumberOfGenericsMismatchError {
    fn build_report(&self, type_environment: &TypeEnvironment, _: &TranspileModuleContext) -> Either<TranspileError, TranspileWarning> {
        let type_name = type_environment.get_type_display_string(&self.ty.value)
            .fg(Color::Yellow).to_string();

        let found_text = self.found.to_string().fg(Color::Red).to_string();
        let expected_text = self.expected.to_string().fg(Color::Yellow).to_string();

        let (define_module_name, define_span) = match &self.ty.value {
            Type::UserType { user_type_info, generics: _, generics_span: _ } => {
                (
                    user_type_info.module_name.clone(),
                    user_type_info.generics_define_span.clone()
                        .unwrap_or_else(|| { user_type_info.name.span.clone() })
                )
            },
            Type::Function { function_info, generics: _ } => {
                (
                    function_info.define_info.module_name.clone(),
                    function_info.define_info.generics_define_span.clone()
                        .unwrap_or_else(|| { function_info.define_info.span.clone() })
                )
            },
            _ => unreachable!()
        };

        Either::Left(TranspileError::new(NumberOfGenericsMismatchErrorReport {
            set_span: self.set_span.clone(),
            type_name,
            found_text,
            expected_text,
            define_module_name,
            define_span
        }))
    }
}


struct NumberOfGenericsMismatchErrorReport {
    set_span: Range<usize>,
    type_name: String,
    found_text: String,
    expected_text: String,
    define_module_name: Arc<String>,
    define_span: Range<usize>
}

impl TranspileReport for NumberOfGenericsMismatchErrorReport {
    fn print(&self, context: &TranspileModuleContext) {
        let module_name = context.module_name.deref();
        let text = &context.context.localized_text;
        let key = ErrorMessageKey::new(NUMBER_OF_GENERICS_MISMATCH_ERROR);

        let message = key.get_massage(text, ErrorMessageType::Message);

        let mut builder = Report::build(ReportKind::Error, module_name, self.set_span.start)
            .with_code(NUMBER_OF_GENERICS_MISMATCH_ERROR)
            .with_message(message);

        builder.add_label(
            Label::new((module_name.clone(), self.set_span.clone()))
                .with_color(Color::Red)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(0))
                        .replace("%found", &self.found_text)
                        .replace("%expected", &self.expected_text)
                )
        );

        builder.add_label(
            Label::new((self.define_module_name.deref().clone(), self.define_span.clone()))
                .with_color(Color::Yellow)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(1))
                        .replace("%type", &self.type_name)
                        .replace("%expected", &self.expected_text)
                )
        );

        if let Some(note) = key.get_massage_optional(text, ErrorMessageType::Note) {
            builder.set_note(note);
        }

        if let Some(help) = key.get_massage_optional(text, ErrorMessageType::Help) {
            builder.set_help(help);
        }


        let bounds_module_context = context.context.get_module_context(&self.define_module_name).unwrap();

        let source_list = vec![
            (module_name.clone(), context.source_code.code.as_str()),
            (self.define_module_name.deref().clone(), bounds_module_context.source_code.code.as_str())
        ];

        builder.finish().print(sources(source_list)).unwrap();
    }
}


struct DuplicatedElement {
    type_name: String,
    element: Spanned<String>,
    count: usize,
    found_elements: Vec<WithDefineInfo<()>>
}

impl TranspileReport for DuplicatedElement {
    fn print(&self, context: &TranspileModuleContext) {
        let module_name = context.module_name.deref();
        let text = &context.context.localized_text;
        let key = ErrorMessageKey::new(DUPLICATED_ELEMENT_ERROR);

        let type_name = self.type_name.clone().fg(Color::Yellow).to_string();
        let count = self.count.to_string().fg(Color::Red).to_string();
        let element = self.element.value.clone().fg(Color::Yellow).to_string();

        let message = key.get_massage(text, ErrorMessageType::Message)
            .replace("%type", &type_name);

        let mut builder = Report::build(ReportKind::Error, module_name, self.element.span.start)
            .with_code(DUPLICATED_ELEMENT_ERROR)
            .with_message(message);

        builder.add_label(
            Label::new((module_name.clone(), self.element.span.clone()))
                .with_color(Color::Red)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(0))
                        .replace("%count", &count)
                )
        );

        let mut module_names = Vec::new();
        module_names.push(module_name.clone());
        
        for found_element in self.found_elements.iter() {
            builder.add_label(
                Label::new((found_element.module_name.deref().clone(), found_element.span.clone()))
                    .with_color(Color::Yellow)
                    .with_message(
                        key.get_massage(text, ErrorMessageType::Label(1))
                            .replace("%element", &element)
                    )
            );

            module_names.push(found_element.module_name.deref().clone());
        }

        if let Some(note) = key.get_massage_optional(text, ErrorMessageType::Note) {
            builder.set_note(note.replace("%type", &type_name).replace("%element", &element));
        }

        if let Some(help) = key.get_massage_optional(text, ErrorMessageType::Help) {
            builder.set_help(help);
        }


        let module_list = module_names.iter()
            .map(|module_name| { context.context.get_module_context(module_name).unwrap() })
            .collect::<Vec<_>>();

        let source_list = module_list.iter()
            .map(|context| { (context.module_name.deref().clone(), context.source_code.code.as_str()) });

        builder.finish().print(sources(source_list)).unwrap();
    }
}


struct UnresolvedInterface {
    original_name: String,
    resolved_name: String,
    reference_span: Range<usize>,
    implementation: WithDefineInfo<()>
}

impl TranspileReport for UnresolvedInterface {
    fn print(&self, context: &TranspileModuleContext) {
        let module_name = context.module_name.deref();
        let text = &context.context.localized_text;
        let key = ErrorMessageKey::new(UNRESOLVED_INTERFACE);

        let original_name = self.original_name.clone().fg(Color::Yellow).to_string();
        let resolved_name = self.resolved_name.clone().fg(Color::Red).to_string();

        let message = key.get_massage(text, ErrorMessageType::Message)
            .replace("%original", &original_name);

        let mut builder = Report::build(ReportKind::Error, module_name, self.reference_span.start)
            .with_code(UNRESOLVED_INTERFACE)
            .with_message(message);

        builder.add_label(
            Label::new((module_name.clone(), self.reference_span.clone()))
                .with_color(Color::Red)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(0))
                        .replace("%resolved", &resolved_name)
                )
        );
        
        builder.add_label(
            Label::new((self.implementation.module_name.deref().clone(), self.implementation.span.clone()))
                .with_color(Color::Yellow)
                .with_message(
                    key.get_massage(text, ErrorMessageType::Label(1))
                )
        );

        if let Some(note) = key.get_massage_optional(text, ErrorMessageType::Note) {
            builder.set_note(note);
        }

        if let Some(help) = key.get_massage_optional(text, ErrorMessageType::Help) {
            builder.set_help(help);
        }

        let impl_module_context = context.context.get_module_context(&self.implementation.module_name).unwrap();

        let source_list = vec![
            (module_name.clone(), context.source_code.code.as_str()),
            (impl_module_context.module_name.deref().clone(), impl_module_context.source_code.code.as_str())
        ];

        builder.finish().print(sources(source_list)).unwrap();
    }
}